import os
import base64
import google.generativeai as genai
from typing import List, Optional, Dict
import sys
from dotenv import load_dotenv
from datetime import datetime
from pathlib import Path
from tkinter import filedialog
import tkinter as tk
from concurrent.futures import ThreadPoolExecutor, as_completed
import threading
import re
import json

# Load environment variables
load_dotenv()

# Configure Gemini
genai.configure(api_key=os.environ.get("GEMINI_API_KEY"))

# Version
__opp_version__ = "1.0"

# Import utilities from PD2
from src.utils import thread_safe_print, retry_with_backoff
from src.opp_sections import sections
from src.profile_prompts import (
    COMPANY_NAME_EXTRACTION_PROMPT,
    get_title_subtitle_prompt,
    get_section_generation_prompt,
    get_section_completeness_check_prompt,
    get_section_enhancement_prompt,
    get_section_polish_prompt,
    _get_section_boundaries
)
from src.pptx_generator import create_profile_pptx
from src.insight_memory import InsightMemory
from src.file_manager import FileManager

# ANSI Color Codes and Styling
RESET = '\033[0m'
BOLD = '\033[1m'
DIM = '\033[2m'

# Colors
RED = '\033[91m'
CYAN = '\033[96m'
WHITE = '\033[97m'
YELLOW = '\033[93m'

# Symbols
CHECK = '✓'
ARROW = '→'
WARNING = '⚠'
CROSS = '✗'

# UI Helper Functions (from PD2)
try:
    import termios
    import tty
except ImportError:
    termios = None
    tty = None
try:
    import msvcrt
except ImportError:
    msvcrt = None

def _read_single_key() -> str:
    """Read a single keypress without requiring Enter. Cross-platform."""
    if msvcrt is not None:
        ch = msvcrt.getch()
        try:
            return ch.decode('utf-8', errors='ignore')
        except Exception:
            return str(ch)
    if termios is None or tty is None:
        return sys.stdin.readline().strip()[:1]
    fd = sys.stdin.fileno()
    old_settings = termios.tcgetattr(fd)
    try:
        tty.setraw(fd)
        ch = sys.stdin.read(1)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
    return ch

def prompt_yes_no(prompt_text: str) -> bool:
    """Display a (y/n) prompt that accepts a single key without Enter."""
    while True:
        thread_safe_print(f"{prompt_text}", end='', flush=True)
        ch = _read_single_key()
        thread_safe_print(ch)
        if not ch:
            continue
        ch = ch.strip().lower()
        if ch in ('y', 'n'):
            return ch == 'y'

def prompt_single_digit(prompt_text: str, valid_digits: str, default_digit: str) -> str:
    """Display a numeric prompt that accepts a single key without Enter."""
    while True:
        thread_safe_print(f"{prompt_text}", end='', flush=True)
        ch = _read_single_key()
        thread_safe_print(ch)
        if not ch or ch in ('\r', '\n'):
            return default_digit
        ch = ch.strip()
        if ch in valid_digits:
            return ch

def select_pdf_files() -> Optional[List[str]]:
    """Interactively select PDF files"""
    while True:
        root = tk.Tk()
        root.withdraw()

        thread_safe_print("\nSelect PDF file(s) for profile generation...")
        pdf_files = filedialog.askopenfilenames(
            title="Select PDF Files",
            filetypes=[("PDF files", "*.pdf"), ("All files", "*.*")]
        )

        root.destroy()

        if not pdf_files:
            thread_safe_print("No files selected.")
            retry_yes = prompt_yes_no("Would you like to try selecting files again? (y/n): ")
            if not retry_yes:
                return None
            continue

        # Filter only PDF files
        pdf_files = [f for f in pdf_files if f.lower().endswith('.pdf')]

        if not pdf_files:
            thread_safe_print("No PDF files found. Please select PDF files.")
            continue

        thread_safe_print(f"Selected {len(pdf_files)} PDF file(s):")
        for pdf in pdf_files:
            thread_safe_print(f"  - {Path(pdf).name}")

        return list(pdf_files)


class WorkerDisplay:
    """Thread-safe display manager for parallel worker status"""

    def __init__(self, num_workers: int):
        self.num_workers = num_workers
        self.worker_status = {}  # {worker_id: (section_num, action)}
        self.lock = threading.Lock()
        self.next_worker_id = 1
        self.worker_ids = {}  # {section_num: worker_id}

    def update(self, section_num: int, action: str):
        """Update a worker's status and redraw the line

        Args:
            section_num: Section number being processed
            action: One of "Draft", "Check", "Enhance", "Polish", "Learn"
        """
        with self.lock:
            # Assign worker ID if this is a new section
            if section_num not in self.worker_ids:
                self.worker_ids[section_num] = self.next_worker_id
                self.next_worker_id += 1

            worker_id = self.worker_ids[section_num]
            self.worker_status[worker_id] = (section_num, action)
            self._redraw()

    def complete(self, section_num: int, completed: int, total: int):
        """Mark a section as complete (silent - no output)"""
        with self.lock:
            # Remove from active workers
            if section_num in self.worker_ids:
                worker_id = self.worker_ids[section_num]
                if worker_id in self.worker_status:
                    del self.worker_status[worker_id]

            # Silent completion - no progress output needed

    def _remove_silent(self, section_num: int):
        """Immediately remove section from display without output"""
        with self.lock:
            if section_num in self.worker_ids:
                worker_id = self.worker_ids[section_num]
                if worker_id in self.worker_status:
                    del self.worker_status[worker_id]

    def _redraw(self):
        """Redraw the worker status line (only active workers)"""
        if not self.worker_status:
            return

        # Sort by worker ID and format each active worker
        parts = []
        for wid in sorted(self.worker_status.keys()):
            sec_num, action = self.worker_status[wid]

            # Format with minimal styling: "Sec. 5 → Draft"
            status = f"{DIM}Sec.{RESET} {BOLD}{sec_num}{RESET} {ARROW} {action}"
            parts.append(status)

        # Join with separator and print
        line = f" {DIM}|{RESET} ".join(parts)
        thread_safe_print(line)


class OnePageProfile:
    """Generates one-page company profiles from PDF documents with parallel section processing"""

    def __init__(self, pdf_files: List[str], model_name: str, workers: int = 2):
        self.pdf_files = pdf_files
        self.model_name = model_name
        self.workers = workers

        # Create models with different temperatures
        self.model_low_temp = genai.GenerativeModel(
            model_name,
            generation_config=genai.types.GenerationConfig(temperature=0.2)
        )
        self.model_medium_temp = genai.GenerativeModel(
            model_name,
            generation_config=genai.types.GenerationConfig(temperature=0.6)
        )

        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        # PDF parts will be prepared once and reused
        self.pdf_parts = None

        # Initialize learning system and file manager
        self.insight_memory = InsightMemory(self.timestamp, model_name=model_name, memory_prefix="opp")
        self.file_manager = FileManager(self.timestamp)

        # Setup directories and ensure memory file exists
        self.file_manager.setup_directories(sections)
        os.makedirs("memory", exist_ok=True)
        self.file_manager.ensure_memory_file_exists(self.insight_memory.get_memory_data())

    def encode_pdf_to_base64(self, pdf_path: str) -> str:
        """Encode PDF file to base64 string"""
        with open(pdf_path, 'rb') as pdf_file:
            return base64.standard_b64encode(pdf_file.read()).decode('utf-8')

    def prepare_pdf_parts(self) -> List:
        """Prepare PDF files as inline base64 parts for Gemini"""
        parts = []

        for pdf_path in self.pdf_files:
            thread_safe_print(f"{CYAN}{ARROW}{RESET} Encoding {Path(pdf_path).name}...")
            pdf_data = self.encode_pdf_to_base64(pdf_path)

            parts.append({
                "inline_data": {
                    "mime_type": "application/pdf",
                    "data": pdf_data
                }
            })

        return parts

    def extract_company_name(self, pdf_parts: List) -> str:
        """Extract company name from PDF documents"""
        thread_safe_print(f"{CYAN}{ARROW}{RESET} Extracting company name...")

        try:
            response = self.model_medium_temp.generate_content(pdf_parts + [COMPANY_NAME_EXTRACTION_PROMPT])
            company_name = response.text.strip()

            # Validate
            if len(company_name) < 2 or len(company_name) > 100:
                company_name = "Company Profile"

            thread_safe_print(f"{CYAN}{CHECK}{RESET} Company: {company_name}")
            return company_name

        except Exception as e:
            thread_safe_print(f"{RED}{CROSS}{RESET} Failed to extract company name: {e}")
            return "Company Profile"

    def generate_title_subtitle(self, company_name: str) -> str:
        """Generate title and subtitle separately"""
        thread_safe_print(f"{CYAN}{ARROW}{RESET} Generating title and subtitle...")

        prompt = get_title_subtitle_prompt(company_name)

        try:
            response = self.model_medium_temp.generate_content(self.pdf_parts + [prompt])
            title_subtitle = response.text.strip()
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Title and subtitle complete")
            return title_subtitle
        except Exception as e:
            thread_safe_print(f"{RED}{CROSS}{RESET} Title generation failed: {e}")
            return f"# {company_name}\n[Company Profile]"

    def _generate_section(self, section: dict) -> str:
        """Step 1: Generate initial section content"""
        # Get relevant learning memory for this section
        relevant_memory = self.insight_memory.get_relevant_memory(section['number'])
        prompt = get_section_generation_prompt(section, relevant_memory)
        response = self.model_medium_temp.generate_content(self.pdf_parts + [prompt])
        return response.text.strip()

    def _check_section_completeness(self, section: dict, content: str) -> str:
        """Step 2: Check section completeness"""
        prompt = get_section_completeness_check_prompt(section, content)
        prompt = prompt.replace("{source_documents}", "[See attached PDF documents]")
        response = self.model_low_temp.generate_content(self.pdf_parts + [prompt])
        return response.text.strip()

    def _enhance_section(self, section: dict, content: str, add_list: str) -> str:
        """Step 3: Enhance section with missing items"""
        prompt = get_section_enhancement_prompt(section, content, add_list)
        prompt = prompt.replace("{source_documents}", "[See attached PDF documents]")
        response = self.model_medium_temp.generate_content(self.pdf_parts + [prompt])
        enhanced = response.text.strip()

        # Fallback to original if enhancement fails
        if not enhanced or len(enhanced) < 50:
            return content
        return enhanced

    def _polish_section(self, section: dict, content: str, word_limit: int) -> str:
        """Step 4: Polish section to word limit"""
        prompt = get_section_polish_prompt(section, content, word_limit)
        response = self.model_medium_temp.generate_content(prompt)
        polished = response.text.strip()

        # Fallback to original if polish fails
        if not polished or len(polished) < 50:
            return content
        return polished

    def _extract_learning(self, section: dict, final_output: str) -> str:
        """Step 5: Extract analytical principles that sharpen analysis"""
        prompt = f"""Extract analytical principles from this analysis that help sharpen and deepen future company analysis.

SECTION TYPE: {section['title']}

COMPLETED ANALYSIS:
---
{final_output}
---

SECTION SCOPE: {section['title']}

SECTION REQUIREMENTS (extract principles that help fulfill THESE ONLY):
{section['specs']}

SECTION BOUNDARIES - STAY IN SCOPE:
This is the "{section['title']}" section. Do NOT extract principles that belong in other sections:
{_get_section_boundaries(section['number'])}

PRINCIPLES SHOULD TRIGGER INSIGHTS, NOT FORCE CALCULATIONS:
Good principles guide toward observations that work with available data.
Avoid principles that require specific calculations or data that might not exist.
Keep principles generic - no company-specific or sector-specific language.

Extract 2-4 analytical PRINCIPLES - NOT company-specific findings, NOT vague wisdom, NOT just red flags.

WHAT TO EXTRACT:

Analytical principles that guide HOW to analyze more deeply:
- **Comparative techniques** - what to compare against what to reveal hidden truths
- **Decomposition approaches** - how to break down aggregates to expose real drivers
- **Verification methods** - how to test claims using different data sources
- **Relationship patterns** - what metrics or trends to correlate for deeper insight

GOOD EXAMPLES (analytical principles):
- "Compare stated strategy against actual capital allocation to reveal true management priorities"
- "Decompose aggregate growth into organic versus inorganic components to assess core business health"
- "Calculate implied operational metrics from management claims to test their plausibility"
- "Trace revenue recognition through to cash collection to verify business quality"
- "Compare segment economics to corporate average to identify where value is actually created"
- "Map capital deployment to subsequent margin changes to evaluate management effectiveness"

BAD EXAMPLES (too vague or too specific):
- "To see the future, analyze the growth segments" (too vague)
- "Revenue concentration above 30% means customer controls pricing" (specific red flag, not analytical principle)
- "Actions speak louder than words" (corporate poetry)

CONSTRAINTS:
- NO company names, NO sector names, NO specific company numbers
- 12-20 words per principle
- Focus on analytical APPROACHES that sharpen analysis
- Must apply to MOST companies (80%+), not just specific situations like:
  * Parent-subsidiary structures or transfer pricing scenarios
  * Multi-entity groups or conglomerates
  * Captive service centers
  * Complex organizational structures
- Avoid techniques that only work in rare or niche situations

OUTPUT FORMAT (JSON):
{{
  "principles": [
    "First analytical principle (12-20 words)",
    "Second analytical principle (12-20 words)"
  ]
}}

Extract principles that guide deeper analytical thinking."""

        return retry_with_backoff(
            lambda: self.model_low_temp.generate_content(prompt).text,
            context=section['number']
        )

    def process_section_main(self, section: dict, worker_display: WorkerDisplay) -> dict:
        """Process a single section through Steps 1-4 (main content generation)"""
        section_num = section['number']
        section_title = section['title']

        # Get section directory (already created by FileManager)
        section_dir = Path(self.file_manager.run_dir) / f"section_{section_num}"

        try:
            # Step 1: Initial Draft
            worker_display.update(section_num, "Draft")
            draft = self._generate_section(section)
            (section_dir / "step1_draft.md").write_text(f"## {section_title}\n{draft}", encoding='utf-8')

            # Step 2: Completeness Check
            worker_display.update(section_num, "Check")
            add_list = self._check_section_completeness(section, draft)
            (section_dir / "step2_add_list.txt").write_text(add_list, encoding='utf-8')

            # Step 3: Enhancement
            worker_display.update(section_num, "Enhance")
            enhanced = self._enhance_section(section, draft, add_list)
            (section_dir / "step3_enhanced.md").write_text(f"## {section_title}\n{enhanced}", encoding='utf-8')

            # Step 4: Polish (100 words)
            worker_display.update(section_num, "Polish")
            polished = self._polish_section(section, enhanced, word_limit=120)
            (section_dir / "step4_polished.md").write_text(f"## {section_title}\n{polished}", encoding='utf-8')

            # Remove from display immediately after Step 4 completes
            worker_display._remove_silent(section_num)

            return {
                'number': section_num,
                'title': section_title,
                'content': polished,
                'success': True
            }

        except Exception as e:
            thread_safe_print(f"{RED}{CROSS}{RESET} Section {section_num} failed: {e}")
            worker_display._remove_silent(section_num)
            return {
                'number': section_num,
                'title': section_title,
                'content': '',
                'success': False,
                'error': str(e)
            }

    def process_section_learn(self, section: dict) -> bool:
        """Process Step 5 (Learning Extraction) for a section - silent background work"""
        section_num = section['number']
        section_dir = Path(self.file_manager.run_dir) / f"section_{section_num}"

        try:
            # Load polished content from Step 4
            polished_file = section_dir / "step4_polished.md"
            if not polished_file.exists():
                return False

            polished = polished_file.read_text(encoding='utf-8')

            # Step 5: Learning Extraction (silent)
            learning = self._extract_learning(section, polished)
            (section_dir / "step5_learning.json").write_text(learning, encoding='utf-8')

            return True

        except Exception as e:
            thread_safe_print(f"{YELLOW}{WARNING}{RESET} Learning extraction failed for Section {section_num}: {e}")
            return False

    def generate_profile(self, company_name: str, worker_display: WorkerDisplay) -> str:
        """Generate profile with parallel section processing (Steps 1-4 only)"""
        thread_safe_print(f"\n{CYAN}Phase 1: Generating content (parallel workers: {self.workers})...{RESET}\n")

        results = []
        completed_count = 0

        # Process sections in parallel (Steps 1-4)
        with ThreadPoolExecutor(max_workers=self.workers) as executor:
            # Submit all section tasks
            future_to_section = {
                executor.submit(self.process_section_main, section, worker_display): section
                for section in sections
            }

            # Collect results as they complete
            for future in as_completed(future_to_section):
                section = future_to_section[future]
                result = future.result()
                results.append(result)
                completed_count += 1
                worker_display.complete(result['number'], completed_count, len(sections))

        # Sort results by section number
        results.sort(key=lambda x: x['number'])

        # Assemble final markdown
        parts = []
        for result in results:
            if result['success']:
                parts.append(f"## {result['title']}\n{result['content']}")

        return '\n\n'.join(parts)

    def _assemble_final_markdown(self, title_subtitle: str, section_content: str) -> str:
        """Combine title/subtitle with section content"""
        return f"{title_subtitle}\n\n{section_content}"

    def _conduct_memory_review(self):
        """Review and update learning memory with UNIVERSAL methodologies only"""
        thread_safe_print(f"\n{CYAN}{ARROW}{RESET} Extracting universal insights...")

        # Collect all learning extractions
        learning_files = []
        for section in sections:
            learning_file = Path(self.file_manager.run_dir) / f"section_{section['number']}" / "step5_learning.json"
            if learning_file.exists():
                learning_files.append(learning_file.read_text(encoding='utf-8'))

        if not learning_files:
            thread_safe_print(f"{YELLOW}{WARNING}{RESET} No learning extractions found")
            return

        combined_learning = "\n\n".join(learning_files)

        # Synthesize analytical principles from learnings
        prompt = f"""Synthesize analytical principles from these section learnings that help sharpen and deepen company analysis.

INDIVIDUAL SECTION LEARNINGS:
{combined_learning}

CURRENT MEMORY STATS:
{json.dumps(self.insight_memory.get_memory_stats(), indent=2)}

Your task: Extract analytical PRINCIPLES that apply to ANY company - NOT vague wisdom, NOT just red flags, NOT calculation procedures.

WHAT TO EXTRACT:

Analytical principles that guide HOW to analyze more deeply:
- **Comparative techniques** - what to compare against what to reveal hidden truths
- **Decomposition approaches** - how to break down aggregates to expose real drivers
- **Verification methods** - how to test claims using different data sources
- **Relationship patterns** - what metrics or trends to correlate for deeper insight

GOOD EXAMPLES (analytical principles):
- "Compare stated strategy against actual capital allocation to reveal true management priorities"
- "Decompose aggregate growth into organic versus inorganic components to assess core business health"
- "Calculate implied operational metrics from management claims to test their plausibility"
- "Trace revenue recognition through to cash collection to verify business quality"
- "Compare segment economics to corporate average to identify where value is actually created"
- "Map capital deployment to subsequent margin changes to evaluate management effectiveness"

BAD EXAMPLES (too vague or too specific):
- "To see the future, analyze the growth segments" (too vague)
- "Revenue concentration above 30% means customer controls pricing" (specific red flag, not analytical principle)
- "Actions speak louder than words" (corporate poetry)

CRITICAL REQUIREMENT:
You MUST extract principles from ALL 4 SECTIONS (1, 2, 3, 4) separately.
Extract up to 6 principles per section.
DO NOT skip Section 4 - it is as important as the other sections.

QUALITY DISTRIBUTION GUIDANCE:
- 9-10/10: Analytical approaches that consistently reveal material insights across companies
- 7-8/10: Solid principles that meaningfully deepen analysis
- 6/10: Standard but useful analytical techniques

OUTPUT FORMAT:
NEW_INSIGHTS:

[Section 1 - Company Overview principles]
- instruction: "[analytical principle in 12-20 words]"
  section_number: 1
  quality_score: [6-10]
...

[Section 2 - Competitive Positioning principles]
- instruction: "[analytical principle in 12-20 words]"
  section_number: 2
  quality_score: [6-10]
...

[Section 3 - Financial KPIs principles]
- instruction: "[analytical principle in 12-20 words]"
  section_number: 3
  quality_score: [6-10]
...

[Section 4 - Strategic Considerations principles]
- instruction: "[analytical principle in 12-20 words]"
  section_number: 4
  quality_score: [6-10]
...

Generate comprehensive principle candidates for ALL 4 SECTIONS - subsequent harsh filtering will select only the best (9-10/10 only).
Up to 6 principles per section. Focus on analytical approaches that sharpen and deepen analysis."""

        new_insights_text = retry_with_backoff(
            lambda: self.model_low_temp.generate_content(prompt).text,
            context="memory_review"
        )

        self.file_manager.save_memory_state({"new_insights": new_insights_text}, "new_insights.txt")

        # Apply memory updates
        try:
            # Archive current memory
            self.file_manager.archive_memory(
                self.insight_memory.get_memory_data(),
                memory_prefix="opp"
            )

            # Process and add new insights (quality filtering happens here)
            self.insight_memory.process_new_insights(new_insights_text)

            # Clean up and diversify memory
            self.insight_memory.cleanup_and_diversify()

            # Update metadata
            self.insight_memory.update_metadata()

            # Save updated memory
            self.insight_memory.save_memory()

            # Save post-run memory state
            self.file_manager.save_memory_state(
                self.insight_memory.get_memory_data(),
                "post_run_memory.json"
            )

            # Show memory stats
            memory_stats = self.insight_memory.get_memory_stats()
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Learning complete: {memory_stats['total_insights']} universal insights")

        except Exception as e:
            thread_safe_print(f"{YELLOW}{WARNING}{RESET} Memory update failed: {e}")

    def save_run_log(self, company_name: str, status: str = "Success", pptx_path: str = None):
        """Save run log to the run directory"""
        log_path = Path(self.file_manager.run_dir) / "run_log.txt"

        pptx_info = f"\n  - PowerPoint: {pptx_path}" if pptx_path else ""

        log_content = f"""OnePageProfile Run Log
{'='*60}

Timestamp: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}
Model: {self.model_name}
Workers: {self.workers}
Company: {company_name}
Run Directory: {self.file_manager.run_dir}

Source Files:
{chr(10).join(f"  - {Path(pdf).name}" for pdf in self.pdf_files)}

Processing:
  - Title/Subtitle generation
  - Phase 1: 4 sections processed in parallel (Steps 1-4: Draft/Check/Enhance/Polish)
  - PowerPoint generated after Phase 1
  - Phase 2: Learning extraction for universal methodologies (background)
  - Memory review and synthesis
  - See section_N/ subdirectories for intermediate outputs{pptx_info}

Status: {status}
"""

        with open(log_path, 'w', encoding='utf-8') as f:
            f.write(log_content)

    def run(self) -> Optional[Path]:
        """Execute the full profile generation pipeline with parallel section processing"""
        try:
            # Prepare PDFs once (will be reused by all workers)
            self.pdf_parts = self.prepare_pdf_parts()

            # Extract company name
            company_name = self.extract_company_name(self.pdf_parts)

            # Generate title and subtitle
            title_subtitle = self.generate_title_subtitle(company_name)

            # Create worker display
            worker_display = WorkerDisplay(self.workers)

            # Generate all sections in parallel
            section_content = self.generate_profile(company_name, worker_display)

            # Assemble final markdown
            final_profile = self._assemble_final_markdown(title_subtitle, section_content)

            # Save final profile
            final_path = Path(self.file_manager.run_dir) / "final_profile.md"
            final_path.write_text(final_profile, encoding='utf-8')

            thread_safe_print(f"\n{CYAN}{CHECK}{RESET} Content generation complete")

            # Generate PowerPoint (deliver to user ASAP)
            thread_safe_print(f"\n{CYAN}{ARROW}{RESET} Generating PowerPoint...")

            try:
                pptx_path = create_profile_pptx(
                    md_path=str(final_path),
                    company_name=company_name,
                    timestamp=self.timestamp
                )
                thread_safe_print(f"{CYAN}{CHECK}{RESET} PowerPoint: {pptx_path}")
            except Exception as e:
                thread_safe_print(f"{YELLOW}{WARNING}{RESET} PowerPoint generation failed: {e}")
                pptx_path = None

            # Phase 2: Learning extraction (background work)
            thread_safe_print(f"\n{CYAN}Phase 2: Extracting learnings (background)...{RESET}")

            with ThreadPoolExecutor(max_workers=self.workers) as executor:
                futures = [
                    executor.submit(self.process_section_learn, section)
                    for section in sections
                ]
                # Wait for all to complete
                for future in as_completed(futures):
                    future.result()  # Silent - just wait for completion

            # Conduct memory review (synthesize universal learnings)
            self._conduct_memory_review()

            # Save run log
            self.save_run_log(company_name, "Success", pptx_path)

            # Display summary
            thread_safe_print(f"\n{CYAN}{'='*60}{RESET}")
            thread_safe_print(f"{CYAN}{BOLD}Profile generation complete!{RESET}")
            thread_safe_print(f"{CYAN}{'='*60}{RESET}")
            thread_safe_print(f"\nOutput saved to: {self.file_manager.run_dir}/")
            thread_safe_print(f"  - final_profile.md")
            thread_safe_print(f"  - section_1/ through section_4/ (intermediate steps)")

            if pptx_path:
                thread_safe_print(f"\n{CYAN}Final deliverable:{RESET}")
                thread_safe_print(f"  - {pptx_path}")

            return final_path

        except Exception as e:
            thread_safe_print(f"{RED}{CROSS}{RESET} Error during profile generation: {e}")
            self.save_run_log(company_name if 'company_name' in locals() else "Unknown", f"Failed: {e}")
            return None


if __name__ == "__main__":
    # Clear terminal screen
    print("\033[2J\033[H", end='')

    thread_safe_print(f"ONEPAGEPROFILE {__opp_version__}")
    thread_safe_print("="*60)

    # Pre-flight checks
    thread_safe_print("\nSystem Check")
    thread_safe_print("="*60)

    # Check API key
    if not os.environ.get("GEMINI_API_KEY"):
        thread_safe_print(f"{RED}ERROR:{RESET} GEMINI_API_KEY not configured")
        thread_safe_print("Please set your API key in .env file:")
        thread_safe_print("  echo 'GEMINI_API_KEY=your-key-here' > .env")
        sys.exit(1)
    thread_safe_print(f"{CYAN}{CHECK}{RESET} API key configured")

    # Check runs directory
    os.makedirs("runs", exist_ok=True)
    thread_safe_print(f"{CYAN}{CHECK}{RESET} Output directory ready")
    thread_safe_print("="*60 + "\n")

    # Model selection
    thread_safe_print("Select LLM model:")
    thread_safe_print("  1) gemini-2.5-flash")
    thread_safe_print("  2) gemini-2.5-pro")
    choice = prompt_single_digit("Choose model [1/2] (default 1): ", valid_digits="12", default_digit="1")
    selected_model = 'gemini-2.5-flash' if choice == "1" else 'gemini-2.5-pro'
    thread_safe_print(f"{CYAN}{CHECK}{RESET} Selected: {selected_model}\n")

    # Worker selection
    thread_safe_print("Select number of parallel workers:")
    thread_safe_print("  1-4 workers (default 2)")
    workers_choice = prompt_single_digit("Choose workers [1-4] (default 2): ", valid_digits="1234", default_digit="2")
    num_workers = int(workers_choice)
    thread_safe_print(f"{CYAN}{CHECK}{RESET} Workers: {num_workers}\n")

    # File selection
    pdf_files = select_pdf_files()
    if not pdf_files:
        thread_safe_print(f"{RED}{CROSS}{RESET} No files selected. Exiting.")
        sys.exit(1)

    thread_safe_print(f"\n{CYAN}Starting profile generation...{RESET}")
    thread_safe_print("="*60 + "\n")

    # Generate profile
    maker = OnePageProfile(pdf_files, selected_model, workers=num_workers)
    profile_path = maker.run()

    if not profile_path:
        thread_safe_print(f"\n{RED}Profile generation failed.{RESET}")
        sys.exit(1)

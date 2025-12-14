import os
import json
import hashlib
import shutil
import google.generativeai as genai
from typing import List, Dict, Optional, Tuple
import sys
try:
    import termios
    import tty
except ImportError:
    # Unix terminal control not available (likely Windows)
    termios = None
    tty = None
try:
    import msvcrt  # Windows
except ImportError:
    # Windows msvcrt not available (likely Unix/Mac)
    msvcrt = None
from dotenv import load_dotenv
from datetime import datetime, timedelta
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed
import time
import threading

# Load environment variables
load_dotenv()

# Configure Gemini
genai.configure(api_key=os.environ.get("GEMINI_API_KEY"))

# Import modular components
from src import CoreAnalyzer, InsightMemory, QualityTracker, FileManager, ProfileGenerator, sections, __version__

# Import thread_safe_print and retry_with_backoff from utils
from src.utils import thread_safe_print, retry_with_backoff

# ANSI Color Codes and Styling
RESET = '\033[0m'
BOLD = '\033[1m'
DIM = '\033[2m'

# Colors (minimal elegant scheme)
RED = '\033[91m'      # Errors only
CYAN = '\033[96m'     # Success, progress (light blue)
WHITE = '\033[97m'    # Base color for everything else

# Symbols
CHECK = '✓'
ARROW = '→'
WARNING = '⚠'
CROSS = '✗'


class WorkerDisplay:
    """Thread-safe display manager for parallel worker status"""

    def __init__(self, num_workers):
        self.num_workers = num_workers
        self.worker_status = {}  # {worker_id: (section_num, action)}
        self.lock = threading.Lock()
        self.next_worker_id = 1
        self.worker_ids = {}  # {section_num: worker_id}

    def update(self, section_num, action):
        """Update a worker's status and redraw the line

        Args:
            section_num: Section number being processed
            action: One of "Draft", "Refine", "Polish"
        """
        with self.lock:
            # Assign worker ID if this is a new section
            if section_num not in self.worker_ids:
                self.worker_ids[section_num] = self.next_worker_id
                self.next_worker_id += 1

            worker_id = self.worker_ids[section_num]
            self.worker_status[worker_id] = (section_num, action)
            self._redraw()

    def complete(self, section_num, completed, total):
        """Mark a section as complete (silent - no output)"""
        with self.lock:
            # Remove from active workers
            if section_num in self.worker_ids:
                worker_id = self.worker_ids[section_num]
                if worker_id in self.worker_status:
                    del self.worker_status[worker_id]

            # Silent completion - no progress output needed

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
        print(f"\r{line}{RESET}{'':80}", end='', flush=True)


class IntelligentAnalyst:
    """Lightweight orchestrator for the intelligent document analysis system"""

    def __init__(self, source_files: dict, model_name: str = 'gemini-2.5-flash'):
        """Initialize ProfileDash with modular components

        Args:
            source_files: Dict with 'pdf_files' list
            model_name: Name of the Gemini model to use
        """
        # Store model name
        self.model_name = model_name

        # Generate run timestamp
        self.run_timestamp = datetime.now().strftime('%Y_%m_%d_%H_%M_%S')

        # Initialize file manager first
        self.file_manager = FileManager(self.run_timestamp)

        # Setup directories and ensure memory file exists (before file processing)
        self.file_manager.setup_directories(sections)

        # Initialize caching variables
        self.uploaded_files = []              # Track uploaded files for cleanup
        self.pdf_parts = []                   # File references for cache
        self.cache = None                     # CachedContent object
        self.cached_model_low_temp = None     # Cached model (temp 0.2)
        self.cached_model_medium_temp = None  # Cached model (temp 0.6)
        self.cached_model_high_temp = None    # Cached model (temp 0.9)

        # Upload PDFs to Files API
        pdf_files = source_files.get('pdf_files', [])
        if not pdf_files:
            raise Exception("No PDF files provided")

        thread_safe_print(f"Uploading {len(pdf_files)} PDF file(s) to Gemini Files API...")
        self.pdf_parts = self.upload_pdfs_to_files_api(pdf_files)

        if not self.pdf_parts:
            raise Exception("No files were successfully uploaded")

        thread_safe_print(f"Total files uploaded: {len(self.pdf_parts)}")

        # Create cache with uploaded PDFs
        self.create_cache()

        # Initialize CoreAnalyzer with cached models and pdf_parts for fallback
        self.core_analyzer = CoreAnalyzer(
            run_timestamp=self.run_timestamp,
            model_name=model_name,
            cached_model_low=self.cached_model_low_temp,
            cached_model_medium=self.cached_model_medium_temp,
            cached_model_high=self.cached_model_high_temp,
            pdf_parts=self.pdf_parts
        )

        # Initialize other components
        self.insight_memory = InsightMemory(self.run_timestamp, model_name=model_name, memory_prefix="pd2")
        self.quality_tracker = QualityTracker()

        # Save pre-run memory state
        self.file_manager.save_memory_state(
            self.insight_memory.get_memory_data(),
            "pre_run_memory.json"
        )

    def upload_pdfs_to_files_api(self, pdf_files: List[str]) -> List:
        """Upload multiple PDFs to Gemini Files API

        Args:
            pdf_files: List of paths to PDF files

        Returns:
            List of uploaded file references
        """
        parts = []
        for pdf_path in pdf_files:
            pdf_filename = Path(pdf_path).name
            thread_safe_print(f"{CYAN}{ARROW}{RESET} Uploading {pdf_filename}...")

            try:
                uploaded_file = genai.upload_file(pdf_path)
                self.uploaded_files.append(uploaded_file)

                # Wait for processing
                while uploaded_file.state.name == 'PROCESSING':
                    time.sleep(2)
                    uploaded_file = genai.get_file(uploaded_file.name)

                if uploaded_file.state.name == 'FAILED':
                    thread_safe_print(f"{RED}{CROSS}{RESET} File processing failed: {pdf_filename}")
                    continue

                thread_safe_print(f"{CYAN}{CHECK}{RESET} {pdf_filename} ready")
                parts.append(uploaded_file)

            except Exception as e:
                thread_safe_print(f"{RED}{CROSS}{RESET} Upload failed for {pdf_filename}: {e}")

        return parts

    def create_cache(self) -> bool:
        """Create cache with all uploaded PDFs

        Returns:
            bool: True if cache created successfully, False otherwise
        """
        try:
            ttl = timedelta(hours=3)  # 3 hours default
            thread_safe_print(f"{CYAN}{ARROW}{RESET} Creating cache (TTL: 180 minutes)...")

            self.cache = genai.caching.CachedContent.create(
                model=self.model_name,
                contents=self.pdf_parts,
                ttl=ttl
            )

            # Create cached models for all three temperatures
            self.cached_model_low_temp = genai.GenerativeModel.from_cached_content(
                cached_content=self.cache,
                generation_config=genai.types.GenerationConfig(temperature=0.2)
            )
            self.cached_model_medium_temp = genai.GenerativeModel.from_cached_content(
                cached_content=self.cache,
                generation_config=genai.types.GenerationConfig(temperature=0.6)
            )
            self.cached_model_high_temp = genai.GenerativeModel.from_cached_content(
                cached_content=self.cache,
                generation_config=genai.types.GenerationConfig(temperature=0.9)
            )

            thread_safe_print(f"{CYAN}{CHECK}{RESET} Cache created (name: {self.cache.name})")
            return True

        except Exception as e:
            thread_safe_print(f"{WARNING} Cache creation failed: {e}")
            thread_safe_print(f"{CYAN}{ARROW}{RESET} Continuing without cache (will use Files API directly)")
            self.cache = None
            self.cached_model_low_temp = None
            self.cached_model_medium_temp = None
            self.cached_model_high_temp = None
            return False

    def cleanup_cache(self):
        """Delete cache from Gemini API"""
        if self.cache:
            try:
                thread_safe_print(f"{CYAN}{ARROW}{RESET} Deleting cache {self.cache.name}...")
                self.cache.delete()
                thread_safe_print(f"{CYAN}{CHECK}{RESET} Cache deleted")
            except Exception as e:
                thread_safe_print(f"{WARNING} Failed to delete cache: {e}")

            self.cache = None
            self.cached_model_low_temp = None
            self.cached_model_medium_temp = None
            self.cached_model_high_temp = None

    def cleanup_uploaded_files(self):
        """Delete uploaded files from Gemini Files API"""
        if self.uploaded_files:
            thread_safe_print(f"{CYAN}{ARROW}{RESET} Cleaning up {len(self.uploaded_files)} uploaded file(s)...")
            for uploaded_file in self.uploaded_files:
                try:
                    genai.delete_file(uploaded_file.name)
                except Exception as e:
                    # Don't fail cleanup on individual file errors
                    pass
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Uploaded files cleaned up")
            self.uploaded_files = []

    def _extract_company_name(self) -> str:
        """Extract company name from cached documents"""
        prompt = """Extract the primary company name from these documents.

Look for the main company being analyzed. This could be in:
- Document titles
- Headers and letterheads
- First mentions in the text

Output ONLY the company name, nothing else. No explanation, no punctuation."""

        try:
            if self.cached_model_low_temp:
                result = retry_with_backoff(
                    lambda: self.cached_model_low_temp.generate_content([prompt]).text.strip(),
                    context="Company name extraction"
                )
            else:
                # Fallback to Files API
                model = genai.GenerativeModel(
                    self.model_name,
                    generation_config=genai.types.GenerationConfig(temperature=0.2)
                )
                result = retry_with_backoff(
                    lambda: model.generate_content(self.pdf_parts + [prompt]).text.strip(),
                    context="Company name extraction"
                )
            return result if result else "Unknown Company"
        except Exception as e:
            thread_safe_print(f"{WARNING} Company name extraction failed: {e}")
            return "Unknown Company"

    def analyze_section(self, section_num: int) -> str:
        """Enhanced 5-step analysis pipeline with completeness and deep analysis."""
        section = next(s for s in sections if s['number'] == section_num)

        try:
            # Get relevant memory for this section
            relevant_memory = self.insight_memory.get_relevant_memory(section_num)

            # Special handling for Section 33: Financial Pattern Analysis
            # Uses a custom 4-layer hypothesis-driven pipeline (16 API calls)
            if section_num == 33:
                # Extract company name using cached model
                company_name = self._extract_company_name()

                # Run the 4-layer pipeline
                worker_display = getattr(self, 'worker_display', None)
                final_output = self.core_analyzer.analyze_section_33(
                    company_name,
                    self.file_manager,
                    worker_display=worker_display
                )

                # Calculate quality metrics for the section
                self.quality_tracker.calculate_section_metrics(section_num, final_output)
                return f"Section {section_num} completed."

            # Step 1: Initial Draft
            if hasattr(self, 'worker_display') and self.worker_display:
                self.worker_display.update(section_num, "Draft")
            else:
                thread_safe_print(f"Section {section_num} → Drafting...")
            initial_draft = self.core_analyzer.create_initial_draft(section, relevant_memory)
            self.file_manager.save_step_output(section_num, "step_1_initial_draft.md", initial_draft)

            # Failsafe: Retry Step 1 once if empty or too short
            def _is_empty(text: str, minimum: int = 100) -> bool:
                return not text or len(text.strip()) < minimum

            if _is_empty(initial_draft):
                thread_safe_print(f"Section {section_num} ⚠ Retrying draft...")
                retry_draft = self.core_analyzer.create_initial_draft(section, relevant_memory)
                if not _is_empty(retry_draft):
                    initial_draft = retry_draft
                # Save retry attempt separately for diagnostics
                self.file_manager.save_step_output(section_num, "step_1_initial_draft_retry.md", retry_draft or "")

            # Initialize variables to avoid UnboundLocalError
            improved_draft = None
            step4_output = None

            # For Section 34 (Data Book), the initial draft is the final output. No critiques needed.
            if section['number'] == self.core_analyzer.SECTION_32_EXEMPT:
                final_output = initial_draft
                self.file_manager.save_step_output(section_num, "step_4_final_section.md", final_output)
            else:
                # Step 2: Completeness Check
                if hasattr(self, 'worker_display') and self.worker_display:
                    self.worker_display.update(section_num, "Refine")
                else:
                    thread_safe_print(f"Section {section_num} → Refining...")
                add_list = self.core_analyzer.completeness_check(section, initial_draft)
                self.file_manager.save_step_output(section_num, "step_2_completeness_check.txt", add_list)

                # Step 3: Apply Completeness Changes
                improved_draft = self.core_analyzer.apply_completeness_only(section, initial_draft, add_list)
                self.file_manager.save_step_output(section_num, "step_3_improved_draft.md", improved_draft)

                # Step 4: Deep Analysis and Polish
                if hasattr(self, 'worker_display') and self.worker_display:
                    self.worker_display.update(section_num, "Polish")
                else:
                    thread_safe_print(f"Section {section_num} → Polishing...")
                step4_output = self.core_analyzer.deep_analysis_and_polish(section, improved_draft)
                self.file_manager.save_step_output(section_num, "step_4_final_section.md", step4_output)

                final_output = step4_output

            # Final failsafe: choose the last non-empty among outputs
            if section['number'] != self.core_analyzer.SECTION_32_EXEMPT:
                candidates = [final_output]
                if step4_output is not None:
                    candidates.append(step4_output)
                if improved_draft is not None:
                    candidates.append(improved_draft)
                candidates.append(initial_draft)
                final_chosen = next((c for c in candidates if c and len(c.strip()) >= 200), None)
                if final_chosen is None:
                    thread_safe_print(f"Section {section_num} ⚠ Empty output, using placeholder")
                    final_output = f"_This section failed to generate content during this run._"
                    # Overwrite final file with placeholder to avoid blank HTML
                    self.file_manager.save_step_output(section_num, "step_4_final_section.md", final_output)
                else:
                    final_output = final_chosen
            else:
                # Section 34 (Data Book) special-case placeholder if empty
                if _is_empty(final_output, minimum=200):
                    thread_safe_print(f"Section {section_num} ⚠ Appendix empty, using placeholder")
                    final_output = "_Appendix could not be generated from the provided documents in this run._"
                    self.file_manager.save_step_output(section_num, "step_4_final_section.md", final_output)

            # Step 6: Learning Extraction - DISABLED
            # Learning extraction has been disabled. Code retained for potential future use.
            # if section['number'] != self.core_analyzer.SECTION_32_EXEMPT:
            #     learning = self.core_analyzer.extract_learning(section, final_output)
            #     learning_str = json.dumps(learning, indent=4)
            #     self.file_manager.save_step_output(section_num, "step_6_learning.json", learning_str)

            # Calculate quality metrics for the section
            self.quality_tracker.calculate_section_metrics(section_num, final_output)
            return f"Section {section_num} completed."

        except Exception as e:
            thread_safe_print(f"Section {section_num} ⚠ Error: {e}")
            # Optionally re-raise or handle as per overall error strategy
            return f"Section {section_num} failed."

    def process_all_sections(self, section_numbers: List[int] = None, max_workers: int = 3):
        """Process multiple sections with three-phase scheduling (Section 34 Data Book deferred)."""
        if section_numbers is None:
            section_numbers = [s['number'] for s in sections]

        results: Dict[int, str] = {}

        # Extract company name once at start (using cached model)
        company_name = self._extract_company_name()

        # Determine worker caps from env (tunable)
        try:
            env_cap = int(os.environ.get("MAX_SECTION_WORKERS", "3"))
            env_cap = 1 if env_cap < 1 else (8 if env_cap > 8 else env_cap)
        except (ValueError, TypeError):
            thread_safe_print("Warning: Invalid MAX_SECTION_WORKERS value, using default of 3")
            env_cap = 3
        actual_workers = min(max_workers, len(section_numbers), env_cap)
        try:
            stagger = float(os.environ.get("SUBMISSION_STAGGER_SEC", "0.5"))
        except (ValueError, TypeError):
            thread_safe_print("Warning: Invalid SUBMISSION_STAGGER_SEC value, using default of 0.5")
            stagger = 0.5

        def _run_parallel(sec_list: List[int]) -> Dict[int, str]:
            local: Dict[int, str] = {}
            if not sec_list:
                return local
            if actual_workers > 1:
                # Initialize worker display for parallel processing
                self.worker_display = WorkerDisplay(actual_workers)
                thread_safe_print(f"Processing {len(sec_list)} sections with {actual_workers} parallel workers...")
                with ThreadPoolExecutor(max_workers=actual_workers) as executor:
                    future_map = {}
                    for i, s_num in enumerate(sec_list):
                        future_map[executor.submit(self.analyze_section, s_num)] = s_num
                        if i < len(sec_list) - 1:
                            time.sleep(stagger)
                    completed = 0
                    for fut in as_completed(future_map):
                        s_num = future_map[fut]
                        try:
                            res = fut.result()
                            local[s_num] = res
                            completed += 1
                            self.worker_display.complete(s_num, completed, len(sec_list))
                        except Exception as e:
                            err = str(e)
                            if "429" in err or "quota" in err.lower():
                                thread_safe_print(f"Section {s_num} ⚠ Rate limit: {e}")
                            else:
                                thread_safe_print(f"Section {s_num} ⚠ Error: {e}")
                            local[s_num] = f"Processing failed: {e}"
            else:
                # Disable worker display for single-worker mode
                self.worker_display = None
                for s_num in sec_list:
                    try:
                        local[s_num] = self.analyze_section(s_num)
                    except Exception as e:
                        thread_safe_print(f"Section {s_num} ⚠ Error: {e}")
                        local[s_num] = f"Processing failed: {e}"
            return local

        try:
            # Special sections that run in their own phases
            SECTION_33_PATTERN_ANALYSIS = 33
            SECTION_34_DATA_BOOK = 34
            special_sections = {SECTION_33_PATTERN_ANALYSIS, SECTION_34_DATA_BOOK}

            # Phase 1: run all sections except 33 and 34
            regular_sections = [n for n in section_numbers if n not in special_sections]
            has33 = SECTION_33_PATTERN_ANALYSIS in section_numbers
            has34 = SECTION_34_DATA_BOOK in section_numbers
            phase1 = _run_parallel(regular_sections)
            results.update(phase1)

            # Save quality metrics and run summary for Phase 1
            quality_scores = self.quality_tracker.get_quality_scores()
            run_number = self.insight_memory.learning_memory["meta"]["total_runs"] + 1
            self.file_manager.save_quality_metrics(quality_scores, run_number)
            self._generate_run_summary(results)

            # Generate profile with Phase 1 results
            thread_safe_print(f"\n{'='*60}")
            thread_safe_print(f"{BOLD}Generating Profile{RESET}")
            thread_safe_print(f"{'='*60}")
            try:
                profile_generator = ProfileGenerator(self.run_timestamp, model_name=self.core_analyzer.model_name)
                profile_generator.generate_html_profile(results, regular_sections, company_name, sections)
                thread_safe_print(f"{CYAN}{CHECK}{RESET} Profile ready")
            except Exception as e:
                thread_safe_print(f"{WARNING} Profile generation failed: {e}")

            # Phase 2: run Section 33 (Financial Pattern Analysis) if selected
            if has33:
                thread_safe_print(f"\n{'='*60}")
                thread_safe_print(f"{BOLD}Generating Financial Pattern Analysis (Section 33){RESET}")
                thread_safe_print(f"{'='*60}")
                try:
                    res33 = self.analyze_section(SECTION_33_PATTERN_ANALYSIS)
                    results[SECTION_33_PATTERN_ANALYSIS] = res33
                except Exception as e:
                    thread_safe_print(f"{WARNING} Pattern analysis generation failed: {e}")
                # Regenerate profile with Section 33 included
                try:
                    profile_generator = ProfileGenerator(self.run_timestamp, model_name=self.core_analyzer.model_name)
                    sections_so_far = regular_sections + [SECTION_33_PATTERN_ANALYSIS]
                    profile_generator.generate_html_profile(results, sections_so_far, company_name, sections)
                    thread_safe_print(f"{CYAN}{CHECK}{RESET} Pattern analysis complete - Profile updated")
                except Exception as e:
                    thread_safe_print(f"{WARNING} Profile update failed: {e}")

            # Phase 3: run Section 34 (Data Book) if selected
            if has34:
                thread_safe_print(f"\n{'='*60}")
                thread_safe_print(f"{BOLD}Generating Data Appendix (Section 34){RESET}")
                thread_safe_print(f"{'='*60}")
                try:
                    res34 = self.analyze_section(SECTION_34_DATA_BOOK)
                    results[SECTION_34_DATA_BOOK] = res34
                except Exception as e:
                    thread_safe_print(f"{WARNING} Appendix generation failed: {e}")
                # Regenerate profile with full set
                try:
                    profile_generator = ProfileGenerator(self.run_timestamp, model_name=self.core_analyzer.model_name)
                    profile_generator.generate_html_profile(results, section_numbers, company_name, sections)
                    thread_safe_print(f"{CYAN}{CHECK}{RESET} Appendix complete - Profile updated")
                except Exception as e:
                    thread_safe_print(f"{WARNING} Profile update failed: {e}")

            return results

        finally:
            # Cleanup cache and uploaded files
            thread_safe_print(f"\n{'='*60}")
            thread_safe_print(f"{BOLD}Cleanup{RESET}")
            thread_safe_print(f"{'='*60}")
            self.cleanup_cache()
            self.cleanup_uploaded_files()
    
    def _conduct_memory_review(self):
        """Review and update learning memory"""
        thread_safe_print(f"{ARROW} Extracting insights...")
        
        # Collect all learning extractions
        learning_files = []
        for section in sections:
            learning_file = f"runs/run_{self.run_timestamp}/section_{section['number']}/step_6_learning.json"
            if os.path.exists(learning_file):
                with open(learning_file, 'r', encoding='utf-8') as f:
                    learning_files.append(f.read())
        
        if not learning_files:
            thread_safe_print("No learning extractions found")
            return
        
        combined_learning = "\n\n".join(learning_files)
        
        # Synthesize analytical principles from learning extractions
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

For each principle, provide:
- instruction: "[analytical principle in 12-20 words]"
- section_number: [the section number this principle applies to]
- quality_score: [6-10, be realistic about distribution]

QUALITY DISTRIBUTION GUIDANCE:
- 9-10/10: Analytical approaches that consistently reveal material insights across companies
- 7-8/10: Solid principles that meaningfully deepen analysis
- 6/10: Standard but useful analytical techniques

OUTPUT FORMAT:
NEW_INSIGHTS:
- instruction: "[analytical principle in 12-20 words]"
  section_number: [section number]
  quality_score: [6-10, realistic distribution]

- instruction: "[another analytical principle in 12-20 words]"
  section_number: [section number]
  quality_score: [6-10, realistic distribution]

Generate comprehensive principle candidates - subsequent harsh filtering will select only the best (9-10/10 only).
Focus on analytical approaches that sharpen and deepen analysis.
"""
        
        model = genai.GenerativeModel(self.core_analyzer.model_name)
        new_insights_text = retry_with_backoff(
            lambda: model.generate_content(prompt).text
        )
        
        self.file_manager.save_memory_state({"new_insights": new_insights_text}, "new_insights.txt")
        
        # Apply memory updates
        try:
            # Archive current memory
            archive_path = self.file_manager.archive_memory(
                self.insight_memory.get_memory_data(),
                memory_prefix="pd2"
            )
            # Silent archival - no output

            # Process and add new insights
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

            # Show memory stats after cleanup
            memory_stats = self.insight_memory.get_memory_stats()
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Memory updated: {BOLD}{memory_stats['total_insights']}{RESET}/{memory_stats['max_possible']} insights ({memory_stats['utilization_percent']}%)")
        except Exception as e:
            thread_safe_print(f"{WARNING} Memory update failed: {e}")
    
    def _generate_run_summary(self, results: Dict):
        """Generates a final summary markdown file only (HTML handled by ProfileGenerator)."""
        thread_safe_print("\n" + "="*20 + " Generating Final Run Summary " + "="*20)
        
        run_dir = Path(f"runs/run_{self.run_timestamp}")
        summary_path = run_dir / "run_summary.md"
        
        # Create simple markdown summary
        summary_content = f"# Run Summary\n\n"
        summary_content += f"**Run ID:** {self.run_timestamp}\n"
        summary_content += f"**Date:** {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n"
        
        # Collect section results
        completed_sections = []
        failed_sections = []

        # Check which sections were actually analyzed in this run
        from src.profile_sections import sections as all_sections
        for section_info in all_sections:
            section_num = section_info['number']
            section_title = section_info['title']
            
            if section_num in results:
                summary_content += f"- **Section {section_num}: {section_title}** - COMPLETED\n"
                completed_sections.append(section_num)
            else:
                summary_content += f"- **Section {section_num}: {section_title}** - SKIPPED\n"
        
        # Overall summary statistics
        summary_content += f"\n---\n"
        summary_content += f"**Sections Analyzed:** {len(completed_sections)}\n"
        summary_content += f"**Sections Completed:** {completed_sections}\n"
        
        # Save summary
        with open(summary_path, 'w', encoding='utf-8') as f:
            f.write(summary_content)
        
        thread_safe_print(f"Run summary saved to: {summary_path}")


# Section Groups Configuration
SECTION_GROUPS = {
    "Company Profile": {
        "sections": list(range(1, 14)),  # 1-13 (now includes Deep Dive Discoveries)
        "prompt": "1. Company profile (sections 1-13) (y/n): "
    },
    "Strategy and SWOT": {
        "sections": list(range(14, 20)),  # 14-19
        "prompt": "2. Strategy and SWOT (sections 14-19) (y/n): "
    },
    "Sellside Positioning": {
        "sections": list(range(20, 27)),  # 20-26
        "prompt": "3. Sellside Positioning (sections 20-26) (y/n): "
    },
    "Buyside Due Diligence": {
        "sections": list(range(27, 33)),  # 27-32
        "prompt": "4. Buyside Due Diligence (sections 27-32) (y/n): "
    },
    "Financial Pattern Analysis": {
        "sections": [33],
        "prompt": "5. Financial Pattern Analysis (section 33) (y/n): "
    },
    "Data Book": {
        "sections": [34],
        "prompt": "6. Data Book (section 34) (y/n): "
    }
}

# PDF Selection Functions
def _read_single_key() -> str:
    """Read a single keypress without requiring Enter. Cross-platform best effort."""
    if msvcrt is not None:
        ch = msvcrt.getch()
        try:
            return ch.decode('utf-8', errors='ignore')
        except Exception:
            return str(ch)
    # POSIX
    if termios is None or tty is None:
        # Fallback: read a full line
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
    """Display a (y/n) prompt that accepts a single key without Enter. Returns True for yes."""
    while True:
        thread_safe_print(f"{prompt_text}", end='', flush=True)
        ch = _read_single_key()
        thread_safe_print(ch)
        if not ch:
            continue
        ch = ch.strip().lower()
        if ch in ('y', 'n'):
            return ch == 'y'
        # Re-prompt on any other key

def prompt_single_digit(prompt_text: str, valid_digits: str, default_digit: str) -> str:
    """Display a numeric prompt that accepts a single key without Enter.
    Returns the chosen digit as a string; falls back to default_digit on Enter/blank.
    """
    while True:
        thread_safe_print(f"{prompt_text}", end='', flush=True)
        ch = _read_single_key()
        # Echo selection for user feedback
        thread_safe_print(ch)
        if not ch or ch in ('\r', '\n'):
            return default_digit
        ch = ch.strip()
        if ch in valid_digits:
            return ch
        # Re-prompt on any other key
def select_pdf_files():
    """Interactively select PDF files for analysis"""
    from tkinter import filedialog
    import tkinter as tk

    while True:
        root = tk.Tk()
        root.withdraw()

        thread_safe_print("\nSelect PDF files for analysis...")
        source_files = filedialog.askopenfilenames(
            title="Select PDF Files for Analysis",
            filetypes=[
                ("PDF files", "*.pdf"),
                ("All files", "*.*")
            ]
        )

        root.destroy()

        if not source_files:
            thread_safe_print("No files selected.")
            retry_yes = prompt_yes_no("Would you like to try selecting files again? (y/n): ")
            if not retry_yes:
                return None
            continue

        # Filter for PDF files only
        pdf_files = [f for f in source_files if f.lower().endswith('.pdf')]
        other_files = [f for f in source_files if not f.lower().endswith('.pdf')]

        thread_safe_print(f"Selected {len(pdf_files)} PDF file(s):")
        for pdf in pdf_files:
            thread_safe_print(f"  - {Path(pdf).name}")

        if other_files:
            thread_safe_print(f"  Warning: {len(other_files)} non-PDF file(s) will be skipped")

        if not pdf_files:
            thread_safe_print("No PDF files found. Please select PDF files.")
            continue

        return {'pdf_files': pdf_files}

# Usage interface
if __name__ == "__main__":
    # Clear terminal screen
    print("\033[2J\033[H", end='')

    thread_safe_print(f"PROFILEDASH {__version__}")
    thread_safe_print("="*60)
    
    # Pre-flight checks
    thread_safe_print("\nSystem Check")
    thread_safe_print("="*60)

    # Check 1: Verify API key is set
    if not os.environ.get("GEMINI_API_KEY"):
        thread_safe_print(f"{RED}ERROR:{RESET} GEMINI_API_KEY not configured")
        thread_safe_print("Please set your API key in .env file:")
        thread_safe_print("  echo 'GEMINI_API_KEY=your-key-here' > .env")
        sys.exit(1)
    thread_safe_print(f"{CYAN}{CHECK}{RESET} API key configured")

    # Check 2: Verify WeasyPrint (PDF report generation)
    try:
        from weasyprint import HTML
        thread_safe_print(f"{CYAN}{CHECK}{RESET} PDF reports ready (WeasyPrint)")
    except ImportError:
        thread_safe_print(f"{WARNING} WeasyPrint not installed - HTML only")
        thread_safe_print(f"  {DIM}Install with: pip install weasyprint{RESET}")
        # Don't exit - HTML reports will still be generated

    # Check 3: Create base directories
    base_dirs = ["runs", "memory", "quality_metrics"]
    for dir_path in base_dirs:
        os.makedirs(dir_path, exist_ok=True)
    thread_safe_print(f"{CYAN}{CHECK}{RESET} Directories verified")
    thread_safe_print("="*60 + "\n")
    
    # Model selection
    thread_safe_print("Select LLM model:")
    thread_safe_print("  1) gemini-2.5-flash")
    thread_safe_print("  2) gemini-3-pro-preview")
    selected_model = None
    choice = prompt_single_digit("Choose model [1/2] (default 1): ", valid_digits="12", default_digit="1")
    if choice == "1":
        selected_model = 'gemini-2.5-flash'
    else:
        selected_model = 'gemini-3-pro-preview'

    # Optional LLM warm-up to reduce first-call latency (uses selected model)
    try:
        model = genai.GenerativeModel(selected_model, generation_config=genai.types.GenerationConfig(temperature=0.0))
        _ = retry_with_backoff(lambda: model.generate_content("ping").text)
        thread_safe_print(f"{CYAN}{CHECK}{RESET} LLM warm-up completed")
    except Exception as e:
        thread_safe_print(f"{WARNING} LLM warm-up skipped/failed: {e}")
    
    # Step 1: Select PDF files with retry capability
    source_file_selection = select_pdf_files()
    if not source_file_selection:
        thread_safe_print("No files selected. Exiting.")
        sys.exit(0)

    # Available sections for validation
    available_sections = [s['number'] for s in sections]

    # Step 2: Select analysis components
    thread_safe_print("\nSelect analysis components:")

    selected_sections = []
    selected_groups = []

    # Normal selection mode - go through all groups
    for group_name, group_info in SECTION_GROUPS.items():
        yn = prompt_yes_no(group_info["prompt"])
        if yn:
            # Validate that all sections in this group actually exist
            valid_group_sections = [s for s in group_info["sections"] if s in available_sections]
            if valid_group_sections:
                selected_sections.extend(valid_group_sections)
                selected_groups.append(group_name)
            else:
                thread_safe_print(f"Warning: No valid sections found for {group_name}")

    if not selected_sections:
        thread_safe_print("\nNo sections selected. Exiting.")
        sys.exit(0)

    # Remove duplicates and sort
    selected_sections = sorted(list(set(selected_sections)))

    thread_safe_print(f"\nSelected groups: {', '.join(selected_groups)}")
    thread_safe_print(f"Processing sections: {selected_sections}")

    # Step 3: Ask about number of workers for section processing
    thread_safe_print("\nAnalysis Settings")
    while True:
        env_cap = int(os.environ.get("MAX_SECTION_WORKERS", "3"))
        env_cap = 1 if env_cap < 1 else (8 if env_cap > 8 else env_cap)
        default_workers = min(2, env_cap)
        valid_digits = ''.join(str(i) for i in range(1, env_cap + 1))
        choice = prompt_single_digit(
            f"Parallel workers (1-{env_cap}, default {default_workers}): ",
            valid_digits=valid_digits,
            default_digit=str(default_workers)
        )
        max_workers = int(choice)
        if 1 <= max_workers <= env_cap:
            break
    
    # Step 4: Now initialize ProfileDash with source files (uploads to Files API)
    # Clear terminal before starting processing
    print("\033[2J\033[H", end='')

    thread_safe_print("="*60)
    thread_safe_print("STARTING PROCESSING")
    thread_safe_print("="*60)

    try:
        analyst = IntelligentAnalyst(source_file_selection, model_name=selected_model)
    except Exception as e:
        thread_safe_print(f"\nFailed to initialize analyst: {e}")
        thread_safe_print("Please check your files and try again.")
        sys.exit(0)
    
    thread_safe_print(f"Starting analysis with {'parallel' if max_workers > 1 else 'sequential'} processing...")
    
    results = analyst.process_all_sections(selected_sections, max_workers)
    
    thread_safe_print(f"\n{'='*60}")
    thread_safe_print("Analysis Complete")
    thread_safe_print(f"{'='*60}")
    thread_safe_print(f"Reports: ReportFiles/")
    thread_safe_print(f"Details: runs/run_{analyst.run_timestamp}/") 
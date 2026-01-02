import os
import google.generativeai as genai
from typing import List, Optional, Dict
import sys
from dotenv import load_dotenv
from datetime import datetime, timedelta
from pathlib import Path
from tkinter import filedialog
import tkinter as tk
from concurrent.futures import ThreadPoolExecutor, as_completed
import threading
import re
import json
import time

# Load environment variables
load_dotenv()

# Configure Gemini
genai.configure(api_key=os.environ.get("GEMINI_API_KEY"))

# Version
__opp_version__ = "1.5"

# Import utilities from PD2
from src.utils import thread_safe_print, retry_with_backoff
from src.opp_sections import sections, get_section_boundaries
from src.profile_prompts import (
    COMPANY_NAME_EXTRACTION_PROMPT,
    get_title_subtitle_prompt,
    get_subtitle_refinement_prompt,
    get_section_generation_prompt,
    get_section_completeness_check_prompt,
    get_section_enhancement_prompt,
    get_section_cleanup_prompt,
    get_section_polish_prompt,
    get_opp_ground_truth_prompt,
    get_opp_hypothesis_prompt,
    get_opp_test_prompt,
    get_opp_synthesis_prompt,
    get_opp_integration_prompt,
    get_opp_cleanup2_prompt,
    get_opp_polish2_prompt
)
from src.pptx_generator import create_profile_pptx
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

def prompt_yes_no(prompt_text: str, default: bool = False) -> bool:
    """Display a (y/n) prompt that accepts a single key. Enter selects default."""
    while True:
        thread_safe_print(f"{prompt_text}", end='', flush=True)
        ch = _read_single_key()
        if not ch:
            continue
        # Enter key returns default
        if ch in ('\r', '\n'):
            default_char = 'y' if default else 'n'
            thread_safe_print(default_char)
            return default
        thread_safe_print(ch)
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
            retry_yes = prompt_yes_no("Would you like to try selecting files again? (y/n, default y): ", default=True)
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


def scan_batch_directory() -> Optional[List[str]]:
    """Scan SourceFiles/SourceBatch/ for PDF files

    Returns:
        List of PDF file paths, or None if directory not found or empty
    """
    batch_dir = Path("SourceFiles/SourceBatch")

    if not batch_dir.exists():
        thread_safe_print(f"{RED}{CROSS}{RESET} Batch directory not found: {batch_dir}")
        return None

    pdfs = sorted(batch_dir.glob("*.pdf"))

    if not pdfs:
        thread_safe_print(f"{RED}{CROSS}{RESET} No PDF files found in {batch_dir}")
        return None

    thread_safe_print(f"\n{CYAN}{CHECK}{RESET} Found {len(pdfs)} PDF file(s) in batch directory:")
    for pdf in pdfs:
        thread_safe_print(f"  - {pdf.name}")

    return [str(pdf) for pdf in pdfs]


def print_batch_summary(results: List[dict]):
    """Print summary of batch processing results

    Args:
        results: List of dicts with 'file', 'status', 'time', and optionally 'output' or 'error'
    """
    thread_safe_print(f"\n\n{'='*60}")
    thread_safe_print("BATCH PROCESSING COMPLETE")
    thread_safe_print(f"{'='*60}")

    successes = [r for r in results if r["status"] == "success"]
    failures = [r for r in results if r["status"] in ("failed", "error")]

    thread_safe_print(f"\nTotal: {len(results)}")
    thread_safe_print(f"Success: {len(successes)}")
    thread_safe_print(f"Failed: {len(failures)}")

    if failures:
        thread_safe_print(f"\n{RED}Failed files:{RESET}")
        for r in failures:
            thread_safe_print(f"  - {r['file']}: {r.get('error', 'Unknown')}")

    total_time = sum(r["time"] for r in results)
    thread_safe_print(f"\nTotal time: {total_time/60:.1f} minutes")
    thread_safe_print(f"{'='*60}\n")


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
            action: Step label - one of:
                Steps 1-5: "Draft", "Check", "Enhance", "Clean-up", "Polish"
                Steps 6-12 (insights): "Ground", "Hypo", "Test", "Synth", "Integ", "Clean2", "Polish2"
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

            # Pad action with dots to align all status words (longest is "Enhance" at 7 chars)
            padded_action = action.ljust(7, '.')

            # Format with minimal styling: "Sec. 5 → Draft.."
            status = f"{DIM}Sec.{RESET} {BOLD}{sec_num}{RESET} {ARROW} {padded_action}"
            parts.append(status)

        # Join with separator
        line = f" {DIM}|{RESET} ".join(parts)

        thread_safe_print(line)


class OnePageProfile:
    """Generates one-page company profiles from PDF documents with parallel section processing"""

    def __init__(self, pdf_files: List[str], model_name: str, workers: int = 2,
                 insights_enabled: bool = False, research_context: str = None,
                 company_name: str = None, run_dir: Path = None):
        self.pdf_files = pdf_files
        self.model_name = model_name
        self.workers = workers
        self.insights_enabled = insights_enabled
        self.research_context = research_context
        self.provided_company_name = company_name
        self.provided_run_dir = run_dir

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

        # Use imported sections
        self.sections = sections
        self.get_section_boundaries = get_section_boundaries

        # PDF parts will be prepared once and reused
        self.pdf_parts = None
        self.uploaded_files = []  # Track uploaded files for cleanup

        # Caching support
        self.cache = None  # Cached content object
        self.cached_model_low_temp = None   # Cached model with temp 0.2
        self.cached_model_medium_temp = None  # Cached model with temp 0.6

        # Initialize file manager
        if self.provided_run_dir:
            # Use provided run directory (from Deep Research)
            self.file_manager = FileManager(self.timestamp, "opp")
            self.file_manager.run_dir = str(self.provided_run_dir)
        else:
            self.file_manager = FileManager(self.timestamp, "opp")

        # Setup directories
        self.file_manager.setup_directories(self.sections)

    def prepare_pdf_parts(self) -> List:
        """Upload PDF files to Gemini Files API and return file references"""
        parts = []

        # Handle research-only mode (no PDFs)
        if not self.pdf_files:
            thread_safe_print(f"{CYAN}{CHECK}{RESET} No PDFs to upload (research-only mode)")
            return parts

        for pdf_path in self.pdf_files:
            pdf_filename = Path(pdf_path).name
            thread_safe_print(f"{CYAN}{ARROW}{RESET} Uploading {pdf_filename}...")

            try:
                # Upload file using old SDK pattern
                uploaded_file = genai.upload_file(pdf_path)
                self.uploaded_files.append(uploaded_file)

                # Wait for file processing to complete
                thread_safe_print(f"{CYAN}{ARROW}{RESET} Processing {pdf_filename}...")
                max_wait = 60  # 60 second timeout
                start_time = time.time()

                while uploaded_file.state.name == 'PROCESSING':
                    if time.time() - start_time > max_wait:
                        raise TimeoutError(f"File processing timeout for {pdf_filename}")
                    time.sleep(2)
                    uploaded_file = genai.get_file(uploaded_file.name)

                if uploaded_file.state.name == 'FAILED':
                    raise RuntimeError(f"File processing failed for {pdf_filename}")

                thread_safe_print(f"{CYAN}{CHECK}{RESET} {pdf_filename} ready (URI: {uploaded_file.uri})")

                # Add to parts list
                parts.append(uploaded_file)

            except Exception as e:
                thread_safe_print(f"{RED}{CROSS}{RESET} Failed to upload {pdf_filename}: {e}")
                raise

        return parts

    def cleanup_uploaded_files(self):
        """Delete uploaded files from Gemini Files API"""
        for uploaded_file in self.uploaded_files:
            try:
                thread_safe_print(f"{CYAN}{ARROW}{RESET} Deleting {uploaded_file.name}...")
                genai.delete_file(uploaded_file.name)
            except Exception as e:
                # Don't fail cleanup on errors
                thread_safe_print(f"{YELLOW}{WARNING}{RESET} Failed to delete {uploaded_file.name}: {e}")

        self.uploaded_files = []
        thread_safe_print(f"{CYAN}{CHECK}{RESET} Cleanup complete")

    def create_cache(self) -> bool:
        """Create cache with uploaded PDFs and research context, return True if successful

        Returns:
            bool: True if cache created successfully, False otherwise
        """
        # Build cache contents
        cache_contents = []

        # Add research context first (if available)
        if self.research_context:
            cache_contents.append({
                'role': 'user',
                'parts': [f"=== WEB RESEARCH CONTEXT ===\n\n{self.research_context}\n\n=== END WEB RESEARCH CONTEXT ==="]
            })

        # Add PDF parts (if available)
        if self.pdf_parts:
            cache_contents.extend(self.pdf_parts)

        # If no content to cache, skip caching
        if not cache_contents:
            thread_safe_print(f"{YELLOW}{WARNING}{RESET} No content to cache")
            return False

        try:
            # TTL: 2 hours without insights, 3 hours with insights
            ttl_hours = 3 if self.insights_enabled else 2
            ttl = timedelta(hours=ttl_hours)

            thread_safe_print(f"{CYAN}{ARROW}{RESET} Creating cache (TTL: {ttl_hours * 60} minutes)...")

            # Create cache with combined contents
            self.cache = genai.caching.CachedContent.create(
                model=self.model_name,
                contents=cache_contents,
                ttl=ttl
            )

            # Create cached models with different temperatures
            self.cached_model_low_temp = genai.GenerativeModel.from_cached_content(
                cached_content=self.cache,
                generation_config=genai.types.GenerationConfig(temperature=0.2)
            )

            self.cached_model_medium_temp = genai.GenerativeModel.from_cached_content(
                cached_content=self.cache,
                generation_config=genai.types.GenerationConfig(temperature=0.6)
            )

            thread_safe_print(f"{CYAN}{CHECK}{RESET} Cache created (name: {self.cache.name})")
            return True

        except Exception as e:
            thread_safe_print(f"{YELLOW}{WARNING}{RESET} Cache creation failed: {e}")
            thread_safe_print(f"{YELLOW}{ARROW}{RESET} Continuing without cache (will use Files API)")
            self.cache = None
            self.cached_model_low_temp = None
            self.cached_model_medium_temp = None
            return False

    def cleanup_cache(self):
        """Delete cache from Gemini API"""
        if self.cache:
            try:
                thread_safe_print(f"{CYAN}{ARROW}{RESET} Deleting cache {self.cache.name}...")
                self.cache.delete()
                thread_safe_print(f"{CYAN}{CHECK}{RESET} Cache deleted")
            except Exception as e:
                # Don't fail cleanup on errors
                thread_safe_print(f"{YELLOW}{WARNING}{RESET} Failed to delete cache: {e}")

            self.cache = None
            self.cached_model_low_temp = None
            self.cached_model_medium_temp = None

    def extract_company_name(self, pdf_parts: List) -> str:
        """Extract company name from PDF documents or use provided name"""
        # If company name was provided (e.g., from Deep Research), use it
        if self.provided_company_name:
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Company: {self.provided_company_name}")
            return self.provided_company_name

        thread_safe_print(f"{CYAN}{ARROW}{RESET} Extracting company name...")

        try:
            if self.cached_model_medium_temp:
                # Use cached model - PDFs already in cache
                company_name = retry_with_backoff(
                    lambda: self.cached_model_medium_temp.generate_content([COMPANY_NAME_EXTRACTION_PROMPT]).text.strip(),
                    context="Company name extraction"
                )
            else:
                # Fallback to non-cached model with Files API
                company_name = retry_with_backoff(
                    lambda: self.model_medium_temp.generate_content(pdf_parts + [COMPANY_NAME_EXTRACTION_PROMPT]).text.strip(),
                    context="Company name extraction"
                )

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
            if self.cached_model_medium_temp:
                # Use cached model
                title_subtitle = retry_with_backoff(
                    lambda: self.cached_model_medium_temp.generate_content([prompt]).text.strip(),
                    context="Title/subtitle generation"
                )
            else:
                # Fallback to Files API
                title_subtitle = retry_with_backoff(
                    lambda: self.model_medium_temp.generate_content(self.pdf_parts + [prompt]).text.strip(),
                    context="Title/subtitle generation"
                )
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Title and subtitle complete")
            return title_subtitle
        except Exception as e:
            thread_safe_print(f"{RED}{CROSS}{RESET} Title generation failed: {e}")
            return f"# {company_name}\n[Company Profile]"

    def _generate_section(self, section: dict) -> str:
        """Step 1: Generate initial section content"""
        prompt = get_section_generation_prompt(section)

        if self.cached_model_medium_temp:
            # Use cached model
            return retry_with_backoff(
                lambda: self.cached_model_medium_temp.generate_content([prompt]).text.strip(),
                context=f"Section {section['number']} Draft"
            )
        else:
            # Fallback to Files API
            return retry_with_backoff(
                lambda: self.model_medium_temp.generate_content(self.pdf_parts + [prompt]).text.strip(),
                context=f"Section {section['number']} Draft"
            )

    def _check_section_completeness(self, section: dict, content: str) -> str:
        """Step 2: Check section completeness"""
        prompt = get_section_completeness_check_prompt(section, content)
        prompt = prompt.replace("{source_documents}", "[See attached PDF documents]")

        if self.cached_model_low_temp:
            # Use cached model with low temperature
            return retry_with_backoff(
                lambda: self.cached_model_low_temp.generate_content([prompt]).text.strip(),
                context=f"Section {section['number']} Check"
            )
        else:
            # Fallback to Files API
            return retry_with_backoff(
                lambda: self.model_low_temp.generate_content(self.pdf_parts + [prompt]).text.strip(),
                context=f"Section {section['number']} Check"
            )

    def _enhance_section(self, section: dict, content: str, add_list: str) -> str:
        """Step 3: Enhance section with missing items"""
        prompt = get_section_enhancement_prompt(section, content, add_list)
        prompt = prompt.replace("{source_documents}", "[See attached PDF documents]")

        if self.cached_model_medium_temp:
            # Use cached model
            enhanced = retry_with_backoff(
                lambda: self.cached_model_medium_temp.generate_content([prompt]).text.strip(),
                context=f"Section {section['number']} Enhance"
            )
        else:
            # Fallback to Files API
            enhanced = retry_with_backoff(
                lambda: self.model_medium_temp.generate_content(self.pdf_parts + [prompt]).text.strip(),
                context=f"Section {section['number']} Enhance"
            )

        # Fallback to original if enhancement fails
        if not enhanced or len(enhanced) < 50:
            return content
        return enhanced

    def _polish_section(self, section: dict, content: str, word_limit: int) -> str:
        """Step 5: Polish section to word limit"""
        prompt = get_section_polish_prompt(section, content, word_limit)
        polished = retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text.strip(),
            context=f"Section {section['number']} Polish"
        )

        # Fallback to original if polish fails
        if not polished or len(polished) < 50:
            return content
        return polished

    def _ground_truth_discovery(self, section: dict, company_name: str) -> str:
        """Step 6: Discover ground truth observations from source documents

        Args:
            section: Section dict with ground_truth_pointer
            company_name: Company name for context

        Returns:
            Ground truth observations (2-3 verifiable facts)
        """
        prompt = get_opp_ground_truth_prompt(section, company_name)

        if self.cached_model_medium_temp:
            return retry_with_backoff(
                lambda: self.cached_model_medium_temp.generate_content([prompt]).text.strip(),
                context=f"Section {section['number']} Ground Truth"
            )
        else:
            return retry_with_backoff(
                lambda: self.model_medium_temp.generate_content(self.pdf_parts + [prompt]).text.strip(),
                context=f"Section {section['number']} Ground Truth"
            )

    def _hypothesis_generation(self, step6_output: str, section: dict, company_name: str) -> str:
        """Step 7: Generate hypotheses from ground truth observations

        CRITICAL: This step deliberately uses NON-CACHED model (no document access)
        to prevent anchoring to source framing.

        Args:
            step6_output: Output from Step 6 (ground truth observations)
            section: Section dict
            company_name: Company name for context

        Returns:
            Testable hypotheses (2-3 predictions)
        """
        prompt = get_opp_hypothesis_prompt(step6_output, section, company_name)

        # CRITICAL: Use non-cached model - NO document access
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text.strip(),
            context=f"Section {section['number']} Hypothesis"
        )

    def _hypothesis_testing(self, step7_output: str, section: dict, company_name: str) -> str:
        """Step 8: Test hypotheses against source documents

        Args:
            step7_output: Output from Step 7 (hypotheses)
            section: Section dict
            company_name: Company name for context

        Returns:
            Evidence and verdicts for each hypothesis
        """
        prompt = get_opp_test_prompt(step7_output, section, company_name)
        prompt = prompt.replace("{source_documents}", "[See attached PDF documents]")

        if self.cached_model_low_temp:
            return retry_with_backoff(
                lambda: self.cached_model_low_temp.generate_content([prompt]).text.strip(),
                context=f"Section {section['number']} Test"
            )
        else:
            return retry_with_backoff(
                lambda: self.model_low_temp.generate_content(self.pdf_parts + [prompt]).text.strip(),
                context=f"Section {section['number']} Test"
            )

    def _insight_synthesis(self, step8_output: str, section: dict, company_name: str) -> str:
        """Step 9: Synthesize confirmed hypotheses into insights

        Args:
            step8_output: Output from Step 8 (test results with verdicts)
            section: Section dict
            company_name: Company name for context

        Returns:
            Synthesized insights (2-4 bullets) for Insights-only PPTX
        """
        prompt = get_opp_synthesis_prompt(step8_output, section, company_name)
        prompt = prompt.replace("{source_documents}", "[See attached PDF documents]")

        if self.cached_model_medium_temp:
            return retry_with_backoff(
                lambda: self.cached_model_medium_temp.generate_content([prompt]).text.strip(),
                context=f"Section {section['number']} Synthesis"
            )
        else:
            return retry_with_backoff(
                lambda: self.model_medium_temp.generate_content(self.pdf_parts + [prompt]).text.strip(),
                context=f"Section {section['number']} Synthesis"
            )

    def _insight_integration(self, step5_output: str, step9_output: str, section: dict, company_name: str) -> str:
        """Step 10: Integrate insights into vanilla description

        Args:
            step5_output: Output from Step 5 (vanilla polished content)
            step9_output: Output from Step 9 (synthesized insights)
            section: Section dict
            company_name: Company name for context

        Returns:
            Integrated content (~150 words) with insights woven in
        """
        prompt = get_opp_integration_prompt(step5_output, step9_output, section, company_name)
        prompt = prompt.replace("{source_documents}", "[See attached PDF documents]")

        if self.cached_model_medium_temp:
            return retry_with_backoff(
                lambda: self.cached_model_medium_temp.generate_content([prompt]).text.strip(),
                context=f"Section {section['number']} Integration"
            )
        else:
            return retry_with_backoff(
                lambda: self.model_medium_temp.generate_content(self.pdf_parts + [prompt]).text.strip(),
                context=f"Section {section['number']} Integration"
            )

    def _cleanup_integrated_sections(self, sections_content: dict, worker_display) -> dict:
        """Step 11: Redistribute integrated content based on relevance

        Args:
            sections_content: Dict mapping section_num -> dict with integrated content
            worker_display: WorkerDisplay instance for progress tracking

        Returns:
            Dict mapping section_num -> dict with cleaned integrated content
        """
        from concurrent.futures import ThreadPoolExecutor

        def cleanup_section(section_num: int):
            """Clean up a single integrated section"""
            result = sections_content[section_num]

            if not result.get('success', False):
                return result

            worker_display.update(section_num, "Clean2")

            section = next(s for s in self.sections if s['number'] == section_num)
            section_dir = Path(self.file_manager.run_dir) / f"section_{section_num}"

            all_sections = [
                {
                    'number': s['number'],
                    'title': s['title'],
                    'specs': s['specs'],
                    'content': sections_content[s['number']]['content']
                }
                for s in self.sections
            ]

            target_section = {
                'number': section['number'],
                'title': section['title'],
                'specs': section['specs'],
                'content': result['content']
            }

            prompt = get_opp_cleanup2_prompt(target_section, all_sections)

            cleaned = retry_with_backoff(
                lambda: self.model_low_temp.generate_content(prompt).text.strip(),
                context=f"Section {section_num} Clean-up 2"
            )

            if not cleaned or len(cleaned) < 20:
                cleaned = result['content']

            (section_dir / "step11_cleaned.md").write_text(
                f"## {result['title']}\n{cleaned}",
                encoding='utf-8'
            )

            worker_display._remove_silent(section_num)

            return {
                'number': section_num,
                'title': result['title'],
                'content': cleaned,
                'success': True
            }

        with ThreadPoolExecutor(max_workers=4) as executor:
            futures = {executor.submit(cleanup_section, i): i for i in range(1, 5)}
            results = {}
            for future in futures:
                section_result = future.result()
                results[section_result['number']] = section_result

        return results

    def _polish_integrated(self, section: dict, content: str, step9_insights: str, word_limit: int) -> str:
        """Step 12: Final polish for integrated content via semantic fusion

        Args:
            section: Section dict
            content: Integrated content from Step 11
            step9_insights: Output from Step 9 (insight meanings that must survive)
            word_limit: Target word limit (typically 100-110)

        Returns:
            Final polished integrated content with insight meanings preserved
        """
        prompt = get_opp_polish2_prompt(section, content, step9_insights, word_limit)

        polished = retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text.strip(),
            context=f"Section {section['number']} Polish 2"
        )

        if not polished or len(polished) < 50:
            return content
        return polished

    def _cleanup_sections(self, sections_content: dict, worker_display) -> dict:
        """Step 4: Redistribute content based on relevance to section specs

        Args:
            sections_content: Dict mapping section_num -> dict with 'number', 'title', 'content', 'success'
            worker_display: WorkerDisplay instance for progress tracking

        Returns:
            Dict mapping section_num -> dict with cleaned content
        """
        from concurrent.futures import ThreadPoolExecutor

        def cleanup_section(section_num: int):
            """Clean up a single section"""
            result = sections_content[section_num]

            if not result['success']:
                # Skip failed sections
                return result

            # Display cleanup progress
            worker_display.update(section_num, "Clean-up")

            # Get the section definition
            section = next(s for s in self.sections if s['number'] == section_num)
            section_dir = Path(self.file_manager.run_dir) / f"section_{section_num}"

            # Build section data for prompt (all 4 sections)
            all_sections = [
                {
                    'number': s['number'],
                    'title': s['title'],
                    'specs': s['specs'],
                    'content': sections_content[s['number']]['content']
                }
                for s in self.sections
            ]

            # Target section for cleanup
            target_section = {
                'number': section['number'],
                'title': section['title'],
                'specs': section['specs'],
                'content': result['content']
            }

            # Generate cleanup prompt
            prompt = get_section_cleanup_prompt(target_section, all_sections)

            # Execute cleanup
            cleaned = retry_with_backoff(
                lambda: self.model_low_temp.generate_content(prompt).text.strip(),
                context=f"Section {section_num} Clean-up"
            )

            # Fallback to original if cleanup fails
            if not cleaned or len(cleaned) < 20:
                cleaned = result['content']

            # Save cleaned content
            (section_dir / "step4_cleaned.md").write_text(
                f"## {result['title']}\n{cleaned}",
                encoding='utf-8'
            )

            # Remove from display
            worker_display._remove_silent(section_num)

            return {
                'number': section_num,
                'title': result['title'],
                'content': cleaned,
                'success': True
            }

        # Run cleanup for all 4 sections in parallel
        with ThreadPoolExecutor(max_workers=4) as executor:
            futures = {executor.submit(cleanup_section, i): i for i in range(1, 5)}
            results = {}
            for future in futures:
                section_result = future.result()
                results[section_result['number']] = section_result

        return results

    def _refine_subtitle(self, current_title_subtitle: str, sections_context: str = "") -> str:
        """Refine subtitle for density and investment focus

        Args:
            current_title_subtitle: Current "# Title\nSubtitle" text
            sections_context: Optional context from refined sections

        Returns:
            Refined "# Title\nRefined subtitle" text (or original on failure)
        """
        prompt = get_subtitle_refinement_prompt(current_title_subtitle, sections_context)

        try:
            refined = retry_with_backoff(
                lambda: self.model_medium_temp.generate_content(prompt).text.strip(),
                context="Subtitle refinement"
            )

            # Validate format and word count (up to 8 words)
            lines = refined.strip().split('\n')
            if len(lines) >= 2 and lines[0].startswith('#') and len(lines[1].split()) <= 8:
                return refined.strip()
            thread_safe_print(f"{YELLOW}{WARNING}{RESET} Subtitle validation failed, keeping original")
            return current_title_subtitle

        except Exception as e:
            thread_safe_print(f"{YELLOW}{WARNING}{RESET} Subtitle refinement error: {e}, keeping original")
            return current_title_subtitle

    def process_section_main(self, section: dict, worker_display: WorkerDisplay) -> dict:
        """Process a single section through Steps 1-3 (Draft/Check/Enhance)

        Args:
            section: Section dict from opp_sections.py
            worker_display: WorkerDisplay instance for progress tracking
        """
        section_num = section['number']
        section_title = section['title']

        # Get section directory (already created by FileManager)
        section_dir = Path(self.file_manager.run_dir) / f"section_{section_num}"

        try:
            # Step 1: Initial Draft
            worker_display.update(section_num, "Draft")
            content = self._generate_section(section)
            (section_dir / "step1_draft.md").write_text(f"## {section_title}\n{content}", encoding='utf-8')

            # Step 2: Completeness Check
            worker_display.update(section_num, "Check")
            add_list = self._check_section_completeness(section, content)
            (section_dir / "step2_add_list.txt").write_text(add_list, encoding='utf-8')

            # Step 3: Enhancement
            worker_display.update(section_num, "Enhance")
            enhanced = self._enhance_section(section, content, add_list)
            (section_dir / "step3_enhanced.md").write_text(f"## {section_title}\n{enhanced}", encoding='utf-8')

            # Remove from display after Step 3 completes
            worker_display._remove_silent(section_num)

            return {
                'number': section_num,
                'title': section_title,
                'content': enhanced,
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

    def generate_profile(self, company_name: str, title_subtitle: str, worker_display: WorkerDisplay) -> Optional[str]:
        """Generate profile through processing pipeline

        Pipeline:
        - Steps 1-5: Vanilla profile (always runs)
        - Steps 6-12: Insight pipeline (only if insights_enabled)

        Args:
            company_name: Company name for file naming
            title_subtitle: Pre-generated title and subtitle
            worker_display: WorkerDisplay instance

        Returns:
            Path to generated PPTX (or integrated PPTX if insights enabled), or None if failed
        """
        current_title_subtitle = title_subtitle
        pptx_paths = []  # Track all generated PPTX files

        # Steps 1-3: Draft/Check/Enhance
        thread_safe_print(f"\n{CYAN}Steps 1-3: Draft/Check/Enhance (parallel workers: {self.workers})...{RESET}\n")

        enhanced_results = []
        completed_count = 0

        with ThreadPoolExecutor(max_workers=self.workers) as executor:
            future_to_section = {
                executor.submit(self.process_section_main, section, worker_display): section
                for section in self.sections
            }

            for future in as_completed(future_to_section):
                section = future_to_section[future]
                result = future.result()
                enhanced_results.append(result)
                completed_count += 1
                worker_display.complete(result['number'], completed_count, len(self.sections))

        # Sort by section number for consistent processing
        enhanced_results.sort(key=lambda x: x['number'])

        # Step 4: Clean-up - redistribute content based on relevance
        thread_safe_print(f"\n{CYAN}Step 4: Refining subtitle and cleaning up sections...{RESET}\n")

        thread_safe_print(f"{CYAN}Refining subtitle...{RESET}")
        current_title_subtitle = self._refine_subtitle(current_title_subtitle, "")

        # Save refined subtitle
        subtitle_path = Path(self.file_manager.run_dir) / "subtitle.md"
        subtitle_path.write_text(current_title_subtitle, encoding='utf-8')
        thread_safe_print(f"{CYAN}{CHECK}{RESET} Subtitle refined")

        # Convert enhanced_results list to dict for cleanup
        enhanced_dict = {r['number']: r for r in enhanced_results}

        # Clean up sections in parallel - redistributes content based on relevance
        cleaned_results = self._cleanup_sections(enhanced_dict, worker_display)

        # Step 5: Polish to 100 words
        thread_safe_print(f"\n{CYAN}Step 5: Polishing to 100 words (parallel)...{RESET}\n")

        def polish_section_task(section_num):
            """Polish a single section"""
            cleaned_result = cleaned_results[section_num]

            if not cleaned_result['success']:
                return cleaned_result

            section = next(s for s in self.sections if s['number'] == section_num)
            section_dir = Path(self.file_manager.run_dir) / f"section_{section_num}"

            # Display polish progress
            worker_display.update(section_num, "Polish")

            # Polish the cleaned content
            polished = self._polish_section(section, cleaned_result['content'], word_limit=110)

            # Save polished content
            (section_dir / "step5_polished.md").write_text(
                f"## {cleaned_result['title']}\n{polished}",
                encoding='utf-8'
            )

            # Remove from display after polish
            worker_display._remove_silent(section_num)

            return {
                'number': section_num,
                'title': cleaned_result['title'],
                'content': polished,
                'success': True
            }

        polished_results = []
        with ThreadPoolExecutor(max_workers=self.workers) as executor:
            futures = {executor.submit(polish_section_task, num): num for num in [1, 2, 3, 4]}

            for future in as_completed(futures):
                result = future.result()
                polished_results.append(result)

        # Sort by section number for final assembly
        polished_results.sort(key=lambda x: x['number'])

        # Convert to dict for later use
        polished_dict = {r['number']: r for r in polished_results}

        # Assemble vanilla markdown
        parts = []
        for result in polished_results:
            if result['success']:
                parts.append(f"## {result['title']}\n{result['content']}")

        profile_content = '\n\n'.join(parts)

        # Save vanilla markdown
        vanilla_profile = self._assemble_final_markdown(current_title_subtitle, profile_content)
        vanilla_path = Path(self.file_manager.run_dir) / "final_profile.md"
        vanilla_path.write_text(vanilla_profile, encoding='utf-8')

        # Generate PPTX based on insights mode
        if not self.insights_enabled:
            # Single output mode - no variant suffix
            thread_safe_print(f"\n{CYAN}{CHECK}{RESET} Content complete, generating PowerPoint...")
            try:
                pptx_path = create_profile_pptx(
                    md_path=str(vanilla_path),
                    company_name=company_name,
                    timestamp=self.timestamp
                )
                thread_safe_print(f"{CYAN}{CHECK}{RESET} PowerPoint: {pptx_path}")
                return pptx_path
            except Exception as pptx_error:
                thread_safe_print(f"{YELLOW}{WARNING}{RESET} PowerPoint generation failed: {pptx_error}")
                return None

        # ========== INSIGHTS PIPELINE (Steps 6-12) ==========
        thread_safe_print(f"\n{CYAN}{CHECK}{RESET} Vanilla content complete, generating Vanilla PPTX...")

        # Generate Vanilla PPTX
        try:
            vanilla_pptx = create_profile_pptx(
                md_path=str(vanilla_path),
                company_name=company_name,
                timestamp=self.timestamp,
                variant="vanilla"
            )
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Vanilla PPTX: {vanilla_pptx}")
            pptx_paths.append(vanilla_pptx)
        except Exception as e:
            thread_safe_print(f"{YELLOW}{WARNING}{RESET} Vanilla PPTX failed: {e}")

        # Steps 6-9: Ground Truth Discovery Pipeline
        thread_safe_print(f"\n{CYAN}Steps 6-9: Ground Truth Discovery (parallel)...{RESET}\n")

        def insight_pipeline_task(section_num):
            """Run Steps 6-9 for a single section"""
            section = next(s for s in self.sections if s['number'] == section_num)
            section_dir = Path(self.file_manager.run_dir) / f"section_{section_num}"

            try:
                # Step 6: Ground Truth Discovery
                worker_display.update(section_num, "Ground")
                step6 = self._ground_truth_discovery(section, company_name)
                (section_dir / "step6_ground_truth.md").write_text(step6, encoding='utf-8')

                # Step 7: Hypothesis Generation (no docs!)
                worker_display.update(section_num, "Hypo")
                step7 = self._hypothesis_generation(step6, section, company_name)
                (section_dir / "step7_hypotheses.md").write_text(step7, encoding='utf-8')

                # Step 8: Hypothesis Testing
                worker_display.update(section_num, "Test")
                step8 = self._hypothesis_testing(step7, section, company_name)
                (section_dir / "step8_test_results.md").write_text(step8, encoding='utf-8')

                # Step 9: Insight Synthesis
                worker_display.update(section_num, "Synth")
                step9 = self._insight_synthesis(step8, section, company_name)
                (section_dir / "step9_synthesis.md").write_text(step9, encoding='utf-8')

                worker_display._remove_silent(section_num)

                return {
                    'number': section_num,
                    'title': section['title'],
                    'content': step9,
                    'success': True,
                    'has_insights': "No hypotheses were confirmed" not in step9
                }

            except Exception as e:
                thread_safe_print(f"{RED}{CROSS}{RESET} Section {section_num} insight pipeline failed: {e}")
                worker_display._remove_silent(section_num)
                return {
                    'number': section_num,
                    'title': section['title'],
                    'content': '',
                    'success': False,
                    'has_insights': False
                }

        insight_results = []
        with ThreadPoolExecutor(max_workers=self.workers) as executor:
            futures = {executor.submit(insight_pipeline_task, num): num for num in [1, 2, 3, 4]}
            for future in as_completed(futures):
                result = future.result()
                insight_results.append(result)

        insight_results.sort(key=lambda x: x['number'])
        insight_dict = {r['number']: r for r in insight_results}

        # Generate Insights-only PPTX
        thread_safe_print(f"\n{CYAN}{CHECK}{RESET} Insights synthesized, generating Insights PPTX...")

        insights_parts = []
        for result in insight_results:
            if result['success'] and result['content']:
                insights_parts.append(f"## {result['title']}\n{result['content']}")

        if insights_parts:
            insights_content = '\n\n'.join(insights_parts)
            insights_profile = self._assemble_final_markdown(current_title_subtitle, insights_content)
            insights_path = Path(self.file_manager.run_dir) / "insights_profile.md"
            insights_path.write_text(insights_profile, encoding='utf-8')

            try:
                insights_pptx = create_profile_pptx(
                    md_path=str(insights_path),
                    company_name=company_name,
                    timestamp=self.timestamp,
                    variant="insights"
                )
                thread_safe_print(f"{CYAN}{CHECK}{RESET} Insights PPTX: {insights_pptx}")
                pptx_paths.append(insights_pptx)
            except Exception as e:
                thread_safe_print(f"{YELLOW}{WARNING}{RESET} Insights PPTX failed: {e}")

        # Steps 10-12: Integration Pipeline
        thread_safe_print(f"\n{CYAN}Steps 10-12: Insight Integration (parallel)...{RESET}\n")

        def integration_pipeline_task(section_num):
            """Run Steps 10-12 for a single section"""
            section = next(s for s in self.sections if s['number'] == section_num)
            section_dir = Path(self.file_manager.run_dir) / f"section_{section_num}"

            vanilla_result = polished_dict.get(section_num, {})
            insight_result = insight_dict.get(section_num, {})

            # If no insights were confirmed, use vanilla content
            if not insight_result.get('has_insights', False):
                return {
                    'number': section_num,
                    'title': section['title'],
                    'content': vanilla_result.get('content', ''),
                    'success': vanilla_result.get('success', False)
                }

            try:
                # Step 10: Integration
                worker_display.update(section_num, "Integ")
                step10 = self._insight_integration(
                    vanilla_result.get('content', ''),
                    insight_result.get('content', ''),
                    section,
                    company_name
                )
                (section_dir / "step10_integrated.md").write_text(step10, encoding='utf-8')

                worker_display._remove_silent(section_num)

                return {
                    'number': section_num,
                    'title': section['title'],
                    'content': step10,
                    'success': True
                }

            except Exception as e:
                thread_safe_print(f"{RED}{CROSS}{RESET} Section {section_num} integration failed: {e}")
                worker_display._remove_silent(section_num)
                # Fallback to vanilla
                return {
                    'number': section_num,
                    'title': section['title'],
                    'content': vanilla_result.get('content', ''),
                    'success': vanilla_result.get('success', False)
                }

        integrated_results = []
        with ThreadPoolExecutor(max_workers=self.workers) as executor:
            futures = {executor.submit(integration_pipeline_task, num): num for num in [1, 2, 3, 4]}
            for future in as_completed(futures):
                result = future.result()
                integrated_results.append(result)

        integrated_results.sort(key=lambda x: x['number'])
        integrated_dict = {r['number']: r for r in integrated_results}

        # Step 11: Clean-up 2 - redistribute integrated content
        thread_safe_print(f"\n{CYAN}Step 11: Cleaning up integrated sections...{RESET}\n")
        cleaned_integrated = self._cleanup_integrated_sections(integrated_dict, worker_display)

        # Step 12: Polish 2 - final polish
        thread_safe_print(f"\n{CYAN}Step 12: Final polish (parallel)...{RESET}\n")

        def polish_integrated_task(section_num):
            """Polish integrated content with semantic fusion"""
            cleaned_result = cleaned_integrated[section_num]

            if not cleaned_result.get('success', False):
                return cleaned_result

            section = next(s for s in self.sections if s['number'] == section_num)
            section_dir = Path(self.file_manager.run_dir) / f"section_{section_num}"

            # Read Step 9 insights to guide semantic fusion
            step9_path = section_dir / "step9_synthesis.md"
            step9_insights = ""
            if step9_path.exists():
                step9_insights = step9_path.read_text(encoding='utf-8')

            worker_display.update(section_num, "Polish2")

            polished = self._polish_integrated(section, cleaned_result['content'], step9_insights, word_limit=110)

            (section_dir / "step12_polished.md").write_text(
                f"## {cleaned_result['title']}\n{polished}",
                encoding='utf-8'
            )

            worker_display._remove_silent(section_num)

            return {
                'number': section_num,
                'title': cleaned_result['title'],
                'content': polished,
                'success': True
            }

        final_integrated = []
        with ThreadPoolExecutor(max_workers=self.workers) as executor:
            futures = {executor.submit(polish_integrated_task, num): num for num in [1, 2, 3, 4]}
            for future in as_completed(futures):
                result = future.result()
                final_integrated.append(result)

        final_integrated.sort(key=lambda x: x['number'])

        # Assemble integrated markdown
        integrated_parts = []
        for result in final_integrated:
            if result['success']:
                integrated_parts.append(f"## {result['title']}\n{result['content']}")

        integrated_content = '\n\n'.join(integrated_parts)
        integrated_profile = self._assemble_final_markdown(current_title_subtitle, integrated_content)
        integrated_path = Path(self.file_manager.run_dir) / "integrated_profile.md"
        integrated_path.write_text(integrated_profile, encoding='utf-8')

        # Generate Integrated PPTX
        thread_safe_print(f"\n{CYAN}{CHECK}{RESET} Integration complete, generating Integrated PPTX...")

        try:
            integrated_pptx = create_profile_pptx(
                md_path=str(integrated_path),
                company_name=company_name,
                timestamp=self.timestamp,
                variant="integrated"
            )
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Integrated PPTX: {integrated_pptx}")
            pptx_paths.append(integrated_pptx)
        except Exception as e:
            thread_safe_print(f"{YELLOW}{WARNING}{RESET} Integrated PPTX failed: {e}")

        # Return integrated PPTX as primary output (last one generated)
        return pptx_paths[-1] if pptx_paths else None

    def _assemble_final_markdown(self, title_subtitle: str, section_content: str) -> str:
        """Combine title/subtitle with section content"""
        return f"{title_subtitle}\n\n{section_content}"

    def save_run_log(self, company_name: str, status: str = "Success", pptx_path: str = None):
        """Save run log to the run directory"""
        log_path = Path(self.file_manager.run_dir) / "run_log.txt"

        # Build processing steps list based on insights mode
        if self.insights_enabled:
            processing_steps = """  - Title/Subtitle generation
  - Steps 1-3: Draft/Check/Enhance in parallel
  - Step 4: Clean-up in parallel - redistributes content based on relevance
  - Step 5: Polish to 100 words in parallel (Vanilla)
  - Steps 6-9: Ground Truth Discovery in parallel
  - Steps 10-12: Insight Integration in parallel
  - PowerPoint generation (3 variants: vanilla, insights, integrated)
  - See section_N/ subdirectories for intermediate outputs"""
        else:
            processing_steps = """  - Title/Subtitle generation
  - Steps 1-3: Draft/Check/Enhance in parallel
  - Step 4: Clean-up in parallel - redistributes content based on relevance
  - Step 5: Polish to 100 words in parallel
  - PowerPoint generation
  - See section_N/ subdirectories for intermediate outputs"""

        # Build source files section
        source_files_section = ""
        if self.research_context:
            source_files_section += "Deep Research: Enabled (see deep_research/ folder)\n"
        if self.pdf_files:
            source_files_section += "PDF Files:\n"
            source_files_section += chr(10).join(f"  - {Path(pdf).name}" for pdf in self.pdf_files)
        else:
            source_files_section += "PDF Files: None (research-only mode)"

        log_content = f"""OnePageProfile Run Log
{'='*60}

Timestamp: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}
Model: {self.model_name}
Workers: {self.workers}
Insights: {'Enabled' if self.insights_enabled else 'Disabled'}
Company: {company_name}
Run Directory: {self.file_manager.run_dir}

Source:
{source_files_section}

Processing:
{processing_steps}

Output: {pptx_path if pptx_path else 'None'}

Status: {status}
"""

        with open(log_path, 'w', encoding='utf-8') as f:
            f.write(log_content)

    def run(self) -> Optional[str]:
        """Execute the full profile generation pipeline with parallel section processing

        Returns:
            Path to generated PPTX, or None if failed
        """
        try:
            # Prepare PDFs (may be empty in research-only mode)
            self.pdf_parts = self.prepare_pdf_parts()

            # Validate we have at least one input source
            if not self.pdf_parts and not self.research_context:
                thread_safe_print(f"{RED}{CROSS}{RESET} Error: No PDFs or research context provided")
                return None

            # Log context sources
            if self.research_context and self.pdf_parts:
                thread_safe_print(f"{CYAN}{CHECK}{RESET} Context: Web research + PDF documents")
            elif self.research_context:
                thread_safe_print(f"{CYAN}{CHECK}{RESET} Context: Web research only")
            else:
                thread_safe_print(f"{CYAN}{CHECK}{RESET} Context: PDF documents only")

            # Create cache with uploaded PDFs and/or research context
            self.create_cache()

            # Extract company name (uses provided name if available)
            company_name = self.extract_company_name(self.pdf_parts)

            # Generate title and subtitle
            title_subtitle = self.generate_title_subtitle(company_name)

            # Create worker display
            worker_display = WorkerDisplay(self.workers)

            # Generate profile and PowerPoint
            pptx_path = self.generate_profile(company_name, title_subtitle, worker_display)

            thread_safe_print(f"\n{CYAN}{CHECK}{RESET} Profile generation complete")

            # Save run log
            self.save_run_log(company_name, "Success", pptx_path)

            # Display summary
            thread_safe_print(f"\n{CYAN}{'='*60}{RESET}")
            thread_safe_print(f"{CYAN}{BOLD}Profile generation complete!{RESET}")
            thread_safe_print(f"{CYAN}{'='*60}{RESET}")
            thread_safe_print(f"\nOutput saved to: {self.file_manager.run_dir}/")
            thread_safe_print(f"  - final_profile.md")
            thread_safe_print(f"  - subtitle.md")
            thread_safe_print(f"  - section_1/ through section_4/ (intermediate steps)")

            if pptx_path:
                thread_safe_print(f"\n{CYAN}Final deliverable:{RESET}")
                thread_safe_print(f"  - {pptx_path}")

            return pptx_path

        except Exception as e:
            thread_safe_print(f"{RED}{CROSS}{RESET} Error during profile generation: {e}")
            self.save_run_log(company_name if 'company_name' in locals() else "Unknown", f"Failed: {e}")
            return None

        finally:
            # Always cleanup cache and uploaded files
            if self.cache or self.uploaded_files:
                thread_safe_print(f"\n{CYAN}Cleaning up...{RESET}")

            if self.cache:
                self.cleanup_cache()

            if self.uploaded_files:
                self.cleanup_uploaded_files()


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

    # Deep Research toggle
    thread_safe_print(f"{BOLD}Enable Deep Research (web search)?{RESET}")
    thread_safe_print(f"  Researches 12 topics via web: footprint, products, customers,")
    thread_safe_print(f"  suppliers, competitors, KPIs, financials, shareholders, M&A, management")
    deep_research_enabled = prompt_yes_no("Enable Deep Research? (y/n, default n): ", default=False)

    research_context = None
    company_name = None
    run_dir = None

    if deep_research_enabled:
        thread_safe_print(f"{CYAN}{CHECK}{RESET} Deep Research enabled\n")

        # Company name is required for web search
        company_name = input("Company name for research: ").strip()
        if not company_name:
            thread_safe_print(f"{RED}ERROR:{RESET} Company name is required for Deep Research")
            sys.exit(1)
        thread_safe_print(f"{CYAN}{CHECK}{RESET} Company: {company_name}\n")

        # Research workers
        thread_safe_print("Research workers (parallel queries):")
        thread_safe_print("  1-4 workers (default 4)")
        research_workers_choice = prompt_single_digit("Choose workers [1-4] (default 4): ", valid_digits="1234", default_digit="4")
        research_workers = int(research_workers_choice)
        thread_safe_print(f"{CYAN}{CHECK}{RESET} Research workers: {research_workers}\n")

        # Create run directory early for research output
        from src.deep_research import DeepResearcher, ResearchDisplay
        from src.research_sections import get_research_sections

        run_timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        run_dir = Path(f"runs/opp_{run_timestamp}")
        run_dir.mkdir(parents=True, exist_ok=True)

        # Run Deep Research
        thread_safe_print(f"{CYAN}Starting Deep Research for {company_name}...{RESET}")
        thread_safe_print("="*60 + "\n")

        researcher = DeepResearcher(company_name, research_workers, run_dir)
        research_sections = get_research_sections()

        display = ResearchDisplay(len(research_sections))
        results = researcher.run_all_sections(research_sections, display)

        # Get summary
        summary = researcher.get_research_summary(results)
        thread_safe_print(f"\n{CYAN}{CHECK}{RESET} Deep Research complete: {summary['completed']}/{summary['total']} topics")

        if summary['failed'] > 0:
            thread_safe_print(f"{YELLOW}{WARNING}{RESET} {summary['failed']} topics failed")

        # Combine research results
        research_context = researcher.combine_research(results)
        thread_safe_print(f"{CYAN}{CHECK}{RESET} Research saved to {run_dir}/deep_research/\n")
    else:
        thread_safe_print(f"{CYAN}{CHECK}{RESET} Deep Research disabled\n")

    # File source selection (only if Deep Research is disabled, or as additional input)
    if deep_research_enabled:
        thread_safe_print(f"{BOLD}Also upload PDF documents?{RESET}")
        thread_safe_print(f"  PDFs will be analyzed alongside web research")
        upload_pdfs = prompt_yes_no("Upload PDFs? (y/n, default n): ", default=False)
        batch_mode = False  # Batch mode not available with Deep Research

        if upload_pdfs:
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Will upload PDFs\n")
        else:
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Research-only mode (no PDFs)\n")
    else:
        upload_pdfs = True  # Must upload PDFs if no research
        thread_safe_print(f"{BOLD}Select file source:{RESET}")
        thread_safe_print(f"  {CYAN}1{RESET} - Select files interactively (default)")
        thread_safe_print(f"  {CYAN}2{RESET} - Process batch directory (SourceFiles/SourceBatch/)")
        file_source_choice = prompt_single_digit("Choose [1/2] (default 1): ", valid_digits="12", default_digit="1")
        batch_mode = (file_source_choice == "2")
        if batch_mode:
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Batch mode selected\n")
        else:
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Interactive mode selected\n")

    # Model selection
    thread_safe_print("Select LLM model:")
    thread_safe_print("  1) gemini-3-flash-preview")
    thread_safe_print("  2) gemini-3-pro-preview")
    choice = prompt_single_digit("Choose model [1/2] (default 1): ", valid_digits="12", default_digit="1")
    if choice == "1":
        selected_model = 'gemini-3-flash-preview'
    else:
        selected_model = 'gemini-3-pro-preview'
    thread_safe_print(f"{CYAN}{CHECK}{RESET} Selected: {selected_model}\n")

    # Worker selection
    thread_safe_print("Select number of parallel workers:")
    thread_safe_print("  1-4 workers (default 4)")
    workers_choice = prompt_single_digit("Choose workers [1-4] (default 4): ", valid_digits="1234", default_digit="4")
    num_workers = int(workers_choice)
    thread_safe_print(f"{CYAN}{CHECK}{RESET} Workers: {num_workers}\n")

    # Insights pipeline toggle
    thread_safe_print(f"{BOLD}Enable insights pipeline?{RESET}")
    thread_safe_print(f"  Adds ground truth discovery and hypothesis testing")
    thread_safe_print(f"  Produces 3 PPTX variants: vanilla, insights, integrated")
    insights_enabled = prompt_yes_no("Enable insights? (y/n, default n): ", default=False)
    if insights_enabled:
        thread_safe_print(f"{CYAN}{CHECK}{RESET} Insights pipeline enabled\n")
    else:
        thread_safe_print(f"{CYAN}{CHECK}{RESET} Insights pipeline disabled (vanilla only)\n")

    # Batch mode processing
    if batch_mode:
        # Scan batch directory
        all_pdfs = scan_batch_directory()
        if not all_pdfs:
            sys.exit(1)

        # Show configuration and confirm
        thread_safe_print(f"\n{BOLD}Batch Configuration:{RESET}")
        thread_safe_print(f"  Files: {len(all_pdfs)}")
        thread_safe_print(f"  Model: {selected_model}")
        thread_safe_print(f"  Workers: {num_workers}")
        thread_safe_print(f"  Insights: {'Enabled' if insights_enabled else 'Disabled'}")

        proceed = prompt_yes_no(f"\nProceed with batch processing? (y/n, default y): ", default=True)
        if not proceed:
            thread_safe_print("Batch processing cancelled.")
            sys.exit(0)

        thread_safe_print(f"\n{CYAN}Starting batch processing...{RESET}")
        thread_safe_print("="*60)

        # Track results for summary
        results = []

        # Process each PDF sequentially
        for i, pdf_path in enumerate(all_pdfs, 1):
            thread_safe_print(f"\n{'='*60}")
            thread_safe_print(f"[{i}/{len(all_pdfs)}] Processing: {Path(pdf_path).name}")
            thread_safe_print(f"{'='*60}")

            start_time = datetime.now()

            try:
                maker = OnePageProfile(
                    pdf_files=[pdf_path],
                    model_name=selected_model,
                    workers=num_workers,
                    insights_enabled=insights_enabled
                )
                pptx_path = maker.run()

                elapsed = (datetime.now() - start_time).total_seconds()

                if pptx_path:
                    thread_safe_print(f"\n{CYAN}{CHECK}{RESET} SUCCESS: {Path(pdf_path).name} ({elapsed:.1f}s)")
                    results.append({"file": Path(pdf_path).name, "status": "success", "time": elapsed, "output": pptx_path})
                else:
                    thread_safe_print(f"\n{RED}{CROSS}{RESET} FAILED: {Path(pdf_path).name} ({elapsed:.1f}s)")
                    results.append({"file": Path(pdf_path).name, "status": "failed", "time": elapsed, "error": "No output"})

            except Exception as e:
                elapsed = (datetime.now() - start_time).total_seconds()
                thread_safe_print(f"\n{RED}{CROSS}{RESET} ERROR: {Path(pdf_path).name} - {e}")
                results.append({"file": Path(pdf_path).name, "status": "error", "time": elapsed, "error": str(e)})

        # Print batch summary
        print_batch_summary(results)

    else:
        # Interactive mode
        pdf_files = []

        if upload_pdfs:
            pdf_files = select_pdf_files()
            if not pdf_files and not research_context:
                thread_safe_print(f"{RED}{CROSS}{RESET} No files selected and no research context. Exiting.")
                sys.exit(1)
            elif not pdf_files:
                thread_safe_print(f"{CYAN}{CHECK}{RESET} No PDFs selected, using research-only mode")

        thread_safe_print(f"\n{CYAN}Starting profile generation...{RESET}")
        thread_safe_print("="*60 + "\n")

        # Generate profile with research context if available
        maker = OnePageProfile(
            pdf_files=pdf_files,
            model_name=selected_model,
            workers=num_workers,
            insights_enabled=insights_enabled,
            research_context=research_context,
            company_name=company_name,
            run_dir=run_dir
        )
        profile_path = maker.run()

        if not profile_path:
            thread_safe_print(f"\n{RED}Profile generation failed.{RESET}")
            sys.exit(1)

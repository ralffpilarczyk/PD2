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
__opp_version__ = "1.3"

# Import utilities from PD2
from src.utils import thread_safe_print, retry_with_backoff
# Note: sections imported dynamically based on user profile type selection
from src.profile_prompts import (
    COMPANY_NAME_EXTRACTION_PROMPT,
    get_title_subtitle_prompt,
    get_subtitle_refinement_prompt,
    get_section_generation_prompt,
    get_section_completeness_check_prompt,
    get_section_enhancement_prompt,
    get_section_density_enhancement_prompt,
    get_section_deduplication_prompt,
    get_section_polish_prompt
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
        self.version_label = None  # e.g., "v1:", "v2:", "v3:"

    def update(self, section_num: int, action: str):
        """Update a worker's status and redraw the line

        Args:
            section_num: Section number being processed
            action: One of "Draft", "Check", "Enhance", "Dedup", "Polish"
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

    def set_version(self, version_label: str):
        """Set the version label for display (e.g., 'v1', 'v2', 'v3')"""
        with self.lock:
            self.version_label = version_label

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

        # Prepend version label if set
        if self.version_label:
            line = f"{BOLD}{self.version_label}:{RESET} {line}"

        thread_safe_print(line)


class OnePageProfile:
    """Generates one-page company profiles from PDF documents with parallel section processing"""

    def __init__(self, pdf_files: List[str], model_name: str, workers: int = 2, iterations: int = 1, profile_type: str = "default"):
        self.pdf_files = pdf_files
        self.model_name = model_name
        self.workers = workers
        self.iterations = iterations
        self.profile_type = profile_type

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

        # Dynamic import of sections based on profile type
        if profile_type == "custom":
            from src.opp_sections_custom import sections, get_section_boundaries
            self.run_dir_prefix = "opp_custom"
        else:
            from src.opp_sections import sections, get_section_boundaries
            self.run_dir_prefix = "opp"

        self.sections = sections
        self.get_section_boundaries = get_section_boundaries

        # Validate sections structure (especially important for custom sections)
        self._validate_sections()

        # PDF parts will be prepared once and reused
        self.pdf_parts = None
        self.uploaded_files = []  # Track uploaded files for cleanup

        # Caching support
        self.cache = None  # Cached content object
        self.cached_model_low_temp = None   # Cached model with temp 0.2
        self.cached_model_medium_temp = None  # Cached model with temp 0.6

        # Initialize file manager
        self.file_manager = FileManager(self.timestamp, self.run_dir_prefix)

        # Setup directories
        self.file_manager.setup_directories(self.sections)

    def _validate_sections(self):
        """Validate section structure (especially important for custom sections)"""
        if len(self.sections) != 4:
            raise ValueError(f"Must have exactly 4 sections, found {len(self.sections)}")

        required_keys = {"number", "title", "specs"}
        for i, section in enumerate(self.sections):
            missing = required_keys - set(section.keys())
            if missing:
                raise ValueError(f"Section {i} missing keys: {missing}")

            if section["number"] != i + 1:
                raise ValueError(f"Section numbers must be 1-4 in order, found {section['number']} at position {i}")

    def prepare_pdf_parts(self) -> List:
        """Upload PDF files to Gemini Files API and return file references"""
        parts = []

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
        """Create cache with uploaded PDFs, return True if successful

        Returns:
            bool: True if cache created successfully, False otherwise
        """
        try:
            # Calculate TTL based on iterations
            # Base: 2 hours, add 1 hour per iteration beyond first
            ttl_hours = 2 + (self.iterations - 1)
            ttl = timedelta(hours=ttl_hours)

            thread_safe_print(f"{CYAN}{ARROW}{RESET} Creating cache (TTL: {ttl_hours * 60} minutes)...")

            # Create cache with uploaded files
            self.cache = genai.caching.CachedContent.create(
                model=self.model_name,
                contents=self.pdf_parts,
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
        """Extract company name from PDF documents"""
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

    def _enhance_section_for_density(self, section: dict, content: str) -> str:
        """Step 3: Enhance section for density when no gaps are found (iterations 2+)"""
        prompt = get_section_density_enhancement_prompt(section, content)
        prompt = prompt.replace("{source_documents}", "[See attached PDF documents]")

        if self.cached_model_medium_temp:
            # Use cached model
            enhanced = retry_with_backoff(
                lambda: self.cached_model_medium_temp.generate_content([prompt]).text.strip(),
                context=f"Section {section['number']} Density Enhancement"
            )
        else:
            # Fallback to Files API
            enhanced = retry_with_backoff(
                lambda: self.model_medium_temp.generate_content(self.pdf_parts + [prompt]).text.strip(),
                context=f"Section {section['number']} Density Enhancement"
            )

        # Fallback to original if enhancement fails
        if not enhanced or len(enhanced) < 50:
            return content
        return enhanced

    def _polish_section(self, section: dict, content: str, word_limit: int) -> str:
        """Step 4b: Polish section to word limit"""
        prompt = get_section_polish_prompt(section, content, word_limit)
        polished = retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text.strip(),
            context=f"Section {section['number']} Polish"
        )

        # Fallback to original if polish fails
        if not polished or len(polished) < 50:
            return content
        return polished

    def _deduplicate_section(self, section: dict, content: str, previous_sections: list) -> str:
        """Step 4a: Remove content that overlaps with previous sections

        Args:
            section: Current section dict
            content: Enhanced content to deduplicate
            previous_sections: List of dicts with 'title' and 'content' from already-processed sections

        Returns:
            Deduplicated content
        """
        if not previous_sections:
            # First section (Section 4), nothing to deduplicate against
            return content

        prompt = get_section_deduplication_prompt(section, content, previous_sections)
        deduplicated = retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text.strip(),
            context=f"Section {section['number']} Dedup"
        )

        # Fallback to original if deduplication fails
        if not deduplicated or len(deduplicated) < 50:
            return content
        return deduplicated

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

    def process_section_main(self, section: dict, worker_display: WorkerDisplay, previous_content: str = None, version_num: int = 1) -> dict:
        """Process a single section through Steps 1-3 (Draft/Check/Enhance) or Steps 2-3 (Check/Enhance for iterations 2+)

        Args:
            section: Section dict from opp_sections.py
            worker_display: WorkerDisplay instance for progress tracking
            previous_content: If provided, skip Draft and start with Check using this content (for iterations 2+)
            version_num: Version number (1, 2, or 3) for file naming
        """
        section_num = section['number']
        section_title = section['title']

        # Get section directory (already created by FileManager)
        section_dir = Path(self.file_manager.run_dir) / f"section_{section_num}"

        # Version suffix for file names
        version_suffix = f"_v{version_num}" if version_num > 1 else ""

        try:
            # Step 1: Initial Draft (only for first iteration)
            if previous_content is None:
                worker_display.update(section_num, "Draft")
                content = self._generate_section(section)
                (section_dir / "step1_draft.md").write_text(f"## {section_title}\n{content}", encoding='utf-8')
            else:
                # For iterations 2+, use previous polished content
                content = previous_content

            # Step 2: Completeness Check
            worker_display.update(section_num, "Check")
            add_list = self._check_section_completeness(section, content)
            (section_dir / f"step2_add_list{version_suffix}.txt").write_text(add_list, encoding='utf-8')

            # Step 3: Enhancement
            worker_display.update(section_num, "Enhance")
            if "No critical gaps" in add_list or "No critical gaps identified" in add_list:
                # No gaps found - use density-focused enhancement
                enhanced = self._enhance_section_for_density(section, content)
            else:
                # Gaps found - use standard enhancement with ADD list
                enhanced = self._enhance_section(section, content, add_list)
            (section_dir / f"step3_enhanced{version_suffix}.md").write_text(f"## {section_title}\n{enhanced}", encoding='utf-8')

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

    def generate_profile(self, company_name: str, title_subtitle: str, worker_display: WorkerDisplay) -> List[str]:
        """Generate profile with lockstep iterations through 3-phase processing

        Args:
            company_name: Company name for file naming
            title_subtitle: Pre-generated title and subtitle
            worker_display: WorkerDisplay instance

        Returns:
            List of PPT paths generated (one per successful iteration)
        """
        pptx_paths = []
        previous_polished_results = None
        current_title_subtitle = title_subtitle  # Will be refined in each iteration

        # Loop through iterations with graceful degradation
        for iteration_num in range(1, self.iterations + 1):
            try:
                version_label = f"v{iteration_num}"
                version_suffix = f"_v{iteration_num}" if iteration_num > 1 else ""
                thread_safe_print(f"\n{CYAN}{'='*60}{RESET}")
                thread_safe_print(f"{CYAN}{BOLD}{version_label}: Starting iteration {iteration_num}/{self.iterations}{RESET}")
                thread_safe_print(f"{CYAN}{'='*60}{RESET}")

                # Update worker display to show version
                worker_display.set_version(version_label)

                # Phase 1: Parallel Steps 1-3 (Draft/Check/Enhance) or 2-3 (Check/Enhance for iter 2+)
                if iteration_num == 1:
                    thread_safe_print(f"\n{CYAN}Phase 1: Draft/Check/Enhance (parallel workers: {self.workers})...{RESET}\n")
                else:
                    thread_safe_print(f"\n{CYAN}Phase 1: Check/Enhance (parallel workers: {self.workers})...{RESET}\n")

                enhanced_results = []
                completed_count = 0

                with ThreadPoolExecutor(max_workers=self.workers) as executor:
                    if iteration_num == 1:
                        # First iteration: full pipeline (steps 1-3)
                        future_to_section = {
                            executor.submit(self.process_section_main, section, worker_display, None, iteration_num): section
                            for section in self.sections
                        }
                    else:
                        # Subsequent iterations: use previous polished content (steps 2-3 only)
                        future_to_section = {
                            executor.submit(
                                self.process_section_main,
                                section,
                                worker_display,
                                previous_polished_results[section['number']]['content'],
                                iteration_num
                            ): section
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

                # Phase 2: Sequential Deduplication in REVERSE order (4→3→2→1)
                thread_safe_print(f"\n{CYAN}Phase 2: Refining subtitle and deduplicating...{RESET}\n")

                # Refine subtitle using context from previous iteration's polished sections (if available)
                sections_context = "\n".join([
                    f"{r['title']}: {r['content'][:150]}..."
                    for r in previous_polished_results.values()
                ])[:500] if previous_polished_results else ""

                thread_safe_print(f"{CYAN}Refining subtitle...{RESET}")
                current_title_subtitle = self._refine_subtitle(current_title_subtitle, sections_context)

                # Save refined subtitle
                subtitle_path = Path(self.file_manager.run_dir) / f"subtitle{version_suffix}.md"
                subtitle_path.write_text(current_title_subtitle, encoding='utf-8')
                thread_safe_print(f"{CYAN}{CHECK}{RESET} Subtitle refined")

                deduplicated_results = {}
                processed_sections = []  # Accumulates sections in reverse order

                # Process in reverse: Section 4, then 3, then 2, then 1
                for section_num in [4, 3, 2, 1]:
                    # Find the enhanced result for this section
                    enhanced_result = next(r for r in enhanced_results if r['number'] == section_num)

                    if not enhanced_result['success']:
                        # Skip failed sections
                        deduplicated_results[section_num] = enhanced_result
                        continue

                    section = next(s for s in self.sections if s['number'] == section_num)

                    # Display dedup progress
                    worker_display.update(section_num, "Dedup")

                    # Deduplicate against already-processed sections
                    deduplicated_content = self._deduplicate_section(
                        section,
                        enhanced_result['content'],
                        processed_sections
                    )

                    # Save deduplicated content for this section
                    deduplicated_results[section_num] = {
                        'number': section_num,
                        'title': enhanced_result['title'],
                        'content': deduplicated_content,
                        'success': True
                    }

                    # Add to processed sections (for next iteration)
                    processed_sections.append({
                        'title': enhanced_result['title'],
                        'content': deduplicated_content
                    })

                    # Remove from display after dedup
                    worker_display._remove_silent(section_num)

                # Phase 3: Parallel Polish to 100 words
                thread_safe_print(f"\n{CYAN}Phase 3: Polishing to 100 words (parallel)...{RESET}\n")

                def polish_section_task(section_num):
                    """Polish a single section"""
                    dedup_result = deduplicated_results[section_num]

                    if not dedup_result['success']:
                        return dedup_result

                    section = next(s for s in self.sections if s['number'] == section_num)
                    section_dir = Path(self.file_manager.run_dir) / f"section_{section_num}"

                    # Display polish progress
                    worker_display.update(section_num, "Polish")

                    # Polish the deduplicated content
                    polished = self._polish_section(section, dedup_result['content'], word_limit=120)

                    # Save polished content with version suffix
                    (section_dir / f"step4_polished{version_suffix}.md").write_text(
                        f"## {dedup_result['title']}\n{polished}",
                        encoding='utf-8'
                    )

                    # Remove from display after polish
                    worker_display._remove_silent(section_num)

                    return {
                        'number': section_num,
                        'title': dedup_result['title'],
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

                # Convert to dict for next iteration
                previous_polished_results = {r['number']: r for r in polished_results}

                # Assemble final markdown for this iteration
                parts = []
                for result in polished_results:
                    if result['success']:
                        parts.append(f"## {result['title']}\n{result['content']}")

                profile_content = '\n\n'.join(parts)

                # Save markdown immediately (with refined subtitle)
                final_profile = self._assemble_final_markdown(current_title_subtitle, profile_content)
                final_path = Path(self.file_manager.run_dir) / f"final_profile{version_suffix}.md"
                final_path.write_text(final_profile, encoding='utf-8')

                thread_safe_print(f"\n{CYAN}{CHECK}{RESET} {version_label} content complete, generating PowerPoint...")

                # Generate PowerPoint immediately
                try:
                    pptx_path = create_profile_pptx(
                        md_path=str(final_path),
                        company_name=company_name,
                        timestamp=self.timestamp,
                        version_suffix=version_suffix,
                        profile_type=self.profile_type
                    )
                    pptx_paths.append(pptx_path)
                    thread_safe_print(f"{CYAN}{CHECK}{RESET} {version_label} PowerPoint: {pptx_path}")
                except Exception as pptx_error:
                    thread_safe_print(f"{YELLOW}{WARNING}{RESET} {version_label} PowerPoint generation failed: {pptx_error}")

            except Exception as e:
                # Graceful degradation: log error and stop iterations, but return what we have
                thread_safe_print(f"\n{RED}{CROSS}{RESET} {version_label} failed: {e}")
                thread_safe_print(f"{YELLOW}{WARNING}{RESET} Stopping at {version_label}. Previous iterations are saved.")
                break  # Exit iteration loop, return partial results

        return pptx_paths

    def _assemble_final_markdown(self, title_subtitle: str, section_content: str) -> str:
        """Combine title/subtitle with section content"""
        return f"{title_subtitle}\n\n{section_content}"

    def save_run_log(self, company_name: str, status: str = "Success", pptx_paths: list = None):
        """Save run log to the run directory"""
        log_path = Path(self.file_manager.run_dir) / "run_log.txt"

        # Format PowerPoint paths
        pptx_info = ""
        if pptx_paths:
            pptx_lines = [f"  - {path}" for path in pptx_paths if path]
            if pptx_lines:
                pptx_info = "\n" + "\n".join(pptx_lines)

        log_content = f"""OnePageProfile Run Log
{'='*60}

Timestamp: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}
Model: {self.model_name}
Workers: {self.workers}
Iterations: {self.iterations}
Company: {company_name}
Run Directory: {self.file_manager.run_dir}

Source Files:
{chr(10).join(f"  - {Path(pdf).name}" for pdf in self.pdf_files)}

Processing:
  - Title/Subtitle generation
  - {self.iterations} iteration(s) of 3-phase processing:
    - Phase 1: Steps 1-3 (Draft/Check/Enhance) or 2-3 (Check/Enhance) in parallel
    - Phase 2: Step 4a (Deduplication) sequentially in reverse (4→3→2→1)
    - Phase 3: Step 4b (Polish to 100 words) in parallel
  - PowerPoint generation for each iteration
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

            # Create cache with uploaded PDFs
            self.create_cache()

            # Extract company name
            company_name = self.extract_company_name(self.pdf_parts)

            # Generate title and subtitle
            title_subtitle = self.generate_title_subtitle(company_name)

            # Create worker display
            worker_display = WorkerDisplay(self.workers)

            # Generate all sections and PowerPoints (returns list of PPT paths, one per iteration)
            pptx_paths = self.generate_profile(company_name, title_subtitle, worker_display)

            thread_safe_print(f"\n{CYAN}{CHECK}{RESET} Profile generation complete")

            # Save run log
            self.save_run_log(company_name, "Success", pptx_paths)

            # Display summary
            thread_safe_print(f"\n{CYAN}{'='*60}{RESET}")
            thread_safe_print(f"{CYAN}{BOLD}Profile generation complete!{RESET}")
            thread_safe_print(f"{CYAN}{'='*60}{RESET}")
            thread_safe_print(f"\nOutput saved to: {self.file_manager.run_dir}/")
            for iteration_num in range(1, self.iterations + 1):
                version_suffix = f"_v{iteration_num}" if self.iterations > 1 else ""
                thread_safe_print(f"  - final_profile{version_suffix}.md")
                thread_safe_print(f"  - subtitle{version_suffix}.md")
            thread_safe_print(f"  - section_1/ through section_4/ (intermediate steps)")

            if any(pptx_paths):
                thread_safe_print(f"\n{CYAN}Final deliverables:{RESET}")
                for pptx_path in pptx_paths:
                    if pptx_path:
                        thread_safe_print(f"  - {pptx_path}")

            # Return the first markdown file path (for compatibility)
            return Path(self.file_manager.run_dir) / "final_profile.md" if self.iterations == 1 else Path(self.file_manager.run_dir) / "final_profile_v1.md"

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

    # Profile type selection
    thread_safe_print(f"{BOLD}Select profile type:{RESET}")
    thread_safe_print(f"  {CYAN}1{RESET} - OnePageProfile (default)")
    thread_safe_print(f"  {CYAN}2{RESET} - Custom Profile")
    profile_choice = prompt_single_digit("Choose profile type [1/2] (default 1): ", valid_digits="12", default_digit="1")

    if profile_choice == "2":
        profile_type = "custom"
        # Validate custom file exists
        custom_file = Path("src/opp_sections_custom.py")
        if not custom_file.exists():
            thread_safe_print(f"\n{RED}{CROSS}{RESET} Custom sections file not found!")
            thread_safe_print(f"Please copy the template:")
            thread_safe_print(f"  cp src/opp_sections_template.py src/opp_sections_custom.py")
            thread_safe_print(f"Then edit it to define your custom sections.\n")
            sys.exit(1)
        thread_safe_print(f"{CYAN}{CHECK}{RESET} Using custom section definitions\n")
    else:
        profile_type = "default"
        thread_safe_print(f"{CYAN}{CHECK}{RESET} Using default OnePageProfile sections\n")

    # Model selection
    thread_safe_print("Select LLM model:")
    thread_safe_print("  1) gemini-2.5-flash")
    thread_safe_print("  2) gemini-3-pro-preview")
    thread_safe_print("  3) gemini-2.5-pro")
    choice = prompt_single_digit("Choose model [1/2/3] (default 1): ", valid_digits="123", default_digit="1")
    if choice == "1":
        selected_model = 'gemini-2.5-flash'
    elif choice == "2":
        selected_model = 'gemini-3-pro-preview'
    else:
        selected_model = 'gemini-2.5-pro'
    thread_safe_print(f"{CYAN}{CHECK}{RESET} Selected: {selected_model}\n")

    # Worker selection
    thread_safe_print("Select number of parallel workers:")
    thread_safe_print("  1-4 workers (default 4)")
    workers_choice = prompt_single_digit("Choose workers [1-4] (default 4): ", valid_digits="1234", default_digit="4")
    num_workers = int(workers_choice)
    thread_safe_print(f"{CYAN}{CHECK}{RESET} Workers: {num_workers}\n")

    # Iterations selection
    thread_safe_print("Select number of density iterations:")
    thread_safe_print("  1-3 iterations (default 1)")
    iterations_choice = prompt_single_digit("Choose iterations [1-3] (default 1): ", valid_digits="123", default_digit="1")
    num_iterations = int(iterations_choice)
    thread_safe_print(f"{CYAN}{CHECK}{RESET} Iterations: {num_iterations}\n")

    # File selection
    pdf_files = select_pdf_files()
    if not pdf_files:
        thread_safe_print(f"{RED}{CROSS}{RESET} No files selected. Exiting.")
        sys.exit(1)

    thread_safe_print(f"\n{CYAN}Starting profile generation...{RESET}")
    thread_safe_print("="*60 + "\n")

    # Generate profile
    maker = OnePageProfile(pdf_files, selected_model, workers=num_workers, iterations=num_iterations, profile_type=profile_type)
    profile_path = maker.run()

    if not profile_path:
        thread_safe_print(f"\n{RED}Profile generation failed.{RESET}")
        sys.exit(1)

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
from datetime import datetime
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor, as_completed
import time
import threading

# Load environment variables
load_dotenv()

# Configure Gemini
genai.configure(api_key=os.environ.get("GEMINI_API_KEY"))

# Import modular components
from src import CoreAnalyzer, InsightMemory, QualityTracker, FileManager, ProfileGenerator, sections, __version__

# Import thread_safe_print, retry_with_backoff, and clean_markdown_tables from utils
from src.utils import thread_safe_print, retry_with_backoff, clean_markdown_tables

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

# Module-level worker function for PDF conversion (needed for multiprocessing)
def _pdf_conversion_worker(pdf_path: str, use_llm: bool = False) -> Optional[str]:
    """Worker function for parallel PDF conversion with optional LLM enhancement

    Args:
        pdf_path: Path to PDF file
        use_llm: Whether to use LLM-enhanced conversion
    """
    # Limit BLAS threads in child processes
    os.environ.setdefault("OMP_NUM_THREADS", "1")
    os.environ.setdefault("MKL_NUM_THREADS", "1")

    # Pass Gemini key to Marker if using LLM
    if use_llm:
        gemini_key = os.environ.get("GEMINI_API_KEY", "")
        if gemini_key:
            os.environ["GOOGLE_API_KEY"] = gemini_key

    try:
        from marker.converters.pdf import PdfConverter
        from marker.models import create_model_dict
        from marker.output import text_from_rendered

        if use_llm and os.environ.get("GOOGLE_API_KEY"):
            # LLM-enhanced converter with curated processors for business documents
            converter = PdfConverter(
                artifact_dict=create_model_dict(),
                llm_service="google",
                config={
                    "llm_model": "gemini-2.5-flash",
                    "llm_batch_size": 10,  # Batch multiple items per API call
                    "table_confidence_threshold": 0.7,
                    "enable_table_llm": True,
                    "enable_complex_regions": True,
                }
            )
        else:
            # Basic converter (current behavior)
            converter = PdfConverter(artifact_dict=create_model_dict())

        rendered = converter(pdf_path)
        full_text, _, _ = text_from_rendered(rendered)
        return full_text if isinstance(full_text, str) else (full_text or "")
    except Exception:
        return None


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
    
    def __init__(self, source_files: dict, model_name: str = 'gemini-2.5-flash', use_llm_pdf_conversion: bool = True):
        """Initialize ProfileDash with modular components

        Args:
            source_files: Dict with 'pdf_files' and 'md_files' lists
            model_name: Name of the Gemini model to use
            use_llm_pdf_conversion: Whether to use LLM-enhanced PDF conversion
        """
        # Store LLM conversion preference
        self.use_llm_pdf_conversion = use_llm_pdf_conversion
        # Generate run timestamp
        self.run_timestamp = datetime.now().strftime('%Y_%m_%d_%H_%M_%S')
        
        # Initialize file manager first
        self.file_manager = FileManager(self.run_timestamp)
        
        # Setup directories and ensure memory file exists (before file processing)
        self.file_manager.setup_directories(sections)
        
        # Process both PDF and markdown files
        thread_safe_print("Processing source files...")
        
        pdf_files = source_files.get('pdf_files', [])
        md_files = source_files.get('md_files', [])
        
        all_markdown_files = []
        
        # Convert PDFs to markdown if any exist
        if pdf_files:
            thread_safe_print(f"Converting {len(pdf_files)} PDF file(s) to markdown...")
            converted_files = self._convert_pdfs_to_markdown(pdf_files)
            all_markdown_files.extend(converted_files)
        
        # Add markdown files directly (no conversion needed)
        if md_files:
            thread_safe_print(f"Using {len(md_files)} markdown file(s) directly...")
            all_markdown_files.extend(md_files)
        
        if not all_markdown_files:
            raise Exception("No files were successfully processed")
        
        thread_safe_print(f"Total files for analysis: {len(all_markdown_files)}")
        
        # Load all markdown files using existing method
        self.full_context = self.file_manager.load_markdown_files(all_markdown_files)
        
        # Initialize other components
        self.core_analyzer = CoreAnalyzer(self.full_context, run_timestamp=self.run_timestamp, model_name=model_name)
        self.insight_memory = InsightMemory(self.run_timestamp, model_name=model_name)
        self.quality_tracker = QualityTracker()
        
        # Ensure memory file exists
        self.file_manager.ensure_memory_file_exists(self.insight_memory.get_memory_data())
        
        # Save pre-run memory state
        self.file_manager.save_memory_state(
            self.insight_memory.get_memory_data(), 
            "pre_run_memory.json"
        )
    
    def _convert_single_pdf(self, pdf_path: str, progress_tracker: dict) -> str:
        """Convert a single PDF file to markdown and save to SourceFiles"""
        source_dir = Path("SourceFiles")
        source_dir.mkdir(parents=True, exist_ok=True)

        try:
            mode_str = "(enhanced)" if self.use_llm_pdf_conversion else ""
            thread_safe_print(f"Converting {mode_str}: {Path(pdf_path).name}")

            # Pass API key to environment for LLM mode
            if self.use_llm_pdf_conversion:
                gemini_key = os.environ.get("GEMINI_API_KEY", "")
                if gemini_key:
                    os.environ["GOOGLE_API_KEY"] = gemini_key

            # Import Marker components
            from marker.converters.pdf import PdfConverter
            from marker.models import create_model_dict
            from marker.output import text_from_rendered

            # Create/reuse converter artifacts lazily via attribute cache
            if not hasattr(self, '_marker_converter'):
                if self.use_llm_pdf_conversion and os.environ.get("GOOGLE_API_KEY"):
                    # LLM-enhanced converter with curated processors
                    self._marker_converter = PdfConverter(
                        artifact_dict=create_model_dict(),
                        llm_service="google",
                        config={
                            "llm_model": "gemini-2.5-flash",
                            "llm_batch_size": 10,
                            "table_confidence_threshold": 0.7,
                            "enable_table_llm": True,
                            "enable_complex_regions": True,
                        }
                    )
                else:
                    # Basic converter
                    self._marker_converter = PdfConverter(
                        artifact_dict=create_model_dict(),
                    )

            # Convert PDF
            rendered = self._marker_converter(pdf_path)
            full_text, _, images = text_from_rendered(rendered)
            
            # Create output filename matching PDF name
            pdf_name = Path(pdf_path).stem
            output_path = source_dir / f"{pdf_name}.md"
            
            # Clean markdown before saving
            thread_safe_print(f"Cleaning markdown tables...")
            cleaned_text = clean_markdown_tables(full_text)
            
            # Save cleaned markdown to SourceFiles
            with open(output_path, 'w', encoding='utf-8') as f:
                f.write(cleaned_text)
            
            # Update progress
            with progress_tracker['lock']:
                progress_tracker['completed'] += 1
                thread_safe_print(f"Successfully converted and saved: {output_path.name} ({progress_tracker['completed']}/{progress_tracker['total']} PDFs completed)")
            
            return str(output_path)
            
        except Exception as e:
            thread_safe_print(f"Failed to convert {Path(pdf_path).name} with Marker: {e}")
            # Retry once with Marker, then fallback to pdfminer
            try:
                from marker.converters.pdf import PdfConverter
                from marker.models import create_model_dict
                from marker.output import text_from_rendered
                if not hasattr(self, '_marker_converter_retry'):
                    # For retry, use basic converter as fallback (simpler, more reliable)
                    self._marker_converter_retry = PdfConverter(
                        artifact_dict=create_model_dict(),
                    )
                rendered = self._marker_converter_retry(pdf_path)
                full_text, _, images = text_from_rendered(rendered)

                pdf_name = Path(pdf_path).stem
                output_path = source_dir / f"{pdf_name}.md"
                thread_safe_print(f"Cleaning markdown tables...")
                cleaned_text = clean_markdown_tables(full_text)
                with open(output_path, 'w', encoding='utf-8') as f:
                    f.write(cleaned_text)
                with progress_tracker['lock']:
                    progress_tracker['completed'] += 1
                    thread_safe_print(f"Successfully converted on retry: {output_path.name} ({progress_tracker['completed']}/{progress_tracker['total']} PDFs completed)")
                return str(output_path)
            except Exception as e2:
                thread_safe_print(f"Marker retry failed for {Path(pdf_path).name}: {e2}. Falling back to pdfminer...")
                try:
                    # Minimal pdfminer text extraction fallback
                    from pdfminer.high_level import extract_text
                    text_content = extract_text(pdf_path)
                    pdf_name = Path(pdf_path).stem
                    output_path = source_dir / f"{pdf_name}.md"
                    thread_safe_print(f"Cleaning markdown tables...")
                    cleaned_text = clean_markdown_tables(text_content or '')
                    with open(output_path, 'w', encoding='utf-8') as f:
                        f.write(cleaned_text)
                    with progress_tracker['lock']:
                        progress_tracker['completed'] += 1
                        thread_safe_print(f"Pdfminer fallback succeeded: {output_path.name} ({progress_tracker['completed']}/{progress_tracker['total']} PDFs completed)")
                    return str(output_path)
                except Exception as e3:
                    thread_safe_print(f"Fallback failed for {Path(pdf_path).name}: {e3}")
                    with progress_tracker['lock']:
                        progress_tracker['failed'] += 1
                    return None

    def _convert_pdfs_to_markdown(self, pdf_files: List[str]) -> List[str]:
        """Convert PDF files to markdown, using SourceFiles directory as cache"""
        converted_files: List[str] = []
        source_dir = Path("SourceFiles")
        source_dir.mkdir(parents=True, exist_ok=True)

        if not pdf_files:
            return converted_files

        # Progress tracking
        progress_tracker = {
            'total': len(pdf_files),
            'completed': 0,
            'failed': 0,
            'lock': threading.Lock()
        }

        thread_safe_print(f"\nChecking SourceFiles cache for existing conversions...")

        # Check SourceFiles for existing conversions
        to_convert: List[str] = []
        for pdf_path in pdf_files:
            pdf_name = Path(pdf_path).stem
            # Check both naming conventions for backward compatibility
            # New convention: exact PDF filename with .md extension
            cached_md = source_dir / f"{pdf_name}.md"
            # Old convention: PDF filename with _m.md suffix
            cached_md_legacy = source_dir / f"{pdf_name}_m.md"

            # Prefer new naming, fall back to legacy
            if cached_md.exists():
                with progress_tracker['lock']:
                    progress_tracker['completed'] += 1
                    thread_safe_print(f"{DIM}[{progress_tracker['completed']}/{progress_tracker['total']}]{RESET} {Path(pdf_path).name} {ARROW} {CYAN}Cached{RESET}")
                converted_files.append(str(cached_md))
            elif cached_md_legacy.exists():
                with progress_tracker['lock']:
                    progress_tracker['completed'] += 1
                    thread_safe_print(f"{DIM}[{progress_tracker['completed']}/{progress_tracker['total']}]{RESET} {Path(pdf_path).name} {ARROW} {CYAN}Cached{RESET}")
                converted_files.append(str(cached_md_legacy))
            else:
                to_convert.append(pdf_path)

        # Process conversion for remaining PDFs
        cached_count = len(pdf_files) - len(to_convert)
        if not to_convert:
            thread_safe_print(f"{CYAN}{CHECK}{RESET} All {BOLD}{len(pdf_files)}{RESET} PDF(s) served from SourceFiles cache - no conversion needed!")
            return converted_files

        if cached_count > 0:
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Reusing {BOLD}{cached_count}{RESET} cached file(s) from SourceFiles")
        thread_safe_print(f"{ARROW} Converting {BOLD}{len(to_convert)}{RESET} new PDF(s) to markdown...")

        # Use process pool to avoid PyTorch tensor conflicts
        # Adjust workers based on LLM mode (API-bound vs CPU-bound)
        try:
            if self.use_llm_pdf_conversion:
                # LLM mode: use fewer workers to avoid API rate limits
                workers_env = int(os.environ.get("MARKER_PROCESS_WORKERS", "2"))
                if workers_env > 2:
                    workers_env = 2  # Cap at 2 for LLM mode
            else:
                # Basic mode: can use more workers
                workers_env = int(os.environ.get("MARKER_PROCESS_WORKERS", "3"))
                if workers_env > 5:
                    workers_env = 5
            if workers_env < 1:
                workers_env = 1
        except (ValueError, TypeError):
            thread_safe_print("Warning: Invalid MARKER_PROCESS_WORKERS value, using default")
            workers_env = 2 if self.use_llm_pdf_conversion else 3

        mode_str = "enhanced" if self.use_llm_pdf_conversion else "basic"
        thread_safe_print(f"Using {BOLD}{workers_env}{RESET} worker(s) for {mode_str} PDF conversion")

        with ProcessPoolExecutor(max_workers=workers_env) as pool:
            # Pass use_llm flag to each worker
            future_map = {pool.submit(_pdf_conversion_worker, p, self.use_llm_pdf_conversion): p for p in to_convert}
            for fut in as_completed(future_map):
                src = future_map[fut]
                try:
                    text = fut.result()
                    pdf_name = Path(src).stem
                    # Save directly to SourceFiles with matching PDF name
                    out_path = source_dir / f"{pdf_name}.md"
                    if text is not None:
                        # Clean tables and write to SourceFiles
                        cleaned = clean_markdown_tables(text)
                        out_path.write_text(cleaned, encoding='utf-8')
                        with progress_tracker['lock']:
                            progress_tracker['completed'] += 1
                            thread_safe_print(f"{DIM}[{progress_tracker['completed']}/{progress_tracker['total']}]{RESET} {Path(src).name} {ARROW} {CYAN}Complete{RESET}")
                        converted_files.append(str(out_path))
                    else:
                        # Fallback to original single-threaded path for this file
                        fallback_result = self._convert_single_pdf(src, progress_tracker)
                        if fallback_result:
                            converted_files.append(fallback_result)
                        else:
                            with progress_tracker['lock']:
                                progress_tracker['failed'] += 1
                                thread_safe_print(f"{RED}{CROSS}{RESET} Failed: {Path(src).name}")
                except Exception as e:
                    thread_safe_print(f"{RED}Conversion task error for {Path(src).name}: {e}{RESET}")

        thread_safe_print(f"\nSummary: {cached_count} cached, {progress_tracker['completed']} converted, {BOLD}{len(converted_files)}{RESET} total")
        if progress_tracker['failed'] > 0:
            thread_safe_print(f"{WARNING} {progress_tracker['failed']} file(s) failed")

        return converted_files
    
    def analyze_section(self, section_num: int) -> str:
        """Enhanced 5-step analysis pipeline with completeness and deep analysis."""
        section = next(s for s in sections if s['number'] == section_num)

        try:
            # Get relevant memory for this section
            relevant_memory = self.insight_memory.get_relevant_memory(section_num)

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

            # For Section 33 (Data Book), the initial draft is the final output. No critiques needed.
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
                # Section 33 (Data Book) special-case placeholder if empty
                if _is_empty(final_output, minimum=200):
                    thread_safe_print(f"Section {section_num} ⚠ Appendix empty, using placeholder")
                    final_output = "_Appendix could not be generated from the provided documents in this run._"
                    self.file_manager.save_step_output(section_num, "step_4_final_section.md", final_output)

            # Step 6: Learning Extraction (Applied to the final output)
            # Run learning extraction only on analytical sections
            if section['number'] != self.core_analyzer.SECTION_32_EXEMPT:
                learning = self.core_analyzer.extract_learning(section, final_output)
                # Convert learning (which can be a dict or string) to a formatted JSON string
                learning_str = json.dumps(learning, indent=4)
                self.file_manager.save_step_output(section_num, "step_6_learning.json", learning_str)
                # Note: Learning will be processed in post-run memory review

            # Calculate quality metrics for the section
            self.quality_tracker.calculate_section_metrics(section_num, final_output)
            return f"Section {section_num} completed."

        except Exception as e:
            thread_safe_print(f"Section {section_num} ⚠ Error: {e}")
            # Optionally re-raise or handle as per overall error strategy
            return f"Section {section_num} failed."

    def process_all_sections(self, section_numbers: List[int] = None, max_workers: int = 3):
        """Process multiple sections with two-phase scheduling (Section 33 Data Book deferred)."""
        if section_numbers is None:
            section_numbers = [s['number'] for s in sections]

        results: Dict[int, str] = {}

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

        # Phase 1: run all sections except 33 (Data Book)
        non32 = [n for n in section_numbers if n != self.core_analyzer.SECTION_32_EXEMPT]
        has32 = any(n == self.core_analyzer.SECTION_32_EXEMPT for n in section_numbers)
        phase1 = _run_parallel(non32)
        results.update(phase1)

        # Save quality metrics and run summary for Phase 1
        quality_scores = self.quality_tracker.get_quality_scores()
        run_number = self.insight_memory.learning_memory["meta"]["total_runs"] + 1
        self.file_manager.save_quality_metrics(quality_scores, run_number)
        self._generate_run_summary(results)

        # Generate profile with Phase 1 results (and potential placeholder for 33)
        thread_safe_print(f"\n{'='*60}")
        thread_safe_print(f"{BOLD}Generating Profile{RESET}")
        thread_safe_print(f"{'='*60}")
        try:
            profile_generator = ProfileGenerator(self.run_timestamp, model_name=self.core_analyzer.model_name)
            # Include 33 in list so placeholder (if present) is picked up
            phase1_list = non32 + ([self.core_analyzer.SECTION_32_EXEMPT] if has32 else [])
            profile_generator.generate_html_profile(results, phase1_list, self.full_context, sections)
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Profile ready")
        except Exception as e:
            thread_safe_print(f"{WARNING} Profile generation failed: {e}")

        # Phase 2: run Section 33 (Data Book) alone (sequential) and regenerate
        if has32:
            thread_safe_print(f"\n{'='*60}")
            thread_safe_print(f"{BOLD}Generating Data Appendix (Section 33){RESET}")
            thread_safe_print(f"{'='*60}")
            try:
                res32 = self.analyze_section(self.core_analyzer.SECTION_32_EXEMPT)
                results[self.core_analyzer.SECTION_32_EXEMPT] = res32
            except Exception as e:
                thread_safe_print(f"{WARNING} Appendix generation failed: {e}")
            # Regenerate profile with full set
            try:
                profile_generator = ProfileGenerator(self.run_timestamp, model_name=self.core_analyzer.model_name)
                profile_generator.generate_html_profile(results, section_numbers, self.full_context, sections)
                thread_safe_print(f"{CYAN}{CHECK}{RESET} Appendix complete - Profile updated")
            except Exception as e:
                thread_safe_print(f"{WARNING} Profile update failed: {e}")

        # Post-run memory review AFTER profile delivery
        if len(section_numbers) > 1:
            thread_safe_print(f"\n{'='*60}")
            thread_safe_print(f"{BOLD}Learning Review{RESET}")
            thread_safe_print(f"{'='*60}")
            try:
                self._conduct_memory_review()
            except Exception as e:
                thread_safe_print(f"{WARNING} Memory review failed: {e}")

        return results
    
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
        
        # Generate new analytical methodology candidates from learning extractions
        prompt = f"""Analyze these methodology extractions to identify transferable analytical techniques for future analysis runs.

METHODOLOGY EXTRACTIONS:
{combined_learning}

CURRENT ANALYTICAL MEMORY STATS:
{json.dumps(self.insight_memory.get_memory_stats(), indent=2)}

Extract transferable analytical methodologies that could enhance future analysis of ANY company. Focus on techniques, not company-specific findings.

METHODOLOGY CRITERIA:
1. Formulated as specific analytical techniques that work across companies/industries
2. Focus on calculation methods, ratio analysis, and pattern recognition
3. Applicable to multiple contexts within the same section type
4. Range from solid practices (6/10) to breakthrough methodologies (9-10/10)

For each methodology, provide:
- instruction: "When analyzing [context type], [specific analytical method] to [reveal insight type]"
- section_number: [the section number this methodology applies to]
- quality_score: [6-10, be realistic about distribution]

QUALITY DISTRIBUTION GUIDANCE:
- 9-10/10: Only breakthrough analytical methodologies that consistently reveal material insights
- 7-8/10: Solid analytical techniques that meaningfully improve analysis quality across companies
- 6/10: Standard but useful analytical practices

OUTPUT FORMAT:
NEW_INSIGHTS:
- instruction: "[analytical methodology for future runs]"
  section_number: [section number]
  quality_score: [6-10, realistic distribution]

- instruction: "[another analytical methodology]"
  section_number: [section number]  
  quality_score: [6-10, realistic distribution]

Generate comprehensive methodology candidates - subsequent harsh filtering will select only the best transferable techniques.
"""
        
        model = genai.GenerativeModel(self.core_analyzer.model_name)
        new_insights_text = retry_with_backoff(
            lambda: model.generate_content(prompt).text
        )
        
        self.file_manager.save_memory_state({"new_insights": new_insights_text}, "new_insights.txt")
        
        # Apply memory updates
        try:
            # Archive current memory
            archive_path = self.file_manager.archive_memory(self.insight_memory.get_memory_data())
            thread_safe_print(f"Memory archived to: {archive_path}")
            
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
    "Data Book": {
        "sections": [33],
        "prompt": "5. Data Book (section 33) (y/n): "
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
def select_source_files():
    """Interactively select source files"""
    from tkinter import filedialog, messagebox
    import tkinter as tk
    
    while True:
        root = tk.Tk()
        root.withdraw()
        
        thread_safe_print("\nSelect PDF files (for conversion) and/or Markdown files (direct use)...")
        source_files = filedialog.askopenfilenames(
            title="Select PDF and/or Markdown Files for Analysis",
            filetypes=[
                ("PDF files", "*.pdf"), 
                ("Markdown files", "*.md"),
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
        
        # Categorize files
        pdf_files = [f for f in source_files if f.lower().endswith('.pdf')]
        md_files = [f for f in source_files if f.lower().endswith('.md')]
        other_files = [f for f in source_files if not (f.lower().endswith('.pdf') or f.lower().endswith('.md'))]
        
        thread_safe_print(f"Selected {len(source_files)} file(s):")
        if pdf_files:
            thread_safe_print(f"  PDF files (will be converted): {len(pdf_files)}")
            for pdf in pdf_files:
                thread_safe_print(f"    - {Path(pdf).name}")
        if md_files:
            thread_safe_print(f"  Markdown files (direct use): {len(md_files)}")
            for md in md_files:
                thread_safe_print(f"    - {Path(md).name}")
        if other_files:
            thread_safe_print(f"  Warning: Unsupported files (will be skipped): {len(other_files)}")
            for other in other_files:
                thread_safe_print(f"    - {Path(other).name}")
        
        if not pdf_files and not md_files:
            thread_safe_print("No PDF or Markdown files found. Please select supported file types.")
            continue
        
        return {
            'pdf_files': pdf_files,
            'md_files': md_files,
            'other_files': other_files
        }

def select_pdf_files():
    """Legacy function for backwards compatibility - now redirects to select_source_files"""
    file_selection = select_source_files()
    if file_selection:
        # Return all files for backwards compatibility, let the class handle the logic
        return file_selection['pdf_files'] + file_selection['md_files']
    return None

# Usage interface
if __name__ == "__main__":
    # Clear terminal screen
    print("\033[2J\033[H", end='')

    thread_safe_print(f"PROFILEDASH {__version__} - with Learning Memory")
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

    # Check 2: Verify required dependencies
    try:
        # Check for Marker (PDF conversion)
        from marker.converters.pdf import PdfConverter
        thread_safe_print(f"{CYAN}{CHECK}{RESET} PDF conversion ready (Marker)")
        pdf_support = True
    except ImportError as e:
        thread_safe_print(f"{WARNING} Marker not installed - PDF files cannot be processed")
        thread_safe_print(f"  {DIM}Install with: pip install marker-pdf{RESET}")
        pdf_support = False
        # Don't exit - user might only want to use MD files

    # Check 3: Verify WeasyPrint (PDF report generation)
    try:
        from weasyprint import HTML
        thread_safe_print(f"{CYAN}{CHECK}{RESET} PDF reports ready (WeasyPrint)")
        pdf_generation_support = True
    except ImportError:
        thread_safe_print(f"{WARNING} WeasyPrint not installed - HTML only")
        thread_safe_print(f"  {DIM}Install with: pip install weasyprint{RESET}")
        pdf_generation_support = False
        # Don't exit - HTML reports will still be generated

    # Check 4: Create base directories
    base_dirs = ["runs", "memory", "quality_metrics"]
    for dir_path in base_dirs:
        os.makedirs(dir_path, exist_ok=True)
    thread_safe_print(f"{CYAN}{CHECK}{RESET} Directories verified")
    thread_safe_print("="*60 + "\n")
    
    # Model selection
    thread_safe_print("Select LLM model:")
    thread_safe_print("  1) gemini-2.5-flash")
    thread_safe_print("  2) gemini-2.5-pro")
    selected_model = None
    choice = prompt_single_digit("Choose model [1/2] (default 1): ", valid_digits="12", default_digit="1")
    if choice == "1":
        selected_model = 'gemini-2.5-flash'
    else:
        selected_model = 'gemini-2.5-pro'

    # Optional LLM warm-up to reduce first-call latency (uses selected model)
    try:
        model = genai.GenerativeModel(selected_model, generation_config=genai.types.GenerationConfig(temperature=0.0))
        _ = retry_with_backoff(lambda: model.generate_content("ping").text)
        thread_safe_print(f"{CYAN}{CHECK}{RESET} LLM warm-up completed")
    except Exception as e:
        thread_safe_print(f"{WARNING} LLM warm-up skipped/failed: {e}")
    
    # Step 1: Select source files (PDF and/or MD) with retry capability
    source_file_selection = select_source_files()
    if not source_file_selection:
        thread_safe_print("No files selected. Exiting.")
        sys.exit(0)
    
    # Check if PDFs were selected without support
    if source_file_selection['pdf_files'] and not pdf_support:
        thread_safe_print("\nERROR: PDF files selected but Marker library not available.")
        thread_safe_print("Please install with: pip install marker-pdf")
        sys.exit(1)
    
    # Available sections for validation
    available_sections = [s['number'] for s in sections]
    
    # Step 2: Select analysis components (before PDF conversion)
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
    
    # PDF conversion uses single worker to avoid PyTorch tensor memory issues
    
    # Step 3: Ask about LLM-enhanced PDF conversion
    thread_safe_print("\nPDF Conversion Settings:")
    use_llm_pdf = True  # Default
    if source_file_selection['pdf_files']:
        thread_safe_print("Enhanced PDF conversion uses AI to better extract tables and complex layouts.")
        thread_safe_print("Recommended for investor presentations and financial documents.")
        yn = prompt_yes_no("Use enhanced PDF conversion? (y/n, default y): ")
        use_llm_pdf = yn
        if use_llm_pdf:
            thread_safe_print("  ✓ Enhanced conversion enabled (better quality)")
        else:
            thread_safe_print("  ✓ Basic conversion enabled (faster processing)")

    # Step 4: Ask about number of workers for section processing
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
    
    # Step 5: Now initialize ProfileDash with source files (includes PDF conversion)
    # Clear terminal before starting processing
    print("\033[2J\033[H", end='')

    thread_safe_print("="*60)
    thread_safe_print("STARTING PROCESSING")
    thread_safe_print("="*60)

    try:
        analyst = IntelligentAnalyst(source_file_selection, model_name=selected_model, use_llm_pdf_conversion=use_llm_pdf)
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
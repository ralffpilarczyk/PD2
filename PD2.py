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
except Exception:
    termios = None
    tty = None
try:
    import msvcrt  # Windows
except Exception:
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
from src import CoreAnalyzer, InsightMemory, QualityTracker, FileManager, ProfileGenerator, sections

# Import thread_safe_print, retry_with_backoff, and clean_markdown_tables from utils
from src.utils import thread_safe_print, retry_with_backoff, clean_markdown_tables

class IntelligentAnalyst:
    """Lightweight orchestrator for the intelligent document analysis system"""
    
    def __init__(self, source_files: dict, model_name: str = 'gemini-2.5-flash'):
        """Initialize ProfileDash with modular components
        
        Args:
            source_files: Dict with 'pdf_files' and 'md_files' lists
        """
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
    
    def _convert_single_pdf(self, pdf_path: str, run_dir: Path, progress_tracker: dict) -> str:
        """Convert a single PDF file to markdown"""
        try:
            thread_safe_print(f"Converting: {Path(pdf_path).name}")
            
            # Import Marker components
            from marker.converters.pdf import PdfConverter
            from marker.models import create_model_dict
            from marker.output import text_from_rendered
            
            # Create/reuse converter artifacts lazily via attribute cache
            if not hasattr(self, '_marker_converter'):
                self._marker_converter = PdfConverter(
                    artifact_dict=create_model_dict(),
                )
            
            # Convert PDF
            rendered = self._marker_converter(pdf_path)
            full_text, _, images = text_from_rendered(rendered)
            
            # Create output filename with _m.md suffix
            pdf_name = Path(pdf_path).stem
            output_path = run_dir / f"{pdf_name}_m.md"
            
            # Clean markdown before saving
            thread_safe_print(f"Cleaning markdown tables in {output_path.name}...")
            cleaned_text = clean_markdown_tables(full_text)
            
            # Save cleaned markdown
            with open(output_path, 'w', encoding='utf-8') as f:
                f.write(cleaned_text)
            
            # Update progress
            with progress_tracker['lock']:
                progress_tracker['completed'] += 1
                thread_safe_print(f"Successfully converted: {output_path.name} ({progress_tracker['completed']}/{progress_tracker['total']} PDFs completed)")
            
            return str(output_path)
            
        except Exception as e:
            thread_safe_print(f"Failed to convert {Path(pdf_path).name} with Marker: {e}")
            # Retry once with Marker, then fallback to pdfminer
            try:
                from marker.converters.pdf import PdfConverter
                from marker.models import create_model_dict
                from marker.output import text_from_rendered
                if not hasattr(self, '_marker_converter_retry'):
                    self._marker_converter_retry = PdfConverter(
                        artifact_dict=create_model_dict(),
                    )
                rendered = self._marker_converter_retry(pdf_path)
                full_text, _, images = text_from_rendered(rendered)

                pdf_name = Path(pdf_path).stem
                output_path = run_dir / f"{pdf_name}_m.md"
                thread_safe_print(f"Cleaning markdown tables in {output_path.name}...")
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
                    output_path = run_dir / f"{pdf_name}_m.md"
                    thread_safe_print(f"Cleaning markdown tables in {output_path.name}...")
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
        """Convert PDF files to markdown using Marker with parallel processing and caching"""
        converted_files: List[str] = []
        run_dir = Path(f"runs/run_{self.run_timestamp}")

        if not pdf_files:
            return converted_files

        # Progress tracking
        progress_tracker = {
            'total': len(pdf_files),
            'completed': 0,
            'failed': 0,
            'lock': threading.Lock()
        }

        thread_safe_print(f"Starting PDF conversion (with cache and process isolation)...")

        # Helper: cache dir
        cache_dir = Path("memory/conversion_cache")
        cache_dir.mkdir(parents=True, exist_ok=True)

        def _hash_file(path: str, block_size: int = 1 << 20) -> str:
            sha = hashlib.sha256()
            with open(path, 'rb') as f:
                while True:
                    data = f.read(block_size)
                    if not data:
                        break
                    sha.update(data)
            return sha.hexdigest()

        # First serve from cache where possible
        to_convert: List[str] = []
        for pdf_path in pdf_files:
            try:
                h = _hash_file(pdf_path)
                cached_md = cache_dir / f"{h}.md"
                if cached_md.exists():
                    pdf_name = Path(pdf_path).stem
                    out_path = run_dir / f"{pdf_name}_m.md"
                    shutil.copyfile(cached_md, out_path)
                    with progress_tracker['lock']:
                        progress_tracker['completed'] += 1
                        thread_safe_print(f"Cache hit: {Path(pdf_path).name} → {out_path.name} ({progress_tracker['completed']}/{progress_tracker['total']})")
                    converted_files.append(str(out_path))
                else:
                    to_convert.append(pdf_path)
            except Exception as e:
                thread_safe_print(f"Cache check failed for {Path(pdf_path).name}: {e}")
                to_convert.append(pdf_path)

        # Process conversion for remaining PDFs using single worker fallback (safe) if none pending
        if not to_convert:
            thread_safe_print(f"All PDFs served from cache")
            return converted_files

        # Use process pool to avoid PyTorch tensor conflicts
        try:
            workers_env = int(os.environ.get("MARKER_PROCESS_WORKERS", "2"))
            if workers_env < 1:
                workers_env = 1
            if workers_env > 5:
                workers_env = 5
        except Exception:
            workers_env = 2

        def _worker(pdf_path: str) -> Optional[str]:
            # Limit BLAS threads in child processes
            os.environ.setdefault("OMP_NUM_THREADS", "1")
            os.environ.setdefault("MKL_NUM_THREADS", "1")
            try:
                from marker.converters.pdf import PdfConverter
                from marker.models import create_model_dict
                from marker.output import text_from_rendered
                converter = PdfConverter(artifact_dict=create_model_dict())
                rendered = converter(pdf_path)
                full_text, _, _ = text_from_rendered(rendered)
                return full_text if isinstance(full_text, str) else (full_text or "")
            except Exception:
                return None

        with ProcessPoolExecutor(max_workers=workers_env) as pool:
            future_map = {pool.submit(_worker, p): p for p in to_convert}
            for fut in as_completed(future_map):
                src = future_map[fut]
                try:
                    text = fut.result()
                    pdf_name = Path(src).stem
                    out_path = run_dir / f"{pdf_name}_m.md"
                    if text is not None:
                        # Clean tables and write
                        cleaned = clean_markdown_tables(text)
                        out_path.write_text(cleaned, encoding='utf-8')
                        # Update cache
                        try:
                            h = _hash_file(src)
                            (cache_dir / f"{h}.md").write_text(cleaned, encoding='utf-8')
                        except Exception as ce:
                            thread_safe_print(f"Cache write failed for {Path(src).name}: {ce}")
                        with progress_tracker['lock']:
                            progress_tracker['completed'] += 1
                            thread_safe_print(f"Converted: {Path(src).name} ({progress_tracker['completed']}/{progress_tracker['total']})")
                        converted_files.append(str(out_path))
                    else:
                        # Fallback to original single-threaded path for this file
                        fallback_result = self._convert_single_pdf(src, run_dir, progress_tracker)
                        if fallback_result:
                            converted_files.append(fallback_result)
                        else:
                            with progress_tracker['lock']:
                                progress_tracker['failed'] += 1
                                thread_safe_print(f"Failed: {Path(src).name}")
                except Exception as e:
                    thread_safe_print(f"Conversion task error for {Path(src).name}: {e}")

        thread_safe_print(f"PDF conversion complete: {progress_tracker['completed']} succeeded, {progress_tracker['failed']} failed")

        return converted_files
    
    def analyze_section(self, section_num: int) -> str:
        """Enhanced 5-step analysis pipeline with completeness and deep analysis."""
        section = next(s for s in sections if s['number'] == section_num)
        
        thread_safe_print(f"\n{'='*50}")
        thread_safe_print(f"ANALYZING SECTION {section_num}: {section['title']}")
        thread_safe_print(f"{'='*50}")
        
        try:
            # Get relevant memory for this section
            relevant_memory = self.insight_memory.get_relevant_memory(section_num)
            
            # Step 1: Initial Draft
            thread_safe_print(f"Section {section_num} - Step 1: Creating initial draft...")
            initial_draft = self.core_analyzer.create_initial_draft(section, relevant_memory)
            self.file_manager.save_step_output(section_num, "step_1_initial_draft.md", initial_draft)

            # Failsafe: Retry Step 1 once if empty or too short
            def _is_empty(text: str, minimum: int = 100) -> bool:
                return not text or len(text.strip()) < minimum

            if _is_empty(initial_draft):
                thread_safe_print(f"WARNING: Section {section_num} Step 1 produced empty/short output. Retrying once...")
                retry_draft = self.core_analyzer.create_initial_draft(section, relevant_memory)
                if not _is_empty(retry_draft):
                    initial_draft = retry_draft
                # Save retry attempt separately for diagnostics
                self.file_manager.save_step_output(section_num, "step_1_initial_draft_retry.md", retry_draft or "")
            
            # For Section 32, the initial draft is the final output. No critiques needed.
            if section['number'] == self.core_analyzer.SECTION_32_EXEMPT:
                thread_safe_print(f"Section {section_num} is a data appendix. Skipping analytical and polish steps.")
                final_output = initial_draft
                self.file_manager.save_step_output(section_num, "step_4_final_section.md", final_output)
            else:
                # Step 2: Completeness Check
                thread_safe_print(f"Section {section_num} - Step 2: Checking for missing content...")
                add_list = self.core_analyzer.completeness_check(section, initial_draft)
                self.file_manager.save_step_output(section_num, "step_2_add_list.txt", add_list)
                
                # Step 3: Apply Completeness Changes
                thread_safe_print(f"Section {section_num} - Step 3: Applying completeness changes...")
                improved_draft = self.core_analyzer.apply_completeness_only(section, initial_draft, add_list)
                self.file_manager.save_step_output(section_num, "step_3_improved_draft.md", improved_draft)
                
                # Step 4: Deep Analysis and Polish
                thread_safe_print(f"Section {section_num} - Step 4: Applying deep analysis and polish...")
                step4_output = self.core_analyzer.deep_analysis_and_polish(section, improved_draft)
                self.file_manager.save_step_output(section_num, "step_4_final_section.md", step4_output)
                
                # Step 5 (Optional): Discovery Pipeline Augmentation
                # Double-check Section 32 is excluded (should never reach here for Section 32, but being safe)
                if self.core_analyzer.use_discovery_pipeline and section['number'] != self.core_analyzer.SECTION_32_EXEMPT:
                    thread_safe_print(f"Section {section_num} - Step 5: Running discovery pipeline augmentation...")
                    # Load Step 3 draft for discovery analysis
                    augmented_output = self.core_analyzer.augment_with_discovery(section, improved_draft, step4_output)
                    self.file_manager.save_step_output(section_num, "step_5_discovery_augmented.md", augmented_output)
                    # Adopt augmented output only if it has substance; otherwise keep Step 4
                    final_output = augmented_output if augmented_output and len(augmented_output.strip()) >= 200 else step4_output
                else:
                    final_output = step4_output
            
            # Final failsafe: choose the last non-empty among outputs
            if section['number'] != self.core_analyzer.SECTION_32_EXEMPT:
                candidates = [final_output]
                try:
                    candidates.append(step4_output)
                except UnboundLocalError:
                    pass
                try:
                    candidates.append(improved_draft)
                except UnboundLocalError:
                    pass
                candidates.append(initial_draft)
                final_chosen = next((c for c in candidates if c and len(c.strip()) >= 200), None)
                if final_chosen is None:
                    thread_safe_print(f"WARNING: Section {section_num} generated empty outputs across steps. Inserting placeholder.")
                    final_output = f"_This section failed to generate content during this run._"
                    # Overwrite final file with placeholder to avoid blank HTML
                    self.file_manager.save_step_output(section_num, "step_4_final_section.md", final_output)
                else:
                    final_output = final_chosen
            else:
                # Section 32 special-case placeholder if empty
                if _is_empty(final_output, minimum=200):
                    thread_safe_print(f"WARNING: Section {section_num} appendix is empty. Inserting placeholder.")
                    final_output = "_Appendix could not be generated from the provided documents in this run._"
                    self.file_manager.save_step_output(section_num, "step_4_final_section.md", final_output)

            # Step 6: Learning Extraction (Applied to the final output)
            thread_safe_print(f"Section {section_num} - Step 6: Learning extraction...")
            # Run learning extraction only on analytical sections
            if section['number'] != self.core_analyzer.SECTION_32_EXEMPT:
                learning = self.core_analyzer.extract_learning(section, final_output)
                # Convert learning (which can be a dict or string) to a formatted JSON string
                learning_str = json.dumps(learning, indent=4)
                self.file_manager.save_step_output(section_num, "step_6_learning.json", learning_str)
                # Note: Learning will be processed in post-run memory review
            
            # Calculate quality metrics for the section
            self.quality_tracker.calculate_section_metrics(section_num, final_output)
            thread_safe_print(f"Section {section_num} completed successfully.")
            return f"Section {section_num} completed."

        except Exception as e:
            thread_safe_print(f"An error occurred in section {section_num}: {e}")
            # Optionally re-raise or handle as per overall error strategy
            return f"Section {section_num} failed."

    def process_all_sections(self, section_numbers: List[int] = None, max_workers: int = 3):
        """Process multiple sections with two-phase scheduling (Section 32 deferred)."""
        if section_numbers is None:
            section_numbers = [s['number'] for s in sections]

        results: Dict[int, str] = {}

        # Determine worker caps from env (tunable)
        try:
            env_cap = int(os.environ.get("MAX_SECTION_WORKERS", "3"))
            env_cap = 1 if env_cap < 1 else (8 if env_cap > 8 else env_cap)
        except Exception:
            env_cap = 3
        actual_workers = min(max_workers, len(section_numbers), env_cap)
        try:
            stagger = float(os.environ.get("SUBMISSION_STAGGER_SEC", "0.5"))
        except Exception:
            stagger = 0.5

        def _run_parallel(sec_list: List[int]) -> Dict[int, str]:
            local: Dict[int, str] = {}
            if not sec_list:
                return local
            if actual_workers > 1:
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
                            thread_safe_print(f"Section {s_num} - Completed ({completed}/{len(sec_list)} sections done)")
                        except Exception as e:
                            err = str(e)
                            if "429" in err or "quota" in err.lower():
                                thread_safe_print(f"Section {s_num} - Hit rate limit: {e}")
                            else:
                                thread_safe_print(f"Section {s_num} - Failed to process: {e}")
                            local[s_num] = f"Processing failed: {e}"
            else:
                for s_num in sec_list:
                    try:
                        local[s_num] = self.analyze_section(s_num)
                    except Exception as e:
                        thread_safe_print(f"Section {s_num} - Failed to process: {e}")
                        local[s_num] = f"Processing failed: {e}"
            return local

        # Phase 1: run all sections except 32
        non32 = [n for n in section_numbers if n != self.core_analyzer.SECTION_32_EXEMPT]
        has32 = any(n == self.core_analyzer.SECTION_32_EXEMPT for n in section_numbers)
        phase1 = _run_parallel(non32)
        results.update(phase1)

        # Save quality metrics and run summary for Phase 1
        quality_scores = self.quality_tracker.get_quality_scores()
        run_number = self.insight_memory.learning_memory["meta"]["total_runs"] + 1
        self.file_manager.save_quality_metrics(quality_scores, run_number)
        self._generate_run_summary(results)

        # Generate profile with Phase 1 results (and potential placeholder for 32)
        thread_safe_print(f"\n{'='*50}")
        thread_safe_print("GENERATING FINAL PROFILE (Phase 1)")
        thread_safe_print(f"{'='*50}")
        try:
            profile_generator = ProfileGenerator(self.run_timestamp, model_name=self.core_analyzer.model_name)
            # Include 32 in list so placeholder (if present) is picked up
            phase1_list = non32 + ([self.core_analyzer.SECTION_32_EXEMPT] if has32 else [])
            profile_generator.generate_html_profile(results, phase1_list, self.full_context, sections)
            thread_safe_print("Profile generation complete (Phase 1)!")
        except Exception as e:
            thread_safe_print(f"Warning: HTML profile generation failed: {e}")

        # Phase 2: run Section 32 alone (sequential) and regenerate
        if has32:
            thread_safe_print(f"\n{'='*50}")
            thread_safe_print("RUNNING APPENDIX (SECTION 32) - PHASE 2")
            thread_safe_print(f"{'='*50}")
            try:
                res32 = self.analyze_section(self.core_analyzer.SECTION_32_EXEMPT)
                results[self.core_analyzer.SECTION_32_EXEMPT] = res32
            except Exception as e:
                thread_safe_print(f"Appendix generation failed: {e}")
            # Regenerate profile with full set
            try:
                profile_generator = ProfileGenerator(self.run_timestamp, model_name=self.core_analyzer.model_name)
                profile_generator.generate_html_profile(results, section_numbers, self.full_context, sections)
                thread_safe_print("Profile updated with Appendix (if available)")
            except Exception as e:
                thread_safe_print(f"Warning: HTML profile regeneration failed: {e}")

        # Post-run memory review AFTER profile delivery (skip in test mode)
        if len(section_numbers) > 1:
            thread_safe_print(f"\n{'='*50}")
            thread_safe_print("CONDUCTING POST-RUN MEMORY REVIEW")
            thread_safe_print("(Running in background - your profile is ready)")
            thread_safe_print(f"{'='*50}")
            try:
                self._conduct_memory_review()
            except Exception as e:
                thread_safe_print(f"Warning: Memory review failed: {e}")
        else:
            thread_safe_print(f"\n{'='*50}")
            thread_safe_print("SKIPPING MEMORY REVIEW (Test Mode)")
            thread_safe_print(f"{'='*50}")

        return results
    
    def _conduct_memory_review(self):
        """Review and update learning memory"""
        thread_safe_print("Extracting new insights...")
        
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
        
        model = genai.GenerativeModel('gemini-2.5-flash')
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
            thread_safe_print("Successfully processed new analytical instructions")
            
            # Clean up and diversify memory
            thread_safe_print("Optimizing insight memory for diversity...")
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
            thread_safe_print(f"Memory optimization completed - {memory_stats['total_insights']}/{memory_stats['max_possible']} insights ({memory_stats['utilization_percent']}% utilized)")
            
            thread_safe_print("Memory review completed")
        except Exception as e:
            thread_safe_print(f"Warning: Memory update failed: {e}")
    
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
        "sections": list(range(1, 13)),  # 1-12
        "prompt": "1. Company profile (sections 1-12) (y/n): "
    },
    "Strategy and SWOT": {
        "sections": list(range(13, 19)),  # 13-18
        "prompt": "2. Strategy and SWOT (sections 13-18) (y/n): "
    },
    "Sellside Positioning": {
        "sections": list(range(19, 26)),  # 19-25
        "prompt": "3. Sellside Positioning (sections 19-25) (y/n): "
    },
    "Buyside Due Diligence": {
        "sections": list(range(26, 32)),  # 26-31
        "prompt": "4. Buyside Due Diligence (sections 26-31) (y/n): "
    },
    "Data Book": {
        "sections": [32],
        "prompt": "5. Data Book (section 32) (y/n): "
    },
    "Test Mode": {
        "sections": [1],
        "prompt": "6. Test Mode (section 1 only) (y/n): "
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
        print(f"{prompt_text}", end='', flush=True)
        ch = _read_single_key()
        print(ch)
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
        print(f"{prompt_text}", end='', flush=True)
        ch = _read_single_key()
        # Echo selection for user feedback
        print(ch)
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
    thread_safe_print("PROFILEDASH 2.0 - with Learning Memory")
    thread_safe_print("="*60)
    
    # Pre-flight checks
    thread_safe_print("\nRunning pre-flight checks...")
    
    # Check 1: Verify API key is set
    if not os.environ.get("GEMINI_API_KEY"):
        thread_safe_print("ERROR: GEMINI_API_KEY environment variable not set")
        thread_safe_print("Please set your Gemini API key in a .env file:")
        thread_safe_print("  echo 'GEMINI_API_KEY=your-key-here' > .env")
        exit(1)
    thread_safe_print("✓ Gemini API key configured")
    
    # Check 2: Verify required dependencies
    try:
        # Check for Marker (PDF conversion)
        from marker.converters.pdf import PdfConverter
        thread_safe_print("✓ Marker library available for PDF conversion")
        pdf_support = True
    except ImportError as e:
        thread_safe_print("WARNING: Marker library not available - PDF files cannot be processed")
        thread_safe_print("To enable PDF support, install with: pip install marker-pdf")
        pdf_support = False
        # Don't exit - user might only want to use MD files
    
    # Check 3: Create base directories
    base_dirs = ["runs", "memory", "quality_metrics"]
    for dir_path in base_dirs:
        os.makedirs(dir_path, exist_ok=True)
    thread_safe_print("✓ Directory structure verified")
    
    thread_safe_print("Pre-flight checks completed\n")
    
    # Model selection (Flash / Flash-Lite / 2.0 Flash)
    thread_safe_print("Select LLM model:")
    thread_safe_print("  1) gemini-2.5-flash       (higher quality)")
    thread_safe_print("  2) gemini-2.5-flash-lite  (cheaper)")
    thread_safe_print("  3) gemini-2.0-flash       (legacy, cheaper)")
    selected_model = None
    choice = prompt_single_digit("Choose model [1/2/3] (default 1): ", valid_digits="123", default_digit="1")
    if choice == "1":
        selected_model = 'gemini-2.5-flash'
    elif choice == "2":
        selected_model = 'gemini-2.5-flash-lite'
    else:
        selected_model = 'gemini-2.0-flash'

    # Optional LLM warm-up to reduce first-call latency (uses selected model)
    try:
        model = genai.GenerativeModel(selected_model, generation_config=genai.types.GenerationConfig(temperature=0.0))
        _ = retry_with_backoff(lambda: model.generate_content("ping").text)
        thread_safe_print("✓ LLM warm-up completed")
    except Exception as e:
        thread_safe_print(f"LLM warm-up skipped/failed: {e}")
    
    # Step 1: Select source files (PDF and/or MD) with retry capability
    source_file_selection = select_source_files()
    if not source_file_selection:
        thread_safe_print("No files selected. Exiting.")
        exit()
    
    # Check if PDFs were selected without support
    if source_file_selection['pdf_files'] and not pdf_support:
        thread_safe_print("\nERROR: PDF files selected but Marker library not available.")
        thread_safe_print("Please install with: pip install marker-pdf")
        exit()
    
    # Available sections for validation
    available_sections = [s['number'] for s in sections]
    
    # Step 2: Select analysis components (before PDF conversion)
    thread_safe_print("\nSelect analysis components:")
    
    selected_sections = []
    selected_groups = []
    
    # Normal selection mode - go through all groups including test mode
    for group_name, group_info in SECTION_GROUPS.items():
        yn = prompt_yes_no(group_info["prompt"])  
        if yn:
            # Validate that all sections in this group actually exist
            valid_group_sections = [s for s in group_info["sections"] if s in available_sections]
            if valid_group_sections:
                selected_sections.extend(valid_group_sections)
                selected_groups.append(group_name)
                if group_name == "Test Mode":
                    thread_safe_print("\nTEST MODE: Only Section 1 (Operating Footprint) will be analyzed.")
                    break
            else:
                thread_safe_print(f"Warning: No valid sections found for {group_name}")
        # Break out of outer loop if test mode was selected
        if "Test Mode" in selected_groups:
            break
    
    if not selected_sections:
        thread_safe_print("\nNo sections selected. Exiting.")
        exit()
    
    # Remove duplicates and sort
    selected_sections = sorted(list(set(selected_sections)))
    
    thread_safe_print(f"\nSelected groups: {', '.join(selected_groups)}")
    thread_safe_print(f"Processing sections: {selected_sections}")
    
    # PDF conversion uses single worker to avoid PyTorch tensor memory issues
    
    # Step 3: Ask about number of workers for section processing
    thread_safe_print("\nSection Processing Settings:")
    thread_safe_print("Note: Rate limiting protection enabled")
    thread_safe_print("- Automatic retry with exponential backoff for rate limits")
    thread_safe_print("- Extracts retry delays from API responses")
    thread_safe_print("- Up to 3 retry attempts with intelligent delays")
    
    while True:
        env_cap = int(os.environ.get("MAX_SECTION_WORKERS", "3"))
        env_cap = 1 if env_cap < 1 else (8 if env_cap > 8 else env_cap)
        default_workers = min(2, env_cap)
        valid_digits = ''.join(str(i) for i in range(1, env_cap + 1))
        choice = prompt_single_digit(
            f"Number of parallel workers for section analysis (1-{env_cap}, default {default_workers}): ",
            valid_digits=valid_digits,
            default_digit=str(default_workers)
        )
        max_workers = int(choice)
        if 1 <= max_workers <= env_cap:
            break
    
    # Step 4: Ask about discovery pipeline
    use_discovery = prompt_yes_no("\nUse experimental discovery pipeline for deep insights? (y/n): ")
    
    # Step 6: Now initialize ProfileDash with source files (includes PDF conversion)
    thread_safe_print("\n" + "="*60)
    thread_safe_print("STARTING PROCESSING")
    thread_safe_print("="*60)
    
    try:
        analyst = IntelligentAnalyst(source_file_selection, model_name=selected_model)
    except Exception as e:
        thread_safe_print(f"\nFailed to initialize analyst: {e}")
        thread_safe_print("Please check your files and try again.")
        exit()
    
    if use_discovery:
        thread_safe_print("Discovery pipeline enabled - will use 6-stage analysis for deeper insights")
        thread_safe_print("Note: This will make ~6 LLM calls per section instead of 1")
        analyst.core_analyzer.use_discovery_pipeline = True
    
    thread_safe_print(f"Starting analysis with {'parallel' if max_workers > 1 else 'sequential'} processing...")
    
    results = analyst.process_all_sections(selected_sections, max_workers)
    
    thread_safe_print(f"\n{'='*60}")
    thread_safe_print("ANALYSIS COMPLETE!")
    thread_safe_print(f"{'='*60}")
    thread_safe_print(f"Run folder: runs/run_{analyst.run_timestamp}/")
    thread_safe_print(f"Professional HTML profile generated")
    thread_safe_print(f"Quality metrics and learning insights saved")
    thread_safe_print(f"Step-by-step analysis available for review") 
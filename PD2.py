import os
import json
import google.generativeai as genai
from typing import List, Dict
from dotenv import load_dotenv
from datetime import datetime
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed
import threading
import time
import random

# Load environment variables
load_dotenv()

# Configure Gemini
genai.configure(api_key=os.environ.get("GEMINI_API_KEY"))

# Import modular components
from src import CoreAnalyzer, InsightMemory, QualityTracker, FileManager, ProfileGenerator, sections

# Thread-safe print lock
print_lock = threading.Lock()

def thread_safe_print(*args, **kwargs):
    """Thread-safe print function"""
    with print_lock:
        print(*args, **kwargs)

def retry_with_backoff(func, max_retries=3, base_delay=1.0):
    """Retry function with exponential backoff for rate limits"""
    for attempt in range(max_retries):
        try:
            return func()
        except Exception as e:
            error_str = str(e)
            if "429" in error_str or "quota" in error_str.lower() or "rate" in error_str.lower():
                if attempt < max_retries - 1:
                    # Extract retry delay from error if available
                    delay = base_delay * (2 ** attempt) + random.uniform(0, 1)
                    if "retry_delay" in error_str:
                        try:
                            import re
                            delay_match = re.search(r'seconds:\s*(\d+)', error_str)
                            if delay_match:
                                delay = max(delay, int(delay_match.group(1)))
                        except:
                            pass
                    
                    thread_safe_print(f"Rate limit hit, retrying in {delay:.1f}s (attempt {attempt + 1}/{max_retries})")
                    time.sleep(delay)
                    continue
                else:
                    thread_safe_print(f"Max retries exceeded for rate limit")
                    raise
            else:
                # Non-rate-limit error, don't retry
                raise
    return None

class IntelligentAnalyst:
    """Lightweight orchestrator for the intelligent document analysis system"""
    
    def __init__(self, source_files: dict):
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
        self.core_analyzer = CoreAnalyzer(self.full_context)
        self.insight_memory = InsightMemory(self.run_timestamp)
        self.quality_tracker = QualityTracker()
        
        # Ensure memory file exists
        self.file_manager.ensure_memory_file_exists(self.insight_memory.get_memory_data())
        
        # Save pre-run memory state
        self.file_manager.save_memory_state(
            self.insight_memory.get_memory_data(), 
            "pre_run_memory.json"
        )
    
    def _convert_pdfs_to_markdown(self, pdf_files: List[str]) -> List[str]:
        """Convert PDF files to markdown using Marker and save to run folder"""
        converted_files = []
        run_dir = Path(f"runs/run_{self.run_timestamp}")
        
        for pdf_path in pdf_files:
            try:
                thread_safe_print(f"Converting: {Path(pdf_path).name}")
                
                # Import Marker components
                from marker.converters.pdf import PdfConverter
                from marker.models import create_model_dict
                from marker.output import text_from_rendered
                
                # Create converter
                converter = PdfConverter(
                    artifact_dict=create_model_dict(),
                )
                
                # Convert PDF
                rendered = converter(pdf_path)
                full_text, _, images = text_from_rendered(rendered)
                
                # Create output filename with _m.md suffix
                pdf_name = Path(pdf_path).stem
                output_path = run_dir / f"{pdf_name}_m.md"
                
                # Save converted markdown
                with open(output_path, 'w', encoding='utf-8') as f:
                    f.write(full_text)
                
                converted_files.append(str(output_path))
                thread_safe_print(f"Successfully converted: {output_path.name}")
                
            except Exception as e:
                thread_safe_print(f"Failed to convert {Path(pdf_path).name}: {e}")
                continue
        
        return converted_files
    
    def analyze_section(self, section_num: int) -> str:
        """Simplified 2-step analysis pipeline for a section."""
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
            
            # For Section 32, the initial draft is the final output. No critiques needed.
            if section['number'] == self.core_analyzer.SECTION_32_EXEMPT:
                thread_safe_print(f"Section {section_num} is a data appendix. Skipping analytical and polish steps.")
                final_output = initial_draft
                self.file_manager.save_step_output(section_num, "step_2_final_section.md", final_output)
            else:
                # Step 2: Single, Aggressive Polish & Cut
                thread_safe_print(f"Section {section_num} - Step 2: Applying aggressive polish and cut...")
                # We reuse the "polish_critique" method, but frame it as a direct instruction to fix the draft.
                polish_instructions = self.core_analyzer.polish_critique(section, initial_draft)
                self.file_manager.save_step_output(section_num, "step_2_polish_instructions.txt", polish_instructions)
                
                # Apply the polish instructions to create the final, polished section
                final_output = self.core_analyzer.apply_critique(section, initial_draft, polish_instructions, "polish")
                self.file_manager.save_step_output(section_num, "step_2_final_section.md", final_output)
            
            # Step 3: Learning Extraction (Applied to the final, polished output)
            thread_safe_print(f"Section {section_num} - Step 3: Learning extraction...")
            # Run learning extraction only on analytical sections
            if section['number'] != self.core_analyzer.SECTION_32_EXEMPT:
                learning = self.core_analyzer.extract_learning(section, final_output)
                # Convert learning (which can be a dict or string) to a formatted JSON string
                learning_str = json.dumps(learning, indent=4)
                self.file_manager.save_step_output(section_num, "step_3_learning.json", learning_str)
                self.insight_memory.add_learning(section_num, json.loads(learning_str)) # Add to memory
            
            self.quality_tracker.log_final_word_count(section_num, final_output)
            thread_safe_print(f"Section {section_num} completed successfully.")
            return f"Section {section_num} completed."

        except Exception as e:
            thread_safe_print(f"An error occurred in section {section_num}: {e}")
            # Optionally re-raise or handle as per overall error strategy
            return f"Section {section_num} failed."

    def process_all_sections(self, section_numbers: List[int] = None, max_workers: int = 3):
        """Process multiple sections in parallel and handle memory updates"""
        if section_numbers is None:
            section_numbers = [s['number'] for s in sections]
        
        results = {}
        
        # Determine optimal number of workers (don't exceed API rate limits)
        actual_workers = min(max_workers, len(section_numbers), 3)  # Cap at 3 to avoid rate limits
        
        if actual_workers > 1:
            # Parallel processing mode
            thread_safe_print(f"Processing {len(section_numbers)} sections with {actual_workers} parallel workers...")
            
            with ThreadPoolExecutor(max_workers=actual_workers) as executor:
                # Submit section analysis tasks with staggered start to avoid rate limit spike
                future_to_section = {}
                for i, section_num in enumerate(section_numbers):
                    future_to_section[executor.submit(self.analyze_section, section_num)] = section_num
                    # Small delay between submissions to stagger API calls
                    if i < len(section_numbers) - 1:  # Don't delay after last submission
                        time.sleep(0.5)
                
                # Collect results as they complete
                completed_count = 0
                for future in as_completed(future_to_section):
                    section_num = future_to_section[future]
                    try:
                        result = future.result()
                        results[section_num] = result
                        completed_count += 1
                        thread_safe_print(f"Section {section_num} - Completed ({completed_count}/{len(section_numbers)} sections done)")
                    except Exception as e:
                        error_str = str(e)
                        if "429" in error_str or "quota" in error_str.lower():
                            thread_safe_print(f"Section {section_num} - Hit rate limit: {e}")
                        else:
                            thread_safe_print(f"Section {section_num} - Failed to process: {e}")
                        results[section_num] = f"Processing failed: {e}"
                        completed_count += 1
            
            thread_safe_print(f"All {len(section_numbers)} sections completed! Moving to memory review...")
        else:
            # Sequential processing mode (original behavior)
            for section_num in section_numbers:
                try:
                    result = self.analyze_section(section_num)
                    results[section_num] = result
                except Exception as e:
                    thread_safe_print(f"Section {section_num} - Failed to process: {e}")
                    results[section_num] = f"Processing failed: {e}"
        
        # Post-run memory review and update
        thread_safe_print(f"\n{'='*50}")
        thread_safe_print("CONDUCTING POST-RUN MEMORY REVIEW")
        thread_safe_print(f"{'='*50}")
        
        try:
            self._conduct_memory_review()
        except Exception as e:
            thread_safe_print(f"Warning: Memory review failed: {e}")
        
        # Save quality metrics
        quality_scores = self.quality_tracker.get_quality_scores()
        run_number = self.insight_memory.learning_memory["meta"]["total_runs"] + 1
        self.file_manager.save_quality_metrics(quality_scores, run_number)
        
        # Final step: generate the consolidated HTML profile
        self._generate_run_summary(results)
        
        # Generate final HTML profile
        thread_safe_print(f"\n{'='*50}")
        thread_safe_print("GENERATING FINAL PROFILE")
        thread_safe_print(f"{'='*50}")
        
        try:
            # Initialize ProfileGenerator and generate HTML profile
            profile_generator = ProfileGenerator(self.run_timestamp)
            profile_generator.generate_html_profile(results, section_numbers, self.full_context, sections)
        except Exception as e:
            thread_safe_print(f"Warning: HTML profile generation failed: {e}")
        
        return results
    
    def _conduct_memory_review(self):
        """Review and update learning memory"""
        thread_safe_print("Extracting new insights...")
        
        # Collect all learning extractions
        learning_files = []
        for section in sections:
            learning_file = f"runs/run_{self.run_timestamp}/section_{section['number']}/step_8_learning_extraction.txt"
            if os.path.exists(learning_file):
                with open(learning_file, 'r', encoding='utf-8') as f:
                    learning_files.append(f.read())
        
        if not learning_files:
            thread_safe_print("No learning extractions found")
            return
        
        combined_learning = "\n\n".join(learning_files)
        
        # Generate new insight candidates using instruction format
        prompt = f"""Analyze these learning extractions to identify new analytical instruction patterns.

LEARNING EXTRACTIONS:
{combined_learning}

CURRENT INSIGHT MEMORY STATS:
{json.dumps(self.insight_memory.get_memory_stats(), indent=2)}

Generate analytical instruction candidates that could enhance future analysis runs. Be comprehensive in generating candidates - harsh quality filtering happens later.

INSTRUCTION CRITERIA:
1. Formulated as specific analytical techniques for future runs
2. Focus on universal patterns about relationships and material impact  
3. Additive to section requirements (don't repeat basic section specs)
4. Range from solid practices (6/10) to breakthrough insights (9-10/10)

For each candidate, provide:
- instruction: "When analyzing [specific context], [specific technique] - [why this reveals insights]"
- section_number: [the specific section number this applies to]
- quality_score: [6-10, be realistic about distribution]

QUALITY DISTRIBUTION GUIDANCE:
- 9-10/10: Only 1-2 truly breakthrough analytical insights per section
- 7-8/10: Solid techniques that meaningfully improve analysis quality  
- 6/10: Standard but useful analytical practices

OUTPUT FORMAT:
NEW_INSIGHTS:
- instruction: "[analytical instruction for future runs]"
  section_number: [section number]
  quality_score: [6-10, realistic distribution]

- instruction: "[another analytical instruction]"
  section_number: [section number]  
  quality_score: [6-10, realistic distribution]

Generate comprehensive candidates - subsequent harsh filtering will select only the best.
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
        """Generates a final summary and consolidated HTML profile from the run."""
        thread_safe_print("\n" + "="*20 + " Generating Final Run Summary " + "="*20)
        
        run_dir = Path(f"runs/run_{self.run_timestamp}")
        summary_path = run_dir / "run_summary.md"
        profile_path = run_dir / "Maxis_profile.html" # Example filename
        
        # Sort sections by number for ordered report
        sorted_section_numbers = sorted([s['number'] for s in sections])

        # Start of HTML content with CSS for styling
        html_content = """
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Company Profile</title>
    <style>
        body { font-family: sans-serif; line-height: 1.6; margin: 20px; }
        h1, h2, h3 { color: #333; }
        h1 { text-align: center; }
        .section { margin-bottom: 30px; padding: 20px; border: 1px solid #ddd; border-radius: 5px; }
        .section-title { font-size: 1.5em; color: #555; }
        table { border-collapse: collapse; width: 100%; margin-bottom: 20px; }
        th, td { border: 1px solid #ccc; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
        .footnote { font-size: 0.9em; color: #777; }
    </style>
</head>
<body>
    <h1>Company Profile</h1>
"""
        summary_content = "# Run Summary\n\n"
        
        total_sections = len(sorted_section_numbers)
        completed_sections = 0
        failed_sections = []

        for section_num in sorted_section_numbers:
            section_dir = run_dir / f"section_{section_num}"
            final_profile_path = section_dir / "step_2_final_section.md"
            
            section_title = next((s['title'] for s in sections if s['number'] == section_num), "Unknown Section")
            
            if final_profile_path.exists():
                try:
                    with open(final_profile_path, 'r', encoding='utf-8') as f:
                        content = f.read()
                    
                    # Basic markdown to HTML conversion
                    import markdown
                    html_from_markdown = markdown.markdown(content, extensions=['tables'])
                    
                    # Add section to HTML content
                    html_content += f'<div class="section" id="section_{section_num}">\n'
                    html_content += f'  <h2 class="section-title">Section {section_num}: {section_title}</h2>\n'
                    html_content += f'  {html_from_markdown}\n'
                    html_content += '</div>\n'
                    
                    summary_content += f"- **Section {section_num}: {section_title}** - COMPLETED\n"
                    completed_sections += 1
                except Exception as e:
                    summary_content += f"- **Section {section_num}: {section_title}** - FAILED (Error reading final file: {e})\n"
                    failed_sections.append(section_num)
            else:
                summary_content += f"- **Section {section_num}: {section_title}** - SKIPPED (Final file not found)\n"

        # End of HTML content
        html_content += """
</body>
</html>
"""
        
        # Overall summary statistics
        summary_content += f"\n---\n"
        summary_content += f"**Overall Status:** {completed_sections}/{total_sections} sections completed.\n"
        if failed_sections:
            summary_content += f"**Failed Sections:** {', '.join(map(str, failed_sections))}\n"

        with open(summary_path, 'w', encoding='utf-8') as f:
            f.write(summary_content)
            
        with open(profile_path, 'w', encoding='utf-8') as f:
            f.write(html_content)
        
        thread_safe_print(f"Run summary saved to: {summary_path}")
        thread_safe_print(f"Final HTML profile saved to: {profile_path}")


# Section Groups Configuration
SECTION_GROUPS = {
    "Company Profile": {
        "sections": list(range(1, 15)),  # 1-14
        "prompt": "Company profile (sections 1-14) (y/n): "
    },
    "SWOT Analysis": {
        "sections": list(range(15, 19)),  # 15-18
        "prompt": "SWOT Analysis (sections 15-18) (y/n): "
    },
    "Sellside Positioning": {
        "sections": list(range(19, 26)),  # 19-25
        "prompt": "Sellside Positioning (sections 19-25) (y/n): "
    },
    "Buyside Due Diligence": {
        "sections": list(range(26, 32)),  # 26-31
        "prompt": "Buyside Due Diligence (sections 26-31) (y/n): "
    },
    "Data Book": {
        "sections": [32],
        "prompt": "Data Book (section 32) (y/n): "
    }
}

# PDF Selection Functions
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
            retry = input("Would you like to try selecting files again? (y/n): ").strip().lower()
            if retry not in ['y', 'yes']:
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
    
    # Select source files (PDF and/or MD) with retry capability
    source_file_selection = select_source_files()
    if not source_file_selection:
        thread_safe_print("No files selected. Exiting.")
        exit()
    
    # Initialize ProfileDash with source files
    try:
        analyst = IntelligentAnalyst(source_file_selection)
    except Exception as e:
        thread_safe_print(f"\nFailed to initialize analyst: {e}")
        thread_safe_print("Please check your files and try again.")
        exit()
    
    # Available sections for validation
    available_sections = [s['number'] for s in sections]
    
    thread_safe_print("\nSelect analysis components:")
    thread_safe_print()
    
    # Collect selected sections
    selected_sections = []
    selected_groups = []
    
    for group_name, group_info in SECTION_GROUPS.items():
        while True:
            response = input(group_info["prompt"]).strip().lower()
            if response in ['y', 'yes', 'n', 'no']:
                if response in ['y', 'yes']:
                    # Validate that all sections in this group actually exist
                    valid_group_sections = [s for s in group_info["sections"] if s in available_sections]
                    if valid_group_sections:
                        selected_sections.extend(valid_group_sections)
                        selected_groups.append(group_name)
                    else:
                        thread_safe_print(f"Warning: No valid sections found for {group_name}")
                break
            else:
                thread_safe_print("Please enter 'y' or 'n'")
    
    if not selected_sections:
        thread_safe_print("\nNo sections selected. Exiting.")
        exit()
    
    # Remove duplicates and sort
    selected_sections = sorted(list(set(selected_sections)))
    
    thread_safe_print(f"\nSelected groups: {', '.join(selected_groups)}")
    thread_safe_print(f"Processing sections: {selected_sections}")
    
    # Ask about parallel processing
    while True:
        parallel_choice = input(f"\nUse parallel processing? (recommended for {len(selected_sections)} sections) (y/n): ").strip().lower()
        if parallel_choice in ['y', 'yes', 'n', 'no']:
            break
        thread_safe_print("Please enter 'y' or 'n'")
    
    if parallel_choice in ['y', 'yes']:
        thread_safe_print("Note: Rate limiting protection enabled")
        thread_safe_print("- Automatic retry with exponential backoff for rate limits")
        thread_safe_print("- Extracts retry delays from API responses")
        thread_safe_print("- Up to 3 retry attempts with intelligent delays")
        
        while True:
            try:
                worker_input = input("Number of parallel workers (1-3, recommended: 2): ").strip()
                max_workers = int(worker_input)
                if 1 <= max_workers <= 3:
                    break
                else:
                    thread_safe_print("Please enter a number between 1 and 3")
            except ValueError:
                thread_safe_print("Please enter a valid number")
    else:
        max_workers = 1
    
    thread_safe_print(f"Starting analysis with {'parallel' if max_workers > 1 else 'sequential'} processing...")
    
    results = analyst.process_all_sections(selected_sections, max_workers)
    
    thread_safe_print(f"\n{'='*60}")
    thread_safe_print("ANALYSIS COMPLETE!")
    thread_safe_print(f"{'='*60}")
    thread_safe_print(f"Run folder: runs/run_{analyst.run_timestamp}/")
    thread_safe_print(f"Professional HTML profile generated")
    thread_safe_print(f"Quality metrics and learning insights saved")
    thread_safe_print(f"Step-by-step analysis available for review") 
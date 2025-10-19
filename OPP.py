import os
import base64
import google.generativeai as genai
from typing import List, Optional
import sys
from dotenv import load_dotenv
from datetime import datetime
from pathlib import Path
from tkinter import filedialog
import tkinter as tk

# Load environment variables
load_dotenv()

# Configure Gemini
genai.configure(api_key=os.environ.get("GEMINI_API_KEY"))

# Version
__version__ = "1.0"

# Import utilities from PD2
from src.utils import thread_safe_print
from src.profile_prompts import (
    COMPANY_NAME_EXTRACTION_PROMPT,
    get_profile_generation_prompt,
    get_completeness_check_prompt,
    get_enhancement_prompt,
    get_polish_prompt
)
from src.pptx_generator import create_profile_pptx
import re

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


class OnePageProfile:
    """Generates one-page company profiles from PDF documents with refinement loop"""

    def __init__(self, pdf_files: List[str], model_name: str):
        self.pdf_files = pdf_files
        self.model_name = model_name

        # Create models with different temperatures for different phases
        self.model_low_temp = genai.GenerativeModel(
            model_name,
            generation_config=genai.types.GenerationConfig(temperature=0.2)
        )
        self.model_medium_temp = genai.GenerativeModel(
            model_name,
            generation_config=genai.types.GenerationConfig(temperature=0.6)
        )

        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        # Create run directory
        self.run_dir = Path("runs") / f"opp_{self.timestamp}"
        self.run_dir.mkdir(parents=True, exist_ok=True)

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

    def generate_profile(self, pdf_parts: List, company_name: str) -> str:
        """Generate the initial profile page content"""
        thread_safe_print(f"{CYAN}{ARROW}{RESET} Generating profile page...")

        prompt = get_profile_generation_prompt(company_name)

        try:
            response = self.model_medium_temp.generate_content(pdf_parts + [prompt])
            profile_content = response.text.strip()
            thread_safe_print(f"{CYAN}{CHECK}{RESET} Initial profile complete")
            return profile_content

        except Exception as e:
            thread_safe_print(f"{RED}{CROSS}{RESET} Failed to generate profile: {e}")
            return None

    def run_completeness_check(self, initial_profile: str, pdf_parts: List) -> str:
        """Step 2: Check for missing data and investor perspectives"""
        thread_safe_print(f"\n{CYAN}Step 2: Completeness Check{RESET}")
        thread_safe_print(f"{CYAN}{ARROW}{RESET} Analyzing profile against source documents...")
        thread_safe_print(f"{CYAN}{ARROW}{RESET} Checking investor perspective completeness...")

        # Generate prompt with profile and section requirements
        prompt = get_completeness_check_prompt(initial_profile)
        # Replace placeholder with note about PDFs
        prompt = prompt.replace("{source_documents}", "[See attached PDF documents]")

        try:
            response = self.model_low_temp.generate_content(pdf_parts + [prompt])
            add_list = response.text.strip()

            # Count items in ADD list
            critical_count = add_list.count("[CRITICAL]")
            important_count = add_list.count("[IMPORTANT]")
            useful_count = add_list.count("[USEFUL]")
            total_items = critical_count + important_count + useful_count

            if "No critical gaps identified" in add_list or total_items == 0:
                thread_safe_print(f"{CYAN}{CHECK}{RESET} No critical gaps identified")
            else:
                thread_safe_print(f"{CYAN}{CHECK}{RESET} Identified {total_items} items to add")

            return add_list

        except Exception as e:
            thread_safe_print(f"{RED}{CROSS}{RESET} Completeness check failed: {e}")
            return "No additions needed due to error."

    def apply_enhancements(self, initial_profile: str, add_list: str, pdf_parts: List) -> str:
        """Step 3: Add missing content from ADD list"""
        thread_safe_print(f"\n{CYAN}Step 3: Enhancement{RESET}")
        thread_safe_print(f"{CYAN}{ARROW}{RESET} Adding missing content...")

        # Format the enhancement prompt
        prompt = get_enhancement_prompt(initial_profile, add_list)
        # Replace placeholder with note about PDFs
        prompt = prompt.replace("{source_documents}", "[See attached PDF documents]")

        try:
            response = self.model_medium_temp.generate_content(pdf_parts + [prompt])
            enhanced_profile = response.text.strip()

            # Check for empty output
            if not enhanced_profile or len(enhanced_profile) < 100:
                thread_safe_print(f"{YELLOW}{WARNING}{RESET} Enhancement produced minimal output, using initial profile")
                return initial_profile

            thread_safe_print(f"{CYAN}{CHECK}{RESET} Enhanced profile complete")
            return enhanced_profile

        except Exception as e:
            thread_safe_print(f"{RED}{CROSS}{RESET} Enhancement failed: {e}")
            return initial_profile

    def parse_sections(self, markdown: str) -> dict:
        """Parse markdown profile into sections"""
        sections = {}

        # Extract title and subtitle (everything before first ##)
        title_match = re.search(r'^(#\s+.+?\n.+?)(?=\n##)', markdown, re.MULTILINE | re.DOTALL)
        if title_match:
            sections['title_subtitle'] = title_match.group(1).strip()

        # Extract each ## section
        section_pattern = r'##\s+(.+?)\n(.*?)(?=\n##|\Z)'
        for match in re.finditer(section_pattern, markdown, re.MULTILINE | re.DOTALL):
            section_name = match.group(1).strip()
            section_content = match.group(2).strip()
            sections[section_name] = section_content

        return sections

    def count_words(self, text: str) -> int:
        """Count all words in text"""
        return len(text.split())

    def polish_section(self, section_name: str, content: str, word_limit: int) -> str:
        """Polish a single section to target word count"""
        if section_name == "title_subtitle":
            display_name = "Title & Subtitle"
        else:
            display_name = section_name

        word_count = self.count_words(content)

        if word_limit == 0:
            thread_safe_print(f"{CYAN}{ARROW}{RESET} Polishing {display_name}...")
        else:
            thread_safe_print(f"{CYAN}{ARROW}{RESET} Polishing {display_name} ({word_limit} words)...")

        prompt = get_polish_prompt(section_name, content, word_limit)

        try:
            response = self.model_medium_temp.generate_content(prompt)
            polished = response.text.strip()

            # Post-processing: Strip LLM preambles and duplicate section titles
            # Remove common preambles like "Here is the condensed X section:"
            polished = re.sub(r'^Here is .*?section:?\s*\n+', '', polished, flags=re.IGNORECASE)
            polished = re.sub(r'^This is .*?section:?\s*\n+', '', polished, flags=re.IGNORECASE)

            # Remove duplicate bold section name (e.g., "**Financial KPIs**")
            polished = re.sub(r'^\*\*' + re.escape(section_name) + r'\*\*\s*\n+', '', polished, flags=re.MULTILINE)

            # Remove "**SECTION: X**" pattern
            polished = re.sub(r'^\*\*SECTION:\s*' + re.escape(section_name) + r'\*\*\s*\n+', '', polished, flags=re.MULTILINE | re.IGNORECASE)

            # Strip any remaining leading/trailing whitespace
            polished = polished.strip()

            # Check for empty output
            if not polished or len(polished) < 50:
                thread_safe_print(f"{YELLOW}{WARNING}{RESET} Polish produced minimal output, using original")
                return content

            # Verify word limit for content sections
            if word_limit > 0:
                final_count = self.count_words(polished)
                if final_count > word_limit * 1.1:  # Allow 10% overage
                    thread_safe_print(f"{YELLOW}{WARNING}{RESET} Section exceeds limit ({final_count} words)")

            return polished

        except Exception as e:
            thread_safe_print(f"{RED}{CROSS}{RESET} Polish failed for {display_name}: {e}")
            return content

    def recombine_sections(self, sections: dict) -> str:
        """Recombine polished sections into final profile"""
        parts = []

        # Add title and subtitle
        if 'title_subtitle' in sections:
            parts.append(sections['title_subtitle'])

        # Add content sections in order
        section_order = ['Company Overview', 'Competitive Positioning', 'Financial KPIs', 'Strategic Considerations']
        for section_name in section_order:
            if section_name in sections:
                parts.append(f"\n## {section_name}\n{sections[section_name]}")

        return '\n'.join(parts)

    def save_step(self, step_name: str, content: str):
        """Save a step output to the run directory"""
        filepath = self.run_dir / f"{step_name}"
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(content)

    def save_run_log(self, company_name: str, status: str = "Success", pptx_path: str = None):
        """Save run log to the run directory"""
        log_path = self.run_dir / "run_log.txt"

        pptx_info = f"\n  - PowerPoint: {pptx_path}" if pptx_path else ""

        log_content = f"""OnePageProfile Run Log
{'='*60}

Timestamp: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}
Model: {self.model_name}
Company: {company_name}
Run Directory: {self.run_dir}

Source Files:
{chr(10).join(f"  - {Path(pdf).name}" for pdf in self.pdf_files)}

Pipeline Steps:
  1. step1_initial.md
  2. step2_completeness_check.txt
  3. step3_enhanced.md
  4. step4_final.md{pptx_info}

Status: {status}
"""

        with open(log_path, 'w', encoding='utf-8') as f:
            f.write(log_content)

    def run(self) -> Optional[Path]:
        """Execute the full 4-step profile generation and refinement pipeline"""
        try:
            # Prepare PDFs
            pdf_parts = self.prepare_pdf_parts()

            # Extract company name
            company_name = self.extract_company_name(pdf_parts)

            # ===== STEP 1: Initial Generation =====
            thread_safe_print(f"\n{CYAN}Step 1: Initial Generation{RESET}")
            initial_profile = self.generate_profile(pdf_parts, company_name)

            if not initial_profile:
                thread_safe_print(f"{RED}{CROSS}{RESET} Initial profile generation failed")
                self.save_run_log(company_name, "Failed at Step 1")
                return None

            self.save_step("step1_initial.md", initial_profile)

            # ===== STEP 2: Completeness Check =====
            add_list = self.run_completeness_check(initial_profile, pdf_parts)
            self.save_step("step2_completeness_check.txt", add_list)

            # ===== STEP 3: Enhancement =====
            enhanced_profile = self.apply_enhancements(initial_profile, add_list, pdf_parts)
            self.save_step("step3_enhanced.md", enhanced_profile)

            # ===== STEP 4: Polish (Section-by-Section) =====
            thread_safe_print(f"\n{CYAN}Step 4: Polish (Section-by-Section){RESET}")

            # Parse into sections
            sections = self.parse_sections(enhanced_profile)

            # Polish title and subtitle (no word limit)
            if 'title_subtitle' in sections:
                sections['title_subtitle'] = self.polish_section(
                    'title_subtitle',
                    sections['title_subtitle'],
                    word_limit=0
                )

            # Polish each content section (500 words each)
            section_order = ['Company Overview', 'Competitive Positioning', 'Financial KPIs', 'Strategic Considerations']
            for section_name in section_order:
                if section_name in sections:
                    sections[section_name] = self.polish_section(
                        section_name,
                        sections[section_name],
                        word_limit=500
                    )

            # Recombine sections
            final_profile = self.recombine_sections(sections)
            self.save_step("step4_final.md", final_profile)

            thread_safe_print(f"{CYAN}{CHECK}{RESET} Final profile complete")

            # Generate PowerPoint
            thread_safe_print(f"\n{CYAN}{ARROW}{RESET} Generating PowerPoint...")
            final_md_path = self.run_dir / "step4_final.md"

            try:
                pptx_path = create_profile_pptx(
                    md_path=str(final_md_path),
                    company_name=company_name,
                    timestamp=self.timestamp
                )
                thread_safe_print(f"{CYAN}{CHECK}{RESET} PowerPoint: {pptx_path}")
            except Exception as e:
                thread_safe_print(f"{YELLOW}{WARNING}{RESET} PowerPoint generation failed: {e}")
                pptx_path = None

            # Save run log
            self.save_run_log(company_name, "Success", pptx_path)

            # Return path to final output
            final_path = self.run_dir / "step4_final.md"

            thread_safe_print(f"\n{CYAN}{'='*60}{RESET}")
            thread_safe_print(f"{CYAN}{BOLD}Profile generation complete!{RESET}")
            thread_safe_print(f"{CYAN}{'='*60}{RESET}")
            thread_safe_print(f"\nOutput saved to: {self.run_dir}/")
            thread_safe_print(f"  - step1_initial.md")
            thread_safe_print(f"  - step2_completeness_check.txt")
            thread_safe_print(f"  - step3_enhanced.md")
            thread_safe_print(f"  - step4_final.md")

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

    thread_safe_print(f"ONEPAGEPROFILE {__version__}")
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

    # File selection
    pdf_files = select_pdf_files()
    if not pdf_files:
        thread_safe_print(f"{RED}{CROSS}{RESET} No files selected. Exiting.")
        sys.exit(1)

    thread_safe_print(f"\n{CYAN}Starting profile generation...{RESET}")
    thread_safe_print("="*60 + "\n")

    # Generate profile
    maker = OnePageProfile(pdf_files, selected_model)
    profile_path = maker.run()

    if not profile_path:
        thread_safe_print(f"\n{RED}Profile generation failed.{RESET}")
        sys.exit(1)

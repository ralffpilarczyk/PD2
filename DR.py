"""
Deep Research - Standalone web research tool
Uses Google's Gemini Deep Research Agent for company research.
"""

import argparse
import os
import re
import shutil
import sys
from datetime import datetime
from pathlib import Path

from src.deep_research import DeepResearcher, ResearchDisplay
from src.research_sections import get_research_sections

__version__ = "1.1"


def ensure_caffeinate():
    """Re-exec under caffeinate on macOS to prevent sleep during long research."""
    if sys.platform != 'darwin':
        return  # Not macOS

    if os.environ.get('_DR_CAFFEINATED'):
        return  # Already running under caffeinate

    # Re-exec under caffeinate -i (prevent idle sleep)
    os.environ['_DR_CAFFEINATED'] = '1'
    args = ['caffeinate', '-i', sys.executable] + sys.argv
    os.execvp('caffeinate', args)

RESET = '\033[0m'
BOLD = '\033[1m'
CYAN = '\033[96m'
GREEN = '\033[92m'
YELLOW = '\033[93m'
RED = '\033[91m'
CHECK = '\u2713'
WARNING = '\u26A0'


def scan_completed_sections(run_dir: Path, all_sections: list) -> tuple[dict, list]:
    """
    Scan a run directory to find completed vs missing sections.
    Returns (completed_results, missing_sections).
    """
    completed = {}
    missing = []

    for section in all_sections:
        section_num = section['number']
        title = section['title']

        # Build expected filename pattern
        safe_title = title.lower().replace(' ', '_').replace('&', 'and')
        safe_title = ''.join(c for c in safe_title if c.isalnum() or c == '_')
        filename = f"section_{section_num:02d}_{safe_title}.md"
        filepath = run_dir / filename

        if filepath.exists():
            # Read the file and extract content (skip header lines)
            content = filepath.read_text(encoding='utf-8')
            # Skip the header (first 5 lines: title, blank, company, status, separator)
            lines = content.split('\n')
            if len(lines) > 5:
                actual_content = '\n'.join(lines[5:])
            else:
                actual_content = content

            # Check if it's a failed result
            if '**Status**: failed' in content or '**Status**: timeout' in content:
                missing.append(section)
            else:
                completed[section_num] = {
                    'number': section_num,
                    'title': title,
                    'content': actual_content,
                    'status': 'completed'
                }
        else:
            missing.append(section)

    return completed, missing


def extract_company_from_run(run_dir: Path) -> str:
    """Extract company name from an existing section file in the run directory."""
    for f in run_dir.glob("section_*.md"):
        content = f.read_text(encoding='utf-8')
        match = re.search(r'\*\*Company\*\*:\s*(.+)', content)
        if match:
            return match.group(1).strip()
    return None


def print_header():
    print(f"\n{BOLD}DEEP RESEARCH {__version__}{RESET}")
    print("=" * 60)
    print("Web-based company research using Google Deep Research Agent")
    print("=" * 60 + "\n")


def prompt_single_digit(prompt_text: str, valid_digits: str, default_digit: str) -> str:
    while True:
        print(prompt_text, end='', flush=True)
        try:
            import termios
            import tty
            fd = sys.stdin.fileno()
            old_settings = termios.tcgetattr(fd)
            try:
                tty.setraw(fd)
                ch = sys.stdin.read(1)
            finally:
                termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
        except (ImportError, termios.error):
            ch = input().strip()
            if not ch:
                print(default_digit)
                return default_digit
            if ch in valid_digits:
                return ch
            continue

        if ch in ('\r', '\n'):
            print(default_digit)
            return default_digit
        print(ch)
        if ch in valid_digits:
            return ch


def main():
    print("\033[2J\033[H", end='')  # Clear terminal
    print_header()

    # Parse command-line arguments
    parser = argparse.ArgumentParser(description='Deep Research - Web-based company research')
    parser.add_argument('--resume', type=str, metavar='RUN_DIR',
                        help='Resume an incomplete run from the specified directory')
    args = parser.parse_args()

    research_sections = get_research_sections()

    # Resume mode
    if args.resume:
        run_dir = Path(args.resume)
        if not run_dir.exists():
            print(f"{RED}ERROR:{RESET} Run directory not found: {run_dir}")
            sys.exit(1)

        # Extract company name from existing files
        company_name = extract_company_from_run(run_dir)
        if not company_name:
            print(f"{RED}ERROR:{RESET} Could not extract company name from run directory")
            sys.exit(1)

        # Scan for completed vs missing sections
        completed_results, missing_sections = scan_completed_sections(run_dir, research_sections)

        print(f"{CYAN}{CHECK}{RESET} Resuming run: {run_dir}")
        print(f"{CYAN}{CHECK}{RESET} Company: {company_name}")
        print(f"{CYAN}{CHECK}{RESET} Completed: {len(completed_results)}/{len(research_sections)} sections")

        if not missing_sections:
            print(f"\n{GREEN}All sections already complete!{RESET}")
            # Just regenerate the combined report
            researcher = DeepResearcher(company_name, 1, run_dir)
            researcher.combine_research(completed_results)
        else:
            print(f"{YELLOW}{WARNING}{RESET} Missing: {len(missing_sections)} sections")
            for s in missing_sections:
                print(f"    - Section {s['number']}: {s['title']}")

            # Workers
            print("\nResearch workers (parallel queries):")
            print("  1-2 workers (default 2)")
            workers_choice = prompt_single_digit("Choose workers [1-2] (default 2): ", valid_digits="12", default_digit="2")
            workers = int(workers_choice)
            print(f"{CYAN}{CHECK}{RESET} Workers: {workers}\n")

            # Run research for missing sections only
            print(f"{CYAN}Resuming Deep Research for {company_name}...{RESET}")
            print("=" * 60 + "\n")

            researcher = DeepResearcher(company_name, workers, run_dir)
            display = ResearchDisplay(len(missing_sections))
            new_results = researcher.run_all_sections(missing_sections, display)

            # Merge with existing results
            results = {**completed_results, **new_results}

            # Summary
            summary = researcher.get_research_summary(results)
            print(f"\n{CYAN}{CHECK}{RESET} Deep Research complete: {summary['completed']}/{summary['total']} topics")

            if summary['failed'] > 0:
                print(f"{YELLOW}{WARNING}{RESET} {summary['failed']} topics still failed")

            # Combine all results
            researcher.combine_research(results)

        print(f"{CYAN}{CHECK}{RESET} Output saved to: {run_dir}/\n")

        # Save final report to ReportsDR
        run_timestamp = run_dir.name.replace('dr_', '')
        reports_dir = Path("ReportsDR")
        reports_dir.mkdir(exist_ok=True)
        safe_company = company_name.replace(" ", "_").replace("/", "_").replace("(", "").replace(")", "")
        safe_company = ''.join(c for c in safe_company if c.isalnum() or c == '_')
        report_filename = f"{safe_company}_DR_{run_timestamp}.md"
        report_path = reports_dir / report_filename

        combined_path = run_dir / "combined_research.md"
        if combined_path.exists():
            shutil.copy(combined_path, report_path)
            print(f"{CYAN}{CHECK}{RESET} Report saved to: {report_path}\n")

    # Normal mode (new run)
    else:
        # Company name
        company_name = input("Company name: ").strip()
        if not company_name:
            print(f"{RED}ERROR:{RESET} Company name is required")
            sys.exit(1)
        print(f"{CYAN}{CHECK}{RESET} Company: {company_name}\n")

        # Workers
        print("Research workers (parallel queries):")
        print("  1-2 workers (default 2)")
        workers_choice = prompt_single_digit("Choose workers [1-2] (default 2): ", valid_digits="12", default_digit="2")
        workers = int(workers_choice)
        print(f"{CYAN}{CHECK}{RESET} Workers: {workers}\n")

        # Create run directory
        run_timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        run_dir = Path(f"runs/dr_{run_timestamp}")
        run_dir.mkdir(parents=True, exist_ok=True)

        # Run research
        print(f"{CYAN}Starting Deep Research for {company_name}...{RESET}")
        print("=" * 60 + "\n")

        researcher = DeepResearcher(company_name, workers, run_dir)

        display = ResearchDisplay(len(research_sections))
        results = researcher.run_all_sections(research_sections, display)

        # Summary
        summary = researcher.get_research_summary(results)
        print(f"\n{CYAN}{CHECK}{RESET} Deep Research complete: {summary['completed']}/{summary['total']} topics")

        if summary['failed'] > 0:
            print(f"{YELLOW}{WARNING}{RESET} {summary['failed']} topics failed")

        # Combine results
        researcher.combine_research(results)
        print(f"{CYAN}{CHECK}{RESET} Output saved to: {run_dir}/\n")

        # Save final report to ReportsDR
        reports_dir = Path("ReportsDR")
        reports_dir.mkdir(exist_ok=True)
        safe_company = company_name.replace(" ", "_").replace("/", "_").replace("(", "").replace(")", "")
        safe_company = ''.join(c for c in safe_company if c.isalnum() or c == '_')
        report_filename = f"{safe_company}_DR_{run_timestamp}.md"
        report_path = reports_dir / report_filename

        # Copy combined report to ReportsDR
        combined_path = run_dir / "combined_research.md"
        if combined_path.exists():
            shutil.copy(combined_path, report_path)
            print(f"{CYAN}{CHECK}{RESET} Report saved to: {report_path}\n")

    # List output files
    print(f"{BOLD}Output files:{RESET}")
    for f in sorted(run_dir.glob("*.md")):
        print(f"  {f.name}")


if __name__ == "__main__":
    ensure_caffeinate()
    main()

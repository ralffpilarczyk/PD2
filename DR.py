"""
Deep Research - Standalone web research tool
Uses Google's Gemini Deep Research Agent for company research.
"""

import sys
from datetime import datetime
from pathlib import Path

from src.deep_research import DeepResearcher, ResearchDisplay
from src.research_sections import get_research_sections

__version__ = "1.0"

RESET = '\033[0m'
BOLD = '\033[1m'
CYAN = '\033[96m'
GREEN = '\033[92m'
YELLOW = '\033[93m'
RED = '\033[91m'
CHECK = '\u2713'
WARNING = '\u26A0'


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
    print_header()

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
    research_sections = get_research_sections()

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

    # List output files
    print(f"{BOLD}Output files:{RESET}")
    for f in sorted(run_dir.glob("*.md")):
        print(f"  {f.name}")
    research_dir = run_dir / "deep_research"
    if research_dir.exists():
        for f in sorted(research_dir.glob("*.md")):
            print(f"  deep_research/{f.name}")


if __name__ == "__main__":
    main()

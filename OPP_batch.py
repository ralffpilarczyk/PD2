#!/usr/bin/env python3
"""
Batch processor for OnePageProfile
Processes all PDFs in a directory sequentially with user-selected settings
"""

from pathlib import Path
from OPP import OnePageProfile
import sys
from datetime import datetime
import os

# Import utilities from OPP for consistent UI
from src.utils import thread_safe_print

# ANSI Color Codes and Styling (matching OPP.py)
RESET = '\033[0m'
BOLD = '\033[1m'
RED = '\033[91m'
CYAN = '\033[96m'
CHECK = '✓'
CROSS = '✗'

# UI Helper Functions (from OPP.py)
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

# SETTINGS
PDF_DIRECTORY = "SourceFiles/SourceBatch"

def main():
    # Clear terminal screen
    print("\033[2J\033[H", end='')

    thread_safe_print("BATCH OPP PROCESSOR v1.2")
    thread_safe_print("="*60)

    # Profile type selection (same as OPP.py)
    thread_safe_print(f"\n{BOLD}Select profile type:{RESET}")
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

    # Model selection (same as OPP.py)
    thread_safe_print("Select LLM model:")
    thread_safe_print("  1) gemini-3-flash-preview")
    thread_safe_print("  2) gemini-3-pro-preview")
    choice = prompt_single_digit("Choose model [1/2] (default 1): ", valid_digits="12", default_digit="1")
    if choice == "1":
        selected_model = 'gemini-3-flash-preview'
    else:
        selected_model = 'gemini-3-pro-preview'
    thread_safe_print(f"{CYAN}{CHECK}{RESET} Selected: {selected_model}\n")

    # Worker selection (same as OPP.py)
    thread_safe_print("Select number of parallel workers:")
    thread_safe_print("  1-4 workers (default 4)")
    workers_choice = prompt_single_digit("Choose workers [1-4] (default 4): ", valid_digits="1234", default_digit="4")
    num_workers = int(workers_choice)
    thread_safe_print(f"{CYAN}{CHECK}{RESET} Workers: {num_workers}\n")

    # Iterations selection (same as OPP.py)
    thread_safe_print("Select number of density iterations:")
    thread_safe_print("  1-3 iterations (default 1)")
    iterations_choice = prompt_single_digit("Choose iterations [1-3] (default 1): ", valid_digits="123", default_digit="1")
    num_iterations = int(iterations_choice)
    thread_safe_print(f"{CYAN}{CHECK}{RESET} Iterations: {num_iterations}\n")

    # Display batch settings
    thread_safe_print("="*60)
    thread_safe_print("BATCH PROCESSING CONFIGURATION")
    thread_safe_print("="*60)
    thread_safe_print(f"Profile type: {profile_type}")
    thread_safe_print(f"Model: {selected_model}")
    thread_safe_print(f"Workers: {num_workers}")
    thread_safe_print(f"Iterations: {num_iterations}")
    thread_safe_print(f"Directory: {PDF_DIRECTORY}")
    thread_safe_print("="*60 + "\n")

    # Find all PDFs
    pdf_dir = Path(PDF_DIRECTORY)
    if not pdf_dir.exists():
        print(f"ERROR: Directory not found: {PDF_DIRECTORY}")
        sys.exit(1)

    pdfs = sorted(pdf_dir.glob("*.pdf"))

    if not pdfs:
        print(f"ERROR: No PDF files found in {PDF_DIRECTORY}")
        sys.exit(1)

    print(f"Found {len(pdfs)} PDF files to process\n")

    # Track results
    results = []

    # Process each PDF sequentially
    for i, pdf in enumerate(pdfs, 1):
        print("\n" + "="*60)
        print(f"[{i}/{len(pdfs)}] Processing: {pdf.name}")
        print("="*60)

        start_time = datetime.now()

        try:
            # Create OnePageProfile instance with user-selected parameters
            maker = OnePageProfile([str(pdf)], selected_model, workers=num_workers, iterations=num_iterations, profile_type=profile_type)

            # Run the profile generation
            profile_path = maker.run()

            elapsed = (datetime.now() - start_time).total_seconds()

            if profile_path:
                print(f"\n✓ SUCCESS: {pdf.name}")
                print(f"  Output: {profile_path}")
                print(f"  Time: {elapsed:.1f}s")
                results.append({"file": pdf.name, "status": "success", "time": elapsed, "output": profile_path})
            else:
                print(f"\n✗ FAILED: {pdf.name} (no output generated)")
                print(f"  Time: {elapsed:.1f}s")
                results.append({"file": pdf.name, "status": "failed", "time": elapsed, "error": "No output"})

        except Exception as e:
            elapsed = (datetime.now() - start_time).total_seconds()
            print(f"\n✗ ERROR: {pdf.name}")
            print(f"  Error: {str(e)}")
            print(f"  Time: {elapsed:.1f}s")
            results.append({"file": pdf.name, "status": "error", "time": elapsed, "error": str(e)})

    # Print summary
    print("\n\n" + "="*60)
    print("BATCH PROCESSING COMPLETE")
    print("="*60)

    successes = [r for r in results if r["status"] == "success"]
    failures = [r for r in results if r["status"] in ("failed", "error")]

    print(f"\nTotal: {len(results)}")
    print(f"Success: {len(successes)}")
    print(f"Failed: {len(failures)}")

    if failures:
        print("\nFailed files:")
        for r in failures:
            error_msg = r.get("error", "Unknown")
            print(f"  - {r['file']}: {error_msg}")

    total_time = sum(r["time"] for r in results)
    print(f"\nTotal processing time: {total_time/60:.1f} minutes")
    print("="*60 + "\n")

if __name__ == "__main__":
    main()

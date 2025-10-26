#!/usr/bin/env python3
"""
Batch processor for OnePageProfile
Processes all PDFs in a directory sequentially with fixed settings
"""

from pathlib import Path
from OPP import OnePageProfile
import sys
from datetime import datetime

# SETTINGS
MODEL = "gemini-2.5-pro"
WORKERS = 4
PDF_DIRECTORY = "SourceFiles/SourceBatch"

def main():
    # Clear terminal screen
    print("\033[2J\033[H", end='')

    print("\n" + "="*60)
    print("BATCH OPP PROCESSOR")
    print("="*60)
    print(f"Model: {MODEL}")
    print(f"Workers: {WORKERS}")
    print(f"Directory: {PDF_DIRECTORY}")
    print("="*60 + "\n")

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
            # Create OnePageProfile instance
            maker = OnePageProfile([str(pdf)], MODEL, workers=WORKERS)

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

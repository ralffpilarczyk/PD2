# ProfileDash 2.0 (PD2) - Project Handover Document

## Table of Contents
1. [Executive Summary](#executive-summary)
2. [System Overview](#system-overview)
3. [Technical Architecture](#technical-architecture)
4. [Analysis Pipeline](#analysis-pipeline)
5. [Development History](#development-history)
6. [Recent Session Work](#recent-session-work)
7. [Current Issues](#current-issues)
8. [Quick Start Guide](#quick-start-guide)

## Executive Summary

ProfileDash 2.0 is an intelligent document analysis system that processes PDF financial documents (annual reports, financial statements, investor presentations) and generates comprehensive company profiles with 32 specialized analytical sections. Built for investment professionals, it uses Google's Gemini 2.5 Flash API to produce insights focused on value creation and business prospects.

**Key Capabilities:**
- Processes multiple PDFs/markdown files in parallel
- Generates 32 different analytical sections (Operating Footprint → Capital Allocation)
- Multi-step refinement process condensing to ~500 words per section
- Learning system that captures and reuses analytical patterns
- Professional HTML output with tables and formatting

## System Overview

### Core Components

```
PD2/
├── PD2.py                    # Main orchestrator & UI
├── src/
│   ├── core_analyzer.py      # 4-6 step analysis pipeline
│   ├── profile_generator.py  # HTML generation & markdown processing
│   ├── insight_memory.py     # Learning system & pattern capture
│   ├── file_manager.py       # File I/O & markdown preprocessing
│   ├── quality_tracker.py    # Metrics & quality scoring
│   ├── profile_sections.py   # 32 section definitions
│   └── utils.py             # Rate limiting, thread safety, table fixes
├── memory/
│   └── learning_memory.json  # Accumulated analytical patterns
└── runs/
    └── run_YYYY_MM_DD_*/     # Analysis outputs
```

### Key Technologies
- **LLM**: Google Gemini 2.5 Flash (via API) with optional warm-up
- **PDF Processing**: Marker library (with PyTorch) using small process pools for safety
- **Markdown→HTML**: Python-Markdown with extensions
- **Parallel Processing**: ThreadPoolExecutor (sections), ProcessPoolExecutor (Marker)
- **Rate Limiting**: Exponential backoff with retry; tunable concurrency

## Technical Architecture

### 1. Input Processing Flow
```
PDFs → (Cache hit?) → If yes: Copy cached .md
         If no: Marker (process pool) → Markdown → Clean Tables → Update cache
                                      ↓
       Existing Markdown Files → Clean Tables → Load into Memory
```

### 2. Analysis Pipeline (Per Section)
```
Step 1: Initial Draft (100K char limit)
   ↓
Step 2: Completeness Check (identifies missing data)
   ↓
Step 3: Enhanced Draft (50K char limit, adds missing data)
   ↓
Step 4: Deep Analysis & Polish (10K char limit, ~500 words)
   ↓
Step 5: (Optional) Discovery Pipeline (6 stages for deeper insights)
   ↓
Step 6: Learning Extraction (captures patterns)
```

### 3. Output Generation
```
Phase 1 (Sections 1–31):
  Section Outputs → Collect Best Version → Fix Markdown Issues → 
  Generate HTML (core profile) → Deliver immediately

Phase 2 (Section 32 if selected):
  Run Appendix → Stitch into combined markdown → Regenerate HTML
```

### 4. Memory System
- Captures analytical methodologies (not company data)
- Section-specific patterns (max 30 words each)
- Quality score filtering (9-10 only)
- Harsh deduplication to prevent repetition

## Analysis Pipeline

### Standard Workflow (5 LLM calls per section)

1. **Initial Draft** (temp 0.6)
   - Comprehensive analysis with all relevant data
   - Target: ~1000 words
   - Includes tables, footnotes, insights

2. **Completeness Check** (temp 0.2)
   - Compares draft against source documents
   - Identifies missing critical data
   - Returns ADD list (max 5 items)

3. **Enhanced Draft** (temp 0.6)
   - Incorporates missing elements
   - Maintains narrative flow
   - Often exceeds 1000 words

4. **Deep Analysis & Polish** (temp 0.6)
   - Condenses to ~500 words maximum
   - Preserves only value-affecting insights
   - Maintains at least one table

5. **Learning Extraction** (temp 0.2)
   - Extracts transferable methodologies
   - JSON format with techniques, patterns, validations

### Discovery Pipeline (Optional, +8 LLM calls)

When enabled, adds 6-stage pattern finding:
1. Extract all quantifiable data
2. Calculate material relationships
3. Identify top 2-3 anomalies
4. Investigate root causes
5. Assess business impact
6. Generate comprehensive insight

Then augments Step 4 output with 1-2 critical insights maximum.

### Special Handling: Section 32 (Appendix)
- Data-only extraction (no analysis)
- Skips steps 2-5
- Focus on 15-20 most important tables
- 500KB character limit (vs 100KB for others)
- Two-phase scheduling: appendix runs after the core profile is delivered; failures here no longer block delivery of Sections 1–31

## Development History

### Original Implementation
- 32 sections covering all aspects of company analysis
- Progressive refinement approach
- Thread-safe parallel processing
- Learning memory system
- Professional HTML output

### Known Challenges Addressed
1. **Rate Limiting**: Implemented exponential backoff with intelligent retry
2. **Memory Management**: Added harsh quality filtering (9-10 scores only)
3. **Table Corruption**: Marker library creates malformed tables
4. **Output Size**: LLMs sometimes generate massive outputs
5. **Thread Safety**: Parallel processing caused garbled output

### New Performance & UX Improvements (2025‑08‑16)
1. **Configurable LLM Concurrency**
   - LLM_MAX_INFLIGHT (default 4) caps in-flight model calls across threads.
   - MAX_SECTION_WORKERS (default 3, max 8) controls parallel section workers.
   - SUBMISSION_STAGGER_SEC (default 0.5) staggers task submission to reduce rate-limit spikes.

2. **Marker Process Pool + Conversion Cache**
   - MARKER_PROCESS_WORKERS (default 2, max 5) runs Marker in separate processes to avoid PyTorch state collisions.
   - BLAS threads per process pinned to 1 (OMP/MKL) to prevent oversubscription.
   - Hash-based cache at `memory/conversion_cache/` (SHA‑256 → `.md`) skips re-conversion for unchanged PDFs.

3. **Two-Phase Scheduling for Section 32**
   - Phase 1 runs all non‑32 sections and generates the core HTML profile immediately.
   - Phase 2 runs Section 32 alone and regenerates the final HTML with the appendix.
   - Prevents slow/failed Appendix from blocking delivery.

4. **Optional LLM Warm‑Up**
   - One tiny call at startup to remove first-call latency spikes; continues even if warm-up fails.

5. **Single‑Key (y/n) Prompts**
   - Early CLI prompts accept a single keypress (no Enter), with cross-platform fallback.

## Recent Session Work

### Critical Issues Fixed

#### 1. Catastrophic Output Failure (run_2025_08_02_19_48_38)
**Problem**: LLM generated 1.3MB+ initial drafts, breaking entire pipeline
**Solution**: 
- Added output size limits (100KB for step 1, 50KB for step 3, 10KB for step 4)
- Added truncation with natural break points
- Special handling for Section 32 (500KB limit)

#### 2. Thread Safety Issues
**Problem**: Parallel processing caused garbled console output
**Solution**:
- Created `thread_safe_print()` function with mutex lock
- Replaced ALL print() statements throughout codebase
- Added to utils.py for centralized access

#### 3. Markdown Table Corruption
**Problem**: Marker library generates tables with millions of dashes/colons
**Solution**:
- Added `clean_markdown_tables()` to fix on file load
- Added `validate_and_fix_tables()` for size constraints (10 cols, 20 rows)
- Added LLM instructions to not reproduce corrupted tables

#### 4. HTML Rendering Failures
**Problem**: Tables and Section 32 not rendering in HTML
**Root Causes**:
- Tables missing required blank lines before them
- Section 32 wrapped in ````html` code blocks
- Footnotes using wrong syntax `[^1]` instead of `[1]`
- Table separators corrupted (`|---|---|` instead of `| :--- | :--- |`)

**Solutions Implemented**:
- Auto-add blank lines before tables and lists
- Remove code block wrappers from Section 32
- Convert footnote formats
- Fix table separator alignment markers
- Comprehensive markdown cleanup in `_clean_markdown_content()`

#### 5. Workflow Improvements
**Problem**: Inefficient user interaction flow
**Solution**:
- Reordered: File selection → Section selection → All options → Process
- Added single-key y/n for early prompts (faster navigation)
- All user input happens upfront
- PDF conversion now supports safe process-level workers (see "Marker Process Pool")

#### 6. PDF Conversion Issues
**Problem**: Parallel PDF conversion fails with PyTorch tensor errors
**Solution**:
- Use a small ProcessPoolExecutor (default 2) for Marker conversions; BLAS threads constrained to 1
- Hash-based caching to avoid re-running Marker for unchanged PDFs
- Parallel section analysis retained with tunable caps (see environment variables)

#### 7. Section Failures
**Problems Observed**:
- Section 3: Empty due to API 500 error
- Section 5: Malformed tables caused empty Steps 3-4
- Section 6: Footnotes not rendering (wrong format)
- Section 12: 504 timeout but content preserved
- Section 32: 504 timeout, no content generated

**Recovery Mechanisms Added**:
- Fallback to previous step if current step fails
- Empty output detection and recovery
- Graceful handling of API errors

### Configuration Updates

#### Truncation Limits (Current)
- Step 1: 100,000 characters (was 50K)
- Step 3: 50,000 characters (was 30K)  
- Step 4: 10,000 characters (unchanged, ~500 words)
- Section 32: 500,000 characters

#### UI/UX Improvements
- Merged section 13-14 from Company Profile to Strategy/SWOT group
- Added comprehensive error messages with section context
- Improved progress tracking with step indicators

### Documentation Updates
- Updated CLAUDE.md for public repository
- Rewrote README.md with clearer instructions
- Cleaned .env.example (removed unused MAX_WORKERS)
- Added troubleshooting section

## Current Issues

### 1. Section 32 Timeouts
- Large document sets can still cause timeouts, but two-phase scheduling ensures core output is delivered first.
- Appendix runs in isolation with its own resource envelope and can be retried without redoing core sections.

### 2. API Failures
- Occasional 500/504 errors from Gemini API
- Rate limiting despite backoff implementation
- Some sections may need re-running

### 3. Table Rendering Edge Cases
- Some tables still don't render if title formatting is unusual
- Very wide tables get truncated
- Nested formatting in cells can break

### 4. Memory Growth
- Learning memory can become repetitive over time
- Harsh filtering helps but isn't perfect
- May need periodic manual cleanup

## Quick Start Guide

### For Testing Latest Fixes

```bash
# 1. Ensure environment is set up
cd /Users/ralfpilarczyk/Documents/Python/PD2
source venv/bin/activate

# 2. (Optional) Tune concurrency safely via env vars
export LLM_MAX_INFLIGHT=4            # in-flight LLM calls (1-16)
export MAX_SECTION_WORKERS=3         # parallel section workers (1-8)
export SUBMISSION_STAGGER_SEC=0.5    # seconds between submissions
export MARKER_PROCESS_WORKERS=2      # Marker process pool (1-5)

# 3. Run with existing markdown files (faster)
python PD2.py
# Select existing .md files from previous runs
# Choose sections to test (recommend 1-6 for quick test)
# Discovery pipeline optional (y/n); single-key y/n works

# 4. Check output
open runs/run_[timestamp]/[Company]_profile.html
```

### For Debugging Issues

1. **Check run summary**: `runs/run_*/run_summary.md`
2. **Check individual sections**: `runs/run_*/section_*/`
3. **Look for patterns**:
   - Empty files = API failure
   - Truncation warnings = Size limits hit
   - "Fixed X tables" = Corruption handling working

### Key Code Locations

**For HTML/Table Issues**:
- `src/profile_generator.py`: `_clean_markdown_content()` method
- `src/utils.py`: `clean_markdown_tables()` and `validate_and_fix_tables()`

**For API/Processing Issues**:
- `src/core_analyzer.py`: All step methods and size limits
- `src/utils.py`: `retry_with_backoff()` for rate limiting
  and global LLM concurrency gate (LLM_MAX_INFLIGHT)

**For PDF Conversion**:
- `PD2.py`: Process-pool Marker conversion, cache, and BLAS thread constraints
- `memory/conversion_cache/`: Hash-keyed converted markdown files

**For Section Definitions**:
- `src/profile_sections.py`: All 32 section specs

### Testing Checklist

After any changes, verify:
1. Tables render in HTML (not as markdown)
2. Section 32 shows as formatted content (not code)
3. Console output is clean (no garbled text)
4. Footnotes appear properly
5. All selected sections appear in final HTML
6. When Section 32 is selected, verify core profile appears first; appendix stitched afterwards

### Common Commands

```bash
# View latest run
ls -la runs/ | tail -5

# Check for errors
grep -i error runs/run_*/run_summary.md

# See what sections completed
grep "completed successfully" runs/run_*/run_summary.md

# Find truncation warnings
grep "WARNING.*exceeded" runs/run_*/run_summary.md
```

## Final Notes

The system is functional and more resilient around:
1. Large document sets: Appendix is isolated via two-phase scheduling
2. API rate limits: Concurrency is tunable; stagger reduces spikes
3. Markdown edge cases: Existing validation and cleanup retained

Recent changes focus on delivery speed and operator control without altering analytical quality. Continue to apply the multi-layer approach: fix at source (LLM instructions), fix during processing (table validation), and fix during output (markdown cleanup).

Good luck with the continued development!
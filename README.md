# ProfileDash 2.2 & OnePageProfile 1.4

Intelligent document analysis tools for M&A and investment analysis:
- **PD2** (ProfileDash 2.2): Comprehensive 34-section company profiles with deep analytical insights
- **OPP** (OnePageProfile v1.4): Concise one-page profiles for quick M&A evaluation

## Quick Start

1. **Install**:
   ```bash
   git clone https://github.com/yourusername/PD2.git
   cd PD2
   pip install -r requirements.txt
   ```

2. **Configure**:
   ```bash
   cp .env.example .env
   # Add your Gemini API key to .env file
   ```

3. **Run**:
   ```bash
   python PD2.py      # For comprehensive 34-section profiles
   python OPP.py      # For one-page M&A profiles
   ```

## ProfileDash 2.2 (PD2)

Comprehensive 34-section company analysis for deep M&A and investment evaluation.

**Usage:**

1. Run `python PD2.py`
2. Select model: Gemini Flash or Pro
3. Select PDFs when prompted
4. Choose section groups to analyze:
   - Company Profile (sections 1-13)
   - Strategy and SWOT (sections 14-19)
   - Sellside Positioning (sections 20-26)
   - Buyside Due Diligence (sections 27-32)
   - Financial Pattern Analysis (section 33)
   - Data Book (section 34)
5. Set parallel workers (1-5, default 2)
6. Wait for processing (~30-60 minutes for full profile)

**Output:**

Professional PDF report saved to `ReportsPD2/`:
- `[Company]_YYMMDD_HHMM.pdf` - Final report with footers and page numbers

Analysis work products saved to `runs/run_YYYY_MM_DD_HH_MM_SS/`:
- `[Company]_profile.md` - Combined markdown
- `section_*/` - Individual section drafts and refinements
- `run_summary.txt` - Processing log

**Architecture:**
- Multi-step analysis pipeline with progressive refinement
- Learning system captures analytical patterns (separate memories for PD2 and OPP)
- Parallel processing with configurable workers
- Automatic retry with exponential backoff for API limits
- Thread-safe operations for concurrent processing

## OnePageProfile v1.4 (OPP)

Concise one-page company profiles for rapid M&A screening and evaluation.

**Usage:**
```bash
python OPP.py
```

1. Select file source: Interactive (default) or Batch directory
2. Select model: Gemini Flash or Pro
3. Set parallel workers (1-4, default 4)
4. Enable insights pipeline (optional)
5. Select PDFs (interactive) or confirm batch processing
6. Wait for processing (~3-5 minutes vanilla, ~8-10 minutes with insights)

**Batch Processing:**

Batch mode processes all PDFs in `SourceFiles/SourceBatch/` sequentially:
- Select option 2 at file source prompt
- Confirm batch configuration before processing
- Progress tracking and summary report at completion

**Output:**

Vanilla mode (insights disabled):
- Markdown: `runs/opp_TIMESTAMP/final_profile.md`
- PowerPoint: `ReportsOPP/[Company]_YYMMDD_HHMMSS.pptx`

Insights mode (insights enabled):
- Three PPTX variants for validation:
  - `[Company]_vanilla_YYMMDD_HHMMSS.pptx` - Descriptive baseline (Step 5)
  - `[Company]_insights_YYMMDD_HHMMSS.pptx` - Ground truth discoveries (Step 9)
  - `[Company]_integrated_YYMMDD_HHMMSS.pptx` - Final deliverable (Step 12)

**Format:**
- Title and subtitle (up to 8 words, prose statement)
- 4 sections: Company Overview, Competitive Positioning, Financial KPIs, Strategic Considerations
- Each section limited to ~100 words with bullet points
- Temporal bias: prioritizes most recent fiscal data over historical data

**Architecture (v1.4):**

12-step pipeline with optional insights:

| Steps 1-5 | Descriptive Track | Vanilla output |
|-----------|-------------------|----------------|
| Steps 6-9 | Ground Truth Track | Insights output |
| Steps 10-12 | Integration Track | Integrated output |

- Steps 1-5: Draft, Check, Enhance, Clean-up, Polish
- Steps 6-9: Ground Truth, Hypothesize, Test, Synthesize (insights enabled)
- Steps 10-12: Integrate, Clean-up 2, Polish 2 (insights enabled)
- Parallel execution across 4 sections with configurable workers

## Requirements

- Python 3.8+
- Gemini API key (free at https://makersuite.google.com/app/apikey)
- 8GB RAM minimum

**Learning System (PD2 only):**
PD2 learns and improves over time:
- Memory: `memory/pd2_learning_memory.json` (34 sections, detailed analysis)
- Archives: `memory/memory_library/pd2_memory_{timestamp}.json`
- Extracts universal analytical methodologies (9-10/10 quality only)
- Applies learned patterns to future runs

OPP v1.4 does not use learning for simplicity and speed.

## License

MIT License - see LICENSE file for details.
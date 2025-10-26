# ProfileDash 2.1 & OnePageProfile 1.1

Intelligent document analysis tools for M&A and investment analysis:
- **PD2** (ProfileDash 2.1): Comprehensive 33-section company profiles with deep analytical insights
- **OPP** (OnePageProfile v1.1): Concise one-page profiles for quick M&A evaluation

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
   python PD2.py      # For comprehensive 33-section profiles
   python OPP.py      # For one-page M&A profiles
   ```

## ProfileDash 2.1 (PD2)

Comprehensive 33-section company analysis for deep M&A and investment evaluation.

**Usage:**

1. Run `python PD2.py`
2. Select model: Gemini Flash or Pro
3. Select PDFs when prompted
4. Choose section groups to analyze:
   - Company Profile (sections 1-13)
   - Strategy and SWOT (sections 14-19)
   - Sellside Positioning (sections 20-26)
   - Buyside Due Diligence (sections 27-32)
   - Data Book (section 33)
5. Set parallel workers (1-5, default 2)
6. Wait for processing (~30-60 minutes for full profile)

**Output:**

Professional PDF report saved to `ReportFiles/`:
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

## OnePageProfile v1.1 (OPP)

Concise one-page company profiles for rapid M&A screening and evaluation.

**Interactive Usage:**
```bash
python OPP.py
```

1. Select model: Gemini Flash or Pro
2. Set parallel workers (1-4, default 4)
3. Select PDFs when prompted
4. Wait for processing (~3-5 minutes)

**Batch Processing:**

For processing multiple PDFs sequentially:
```bash
python batch_opp.py
```

Automatically processes all PDFs in `SourceFiles/SourceBatch/` with:
- Model: Gemini Pro
- Workers: 4
- Progress tracking and summary report

**Output:**
- Markdown: `runs/run_TIMESTAMP/final_profile.md`
- PowerPoint: `ReportFiles/[Company]_TIMESTAMP.pptx`

**Format:**
- Title and subtitle
- 4 sections: Company Overview, Competitive Positioning, Financial KPIs, Strategic Considerations
- Each section limited to ~100 words with bullet points

**Architecture (v1.1):**
- **Phase 1**: Parallel Draft/Check/Enhance for all sections
- **Phase 2**: Sequential deduplication in reverse priority (Section 4→3→2→1)
  - Section 4 (Strategic Considerations) keeps richest content
  - Earlier sections remove overlaps with later ones
- **Phase 3**: Parallel polish to 100 words
- No learning system (removed in v1.1 for simplicity)

## Requirements

- Python 3.8+
- Gemini API key (free at https://makersuite.google.com/app/apikey)
- 8GB RAM minimum

**Learning System (PD2 only):**
PD2 learns and improves over time:
- Memory: `memory/pd2_learning_memory.json` (33 sections, detailed analysis)
- Archives: `memory/memory_library/pd2_memory_{timestamp}.json`
- Extracts universal analytical methodologies (9-10/10 quality only)
- Applies learned patterns to future runs

OPP v1.1 does not use learning for simplicity and speed.

## License

MIT License - see LICENSE file for details.
# ProfileDash 2.1 & OnePageProfile 1.0

Intelligent document analysis tools for M&A and investment analysis:
- **PD2** (ProfileDash 2.1): Comprehensive 33-section company profiles with deep analytical insights
- **OPP** (OnePageProfile v1.0): Concise one-page profiles for quick M&A evaluation

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

## OnePageProfile v1.0 (OPP)

Concise one-page company profiles for rapid M&A screening and evaluation.

**Interactive Usage:**
```bash
python OPP.py
```

1. Select model: Gemini Flash or Pro
2. Set parallel workers (1-4, default 4)
3. Choose learning mode: exclude (default) or include learnings
4. Select PDFs when prompted
5. Wait for processing (~5-10 minutes)

**Batch Processing:**

For processing multiple PDFs sequentially:
```bash
python batch_opp.py
```

Automatically processes all PDFs in `SourceFiles/SourceBatch/` with:
- Model: Gemini Pro
- Workers: 4
- Learning: disabled
- Progress tracking and summary report

**Output:**
- Markdown: `runs/run_TIMESTAMP/final_profile.md`
- PowerPoint: `ReportFiles/[Company]_TIMESTAMP.pptx`

**Format:**
- Title and subtitle
- 4 sections: Company Overview, Competitive Positioning, Financial KPIs, Strategic Considerations
- Each section limited to ~120 words with bullet points

## Requirements

- Python 3.8+
- Gemini API key (free at https://makersuite.google.com/app/apikey)
- 8GB RAM minimum

**Learning System:**
Both systems learn and improve over time, but maintain separate learning memories:
- PD2: `memory/pd2_learning_memory.json` (33 sections, detailed analysis)
- OPP: `memory/opp_learning_memory.json` (4 sections, M&A screening)
- Archives: `memory/memory_library/{prefix}_memory_{timestamp}.json`

Each system extracts universal analytical methodologies (9-10/10 quality only) and applies them to future runs.

## License

MIT License - see LICENSE file for details.
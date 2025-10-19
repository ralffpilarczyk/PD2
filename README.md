# ProfileDash 2.1 & OnePageProfile

Intelligent document analysis tools for M&A and investment analysis:
- **PD2** (ProfileDash 2.1): Comprehensive 33-section company profiles with deep analytical insights
- **OPP** (OnePageProfile): Concise one-page profiles for quick M&A evaluation

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

## OnePageProfile (OPP)

Quick one-page company profiles for M&A evaluation.

**Usage:**
```bash
python OPP.py
```

1. Select PDFs when prompted
2. Wait for processing (~5-10 minutes)

**Output:**
- Markdown: `runs/opp_TIMESTAMP/step4_final.md`
- PowerPoint: `ProfileFiles/[Company]_TIMESTAMP.pptx`

**Format:**
- Title and subtitle
- 4 sections: Company Overview, Competitive Positioning, Financial KPIs, Strategic Considerations
- Each section limited to 500 words with bullet points

## ProfileDash 2.1 (PD2)

Comprehensive 33-section company analysis.

## Requirements

- Python 3.8+
- Gemini API key (free at https://makersuite.google.com/app/apikey)
- 8GB RAM minimum

**Usage:**

1. Run `python PD2.py`
2. Select PDFs when prompted
3. Choose section groups to analyze:
   - Company Profile (sections 1-13)
   - Strategy and SWOT (sections 14-19)
   - Sellside Positioning (sections 20-26)
   - Buyside Due Diligence (sections 27-32)
   - Data Book (section 33)
4. Set parallel workers (1-5, default 2)
5. Wait for processing (~30-60 minutes for full profile)

**Output:**

Professional PDF report saved to `ReportFiles/`:
- `[Company]_YYMMDD_HHMM.pdf` - Final report with footers and page numbers

Analysis work products saved to `runs/run_YYYY_MM_DD_HH_MM_SS/`:
- `[Company]_profile.md` - Combined markdown
- `section_*/` - Individual section drafts and refinements
- `run_summary.txt` - Processing log

**Architecture:**
- Multi-step analysis pipeline with progressive refinement
- Learning system captures analytical patterns
- Parallel processing with configurable workers
- Automatic retry with exponential backoff for API limits
- Thread-safe operations for concurrent processing

## License

MIT License - see LICENSE file for details.
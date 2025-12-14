# ProfileDash 2.2 & OnePageProfile 1.3

Intelligent document analysis tools for M&A and investment analysis:
- **PD2** (ProfileDash 2.2): Comprehensive 34-section company profiles with deep analytical insights
- **OPP** (OnePageProfile v1.3): Concise one-page profiles for quick M&A evaluation

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

## OnePageProfile v1.3 (OPP)

Concise one-page company profiles for rapid M&A screening and evaluation.

**Interactive Usage:**
```bash
python OPP.py
```

1. Select profile type: OnePageProfile (default) or Custom Profile
2. Select model: Gemini Flash or Pro
3. Set parallel workers (1-4, default 4)
4. Set density iterations (1-3, default 1)
5. Select PDFs when prompted
6. Wait for processing (~3-5 minutes)

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
- Markdown: `runs/opp_TIMESTAMP/final_profile.md` (or `runs/opp_custom_TIMESTAMP/` for custom profiles)
- PowerPoint: `ProfileFiles/[Company]_TIMESTAMP.pptx` (or `Custom_[Company]_TIMESTAMP.pptx` for custom profiles)
- Multiple iterations create versioned outputs: `_v1.pptx`, `_v2.pptx`, `_v3.pptx`

**Format:**
- Title and subtitle (up to 8 words, prose statement)
- 4 sections: Company Overview, Competitive Positioning, Financial KPIs, Strategic Considerations
- Each section limited to ~100 words with bullet points
- Temporal bias: prioritizes most recent fiscal data over historical data

**Custom Sections (Introduced in v1.2):**

Create custom section definitions for specialized use cases:

```bash
# 1. Copy template to create custom sections file
cp src/opp_sections_template.py src/opp_sections_custom.py

# 2. Edit opp_sections_custom.py to define your 4 custom sections

# 3. Run OPP and select "2 - Custom Profile"
python OPP.py
```

Custom profiles use distinct output naming for easy identification.

**Architecture (v1.3):**
- **Phase 1**: Parallel Draft/Check/Enhance for all sections
- **Phase 2**: Sequential deduplication in reverse priority (Section 4→3→2→1)
  - Section 4 (Strategic Considerations) keeps richest content
  - Earlier sections remove overlaps with later ones
- **Phase 3**: Parallel polish to 100 words
- **Iterations**: Optional 2-3 iterations for increased density (subtitle refinement + section enhancement)
- Dynamic section loading: supports default or custom section definitions

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

OPP v1.3 does not use learning for simplicity and speed.

## License

MIT License - see LICENSE file for details.
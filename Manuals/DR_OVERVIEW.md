# DR (Deep Research) Technical Overview

Version 1.0

---

## Executive Summary

DR (Deep Research) is a standalone web research tool that uses Google's Gemini Deep Research Agent to gather publicly available information about companies. It operates independently from PD2 and OPP, allowing users to run extended research queries without blocking the main document analysis pipelines.

Key characteristics:
- **Standalone operation**: Runs independently via `python DR.py`
- **Web-based research**: Uses Google's Deep Research Agent (not document analysis)
- **12 research topics**: Covers company profile sections from PD2 methodology
- **Parallel execution**: Supports 1-2 concurrent research workers
- **Long-running**: Typical research takes 20-40 minutes for all 12 topics

---

## Architecture Overview

### System Components

```
DR.py                    Entry point and CLI interface
    |
    v
src/deep_research.py     Core research engine
    |                    - DeepResearcher class
    |                    - ResearchDisplay class
    |
    v
src/research_sections.py Section definitions (from PD2)
    |
    v
Google Gemini API        Deep Research Agent
(google-genai SDK)       Model: deep-research-pro-preview-12-2025
```

### SDK Isolation

DR uses `google-genai` SDK (new Interactions API), while PD2/OPP use `google-generativeai` SDK (classic GenerativeModel API). This separation:
- Avoids SDK version conflicts
- Allows independent SDK upgrades
- Isolates experimental API features

---

## Core Components

### DR.py (Entry Point)

Simple CLI that:
1. Prompts for company name
2. Prompts for number of workers (1-2, default 2)
3. Creates timestamped run directory
4. Executes research via DeepResearcher
5. Saves combined report to ReportsDR/

```python
def main():
    # Company name input
    company_name = input("Company name: ").strip()

    # Workers selection (1-2, default 2)
    workers = int(prompt_single_digit(...))

    # Create run directory
    run_dir = Path(f"runs/dr_{timestamp}")

    # Execute research
    researcher = DeepResearcher(company_name, workers, run_dir)
    results = researcher.run_all_sections(sections, display)

    # Combine and save
    researcher.combine_research(results)
```

### DeepResearcher Class

Core engine in `src/deep_research.py`:

| Method | Purpose |
|--------|---------|
| `research_section()` | Run single research query via Interactions API |
| `run_all_sections()` | Execute all 12 sections with parallel workers |
| `_build_research_prompt()` | Adapt PD2 section specs for web research |
| `_poll_for_result()` | Poll async interaction until completion |
| `_save_section_output()` | Save individual section results |
| `combine_research()` | Merge all sections into single report |
| `get_research_summary()` | Return completion statistics |

### ResearchDisplay Class

Thread-safe progress display:

```python
class ResearchDisplay:
    def start(section_num, title):   # Mark section as started
    def complete(section_num, title, status):  # Mark section done
```

---

## Research Sections

DR researches the first 12 sections from PD2 (Company Profile group):

| # | Section | Focus |
|---|---------|-------|
| 1 | Operating Footprint | Physical assets, employees, geographic presence |
| 2 | Products and Services | Product offerings, value propositions, market positioning |
| 3 | Key Customers | Major customers, concentration, relationships |
| 4 | Key Suppliers | Supplier dependencies, concentration, terms |
| 5 | Key Competitors | Competitive landscape, market structure, dynamics |
| 6 | Operational KPIs | Volume metrics, pricing, efficiency, unit economics |
| 7 | Summary Financials (Consolidated) | Revenue, EBITDA, margins, cash flow |
| 8 | Summary Financials (Segment) | Segment performance, contribution analysis |
| 9 | Balance Sheet (Most Recent) | Assets, liabilities, leverage, working capital |
| 10 | Top 10 Shareholders | Ownership structure, control, voting rights |
| 11 | M&A Agenda | Transaction history, strategic priorities, capacity |
| 12 | Key Decision Makers | Executives, board, compensation, governance |

---

## Data Flow

### Research Flow

```
User Input (company name, workers)
         |
         v
    [Section 1]  [Section 2]  ...  [Section 12]
         |            |                 |
         v            v                 v
    Google Deep Research Agent (parallel up to 2)
         |            |                 |
         v            v                 v
   Poll for completion (up to 60 min each)
         |            |                 |
         v            v                 v
    section_01_*.md  section_02_*.md  section_12_*.md
         |            |                 |
         +------------+--------+--------+
                      |
                      v
            combined_research.md
                      |
                      v
            ReportsDR/{company}_DR_{timestamp}.md
```

### Prompt Adaptation

PD2 section specs are adapted for web research:

```python
def _build_research_prompt(self, section: dict) -> str:
    specs = section['specs']
    # Adapt document-specific language to web research
    specs = specs.replace('Extract and analyze', 'Research and analyze')
    specs = specs.replace('document references, page numbers', 'source URLs')

    return f"""Research {self.company_name} with focus on: {section['title']}

{specs}

Output format:
- Use markdown with headers and bullet points
- Include source citations (URLs) for all claims
- Focus on publicly available information from the last 2 years
- Note any conflicting information found across sources
"""
```

---

## Output Artifacts

### Run Directory Structure

```
runs/dr_YYYYMMDD_HHMMSS/
    section_01_operating_footprint.md
    section_02_products_and_services.md
    section_03_key_customers.md
    section_04_key_suppliers.md
    section_05_key_competitors.md
    section_06_operational_kpis.md
    section_07_summary_financials_consolidated.md
    section_08_summary_financials_segment.md
    section_09_balance_sheet_most_recent.md
    section_10_top_10_shareholders.md
    section_11_manda_agenda_and_material_corporate_activity.md
    section_12_key_decision_makers.md
    combined_research.md
```

### Final Report Location

```
ReportsDR/{CompanyName}_DR_YYYYMMDD_HHMMSS.md
```

### Section Output Format

Each section file contains:

```markdown
# {Section Title}

**Company**: {company_name}
**Status**: completed|failed|timeout

---

{Research content with source URLs}
```

### Combined Report Format

```markdown
# Deep Research Report: {company_name}

## 1. Operating Footprint

{content}

---

## 2. Products and Services

{content}

---

... (all 12 sections)
```

---

## API Integration

### Google Deep Research Agent

Uses the Interactions API from `google-genai`:

```python
from google import genai

client = genai.Client(api_key=api_key)

# Start background research
interaction = client.interactions.create(
    input=prompt,
    agent='deep-research-pro-preview-12-2025',
    background=True,
    store=True
)

# Poll for completion
while True:
    interaction = client.interactions.get(interaction.id)
    if interaction.status == "completed":
        result = interaction.outputs[-1].text
        break
    time.sleep(10)  # Poll every 10 seconds
```

### Rate Limiting

Google's Deep Research Agent has concurrency limits:
- 2 parallel workers: Reliable
- 4+ parallel workers: Causes immediate failures

DR enforces 1-2 workers maximum.

---

## Configuration

### Environment Variables

| Variable | Required | Description |
|----------|----------|-------------|
| `GEMINI_API_KEY` | Yes | Google Gemini API key |

### Constants (deep_research.py)

```python
AGENT = 'deep-research-pro-preview-12-2025'  # Research model
POLL_INTERVAL = 10                            # Seconds between status checks
MAX_RESEARCH_TIME = 3600                      # 60 minute timeout per section
```

---

## Error Handling

### Research Failures

Each section can have status:
- `completed`: Research successful
- `failed`: API error or exception
- `timeout`: Exceeded 60 minute limit

Failed sections are logged with full traceback:

```python
except Exception as e:
    tb = traceback.format_exc()
    error_msg = f"Research failed: {str(e)}\n\nTraceback:\n{tb}"
```

### Summary Statistics

```python
summary = researcher.get_research_summary(results)
# Returns: {'total': 12, 'completed': 10, 'failed': 2, 'timeout': 0, 'success_rate': 0.83}
```

---

## Performance Characteristics

### Timing

| Metric | Typical Value |
|--------|---------------|
| Per-section research | 2-5 minutes |
| Full 12-section run | 20-40 minutes |
| Maximum timeout | 60 minutes per section |
| Poll interval | 10 seconds |

### Resource Usage

- Network: Continuous polling during research
- CPU: Minimal (waiting on API)
- Memory: Minimal (text processing only)

---

## Usage

### Basic Usage

```bash
python DR.py
```

Interactive prompts:
```
DEEP RESEARCH 1.0
============================================================
Web-based company research using Google Deep Research Agent
============================================================

Company name: Axiata Group Berhad
Research workers (parallel queries):
  1-2 workers (default 2)
Choose workers [1-2] (default 2): 2

Starting Deep Research for Axiata Group Berhad...
============================================================

  [1/12] Starting: Operating Footprint
  [2/12] Starting: Products and Services
  [1/12] Done: Operating Footprint (ok)
  ...
  [12/12] Done: Key Decision Makers (ok)

Deep Research complete: 12/12 topics
Output saved to: runs/dr_20250103_143022/
Report saved to: ReportsDR/Axiata_Group_Berhad_DR_20250103_143022.md
```

### Workflow Integration

DR output can be used as context for OPP/PD2:
1. Run `python DR.py` for target company
2. Wait for completion (20-40 minutes)
3. Use research output to inform document analysis

---

## File Reference

| File | Purpose |
|------|---------|
| `DR.py` | Entry point, CLI interface |
| `src/deep_research.py` | DeepResearcher and ResearchDisplay classes |
| `src/research_sections.py` | Section definitions (imports from profile_sections) |
| `runs/dr_*/` | Run output directories |
| `ReportsDR/` | Final report storage |

---

## Dependencies

### Python Packages

```
google-genai>=1.56.0    # New Interactions API
python-dotenv           # Environment loading
```

### API Requirements

- Google Gemini API key with Deep Research Agent access
- Interactions API enabled (experimental)

---

## Limitations

1. **Research scope**: Limited to publicly available web information
2. **Concurrency**: Maximum 2 parallel workers due to API limits
3. **Timing**: Long execution time (20-40 minutes typical)
4. **Experimental API**: Deep Research Agent is in preview
5. **No document integration**: Cannot analyze uploaded PDFs (use PD2/OPP for that)

---

## Version History

| Version | Changes |
|---------|---------|
| 1.0 | Initial standalone release, extracted from OPP |

# OPP Code Overview - Complete Technical Documentation (v1.4)

## Executive Summary

OnePageProfile (OPP) v1.4 is a focused document analysis tool that transforms PDF documents into concise one-page company profiles for M&A evaluation using Google's Gemini LLM API. The system employs a 5-step vanilla pipeline with parallel processing, plus an optional 12-step insight pipeline for hypothesis-driven analysis. New in v1.4: Ground truth discovery, hypothesis testing, batch processing integration, and three PPTX output variants.

**Key Metrics**:
- 4 analytical sections with ground truth pointers
- 5-step vanilla pipeline: Steps 1-3 (Draft/Check/Enhance) → Step 4 (Clean-up) → Step 5 (Polish)
- Optional 12-step insight pipeline: Steps 6-9 (Ground Truth) → Steps 10-12 (Integration)
- 100-word limit per section
- Parallel processing with 1-4 configurable workers
- Batch processing for multiple PDFs (integrated into main OPP.py)
- Output: Markdown + PowerPoint (1 or 3 variants depending on insights mode)
- Processing time: ~3-5 minutes (vanilla) or ~8-12 minutes (with insights)
- Files API and context caching (2h vanilla / 3h insights TTL)

## What's New in v1.4

### 1. Ground Truth Insight Pipeline (Steps 6-12)

Optional hypothesis-driven analysis that discovers insights beyond management narrative:

- **Step 6 (Ground Truth)**: Discovers 2-3 verifiable observations using section-specific ground truth pointers
- **Step 7 (Hypothesis)**: Generates testable hypotheses WITHOUT document access (prevents anchoring)
- **Step 8 (Test)**: Tests hypotheses against source documents with evidence and verdicts
- **Step 9 (Synthesis)**: Synthesizes confirmed hypotheses into actionable insights
- **Step 10 (Integration)**: Weaves insights into vanilla description (~150 words)
- **Step 11 (Clean-up 2)**: Redistributes integrated content across sections
- **Step 12 (Polish 2)**: Final polish to ~100 words

**Critical Design**: Step 7 deliberately has NO document attachment to prevent LLM anchoring to source framing.

### 2. Three PPTX Output Variants (when insights enabled)

| Variant | Source | Filename |
|---------|--------|----------|
| Vanilla | Step 5 | `[Company]_vanilla_YYMMDD_HHMMSS.pptx` |
| Insights | Step 9 | `[Company]_insights_YYMMDD_HHMMSS.pptx` |
| Integrated | Step 12 | `[Company]_integrated_YYMMDD_HHMMSS.pptx` |

When insights disabled: Single output `[Company]_YYMMDD_HHMMSS.pptx` (no variant suffix).

### 3. Batch Processing Integration

OPP_batch.py has been merged into OPP.py with unified UI:

- **File source selection**: Choose between interactive file picker or batch directory
- **Batch directory**: `SourceFiles/SourceBatch/` scanned for PDFs
- **Per-document isolation**: Each PDF processed independently with its own cache
- **Batch summary**: Success/failure counts with timing at completion
- **Insights toggle**: Works in both interactive and batch modes

### 4. Ground Truth Pointers

New field in section definitions guides insight discovery:

| Section | Ground Truth Pointer |
|---------|---------------------|
| Company Overview | What does the actual operational structure reveal about how this company creates and captures value? |
| Competitive Positioning | Are claimed market positions and competitive advantages supported by measurable outcomes? |
| Financial KPIs | What do the numbers reveal about earnings quality, cash generation, and sustainable performance? |
| Strategic Considerations | What do ownership, transaction history, and management incentives reveal about likely agendas? |

### 5. Dynamic Cache TTL

- **Without insights**: 2-hour cache TTL
- **With insights**: 3-hour cache TTL (longer pipeline requires more time)

### 6. Removed Features

The following features from v1.2/v1.3 have been removed for simplicity:

- **Density iterations** (1-3 iterations): Replaced by insight pipeline
- **Custom sections** (`opp_sections_custom.py`): Removed - use standard 4 sections only
- **OPP_batch.py**: Merged into main OPP.py

## Architecture Overview

```
┌─────────────────────────────────────────────────────┐
│                    User Input                        │
│    (PDF Documents - Interactive or Batch mode)       │
└────────────────────┬───────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────┐
│                   OPP.py                            │
│         (Main Orchestrator & CLI Interface)          │
│   • Gemini API configuration                         │
│   • File source selection (interactive/batch)        │
│   • ThreadPoolExecutor (1-4 workers)                 │
│   • 5-step vanilla pipeline (always)                 │
│   • 12-step insight pipeline (optional)              │
│   • PowerPoint generation (1 or 3 variants)          │
└────────────────────┬───────────────────────────────┘
                     │
        ┌────────────┼────────────┐
        ▼            ▼            ▼
┌──────────────┐ ┌────────────┐ ┌──────────────────┐
│opp_sections  │ │profile_    │ │pptx_generator    │
│.py           │ │prompts.py  │ │                  │
│              │ │            │ │                  │
│•4 section    │ │•Steps 1-5  │ │•Markdown parsing │
│ definitions  │ │ prompts    │ │•Native bullets   │
│•Ground truth │ │•Steps 6-12 │ │•A4 landscape     │
│ pointers     │ │ prompts    │ │•Variant naming   │
└──────────────┘ └────────────┘ └──────────────────┘
```

## Core Principles & Design Philosophy

### 1. Speed Over Depth
- Optimized for quick M&A screening, not comprehensive analysis
- 5-step pipeline: Steps 1-3 (parallel) → Step 4 (Clean-up) → Step 5 (Polish)
- No learning system (removed in v1.1 for simplicity)
- Parallel processing with configurable workers (1-4)
- Target: Under 5 minutes per profile (with 4 workers)

### 2. Clarity Over Completeness
- 100 words max per section (enforced at polish step)
- Focus on essential insights only
- "Does this matter to an investor?" filter applied throughout

### 3. Direct PDF Processing
- PDFs sent directly to Gemini (no markdown conversion)
- Inline base64 encoding for simplicity
- PDFs prepared once and reused across all parallel workers
- Reduces complexity compared to PD2's conversion pipeline

### 4. Professional Presentation
- Native PowerPoint bullets with proper hanging indents
- A4 landscape format (11.69" × 8.27")
- Consistent color scheme (dark blue #2d5a87, dark grey #4a5568)
- Auto-generated footnotes with version and date

### 5. Five-Step Pipeline (introduced v1.1, enhanced v1.2)
- **Steps 1-3**: Draft/Check/Enhance for all 4 sections (parallel)
- **Step 4**: Clean-up - redistribute content based on section relevance (parallel)
- **Step 5**: Polish to 100 words (parallel)
- ThreadPoolExecutor manages worker pools
- WorkerDisplay provides real-time progress tracking

## File Structure & Module Breakdown

```
PD2/
├── OPP.py                           # Main entry point (~1500 lines)
├── src/
│   ├── opp_sections.py             # Section definitions with ground_truth_pointer (~100 lines)
│   ├── profile_prompts.py          # Prompt templates for Steps 1-12 (~600 lines)
│   ├── pptx_generator.py           # PowerPoint generation with variant support (~150 lines)
│   ├── file_manager.py             # File I/O and directory management
│   └── utils.py                    # Thread-safe utilities (shared with PD2)
├── Manuals/
│   ├── OPP_OVERVIEW.md             # This technical documentation
│   ├── Profile_Page.md             # User-facing specification
│   └── Refinement.md               # Pipeline documentation
├── SourceFiles/
│   └── SourceBatch/                # Batch processing directory
├── requirements.txt                # Dependencies (includes python-pptx)
├── ReportsOPP/                     # PowerPoint output directory
│   ├── [Company]_TIMESTAMP.pptx              # Vanilla-only output (insights disabled)
│   ├── [Company]_vanilla_TIMESTAMP.pptx      # Vanilla output (insights enabled)
│   ├── [Company]_insights_TIMESTAMP.pptx     # Insights-only output (insights enabled)
│   └── [Company]_integrated_TIMESTAMP.pptx   # Integrated output (insights enabled)
└── runs/                           # Processing work products
    └── opp_YYMMDD_HHMMSS/
        ├── section_1/              # Company Overview
        │   ├── step1_draft.md
        │   ├── step2_add_list.txt
        │   ├── step3_enhanced.md
        │   ├── step4_cleaned.md
        │   ├── step5_polished.md           # Vanilla content
        │   ├── step6_ground_truth.md       # (if insights enabled)
        │   ├── step7_hypotheses.md         # (if insights enabled)
        │   ├── step8_test_results.md       # (if insights enabled)
        │   ├── step9_synthesis.md          # (if insights enabled)
        │   ├── step10_integrated.md        # (if insights enabled)
        │   ├── step11_cleaned.md           # (if insights enabled)
        │   └── step12_polished.md          # (if insights enabled)
        ├── section_2/              # Competitive Positioning (same structure)
        ├── section_3/              # Financial KPIs (same structure)
        ├── section_4/              # Strategic Considerations (same structure)
        ├── subtitle.md             # Refined subtitle
        ├── final_profile.md        # Vanilla markdown
        ├── insights_profile.md     # Insights-only markdown (if insights enabled)
        ├── integrated_profile.md   # Integrated markdown (if insights enabled)
        └── run_log.txt             # Processing log
```

## Detailed Module Documentation

### 1. OPP.py - Main Orchestrator

**Purpose**: Entry point and orchestration layer for parallel one-page profile generation with optional insights pipeline.

#### OnePageProfile Class

```python
class OnePageProfile:
    def __init__(self, pdf_files: List[str], model_name: str, workers: int = 2,
                 insights_enabled: bool = False):
        self.pdf_files = pdf_files
        self.model_name = model_name
        self.workers = workers              # Parallel worker count (1-4)
        self.insights_enabled = insights_enabled  # Enable Steps 6-12

        # Section definitions with ground_truth_pointer
        from src.opp_sections import sections, get_section_boundaries
        self.sections = sections

        # Four model variants for different steps
        # Steps with docs attached use cached models, steps without use non-cached
        self.model_low_temp = genai.GenerativeModel(model_name, temperature=0.2)
        self.model_medium_temp = genai.GenerativeModel(model_name, temperature=0.6)
        self.cached_model_low_temp = None   # Created after cache setup
        self.cached_model_medium_temp = None

        # Cache TTL: 2 hours vanilla, 3 hours with insights
        self.cache_ttl = timedelta(hours=3 if insights_enabled else 2)
```

**Key Design Decisions**:
- Configurable parallel workers (1-4, default 4)
- Optional insights pipeline (Steps 6-12)
- Four model variants: cached/non-cached × low/medium temperature
- PDF parts prepared once, shared across all workers
- Dynamic cache TTL based on insights mode
- Run directory: `runs/opp_TIMESTAMP/` with section_N subdirectories
- Version tracking: `__opp_version__ = "1.4"`

#### WorkerDisplay Class

```python
class WorkerDisplay:
    def __init__(self, num_workers: int):
        self.num_workers = num_workers
        self.status = {}  # {section_num: status_text}
        self.lock = threading.Lock()

    def update(self, section_num: int, action: str):
        # Show real-time progress: "Sec. 1 → Draft | Sec. 2 → Polish"
        # Steps 1-5: "Draft", "Check", "Enhance", "Clean-up", "Polish"
        # Steps 6-12: "Ground", "Hypo", "Test", "Synth", "Integ", "Clean2", "Polish2"

    def complete(self, section_num: int, completed: int, total: int):
        # Mark section complete (silent)
```

**Key Features**:
- Thread-safe status updates with lock
- Real-time display of active workers
- Step labels for both vanilla (1-5) and insights (6-12) pipelines

#### The 12-Step Pipeline (v1.4)

**Vanilla Pipeline (Steps 1-5)** - Always runs:

| Step | Name | Docs | Temp | Model | Purpose |
|------|------|------|------|-------|---------|
| 1 | Draft | YES | 0.6 | cached | Initial section content |
| 2 | Check | YES | 0.2 | cached | Completeness validation |
| 3 | Enhance | YES | 0.6 | cached | Add missing items |
| 4 | Clean-up | NO | 0.2 | non-cached | Redistribute across sections |
| 5 | Polish | NO | 0.6 | non-cached | Condense to ~100 words |

**Insight Pipeline (Steps 6-12)** - Only if insights_enabled:

| Step | Name | Docs | Temp | Model | Purpose |
|------|------|------|------|-------|---------|
| 6 | Ground Truth | YES | 0.6 | cached | Discover verifiable observations |
| 7 | Hypothesis | **NO** | 0.6 | non-cached | Generate testable predictions (NO anchoring) |
| 8 | Test | YES | 0.2 | cached | Validate against documents |
| 9 | Synthesis | YES | 0.6 | cached | Create insight paragraph |
| 10 | Integration | YES | 0.6 | cached | Weave into vanilla content |
| 11 | Clean-up 2 | NO | 0.2 | non-cached | Redistribute integrated content |
| 12 | Polish 2 | NO | 0.6 | non-cached | Final ~100 word polish |

**Critical**: Step 7 has NO document attachment to prevent LLM anchoring to source framing.

---

### Steps 1-3: Draft, Check, Enhance

**process_section_main(section: dict, worker_display: WorkerDisplay) -> dict**
```python
# Orchestrates one section through Steps 1-3
# Creates: runs/opp_TIMESTAMP/section_N/ directory
# Returns: {'number': int, 'title': str, 'content': enhanced_content, 'success': bool}
```

---

**Step 1: Initial Draft**
```python
def _generate_section(self, section: dict) -> str:
    # Input: section dict from opp_sections.py
    # Temperature: 0.6 (balanced creativity)
    # Prompt includes: section['specs'] (requirements)
    # PDFs attached: self.pdf_parts (shared across workers)
    # Output: section_N/step1_draft.md
```

**Detailed Input/Output**:
- **Input**:
  - `section`: `{'number': 1, 'title': 'Company Overview', 'specs': '...'}`
  - PDF parts (base64 encoded documents)
  - Prompt with section specifications
- **Processing**:
  - LLM generates comprehensive section content
  - Enforces critical rules:
    - Max 20 words per sentence
    - At least one number per sentence (ideally)
    - Bold 1-2 keywords per bullet (**word** syntax)
    - Skip silently if no data available
- **Output**:
  ```markdown
  ## Company Overview
  * **Based** in Singapore, Axiata is a telecommunications holding company.
  * Operating footprint spans 9 countries with **23,000 employees**.
  ...
  ```
- **Saved to**: `section_1/step1_draft.md`

---

**Step 2: Completeness Check**
```python
def _check_section_completeness(self, section: dict, content: str) -> str:
    # Input: section dict + step 1 content
    # Temperature: 0.2 (precision)
    # Validates against section['specs']
    # Output: section_N/step2_add_list.txt
```

**Detailed Input/Output**:
- **Input**:
  - `section`: Section definition with specs
  - `content`: Draft from Step 1
  - PDF parts for validation
  - Prompt with dual assessment framework
- **Processing**:
  - Validates against original section requirements
  - Dual assessment:
    - Data completeness (missing from source docs)
    - Investor perspective completeness (missing insights)
  - Critical constraint: Only suggest if data exists in source
  - Materiality filter: "Would this affect investment decision?"
  - Max 5 suggestions per section
- **Output**:
  ```
  [CRITICAL] Missing revenue breakdown by geography with percentages (Annual Report p.45)
  [IMPORTANT] Missing customer concentration metrics for top 5 customers (10-K p.23)
  ```
- **Saved to**: `section_1/step2_add_list.txt`

---

**Step 3: Enhancement**
```python
def _enhance_section(self, section: dict, content: str, add_list: str) -> str:
    # Input: section dict + step 1 content + ADD list
    # Temperature: 0.6 (balanced)
    # Incorporates ADD items from step 2
    # Output: section_N/step3_enhanced.md
```

**Detailed Input/Output**:
- **Input**:
  - `section`: Section definition
  - `content`: Draft from Step 1
  - `add_list`: ADD list from Step 2
  - PDF parts for data lookup
  - Prompt with enhancement instructions
- **Processing**:
  - Add ALL items from ADD list where data exists
  - Skip silently if no supporting data (no placeholders)
  - Maintain narrative flow and bullet format
  - Preserve all existing content
  - Integrate additions smoothly
  - Keep **bold** keyword formatting
- **Output**:
  ```markdown
  ## Company Overview
  * **Based** in Singapore, Axiata is a telecommunications holding company.
  * Operating footprint spans 9 countries with **23,000 employees**.
  * **Revenue** breakdown: Malaysia 35%, Indonesia 28%, Bangladesh 15%, others 22%.
  * **Top 5 customers** represent 12% of total revenue with average 7-year relationships.
  ...
  ```
- **Saved to**: `section_1/step3_enhanced.md`
- **Fallback**: If enhancement fails, returns Step 1 content unchanged

---

### Step 4: Clean-up (introduced v1.1)

**Architecture**: After Steps 1-3 complete, sections are cleaned up to redistribute content based on section relevance.

```python
def _deduplicate_section(self, section: dict, content: str, previous_sections: list) -> str:
    # Input: section dict + enhanced content + previously processed sections
    # Temperature: 0.6 (balanced)
    # Removes overlaps with higher-priority sections
    # Output: deduplicated content (not saved separately, passed to polish)
```

**Processing Order**:
1. **Section 4** (Strategic Considerations): Processes first, keeps all content
2. **Section 3** (Financial KPIs): Removes overlap with Section 4
3. **Section 2** (Competitive Positioning): Removes overlap with Sections 4, 3
4. **Section 1** (Company Overview): Removes overlap with Sections 4, 3, 2

**Detailed Input/Output**:
- **Input**:
  - `section`: Current section definition
  - `content`: Enhanced content from Step 3
  - `previous_sections`: List of already-deduplicated sections (in reverse order)
- **Processing**:
  - If first section (Section 4): Returns content unchanged
  - Otherwise: LLM identifies and removes overlapping bullet points
  - Keeps bullets with unique information or different angle
  - Removes bullets duplicating facts from previous sections
  - Preserves section header and format
- **Output**: Deduplicated content (in memory, not saved to file)
- **Fallback**: If deduplication fails, returns enhanced content unchanged

**Why Reverse Order?**
- Section 4 (Strategic Considerations) is most important for M&A evaluation
- Strategic insights take precedence over basic company facts
- Earlier sections become lean foundational context
- Matches how investors read: thesis first, details second

---

### Step 5: Polish
```python
def _polish_section(self, section: dict, content: str, word_limit: int) -> str:
    # Input: section dict + deduplicated content + word limit (100)
    # Temperature: 0.6 (balanced)
    # Condenses to essential insights only
    # Output: section_N/step5_polished.md
```

**Detailed Input/Output**:
- **Input**:
  - `section`: Section definition with specs
  - `content`: Cleaned content from Step 4
  - `word_limit`: 100 words (hard target)
  - Prompt with condensing instructions
- **Processing**:
  - References original section['specs'] for relevance filter
  - Preserves only content addressing requirements
  - Removes generic statements and repetition
  - Maintains data density (numbers, percentages, trends)
  - Condenses to ≤100 words
  - Preserves bullet format with **bold** keywords
  - NO preambles or duplicate section names
- **Output**:
  ```markdown
  ## Company Overview
  * **Based** in Singapore, Axiata operates 9 telcos across Asia with 23,000 employees.
  * **Revenue** breakdown: Malaysia 35%, Indonesia 28%, Bangladesh 15%, others 22%.
  * **Owned** spectrum licenses in 8 markets, leased tower infrastructure (15,000 sites).
  ...
  ```
- **Saved to**: `section_1/step5_polished.md`
- **Fallback**: If polish fails, returns deduplicated content unchanged

---

After all sections complete the 5-step pipeline, the final polished content from each section is assembled into the complete profile.

---

## Parallel Execution Architecture (v1.2)

**5-Step Processing**:
```python
def generate_profile(self, company_name: str, worker_display: WorkerDisplay) -> str:
    # Steps 1-3: Draft/Check/Enhance for all sections (parallel)
    # Step 4: Clean-up - redistribute content (parallel)
    # Step 5: Polish to 100 words (parallel)
    # Returns: Assembled markdown with all 4 sections
```

**Processing Flow**:

**Steps 1-3 - Parallel Draft/Check/Enhance**:
1. Create ThreadPoolExecutor with `self.workers` max workers
2. Submit all 4 sections to process Steps 1-3 in parallel
3. Collect enhanced results as they complete
4. Sort by section number for consistent processing

**Step 4 - Clean-up**:
1. Process Section 4 first (keeps all content)
2. Process Section 3 (removes overlap with 4)
3. Process Section 2 (removes overlap with 4, 3)
4. Process Section 1 (removes overlap with 4, 3, 2)
5. Each deduplication is a single LLM call

**Step 5 - Parallel Polish**:
1. Create new ThreadPoolExecutor
2. Submit all 4 deduplicated sections to polish in parallel
3. Each polishes to 100 words independently
4. Collect polished results and save to step5_polished.md files

**Final Assembly**:
1. Sort polished sections by number (1-4)
   - Update progress counter
   - Worker display shows completion
4. Sort results by section number
5. Assemble final markdown: `## Section\n{content}` for each

#### Helper Methods

**Company Name Extraction**:
```python
def extract_company_name(self, pdf_parts: List) -> str:
    # Temperature: 0.6 (medium)
    # Searches: Document titles, headers, letterheads
    # Validation: 2-100 characters
    # Fallback: "Company Profile"
```

**Title and Subtitle Generation**:
```python
def generate_title_subtitle(self, company_name: str) -> str:
    # Temperature: 0.6 (medium)
    # Input: Company name + PDF parts
    # Output: "# {company_name}\n[Subtitle: 4-8 word message]"
    # Generated separately before section processing
```

**Final Assembly**:
```python
def _assemble_final_markdown(self, title_subtitle: str, section_content: str) -> str:
    # Combines: Title/subtitle + all 4 sections
    # Section order: By section number (1-4)
    # Output: Complete profile markdown
```

**Run Logging**:
```python
def save_run_log(self, company_name: str, status: str = "Success", pptx_path: str = None):
    # Logs:
    #   - Timestamp, model, and worker count
    #   - Company name
    #   - Source files
    #   - Processing info (parallel sections, output locations)
    #   - Status
```

#### Main Execution Flow

```python
def run(self) -> Optional[Path]:
    # 1. Prepare PDFs once (encode to base64, store in self.pdf_parts)
    # 2. Extract company name from PDFs
    # 3. Generate title and subtitle
    # 4. Create WorkerDisplay for progress tracking
    # 5. Generate all 4 sections in parallel (each through 5 steps)
    # 6. Conduct memory review (synthesize universal learnings)
    # 7. Assemble final markdown (title + sections)
    # 8. Save to runs/opp_TIMESTAMP/final_profile.md
    # 9. Generate PowerPoint from final markdown
    # 10. Save run log
    # 11. Return path to final_profile.md
```

**Key Changes from Sequential Architecture**:
- PDFs prepared once at start, reused by all workers
- Sections processed in parallel (not sequentially)
- Each section goes through all 4 steps independently
- WorkerDisplay shows real-time progress
- Final assembly happens after all sections complete

#### CLI Main

```python
if __name__ == "__main__":
    # 1. System checks (API key, directories)
    # 2. File source selection (interactive or batch)
    # 3. Select LLM model (gemini-3-flash-preview or gemini-3-pro-preview)
    # 4. Select worker count (1-4, default 4)
    # 5. Enable insights pipeline? (y/N)
    # 6. If batch: scan SourceFiles/SourceBatch/, confirm, loop
    #    If interactive: file dialog for PDFs
    # 7. Initialize OnePageProfile with workers and insights_enabled
    # 8. Generate profile (vanilla or vanilla + insights + integrated)
    # 9. Output paths to markdown and PowerPoint(s)
```

**UI Flow Example**:
```
ONEPAGEPROFILE 1.4
============================================================
System Check
============================================================
✓ API key configured
✓ Output directory ready
============================================================

Select file source:
  1 - Select files interactively (default)
  2 - Process batch directory (SourceFiles/SourceBatch/)
Choose [1/2] (default 1): 1

Select LLM model:
  1) gemini-3-flash-preview
  2) gemini-3-pro-preview
Choose model [1/2] (default 1): 1

Select number of parallel workers:
  1-4 workers (default 4)
Choose workers [1-4] (default 4): 4

Enable insights pipeline?
  Adds ground truth discovery and hypothesis testing
  Produces 3 PPTX variants: vanilla, insights, integrated
Enable insights? (y/N): y
```

### 2. src/opp_sections.py - Section Definitions

**Purpose**: Declarative specification of all 4 profile sections (single source of truth).

**Design Pattern**: Similar to PD2's profile_sections.py - separates content requirements from processing logic.

#### sections List

```python
sections = [
    {
        "number": 1,
        "title": "Company Overview",
        "specs": """Provide a comprehensive overview of the company's business.

Include the following elements:
• One sentence where the company is based (city and country) and what its primary business is (key subsector, not high level like 'technology company')
• One sentence on the company's primary operating footprint, i.e. where its people are based, with numbers
• One sentence on the company's asset base, i.e. where its key assets are based, and if they are owned or leased
• One sentence on the mix of products and services and their value proposition, indicating what's most important
• One sentence on the mix of geography, indicating what's most important
• One sentence on the mix of customers, indicating what's most important, and highlight key customers, if any, and customer relationships, e.g. long term relationships, exclusivity, etc.
• One sentence on the company's key suppliers and concentration risk, but only if there is a critical exposure

Critical rules:
• Keep each sentence as short as possible, no longer than 20 words
• Ideally, each sentence should have at least one number
• If there is no meaningful data to write a sentence as instructed, skip it silently
• Do not make up information
• A precise sentence which may not perfectly suit the purpose is better than a beautifully tailored sentence which is not grounded
• Bold the 1-2 most important words in each bullet point using **word** syntax"""
    },
    # ... sections 2-4 with similar structure
]
```

**Structure**:
- Each section has: `number`, `title`, `specs`
- `specs` contains: Element requirements + Critical rules
- Used by all prompt generation functions in profile_prompts.py

**Key Benefit**: Changing section requirements requires editing only this file, not prompt templates.

---

### 3. src/profile_prompts.py - Prompt Templates

**Purpose**: Generates prompts for each step using section definitions from opp_sections.py.

**Design Pattern**: Single-section processing (not whole-document) - each prompt operates on one section at a time.

#### Company Name Extraction

```python
COMPANY_NAME_EXTRACTION_PROMPT = """Extract the primary company name from these documents.

Look for the company name in:
- Document titles and headers
- Letterheads and footers
- About sections
- First few pages

Return ONLY the company name, nothing else. Keep it short (2-50 characters).
If unclear, return "Company Profile"."""
```

#### Prompt Generation Functions

**1. get_title_subtitle_prompt(company_name: str) -> str**
- Generates title (company name) and subtitle (4-8 word message)
- Separate from section processing
- Called once before parallel section processing begins

**2. get_section_generation_prompt(section: dict) -> str**
- **Input**: section dict from opp_sections.py
- **Embeds**: `section['title']` and `section['specs']`
- **Enforces**: Critical rules from specs (20 words/sentence, numbers, bold keywords)
- **Output format**: `## {section['title']}\n[bullets]`
- **Used in**: Step 1 (_generate_section)

**3. get_section_completeness_check_prompt(section: dict, section_content: str) -> str**
- **Input**: section dict + current section content
- **Validates against**: `section['specs']` (original requirements)
- **Dual assessment**: Data completeness + Investor perspective
- **Critical constraint**: Only suggest if source data exists
- **Materiality filter**: CRITICAL, IMPORTANT, USEFUL levels
- **Max**: 5 suggestions per section
- **Output**: ADD list with source locations
- **Used in**: Step 2 (_check_section_completeness)

**4. get_section_enhancement_prompt(section: dict, section_content: str, add_list: str) -> str**
- **Input**: section dict + current content + ADD list
- **Instructions**:
  - Add ALL items from ADD list where data exists
  - Skip silently if no supporting data (no placeholders)
  - Maintain narrative flow and bullet format
  - Preserve existing content
- **Prohibits**: "metrics not disclosed" type statements
- **Used in**: Step 3 (_enhance_section)

**5. get_section_polish_prompt(section: dict, section_content: str, word_limit: int) -> str**
- **Input**: section dict + current content + word limit (100)
- **References**: `section['specs']` for relevance filter
- **Condensing logic**:
  - Preserve only content addressing original requirements
  - Remove generic statements and repetition
  - Maintain data density (numbers, percentages, trends)
  - Target: ≤100 words
- **Critical output rules**:
  - NO preambles ("Here is...")
  - NO duplicate section names
  - Start directly with first bullet point
- **Used in**: Step 4 (_polish_section)

**Key Difference from Old Architecture**:
- All prompts are section-specific (not whole-document)
- Each prompt references section dict from opp_sections.py
- Enables parallel processing of sections

### 4. src/pptx_generator.py - PowerPoint Generation

**Purpose**: Converts markdown profile to professionally formatted PowerPoint presentation.

**(No changes from previous architecture - still parses final_profile.md and generates PowerPoint)**

#### Key Functions

**1. parse_markdown_profile(md_path: str) -> Dict**
```python
# Extracts:
#   - Title: # Title
#   - Subtitle: First line after title
#   - Sections: ## Section Name with bullets
# Returns: {'title': str, 'subtitle': str, 'sections': {...}}
```

**Regex Patterns**:
- Title: `r'^#\s+(.+?)$'`
- Subtitle: `r'^#\s+.+?\n(.+?)(?=\n##)'`
- Sections: `r'##\s+(.+?)\n(.*?)(?=\n##|\Z)'`
- Bullets: Lines starting with `*`

**2. create_profile_pptx(md_path: str, company_name: str, timestamp: str) -> str**
```python
# Pipeline:
#   1. Parse markdown → structured data
#   2. Create presentation (A4 landscape)
#   3. Add title area
#   4. Add 2×2 grid of section boxes
#   5. Add footnote
#   6. Save to ReportsOPP/
# Returns: Path to .pptx file
```

**Slide Layout**:
- **Size**: 11.69" × 8.27" (A4 landscape)
- **Title**: 0.5" top, Arial Bold 32pt, dark blue
- **Subtitle**: 1.053" top, Arial Bold 24pt, dark blue
- **Boxes**: 2×2 grid starting at 1.806" top
- **Footnote**: 8.05" top, Arial 7pt, dark blue

**3. _add_title_area(slide, title: str, subtitle: str)**
- Title: 32pt bold, top margin
- Subtitle: 24pt bold, 5mm below title (reduced spacing)

**4. _add_2x2_boxes(slide, sections: Dict[str, List[str]])**

**Layout Grid**:
```
┌─────────────────────┬─────────────────────┐
│  Company Overview   │ Competitive         │
│  (0.5", 1.806")    │ Positioning         │
│  5.195" × 2.635"   │ (6.195", 1.806")   │
├─────────────────────┼─────────────────────┤
│  Financial KPIs     │ Strategic           │
│  (0.5", 4.741")    │ Considerations      │
│  5.195" × 2.635"   │ (6.195", 4.741")   │
└─────────────────────┴─────────────────────┘
```

**5. _add_section_box(slide, section_name, bullets, left, top, width, height)**
- Section title: 12pt bold, dark blue, 6pt space after
- Bullets: Native PowerPoint bullet formatting
- Text: Arial 10pt with **bold** keyword parsing

**6. _format_bullet_text(paragraph, text: str)**

**Markdown Parsing**:
```python
# Split by **bold** markers: r'(\*\*.*?\*\*)'
# For each part:
#   - **keyword** → Dark blue, bold, 10pt
#   - Regular text → Dark grey, 10pt
```

**7. _enable_bullet(paragraph)**

**Native PowerPoint Bullets**:
```python
# Sets via XML:
#   - Bullet character: •
#   - marL: 114300 EMUs (0.125")  # Where text starts
#   - indent: -114300 EMUs        # Bullet offset (hanging)
#   - buFont: Arial
# Result: Proper hanging indent for wrapped lines
```

**Key Insight**: Reduced indent (0.125" vs default 0.25") for tighter spacing.

**8. _add_footnote(slide)**
```python
# Text: "Note: Generated with OnePageProfile v{OPP_VERSION} as of {DD-MMM-YY}."
# Position: Bottom (8.05" from top)
# Font: Arial 7pt, dark blue
# Version: Reads from OPP.__opp_version__
```

#### Color Scheme

```python
DARK_BLUE = RGBColor(45, 90, 135)   # #2d5a87 - titles, bold keywords
DARK_GREY = RGBColor(74, 85, 104)   # #4a5568 - regular text
```

Matches PD2's color scheme for brand consistency.

#### Version Management

```python
def _get_opp_version():
    # Reads __opp_version__ from OPP.py
    # Avoids circular import by reading file directly
    # Fallback: "1.0"
```

## Data Flow & Processing Pipeline

### Input Processing Flow
```
1. PDF Selection
   └─> File dialog (multi-select)

2. PDF Encoding
   ├─> Read each PDF as binary
   ├─> Base64 encode
   └─> Create Gemini API parts list
       [{mime_type: "application/pdf", data: base64_data}, ...]
```

**Key Difference from PD2**: No PDF→Markdown conversion, no caching.

### Profile Generation Flow (Parallel Architecture)

```
Preparation Phase
     │  • Prepare PDFs once (encode to base64)
     │  • Store in self.pdf_parts (shared by all workers)
     │  • Extract company name
     │  • Generate title and subtitle
     ↓
Parallel Section Processing (ThreadPoolExecutor)
     ├─────────────┬─────────────┬─────────────┬─────────────┐
     │ Section 1   │ Section 2   │ Section 3   │ Section 4   │
     │ (Worker 1)  │ (Worker 2)  │ (Worker 1)  │ (Worker 2)  │  ← With 2 workers
     │             │             │             │             │
     │ Step 1:     │ Step 1:     │ Step 1:     │ Step 1:     │
     │ Draft       │ Draft       │ Draft       │ Draft       │
     │ ↓           │ ↓           │ ↓           │ ↓           │
     │ Step 2:     │ Step 2:     │ Step 2:     │ Step 2:     │
     │ Check       │ Check       │ Check       │ Check       │
     │ ↓           │ ↓           │ ↓           │ ↓           │
     │ Step 3:     │ Step 3:     │ Step 3:     │ Step 3:     │
     │ Enhance     │ Enhance     │ Enhance     │ Enhance     │
     │ ↓           │ ↓           │ ↓           │ ↓           │
     │ Step 4:     │ Step 4:     │ Step 4:     │ Step 4:     │
     │ Polish      │ Polish      │ Polish      │ Polish      │
     │ (100 words) │ (100 words) │ (100 words) │ (100 words) │
     │ ↓           │ ↓           │ ↓           │ ↓           │
     │ Step 5:     │ Step 5:     │ Step 5:     │ Step 5:     │
     │ Learn       │ Learn       │ Learn       │ Learn       │
     │             │             │             │             │
     └─────────────┴─────────────┴─────────────┴─────────────┘
                               ↓
                    Wait for all to complete
                               ↓
                    Sort results by section number
                               ↓
                    Assemble final markdown
                               ↓
                    Title + Subtitle + 4 Sections
                               ↓
                    Save to final_profile.md
                               ↓
Memory Review & Learning
     │  • Collect all step5_learning.json files
     │  • Synthesize universal methodologies with LLM
     │  • Apply quality filtering (9-10/10 only)
     │  • Update memory/opp_learning_memory.json
     │  • Archive old memory, save new memory
                               ↓
PowerPoint Generation
     │  • Input: final_profile.md
     │  • Parse: Extract title, subtitle, sections, bullets
     │  • Format: A4 landscape, 2×2 grid, native bullets
     │  • Output: ReportsOPP/[Company]_[timestamp].pptx
```

**Key Features**:
- **PDF Reuse**: PDF parts prepared once, shared across all workers (memory efficient)
- **True Parallelism**: With 2 workers, sections 1&3 and 2&4 run simultaneously
- **Independent Processing**: Each section goes through all 4 steps without dependencies
- **Progress Tracking**: WorkerDisplay shows real-time status of active workers
- **Deterministic Output**: Results sorted by section number before assembly

**API Call Count**:
- Title/subtitle: 1 call
- Company name extraction: 1 call
- Section processing: 4 sections × 4 steps = 16 calls
- **Total: 18 API calls** (vs 7 in sequential architecture)

**Wall-Clock Time** (with 2 workers):
- Sequential: 4 sections × (draft + check + enhance + polish) = ~6-8 minutes
- Parallel: max(section times) ≈ ~3-4 minutes (nearly 2x faster)

### Output Generation Flow
```
1. Section Results (from parallel processing)
   ├─> Collected as they complete
   ├─> Sorted by section number (1-4)
   └─> Each contains: title + polished content

2. Assemble Markdown
   ├─> Combine title/subtitle with section content
   ├─> Format: Title\n\nSubtitle\n\n## Section1\n...\n\n## Section2\n...
   └─> Save to runs/opp_TIMESTAMP/final_profile.md

3. Parse Markdown
   ├─> Extract title (# line)
   ├─> Extract subtitle (line after #)
   └─> Extract 4 sections with bullets (## + * lines)

4. Create PowerPoint
   ├─> Set A4 landscape dimensions
   ├─> Add title and subtitle
   ├─> Add 2×2 grid of sections
   │   ├─> Top-left: Company Overview
   │   ├─> Top-right: Competitive Positioning
   │   ├─> Bottom-left: Financial KPIs
   │   └─> Bottom-right: Strategic Considerations
   ├─> Add footnote with version and date
   └─> Save to ReportsOPP/[Company]_[timestamp].pptx

5. Run Log
   └─> Save processing summary to runs/opp_TIMESTAMP/run_log.txt
```

## Error Handling & Recovery Strategies

### Retry Strategy
- Same as PD2: Exponential backoff (1s, 2s, 4s) with jitter
- Honors Retry-After headers from API
- Max 3 retries before giving up

### Fallback Mechanisms
1. **Company name extraction failure** → Use "Company Profile"
2. **Empty section output** → Keep previous step's content
3. **PowerPoint generation failure** → Log error, continue (markdown still available)
4. **Invalid markdown structure** → Best-effort parsing, skip malformed sections

### Validation Points
- Company name: 2-50 characters
- Section presence: All 4 sections required
- Bullet format: Must start with `*`
- Word count: Check at polish step (informational only, not blocking)

## Performance Characteristics

### Processing Time (With 2 Workers - Default)

**Sequential Operations**:
- **PDF encoding**: ~1-2 seconds (depends on file size, done once)
- **Company name extraction**: ~5-10 seconds
- **Title/subtitle generation**: ~10-20 seconds

**Parallel Section Processing** (per section, but 2 run simultaneously):
- **Step 1 (Draft)**: ~30-45 seconds per section
- **Step 2 (Check)**: ~15-30 seconds per section
- **Step 3 (Enhance)**: ~30-45 seconds per section
- **Step 4 (Polish)**: ~20-40 seconds per section
- **Total per section**: ~2-3 minutes

**Overall Timeline**:
- With 2 workers processing 4 sections in parallel:
  - Worker 1: Sections 1 & 3 (sequential within worker)
  - Worker 2: Sections 2 & 4 (sequential within worker)
- **Wall-clock time**: max(Worker1, Worker2) ≈ 4-6 minutes
- **PowerPoint generation**: ~1-2 seconds
- **Total**: ~3-7 minutes per profile

**Comparison by Worker Count**:
- 1 worker: ~8-12 minutes (fully sequential)
- 2 workers: ~3-7 minutes (2x parallelism) ← **Default**
- 3 workers: ~3-5 minutes (3 sections parallel, 1 waits)
- 4 workers: ~2.5-4 minutes (all 4 sections parallel)

**Speedup Analysis**:
- 2 workers vs 1: ~50% faster
- 4 workers vs 1: ~60-70% faster (diminishing returns due to overhead)

### Resource Usage

**Memory**:
- **Base**: ~500MB-1GB (PDFs in memory as base64, prepared once)
- **Per worker**: ~50-100MB additional overhead
- **Total with 2 workers**: ~600MB-1.2GB

**API Calls**:
- Company name: 1 call
- Title/subtitle: 1 call
- Section processing: 4 sections × 4 steps = 16 calls
- **Total**: 18 API calls per profile

**Token Usage**:
- Per section per step: ~5-10k tokens
- **Total**: ~80-160k tokens per profile (higher than sequential due to no whole-document processing)

**Disk**:
- Per section: ~50-100KB (4 step files)
- Final profile: ~50KB
- Run log: ~2KB
- **Total**: ~250-450KB per run

**Network**:
- Parallel API calls (up to 2-4 simultaneous)
- Requires stable internet connection
- API quota: 18 calls can complete in 3-7 minutes

### Scalability Considerations

**Worker Count Selection**:
- **1 worker**: Use for API quota conservation or debugging
- **2 workers**: Optimal balance of speed and resource usage (default)
- **3-4 workers**: Maximum speed, higher memory and API rate usage

**Bottlenecks**:
- API rate limits (Gemini free tier: 15 RPM)
- Network latency and bandwidth
- PDF size affecting encoding time

**Future Optimization Opportunities**:
1. **PDF streaming**: Reduce memory footprint for large files
2. **Adaptive worker count**: Auto-detect based on API quota
3. **Batch processing**: Multiple companies in one session
4. **Result caching**: Store successful steps for retry scenarios

## Configuration & Environment

### Environment Variables
```bash
GEMINI_API_KEY=your_key_here        # Required
```

### Key Dependencies
```
python-pptx             # PowerPoint generation (new for OPP)
google-generativeai     # Gemini API client
python-dotenv           # Environment management
pathlib                 # File operations
tkinter                 # File selection dialog
```

### System Requirements
- Python 3.8+
- 2GB RAM minimum (4GB recommended)
- Gemini API key (free tier sufficient)
- Internet connection

## Output Artifacts

### Final Deliverables
```
ReportsOPP/
└── [Company]_YYMMDD_HHMMSS.pptx
    • A4 landscape (11.69" × 8.27")
    • Single slide with 2×2 grid
    • Native bullet formatting
    • Dark blue titles and bold keywords
    • Footnote with version and date
```

### Work Products
```
runs/opp_YYMMDD_HHMMSS/
├── section_1/                    # Company Overview
│   ├── step1_draft.md            # Initial draft (with learned memory)
│   ├── step2_add_list.txt        # Completeness check ADD list
│   ├── step3_enhanced.md         # Enhanced with additions
│   ├── step4_cleaned.md          # Cleaned/redistributed content
│   ├── step5_polished.md         # Polished to 100 words
│   └── step5_learning.json       # Universal methodologies extracted
├── section_2/                    # Competitive Positioning
│   ├── step1_draft.md
│   ├── step2_add_list.txt
│   ├── step3_enhanced.md
│   ├── step4_cleaned.md
│   ├── step5_polished.md
│   └── step5_learning.json
├── section_3/                    # Financial KPIs
│   ├── step1_draft.md
│   ├── step2_add_list.txt
│   ├── step3_enhanced.md
│   ├── step4_cleaned.md
│   ├── step5_polished.md
│   └── step5_learning.json
├── section_4/                    # Strategic Considerations
│   ├── step1_draft.md
│   ├── step2_add_list.txt
│   ├── step3_enhanced.md
│   ├── step4_cleaned.md
│   ├── step5_polished.md
│   └── step5_learning.json
├── final_profile.md              # Assembled final output
├── run_log.txt                   # Processing log with worker info
├── new_insights.txt              # Synthesis of learning extractions
└── post_run_memory.json          # Memory state after this run
```

**Key Benefits of New Structure**:
- Each section's full refinement history is isolated
- Easy to debug individual section issues
- Parallel processing artifacts are clearly separated
- Can compare section evolution across all 4 steps

## Key Algorithms & Techniques

### 1. Parallel Section Processing Algorithm
```
Input: 4 section definitions, PDF parts, company name, worker count
Output: Final assembled profile

1. Preparation (sequential):
   a. Encode PDFs to base64 (done once)
   b. Extract company name
   c. Generate title and subtitle

2. Parallel Processing (ThreadPoolExecutor):
   a. Create worker pool (1-4 workers)
   b. Submit 4 section tasks
   c. Each worker processes section through 5 steps:
      - Step 1: Draft (with learned memory)
      - Step 2: Completeness check
      - Step 3: Enhancement
      - Step 4: Polish (100 words)
      - Step 5: Learning extraction
   d. Collect results as they complete
   e. Track progress with WorkerDisplay

3. Assembly (sequential):
   a. Sort results by section number
   b. Combine: title + subtitle + sections
   c. Save to final_profile.md
   d. Conduct memory review (synthesize universal learnings)

4. Output Generation:
   a. Parse markdown
   b. Generate PowerPoint
```

**Key Insights**:
- PDF parts prepared once, shared across workers (memory efficient)
- Each section processed independently (no cross-section dependencies)
- Deterministic output via sorting (regardless of completion order)
- Progress tracking with thread-safe WorkerDisplay

### 2. Markdown **Bold** Parsing Algorithm
```
Input: "This is **keyword** text with **another** word"
Output: PowerPoint runs with mixed formatting

1. Split by regex: r'(\*\*.*?\*\*)'
   → ["This is ", "**keyword**", " text with ", "**another**", " word"]
2. For each part:
   a. If starts/ends with **:
      → Bold run, dark blue, 10pt
   b. Else:
      → Regular run, dark grey, 10pt
3. Append all runs to paragraph
```

**Key Insight**: Preserves markdown bold syntax in PowerPoint with visual distinction.

### 3. Native Bullet Formatting Algorithm
```
Input: Paragraph object in PowerPoint
Output: Properly formatted bullet with hanging indent

1. Access paragraph XML element (pPr)
2. Set marL attribute: 114300 EMUs (0.125")
   → Where text starts (left margin)
3. Set indent attribute: -114300 EMUs (-0.125")
   → Where bullet is positioned (hanging)
4. Create buChar element: • character
5. Create buFont element: Arial
6. Result: PowerPoint handles wrapped line alignment automatically
```

**Key Insight**: Using PowerPoint's native bullet system instead of manual "• " text ensures proper wrapping behavior.

### 4. Regex Cleanup Algorithm
```
Input: LLM output with potential preambles
Output: Clean section content

Patterns to remove (case-insensitive):
1. r'^Here is .*?section:?\s*\n+'
2. r'^This is .*?section:?\s*\n+'
3. r'^\*\*' + section_name + r'\*\*\s*\n+'  # Duplicate bold title
4. r'^\*\*SECTION:\s*' + section_name + r'\*\*\s*\n+'

Applied after LLM generation as safety net.
```

**Design Pattern**: Defense in depth - prompt instructions + regex cleanup.

## Comparison: OPP vs PD2

### Similarities
- Gemini API client
- Progressive refinement approach
- Markdown intermediate format
- Thread-safe console output
- Color scheme (dark blue, dark grey)
- Version management pattern

### Differences

| Aspect | OPP | PD2 |
|--------|-----|-----|
| **Purpose** | Quick M&A screening | Comprehensive analysis |
| **Sections** | 4 fixed sections | 33 configurable sections |
| **Pipeline** | 5-step vanilla + optional 12-step insights | 6 steps per section (+ discovery) |
| **Word limit** | 100 per section | ~500 per section (Step 4) |
| **Output** | Markdown + PowerPoint (1-3 variants) | Markdown + PDF |
| **PDF handling** | Files API + context caching | Convert to markdown |
| **Insights** | Ground truth discovery + hypothesis testing | Insight memory across runs |
| **Parallel** | ThreadPoolExecutor (1-4 workers) | ThreadPoolExecutor (1-8 workers) |
| **Processing time** | 3-5 min (vanilla) / 8-12 min (insights) | 30-60 minutes |
| **Complexity** | Focused with optional depth | Sophisticated, extensible |
| **API calls** | ~18 (vanilla) / ~42 (insights) per profile | 200+ per profile |

## Known Issues & Limitations

### Design Constraints
1. **Fixed sections**: Cannot customize the 4 section definitions
2. **No cross-run learning**: Each run starts fresh (unlike PD2)
3. **Single slide**: PowerPoint limited to one slide per variant
4. **Limited worker range**: Max 4 parallel workers (vs PD2's 8)

### Technical Limitations
1. **PDF memory**: All PDFs loaded into memory via Files API
   - Large PDF sets (>100MB total) may cause issues
   - Mitigation: Batch mode processes one PDF at a time

2. **PowerPoint complexity**: python-pptx library limitations
   - Native bullet formatting requires XML manipulation
   - Complex formatting may not render identically in all viewers

3. **LLM output variability**:
   - May add preambles despite instructions
   - May duplicate section titles
   - Mitigation: Regex cleanup patterns

4. **Hypothesis anchoring**: If Step 7 accidentally receives document context, hypotheses may anchor to source framing

## Future Enhancement Opportunities

### Potential Improvements
1. **Multi-slide support**: Option for detailed PowerPoint (1 section per slide)
2. **PDF streaming**: Reduce memory footprint for large files
3. **Word output**: Generate .docx in addition to .pptx
4. **Template customization**: Configurable color schemes and layouts
5. **Comparison mode**: Side-by-side profiles for multiple companies
6. **Adaptive worker count**: Auto-detect optimal workers based on API quota

### Architectural Enhancements
1. **Plugin system**: Extensible output formats
2. **Web interface**: Browser-based UI instead of CLI
3. **API mode**: RESTful service for integration
4. **Cloud deployment**: Serverless function implementation

## Conclusion

OnePageProfile v1.4 represents a focused, pragmatic approach to M&A screening with optional analytical depth:

- **Fast**: 3-5 minutes (vanilla) or 8-12 minutes (with insights)
- **Clear**: 100 words per section, dense with data
- **Insightful**: Optional ground truth discovery and hypothesis testing
- **Professional**: PowerPoint output with native formatting (1-3 variants)
- **Scalable**: Batch processing for multiple PDFs, configurable workers (1-4)
- **Cost-effective**: Context caching for 85-90% input token savings

The system offers two modes: vanilla for rapid screening, or insights-enabled for hypothesis-driven analysis that goes beyond management narrative.

**Key Design Decisions**:
- Files API + context caching for cost efficiency
- Parallel section processing (ThreadPoolExecutor with 1-4 workers)
- Section definitions with ground_truth_pointer in opp_sections.py
- Step 7 deliberately has NO document access (prevents anchoring)
- Three PPTX variants when insights enabled (vanilla, insights, integrated)
- Native PowerPoint bullets (proper formatting)
- Batch mode integrated into main CLI

**Recommended Usage**:
1. Initial screening of acquisition targets (vanilla mode)
2. Deep-dive on shortlisted targets (insights mode)
3. Batch processing of CIM documents
4. Quick updates on portfolio companies
5. Preparation for investor meetings

**Recommended Next Steps for New Developers**:
1. Read `Manuals/Profile_Page.md` for feature specification
2. Review `src/profile_prompts.py` to understand Steps 1-12
3. Trace execution flow starting from `OPP.py` main block
4. Generate a test profile with sample PDFs (try both vanilla and insights modes)
5. Examine `runs/opp_*/` directory outputs (compare step files)
6. Open generated PowerPoint variants to understand formatting
7. Compare with PD2 in `Manuals/CODE_OVERVIEW.md`

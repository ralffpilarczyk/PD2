# OPP Code Overview - Complete Technical Documentation (v1.2)

## Executive Summary

OnePageProfile (OPP) v1.2 is a focused document analysis tool that transforms PDF documents into concise one-page company profiles for M&A evaluation using Google's Gemini LLM API. The system employs a 3-phase architecture with parallel processing, sequential deduplication, and optional density iterations. New in v1.2: custom section definitions and improved data recency handling.

**Key Metrics**:
- 4 analytical sections (default or custom-defined)
- 3-phase architecture: Parallel Draft/Check/Enhance → Sequential Dedup (reverse) → Parallel Polish
- 100-word limit per section
- Parallel processing with 1-4 configurable workers
- Optional 1-3 density iterations for enhanced output quality
- Dual output: Markdown + PowerPoint
- Processing time: ~3-5 minutes per profile (with 4 workers, 1 iteration)
- No learning system (removed in v1.1 for simplicity)
- Custom sections support (new in v1.2)

## What's New in v1.2

### 1. Custom Section Definitions

Users can now define custom section specifications for specialized use cases:

- **Custom file**: `src/opp_sections_custom.py` (ships with default sections as working example)
- **Usage**: Simply edit the file to define your custom sections
- **Reset**: `git checkout src/opp_sections_custom.py` to restore defaults
- **Structure**: Must maintain exactly 4 sections for 2×2 PowerPoint layout
- **Validation**: Automatic validation on load prevents malformed sections
- **Output naming**: Custom profiles use distinct naming (`Custom_*.pptx`, `runs/opp_custom_*/`)
- **Dynamic loading**: Profile type selection in UI determines which sections to load

### 2. Density Iterations

Optional multi-iteration processing for enhanced content quality:

- **Iteration count**: 1-3 iterations (default: 1)
- **Per iteration**:
  - Subtitle refinement based on evolved content
  - Completeness check flags missing/outdated content
  - Density enhancement when no gaps found
  - Full 3-phase processing with deduplication and polish
- **Versioned outputs**: `_v1.pptx`, `_v2.pptx`, `_v3.pptx` for comparison
- **Processing time**: ~3-5 minutes per iteration

### 3. Temporal Bias for Data Recency

Enhanced prompts prioritize recent data over historical data:

- **Principle**: "ALWAYS use the most recent data available in source documents"
- **Completeness check**: Part C identifies outdated metrics for replacement
- **Enhancement step**: Can replace old data with newer data (not just additive)
- **Polish step**: Prefers most recent fiscal period when multiple metrics exist
- **Problem solved**: Prevents LLM from selecting 2019 prospectus data when 2025 financials exist

### 4. M&A Clarity Principles

Universal quality improvements for banker-friendly outputs:

- **Metrics with context**: Every metric must include units AND time/period
- **Redundancy elimination**: Never repeat facts across bullets
- **Subsector precision**: Exact subsector specification (e.g., "pharmaceutical distribution" not "healthcare")
- **Financial clarity**: Explicit units for all metrics (%, currency, etc.)
- **Deal implications**: Strategic observations must explain WHY they matter for valuation/synergies/risk
- **Subtitle format**: Up to 8 words as prose statement (not cryptic keywords)

## Architecture Overview

```
┌─────────────────────────────────────────────────────┐
│                    User Input                        │
│                  (PDF Documents)                     │
└────────────────────┬───────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────┐
│                   OPP.py                            │
│         (Main Orchestrator & CLI Interface)          │
│   • Gemini API configuration                         │
│   • PDF selection and encoding                       │
│   • ThreadPoolExecutor (1-4 workers)                 │
│   • Parallel section processing                      │
│   • PowerPoint generation                            │
└────────────────────┬───────────────────────────────┘
                     │
        ┌────────────┼────────────┐
        ▼            ▼            ▼
┌──────────────┐ ┌────────────┐ ┌──────────────────┐
│opp_sections  │ │profile_    │ │pptx_generator    │
│.py           │ │prompts.py  │ │                  │
│              │ │            │ │                  │
│•4 section    │ │•Prompt     │ │•Markdown parsing │
│ definitions  │ │ templates  │ │•Native bullets   │
│•Specs for    │ │•Single-    │ │•A4 landscape     │
│ each section │ │ section    │ │•Formatting       │
└──────────────┘ │ processing │ └──────────────────┘
                 └────────────┘
```

## Core Principles & Design Philosophy

### 1. Speed Over Depth
- Optimized for quick M&A screening, not comprehensive analysis
- 3-phase architecture: Parallel Steps 1-3 → Sequential Dedup → Parallel Polish
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

### 5. Three-Phase Processing with Deduplication (introduced v1.1, enhanced v1.2)
- **Phase 1**: Parallel Draft/Check/Enhance for all 4 sections
- **Phase 2**: Sequential deduplication in reverse priority (Section 4→3→2→1)
  - Section 4 (Strategic Considerations) processes first, keeps richest content
  - Earlier sections remove overlaps with later ones
- **Phase 3**: Parallel polish to 100 words
- ThreadPoolExecutor manages worker pools for parallel phases
- WorkerDisplay provides real-time progress tracking

## File Structure & Module Breakdown

```
PD2/
├── OPP.py                           # Main entry point (~930 lines)
├── src/
│   ├── opp_sections.py             # Default section definitions (~97 lines)
│   ├── opp_sections_custom.py      # Custom sections (edit directly, ships with defaults) - NEW v1.2
│   ├── profile_prompts.py          # Prompt templates (~420 lines, enhanced v1.2)
│   ├── pptx_generator.py           # PowerPoint generation (~140 lines)
│   ├── file_manager.py             # File I/O and directory management
│   └── utils.py                    # Thread-safe utilities (shared with PD2)
├── Manuals/
│   ├── OPP_OVERVIEW.md             # This technical documentation
│   ├── Profile_Page.md             # User-facing specification
│   └── Refinement.md               # Pipeline documentation
├── requirements.txt                # Dependencies (includes python-pptx)
├── ProfileFiles/                   # PowerPoint output directory
│   ├── [Company]_TIMESTAMP.pptx    # Default profiles
│   └── Custom_[Company]_TIMESTAMP.pptx  # Custom profiles
└── runs/                           # Processing work products
    ├── opp_YYMMDD_HHMMSS/          # Default profile runs
    │   ├── section_1/              # Company Overview steps
    │   ├── section_2/              # Competitive Positioning steps
    │   ├── section_3/              # Financial KPIs steps
    │   ├── section_4/              # Strategic Considerations steps
    │   ├── step4_final_v1.md       # Iteration 1 output
    │   ├── step4_final_v2.md       # Iteration 2 output (if enabled)
    │   └── step4_final_v3.md       # Iteration 3 output (if enabled)
    └── opp_custom_YYMMDD_HHMMSS/   # Custom profile runs - NEW v1.2
```

## Detailed Module Documentation

### 1. OPP.py - Main Orchestrator

**Purpose**: Entry point and orchestration layer for parallel one-page profile generation.

#### OnePageProfile Class

```python
class OnePageProfile:
    def __init__(self, pdf_files: List[str], model_name: str, workers: int = 2,
                 iterations: int = 1, profile_type: str = "default"):
        self.pdf_files = pdf_files
        self.model_name = model_name
        self.workers = workers              # Parallel worker count (1-4)
        self.iterations = iterations        # Density iterations (1-3) - NEW v1.2
        self.profile_type = profile_type    # "default" or "custom" - NEW v1.2
        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        # Dynamic section loading based on profile type - NEW v1.2
        if profile_type == "custom":
            from src.opp_sections_custom import sections, get_section_boundaries
            self.run_dir_prefix = "opp_custom"
        else:
            from src.opp_sections import sections, get_section_boundaries
            self.run_dir_prefix = "opp"

        self.sections = sections
        self.get_section_boundaries = get_section_boundaries
        self._validate_sections()  # Validate structure - NEW v1.2

        # Two models with different temperatures
        self.model_low_temp = genai.GenerativeModel(model_name, temperature=0.2)
        self.model_medium_temp = genai.GenerativeModel(model_name, temperature=0.6)

        # PDF parts prepared once and reused by all workers
        self.pdf_parts = None
```

**Key Design Decisions**:
- Configurable parallel workers (1-4, default 4) - changed default in v1.2
- Configurable density iterations (1-3, default 1) - NEW v1.2
- Dynamic section loading (default or custom) - NEW v1.2
- Separate models for different tasks (temperature optimization)
- PDF parts prepared once, shared across all workers (memory efficient)
- Run directory: `runs/opp_TIMESTAMP/` or `runs/opp_custom_TIMESTAMP/` with section_N subdirectories
- Version tracking: `__opp_version__ = "1.2"` (distinct from PD2's `__version__`)

#### WorkerDisplay Class

```python
class WorkerDisplay:
    def __init__(self, num_workers: int):
        self.num_workers = num_workers
        self.status = {}  # {section_num: status_text}
        self.lock = threading.Lock()

    def update(self, section_num: int, action: str):
        # Show real-time progress: "Sec. 1 → Draft | Sec. 2 → Polish"

    def complete(self, section_num: int, completed: int, total: int):
        # Mark section complete: "✓ Section 1 complete (1/4)"
```

**Key Features**:
- Thread-safe status updates with lock
- Real-time display of active workers
- Completion tracking with progress counter

#### The 3-Phase Architecture (v1.2)

**Phase 1 - Parallel Steps 1-3**: All 4 sections process Draft/Check/Enhance simultaneously.

**Phase 2 - Sequential Deduplication**: Sections deduplicate in reverse order (4→3→2→1) to eliminate redundancy.

**Phase 3 - Parallel Polish**: All 4 sections polish to 100 words simultaneously.

---

### Steps 1-3: Draft, Check, Enhance (Parallel Phase 1)

**process_section_main(section: dict, worker_display: WorkerDisplay) -> dict**
```python
# Orchestrates one section through Steps 1-3 (Phase 1)
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

### Step 4a: Deduplication (Sequential Phase 2 - introduced v1.1)

**Architecture**: After Phase 1 completes, sections are deduplicated **sequentially in reverse order (4→3→2→1)** to eliminate overlaps while preserving the richest content.

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

### Step 4b: Polish (Parallel Phase 3)
```python
def _polish_section(self, section: dict, content: str, word_limit: int) -> str:
    # Input: section dict + deduplicated content + word limit (100)
    # Temperature: 0.6 (balanced)
    # Condenses to essential insights only
    # Output: section_N/step4_polished.md
```

**Detailed Input/Output**:
- **Input**:
  - `section`: Section definition with specs
  - `content`: Deduplicated content from Step 4a
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
- **Saved to**: `section_1/step4_polished.md`
- **Fallback**: If polish fails, returns deduplicated content unchanged

---

After all sections complete the 3-phase pipeline, the final polished content from each section is assembled into the complete profile.

---

## Parallel Execution Architecture (v1.2)

**3-Phase Processing**:
```python
def generate_profile(self, company_name: str, worker_display: WorkerDisplay) -> str:
    # Phase 1: Parallel Steps 1-3 (Draft/Check/Enhance) for all sections
    # Phase 2: Sequential Dedup in reverse (4→3→2→1)
    # Phase 3: Parallel Polish to 100 words for all sections
    # Returns: Assembled markdown with all 4 sections
```

**Processing Flow**:

**Phase 1 - Parallel Draft/Check/Enhance**:
1. Create ThreadPoolExecutor with `self.workers` max workers
2. Submit all 4 sections to process Steps 1-3 in parallel
3. Collect enhanced results as they complete
4. Sort by section number for consistent processing

**Phase 2 - Sequential Deduplication (Reverse Order)**:
1. Process Section 4 first (keeps all content)
2. Process Section 3 (removes overlap with 4)
3. Process Section 2 (removes overlap with 4, 3)
4. Process Section 1 (removes overlap with 4, 3, 2)
5. Each deduplication is a single LLM call

**Phase 3 - Parallel Polish**:
1. Create new ThreadPoolExecutor
2. Submit all 4 deduplicated sections to polish in parallel
3. Each polishes to 100 words independently
4. Collect polished results and save to step4_polished.md files

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
    # 2. Select LLM model (gemini-2.5-flash or gemini-2.5-pro)
    # 3. Select worker count (1-4, default 2)
    # 4. Select PDF files (file dialog)
    # 5. Initialize OnePageProfile with workers
    # 6. Generate profile with parallel processing
    # 7. Output paths to markdown and PowerPoint
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
#   6. Save to ProfileFiles/
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
     │  • Output: ProfileFiles/[Company]_[timestamp].pptx
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
   └─> Save to ProfileFiles/[Company]_[timestamp].pptx

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
ProfileFiles/
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
│   ├── step4_polished.md         # Polished to 100 words
│   └── step5_learning.json       # Universal methodologies extracted
├── section_2/                    # Competitive Positioning
│   ├── step1_draft.md
│   ├── step2_add_list.txt
│   ├── step3_enhanced.md
│   ├── step4_polished.md
│   └── step5_learning.json
├── section_3/                    # Financial KPIs
│   ├── step1_draft.md
│   ├── step2_add_list.txt
│   ├── step3_enhanced.md
│   ├── step4_polished.md
│   └── step5_learning.json
├── section_4/                    # Strategic Considerations
│   ├── step1_draft.md
│   ├── step2_add_list.txt
│   ├── step3_enhanced.md
│   ├── step4_polished.md
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
| **Pipeline** | 4 steps per section | 6 steps per section (+ discovery) |
| **Word limit** | 100 per section | ~500 per section (Step 4) |
| **Output** | Markdown + PowerPoint | Markdown + PDF |
| **PDF handling** | Inline base64 | Convert to markdown |
| **Learning** | None | Insight memory across runs |
| **Parallel** | ThreadPoolExecutor (1-4 workers) | ThreadPoolExecutor (1-8 workers) |
| **Processing time** | 3-7 minutes (2 workers) | 30-60 minutes |
| **Complexity** | Simple, focused | Sophisticated, extensible |
| **API calls** | 18 per profile | 200+ per profile |

## Known Issues & Limitations

### Design Constraints
1. **Fixed sections**: Cannot customize the 4 sections
2. **No learning**: Each run starts fresh (unlike PD2)
3. **No discovery**: No quantitative pattern finding
4. **Single slide**: PowerPoint limited to one slide
5. **Limited worker range**: Max 4 parallel workers (vs PD2's 8)

### Technical Limitations
1. **PDF memory**: All PDFs loaded into memory as base64
   - Large PDF sets (>100MB total) may cause issues
   - Mitigation: Process in batches

2. **PowerPoint complexity**: python-pptx library limitations
   - No paragraph_format in older versions → Use XML directly
   - Native bullet formatting required XML manipulation

3. **LLM output variability**:
   - May add preambles despite instructions
   - May duplicate section titles
   - Mitigation: Regex cleanup patterns

4. **No formal testing**: Manual validation only

## Future Enhancement Opportunities

### Potential Improvements
1. **Multi-slide support**: Option for detailed PowerPoint (1 section per slide)
2. **Custom sections**: User-defined section templates
3. **Batch mode**: Process multiple companies sequentially
4. **Learning integration**: Optional connection to PD2's insight memory (architecture ready)
5. **PDF streaming**: Reduce memory footprint for large files
6. **Word output**: Generate .docx in addition to .pptx
7. **Section selection**: Allow user to pick subset of 4 sections
8. **Template customization**: Configurable color schemes and layouts
9. **Comparison mode**: Side-by-side profiles for multiple companies
10. **Adaptive worker count**: Auto-detect optimal workers based on API quota

### Architectural Enhancements
1. **Plugin system**: Extensible output formats
2. **Web interface**: Browser-based UI instead of CLI
3. **API mode**: RESTful service for integration
4. **Cloud deployment**: Serverless function implementation
5. **Caching layer**: Store intermediate results for iteration

## Conclusion

OnePageProfile represents a focused, pragmatic approach to M&A screening:
- **Fast**: 3-7 minute turnaround with parallel processing (2 workers)
- **Clear**: 100 words per section, dense with insights
- **Professional**: PowerPoint output with native formatting
- **Simple**: 5-step pipeline per section with learning, clean architecture
- **Scalable**: Configurable workers (1-4) for speed/resource trade-offs
- **Learning**: Builds universal analytical methodologies over time

The system trades PD2's depth and sophistication for speed and clarity. It's optimized for the specific use case of rapid company evaluation in M&A workflows.

**Key Design Decisions**:
- Direct PDF processing (no conversion overhead)
- Parallel section processing (ThreadPoolExecutor with 1-4 workers)
- Section definitions in opp_sections.py (single source of truth)
- Independent section processing through all 4 steps
- Native PowerPoint bullets (proper formatting)
- Defense in depth (prompt + regex) for output quality
- Architecture ready for future learning integration (like PD2)

**Recommended Usage**:
1. Initial screening of acquisition targets
2. Quick updates on portfolio companies
3. Preparation for investor meetings
4. Pitch deck appendix material
5. CIM comparison summaries

**Recommended Next Steps for New Developers**:
1. Read `Manuals/Profile_Page.md` for feature specification
2. Review `src/profile_prompts.py` to understand section requirements
3. Trace execution flow starting from `OPP.py` main block
4. Generate a test profile with sample PDFs
5. Examine `runs/opp_*/` directory outputs
6. Open generated PowerPoint to understand formatting
7. Compare with PD2 in `Manuals/CODE_OVERVIEW.md`

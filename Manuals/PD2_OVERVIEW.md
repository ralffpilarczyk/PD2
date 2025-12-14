# PD2 Code Overview - Complete Technical Documentation

## Executive Summary

ProfileDash 2.2 (PD2) is a sophisticated financial document analysis system that transforms PDF documents into comprehensive 34-section company profiles using Google's Gemini LLM API. The system employs a multi-stage refinement pipeline with parallel processing and intelligent caching.

**Key Metrics**:
- 34 analytical sections organized into 6 groups
- 4-step progressive refinement pipeline per section (sections 1-32)
- Custom 4-layer hypothesis-driven pipeline for Section 34 (Financial Pattern Analysis)
- Configurable 1-8 parallel workers
- Professional PDF output with page footers and numbering

## Architecture Overview

```
┌─────────────────────────────────────────────────────┐
│                    User Input                        │
│            (PDFs/Markdown Documents)                 │
└────────────────────┬───────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────┐
│                   PD2.py                            │
│         (Main Orchestrator & CLI Interface)          │
│   • Model selection (Gemini 2.5/2.0)                │
│   • File selection & PDF conversion                  │
│   • Section group selection                          │
│   • Worker pool management                           │
│   • Real-time parallel worker display                │
└────────────────────┬───────────────────────────────┘
                     │
        ┌────────────┴────────────┬──────────────┐
        ▼                         ▼              ▼
┌──────────────┐      ┌──────────────┐   ┌──────────────┐
│FileManager   │      │CoreAnalyzer  │   │ProfileGen    │
│              │      │              │   │              │
│•Load files   │      │•6-step       │   │•PDF output   │
│•PDF→Markdown │      │ refinement   │   │•HTML→PDF     │
│•Caching      │      │•LLM calls    │   │•Styling      │
│•Table cleanup│      │•Learning     │   │•Footers      │
└──────────────┘      └──────┬───────┘   └──────────────┘
                             │
                    ┌────────┴────────┐
                    ▼                 ▼
            ┌──────────────┐  ┌──────────────┐
            │InsightMemory │  │QualityTracker│
            │              │  │              │
            │•Pattern      │  │•Metrics      │
            │ storage      │  │•Scoring      │
            │•Retrieval    │  │•Analytics    │
            └──────────────┘  └──────────────┘
```

## Core Principles & Design Philosophy

### 1. Progressive Refinement
Analysis proceeds through multiple stages, each with specific goals:
- **Expansion phase** (Steps 1-3): Capture all relevant data, identify gaps
- **Condensation phase** (Step 4): Distill to essential insights with investor relevance filter

### 2. Parallel Processing Architecture
- **Process pools** for PDF conversion (CPU-intensive, PyTorch isolation required)
- **Thread pools** for section analysis (I/O-bound, API call intensive)
- **Semaphore limiting** to prevent API quota exhaustion
- **Real-time worker display** shows active sections and their current refinement stage

### 3. Quality-First Approach
- Every insight must pass: "Would this change an investor's view?"
- Learning insights filtered to quality score 9-10 only
- Table size constraints (max 10 columns × 20 rows)
- Output size limits prevent runaway generation

### 4. Graceful Degradation
- Failed sections don't block others
- Empty outputs trigger retry with fallback
- API rate limits handled with exponential backoff
- Cache prevents redundant work

## File Structure & Module Breakdown

```
PD2/
├── PD2.py                      # Main entry point (1,087 lines)
├── src/
│   ├── __init__.py            # Package initialization
│   ├── core_analyzer.py       # Analysis pipeline (438 lines)
│   ├── file_manager.py        # File I/O operations (124 lines)
│   ├── insight_memory.py      # Learning system (350 lines)
│   ├── profile_generator.py   # HTML/PDF generation (602 lines)
│   ├── pdf_generator.py       # WeasyPrint wrapper (54 lines)
│   ├── profile_sections.py    # Section definitions (711 lines)
│   ├── quality_tracker.py     # Quality metrics (54 lines)
│   └── utils.py               # Utility functions (265 lines)
├── requirements.txt           # Python dependencies
├── .env                       # API keys (not in repo)
├── CLAUDE.md                  # AI assistant instructions
└── README.md                  # User documentation
```

## Detailed Module Documentation

### 1. PD2.py - Main Orchestrator

**Purpose**: Entry point and orchestration layer coordinating all system components.

#### Key Classes

##### WorkerDisplay
Manages real-time parallel worker status display.

```python
class WorkerDisplay:
    def __init__(self, num_workers):
        # Thread-safe status tracking for parallel workers

    def update(self, section_num, action):
        # Update worker status: "Draft" | "Refine" | "Polish"

    def complete(self, section_num, completed, total):
        # Mark section complete, print completion message

    def _redraw(self):
        # Redraw active worker status line
```

**Display Format**:
```
Sec. 5 → Draft       | Sec. 12 → Refine     | Sec. 23 → Polish
Sec. 5 ✓ Complete (1/32)
```

##### IntelligentAnalyst
Main analysis orchestrator.

```python
class IntelligentAnalyst:
    def __init__(self, source_files, model_name, use_llm_pdf_conversion):
        # Initialize all components:
        # - FileManager for I/O
        # - CoreAnalyzer for section processing
        # - InsightMemory for learning
        # - QualityTracker for metrics
        # - Convert PDFs to markdown in parallel
```

**Key Methods**:

1. **`_convert_pdfs_parallel(pdf_files, use_llm)`**
   - Uses ProcessPoolExecutor (2-5 workers)
   - SHA-256 based caching in `SourceFiles/`
   - Worker count adapts: 2 for LLM mode (API-bound), 3-5 for basic (CPU-bound)
   - Returns: `{pdf_path: markdown_text}`

2. **`analyze_section(section_num)`**
   - Implements 6-step pipeline (delegated to CoreAnalyzer)
   - Integrates with WorkerDisplay for progress tracking
   - Three status updates: "Draft" → "Refine" → "Polish"
   - Special handling for Section 33 (skip refinement steps)

3. **`run_analysis(section_numbers, workers)`**
   - Two-phase scheduling: sections 1-32, then section 33
   - ThreadPoolExecutor with configurable workers (1-8)
   - Progress tracking with completion counts
   - Calls ProfileGenerator for final PDF output
   - Returns analysis results dictionary

#### Module-Level Functions

**PDF Conversion Worker**:
```python
def _pdf_conversion_worker(pdf_path: str, use_llm: bool = False) -> Optional[str]:
    # Runs in separate process
    # Sets BLAS threads to 1 (prevent resource contention)
    # Uses Marker library for PDF→Markdown
    # Returns: markdown text or None on failure
```

**UI Helper Functions**:
- `_read_single_key()`: Cross-platform single key input (Windows/Unix)
- `prompt_yes_no(prompt)`: Y/N prompts with validation
- `prompt_single_digit(prompt, options)`: Digit selection from list
- `select_source_files()`: PDF vs existing markdown selection
- `select_pdf_files()`: PDF multi-select dialog

#### Main Execution Flow

```python
if __name__ == "__main__":
    # 1. Select Gemini model (2.5-flash, 2.5-flash-lite, 2.0-flash)
    # 2. Select source files (PDFs or existing markdown)
    # 3. Optional: LLM-enhanced PDF conversion
    # 4. Initialize IntelligentAnalyst (converts PDFs)
    # 5. Select section groups:
    #    - Company Profile (1-13)
    #    - Strategy and SWOT (14-19)
    #    - Sellside Positioning (20-26)
    #    - Buyside Due Diligence (27-32)
    #    - Data Book (33)
    #    - Custom selection
    # 6. Set worker count (1-8, default 2)
    # 7. Run analysis with progress tracking
    # 8. Generate PDF report
```

### 2. src/core_analyzer.py - Analysis Pipeline

**Purpose**: Implements the 4-step progressive refinement pipeline for each section.

#### CoreAnalyzer Class

```python
class CoreAnalyzer:
    def __init__(self, full_context, run_timestamp, model_name):
        self.full_context = full_context  # All documents combined
        self.run_timestamp = run_timestamp
        self.model = genai.GenerativeModel(model_name)
        self.file_manager = FileManager(run_timestamp)
```

#### The 4-Step Pipeline

**Step 1: Initial Draft**
```python
def create_initial_draft(self, section, relevant_memory):
    # Temperature: 0.6 (balanced creativity)
    # Max tokens: 100,000
    # Target: ~1000 words
    # Includes: All data, tables, footnotes
    # Context: Full documents + section specs + learning insights
```

**Step 2: Completeness Check**
```python
def completeness_check(self, section, draft):
    # Temperature: 0.2 (high precision)
    # Validates against source documents
    # Returns: ADD list (max 5 missing items)
    # Format: Bullet list of critical gaps
```

**Step 3: Enhanced Draft**
```python
def generate_enhanced_draft(self, section, initial_draft, add_list):
    # Temperature: 0.6
    # Max tokens: 50,000
    # Incorporates missing elements from Step 2
    # Often exceeds 1000 words
```

**Step 4: Deep Analysis & Polish**
```python
def deep_analysis_and_polish(self, section, enhanced_draft):
    # Temperature: 0.6
    # Max tokens: 10,000
    # Target: ~500 words essential insights
    # Filter: "Would this change an investor's view?"
    # Must include: At least one data table
    # Quality focus: Remove corporate fluff, maximize insight density
```

#### Special Handling

**Section 33 (Data Book)**:
- Skips Steps 2-4 (completeness, enhancement, polish)
- Pure extraction mode with no word limits
- Preserves all data tables and numerical content
- Single-pass processing

**Empty Output Detection**:
- Check for suspiciously short outputs (< 50 words)
- Retry once with adjusted parameters
- Fall back to previous step on repeated failure

### 3. src/file_manager.py - File Operations

**Purpose**: Handles all file I/O, markdown preprocessing, and table cleaning.

#### FileManager Class

```python
class FileManager:
    def __init__(self, run_timestamp):
        self.run_dir = f"runs/run_{run_timestamp}"
        self.ensure_directories_exist()
```

**Key Methods**:

1. **`load_markdown_files(file_dict) -> str`**
   - Loads multiple markdown files
   - Applies table cleaning pipeline
   - Combines into single context string
   - Returns: Combined document text

2. **`save_step_output(section_num, filename, content)`**
   - Saves section work products
   - Creates section directory if needed
   - Path: `runs/run_*/section_N/filename`

3. **`save_to_json(data, filepath)`**
   - JSON serialization with pretty printing
   - 2-space indentation
   - UTF-8 encoding

4. **`load_from_json(filepath)`**
   - Safe JSON loading with error handling
   - Returns empty dict on failure

5. **`ensure_memory_file_exists()`**
   - Creates `memory/learning_memory.json` if missing
   - Initializes with empty structure

#### Table Cleaning Pipeline

Uses utilities from `utils.py`:
- `clean_markdown_tables()`: Fix Marker library corruption
- `validate_and_fix_tables()`: Enforce size constraints
- Removes excessive separator lines
- Fixes column alignment
- Ensures proper spacing

### 4. src/insight_memory.py - Learning System

**Purpose**: Captures and reuses analytical patterns across runs for continuous improvement.

#### InsightMemory Class

```python
class InsightMemory:
    def __init__(self, file_manager):
        self.memory = {}  # {section_num: [insights]}
        self.file_manager = file_manager
        self.memory_file = "memory/learning_memory.json"
        self.load_memory()
```

**Data Structure**:
```python
{
  "section_number": [
    {
      "instruction": "Specific analytical instruction",
      "quality_score": 9.5,
      "source_run": "run_2025_10_15_14_30_00"
    },
    ...  # Max 6 insights per section
  ]
}
```

**Key Methods**:

1. **`add_learning(section_num, learning_data)`**
   - Validates quality score (must be 9-10)
   - Enforces max 30 words per insight
   - Deduplicates similar insights (fuzzy matching)
   - Limits to 6 insights per section (keeps highest quality)
   - Returns: True if added, False if rejected

2. **`get_relevant_memory(section_num) -> List[str]`**
   - Retrieves learning insights for specific section
   - Formats as numbered instruction list
   - Used as context in Step 1 (Initial Draft)

3. **`save_memory()`**
   - Atomic write with temporary file
   - Creates timestamped snapshot
   - Preserves history in `memory/snapshots/`

4. **`calculate_statistics() -> dict`**
   - Total insights per section
   - Average quality scores
   - Coverage metrics (sections with learning)
   - Used for system monitoring

**Quality Filtering Strategy**:
1. LLM assigns quality score 1-10 during learning extraction
2. Only scores 9-10 pass first filter
3. Deduplication removes similar insights
4. Top 6 insights retained per section (by score)
5. Result: High signal-to-noise ratio

### 5. src/profile_generator.py - HTML/PDF Generation

**Purpose**: Converts markdown sections to professional PDF reports via HTML intermediate format.

#### ProfileGenerator Class

```python
class ProfileGenerator:
    def __init__(self, run_timestamp, model_name):
        self.run_timestamp = run_timestamp
        self.model = genai.GenerativeModel(model_name, temperature=0.6)
```

**Generation Pipeline**:
```
Markdown sections → Combine → Add cover page → CSS styling → HTML → WeasyPrint → PDF
```

**Key Methods**:

1. **`generate_html_profile(results, section_numbers, full_context)`**
   - Extracts company name via LLM
   - Collects section markdown from run directory
   - Generates combined HTML with cover page
   - Triggers PDF generation
   - Returns: HTML file path

2. **`_collect_section_markdown() -> (str, List[tuple])`**
   - Scans run directory for section files
   - Prioritizes final over draft
   - Applies markdown cleaning pipeline
   - Returns: (combined_markdown, processed_sections list)

3. **`_clean_markdown_content(content) -> str`**
   - Removes markdown/HTML code block wrappers
   - Strips duplicate section titles
   - Ensures blank lines before tables and lists
   - Preserves Section 33 code blocks

4. **`_manage_footnotes(markdown) -> str`**
   - Scopes footnote labels per section
   - Transforms `[^1]` → `[^s1_1]` in section 1
   - Prevents cross-section footnote conflicts

5. **`_generate_html_from_markdown(markdown, sections, company, clean_name)`**
   - Generates cover page with table of contents
   - Converts markdown to HTML (python-markdown library)
   - Applies CSS styling with footer date
   - Creates complete HTML document
   - Saves to `ReportFiles/[Company]_YYMMDD_HHMM.html`
   - Calls PDF generator
   - Returns: HTML file path

6. **`_generate_cover_page(sections, company_name) -> str`**
   - Company name (large, centered)
   - Product name: "ProfileDash 2.2"
   - Generation info: Model and date
   - Table of contents grouped by category:
     - Company Profile (1-13)
     - Strategy and SWOT (14-19)
     - Sellside Positioning (20-26)
     - Buyside Due Diligence (27-32)
     - Data Book (33)
   - Clickable section links with anchor tags

7. **`_extract_company_name(full_context) -> str`**
   - Uses LLM to extract primary company name
   - Searches: Document titles, headers, letterheads
   - Fallback: "Company Profile" if unclear
   - Validation: Length 2-100 characters

8. **`_markdown_to_html(markdown) -> str`**
   - python-markdown library
   - Extensions: tables, fenced_code, extra, footnotes
   - Converts markdown to clean HTML

9. **`_get_css_styles(footer_date) -> str`**
   - Professional styling with Georgia serif font
   - A4 page sizing with explicit margins
   - Pixel-based font sizes (12px base for consistent PDF rendering)
   - Page footer CSS:
     - `@bottom-left`: "Generated by ProfileDash 2.2 on [date]"
     - `@bottom-right`: "Page X of Y"
     - 7px font, blue color (#2d5a87)
   - Table styling with alternating row colors
   - Print-friendly media queries

### 6. src/pdf_generator.py - PDF Output

**Purpose**: Wrapper for WeasyPrint HTML-to-PDF conversion.

```python
def generate_pdf_from_html(html_path: str) -> Optional[str]:
    # Input: HTML file path
    # Output: PDF file path (same name, .pdf extension)
    # Uses: WeasyPrint library
    # Returns: PDF path or None on failure
```

**Features**:
- CSS @page rules for pagination
- @bottom-left and @bottom-right margin boxes for footers
- counter(page) and counter(pages) for page numbering
- WeasyPrint v66+ required for proper margin box support

### 7. src/profile_sections.py - Section Definitions

**Purpose**: Declarative specification of all 33 analysis sections.

**Data Structure**:
```python
sections = [
    {
        "number": int,
        "title": str,
        "specs": str  # Detailed analysis instructions
    },
    ...
]
```

#### Section Groups

**Group 1: Company Profile (Sections 1-13)**
1. Operating Footprint - Geographic and operational scope
2. Products and Services - Revenue streams and offerings
3. Key Customers - Customer concentration and relationships
4. Key Suppliers - Supply chain dependencies
5. Key Competitors - Competitive landscape
6. Operational KPIs - Performance metrics and benchmarks
7. Summary Financials (Consolidated) - Historical P&L
8. Summary Financials (Segment) - Business unit performance
9. Balance Sheet - Most recent financial position
10. Top 10 Shareholders - Ownership structure
11. M&A Agenda - Corporate transactions and strategy
12. Key Decision Makers - Management team
13. Deep Dive Discoveries - Critical insights not captured elsewhere

**Group 2: Strategy and SWOT (Sections 14-19)**
14. Strategic Objectives - Corporate strategies and goals
15. Strategic Constraints - Limitations and headwinds
16. Strengths - Competitive advantages
17. Weaknesses - Vulnerabilities and gaps
18. Opportunities - Growth vectors
19. Threats - External risks

**Group 3: Sellside Positioning (Sections 20-26)**
20. Macro - Economic and market environment
21. Industry - Sector dynamics and trends
22. Competitive Positioning - Market share and differentiation
23. Operating Performance - Operational efficiency analysis
24. Financial Performance - Profitability and returns
25. Management - Leadership quality and track record
26. Investor Concerns - Potential red flags and mitigants

**Group 4: Buyside Due Diligence (Sections 27-32)**
27. Macro - Deep dive on economic factors
28. Industry - Comprehensive sector analysis
29. Competitive Positioning - Sustainable advantages
30. Operating Performance - Operational deep dive
31. Financial Performance - Financial health assessment
32. Management - Management quality and governance

**Group 5: Data Book (Section 33)**
33. Appendix: Data Book - Complete data extraction
   - No word limits
   - All tables and numbers
   - Pure extraction mode

### 8. src/quality_tracker.py - Metrics Tracking

**Purpose**: Tracks quality metrics and statistics for analysis runs.

```python
class QualityTracker:
    def calculate_section_metrics(self, section_content) -> dict:
        # Metrics:
        # - Word count
        # - Numeric density (numbers per 100 words)
        # - Table count and total rows
        # - Insight depth ratio

    def get_quality_scores(self) -> dict:
        # Returns aggregate quality metrics
```

**Usage**: Monitoring and debugging analysis quality over time.

### 9. src/utils.py - Utility Functions

**Purpose**: Shared utilities for thread safety, error handling, and data cleaning.

#### Key Functions

**1. Thread-Safe Output**
```python
def thread_safe_print(*args, **kwargs):
    with print_lock:
        print(*args, **kwargs)
        sys.stdout.flush()
```
- Required in all parallel processing contexts
- Prevents garbled console output
- Global lock ensures serialized printing

**2. Markdown Table Cleaning**
```python
def clean_markdown_tables(content: str) -> str:
    # Fixes Marker library corruption:
    # - Remove excessive separator lines (|||||||)
    # - Fix misaligned columns
    # - Ensure proper spacing around tables
    # - Handle edge cases (empty cells, special chars)
```

**3. Table Validation and Size Enforcement**
```python
def validate_and_fix_tables(content: str) -> str:
    # Enforces constraints:
    # - Max 10 columns
    # - Max 20 rows
    # - Truncates with [...] markers
    # - Preserves headers
```

**4. API Retry Logic**
```python
def retry_with_backoff(func, max_retries=3, base_delay=1.0):
    # Exponential backoff: 1s, 2s, 4s
    # Jitter: Random ±25% to prevent thundering herd
    # Honors Retry-After headers from API
    # Handles: 429 rate limits, transient failures
    # Returns: Function result or raises exception
```

## Data Flow & Processing Pipeline

### Input Processing Flow
```
1. PDF Selection
   ├─> SHA-256 hash check
   ├─> Cache hit? → Load from SourceFiles/
   └─> Cache miss:
       ├─> ProcessPoolExecutor (2-5 workers)
       ├─> Marker PDF→Markdown conversion
       ├─> Table cleaning pipeline
       └─> Save to SourceFiles/ (SHA-256 filename)

2. Markdown Combination
   ├─> Load all markdown files
   ├─> Apply table cleaning
   └─> Create full_context (all documents combined)
```

### Section Analysis Flow (Parallel)
```
For each section (ThreadPoolExecutor, 1-8 workers):
    Step 1: Initial Draft (~1000 words)
         │  • Display: "Sec. N → Draft"
         ↓
    Step 2: Completeness Check
         │  • Identify missing elements
         ↓
    Step 3: Enhanced Draft
         │  • Display: "Sec. N → Refine"
         │  • Incorporate missing elements
         ↓
    Step 4: Deep Analysis & Polish (~500 words)
         │  • Display: "Sec. N → Polish"
         │  • Apply investor relevance filter
         ↓
    Save to: runs/run_*/section_N/
         │
         ↓
    Display: "Sec. N ✓ Complete (X/total)"
```

### Output Generation Flow
```
1. Collect Section Markdown
   ├─> Scan runs/run_*/ directories
   ├─> Prioritize: final > draft
   └─> Apply markdown cleaning

2. Generate HTML
   ├─> Extract company name (LLM)
   ├─> Create cover page with TOC
   ├─> Convert markdown to HTML (python-markdown)
   ├─> Apply CSS styling
   └─> Save to ReportFiles/[Company]_YYMMDD_HHMM.html

3. Generate PDF
   ├─> WeasyPrint HTML→PDF conversion
   ├─> Apply @page rules (A4, margins)
   ├─> Render footers with page numbers
   └─> Save to ReportFiles/[Company]_YYMMDD_HHMM.pdf
```

## Concurrency Model & Resource Management

### Thread Pools (Section Analysis)
- **Purpose**: Parallel section processing (I/O-bound)
- **Implementation**: ThreadPoolExecutor
- **Worker count**: 1-8 (user configurable, default 2)
- **Limiting**: Global semaphore (4 concurrent API calls)
- **Scheduling**: 0.5s stagger between task submissions

### Process Pools (PDF Conversion)
- **Purpose**: Parallel PDF→Markdown conversion (CPU-bound)
- **Implementation**: ProcessPoolExecutor
- **Worker count**: 2-5 (adapts to LLM mode)
  - LLM mode: 2 workers (API-bound)
  - Basic mode: 3-5 workers (CPU-bound)
- **Isolation**: Prevents PyTorch tensor memory conflicts
- **BLAS pinning**: 1 thread per process

### Real-Time Progress Display
- **WorkerDisplay class**: Thread-safe status tracking
- **Status states**: "Draft" → "Refine" → "Polish"
- **Display format**:
  ```
  Sec. 5 → Draft       | Sec. 12 → Refine     | Sec. 23 → Polish
  ```
- **Completion messages**:
  ```
  Sec. 5 ✓ Complete (1/32)
  ```

## Error Handling & Recovery Strategies

### Retry Strategy
```python
Attempt 1: Immediate execution
Attempt 2: Wait 1s + jitter (±250ms)
Attempt 3: Wait 2s + jitter (±500ms)
Attempt 4: Wait 4s + jitter (±1s)
Give up: Raise exception
```

### Fallback Mechanisms
1. **Empty output** → Use previous pipeline step
2. **API timeout** → Retry with exponential backoff
3. **PDF conversion failure** → Skip file, continue others
4. **Section analysis failure** → Mark failed, continue others
5. **Company name extraction failure** → Use "Company Profile"

### Error Contexts
- **API Rate Limit (429)**: Honor Retry-After, exponential backoff
- **API Quota Exceeded**: Reduce workers, increase stagger
- **Marker Crash**: Process isolation prevents system crash
- **Table Corruption**: Automatic cleaning pipeline

## Performance Optimizations

### Caching Strategy
- **PDF Cache**: SHA-256 hash → markdown in `SourceFiles/`
- **Marker Artifacts**: Reused within same session
- **Learning Memory**: Accumulated across all runs
- **Benefits**: Eliminates redundant PDF conversions

### Parallel Processing
- **PDF Conversion**: 2-5 parallel processes
- **Section Analysis**: 1-8 parallel threads
- **Two-Phase Scheduling**: Sections 1-32 first, then 33
- **Benefits**: ~3-5x speedup with default settings

### Resource Management
- **Output Size Limits**: 10KB-500KB per section
- **Table Constraints**: Max 10 columns × 20 rows
- **Memory Snapshots**: Learning memory capped at 6 insights/section
- **Thread-Safe Operations**: All parallel contexts use synchronized I/O

## Configuration & Environment

### Environment Variables
```bash
GEMINI_API_KEY=your_key_here        # Required, obtain from Google AI Studio
```

### Key Dependencies
```
marker-pdf              # PDF to Markdown conversion
python-dotenv           # Environment management
google-generativeai     # Gemini API client
markdown                # Markdown to HTML conversion
weasyprint              # HTML to PDF conversion (requires v66+)
requests                # HTTP client
pdfminer.six            # PDF parsing utilities
```

### System Requirements
- Python 3.8+
- 8GB RAM minimum (16GB recommended for parallel processing)
- Gemini API key (free tier available)
- Internet connection (API calls)

## Output Artifacts

### Final Deliverable
```
ReportFiles/
└── [Company]_YYMMDD_HHMM.pdf
    • Professional PDF with A4 sizing
    • Georgia serif font, 12px base
    • Page footers: generation date (left), page numbers (right)
    • Cover page with clickable table of contents
    • 33 analytical sections (or selected subset)
    • Tables, charts (as available from source)
```

### Work Products
```
runs/run_YYYY_MM_DD_HH_MM_SS/
├── section_1/
│   ├── step_1_initial_draft.md
│   ├── step_2_completeness_check.txt
│   ├── step_3_enhanced_draft.md
│   └── step_4_final_section.md
├── section_2/
│   └── ...
├── [Company]_profile.md
└── run_summary.txt
```

## Key Algorithms & Techniques

### 1. Progressive Refinement Algorithm
```
Input: Document context, section specification
Output: Polished section content (~500 words)

1. EXPAND: Generate comprehensive draft (1000 words)
2. VALIDATE: Check completeness against source
3. ENHANCE: Incorporate missing elements
4. CONDENSE: Apply relevance filter, distill to 500 words

Quality gate at each step: minimum content requirements
```

### 3. Table Cleaning Algorithm
```
Input: Corrupted markdown table from Marker
Output: Valid markdown table

1. Identify table blocks (lines starting with |)
2. Remove excessive separator lines (|||||||)
3. Count columns in header row
4. Ensure all rows have same column count
5. Add missing | delimiters
6. Enforce size constraints (10 cols × 20 rows)
7. Truncate with [...] if oversized
8. Preserve header row
9. Ensure blank lines before/after table
```

### 4. Footnote Scoping Algorithm
```
Input: Combined markdown with multiple sections
Output: Scoped footnotes (no conflicts)

1. Split content at section anchors
2. For each section:
   a. Extract section number (N)
   b. Transform [^1] → [^sN_1] in references
   c. Transform [^1]: → [^sN_1]: in definitions
3. Reassemble content

Prevents: Cross-section footnote collisions
```

## Testing & Quality Assurance

### Testing Approach
- **No Formal Test Suite**: Manual validation only
- **Test Documents**: Sample PDFs from different industries
- **Validation Points**:
  - Section completeness (all selected sections present)
  - PDF formatting (footers, page numbers, styling)
  - Table rendering (no overflow, proper borders)
  - Content quality (passes investor relevance filter)
  - Error recovery (graceful handling of failures)

### Monitoring Points
- `runs/run_*/run_summary.txt`: Processing log and errors
- `memory/learning_memory.json`: Learning accumulation over time
- Console output: Real-time progress and errors
- PDF output: Visual inspection of formatting

### Quality Metrics
- **Insight Density**: Insights per word (target: maximize)
- **Numeric Density**: Numbers per 100 words (target: high)
- **Table Presence**: At least 1 table per section (where applicable)
- **Learning Quality**: Average score of retained insights (target: 9.5+)

## Known Issues & Limitations

### Technical Constraints
1. **Marker Table Corruption**: PDF tables often corrupted during conversion
   - Mitigation: Aggressive cleaning pipeline
   - Limitation: Some tables unrecoverable

2. **Process Isolation Required**: PyTorch tensor memory conflicts
   - Mitigation: ProcessPoolExecutor for PDF conversion
   - Limitation: Higher overhead, limited workers

3. **Learning Memory Growth**: Unbounded accumulation over time
   - Mitigation: 6 insights/section cap
   - Limitation: No automatic pruning of outdated insights

4. **No Formal Testing**: Manual validation only
   - Mitigation: Careful code review, defensive programming
   - Limitation: Regressions possible

### Design Trade-offs
1. **Quality vs Speed**: Emphasis on quality → longer processing times
2. **Completeness vs Conciseness**: Multiple refinement passes → higher token usage
3. **Learning vs Overfitting**: Aggressive quality filter → slower learning accumulation
4. **Parallel vs Sequential**: Parallelism → harder debugging

## Future Enhancement Opportunities

### Potential Improvements
1. **Streaming Output**: Progressive PDF delivery as sections complete
2. **Incremental Analysis**: Resume interrupted runs from checkpoints
3. **Distributed Processing**: Multi-machine orchestration
4. **Custom Sections**: User-defined analysis templates
5. **Alternative LLMs**: OpenAI GPT-4, Anthropic Claude support
6. **Real-time Monitoring**: Web dashboard for progress tracking
7. **Automated Testing**: Regression suite with golden outputs
8. **Learning Curation**: UI for reviewing and pruning insights
9. **Version Control**: Track section definition changes over time
10. **Multi-document Context**: Preserve document boundaries in context

### Architectural Enhancements
1. **Plugin System**: Extensible section analyzers
2. **Event Sourcing**: Audit trail of all analysis decisions
3. **Configurable Pipelines**: User-selectable refinement steps
4. **Output Formats**: Word, PowerPoint, Excel support
5. **Collaborative Editing**: Multi-user review and annotation

## Conclusion

ProfileDash 2.2 represents a sophisticated document analysis pipeline with careful attention to:
- **Parallel processing** for performance
- **Progressive refinement** for quality
- **Learning accumulation** for continuous improvement
- **Error handling** for reliability
- **Professional output** for presentation

The modular architecture enables independent enhancement of components while maintaining system stability. The learning system provides continuous improvement potential, though it requires periodic curation to prevent drift.

**Key Success Factors**:
- Clear separation of concerns across modules
- Thread-safe operations throughout
- Quality-first approach with multiple filters
- Graceful degradation on failures
- Professional PDF output with proper typography

**Recommended Next Steps for New Developers**:
1. Read `CLAUDE.md` for AI assistant guidance
2. Review `profile_sections.py` to understand analysis scope
3. Trace execution flow starting from `PD2.py` main block
4. Experiment with small section subsets (e.g., sections 1-3 only)
5. Monitor `runs/` directory to understand pipeline outputs
6. Review learning memory to see accumulated patterns

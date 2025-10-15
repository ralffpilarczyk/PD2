# PD2 Code Overview - Complete Technical Documentation

## Executive Summary

ProfileDash 2.0 (PD2) is a sophisticated financial document analysis system that transforms PDF documents into comprehensive 33-section company profiles using Google's Gemini LLM API. The system employs a multi-stage refinement pipeline with parallel processing, intelligent caching, and a learning system that captures analytical patterns for continuous improvement.

**Total Codebase**: 3,582 lines of Python code across 9 files

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
│   • Model selection                                  │
│   • File selection & conversion                      │
│   • Section group selection                          │
│   • Worker pool management                           │
└────────────────────┬───────────────────────────────┘
                     │
        ┌────────────┴────────────┬──────────────┐
        ▼                         ▼              ▼
┌──────────────┐      ┌──────────────┐   ┌──────────────┐
│FileManager   │      │CoreAnalyzer  │   │ProfileGen    │
│(file_manager)│      │(core_analyzer)│  │(profile_gen) │
│              │      │              │   │              │
│•Load files   │      │•6-step       │   │•HTML output  │
│•PDF→Markdown │      │ refinement   │   │•MD→HTML      │
│•Caching      │      │•LLM calls    │   │•Formatting   │
└──────────────┘      └──────┬───────┘   └──────────────┘
                             │
                    ┌────────┴────────┐
                    ▼                 ▼
            ┌──────────────┐  ┌──────────────┐
            │InsightMemory │  │QualityTracker│
            │(insight_mem) │  │(quality_track)│
            │              │  │              │
            │•Learning     │  │•Metrics      │
            │•Patterns     │  │•Scoring      │
            └──────────────┘  └──────────────┘
```

## File Structure

```
PD2/
├── PD2.py                      # Main entry point (1026 lines)
├── src/
│   ├── __init__.py            # Package initialization (26 lines)
│   ├── core_analyzer.py       # Analysis pipeline (438 lines)
│   ├── file_manager.py        # File I/O operations (124 lines)
│   ├── insight_memory.py      # Learning system (350 lines)
│   ├── profile_generator.py   # HTML generation (588 lines)
│   ├── profile_sections.py    # Section definitions (711 lines)
│   ├── quality_tracker.py     # Quality metrics (54 lines)
│   └── utils.py               # Utility functions (265 lines)
```

## Detailed File Documentation

### 1. PD2.py - Main Orchestrator (1026 lines)

**Purpose**: Entry point and orchestration layer that coordinates all modules

#### Key Components:

##### Module-Level Functions:
```python
def _pdf_conversion_worker(pdf_path: str, use_llm: bool = False) -> Optional[str]
```
- Runs in separate process (ProcessPoolExecutor)
- Uses Marker library for PDF→Markdown conversion
- LLM enhancement parameter exists but unused (no benefit found)
- Sets BLAS threads to 1 to prevent resource contention
- Returns markdown text or None on failure

##### Main Class: IntelligentAnalyst
```python
class IntelligentAnalyst:
    def __init__(self, source_files: dict, model_name: str = 'gemini-2.5-flash', use_llm_pdf_conversion: bool = True)
```

**Core Methods**:

1. **`__init__`**: Initializes all components
   - Creates run timestamp
   - Initializes FileManager
   - Converts PDFs in parallel (ProcessPoolExecutor, 2-5 workers)
   - Loads markdown files
   - Creates CoreAnalyzer, InsightMemory, QualityTracker

2. **`_convert_pdfs_parallel`**: Manages parallel PDF conversion
   - SHA-256 based caching
   - Progress tracking
   - Worker pool management based on LLM mode

3. **`_convert_single_pdf`**: Single PDF conversion (for serial mode)
   - Used when Marker artifacts need to be reused
   - Calls Marker directly without process isolation

4. **`run_analysis`**: Main analysis orchestration
   - Manages section analysis with ThreadPoolExecutor
   - Two-phase scheduling (Sections 1-32, then 33)
   - Progress tracking and error handling
   - Calls profile generator for HTML output

5. **`_analyze_single_section`**: Wrapper for section analysis
   - Calls CoreAnalyzer for the actual work
   - Handles exceptions and logging
   - Thread-safe progress updates

##### UI Helper Functions:
```python
def _read_single_key() -> str              # Cross-platform single key input
def prompt_yes_no(prompt: str) -> bool     # Y/N prompts
def prompt_single_digit(...) -> str        # Digit selection prompts
def select_source_files() -> tuple         # File selection UI
def select_pdf_files() -> list             # PDF selection dialog
```

##### Main Execution Flow:
```python
if __name__ == "__main__":
    # 1. Select Gemini model
    # 2. Select source files (PDFs or existing markdown)
    # 3. Initialize IntelligentAnalyst
    # 4. Select section groups (1-5 or custom)
    # 5. Enable/disable discovery pipeline
    # 6. Set worker count (1-8)
    # 7. Run analysis
    # 8. Save outputs
```

### 2. src/core_analyzer.py - Analysis Pipeline (438 lines)

**Purpose**: Implements the 6-step progressive refinement pipeline for each section

#### Key Class: CoreAnalyzer

**Initialization**:
```python
def __init__(self, full_context: str, run_timestamp: str, model_name: str = 'gemini-2.5-flash')
```
- Stores full document context
- Configures Gemini model
- Creates output directories

**Core Pipeline Method**:
```python
def analyze_section(self, section_number: int, enable_discovery: bool = False) -> dict
```

**The 6-Step Pipeline**:

1. **Step 1: Initial Draft** (`_generate_initial_draft`)
   - Temperature: 0.6
   - Max tokens: 100,000
   - Target: ~1000 words comprehensive analysis
   - Includes tables, footnotes, all data

2. **Step 2: Completeness Check** (`_check_completeness`)
   - Temperature: 0.2
   - Validates against source documents
   - Returns ADD list (max 5 items)
   - Identifies missing critical information

3. **Step 3: Enhanced Draft** (`_generate_enhanced_draft`)
   - Temperature: 0.6
   - Max tokens: 50,000
   - Incorporates missing elements from Step 2
   - Often exceeds 1000 words

4. **Step 4: Deep Analysis & Polish** (`_deep_analysis_and_polish`)
   - Temperature: 0.6
   - Max tokens: 10,000
   - Condenses to ~500 words of essential insights
   - Must include at least one data table
   - Filter: "Would this change an investor's view?"

5. **Step 5: Discovery Pipeline** (`_run_discovery_pipeline`) [Optional]
   - 6 sub-stages for deep pattern finding:
     - Extract quantifiable data
     - Calculate relationships
     - Identify anomalies
     - Investigate root causes
     - Assess business impact
     - Generate comprehensive insight

6. **Step 6: Learning Extraction** (`_extract_learning`)
   - Temperature: 0.2
   - Captures reusable methodologies
   - JSON format with techniques, patterns
   - Quality threshold: 9-10 only

**Special Handling**:
- Section 33 (Appendix: Data Book) skips steps 2-5
- Empty output detection and retry logic
- Exponential backoff for API failures
- All outputs saved to `runs/run_*/section_N/`

### 3. src/file_manager.py - File Operations (124 lines)

**Purpose**: Handles all file I/O operations and markdown preprocessing

#### Key Class: FileManager

**Methods**:

1. **`load_markdown_files`**: Load and combine markdown files
   - Cleans corrupted tables
   - Combines multiple files
   - Returns single context string

2. **`save_to_json`**: JSON serialization with proper formatting

3. **`load_from_json`**: Safe JSON loading with error handling

4. **`ensure_memory_file_exists`**: Creates memory directory/file if needed

5. **`save_memory_state`**: Saves learning memory snapshots

6. **`clean_source_files`**: Removes temporary/test markdown files

**Table Cleaning Pipeline**:
- Calls `clean_markdown_tables` from utils
- Fixes Marker library corruption issues
- Ensures valid markdown table syntax

### 4. src/insight_memory.py - Learning System (350 lines)

**Purpose**: Captures and reuses analytical patterns across runs

#### Key Class: InsightMemory

**Core Concepts**:
- **Learning Memory**: Accumulated insights organized by section
- **Quality Scoring**: 1-10 scale, only 9+ saved
- **Max 6 Insights Per Section**: Prevents memory bloat
- **Instruction Format**: Insights formatted as actionable instructions

**Key Methods**:

1. **`load_memory`**: Load existing patterns from JSON
   - File: `memory/learning_memory.json`
   - Structure: `{section_number: [insights]}`

2. **`add_learning`**: Add new insights from analysis
   - Validates quality score
   - Enforces word limits
   - Deduplicates similar insights

3. **`get_relevant_learnings`**: Retrieve patterns for section
   - Returns list of high-quality insights
   - Used as context for new analyses

4. **`save_memory`**: Persist to disk
   - Atomic write with temporary file
   - Preserves history in snapshots

5. **`calculate_memory_statistics`**: Analytics on learning system
   - Total insights per section
   - Average quality scores
   - Coverage metrics

**Learning Format**:
```json
{
  "instruction": "Specific analytical instruction",
  "quality_score": 9.5,
  "source_run": "run_2024_01_15_10_30_45"
}
```

### 5. src/profile_generator.py - HTML Generation (588 lines)

**Purpose**: Converts markdown sections to professional HTML output

#### Key Class: ProfileGenerator

**Core Functionality**:

1. **`generate_profile`**: Main generation method
   - Combines all sections into single HTML
   - Two-phase for Section 33 (if present)
   - Professional styling with Georgia font
   - Responsive design

2. **`_generate_html`**: HTML document creation
   - Custom CSS styling
   - Table formatting
   - Navigation structure
   - Print-friendly layout

3. **`_convert_section_to_html`**: Markdown→HTML conversion
   - Uses python-markdown library
   - Preserves tables with special handling
   - Footnote processing
   - Code block formatting for Section 33

4. **`_post_process_html`**: HTML cleanup
   - BeautifulSoup parsing
   - Table class additions
   - Link processing
   - Whitespace normalization

**Special Features**:
- Section 33 code block preservation
- Table hover effects
- Automatic TOC generation
- Mobile-responsive tables
- Clean print layout

### 6. src/profile_sections.py - Section Definitions (711 lines)

**Purpose**: Defines all 33 analysis sections with detailed specifications

#### Data Structure:
```python
sections = [
    {
        "number": 1-33,
        "title": "Section Name",
        "specs": "Detailed analysis instructions..."
    },
    ...
]
```

#### Section Groups:

**Group 1: Company Profile (1-13)**
1. Operating Footprint
2. Products and Services
3. Key Customers
4. Key Suppliers
5. Key Competitors
6. Operational KPIs
7. Summary Financials (Consolidated)
8. Summary Financials (Segment)
9. Balance Sheet (Most Recent)
10. Top 10 Shareholders
11. M&A Agenda and Material Corporate Activity
12. Key Decision Makers
13. Deep Dive Discoveries

**Group 2: Strategy and SWOT (14-19)**
14. Strategic Objectives and Corporate Strategies
15. Strategic Constraints
16. Strengths
17. Weaknesses
18. Opportunities
19. Threats

**Group 3: Sellside Positioning (20-26)**
20. Sellside Positioning - Macro
21. Sellside Positioning - Industry
22. Sellside Positioning - Competitive Positioning
23. Sellside Positioning - Operating Performance
24. Sellside Positioning - Financial Performance
25. Sellside Positioning - Management
26. Sellside Positioning - Potential Investor Concerns and Mitigants

**Group 4: Buyside Due Diligence (27-32)**
27. Buyside Due Diligence - Macro
28. Buyside Due Diligence - Industry
29. Buyside Due Diligence - Competitive Positioning
30. Buyside Due Diligence - Operating Performance
31. Buyside Due Diligence - Financial Performance
32. Buyside Due Diligence - Management

**Special Section:**
33. **Appendix: Data Book** (Special handling - extraction only, no word limits)

### 7. src/quality_tracker.py - Metrics Tracking (54 lines)

**Purpose**: Tracks quality metrics and statistics for analysis runs

#### Key Class: QualityTracker

**Methods**:

1. **`calculate_section_metrics`**: Analyze section output quality
2. **`get_quality_scores`**: Return current metrics

**Metrics Tracked**:
- Insight count and depth ratio
- Output length in words
- Numeric density per 100 words
- Table presence and row count

### 8. src/utils.py - Utility Functions (265 lines)

**Purpose**: Shared utilities for thread safety, table cleaning, and API handling

#### Key Functions:

1. **`thread_safe_print`**: Synchronized console output
   ```python
   def thread_safe_print(*args, **kwargs):
       with print_lock:
           print(*args, **kwargs)
           sys.stdout.flush()
   ```

2. **`clean_markdown_tables`**: Fix Marker corruption
   - Remove excessive separator lines
   - Fix column alignment
   - Ensure proper spacing
   - Handle edge cases

3. **`validate_and_fix_tables`**: Size enforcement
   - Max 10 columns
   - Max 20 rows
   - Truncate with [...] markers
   - Preserve headers

4. **`retry_with_backoff`**: API retry logic
   ```python
   def retry_with_backoff(func, max_retries=3, base_delay=1.0):
       # Exponential backoff with jitter
       # Handles 429 rate limits
       # Parses Retry-After headers
   ```

## Data Flow

### 1. Input Processing
```
PDFs → Marker (ProcessPool) → Markdown → Table Cleanup → Combined Context
                    ↓
            SHA-256 Cache Check
                    ↓
             SourceFiles/
```

### 2. Section Analysis (Parallel)
```
For each section (ThreadPoolExecutor):
    Step 1: Initial Draft (1000 words)
         ↓
    Step 2: Completeness Check
         ↓
    Step 3: Enhanced Draft
         ↓
    Step 4: Polish (500 words)
         ↓
    [Optional] Step 5: Discovery Pipeline
         ↓
    Step 6: Learning Extraction
         ↓
    Save to runs/run_*/section_N/
```

### 3. Output Generation
```
Phase 1: Sections 1-32 → ProfileGenerator → HTML → Immediate delivery
Phase 2: Section 33 → Append to HTML → Final output
```

## Configuration & Environment

### Environment Variables
```bash
GEMINI_API_KEY=your_key_here        # Required
LLM_MAX_INFLIGHT=4                  # Concurrent LLM calls
MAX_SECTION_WORKERS=3                # Parallel sections
SUBMISSION_STAGGER_SEC=0.5          # Task submission delay
MARKER_PROCESS_WORKERS=2             # PDF conversion workers
```

### Key Dependencies
- `google-generativeai`: Gemini API client
- `marker-pdf`: PDF to Markdown conversion
- `python-markdown`: Markdown to HTML
- `beautifulsoup4`: HTML processing
- `python-dotenv`: Environment management

## Concurrency Model

### Thread Pools
- **Section Analysis**: ThreadPoolExecutor (3-8 workers)
- **Global Semaphore**: Limits concurrent LLM calls (default 4)
- **Submission Stagger**: 0.5s between task submissions

### Process Pools
- **PDF Conversion**: ProcessPoolExecutor (2-5 workers)
- **Process Isolation**: Prevents PyTorch/Marker conflicts
- **BLAS Pinning**: 1 thread per process

## Error Handling

### Retry Strategy
1. Exponential backoff (1s, 2s, 4s...)
2. Jitter to prevent thundering herd
3. Respect Retry-After headers
4. Max 3 retries per operation

### Fallback Mechanisms
1. Empty output → Use previous step
2. API timeout → Retry with backoff
3. PDF conversion fail → Skip file
4. Section fail → Continue others

## Performance Optimizations

### Caching
- SHA-256 based PDF cache
- Reuse Marker artifacts in same session
- Learning memory accumulation

### Parallel Processing
- Concurrent section analysis
- Parallel PDF conversion
- Two-phase scheduling for Section 33

### Resource Management
- Output size limits (10KB-500KB)
- Table size constraints
- Memory-mapped file operations
- Thread-safe operations

## Additional Considerations for Future Development

### Potential Enhancements
1. **Streaming Output**: Progressive HTML delivery
2. **Incremental Analysis**: Resume interrupted runs
3. **Distributed Processing**: Multi-machine support
4. **Custom Section Definitions**: User-defined analysis
5. **Alternative LLMs**: OpenAI, Anthropic support

### Current Limitations
1. **Single Document Context**: All files combined
2. **Memory Growth**: Learning system unbounded
3. **No Version Control**: Section definitions hardcoded
4. **Limited Error Recovery**: Manual intervention needed
5. **No Real-time Updates**: Batch processing only

### Testing Approach
- No formal test suite
- Manual testing with sample documents
- Run summaries in `runs/run_*/run_summary.txt`
- Quality metrics in `quality_metrics/`

## Conclusion

PD2 represents a sophisticated document analysis pipeline with careful attention to parallel processing, error handling, and progressive refinement. The modular architecture allows for independent enhancement of components while maintaining system stability. The learning system provides continuous improvement potential, though it requires periodic curation to prevent drift.
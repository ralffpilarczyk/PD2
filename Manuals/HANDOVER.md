# ProfileDash 2.0 (PD2) - Complete Project Handover Document

## Table of Contents
1. [Executive Summary](#executive-summary)
2. [System Architecture](#system-architecture)
3. [Core PD2 Analysis Pipeline](#core-pd2-analysis-pipeline)
4. [Competitive Intelligence Module](#competitive-intelligence-module)
5. [Technical Implementation Details](#technical-implementation-details)
6. [Configuration and Environment](#configuration-and-environment)
7. [Recent Development History](#recent-development-history)
8. [Current State and Known Issues](#current-state-and-known-issues)
9. [Operational Procedures](#operational-procedures)
10. [Critical Design Principles](#critical-design-principles)

## Executive Summary

ProfileDash 2.0 is an intelligent document analysis system that processes financial documents (PDFs/Markdown) and generates comprehensive 32-section company profiles using Google's Gemini API. The system includes an optional competitive intelligence module that performs web-based competitor analysis and market positioning.

### Key Capabilities
- **Document Processing**: Converts PDFs to Markdown, handles multiple documents in parallel
- **32-Section Analysis**: Comprehensive coverage from Operating Footprint to Capital Allocation
- **Multi-Step Refinement**: 4-6 step pipeline per section with progressive quality enhancement
- **Competitive Intelligence**: Web-based search for competitors, metrics collection, strategic analysis
- **Learning System**: Captures and reuses analytical patterns across runs
- **Professional Output**: HTML reports with tables, formatting, and data visualizations

### Critical Numbers
- **Typical Run Time**: 25-35 minutes for full profile, 90 minutes with competitive analysis
- **Section Pipeline**: 5 LLM calls standard, 13 with discovery pipeline
- **Parallel Processing**: 3 workers default, max 8
- **Rate Limiting**: 6 seconds between web searches, exponential backoff for API calls
- **Output Limits**: 100KB step 1, 50KB step 3, 10KB step 4, 500KB section 32

## System Architecture

### Directory Structure
```
PD2/
├── PD2.py                          # Main orchestrator & CLI interface
├── src/
│   ├── core_analyzer.py            # Multi-step analysis pipeline
│   ├── profile_generator.py        # HTML generation & markdown processing
│   ├── insight_memory.py           # Learning system for pattern capture
│   ├── file_manager.py             # File I/O & markdown preprocessing
│   ├── quality_tracker.py          # Section quality metrics
│   ├── profile_sections.py         # 32 section definitions
│   ├── utils.py                    # Utilities: rate limiting, thread safety, table fixes
│   └── competitive/                # Competitive intelligence module
│       ├── cli.py                  # Standalone CLI & PD2 integration hooks
│       ├── market_mapper.py        # Market cell discovery from documents
│       ├── peer_discovery.py       # Competitor discovery via web search
│       ├── metric_engine.py        # Metric collection & normalization
│       ├── report_generator.py     # HTML/JSON report generation
│       ├── database.py             # SQLite schema & operations
│       ├── entity_resolution.py    # Brand/subsidiary mapping
│       └── pivot_exporter.py       # CSV export for data tables
├── memory/
│   ├── learning_memory.json        # Accumulated analytical patterns
│   └── conversion_cache/           # SHA-256 keyed PDF→MD cache
├── runs/
│   └── run_YYYY_MM_DD_HH_MM_SS/   # Analysis outputs
│       ├── section_*/              # Per-section artifacts
│       ├── [Company]_profile.html  # Final PD2 output
│       └── competitive/            # Competitive analysis outputs
│           ├── competitive_analysis.db
│           ├── *_competitive_analysis.html
│           └── *_evidence_pack.json
└── Manuals/
    ├── HANDOVER.md                 # This document
    └── WEBSEARCH.md                # Competitive module detailed spec
```

### Data Flow Architecture

#### PD2 Core Flow
```
1. Input Processing:
   PDFs → Marker (process pool) → Markdown → Table Cleanup → Combined Context
   Markdown Files → Table Cleanup → Combined Context

2. Section Analysis (parallel):
   For each section (1-32):
     Step 1: Initial Draft (comprehensive, ~1000 words)
     Step 2: Completeness Check (identifies gaps)
     Step 3: Enhanced Draft (incorporates missing elements)
     Step 4: Deep Analysis & Polish (~500 words, essential insights only)
     Step 5: [Optional] Discovery Pipeline (6-stage pattern finding)
     Step 6: Learning Extraction (captures methodologies)

3. Output Generation:
   Phase 1: Sections 1-31 → HTML Profile → Immediate Delivery
   Phase 2: Section 32 (if selected) → Append to Profile → Final HTML
```

#### Competitive Module Flow
```
1. Market Discovery:
   Documents → Company Context Extraction → Market Cell Discovery
   Market Cell = Product/Service × Geography × Customer Segment

2. Competitor Discovery:
   Market Cell → Search Query Generation → Web Search → Competitor Extraction
   Includes subsidiary detection for holding companies

3. Metric Collection:
   Select 10 diverse metrics → Batch Search (multiple metrics per query) →
   Value Extraction → Currency/Period Normalization → Database Storage

4. Analysis & Reporting:
   Data Collection → Competitive Scoring → Strategy Bundling →
   HTML Report + JSON Evidence Pack
```

## Core PD2 Analysis Pipeline

### Section Pipeline Details

#### Step 1: Initial Draft
- **Purpose**: Comprehensive first-pass analysis
- **Temperature**: 0.6
- **Target Length**: ~1000 words
- **Output Limit**: 100KB
- **Includes**: Tables, insights, footnotes
- **Key Instruction**: "Find all relevant data and patterns"

#### Step 2: Completeness Check
- **Purpose**: Identify missing critical information
- **Temperature**: 0.2
- **Output**: ADD list (max 5 items)
- **Validation**: Compares draft against source documents
- **Key Question**: "What material information is missing?"

#### Step 3: Enhanced Draft
- **Purpose**: Incorporate missing elements
- **Temperature**: 0.6
- **Output Limit**: 50KB
- **Often exceeds**: 1000 words
- **Focus**: Completeness over conciseness

#### Step 4: Deep Analysis & Polish
- **Purpose**: Condense to essential insights
- **Temperature**: 0.6
- **Target Length**: ~500 words
- **Output Limit**: 10KB
- **Criteria**: "Would this change an investor's view?"
- **Requirement**: At least one data table

#### Step 5: Discovery Pipeline (Optional)
When enabled, adds 6 additional stages:
1. Extract all quantifiable data
2. Calculate material relationships
3. Identify top 2-3 anomalies
4. Investigate root causes
5. Assess business impact
6. Generate comprehensive insight

#### Step 6: Learning Extraction
- **Purpose**: Capture reusable methodologies
- **Temperature**: 0.2
- **Format**: JSON with techniques, patterns, validations
- **Quality Filter**: Score 9-10 only
- **Max Length**: 30 words per insight

### Special Handling: Section 32 (Appendix)
- **Type**: Pure data extraction (no analysis)
- **Process**: Single-step (skips 2-5)
- **Focus**: 15-20 most important tables
- **Output Limit**: 500KB (5x normal)
- **Scheduling**: Two-phase (runs after main profile delivery)

### Thread Safety and Parallelism
- **Section Workers**: ThreadPoolExecutor (default 3, max 8)
- **LLM Concurrency**: Global semaphore (default 4 in-flight)
- **PDF Conversion**: ProcessPoolExecutor (default 2, max 5)
- **BLAS Threads**: Pinned to 1 per process (prevents oversubscription)
- **Submission Stagger**: 0.5s between task submissions

## Competitive Intelligence Module

### Core Components

#### 1. Market Mapper (`market_mapper.py`)
**Purpose**: Extract company context and discover market cells

**Company Context Extraction**:
- Analyzes documents to extract structured company information
- Fields: name, ticker, industry, business model, geography, products/services
- Embeds original document text for segment inference
- No hardcoded company names or criteria

**Market Cell Discovery**:
- First attempts segment-based inference from documents
- Falls back to LLM-based discovery if needed
- Target: 3-8 market cells with >20% materiality
- Consolidation: Groups by dominant dimension if >8 cells
- Validation: "Would an executive think of these as separate businesses?"

#### 2. Peer Discovery (`peer_discovery.py`)
**Purpose**: Find competitors for each market cell

**Search Query Generation**:
- 2-4 context-aware queries per market cell
- Detects holding companies (searches for subsidiaries)
- Uses current year, industry terminology, geographic qualifiers
- No hardcoded company names

**Grounded Search Execution**:
- Uses Gemini 2.5 Flash with Google Search tool
- Strict sequential execution (6s delay between searches)
- Caches results for 30 days
- Extracts grounding metadata (sources, citations)

**Competitor Extraction**:
- Extracts company names with evidence scores (0.0-1.0)
- Identifies parent companies and subsidiaries
- Entity resolution maps brands to parents
- Maximum 3-7 competitors per market cell

#### 3. Metric Engine (`metric_engine.py`)
**Purpose**: Collect and normalize competitive metrics

**Metric Selection**:
- 10 diverse metrics across 6 capability families
- Families: Revenue, Profitability, Efficiency, Growth, Quality, Scale
- Generic fallback metrics if industry-specific ones fail
- No hardcoded metric names

**Batch Search Optimization**:
- Combines multiple metrics into single search queries
- Reduces search volume from ~280 to ~30 queries
- Query broadening if no results (up to 2 attempts)
- 6-second delay between searches

**Value Extraction**:
- LLM extracts numeric values with metadata
- Captures: value, units, period, scope, confidence
- Multiple values per metric tracked

**Normalization Process**:
```python
1. Currency Conversion:
   - Primary: ECB API via exchangerate.host
   - Secondary: exchangerate-api.com for broader coverage
   - Fallback: Web search for any missing rates
   - NO HARDCODED EXCHANGE RATES
   - Period-aware (uses historical rates when available)

2. Period Alignment:
   - Preference: TTM > Full Year > Quarter (annualized)
   - Quarterly → Annual: multiply by 4
   - Monthly → Annual: multiply by 12

3. Comparability Classification:
   - Exact: Same metric, period, scope
   - Adjusted: Minor differences, normalized
   - Proxy: Different but related metric
```

#### 4. Report Generator (`report_generator.py`)
**Purpose**: Generate HTML and JSON outputs

**Data Collection Mode**:
- Pure data tables (no narrative text)
- Company + competitors in columns
- 10 metrics in rows
- Period row under company names
- Empty cells for missing data
- Consolidated footnotes

**HTML Styling**:
- Georgia font (PD2 standard)
- Clean table formatting
- Hover effects on rows
- Mobile-responsive design

**JSON Evidence Pack**:
```json
{
  "metadata": {
    "company_id": 1,
    "company_name": "Example Corp",
    "timestamp": "2025-01-20T10:30:00",
    "analysis_version": "1.0.0"
  },
  "company_context": {...},
  "market_cells": [...],
  "competitive_analyses": {...},
  "data_quality": {
    "total_observations": 673,
    "avg_confidence": 0.72,
    "high_confidence_count": 412
  }
}
```

### Database Schema (SQLite)

```sql
-- Core Tables
companies (id, name, context_json, created_at)
market_cells (id, company_id, product_service, geography, customer_segment, materiality_score)
competitors (id, market_cell_id, name, parent_company, evidence_score, presence_evidence)
metrics (id, name, definition, capability_family, calculation_type)

-- Observations (the heart of the system)
observations (
  id, market_cell_id, competitor_id, metric_id,
  value, units, period, scope, currency,
  normalized_value, comparability_class,
  confidence_score, normalization_notes,
  created_at
)

-- Provenance
sources (id, url, title, domain, accessed_at)
observation_sources (observation_id, source_id)

-- Caching
search_cache (id, query_hash, query_text, response_json, grounding_metadata_json, expires_at)
```

## Technical Implementation Details

### Rate Limiting and Retry Logic

```python
# In utils.py
def retry_with_backoff(func, max_retries=3, initial_delay=1.0):
    """
    Exponential backoff with jitter
    Handles rate limits gracefully
    Parses Retry-After headers
    """
    for attempt in range(max_retries):
        try:
            return func()
        except Exception as e:
            if "429" in str(e):  # Rate limit
                delay = parse_retry_after(e) or (initial_delay * (2 ** attempt))
                time.sleep(delay + random.uniform(0, 1))
            else:
                raise
```

### Thread Safety Implementation

```python
# Global print lock in utils.py
print_lock = threading.Lock()

def thread_safe_print(message):
    """All console output must use this"""
    with print_lock:
        print(message)
        sys.stdout.flush()
```

### Table Cleanup Functions

```python
# In utils.py
def clean_markdown_tables(content):
    """
    Fixes Marker library table corruption
    - Removes excessive dashes (>50)
    - Fixes alignment markers
    - Ensures proper spacing
    """
    
def validate_and_fix_tables(content):
    """
    Enforces size constraints
    - Max 10 columns
    - Max 20 rows
    - Truncates with [...] markers
    """
```

### Currency Conversion (NO HARDCODING)

```python
# In metric_engine.py
def normalize_currency(self, value, from_currency, to_currency="USD", period=None):
    """
    Dynamic currency conversion - NO HARDCODED RATES
    1. Try ECB API
    2. Try exchangerate-api.com
    3. Web search for specific rate
    """
    rates = self.get_ecb_exchange_rates(fx_date)
    
    if from_currency not in rates:
        # Dynamically search for rate
        rate = self._search_exchange_rate(from_currency, 'EUR', fx_date)
        if rate:
            rates[from_currency] = rate
```

## Configuration and Environment

### Required Environment Variables
```bash
# In .env file
GEMINI_API_KEY=your_api_key_here

# Optional tuning (can set via export)
LLM_MAX_INFLIGHT=4              # Concurrent LLM calls (1-16)
MAX_SECTION_WORKERS=3           # Parallel section workers (1-8)
SUBMISSION_STAGGER_SEC=0.5      # Delay between submissions
MARKER_PROCESS_WORKERS=2        # PDF conversion workers (1-5)
```

### Model Selection
Interactive prompt at startup:
1. `gemini-2.5-flash` (default, required for web search)
2. `gemini-2.5-flash-lite` (cheaper, no web search)
3. `gemini-2.0-flash` (legacy)

### Python Dependencies
```
google-generativeai>=0.8.0
marker-pdf>=0.3.0
python-markdown>=3.5
beautifulsoup4>=4.12
pandas>=2.0
sqlite3 (built-in)
```

## Recent Development History

### Session: January 20, 2025

#### Major Changes to Competitive Module

**1. Removed All Hardcoded Values**
- Previously had hardcoded company names (Axiata, Maxis, etc.)
- Previously had hardcoded currency rates (MYR=4.6, etc.)
- Now uses principle-based approaches throughout
- Dynamic web search for any missing data

**2. Performance Optimization**
- Batch search: Reduced from ~280 to ~30 searches
- Combined multiple metrics per search query
- 90-minute runtime (down from 4+ hours)

**3. Fixed Critical Bugs**
- sqlite3.Row access error in report_generator.py
- Currency conversion failures for Asian currencies
- Missing subsidiaries for holding companies

**4. Added to CLAUDE.md**
```
"Any document analysis should be designed using principle-based 
approaches that make wise choices, avoiding hardcoded specific 
criteria unless absolutely necessary and explicitly approved"
```

### Previous Session: August 16, 2025

#### Performance Improvements
1. **Configurable Concurrency**: LLM_MAX_INFLIGHT, MAX_SECTION_WORKERS
2. **Process Pool for Marker**: Avoids PyTorch conflicts
3. **Two-Phase Scheduling**: Section 32 doesn't block main profile
4. **Conversion Cache**: SHA-256 based, skips re-conversion
5. **Single-Key Prompts**: Faster UI navigation

#### Critical Fixes
1. **Output Size Limits**: Prevents 1.3MB+ runaway outputs
2. **Thread Safety**: thread_safe_print() everywhere
3. **Table Corruption**: Multiple cleanup functions
4. **HTML Rendering**: Fixed tables, footnotes, Section 32

## Current State and Known Issues

### Working Well
✅ Full 32-section analysis pipeline
✅ Competitive intelligence with web search
✅ PDF conversion with caching
✅ Parallel processing with rate limiting
✅ HTML output with tables and formatting
✅ Learning system capturing patterns
✅ Two-phase scheduling for Section 32

### Known Issues

#### 1. API Reliability
- Occasional 500/504 errors from Gemini
- Rate limits despite backoff
- **Mitigation**: Retry logic, fallback to previous steps

#### 2. Currency Conversion
- Some currencies not in primary APIs
- Web search fallback can be slow
- **Mitigation**: Multi-tier approach (ECB → alt API → web search)

#### 3. Table Rendering
- Edge cases with unusual formatting
- Very wide tables truncated
- **Mitigation**: Multiple cleanup passes

#### 4. Section 32 Timeouts
- Large documents can timeout
- **Mitigation**: Two-phase scheduling, 500KB limit

#### 5. Memory Growth
- Learning memory can become repetitive
- **Mitigation**: Quality score filtering (9-10 only)

## Operational Procedures

### Running Full Analysis

```bash
# 1. Navigate to project
cd /Users/ralfpilarczyk/Documents/Python/PD2
source venv/bin/activate

# 2. Run main application
python PD2.py

# 3. Follow prompts:
- Select model (1 for Flash)
- Choose files (PDFs or existing .md)
- Select sections (1-5 for all, or specific groups)
- Run competitive analysis? (y/n)
- Enable discovery pipeline? (y/n)
- Number of workers (3 recommended)

# 4. Monitor progress
- Watch for "Section X completed successfully"
- Check for rate limit retries
- Competitive module shows search progress

# 5. View results
open runs/run_*/[Company]_profile.html
open runs/run_*/competitive/*_competitive_analysis.html
```

### Running Competitive Analysis Standalone

```bash
python -m src.competitive.cli

# Options:
1. Discovery only (find competitors)
2. Full collection (competitors + metrics)
3. Exit

# Provide documents path when prompted
# Results in runs/run_*/competitive/
```

### Debugging Issues

```bash
# Check run summary
cat runs/run_*/run_summary.txt

# Find errors
grep -i error runs/run_*/run_summary.txt

# Check specific section
ls runs/run_*/section_5/

# View database
sqlite3 runs/run_*/competitive/competitive_analysis.db
.tables
SELECT COUNT(*) FROM observations;

# Check cache
ls memory/conversion_cache/
```

### Common Problems and Solutions

**Problem**: "sqlite3.Row object has no attribute 'get'"
**Solution**: Already fixed - use dict-like access without .get()

**Problem**: Currency conversion failures
**Solution**: System now uses dynamic APIs and web search

**Problem**: Empty sections in output
**Solution**: Check step files, likely API timeout

**Problem**: Tables not rendering in HTML
**Solution**: Check for blank lines before tables

**Problem**: Section 32 timeout
**Solution**: Normal - two-phase scheduling handles it

## Critical Design Principles

### 1. No Hardcoding
- **Never** hardcode company names
- **Never** hardcode currency rates
- **Never** hardcode industry-specific criteria
- Use principle-based approaches
- Make intelligent choices dynamically

### 2. Progressive Refinement
- Start comprehensive (Step 1)
- Identify gaps (Step 2)
- Enhance (Step 3)
- Polish to essentials (Step 4)
- Each step builds on previous

### 3. Provenance and Transparency
- Every data point traces to sources
- Confidence scores on all observations
- Normalization notes explain adjustments
- Comparability classifications documented

### 4. Graceful Degradation
- Fallback to previous step if current fails
- Empty output detection and recovery
- Cache to avoid repeated API calls
- Multiple attempts with broadening queries

### 5. User Experience First
- All inputs collected upfront
- Progress visible throughout
- Immediate delivery of core profile
- Clean, professional HTML output

### 6. Resource Management
- Tunable concurrency controls
- Process isolation for risky operations
- Memory limits on outputs
- Thread-safe operations throughout

## Integration Points

### PD2 → Competitive Module
```python
# In PD2.py
if run_competitive:
    from src.competitive.cli import run_competitive_analysis
    evidence_pack_path = run_competitive_analysis(
        combined_content=combined_content,
        company_name=company_name,
        output_dir=run_dir
    )
```

### Competitive → PD2 Sections
Currently injects mini-tables into Sections 13-18 during HTML generation.
Future: Context-aware injections throughout profile.

## Testing Checklist

Before committing changes, verify:

1. **Core Functionality**
   - [ ] Sections generate with content
   - [ ] HTML renders properly
   - [ ] Tables display correctly
   - [ ] Footnotes appear

2. **Competitive Module**
   - [ ] Finds relevant competitors
   - [ ] Collects metrics successfully
   - [ ] Handles missing data gracefully
   - [ ] No hardcoded values used

3. **Performance**
   - [ ] Parallel processing works
   - [ ] Rate limiting prevents 429s
   - [ ] Cache reduces redundant calls
   - [ ] Runtime reasonable (<40 min typical)

4. **Error Handling**
   - [ ] API failures don't crash pipeline
   - [ ] Empty steps have fallbacks
   - [ ] Thread safety maintained
   - [ ] Size limits enforced

## Next Conversation Instructions

When starting a new conversation about PD2:

1. **Have Claude read this document first**:
   "Please read HANDOVER.md in the Manuals folder to understand the PD2 system"

2. **For competitive module work, also read**:
   "Please also read WEBSEARCH.md for competitive module details"

3. **Key context to provide**:
   - Current working directory
   - Any recent error messages
   - Specific section/module you're working on
   - Whether running standalone or integrated

4. **Critical reminders**:
   - NO HARDCODED VALUES (companies, currencies, criteria)
   - Use thread_safe_print() for all output
   - Respect rate limits (6s for web search)
   - Test with small section groups first

## Conclusion

This document provides complete context for continuing PD2 development. The system is functional but has room for enhancement in reliability, performance, and integration. Focus on maintaining the principle-based approach while improving robustness and user experience.

For detailed competitive module architecture and workflows, see WEBSEARCH.md.
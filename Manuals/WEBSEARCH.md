# Competitive Intelligence Module - Complete Technical Specification

## Table of Contents
1. [Module Overview](#module-overview)
2. [Architecture and Data Flow](#architecture-and-data-flow)
3. [Detailed Component Specifications](#detailed-component-specifications)
4. [API Contracts and Interfaces](#api-contracts-and-interfaces)
5. [Search Strategy and Optimization](#search-strategy-and-optimization)
6. [Data Normalization Pipeline](#data-normalization-pipeline)
7. [Database Schema and Operations](#database-schema-and-operations)
8. [Integration with PD2](#integration-with-pd2)
9. [Prompt Templates](#prompt-templates)
10. [Error Handling and Recovery](#error-handling-and-recovery)
11. [Performance Optimization](#performance-optimization)
12. [Testing and Validation](#testing-and-validation)

## Module Overview

The Competitive Intelligence Module is a standalone system that can run independently or integrate with PD2. It performs web-based competitive analysis through systematic discovery, data collection, normalization, and strategic analysis.

### Core Capabilities
- **Market Cell Discovery**: Identifies business segments from financial documents
- **Competitor Discovery**: Finds competitors via grounded web search
- **Metric Collection**: Gathers 10 key metrics per market with batch optimization
- **Data Normalization**: Currency conversion, period alignment, comparability scoring
- **Strategic Analysis**: Competitive positioning and strategy recommendations (optional)
- **Report Generation**: HTML reports and JSON evidence packs

### Critical Design Principles
1. **NO HARDCODED VALUES**: No company names, currencies, or industry-specific criteria
2. **Principle-Based Logic**: Intelligent choices based on context, not fixed rules
3. **Full Provenance**: Every data point traces to sources with confidence scores
4. **Graceful Degradation**: Multiple fallbacks for missing data or API failures
5. **Performance Optimization**: Batch searches, caching, query broadening
6. **Market Separation**: Analyze each market cell independently, no cross-market consolidation

## Architecture and Data Flow

### High-Level Flow
```
1. Document Analysis
   ├── Extract Company Context
   └── Discover Market Cells (Product × Geography × Segment)

2. Competitor Discovery (Per Market Cell)
   ├── Generate Search Queries (2-4 per cell)
   ├── Execute Grounded Searches (sequential, 6s delay)
   ├── Extract Competitors (3-7 per cell)
   └── Entity Resolution (brands → parents)

3. Metric Collection (Per Competitor)
   ├── Select 10 Diverse Metrics
   ├── Batch Search (combine metrics)
   ├── Extract Values with Metadata
   └── Normalize (currency, period, comparability)

4. Analysis & Reporting
   ├── Data Collection Tables (no narrative)
   ├── HTML Report Generation
   └── JSON Evidence Pack
```

### File Structure
```
src/competitive/
├── cli.py                    # Entry point & orchestration
├── market_mapper.py          # Market cell discovery
├── peer_discovery.py         # Competitor finding
├── metric_engine.py          # Metric collection & normalization
├── report_generator.py       # HTML/JSON output
├── database.py              # SQLite operations
├── entity_resolution.py     # Brand mapping
└── pivot_exporter.py        # CSV export utilities
```

## Detailed Component Specifications

### 1. CLI Interface (`cli.py`)

**Main Entry Points**:
```python
def main():
    """Standalone CLI with interactive prompts"""
    # Options:
    # 1. Discovery only (competitors)
    # 2. Full collection (competitors + metrics)
    
def run_competitive_analysis(combined_content, company_name, output_dir):
    """PD2 integration entry point"""
    # Returns: path to evidence_pack.json
```

**Workflow Orchestration**:
1. Initialize database in output directory
2. Extract company context from documents
3. Discover market cells (3-8 target)
4. For each market cell:
   - Discover competitors (including subsidiaries for holding companies)
   - Collect metrics (10 per market)
5. Generate reports (HTML + JSON)

### 2. Market Mapper (`market_mapper.py`)

**Company Context Extraction**:
```python
def extract_company_context(documents: str) -> Dict:
    """
    Extract structured company information from documents
    
    Returns:
    {
        "company_name": str,
        "ticker": str,
        "industry": str,
        "business_model": str,  # B2B/B2C/platform
        "primary_geography": str,
        "products_services": List[str],
        "document_text": str  # For segment inference
    }
    
    NO HARDCODED COMPANY NAMES
    """
```

**Market Cell Discovery**:
```python
def discover_market_cells(company_context: Dict) -> List[Dict]:
    """
    Discover market cells from document segments
    
    Market Cell = Product/Service × Geography × Customer Segment
    
    Returns:
    [{
        "product_service": str,
        "geography": str,
        "customer_segment": str,
        "materiality_score": float,  # 0.0-1.0
        "evidence": str  # From document
    }]
    
    Rules:
    - Target 3-8 cells total
    - Each >20% materiality threshold
    - Respect company's segment reporting
    - If >8, consolidate by dominant dimension
    """
```

**Consolidation Logic**:
```python
def consolidate_market_cells(cells: List[Dict]) -> List[Dict]:
    """
    If >8 cells, intelligent consolidation:
    
    1. Find dominant dimension (product/geo/customer)
    2. Group by that dimension
    3. Drop groups <10% materiality
    4. Redistribute weights to sum to 1.0
    
    Preserves business logic, not arbitrary
    """
```

### 3. Peer Discovery (`peer_discovery.py`)

**Search Query Generation**:
```python
def generate_search_queries(company_context: Dict, market_cell: Dict) -> List[str]:
    """
    Generate 2-4 context-aware queries per market
    
    Query Patterns:
    - "[product] companies in [geography] [current_year]"
    - "[industry] competitors [geography] market share latest"
    - "Top [product] providers [customer_segment] recent"
    
    Special Cases:
    - Holding companies: "subsidiaries of [company_name]"
    - Regional markets: Include local language terms
    
    NO HARDCODED COMPANY NAMES
    Uses current date for recency
    """
```

**Holding Company Detection**:
```python
def is_holding_company(company_context: Dict) -> bool:
    """
    Detect if company is a holding company
    
    Signals:
    - Name contains: Group, Holdings, Berhad, PLC
    - Multiple distinct business units
    - Subsidiary mentions in documents
    
    If true, search for subsidiaries first
    """
```

**Grounded Search Execution**:
```python
def execute_grounded_search(query: str) -> Dict:
    """
    Execute web search with Google grounding
    
    Process:
    1. Check cache (30-day TTL)
    2. If miss, execute with Gemini 2.5 Flash
    3. Wait 6 seconds (rate limit)
    4. Extract grounding metadata
    5. Save to cache
    
    Returns:
    {
        "response_text": str,
        "grounding_metadata": {
            "sources": [{"url": str, "title": str}],
            "search_queries": [str]
        }
    }
    """
```

**Competitor Extraction**:
```python
def extract_competitors(search_results: Dict, 
                       company_context: Dict,
                       market_cell: Dict) -> List[Dict]:
    """
    Extract competitors from search results
    
    Returns:
    [{
        "name": str,
        "parent_company": str (optional),
        "evidence": str,
        "evidence_score": float  # 0.0-1.0
    }]
    
    Rules:
    - Max 7 competitors per market
    - Min evidence score 0.3
    - Include subsidiaries if holding company
    - Apply entity resolution
    """
```

### 4. Metric Engine (`metric_engine.py`)

**Metric Selection**:
```python
def select_metrics_for_market_cell(company_context: Dict, 
                                  market_cell: Dict, 
                                  target_count: int = 10) -> List[Dict]:
    """
    Select 10 diverse metrics across 6 families
    
    Capability Families:
    1. Revenue (revenue, market share)
    2. Profitability (margins, EBITDA, ROE)
    3. Efficiency (utilization, productivity)
    4. Growth (YoY growth, CAGR)
    5. Quality (NPS, churn, satisfaction)
    6. Scale (subscribers, stores, capacity)
    
    NO HARDCODED METRIC NAMES
    Adapts to industry context
    """
```

**Batch Search Optimization**:
```python
def generate_batch_search_query(company_context: Dict,
                               competitor_name: str,
                               market_cell: Dict,
                               metrics: List[Dict]) -> str:
    """
    Combine multiple metrics into single query
    
    Example:
    "[Competitor] [Geography] revenue EBITDA margin market share 
     subscribers growth latest quarterly earnings report"
    
    Reduces searches from 280 to ~30
    Massive performance improvement
    """
```

**Query Broadening**:
```python
def broaden_search_query(original_query: str, attempt: int) -> str:
    """
    Progressive broadening (max 2 attempts)
    
    Attempt 1: Remove time qualifiers
    - "Q3 2024" → "recent"
    - "latest quarter" → "current"
    
    Attempt 2: Remove specificity
    - Drop geographic qualifiers
    - Use broader metric terms
    
    Preserves company name and core intent
    """
```

**Value Extraction**:
```python
def extract_batch_metrics(search_results: Dict, 
                         competitor_name: str,
                         metrics: List[Dict]) -> List[Dict]:
    """
    Extract all metrics from single search
    
    For each metric:
    {
        "metric_name": str,
        "metric_found": bool,
        "values": [{
            "value": float,
            "units": str,  # %, $, M, B
            "period": str,  # Q3 2024, FY2023, TTM
            "scope": str,   # Geographic qualifier
            "confidence": float  # 0.0-1.0
        }],
        "confidence": float  # Overall
    }
    """
```

**Currency Normalization (NO HARDCODING)**:
```python
def normalize_currency(value: float, from_currency: str, 
                      to_currency: str = "USD", period: str = None):
    """
    Dynamic currency conversion - NO HARDCODED RATES
    
    Three-tier approach:
    1. ECB API via exchangerate.host
    2. exchangerate-api.com (broader coverage)
    3. Web search for specific rate
    
    Period-aware:
    - Extracts year from period string
    - Uses historical rates when available
    
    Returns: (normalized_value, fx_info)
    """

def _search_exchange_rate(from_currency: str, to_currency: str, 
                         date: str = None) -> Optional[float]:
    """
    Web search fallback for any currency
    
    Query: "[from] to [to] exchange rate [date]"
    Uses Gemini with grounding
    Parses numeric value from response
    
    Can handle ANY currency pair
    """
```

**Period Normalization**:
```python
def normalize_period(value: float, period: str, 
                    target_period: str = "TTM") -> Tuple[float, str]:
    """
    Standardize time periods
    
    Preference: TTM > Annual > Quarterly
    
    Conversions:
    - Quarterly → Annual: ×4
    - Monthly → Annual: ×12
    - Keeps as-is if incompatible
    
    Returns: (normalized_value, note)
    """
```

### 5. Report Generator (`report_generator.py`)

**Data Collection Mode**:
```python
def generate_data_collection_report(company_id: int) -> str:
    """
    Generate pure data tables (no narrative)
    
    Per Market Cell:
    ┌─────────────┬──────────┬──────────┬──────────┐
    │ Metric      │ Company  │ Comp 1   │ Comp 2   │
    ├─────────────┼──────────┼──────────┼──────────┤
    │ Period      │ Q3 2024  │ Q3 2024  │ Q2 2024  │
    ├─────────────┼──────────┼──────────┼──────────┤
    │ Revenue     │ $450M    │ $520M    │ $380M    │
    │ Market Share│ 28%      │ 32%      │ 24%      │
    │ EBITDA %    │ 42%      │ 45%      │ 38%      │
    │ Growth YoY  │ 12%      │ 8%       │ 15%      │
    │ ...         │ ...      │ ...      │ ...      │
    └─────────────┴──────────┴──────────┴──────────┘
    
    HTML with Georgia font (PD2 standard)
    Empty cells for missing data
    Consolidated footnotes at end
    """
```

**sqlite3.Row Handling**:
```python
# Fixed issue with sqlite3.Row access
# sqlite3.Row objects don't have .get() method
# Use try/except for safe access:

try:
    customer_seg = market_cell['customer_segment']
except (KeyError, IndexError):
    customer_seg = ''
```

**JSON Evidence Pack**:
```python
def generate_evidence_pack(company_id: int) -> Dict:
    """
    Complete evidence for PD2 integration
    
    Structure:
    {
        "metadata": {
            "company_id": int,
            "company_name": str,
            "timestamp": str,
            "analysis_version": "1.0.0",
            "total_searches": int,
            "runtime_minutes": float
        },
        "company_context": {...},
        "market_cells": [
            {
                "id": int,
                "product_service": str,
                "geography": str,
                "customer_segment": str,
                "materiality_score": float
            }
        ],
        "competitors_by_market": {
            "market_1": [...],
            "market_2": [...]
        },
        "observations": [
            {
                "market_cell_id": int,
                "competitor": str,
                "metric": str,
                "value": float,
                "units": str,
                "period": str,
                "confidence": float,
                "sources": [...]
            }
        ],
        "data_quality": {
            "total_observations": int,
            "avg_confidence": float,
            "high_confidence_count": int,
            "coverage_by_metric": {...}
        },
        "mini_tables": [...]  # For PD2 injection
    }
    """
```

## Database Schema and Operations

### Complete SQLite Schema
```sql
-- Core entity tables
CREATE TABLE companies (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    context_json TEXT,  -- Full extracted context
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE market_cells (
    id INTEGER PRIMARY KEY,
    company_id INTEGER,
    product_service TEXT,
    geography TEXT,
    customer_segment TEXT,
    materiality_score REAL,  -- 0.0-1.0
    evidence TEXT,
    FOREIGN KEY (company_id) REFERENCES companies(id)
);

CREATE TABLE competitors (
    id INTEGER PRIMARY KEY,
    market_cell_id INTEGER,
    name TEXT NOT NULL,
    parent_company TEXT,  -- For subsidiaries
    evidence_score REAL,  -- 0.0-1.0 confidence
    presence_evidence TEXT,
    FOREIGN KEY (market_cell_id) REFERENCES market_cells(id),
    UNIQUE(market_cell_id, name)  -- One entry per competitor per market
);

CREATE TABLE metrics (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    definition TEXT,
    capability_family TEXT,  -- Revenue, Profitability, etc.
    calculation_type TEXT    -- higher_better, lower_better
);

-- Core observations table (the heart of the system)
CREATE TABLE observations (
    id INTEGER PRIMARY KEY,
    market_cell_id INTEGER,
    competitor_id INTEGER,
    metric_id INTEGER,
    value REAL,
    units TEXT,              -- %, $, M, B, K
    period TEXT,             -- Q3 2024, FY2023, TTM
    scope TEXT,              -- Geographic/segment
    currency TEXT,           -- Original currency
    normalized_value REAL,   -- After conversion
    normalized_currency TEXT DEFAULT 'USD',
    comparability_class TEXT, -- exact, adjusted, proxy
    confidence_score REAL,   -- 0.0-1.0
    normalization_notes TEXT, -- JSON details
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (market_cell_id) REFERENCES market_cells(id),
    FOREIGN KEY (competitor_id) REFERENCES competitors(id),
    FOREIGN KEY (metric_id) REFERENCES metrics(id)
);

-- Provenance tracking
CREATE TABLE sources (
    id INTEGER PRIMARY KEY,
    url TEXT,
    title TEXT,
    domain TEXT,
    accessed_at TIMESTAMP
);

CREATE TABLE observation_sources (
    observation_id INTEGER,
    source_id INTEGER,
    PRIMARY KEY (observation_id, source_id),
    FOREIGN KEY (observation_id) REFERENCES observations(id),
    FOREIGN KEY (source_id) REFERENCES sources(id)
);

-- Search cache (30-day TTL)
CREATE TABLE search_cache (
    id INTEGER PRIMARY KEY,
    query_hash TEXT UNIQUE,  -- SHA-256 of query
    query_text TEXT,
    response_json TEXT,
    grounding_metadata_json TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP
);

-- Performance indexes
CREATE INDEX idx_obs_market_metric ON observations(market_cell_id, metric_id);
CREATE INDEX idx_obs_competitor ON observations(competitor_id);
CREATE INDEX idx_cache_expires ON search_cache(expires_at);
CREATE INDEX idx_obs_confidence ON observations(confidence_score);
```

### Key Database Operations
```python
class CompetitiveAnalysisDB:
    def get_or_create_self_competitor(self, market_cell_id: int, 
                                     company_name: str) -> int:
        """
        Ensure company itself exists as competitor
        For collecting company's own metrics
        """
    
    def cleanup_expired_cache(self):
        """Remove cache entries older than 30 days"""
    
    def get_observation_stats(self) -> Dict:
        """
        Returns quality metrics:
        - Total observations
        - Average confidence
        - Distribution by comparability class
        """
```

## Search Strategy and Optimization

### Performance Timeline
```
Original: 4+ hours (individual metric searches)
Current:  90 minutes (batch optimization)

Breakdown:
- Market discovery: 2-3 minutes
- Competitor discovery: 10-15 minutes (sequential)
- Metric collection: 60-75 minutes (batched)
- Report generation: 5 minutes
```

### Optimization Techniques

#### 1. Batch Search (90% reduction in API calls)
```python
# BAD: 10 metrics × 7 competitors × 4 markets = 280 searches
for metric in metrics:
    for competitor in competitors:
        search(f"{competitor} {metric}")

# GOOD: 1 search × 7 competitors × 4 markets = 28 searches
for competitor in competitors:
    search(f"{competitor} {' '.join(all_metrics)}")
```

#### 2. Query Broadening (prevents empty results)
```python
# Original query fails
"CelcomDigi Malaysia Q3 2024 revenue EBITDA margin"

# Attempt 1: Remove time specificity
"CelcomDigi Malaysia recent revenue EBITDA margin"

# Attempt 2: Remove geography
"CelcomDigi revenue EBITDA margin latest"
```

#### 3. Subsidiary Detection (captures all competitors)
```python
# For holding companies like Axiata
if is_holding_company(company):
    # First search for subsidiaries
    search("Axiata subsidiaries operating companies")
    # Returns: CelcomDigi, XL, Robi, Dialog, etc.
    
    # Then search for each subsidiary's metrics
    for subsidiary in subsidiaries:
        search(f"{subsidiary} metrics...")
```

#### 4. Cache Strategy
```python
# 30-day cache for search results
cache_key = hashlib.sha256(query.encode()).hexdigest()
if cached := get_cache(cache_key):
    return cached
else:
    result = perform_search(query)
    save_cache(cache_key, result, ttl_days=30)
```

## Integration with PD2

### Entry Point in PD2
```python
# In PD2.py, after file selection
if prompt_yn("Run competitive analysis first?"):
    from src.competitive.cli import run_competitive_analysis
    
    evidence_pack_path = run_competitive_analysis(
        combined_content=combined_markdown,
        company_name=company_name,
        output_dir=run_directory
    )
    
    # Store for section injection
    competitive_data = load_json(evidence_pack_path)
```

### Section Injection (Currently 13-18)
```python
# In profile_generator.py
def inject_competitive_table(section_content: str, 
                            section_num: int,
                            evidence_pack: Dict) -> str:
    """
    For sections 13-18:
    Append mini-table with relevant metrics
    
    Future: Context-aware injection in all sections
    """
    if section_num in range(13, 19):
        mini_table = evidence_pack['mini_tables'][0]
        return section_content + "\n\n" + format_table(mini_table)
    return section_content
```

## Prompt Templates

### Company Context Extraction
```python
COMPANY_CONTEXT_PROMPT = """
Analyze these financial documents and extract structured company information.

Return JSON with these fields:
- company_name: Official name
- ticker: Stock ticker if public
- industry: Primary industry classification
- business_model: B2B/B2C/platform/marketplace
- primary_geography: Main geographic focus
- products_services: List of core offerings
- target_customers: Customer segments served

Extract ONLY from provided documents.
NO assumptions or external knowledge.
"""
```

### Market Cell Discovery
```python
MARKET_CELL_PROMPT = """
Based on the company context, identify distinct market cells where this company competes.

A market cell = Product/Service × Geography × Customer Segment

Rules:
1. Use company's segment reporting if available
2. Each cell should be >20% of business
3. Target 3-8 total cells
4. Each should be managed as distinct P&L

Return JSON array with:
- product_service
- geography
- customer_segment
- materiality_score (0.0-1.0)
- evidence (quote from documents)

Think like an executive: Would these be separate business units?
"""
```

### Competitor Discovery
```python
COMPETITOR_SEARCH_PROMPT = """
Search for direct competitors in this market:
Product/Service: {product}
Geography: {geography}
Customer Segment: {segment}

Find companies that:
1. Offer similar products/services
2. Operate in same geography
3. Target same customers
4. Compete for same revenue

Include subsidiaries of larger companies.
Maximum 7 competitors.
"""
```

### Metric Extraction
```python
BATCH_METRIC_PROMPT = """
Extract these metrics for {competitor} from search results:
{metrics_list}

For EACH metric, provide:
- metric_found: true/false
- values: Array of found values
  - value: numeric only
  - units: %, $, M, B, K
  - period: Q3 2024, FY2023, TTM, etc.
  - confidence: 0.0-1.0
  - extraction_notes: Any caveats

Return EMPTY array if metric not found.
Be precise with numbers and periods.
"""
```

## Error Handling and Recovery

### API Error Handling
```python
def handle_api_error(error: Exception, context: Dict):
    """
    Comprehensive error recovery
    
    429 (Rate Limit):
    - Parse Retry-After header
    - Exponential backoff
    - Maximum 3 retries
    
    500/504 (Server Error):
    - Log error details
    - Mark as failed
    - Continue with next item
    
    Network Error:
    - Retry with backoff
    - Check internet connection
    - Fallback to cache
    """
```

### Missing Data Strategies
```python
def handle_missing_metric(metric: str, competitor: str):
    """
    1. Try broadened query (2 attempts)
    2. Check if metric industry-relevant
    3. Leave cell empty in report
    4. Note in data quality stats
    
    Never fail entire analysis for one metric
    """
```

### Recovery from Partial Runs
```python
def resume_from_checkpoint(db_path: str):
    """
    1. Check existing database
    2. Identify last successful operation
    3. Skip completed work
    4. Resume from failure point
    
    Prevents re-running expensive searches
    """
```

## Testing and Validation

### Critical Validation Points

#### No Hardcoding Validation
```python
def validate_no_hardcoding(code_file: str):
    """
    Check for hardcoded values:
    - Company names (Axiata, Maxis, etc.)
    - Currency rates (MYR=4.6, etc.)
    - Industry-specific metrics
    
    Should find ZERO instances
    """
```

#### Rate Limit Compliance
```python
def validate_rate_limits(search_log: List):
    """
    Verify:
    - 6+ seconds between searches
    - No parallel grounded searches
    - Proper backoff on 429
    """
```

#### Data Quality Checks
```python
def validate_data_quality(observations: List):
    """
    Check:
    - Confidence scores reasonable (0.3-0.9)
    - Periods make sense (recent)
    - Values within expected ranges
    - Currency conversions applied
    """
```

### Test Scenarios

1. **Holding Company Test**
   - Input: Axiata Group documents
   - Expected: Find subsidiaries (CelcomDigi, XL, Robi)
   - Verify: Metrics for each subsidiary

2. **Multi-Currency Test**
   - Markets in different countries
   - Verify: All converted to USD
   - Check: Conversion rates logged

3. **Missing Data Test**
   - Competitor with limited public data
   - Verify: Empty cells, not errors
   - Check: Other competitors still processed

4. **Cache Test**
   - Run same analysis twice
   - Verify: Second run uses cache
   - Check: Runtime much faster

## Common Issues and Solutions

### Issue: "Currency conversion failed: MYR not found"
**Root Cause**: MYR not in ECB API
**Solution**: System now uses 3-tier approach with web search
**Status**: FIXED

### Issue: "Analysis took 4+ hours"
**Root Cause**: Individual searches for each metric
**Solution**: Batch search optimization
**Status**: FIXED (now ~90 minutes)

### Issue: "Missing important subsidiaries"
**Root Cause**: Not detecting holding companies
**Solution**: Added subsidiary search for holdings
**Status**: FIXED

### Issue: "sqlite3.Row has no attribute 'get'"
**Root Cause**: sqlite3.Row doesn't support .get()
**Solution**: Use try/except for safe access
**Status**: FIXED

### Issue: "No competitors found for market"
**Potential Causes**:
1. Market definition too narrow
2. Geography spelling variations
3. Industry terminology mismatch
**Solution**: Broadening strategy, multiple query variants

## Performance Metrics

### Current Performance (January 2025)
- **Total Runtime**: ~90 minutes (was 4+ hours)
- **Searches**: ~30 batched (was ~280 individual)
- **Success Rate**: 70-80% metrics found
- **Cache Hit Rate**: 20-30% on re-runs
- **Database Size**: ~5-10 MB per company

### Resource Usage
- **Memory**: <500 MB
- **CPU**: Single-threaded (rate limited)
- **Network**: ~100-200 MB total
- **Disk**: ~10 MB (DB + cache)

## Future Enhancements

### Planned Improvements

1. **Richer PD2 Integration**
   - Context-aware injection in all sections
   - Competitive narrative generation
   - Cross-references between sections

2. **Smarter Search**
   - Learn from successful queries
   - Industry-specific query templates
   - Multi-language support

3. **Advanced Analytics**
   - Time series tracking
   - Competitive dynamics
   - Market share evolution

4. **Data Quality**
   - Source reputation scoring
   - Recency weighting
   - Confidence calibration

### Architecture Extensions

1. **Multi-Company Mode**
   - Compare multiple companies
   - Industry benchmarking
   - Peer group analysis

2. **Real-Time Updates**
   - Webhook triggers
   - Incremental updates
   - Change detection

3. **Export Options**
   - Excel with formulas
   - PowerBI integration
   - API endpoints

## Conclusion

This specification provides complete technical documentation for the Competitive Intelligence Module. Key achievements:

- **Performance**: 90 minutes (75% reduction from original)
- **Coverage**: Handles any company/industry/geography
- **Quality**: Full provenance and confidence scoring
- **Robustness**: Multiple fallbacks, no hardcoding
- **Integration**: Clean interface with PD2

The system exemplifies principle-based design: making intelligent choices based on context rather than hardcoded rules. It can handle any currency, any market structure, and any industry without modification.

For PD2 integration details and overall system context, see HANDOVER.md.
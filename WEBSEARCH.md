## Competitive Web Search App (Standalone, Integrable with PD2)

### Purpose
A standalone app (under `src/competitive/`) that discovers competitors per market cell, retrieves and grounds comparable metrics from high‑trust web sources, normalizes and stores them with provenance, ranks relative strengths/weaknesses, and proposes strategy bundles. Later, PD2 can consume its "Competitive Evidence Pack" to enrich Sections 13–18 (and selectively 1–12), excluding Section 32.

### Design Principles
- **Simplicity First**: One search per metric per competitor, sequential execution to avoid rate limit complexity
- **Principles-Based Decisions**: Minimal hardcoded rules, maximum intelligent choice-making based on business logic
- **Non-Disruptive Integration**: Add functionality without refactoring existing PD2 code
- **Quality Over Speed**: 25-35 minutes total execution time acceptable for high-quality competitive intelligence
- **Automatic Model Selection**: Use Gemini 2.5 Flash for web grounding (required), user's selected model for other tasks
- **Market Separation**: Analyze competition in each market cell separately, do NOT consolidate competitors across markets
- **Business Judgment**: Apply deep thinking including resource allocation analysis for strategy conflicts

### Operating Modes
1. **Standalone CLI**
   - `python -m src.competitive.cli`
   - Prompts for Company Name, produces SQLite DB + HTML report + JSON Evidence Pack
   
2. **PD2‑Integrated**
   - Menu option in PD2 startup: "Run competitive analysis first? (y/n)"
   - Uses same source documents that user provides for company profile
   - Runs immediately when selected (starts before/during PDF conversion if possible)
   - Automatically enhances Sections 13–18 with competitive data and selective injection in 1–12

### Performance Expectations
- **Web Search Calls**: ~80-120 grounded searches (sequential, 6-second intervals)
- **LLM Calls**: ~25-30 non-grounded calls for analysis and strategy generation
- **Total Execution Time**: 25-35 minutes for typical company (3 market cells, 8 metrics each)
- **Rate Limiting**: Max 10 grounded requests per minute to avoid API limits

## Market Cell Definition & Discovery

### Enhanced Company Context Extraction
Before creating market cells, perform comprehensive company profiling using structured LLM analysis:

1. **Company Understanding Phase**: Extract structured company context from source documents:
```json
{
    "company_name": "primary company name",
    "ticker": "ticker if public, else null",
    "other_companies": ["subsidiaries/affiliates mentioned"],
    "industry": "primary industry",
    "sub_sectors": ["specific verticals/niches"],
    "business_model": "B2B/B2C/marketplace/platform",
    "headquarters": "HQ location",
    "primary_markets": ["list of key geographic markets"],
    "geography": "regional focus description",
    "products_services": "core offerings description",
    "target_customers": "customer segments served",
    "revenue_model": "how they monetize",
    "stage": "startup/growth/mature",
    "financial_highlights": "key metrics mentioned",
    "competitive_keywords": ["terms for finding competitors"],
    "market_keywords": ["terms for market research"]
}
```

2. **Document Analysis Prompt**:
```
Based on these company documents, extract comprehensive company understanding:

[DOCUMENT CONTENT]

Extract structured information covering:
- Company identification and corporate structure
- Industry classification and business model
- Geographic presence and market focus  
- Product/service portfolio and customer base
- Financial indicators and company stage
- Keywords optimal for competitive research

Return structured JSON with the exact format specified above.
```

### Input Source & Processing
- **Same documents** that user provides to PD2 for company profile analysis
- **Enhanced parsing**: First extract company context, then derive market cells
- **Intelligent segmentation**: Use LLM understanding to identify true business divisions

### Market Cell Creation Process
1. **Company Profiling**: Extract structured company context as shown above
2. **Business Logic Mapping**: Use company context to intelligently map dimensions
3. **Smart Segmentation**: Create matrix of (product/service) × (geography) × (customer segment) based on company's actual business structure
4. **Materiality Filter**: Include cells if ≥20% of core metrics (users or revenue)
5. **Business Validation**: "Would a business executive think of these as separate businesses?"

### Market Consolidation Rules (If >10 cells found)
- **Principle**: "Business sense over mathematical precision"
- **Method**: Group by largest dimension (geography, product, or customer)
- **Examples**:
  - "Mobile Germany" + "Mobile France" = "European Mobile Services"
  - "Enterprise Software" + "Consumer Software" might stay separate if truly different businesses
- **Validation**: Each consolidated group must represent ≥10% of company activity
- **Outcome**: Maximum 8 final market cells

### Geographic Scope Strategy
- **Adaptive to Company Disclosure**: Match the company's own reporting granularity
- **Country-level**: If company reports by country
- **Regional**: If company reports by regions (EMEA, APAC, Americas)  
- **Principle**: "Match the company's own reporting structure"

## Peer Discovery (Sequential, Market-Specific)

### Intelligent Query Generation
Use company context to generate smart, targeted search queries:

1. **Context-Aware Query Generation**:
```
Based on this company context, generate targeted search queries for direct competitors in [MARKET_CELL]:

COMPANY CONTEXT:
{structured_company_context_from_above}

MARKET CELL: {product_service} × {geography} × {customer_segment}

Generate 2-3 highly specific search queries that will find direct competitors:
- Include industry-specific terminology from the company context
- Use geographic qualifiers that match the market cell
- Include business model descriptors (B2B/B2C/enterprise/consumer)
- Add recent time qualifiers (2024, 2023)
- Use competitive keywords extracted from company documents

Examples:
- "Singapore digital banking fintech competitors DBS UOB 2024"
- "European B2B payment processing competitors Stripe Adyen"
- "Southeast Asia e-commerce logistics competitors Shopee Lazada"

Return 2-3 optimized search queries per market cell.
```

### Discovery Process
1. **Smart Query Generation**: Use LLM with company context to create intelligent, market-specific queries
2. **Sequential Search Execution**: 2-3 grounded search calls per market cell (6-second intervals)
3. **Enhanced Evidence Extraction**: Competitor names + presence evidence + competitive positioning
4. **Source Quality Prioritization**: Regulatory filings → industry reports → company disclosures → news sources

### Entity Resolution Rules
- **Keep Markets Separate**: "Vodafone Germany" and "Vodafone UK" are separate competitors in their respective markets
- **Subsidiary Resolution**: Resolve brands/subsidiaries to parent companies WITHIN each market
- **Parent Company Logic**: Subsidiaries → controlling parent, but maintain market-specific analysis
- **JV Handling**: Joint ventures → controlling shareholder
- **Sub-brand Logic**: Treat separately only if they compete distinctly in that specific market cell

### Competitor Selection
- **Default**: Top 3 competitors per market cell
- **Configurable**: Allow user to choose different number (range 1-7)
- **Selection Criteria**: Evidence quality + market presence + source reliability
- **Market Separation**: Same parent company can appear as different competitors in different markets

## Metric Selection & Coverage

### Capability Framework
Required capability families to ensure comprehensive coverage:
1. **Cost Position**: Cost per unit, operational efficiency, labor productivity
2. **Scale & Footprint**: Market share, customer base, geographic reach
3. **Customer Economics**: Revenue per customer, retention rates, acquisition costs
4. **Quality & Coverage**: Service quality metrics, network coverage, customer satisfaction
5. **Asset Intensity**: Capex ratios, asset utilization, infrastructure efficiency
6. **Innovation & Regulatory**: R&D spend, patents, regulatory compliance, new product launches

### Auto-Selection Logic
- **Quantity**: 6-10 metrics per market cell (prefer 8 for optimal balance)
- **Relevance**: Apply business judgment to pick metrics most relevant to value creation in that market
- **Diversity Requirements**:
  - **Financial**: Revenue, margins, profitability (2-3 metrics)
  - **Operational**: Efficiency, scale, quality (2-3 metrics)  
  - **Strategic**: Innovation, positioning, growth (2-3 metrics)
- **Principle**: "Focus on metrics that make the most difference to value creation"

### Metric Prioritization Process
1. **Value Impact Assessment**: How much does this metric affect margins/ROCE/growth?
2. **Competitive Relevance**: Is this where competitive battles are won/lost?
3. **Data Availability**: Can we reasonably expect to find this data?
4. **Capability Coverage**: Does this help us understand a key capability?
5. **Business Judgment**: Does an executive care about this metric?

## Search & Grounding Engine (Sequential Only)

### Model Strategy
- **Web Grounding**: Always Gemini 2.5 Flash (required for google_search tool)
- **Auto-Switch Logic**: If user selected Flash-Lite for PD2, automatically use 2.5 Flash for competitive analysis searches
- **Other Tasks**: Use user's selected model for non-grounding tasks (analysis, strategy, etc.)

### Search Execution Strategy
- **Sequential Only**: No parallel processing to avoid rate limit complexity (simpler and more reliable than parallel)
- **One Search Per Task**: One metric per competitor per search call (clean, predictable parsing)
- **Rate Limiting**: 6+ second intervals between searches (max 10 per minute)
- **Google Search Grounding**: Use Gemini's built-in grounding (included with subscription)

### Intelligent Query Generation for Metrics
Use company context and competitor profiles to create optimized search queries:

```
Generate targeted search query for this competitive metric:

COMPANY CONTEXT: {structured_company_context}
COMPETITOR: {competitor_name} in {market_cell}  
METRIC: {metric_name} - {metric_definition}

Create 1 optimized search query that will find this specific metric:
- Use exact competitor name and market geography
- Include metric-specific terminology  
- Add recent time qualifiers (2024, Q3 2024, latest)
- Use industry-specific language from company context
- Target high-quality sources (earnings, reports, filings)

Example outputs:
- "Vodafone Germany mobile subscriber numbers Q3 2024 earnings"
- "Deutsche Telekom Germany market share telecommunications 2024"
- "O2 Germany ARPU average revenue per user latest quarterly"

Return single optimized query.
```

### Query Execution Examples
**Context-Aware**: 
- "Vodafone Germany mobile revenue Q3 2024 earnings report"
- "Deutsche Telekom Germany 5G market share 2024 regulatory filing"  
- "O2 Germany customer acquisition cost quarterly results 2024"

**NOT Generic**:
- "Vodafone revenue" (too broad)
- "telecom market share" (no specific company/market)

### Search Recovery Logic
- **First Attempt**: Specific query (market + metric + recent period)
- **If No Results**: Try broader query (drop most specific qualifier)
  - Drop quarter → "Vodafone Germany mobile revenue 2024"
  - Still nothing → "Vodafone Germany revenue 2024"  
  - Last resort → "Vodafone revenue 2024"
- **After 2 Attempts**: Mark as "data not found" and continue
- **Keep Simple**: Don't over-engineer the recovery logic

### Caching Strategy
- **Cache Duration**: 30 days for identical queries
- **Cache Key**: "company_name + market + metric + period"
- **Example**: "vodafone_germany_mobile_revenue_2024_q3"
- **Cache Hit Logic**: Check exact query first, then check broader variants
- **Storage**: SQLite database with query hash and results

## Data Extraction & Quality Assessment

### AI-Powered Extraction with Confidence Scoring
Use structured LLM analysis to extract metrics with confidence assessment:

```
Extract competitive metric from this grounded search result:

SEARCH QUERY: {original_search_query}
COMPETITOR: {competitor_name}
METRIC SOUGHT: {metric_name} - {metric_definition}
SEARCH RESULTS: {grounded_search_response}

Extract and structure this information:

{
    "metric_found": true/false,
    "values": [
        {
            "value": "extracted numeric value",
            "units": "currency/users/percent/etc",
            "period": "Q3 2024/2024/TTM/etc", 
            "scope": "geographic/business scope",
            "source_quality": "earnings_report/regulatory_filing/news/analyst",
            "confidence": 0.9, // 0.0-1.0 based on source quality and clarity
            "extraction_notes": "context about the metric"
        }
    ],
    "source_metadata": {
        "source_title": "title from grounding metadata",
        "source_url": "URL from grounding metadata", 
        "search_query": "original query used"
    },
    "data_quality_flags": ["recent/outdated", "exact_match/proxy_metric", "clear/ambiguous"]
}
```

**Confidence Scoring Logic**:
- **0.9-1.0**: Earnings reports, regulatory filings, clear recent data
- **0.7-0.8**: Company disclosures, analyst reports, clear methodology  
- **0.5-0.6**: News articles, press releases, indirect mentions
- **0.3-0.4**: Unclear sources, old data, proxy metrics
- **0.0-0.2**: Speculative, unreliable, or contradictory data

### Professional Citation System
- **Parse Grounding Response**: Extract from response.text + groundingMetadata for each data point
- **Source Hierarchy**: Prioritize earnings reports → regulatory filings → analyst reports → news
- **Citation Format**: "[Company] [Metric]: [Value] ([Source Title], [Date])" 
- **Multi-Value Handling**: Capture all values found with individual confidence scores and sources
- **Provenance Chain**: Maintain search query → source URL → extracted value → confidence score

### Source Quality Assessment
- **High Quality (0.8-1.0)**: Earnings releases, SEC filings, regulatory submissions, audited reports
- **Medium Quality (0.5-0.7)**: Company press releases, analyst estimates, industry reports  
- **Low Quality (0.2-0.4)**: News articles, blogs, unverified sources
- **Flag for Review (<0.2)**: Contradictory sources, outdated data, speculation

## Data Normalization (Principles-Based)

### Currency Conversion
- **Source**: European Central Bank daily exchange rates (free, authoritative)
- **Principle**: "Use the most authoritative, accessible source"
- **Timing**: Use exchange rate from the data period (Q3 2024 data → Q3 2024 rate)
- **Fallback**: If ECB doesn't have the rate, flag as "unconverted" with original currency
- **Logging**: Always log which FX rate was used and when

### Period Alignment  
- **Preference Order**:
  1. **TTM (Trailing Twelve Months)**: Most comparable
  2. **Latest Full Year**: If TTM not available
  3. **Latest Quarter**: If annualized data not available
- **Normalization Notes**: Always document what period alignment was performed
- **Example**: "Q3 2024 data annualized based on 4x quarterly figure"

### Scope Alignment
- **Match Company Granularity**: Use the same geographic/business scope as the company's own reporting
- **Fallback Ladder**: 
  1. **Exact Match**: Same market cell scope
  2. **Segment Level**: Business segment data if market-specific not available
  3. **Geographic Level**: Country/region data if segment not available  
  4. **Consolidated**: Company-wide data as last resort
- **Flag Differences**: Always note when scope doesn't exactly match

### Comparability Classification
- **Exact**: Same metric, same period, same scope (highest confidence)
- **Adjusted**: Minor differences we can normalize (currency, time period) 
- **Proxy**: Related but different metric (e.g., "revenue per employee" when we want "productivity")
- **Principle**: "Transparency about uncertainty" - flag everything as what it actually is

### Data Quality Thresholds
- **Insufficient Data Criteria**:
  - No data found after 2 search attempts with broadening
  - Only proxy metrics available with low confidence
  - Data older than 24 months (flag as stale)
- **Action**: Mark clearly, continue analysis, note limitations in output

## Competitive Scoring & Analysis

### Scoring Framework
- **Anchor Strategy**: Default to best-in-class competitor as benchmark
- **Four Dimensions**:
  1. **Differentiation**: How different is company vs. anchor? (raw delta)
  2. **Impact**: How much does this metric affect value creation? (business materiality)
  3. **Confidence**: How reliable is this data? (source quality + recency + comparability)
  4. **Addressability**: Can this be improved within 12 months? (actionability)

### Value Impact Mapping
- **Margin Drivers**: Metrics that directly affect gross/EBITDA/net margins
- **ROCE Drivers**: Metrics affecting return on capital employed
- **Growth Drivers**: Metrics affecting revenue/customer/market growth
- **Scalability Drivers**: Metrics affecting operational leverage and efficiency

### Competitive Position Analysis
1. **Identify Top 3 Strengths**: Where company outperforms best-in-class
2. **Identify Top 3 Weaknesses**: Where company underperforms significantly  
3. **Calculate Gaps**: Quantify the performance differences
4. **Assess Materiality**: Focus on gaps that affect business prospects

## Strategy Bundler (Deep Analysis)

### Conflict Detection Logic
- **Resource Allocation Analysis**: Deep thinking about competing resource demands
- **Common Conflicts**:
  - **Cost Reduction vs. Innovation Investment**: Both compete for capex/opex budget
  - **Scale/Volume vs. Premium Positioning**: Market positioning conflicts
  - **Speed/Agility vs. Operational Efficiency**: Process optimization trade-offs
  - **Geographic Expansion vs. Market Deepening**: Strategic focus conflicts

### Strategy Bundle Generation
- **Output**: 2-3 viable strategy bundles per market cell
- **Components for Each Bundle**:
  - **Mechanism**: How to achieve the improvement
  - **Required Enablers**: What capabilities/resources needed
  - **Expected KPI Shifts**: Quantified impact ranges (conservative/optimistic)
  - **Risks**: What could go wrong
  - **Timeline**: 12-month actionability assessment
  - **Resource Requirements**: Capex, opex, people implications

### Conflict Resolution Suggestions
- **Prioritization Framework**: ROI timeline analysis
- **Sequencing Logic**: Address foundational issues first
- **Segmentation Strategy**: Different approaches for different markets
- **Resource Optimization**: Balanced automation vs. investment approaches

## Storage & Database Design

### SQLite Schema
- **companies**: Basic company information and metadata
- **market_cells**: Product×Geography×Customer combinations with materiality scores
- **competitors**: Discovered competitors with evidence quality scores
- **metrics**: Standardized metric definitions and capability mappings
- **observations**: Actual data points with full normalization provenance
- **sources**: Web sources with trust scores, caching info, and snapshots
- **comparisons**: Head-to-head competitive positioning analysis
- **strategies**: Generated strategy bundles with conflict analysis
- **cache**: Search query results with timestamps for 30-day caching

### Integration with PD2 Structure
- **Location**: `runs/run_YYYY_MM_DD_HH_MM_SS/competitive/`
- **Database File**: `competitive_analysis.db`
- **Reports**: `competitive_report.html` and `evidence_pack.json`
- **Source Cache**: `sources/` folder with cached web content

### Provenance Tracking
- **Search Queries**: Store exact queries used for each data point
- **Source URLs**: Full web source URLs and titles
- **Citation Spans**: Exact text spans that support each claim
- **Normalization Steps**: Document every adjustment made to raw data
- **Timestamps**: When each search was performed and data retrieved

## Output Generation

### HTML Report (Comprehensive)
- **Format**: Similar to PD2 company profile HTML output
- **Fonts**: Same typography and styling as PD2 reports
- **Length**: 20-30 pages comprehensive competitive intelligence report
- **Structure**:
  1. **Executive Summary**: Key competitive findings and strategic implications
  2. **Market Analysis**: Market cell definitions and competitive landscape
  3. **Competitive Positioning**: Detailed benchmarking by market cell
  4. **Strategic Recommendations**: Strategy bundles with conflict analysis
  5. **Data Appendix**: Detailed metrics tables with methodology notes
  6. **Footnotes**: All citations and sources at the very end (similar to PD2 format)

### JSON Evidence Pack
- **Purpose**: Structured data for PD2 integration
- **Contents**: 
  - Market cell definitions and competitors
  - Normalized competitive metrics with confidence scores
  - Strategic insights and recommendations
  - Mini-tables ready for injection into PD2 sections
- **Format**: Hierarchical JSON with clear metadata

### Database Export
- **Full Dataset**: Complete competitive intelligence database
- **Query Interface**: SQL access for custom analysis
- **Update Tracking**: Version history and change logs

## Error Handling & Recovery

### Soft Failures (Continue with Warnings)
- **Peer Discovery Fails**: Skip that market cell, log warning, continue with other cells
- **Metric Search Fails**: Try broader search terms once, then mark as "data not found"
- **Normalization Issues**: Flag as proxy data with lower confidence, continue analysis
- **Partial Data**: Continue with available data, clearly mark limitations

### Hard Failures (Stop and Report)
- **No Market Cells Found**: Review company definition and suggest manual input
- **No Competitors Found**: Check market definition, suggest broader geographic scope
- **Rate Limits Exceeded**: Wait and retry with longer intervals
- **Database Corruption**: Clear cache and restart analysis

### Recovery Strategies
- **Search Failures**: Try progressively broader queries (drop geography, then time period)
- **Data Quality Issues**: Accept lower-confidence proxy metrics if no exact matches
- **Timeout Issues**: Continue with partial results, note what was missed
- **Keep Simple**: Don't over-engineer recovery logic, fail gracefully with clear messages

## Integration with PD2 (Non-Disruptive)

### File Structure (No Refactoring)
```
PD2/
├── PD2.py                    # Add menu option + import competitive module  
├── src/
│   ├── [all existing files unchanged]
│   └── competitive/          # NEW: competitive analysis module
│       ├── __init__.py
│       ├── cli.py           # Standalone runner
│       ├── market_mapper.py  # Market cell discovery and consolidation
│       ├── peer_discovery.py # Competitor finding with grounding
│       ├── metric_engine.py  # Metric selection, normalization, scoring
│       ├── strategy_bundler.py # Strategy recommendations and conflict analysis
│       ├── database.py       # SQLite operations and caching
│       └── report_generator.py # HTML and JSON output generation
```

### Code Reuse Strategy (Import, Don't Extract)
- **Thread Safety**: `from src.utils import thread_safe_print, retry_with_backoff`
- **File Management**: `from src.file_manager import FileManager` 
- **LLM Utilities**: Reuse rate limiting and model configuration
- **HTML Generation**: Import and extend PD2's HTML styling and formatting
- **Zero Refactoring**: No changes to existing PD2 code

### PD2 Integration Points
1. **Startup Menu**: "Run competitive analysis first? (y/n)"
2. **Document Source**: Use same PDF/markdown files user provides to PD2
3. **Timing**: Start immediately when selected (during/before PDF conversion if possible)
4. **Section Enhancement**:
   - **Sections 13-18**: Automatic injection of competitive data and mini-tables
   - **Sections 1-12**: Selective competitive context where relevant
   - **Section 32**: Skip (no competitive data in appendix)

### Data Flow Integration
1. **PD2 File Processing**: Extract market information from same source documents
2. **Competitive Analysis**: Run full analysis pipeline
3. **Evidence Pack**: Generate JSON with mini-tables and insights
4. **Section Processing**: PD2 imports competitive data during section analysis
5. **HTML Generation**: Enhanced sections with competitive intelligence


## Implementation Strategy

### Development Phases
1. **Phase 1**: Market discovery + peer discovery (prove core concept)
2. **Phase 2**: Metric selection + data collection + normalization
3. **Phase 3**: Competitive scoring + strategy bundling + conflict analysis  
4. **Phase 4**: HTML report generation + PD2 integration hooks
5. **Phase 5**: Error handling + caching + polish

### MVP Definition
- **Full Workflow**: Complete end-to-end competitive analysis
- **Enhanced Features**: 
  - Structured company context extraction
  - AI-powered metric extraction with confidence scoring
  - Professional citation system with source hierarchy
  - Market discovery through strategy recommendations
- **Quality Focus**: Robust implementation of core principles
- **Integration Ready**: PD2 hooks from day one
- **Execution**: Sequential processing only (no parallel complexity)

### Success Criteria
- **Data Coverage**: >80% of target metrics found for major competitors
- **Source Quality**: >70% of data from primary/regulatory sources  
- **Execution Time**: <35 minutes for typical 3-market analysis
- **User Experience**: Fully automated with clear progress indicators and quality flags

This comprehensive specification captures all the critical decisions and principles from our detailed planning discussion, focusing on simplicity, reliability, and intelligent competitive analysis.
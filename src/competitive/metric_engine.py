"""
Metric Engine for Competitive Analysis.
Handles capability framework, metric selection, data collection, and normalization.
"""

import json
import re
import time
import requests
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
from ..utils import thread_safe_print, retry_with_backoff
from .database import CompetitiveDatabase
import google.generativeai as genai
from google.genai import types
from google import genai as genai_client


class MetricEngine:
    """Manages metric selection, data collection, and normalization"""
    
    # Capability framework for comprehensive coverage
    CAPABILITY_FAMILIES = {
        "cost_position": {
            "name": "Cost Position",
            "description": "Cost per unit, operational efficiency, labor productivity",
            "sample_metrics": ["cost per unit", "operational efficiency ratio", "labor productivity", "cost of goods sold margin"]
        },
        "scale_footprint": {
            "name": "Scale & Footprint", 
            "description": "Market share, customer base, geographic reach",
            "sample_metrics": ["market share", "customer base size", "geographic coverage", "revenue scale"]
        },
        "customer_economics": {
            "name": "Customer Economics",
            "description": "Revenue per customer, retention rates, acquisition costs", 
            "sample_metrics": ["revenue per customer", "customer retention rate", "customer acquisition cost", "customer lifetime value"]
        },
        "quality_coverage": {
            "name": "Quality & Coverage",
            "description": "Service quality metrics, network coverage, customer satisfaction",
            "sample_metrics": ["service quality score", "network coverage", "customer satisfaction", "product quality ratings"]
        },
        "asset_intensity": {
            "name": "Asset Intensity",
            "description": "Capex ratios, asset utilization, infrastructure efficiency",
            "sample_metrics": ["capex as % of revenue", "asset turnover", "infrastructure efficiency", "return on assets"]
        },
        "innovation_regulatory": {
            "name": "Innovation & Regulatory",
            "description": "R&D spend, patents, regulatory compliance, new product launches",
            "sample_metrics": ["R&D as % of revenue", "patent count", "new product launches", "regulatory compliance score"]
        }
    }
    
    def __init__(self, db: CompetitiveDatabase, model_name: str = "gemini-2.5-flash"):
        """Initialize with database and models"""
        self.db = db
        self.model_name = model_name
        # Use client+config for grounded calls; GenerativeModel for analysis
        self.client = genai_client.Client()
        self.analysis_model = genai.GenerativeModel(model_name)
        self.search_delay = 6.0
        self.last_search_time = 0
        
        # Initialize ECB exchange rates cache
        self.fx_rates_cache = {}
        self.fx_cache_date = None
    
    def _rate_limit_search(self):
        """Ensure minimum delay between searches"""
        current_time = time.time()
        time_since_last = current_time - self.last_search_time
        
        if time_since_last < self.search_delay:
            sleep_time = self.search_delay - time_since_last
            thread_safe_print(f"Rate limiting: waiting {sleep_time:.1f}s...")
            time.sleep(sleep_time)
        
        self.last_search_time = time.time()

    def _broaden_query(self, query: str, step: int) -> str:
        """Progressively broaden a search query by relaxing specificity.
        step 1: remove quarter qualifiers (e.g., Q1/Q2/Q3/Q4)
        step 2: remove explicit years (20xx)
        """
        broadened = query
        try:
            if step == 1:
                broadened = re.sub(r"\bQ[1-4]\b", "", broadened, flags=re.IGNORECASE)
                broadened = re.sub(r"\bQ[1-4]\s*20\d{2}\b", "", broadened, flags=re.IGNORECASE)
            elif step >= 2:
                broadened = re.sub(r"\b20\d{2}\b", "", broadened)
            # Collapse extra whitespace
            broadened = re.sub(r"\s+", " ", broadened).strip()
        except Exception:
            pass
        return broadened
    
    def select_metrics_for_market_cell(self, company_context: Dict[str, Any], 
                                     market_cell: Dict[str, Any],
                                     target_count: int = 10) -> List[Dict[str, Any]]:
        """
        Select optimal metrics for a market cell - focusing on data collection.
        Returns list of metric definitions for competitive data matrix.
        """
        thread_safe_print(f"Selecting metrics for market cell: {market_cell['product_service']} x {market_cell['geography']} x {market_cell['customer_segment']}")
        
        prompt = f"""Select exactly 10 metrics for side-by-side competitive data collection in this market:

COMPANY CONTEXT:
{json.dumps(company_context, indent=2)}

MARKET CELL: {market_cell['product_service']} × {market_cell['geography']} × {market_cell['customer_segment']}

Select 10 metrics following this distribution:
- Competitive Position (1-2): Market share, competitive ranking
- Scale/Operations (2-3): Subscribers/customers, coverage, network metrics, utilization
- Financial Performance (2-3): Revenue, EBITDA, EBITDA margin, profitability
- Efficiency Ratios (2-3): ARPU, Capex/Sales, productivity metrics, cost ratios  
- Growth/Momentum (1-2): Net adds, growth rates, churn

Choose metrics that:
1. Are commonly reported and findable via web search
2. Enable direct competitor comparison with numerical values
3. Cover diverse aspects (not just financials)
4. Are relevant to this specific market and industry

Return strictly valid JSON array with this exact format (no comments, no trailing commas):
[
    {{
        "name": "Market Share",
        "definition": "Percentage of total market revenue or subscribers",
        "unit_hint": "percentage",
        "search_keywords": ["market share", "market position", "subscriber share"]
    }}
]

Focus on metrics with numerical values that can populate a data table."""
        
        try:
            response_text = retry_with_backoff(
                lambda: self.analysis_model.generate_content(prompt).text
            )
            
            # Extract JSON array from response
            json_match = re.search(r'\[.*?\]', response_text, re.DOTALL)
            if json_match:
                metrics = json.loads(json_match.group())
                
                # Validate and clean metrics
                valid_metrics = []
                
                for metric in metrics:
                    if (isinstance(metric, dict) and 
                        all(key in metric for key in ['name', 'definition'])):
                        
                        # Ensure required fields for data collection
                        metric['unit_hint'] = metric.get('unit_hint', 'numeric')
                        metric['search_keywords'] = metric.get('search_keywords', [metric['name'].lower()])
                        
                        # Remove analysis-oriented fields - we just want data
                        metric.pop('capability_family', None)
                        metric.pop('directionality', None)
                        metric.pop('value_impact_category', None)
                        metric.pop('priority_score', None)
                        metric.pop('rationale', None)
                        
                        valid_metrics.append(metric)
                
                # Ensure we have exactly 10 metrics
                if len(valid_metrics) < 10:
                    # Add common fallback metrics to reach 10
                    fallbacks = self._get_fallback_metrics(market_cell)
                    for fb in fallbacks:
                        if len(valid_metrics) >= 10:
                            break
                        if not any(m['name'] == fb['name'] for m in valid_metrics):
                            valid_metrics.append(fb)
                
                thread_safe_print(f"Selected {len(valid_metrics)} metrics for data collection")
                return valid_metrics[:10]
                
            else:
                raise ValueError("No valid JSON array found in response")
                
        except Exception as e:
            thread_safe_print(f"Error selecting metrics: {e}")
            # Return fallback metrics
            return self._get_fallback_metrics(market_cell)
    
    def _get_fallback_metrics(self, market_cell: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Generate fallback metrics if LLM selection fails - 10 diverse metrics for data collection"""
        thread_safe_print("Using fallback metric selection...")
        
        # Universal metrics that apply across industries
        # These should be discovered based on what's actually reported in the industry
        fallback_metrics = [
            {"name": "Market Share", "definition": "Position in the market", "unit_hint": "percentage", "search_keywords": ["market share", "market position"]},
            {"name": "Revenue", "definition": "Total revenue or sales", "unit_hint": "currency", "search_keywords": ["revenue", "sales", "turnover"]},
            {"name": "Customer Base", "definition": "Number of customers or users", "unit_hint": "count", "search_keywords": ["customers", "users", "subscribers", "clients"]},
            {"name": "Operating Margin", "definition": "Operating profit margin", "unit_hint": "percentage", "search_keywords": ["EBITDA margin", "operating margin", "EBIT margin"]},
            {"name": "Growth Rate", "definition": "Year-over-year growth", "unit_hint": "percentage", "search_keywords": ["growth rate", "YoY growth", "annual growth"]},
            {"name": "Unit Economics", "definition": "Per-unit revenue or cost", "unit_hint": "currency", "search_keywords": ["ARPU", "unit revenue", "per customer", "per user"]},
            {"name": "Efficiency Ratio", "definition": "Key efficiency metric", "unit_hint": "percentage", "search_keywords": ["efficiency", "productivity", "utilization", "intensity"]},
            {"name": "Quality Metric", "definition": "Service or product quality", "unit_hint": "varies", "search_keywords": ["quality", "satisfaction", "NPS", "coverage", "availability"]},
            {"name": "Volume Metric", "definition": "Volume or usage measure", "unit_hint": "varies", "search_keywords": ["volume", "usage", "consumption", "transactions", "traffic"]},
            {"name": "Financial Returns", "definition": "Return metrics", "unit_hint": "percentage", "search_keywords": ["ROE", "ROA", "ROCE", "returns"]}
        ]
        
        return fallback_metrics
    
    def save_metrics_to_database(self, metrics: List[Dict[str, Any]]) -> List[int]:
        """Save metric definitions to database and return metric IDs"""
        metric_ids = []
        
        with self.db.get_connection() as conn:
            for metric in metrics:
                cursor = conn.execute("""
                    INSERT OR REPLACE INTO metrics 
                    (name, definition, capability_family, unit_hint, directionality, value_impact_category)
                    VALUES (?, ?, ?, ?, ?, ?)
                """, (
                    metric['name'],
                    metric['definition'], 
                    'data_collection',  # Default value since we're not using categories
                    metric.get('unit_hint', 'numeric'),
                    'neutral',  # No directionality for pure data collection
                    'data'  # Simple category
                ))
                metric_ids.append(cursor.lastrowid)
        
        thread_safe_print(f"Saved {len(metric_ids)} metrics to database")
        return metric_ids
    
    def generate_metric_search_query(self, company_context: Dict[str, Any],
                                   competitor_name: str, market_cell: Dict[str, Any],
                                   metric: Dict[str, Any]) -> str:
        """
        Generate intelligent search query for a specific metric.
        Uses company context and competitor info to create targeted queries.
        """
        prompt = f"""Generate a targeted search query for this competitive metric using recency without fixed years:

COMPANY CONTEXT: {json.dumps(company_context, indent=2)}
COMPETITOR: {competitor_name} in {market_cell['geography']}
MARKET CELL: {market_cell['product_service']} × {market_cell['geography']} × {market_cell['customer_segment']}
METRIC: {metric['name']} - {metric['definition']}

Create 1 optimized search query that will find this specific metric:
- Use exact competitor name and market geography
- Include metric-specific terminology
- Add CURRENT year or recent quarter for fresher results
- Use industry-specific language from company context
- Target high-quality sources (earnings, reports, filings)

Examples of good query patterns:
- "[Company] [Geography] [metric name] [recent quarter] earnings"
- "[Company] [Geography] market share [industry] [current year]"
- "[Company] [Geography] ARPU [recent quarter] quarterly report"
- "[Company] [Geography] revenue [current year] annual report"

Examples of poor queries:
- "Vodafone revenue" (no time period, too broad)
- "telecom market share 2021" (old data)

Return single optimized query only, no explanation needed."""
        
        try:
            response_text = retry_with_backoff(
                lambda: self.analysis_model.generate_content(prompt).text
            )
            
            # Clean up the response to get just the query
            query = response_text.strip().strip('"').strip("'")
            
            # Remove any extra text, keep just the search query
            lines = query.split('\n')
            query = lines[0].strip()
            
            if len(query) > 10:  # Basic validation
                return query
            else:
                # Fallback query construction
                keywords = ' '.join(metric.get('search_keywords', [metric['name'].lower()]))
                return f"{competitor_name} {market_cell['geography']} {keywords} latest"
                
        except Exception as e:
            thread_safe_print(f"Error generating search query: {e}")
            # Fallback query
            keywords = ' '.join(metric.get('search_keywords', [metric['name'].lower()]))
            return f"{competitor_name} {market_cell['geography']} {keywords} latest"
    
    def search_metric_grounded(self, search_query: str, company_name: str,
                              market_cell_key: str, metric_name: str) -> Optional[Dict[str, Any]]:
        """
        Execute grounded search for a specific metric with caching.
        Returns search results with grounding metadata.
        """
        # Check cache first
        cached_result = self.db.get_cached_search(
            query_text=search_query,
            company_name=company_name,
            market_cell=market_cell_key,
            metric_name=metric_name
        )
        
        if cached_result:
            thread_safe_print(f"✓ Using cached search for {metric_name}: {search_query[:50]}...")
            return {
                'response_text': cached_result['response_json'],
                'grounding_metadata': json.loads(cached_result['grounding_metadata_json'])
            }
        
        # Try up to 3 attempts: original + 2 broadened variants
        current_query = search_query
        for attempt in range(3):
            # Rate limit before new search
            self._rate_limit_search()
            thread_safe_print(f"Searching {metric_name}: {current_query}")

            try:
                grounding_tool = types.Tool(google_search=types.GoogleSearch())
                config = types.GenerateContentConfig(tools=[grounding_tool])
                response = retry_with_backoff(
                    lambda: self.client.models.generate_content(
                        model="gemini-2.5-flash",
                        contents=f"Find specific data for this metric search: {current_query}. Focus on numerical values, units, time periods, and source credibility.",
                        config=config,
                    )
                )

                # Extract grounding metadata
                grounding_metadata = {}
                if (hasattr(response, 'candidates') and 
                    response.candidates and 
                    hasattr(response.candidates[0], 'grounding_metadata')):

                    metadata = response.candidates[0].grounding_metadata
                    grounding_metadata = {
                        'web_search_queries': getattr(metadata, 'web_search_queries', []),
                        'grounding_chunks': [],
                        'grounding_supports': []
                    }

                    # Extract grounding chunks (sources)
                if metadata is not None and hasattr(metadata, 'grounding_chunks') and metadata.grounding_chunks:
                    for chunk in (metadata.grounding_chunks or []):
                            if hasattr(chunk, 'web'):
                                grounding_metadata['grounding_chunks'].append({
                                    'uri': chunk.web.uri,
                                    'title': chunk.web.title
                                })

                    # Extract grounding supports (citations)
                if metadata is not None and hasattr(metadata, 'grounding_supports') and metadata.grounding_supports:
                    for support in (metadata.grounding_supports or []):
                            grounding_metadata['grounding_supports'].append({
                                'segment_text': support.segment.text,
                                'start_index': support.segment.start_index,
                                'end_index': support.segment.end_index,
                                'chunk_indices': support.grounding_chunk_indices
                            })

                result = {
                    'response_text': response.text,
                    'grounding_metadata': grounding_metadata
                }

                # Cache the result
                self.db.cache_search_query(
                    query_text=current_query,
                    company_name=company_name,
                    market_cell=market_cell_key,
                    metric_name=metric_name,
                    response=response.text,
                    grounding_metadata=grounding_metadata
                )

                num_sources = len(grounding_metadata.get('grounding_chunks', []))
                thread_safe_print(f"Search completed with {num_sources} sources")
                if num_sources == 0 and attempt < 2:
                    # Broaden and retry
                    current_query = self._broaden_query(current_query, attempt + 1)
                    continue
                return result

            except Exception as e:
                thread_safe_print(f"Search failed: {e}")
                if attempt < 2:
                    current_query = self._broaden_query(current_query, attempt + 1)
                    continue
                return None
    
    def extract_metric_data(self, search_results: Dict[str, Any], 
                           competitor_name: str, metric: Dict[str, Any]) -> Dict[str, Any]:
        """
        Extract metric data from grounded search results using AI-powered analysis.
        Returns structured data with confidence scoring.
        """
        if not search_results:
            return {"metric_found": False, "values": [], "confidence": 0.0}
        
        prompt = f"""Extract competitive metric from this grounded search result:

SEARCH RESULTS: {search_results['response_text']}
COMPETITOR: {competitor_name}
METRIC SOUGHT: {metric['name']} - {metric['definition']}

Extract and structure this information as strictly valid JSON (no comments, no trailing commas):

IMPORTANT: For "value" field, extract ONLY the numeric part (e.g., "45.7" not "45.7%" or "USD 45.7")
For "units" field, specify the unit type: "percentage", "millions", "billions", "currency", "count", "GB", etc.

{{
  "metric_found": true,
  "values": [
    {{
      "value": "45.7",
      "units": "percentage",
      "period": "recent quarter",
      "scope": "Malaysia",
      "source_quality": "earnings_report",
      "confidence": 0.9,
      "extraction_notes": "EBITDA margin from latest quarterly report"
    }}
  ],
  "source_metadata": {{
    "source_title": "title from search results",
    "source_url": "URL if available", 
    "search_query": "original query used"
  }},
  "data_quality_flags": ["recent", "exact_match"]
}}

Confidence scoring guide (no code comments in JSON output):
- 0.9-1.0: earnings reports, regulatory filings, clear recent data
- 0.7-0.8: company disclosures, analyst reports
- 0.5-0.6: news articles, press releases
- 0.3-0.4: unclear sources, old data, proxy metrics
- 0.0-0.2: speculative or contradictory data

Extract ALL numerical values found, even if multiple or conflicting."""
        
        try:
            response_text = retry_with_backoff(
                lambda: self.analysis_model.generate_content(prompt).text
            )
            
            # Extract JSON from response (first JSON object best-effort)
            json_match = re.search(r'\{[\s\S]*\}', response_text)
            if json_match:
                extracted_data = json.loads(json_match.group())
                
                # Add grounding metadata if available
                if search_results.get('grounding_metadata'):
                    extracted_data['grounding_chunks'] = search_results['grounding_metadata'].get('grounding_chunks', [])
                
                thread_safe_print(f"Extracted {len(extracted_data.get('values', []))} values for {metric['name']}")
                return extracted_data
            else:
                raise ValueError("No valid JSON found in response")
                
        except Exception as e:
            thread_safe_print(f"Error extracting metric data: {e}")
            return {"metric_found": False, "values": [], "confidence": 0.0}
    
    def generate_batch_search_query(self, company_context: Dict[str, Any],
                                   competitor_name: str, market_cell: Dict[str, Any],
                                   metrics: List[Dict[str, Any]]) -> str:
        """Generate a single search query for multiple metrics at once."""
        
        # Build metric keywords list
        metric_keywords = []
        for metric in metrics[:10]:  # Limit to avoid query being too long
            if 'market share' in metric['name'].lower():
                metric_keywords.append('market share')
            elif 'revenue' in metric['name'].lower():
                metric_keywords.append('revenue')
            elif 'ebitda' in metric['name'].lower():
                metric_keywords.append('EBITDA margin')
            elif 'subscriber' in metric['name'].lower():
                metric_keywords.append('subscribers')
            elif 'arpu' in metric['name'].lower():
                metric_keywords.append('ARPU')
            elif 'capex' in metric['name'].lower():
                metric_keywords.append('capex')
            elif 'churn' in metric['name'].lower():
                metric_keywords.append('churn rate')
            elif 'coverage' in metric['name'].lower():
                metric_keywords.append('network coverage')
            elif 'data' in metric['name'].lower():
                metric_keywords.append('data usage')
            else:
                metric_keywords.append(metric['name'].lower())
        
        # Build query with company, geography, metrics, and time
        geography = market_cell.get('geography', '')
        current_year = datetime.now().year
        current_quarter = (datetime.now().month - 1) // 3 + 1
        
        # Format: "Company Geography metric1 metric2 metric3 YYYY quarterly report"
        query = f"{competitor_name} {geography} {' '.join(metric_keywords[:5])} {current_year} quarterly earnings report"
        
        return query
    
    def extract_batch_metrics(self, search_results: Dict[str, Any], 
                             competitor_name: str, metrics: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Extract multiple metrics from a single search result."""
        
        if not search_results:
            return [{"metric_found": False, "values": [], "confidence": 0.0, 
                    "metric_definition": m} for m in metrics]
        
        prompt = f"""Extract ALL the following competitive metrics from this search result:

SEARCH RESULTS: {search_results['response_text']}
COMPETITOR: {competitor_name}

METRICS TO FIND:
{json.dumps([{"name": m['name'], "definition": m['definition']} for m in metrics], indent=2)}

For EACH metric listed above, extract and structure the data. Return a JSON array with one object per metric.
Each object should have this structure:

[
  {{
    "metric_name": "Market Share",
    "metric_found": true,
    "values": [
      {{
        "value": "32.5",
        "units": "percentage",
        "period": "recent quarter",
        "scope": "Malaysia",
        "confidence": 0.9,
        "extraction_notes": "From quarterly earnings report"
      }}
    ]
  }},
  {{
    "metric_name": "Revenue",
    "metric_found": true,
    "values": [
      {{
        "value": "2610000000",
        "units": "currency",
        "period": "latest fiscal year",
        "scope": "Malaysia",
        "confidence": 0.85,
        "extraction_notes": "Annual revenue in MYR"
      }}
    ]
  }}
]

IMPORTANT: 
- Extract ONLY numeric values in the "value" field
- Include an entry for EVERY metric requested, even if not found (metric_found: false)
- Return strictly valid JSON array"""
        
        try:
            response_text = retry_with_backoff(
                lambda: self.analysis_model.generate_content(prompt).text
            )
            
            # Extract JSON array
            json_match = re.search(r'\[[\s\S]*\]', response_text)
            if json_match:
                extracted_metrics = json.loads(json_match.group())
                
                # Map extracted data back to metric definitions
                results = []
                for metric_def in metrics:
                    # Find matching extraction
                    found = False
                    for extracted in extracted_metrics:
                        if extracted.get('metric_name', '').lower() == metric_def['name'].lower():
                            extracted['metric_definition'] = metric_def
                            extracted['metric_found'] = extracted.get('metric_found', False)
                            if search_results.get('grounding_metadata'):
                                extracted['grounding_chunks'] = search_results['grounding_metadata'].get('grounding_chunks', [])
                            results.append(extracted)
                            found = True
                            break
                    
                    if not found:
                        # Metric not found in extraction
                        results.append({
                            "metric_name": metric_def['name'],
                            "metric_found": False,
                            "values": [],
                            "confidence": 0.0,
                            "metric_definition": metric_def
                        })
                
                return results
            else:
                raise ValueError("No valid JSON array found")
                
        except Exception as e:
            thread_safe_print(f"Error extracting batch metrics: {e}")
            return [{"metric_found": False, "values": [], "confidence": 0.0, 
                    "metric_definition": m} for m in metrics]
    
    def collect_metrics_for_competitor(self, company_context: Dict[str, Any],
                                     market_cell: Dict[str, Any],
                                     competitor: Dict[str, Any],
                                     metrics: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Collect all metrics for a single competitor using batch search.
        Returns list of collected metric data.
        """
        market_cell_key = f"{market_cell['product_service']}_{market_cell['geography']}_{market_cell['customer_segment']}"
        competitor_name = competitor['name']
        
        thread_safe_print(f"\nCollecting metrics for {competitor_name} in {market_cell_key} (batch mode)")
        
        # Generate single batch query for all metrics
        batch_query = self.generate_batch_search_query(
            company_context=company_context,
            competitor_name=competitor_name,
            market_cell=market_cell,
            metrics=metrics
        )
        
        thread_safe_print(f"  Batch search: {batch_query[:100]}...")
        
        # Execute single search for all metrics
        search_results = self.search_metric_grounded(
            search_query=batch_query,
            company_name=company_context['company_name'],
            market_cell_key=market_cell_key,
            metric_name="batch_metrics"
        )
        
        # Extract all metrics from the single search result
        extracted_metrics = self.extract_batch_metrics(
            search_results=search_results,
            competitor_name=competitor_name,
            metrics=metrics
        )
        
        # Check for suspicious values and retry if needed
        suspicious_count = 0
        for metric_data in extracted_metrics:
            if metric_data.get('metric_found') and metric_data.get('values'):
                for value_data in metric_data['values']:
                    # Quick validation check
                    metric_name = metric_data.get('metric_definition', {}).get('name', '')
                    raw_value = str(value_data.get('value', '0'))
                    units = value_data.get('units', '').lower()
                    
                    # Parse and scale value
                    numeric_str = re.sub(r'[^\d.-]', '', raw_value)
                    value = float(numeric_str) if numeric_str else 0
                    if 'trillion' in units:
                        value *= 1e12
                    elif 'billion' in units:
                        value *= 1e9
                    elif 'million' in units:
                        value *= 1e6
                    
                    # Principle: Use LLM judgment to detect implausible values
                    # Quick heuristic check first to avoid excessive LLM calls
                    # Only check values that seem extremely large
                    if value > 1e15:  # Quick filter for obviously huge numbers
                        suspicious_count += 1
                        thread_safe_print(f"  WARNING: Extremely large value detected: {value:.2e} for {competitor_name}")
                        break
        
        # If too many suspicious values, try a refined search
        if suspicious_count >= 3 and not search_results.get('is_refined_search'):
            thread_safe_print(f"  Detected {suspicious_count} suspicious values, attempting refined search...")
            refined_query = f'"{competitor_name}" official financial results investor relations annual report latest {market_cell["geography"]}'
            refined_results = self.search_metric_grounded(
                search_query=refined_query,
                company_name=company_context['company_name'],
                market_cell_key=market_cell_key,
                metric_name="batch_metrics_refined"
            )
            refined_results['is_refined_search'] = True  # Prevent infinite recursion
            
            # Re-extract with refined results
            refined_metrics = self.extract_batch_metrics(
                search_results=refined_results,
                competitor_name=competitor_name,
                metrics=metrics
            )
            
            # Use refined metrics if they look better
            refined_suspicious = sum(1 for m in refined_metrics 
                                   if m.get('metric_found') and any(
                                       float(re.sub(r'[^\d.-]', '', str(v.get('value', '0')))) > 1e12 
                                       for v in m.get('values', [])))
            if refined_suspicious < suspicious_count:
                thread_safe_print(f"  Using refined search results (reduced suspicious values from {suspicious_count} to {refined_suspicious})")
                extracted_metrics = refined_metrics
        
        # Process each extracted metric
        collected_data = []
        for extracted in extracted_metrics:
            extracted['competitor_info'] = competitor
            extracted['search_query'] = batch_query
            collected_data.append(extracted)
            
            if extracted.get('metric_found'):
                metric_name = extracted.get('metric_name', 'Unknown')
                values_count = len(extracted.get('values', []))
                thread_safe_print(f"    ✓ {metric_name}: {values_count} values found")
            else:
                metric_name = extracted.get('metric_name', 'Unknown')
                thread_safe_print(f"    ✗ {metric_name}: Not found")
        
        found_count = sum(1 for d in collected_data if d.get('metric_found'))
        thread_safe_print(f"  Batch complete: {found_count} of {len(metrics)} metrics found")
        return collected_data
    
    def collect_all_metrics(self, company_id: int) -> Dict[int, Dict[int, List[Dict[str, Any]]]]:
        """
        Collect metrics for all competitors across all market cells of a company.
        Returns nested dict: {market_cell_id: {competitor_id: [metric_data]}}
        """
        thread_safe_print(f"\nStarting metric collection for company ID: {company_id}")
        
        # Get company context
        company_row = self.db.get_connection().execute(
            "SELECT * FROM companies WHERE id = ?", (company_id,)
        ).fetchone()
        
        if not company_row:
            thread_safe_print(f"Company not found: {company_id}")
            return {}
        
        company_context = json.loads(company_row['context_json'])
        
        # Get market cells
        market_cells = self.db.get_market_cells_for_company(company_id)
        if not market_cells:
            thread_safe_print(f"No market cells found for company: {company_id}")
            return {}
        
        all_metric_data = {}
        
        for market_cell_row in market_cells:
            market_cell_id = market_cell_row['id']
            market_cell_dict = {
                'product_service': market_cell_row['product_service'],
                'geography': market_cell_row['geography'],
                'customer_segment': market_cell_row['customer_segment'],
                'materiality_score': market_cell_row['materiality_score']
            }
            
            thread_safe_print(f"\nProcessing market cell: {market_cell_dict['product_service']} x {market_cell_dict['geography']} x {market_cell_dict['customer_segment']}")
            
            # Select metrics for this market cell
            metrics = self.select_metrics_for_market_cell(
                company_context=company_context,
                market_cell=market_cell_dict,
                target_count=10
            )
            
            if not metrics:
                thread_safe_print(f"No metrics selected for market cell {market_cell_id}")
                continue
            
            # Save metrics to database
            metric_ids = self.save_metrics_to_database(metrics)
            
            # Ensure subject company competitor exists
            try:
                self.db.get_or_create_self_competitor(market_cell_id, company_context.get('company_name', 'Company'))
            except Exception:
                pass
            # Get competitors for this market cell
            competitors = self.db.get_competitors_for_market_cell(market_cell_id)
            if not competitors:
                thread_safe_print(f"No competitors found for market cell {market_cell_id}")
                continue
            
            # Collect metrics for each competitor
            market_cell_data = {}
            
            for competitor_row in competitors:
                competitor_dict = {
                    'id': competitor_row['id'],
                    'name': competitor_row['name'],
                    'parent_company': competitor_row['parent_company'],
                    'evidence_score': competitor_row['evidence_score']
                }
                
                metric_data = self.collect_metrics_for_competitor(
                    company_context=company_context,
                    market_cell=market_cell_dict,
                    competitor=competitor_dict,
                    metrics=metrics
                )
                
                market_cell_data[competitor_row['id']] = metric_data
            
            all_metric_data[market_cell_id] = market_cell_data
        
        # Clean up expired cache
        self.db.cleanup_expired_cache()
        
        thread_safe_print(f"\nMetric collection complete. Processed {len(all_metric_data)} market cells")
        return all_metric_data
    
    def _search_exchange_rate(self, from_currency: str, to_currency: str, date: str = None) -> Optional[float]:
        """
        Search for exchange rate using web search when APIs don't have the currency.
        Returns the exchange rate or None if not found.
        """
        try:
            # Rate limit before search
            import time
            time.sleep(2)
            
            # Generate search query
            if date:
                query = f"{from_currency} to {to_currency} exchange rate {date}"
            else:
                query = f"{from_currency} to {to_currency} exchange rate today"
            
            thread_safe_print(f"Searching for exchange rate: {query}")
            
            # Use grounded search via Google
            from google import genai as genai_client
            from google.genai import types
            
            client = genai_client.Client()
            grounding_tool = types.Tool(google_search=types.GoogleSearch())
            config = types.GenerateContentConfig(tools=[grounding_tool])
            
            prompt = f"""Find the current exchange rate for {from_currency} to {to_currency}.
Return ONLY the numeric exchange rate value (e.g., 4.65).
If the rate is not found, return 'NOT_FOUND'."""
            
            response = retry_with_backoff(
                lambda: client.models.generate_content(
                    model="gemini-2.5-flash",
                    contents=prompt,
                    config=config,
                )
            )
            
            # Parse the response for a numeric value
            import re
            text = response.text.strip()
            
            # Look for a number in the response
            match = re.search(r'(\d+\.?\d*)', text)
            if match:
                rate = float(match.group(1))
                thread_safe_print(f"Found exchange rate {from_currency} to {to_currency}: {rate}")
                return rate
            else:
                thread_safe_print(f"Could not parse exchange rate from response: {text}")
                return None
                
        except Exception as e:
            thread_safe_print(f"Error searching for exchange rate: {e}")
            return None
    
    def get_ecb_exchange_rates(self, date: str = None) -> Dict[str, float]:
        """
        Get exchange rates from European Central Bank.
        Uses authoritative, free source as specified in WEBSEARCH.md.
        """
        if date is None:
            date = datetime.now().strftime("%Y-%m-%d")
        
        # Check cache first (daily rates)
        if (self.fx_cache_date == date and self.fx_rates_cache):
            return self.fx_rates_cache
        
        try:
            # Use exchangerate.host with ECB source and optional historical date
            if date is None:
                url = "https://api.exchangerate.host/latest?base=EUR&source=ecb"
            else:
                url = f"https://api.exchangerate.host/{date}?base=EUR&source=ecb"
            response = requests.get(url, timeout=10)
            response.raise_for_status()
            
            data = response.json()
            rates = data.get('rates', {})
            
            # Add EUR as 1.0 (base currency)
            rates['EUR'] = 1.0
            
            # Cache the rates
            self.fx_rates_cache = rates
            self.fx_cache_date = date
            
            thread_safe_print(f"Retrieved exchange rates for {len(rates)} currencies")
            return rates
            
        except Exception as e:
            thread_safe_print(f"Error fetching ECB exchange rates: {e}")
            # Try alternative API for broader currency coverage
            try:
                # Use exchangerate-api.com which supports more currencies
                alt_url = f"https://api.exchangerate-api.com/v4/latest/USD"
                response = requests.get(alt_url, timeout=10)
                response.raise_for_status()
                
                data = response.json()
                rates_usd_base = data.get('rates', {})
                
                # Convert to EUR base to match ECB format
                if 'EUR' in rates_usd_base:
                    eur_to_usd = rates_usd_base['EUR']
                    rates = {}
                    for currency, rate in rates_usd_base.items():
                        # Convert from USD base to EUR base
                        rates[currency] = rate / eur_to_usd
                    
                    self.fx_rates_cache = rates
                    self.fx_cache_date = date
                    thread_safe_print(f"Retrieved {len(rates)} exchange rates from alternative source")
                    return rates
            except Exception as alt_e:
                thread_safe_print(f"Alternative API also failed: {alt_e}")
            
            # Return empty dict - will trigger web search for specific currencies as needed
            thread_safe_print("Exchange rate APIs unavailable, will search for rates as needed")
            return {}
    
    def normalize_currency(self, value: float, from_currency: str, 
                          to_currency: str = "USD", period: str = None) -> Tuple[float, str]:
        """
        Normalize currency using ECB rates from the appropriate period.
        Returns (normalized_value, fx_rate_info).
        """
        if from_currency.upper() == to_currency.upper():
            return value, f"No conversion needed ({from_currency})"
        
        try:
            # Get exchange rates (use period-specific date if provided)
            fx_date = None
            if period and re.search(r'20\d{2}', period):
                # Extract year from period and use end-of-year rate
                year_match = re.search(r'(20\d{2})', period)
                if year_match:
                    year = year_match.group(1)
                    fx_date = f"{year}-12-31"
            
            rates = self.get_ecb_exchange_rates(fx_date)
            
            from_rate = rates.get(from_currency.upper())
            to_rate = rates.get(to_currency.upper())
            
            # If rate not found, search for it using web search
            if from_rate is None and from_currency.upper() != 'EUR':
                from_rate = self._search_exchange_rate(from_currency, 'EUR', fx_date)
                if from_rate:
                    rates[from_currency.upper()] = from_rate
            
            if to_rate is None and to_currency.upper() != 'EUR':
                to_rate = self._search_exchange_rate(to_currency, 'EUR', fx_date)
                if to_rate:
                    rates[to_currency.upper()] = to_rate
            
            if from_rate is None or to_rate is None:
                raise ValueError(f"Exchange rate not available for {from_currency} or {to_currency}")
            
            # Convert via EUR base
            eur_value = value / from_rate
            converted_value = eur_value * to_rate
            
            fx_info = f"Converted {from_currency} to {to_currency} at rate {to_rate/from_rate:.4f}"
            if fx_date:
                fx_info += f" (period: {fx_date})"
            
            return converted_value, fx_info
            
        except Exception as e:
            thread_safe_print(f"Currency conversion failed: {e}")
            return value, f"Conversion failed, kept original {from_currency}"
    
    def normalize_period(self, value: float, period: str, target_period: str = "TTM") -> Tuple[float, str]:
        """
        Normalize time periods to make metrics comparable.
        Preference: TTM > Latest Full Year > Latest Quarter (annualized).
        """
        period_lower = period.lower()
        target_lower = target_period.lower()
        
        if target_lower in period_lower or period_lower in target_lower:
            return value, "No period adjustment needed"
        
        try:
            # Quarterly to annual conversion
            if any(q in period_lower for q in ['q1', 'q2', 'q3', 'q4', 'quarter']):
                if target_lower in ['ttm', 'annual', 'yearly']:
                    annualized_value = value * 4
                    return annualized_value, f"Annualized quarterly data (x4): {period} → {target_period}"
            
            # Monthly to annual conversion
            if any(m in period_lower for m in ['month', 'monthly']):
                if target_lower in ['ttm', 'annual', 'yearly']:
                    annualized_value = value * 12
                    return annualized_value, f"Annualized monthly data (x12): {period} → {target_period}"
            
            # No conversion possible
            return value, f"Period alignment not possible: {period} vs {target_period}"
            
        except Exception as e:
            thread_safe_print(f"Period normalization failed: {e}")
            return value, f"Period normalization failed, kept original period"
    
    def _validate_business_judgment(self, metric_name: str, value: float, 
                                   currency: str, units: str) -> Optional[str]:
        """
        Apply principle-based validation by asking the LLM to exercise judgment.
        No specific criteria - relies on contextual reasoning.
        """
        # Principle: Let the LLM determine if a value seems implausible in context
        # Rather than hardcoding rules, we ask for wise judgment
        
        validation_prompt = f"""Given this metric data, assess if the value seems plausible:
Metric: {metric_name}
Value: {value:,.2f}
Units: {units}
Currency indicator: {currency if currency else 'None'}

Apply these principles:
1. Does this value make sense given what this metric represents?
2. Could this be a data extraction or unit conversion error?
3. Is the order of magnitude reasonable for this type of measurement?

If the value seems implausible, respond with a brief reason (max 10 words).
If the value seems plausible, respond with exactly: "PLAUSIBLE"

Your response:"""

        try:
            # Use the LLM to make a judgment call
            response = retry_with_backoff(
                lambda: self.analysis_model.generate_content(
                    validation_prompt,
                    generation_config={"temperature": 0.1, "max_output_tokens": 50}
                ).text.strip()
            )
            
            if response.upper() == "PLAUSIBLE":
                return None
            else:
                return response[:100]  # Limit response length
                
        except Exception as e:
            # If validation fails, assume plausible to avoid blocking
            thread_safe_print(f"Validation check failed: {e}")
            return None
    
    def classify_comparability(self, metric_name: str, value_data: Dict[str, Any], 
                             target_metric: Dict[str, Any]) -> str:
        """
        Classify data comparability: Exact, Adjusted, or Proxy.
        Applies transparency principle about uncertainty.
        """
        try:
            # Check metric name similarity
            metric_similarity = metric_name.lower() in target_metric['name'].lower() or \
                              target_metric['name'].lower() in metric_name.lower()
            
            # Check period alignment (generic, non-year-specific examples)
            period_text = value_data.get('period', '')
            period_lower = period_text.lower()
            period_aligned = (
                'ttm' in period_lower or
                'fy' in period_lower or
                'annual' in period_lower or
                'year' in period_lower or
                bool(re.search(r'20\d{2}', period_text))
            )
            
            # Check scope alignment
            scope_notes = value_data.get('extraction_notes', '').lower()
            scope_aligned = any(scope_indicator in scope_notes 
                              for scope_indicator in ['segment', 'division', 'geographic'])
            
            # Classification logic
            if metric_similarity and period_aligned:
                return "exact"
            elif metric_similarity or (period_aligned and scope_aligned):
                return "adjusted"
            else:
                return "proxy"
                
        except Exception as e:
            thread_safe_print(f"Comparability classification failed: {e}")
            return "proxy"  # Conservative default
    
    def normalize_observation(self, metric_data: Dict[str, Any], 
                            target_currency: str = "USD") -> List[Dict[str, Any]]:
        """
        Normalize all values in a metric observation for comparability.
        Returns list of normalized observations.
        """
        if not metric_data.get('metric_found') or not metric_data.get('values'):
            return []
        
        normalized_observations = []
        metric_definition = metric_data.get('metric_definition', {})
        
        for value_data in metric_data['values']:
            try:
                # Parse the value properly
                raw_value = str(value_data.get('value', '0'))
                units = value_data.get('units', '').lower()
                period = value_data.get('period', '')
                
                # Extract numeric value, removing any currency symbols or commas
                numeric_str = re.sub(r'[^\d.-]', '', raw_value)
                original_value = float(numeric_str) if numeric_str else 0
                
                # Apply unit multipliers BEFORE currency conversion
                # This is critical for handling "billion IDR", "trillion IDR" etc.
                scaled_value = original_value
                if 'trillion' in units:
                    scaled_value = original_value * 1e12
                elif 'billion' in units or units == 'b':
                    scaled_value = original_value * 1e9
                elif 'million' in units or units == 'm':
                    scaled_value = original_value * 1e6
                elif 'thousand' in units or units == 'k':
                    scaled_value = original_value * 1e3
                
                # Principle: Detect currency by pattern recognition, not hardcoded lists
                # Currency codes typically follow ISO 4217: 3 uppercase letters
                # Some local abbreviations exist (e.g., RM for Malaysian Ringgit)
                currency = None
                
                # Check if units suggest currency
                if 'currency' in units or '$' in raw_value or '€' in raw_value or '£' in raw_value or '¥' in raw_value:
                    # Extract potential currency codes (3 uppercase letters)
                    currency_patterns = re.findall(r'\b[A-Z]{2,3}\b', raw_value.upper() + ' ' + units.upper())
                    if currency_patterns:
                        currency = currency_patterns[0]
                        # Normalize known aliases based on ISO standards
                        # RM is commonly used for Malaysian Ringgit (ISO: MYR)
                        if currency == 'RM':
                            currency = 'MYR'
                elif any(indicator in metric_definition.get('name', '').lower() 
                        for indicator in ['revenue', 'cost', 'profit', 'income', 'expense']):
                    # Financial metrics likely have currency even if not explicitly marked
                    # Try to extract any 3-letter code
                    currency_patterns = re.findall(r'\b[A-Z]{3}\b', raw_value.upper() + ' ' + units.upper())
                    if currency_patterns:
                        currency = currency_patterns[0]
                
                # Currency normalization only if it's actually currency
                if currency and currency != target_currency:
                    normalized_value, fx_info = self.normalize_currency(
                        scaled_value, currency, target_currency, period  # Use scaled_value, not original_value
                    )
                else:
                    normalized_value = scaled_value  # Use scaled_value even without currency conversion
                    fx_info = "No currency conversion needed"
                
                # Period normalization (if metric is flow-based)
                if metric_definition.get('value_impact_category') in ['growth', 'margin']:
                    normalized_value, period_info = self.normalize_period(
                        normalized_value, period, "TTM"
                    )
                else:
                    period_info = "No period adjustment (stock metric)"
                
                # Comparability classification
                comparability = self.classify_comparability(
                    metric_definition.get('name', ''),
                    value_data,
                    metric_definition
                )
                
                # Business judgment validation
                suspicious_value = self._validate_business_judgment(
                    metric_definition.get('name', ''),
                    normalized_value,
                    currency,
                    units
                )
                
                # Compile normalization notes
                normalization_notes = {
                    'original_value': original_value,
                    'scaled_value': scaled_value,
                    'original_currency': currency,
                    'original_period': period,
                    'fx_conversion': fx_info,
                    'period_adjustment': period_info,
                    'comparability_class': comparability,
                    'data_quality_flags': value_data.get('data_quality_flags', []),
                    'suspicious_value': suspicious_value
                }
                
                normalized_observation = {
                    'value': original_value,
                    'normalized_value': normalized_value,
                    'units': units,  # Keep original units (percentage, millions, etc.)
                    'normalized_currency': currency if currency else None,
                    'period': period,
                    'scope': value_data.get('scope', ''),
                    'comparability_class': comparability,
                    'confidence_score': value_data.get('confidence', 0.0) * (0.5 if suspicious_value else 1.0),  # Reduce confidence for suspicious values
                    'source_quality': value_data.get('source_quality', 'unknown'),
                    'extraction_notes': value_data.get('extraction_notes', ''),
                    'normalization_notes': json.dumps(normalization_notes)
                }
                
                if suspicious_value:
                    thread_safe_print(f"WARNING: Suspicious value detected for {metric_definition.get('name', '')}: {normalized_value} ({suspicious_value})")
                
                normalized_observations.append(normalized_observation)
                
            except Exception as e:
                thread_safe_print(f"Error normalizing observation: {e}")
                continue
        
        return normalized_observations
    
    def save_observations_to_database(self, market_cell_id: int, competitor_id: int,
                                    metric_id: int, normalized_observations: List[Dict[str, Any]]) -> List[int]:
        """Save normalized observations to database and return observation IDs"""
        observation_ids = []
        
        with self.db.get_connection() as conn:
            for obs in normalized_observations:
                cursor = conn.execute("""
                    INSERT INTO observations 
                    (market_cell_id, competitor_id, metric_id, value, units, period, scope, 
                     currency, normalized_value, comparability_class, confidence_score, normalization_notes)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (
                    market_cell_id, competitor_id, metric_id,
                    obs['value'], obs['units'], obs['period'], obs['scope'],
                    obs['normalized_currency'], obs['normalized_value'], 
                    obs['comparability_class'], obs['confidence_score'],
                    obs['normalization_notes']
                ))
                observation_ids.append(cursor.lastrowid)
        
        return observation_ids

    # Removed domain-based trust scoring to adhere to principles-based approach
    
    def process_and_save_all_metrics(self, company_id: int) -> Dict[str, Any]:
        """
        Complete Phase 2 pipeline: collect metrics, normalize data, save to database.
        Returns summary of processed data.
        """
        thread_safe_print(f"\nStarting Phase 2: Metric Collection & Normalization for company {company_id}")
        
        # Step 1: Collect all raw metric data
        all_metric_data = self.collect_all_metrics(company_id)
        
        if not all_metric_data:
            thread_safe_print("No metric data collected")
            return {"success": False, "error": "No metric data collected"}
        
        # Step 2: Process and normalize each metric observation
        processing_summary = {
            "market_cells_processed": 0,
            "competitors_processed": 0,
            "metrics_attempted": 0,
            "observations_saved": 0,
            "normalization_errors": 0
        }
        
        # Get metric definitions from database for IDs
        with self.db.get_connection() as conn:
            metric_rows = conn.execute("SELECT id, name FROM metrics ORDER BY id").fetchall()
            metric_name_to_id = {row['name']: row['id'] for row in metric_rows}
        
        for market_cell_id, competitors_data in all_metric_data.items():
            processing_summary["market_cells_processed"] += 1
            
            for competitor_id, metric_data_list in competitors_data.items():
                processing_summary["competitors_processed"] += 1
                
                for metric_data in metric_data_list:
                    processing_summary["metrics_attempted"] += 1
                    
                    try:
                        # Get metric ID
                        metric_name = metric_data.get('metric_definition', {}).get('name')
                        metric_id = metric_name_to_id.get(metric_name)
                        
                        if not metric_id:
                            thread_safe_print(f"Metric ID not found for: {metric_name}")
                            continue
                        
                        # Normalize the observations
                        normalized_observations = self.normalize_observation(metric_data)
                        
                        if normalized_observations:
                            # Save to database
                            observation_ids = self.save_observations_to_database(
                                market_cell_id=market_cell_id,
                                competitor_id=competitor_id,
                                metric_id=metric_id,
                                normalized_observations=normalized_observations
                            )
                            
                            processing_summary["observations_saved"] += len(observation_ids)
                            
                            thread_safe_print(f"Saved {len(observation_ids)} observations for {metric_name}")

                            # Provenance: upsert sources from grounding and link to each observation
                            try:
                                grounding_chunks = metric_data.get('grounding_chunks', [])
                                if grounding_chunks and observation_ids:
                                    for chunk in grounding_chunks:
                                        url = chunk.get('uri')
                                        title = chunk.get('title')
                                        source_id = self.db.upsert_source(url=url, title=title, trust_score=None)
                                        for obs_id in observation_ids:
                                            self.db.link_observation_to_source(observation_id=obs_id, source_id=source_id)
                            except Exception as link_err:
                                thread_safe_print(f"Error linking provenance: {link_err}")
                        
                    except Exception as e:
                        processing_summary["normalization_errors"] += 1
                        thread_safe_print(f"Error processing metric data: {e}")
                        continue
        
        # Step 3: Generate processing summary
        thread_safe_print(f"\nPhase 2 Complete - Processing Summary:")
        thread_safe_print(f"  Market Cells: {processing_summary['market_cells_processed']}")
        thread_safe_print(f"  Competitors: {processing_summary['competitors_processed']}")
        thread_safe_print(f"  Metrics Attempted: {processing_summary['metrics_attempted']}")
        thread_safe_print(f"  Observations Saved: {processing_summary['observations_saved']}")
        thread_safe_print(f"  Normalization Errors: {processing_summary['normalization_errors']}")
        
        # Calculate success rate
        success_rate = processing_summary["observations_saved"] / max(1, processing_summary["metrics_attempted"])
        thread_safe_print(f"  Success Rate: {success_rate:.1%}")
        
        return {
            "success": True,
            "summary": processing_summary,
            "success_rate": success_rate,
            "raw_data": all_metric_data
        }
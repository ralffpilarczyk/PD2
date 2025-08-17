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
        # Use 2.5-flash for grounding, user's model for analysis
        self.grounding_model = genai.GenerativeModel("gemini-2.5-flash")
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
    
    def select_metrics_for_market_cell(self, company_context: Dict[str, Any], 
                                     market_cell: Dict[str, Any],
                                     target_count: int = 8) -> List[Dict[str, Any]]:
        """
        Select optimal metrics for a market cell using capability framework.
        Returns list of metric definitions with capability mappings.
        """
        thread_safe_print(f"Selecting metrics for market cell: {market_cell['product_service']} x {market_cell['geography']} x {market_cell['customer_segment']}")
        
        prompt = f"""Select optimal competitive metrics for this market cell using the capability framework:

COMPANY CONTEXT:
{json.dumps(company_context, indent=2)}

MARKET CELL: {market_cell['product_service']} × {market_cell['geography']} × {market_cell['customer_segment']}

CAPABILITY FRAMEWORK (must cover these families):
{json.dumps(self.CAPABILITY_FAMILIES, indent=2)}

Select {target_count} metrics that:
1. Cover all 6 capability families (at least 1 metric per family)
2. Are most relevant to value creation in this specific market
3. Are realistic to find through web search (avoid overly obscure metrics)
4. Focus on metrics where competitive battles are won/lost
5. Balance financial, operational, and strategic dimensions

Return JSON array with this exact format:
[
    {
        "name": "Market Share",
        "definition": "Percentage of total market revenue captured by the company",
        "capability_family": "scale_footprint",
        "unit_hint": "percentage", 
        "directionality": "higher_better",
        "value_impact_category": "growth",
        "priority_score": 0.95,
        "search_keywords": ["market share", "market position", "competitive position"],
        "rationale": "Critical metric for understanding competitive position and scale advantages"
    }
]

Directionality options: "higher_better", "lower_better", "neutral"
Value impact categories: "margin", "roce", "growth", "scalability"
Priority score: 0.0-1.0 based on importance to value creation

Focus on metrics that executives care about and that drive business value."""
        
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
                capability_coverage = set()
                
                for metric in metrics:
                    if (isinstance(metric, dict) and 
                        all(key in metric for key in ['name', 'definition', 'capability_family']) and
                        metric['capability_family'] in self.CAPABILITY_FAMILIES):
                        
                        # Ensure required fields
                        metric['unit_hint'] = metric.get('unit_hint', 'numeric')
                        metric['directionality'] = metric.get('directionality', 'higher_better')
                        metric['value_impact_category'] = metric.get('value_impact_category', 'growth')
                        metric['priority_score'] = max(0.0, min(1.0, metric.get('priority_score', 0.5)))
                        metric['search_keywords'] = metric.get('search_keywords', [metric['name'].lower()])
                        metric['rationale'] = metric.get('rationale', 'Selected for competitive analysis')
                        
                        valid_metrics.append(metric)
                        capability_coverage.add(metric['capability_family'])
                
                # Check capability coverage and backfill to enforce coverage
                missing_capabilities = list(set(self.CAPABILITY_FAMILIES.keys()) - capability_coverage)
                if missing_capabilities:
                    thread_safe_print(f"Missing capability coverage: {missing_capabilities}")
                    for family_key in missing_capabilities:
                        samples = self.CAPABILITY_FAMILIES[family_key].get('sample_metrics', [])
                        if samples:
                            name = samples[0].title()
                            valid_metrics.append({
                                "name": name,
                                "definition": samples[0],
                                "capability_family": family_key,
                                "unit_hint": "numeric",
                                "directionality": "higher_better",
                                "value_impact_category": "growth",
                                "priority_score": 0.5,
                                "search_keywords": [samples[0]],
                                "rationale": "Backfilled to ensure capability coverage"
                            })
                
                # Sort by priority score
                valid_metrics.sort(key=lambda x: x['priority_score'], reverse=True)
                
                thread_safe_print(f"Selected {len(valid_metrics)} metrics covering {len(capability_coverage)}/6 capabilities")
                return valid_metrics[:target_count]
                
            else:
                raise ValueError("No valid JSON array found in response")
                
        except Exception as e:
            thread_safe_print(f"Error selecting metrics: {e}")
            # Return fallback metrics
            return self._get_fallback_metrics(market_cell)
    
    def _get_fallback_metrics(self, market_cell: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Generate fallback metrics if LLM selection fails"""
        thread_safe_print("Using fallback metric selection...")
        
        fallback_metrics = [
            {
                "name": "Market Share",
                "definition": "Percentage of total market revenue",
                "capability_family": "scale_footprint", 
                "unit_hint": "percentage",
                "directionality": "higher_better",
                "value_impact_category": "growth",
                "priority_score": 0.95,
                "search_keywords": ["market share", "market position"],
                "rationale": "Fundamental competitive metric"
            },
            {
                "name": "Revenue",
                "definition": "Total revenue in the market segment", 
                "capability_family": "scale_footprint",
                "unit_hint": "currency",
                "directionality": "higher_better", 
                "value_impact_category": "growth",
                "priority_score": 0.90,
                "search_keywords": ["revenue", "sales"],
                "rationale": "Scale indicator"
            },
            {
                "name": "Customer Base",
                "definition": "Number of active customers",
                "capability_family": "customer_economics",
                "unit_hint": "count", 
                "directionality": "higher_better",
                "value_impact_category": "growth",
                "priority_score": 0.85,
                "search_keywords": ["customers", "user base"],
                "rationale": "Customer reach metric"
            }
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
                    metric['capability_family'],
                    metric['unit_hint'],
                    metric['directionality'],
                    metric['value_impact_category']
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
        prompt = f"""Generate targeted search query for this competitive metric:

COMPANY CONTEXT: {json.dumps(company_context, indent=2)}
COMPETITOR: {competitor_name} in {market_cell['geography']}
MARKET CELL: {market_cell['product_service']} × {market_cell['geography']} × {market_cell['customer_segment']}
METRIC: {metric['name']} - {metric['definition']}

Create 1 optimized search query that will find this specific metric:
- Use exact competitor name and market geography
- Include metric-specific terminology
- Add recent time qualifiers (2024, Q3 2024, latest)
- Use industry-specific language from company context
- Target high-quality sources (earnings, reports, filings)

Examples of good queries:
- "Vodafone Germany mobile subscriber numbers Q3 2024 earnings"
- "Deutsche Telekom Germany market share telecommunications 2024"
- "O2 Germany ARPU average revenue per user latest quarterly"

Examples of poor queries:
- "Vodafone revenue" (too broad)
- "telecom market share" (no specific company/market)

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
                return f"{competitor_name} {market_cell['geography']} {keywords} 2024"
                
        except Exception as e:
            thread_safe_print(f"Error generating search query: {e}")
            # Fallback query
            keywords = ' '.join(metric.get('search_keywords', [metric['name'].lower()]))
            return f"{competitor_name} {market_cell['geography']} {keywords} 2024"
    
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
                response = retry_with_backoff(
                    lambda: self.grounding_model.generate_content(
                        f"Find specific data for this metric search: {current_query}. Focus on numerical values, units, time periods, and source credibility.",
                        tools=[{"google_search": {}}]
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
                    if hasattr(metadata, 'grounding_chunks'):
                        for chunk in metadata.grounding_chunks:
                            if hasattr(chunk, 'web'):
                                grounding_metadata['grounding_chunks'].append({
                                    'uri': chunk.web.uri,
                                    'title': chunk.web.title
                                })

                    # Extract grounding supports (citations)
                    if hasattr(metadata, 'grounding_supports'):
                        for support in metadata.grounding_supports:
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

Extract and structure this information:

{{
    "metric_found": true/false,
    "values": [
        {{
            "value": "extracted numeric value",
            "units": "currency/users/percent/etc",
            "period": "Q3 2024/2024/TTM/etc", 
            "scope": "geographic/business scope",
            "source_quality": "earnings_report/regulatory_filing/news/analyst",
            "confidence": 0.9, // 0.0-1.0 based on source quality and clarity
            "extraction_notes": "context about the metric"
        }}
    ],
    "source_metadata": {{
        "source_title": "title from search results",
        "source_url": "URL if available", 
        "search_query": "original query used"
    }},
    "data_quality_flags": ["recent/outdated", "exact_match/proxy_metric", "clear/ambiguous"]
}}

Confidence scoring guide:
- 0.9-1.0: Earnings reports, regulatory filings, clear recent data
- 0.7-0.8: Company disclosures, analyst reports, clear methodology  
- 0.5-0.6: News articles, press releases, indirect mentions
- 0.3-0.4: Unclear sources, old data, proxy metrics
- 0.0-0.2: Speculative, unreliable, or contradictory data

Extract ALL numerical values found, even if multiple or conflicting."""
        
        try:
            response_text = retry_with_backoff(
                lambda: self.analysis_model.generate_content(prompt).text
            )
            
            # Extract JSON from response
            json_match = re.search(r'\{.*?\}', response_text, re.DOTALL)
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
    
    def collect_metrics_for_competitor(self, company_context: Dict[str, Any],
                                     market_cell: Dict[str, Any],
                                     competitor: Dict[str, Any],
                                     metrics: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Collect all metrics for a single competitor in a market cell.
        Returns list of collected metric data.
        """
        market_cell_key = f"{market_cell['product_service']}_{market_cell['geography']}_{market_cell['customer_segment']}"
        competitor_name = competitor['name']
        
        thread_safe_print(f"\nCollecting metrics for {competitor_name} in {market_cell_key}")
        
        collected_data = []
        
        for metric in metrics:
            thread_safe_print(f"  Searching: {metric['name']}")
            
            # Generate search query
            search_query = self.generate_metric_search_query(
                company_context=company_context,
                competitor_name=competitor_name,
                market_cell=market_cell,
                metric=metric
            )
            
            # Execute search
            search_results = self.search_metric_grounded(
                search_query=search_query,
                company_name=company_context['company_name'],
                market_cell_key=market_cell_key,
                metric_name=metric['name']
            )
            
            # Extract data
            extracted_data = self.extract_metric_data(
                search_results=search_results,
                competitor_name=competitor_name,
                metric=metric
            )
            
            # Add metadata
            extracted_data['metric_definition'] = metric
            extracted_data['competitor_info'] = competitor
            extracted_data['search_query'] = search_query
            
            collected_data.append(extracted_data)
            
            # Log result
            if extracted_data.get('metric_found'):
                values_count = len(extracted_data.get('values', []))
                avg_confidence = sum(v.get('confidence', 0) for v in extracted_data.get('values', [])) / max(1, values_count)
                thread_safe_print(f"    Found {values_count} values (avg confidence: {avg_confidence:.2f})")
            else:
                thread_safe_print(f"    No data found")
        
        thread_safe_print(f"Collected metrics for {competitor_name}: {sum(1 for d in collected_data if d.get('metric_found'))} of {len(metrics)} found")
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
                target_count=8
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
            thread_safe_print(f"Error fetching exchange rates: {e}")
            # Return fallback rates (major currencies only)
            return {
                'EUR': 1.0,
                'USD': 1.10,
                'GBP': 0.85,
                'JPY': 130.0,
                'CNY': 7.8,
                'CHF': 0.95
            }
    
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
            
            # Check period alignment
            period_aligned = any(period_indicator in value_data.get('period', '').lower() 
                               for period_indicator in ['ttm', '2024', '2023', 'annual', 'yearly'])
            
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
                original_value = float(value_data.get('value', 0))
                currency = value_data.get('units', '').upper()
                period = value_data.get('period', '')
                
                # Currency normalization
                if currency in ['USD', 'EUR', 'GBP', 'JPY', 'CNY', 'CHF']:
                    normalized_value, fx_info = self.normalize_currency(
                        original_value, currency, target_currency, period
                    )
                else:
                    normalized_value = original_value
                    fx_info = f"No currency conversion (units: {currency})"
                
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
                
                # Compile normalization notes
                normalization_notes = {
                    'original_value': original_value,
                    'original_currency': currency,
                    'original_period': period,
                    'fx_conversion': fx_info,
                    'period_adjustment': period_info,
                    'comparability_class': comparability,
                    'data_quality_flags': value_data.get('data_quality_flags', [])
                }
                
                normalized_observation = {
                    'value': original_value,
                    'normalized_value': normalized_value,
                    'units': currency,
                    'normalized_currency': target_currency,
                    'period': period,
                    'scope': value_data.get('scope', ''),
                    'comparability_class': comparability,
                    'confidence_score': value_data.get('confidence', 0.0),
                    'source_quality': value_data.get('source_quality', 'unknown'),
                    'extraction_notes': value_data.get('extraction_notes', ''),
                    'normalization_notes': json.dumps(normalization_notes)
                }
                
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
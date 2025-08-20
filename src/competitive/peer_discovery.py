"""
Peer Discovery using Grounded Search.
Finds direct competitors for each market cell using intelligent query generation.
"""

import json
import re
import time
from typing import Dict, List, Optional, Tuple, Any
from ..utils import thread_safe_print, retry_with_backoff
from .database import CompetitiveDatabase
import google.generativeai as gga
from google import genai as genai_client
from google.genai import types
from .entity_resolution import resolve_entities


class PeerDiscovery:
    """Discovers competitors using grounded web search"""
    
    def __init__(self, db: CompetitiveDatabase, model_name: str = "gemini-2.5-flash"):
        """Initialize with database and grounding-capable model"""
        self.db = db
        self.model_name = model_name
        # Client for grounded Google Search calls
        self.client = genai_client.Client()
        # Use GenerativeModel for non-grounded analysis prompts
        self.analysis_model = gga.GenerativeModel(model_name)
        self.search_delay = 6.0  # 6 second minimum between searches
        self.last_search_time = 0
    
    def _rate_limit_search(self):
        """Ensure minimum delay between searches to respect rate limits"""
        current_time = time.time()
        time_since_last = current_time - self.last_search_time
        
        if time_since_last < self.search_delay:
            sleep_time = self.search_delay - time_since_last
            thread_safe_print(f"Rate limiting: waiting {sleep_time:.1f}s...")
            time.sleep(sleep_time)
        
        self.last_search_time = time.time()
    
    def generate_competitor_search_queries(self, company_context: Dict[str, Any], 
                                         business_segment: Dict[str, Any]) -> List[str]:
        """
        Generate segment-aware search queries for finding competitors.
        Uses actual business segment information to create targeted searches.
        """
        segment_name = business_segment.get('segment_name', 'Unknown Segment')
        thread_safe_print(f"Generating search queries for segment: {segment_name}")
        
        # Check if this is a holding company with subsidiaries
        company_name = company_context.get('company_name', '')
        is_holding_company = any(term in company_name.lower() for term in ['group', 'berhad', 'holdings', 'plc'])
        
        if is_holding_company:
            # For holding companies, also search for subsidiaries that operate in this market
            prompt = f"""Based on this company context, generate search queries for BOTH subsidiaries AND competitors:

COMPANY CONTEXT:
{json.dumps(company_context, indent=2)}

BUSINESS SEGMENT: {business_segment['segment_name']}
Description: {business_segment.get('description', '')}
Geographic Focus: {business_segment.get('geographic_focus', '')}
Products/Services: {business_segment.get('products_services', '')}

This appears to be a holding company. Generate 3-4 search queries that will find:
1. The company's own operating subsidiaries in this segment
2. Direct competitors to those subsidiaries

Use the segment details to create specific searches:
- Search for companies in the same segment/business area
- Include geographic focus if specified
- Use product/service descriptions from the segment

Return JSON array of 3-4 optimized search queries:
["query 1", "query 2", "query 3", "query 4"]"""
        else:
            prompt = f"""Based on this company context, generate targeted search queries for direct competitors in the specified market cell:

COMPANY CONTEXT:
{json.dumps(company_context, indent=2)}

BUSINESS SEGMENT: {business_segment['segment_name']}
Description: {business_segment.get('description', '')}
Geographic Focus: {business_segment.get('geographic_focus', '')}
Products/Services: {business_segment.get('products_services', '')}
Revenue Contribution: {business_segment.get('revenue_contribution', '')}

Generate 2-3 highly specific search queries that will find direct competitors:
- Search for companies operating in the EXACT same segment
- Include geographic focus from the segment details
- Use product/service descriptions to find similar companies
- Focus on companies of similar scale (use revenue contribution as guide)
- Add recent time qualifiers for current competitors

Examples of good query patterns:
- "[Geography] [industry] competitors market leaders [current year]"
- "[Geography] [service type] market share top players [current year]"
- "[Geography] [industry] operators comparison [current year]"

Return JSON array of 2-3 optimized search queries:
["query 1", "query 2", "query 3"]

Focus on finding companies that compete directly in this specific market cell."""
        
        try:
            response_text = retry_with_backoff(
                lambda: self.analysis_model.generate_content(prompt).text
            )
            
            # Extract JSON array from response
            json_match = re.search(r'\[.*?\]', response_text, re.DOTALL)
            if json_match:
                queries = json.loads(json_match.group())
                
                # Validate and clean queries
                valid_queries = []
                for query in queries:
                    if isinstance(query, str) and len(query.strip()) > 10:
                        valid_queries.append(query.strip())
                
                if valid_queries:
                    thread_safe_print(f"Generated {len(valid_queries)} search queries")
                    return valid_queries
                else:
                    raise ValueError("No valid queries generated")
                    
            else:
                raise ValueError("No valid JSON array found in response")
                
        except Exception as e:
            thread_safe_print(f"Error generating search queries: {e}")
            # Return fallback queries using business_segment
            company_name = company_context.get('company_name', 'company')
            industry = company_context.get('industry', 'industry')
            geography = business_segment.get('geographic_focus', 'market')
            segment_name = business_segment.get('segment_name', '')
            
            return [
                f"{segment_name} competitors {geography} {industry} latest",
                f"{industry} companies {geography} competition latest",
                f"leading {industry} players {geography} market share recent"
            ]
    
    def search_competitors_grounded(self, search_query: str, company_name: str,
                                  market_cell_key: str) -> Optional[Dict[str, Any]]:
        """
        Execute grounded search for competitors with caching.
        Returns search results with grounding metadata.
        """
        # Check cache first
        cached_result = self.db.get_cached_search(
            query_text=search_query,
            company_name=company_name,
            market_cell=market_cell_key,
            metric_name="competitor_discovery"
        )
        
        if cached_result:
            thread_safe_print(f"Using cached search result for: {search_query[:50]}...")
            return {
                'response_text': cached_result['response_json'],
                'grounding_metadata': json.loads(cached_result['grounding_metadata_json'])
            }
        
        # Rate limit before new search
        self._rate_limit_search()
        
        thread_safe_print(f"Searching: {search_query}")
        
        try:
            grounding_tool = types.Tool(google_search=types.GoogleSearch())
            config = types.GenerateContentConfig(tools=[grounding_tool])
            response = retry_with_backoff(
                lambda: self.client.models.generate_content(
                    model="gemini-2.5-flash",
                    contents=f"Find direct competitors for this search: {search_query}. Focus on company names, market presence evidence, and competitive positioning.",
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
                query_text=search_query,
                company_name=company_name,
                market_cell=market_cell_key,
                metric_name="competitor_discovery",
                response=response.text,
                grounding_metadata=grounding_metadata
            )
            
            thread_safe_print(f"Search completed with {len(grounding_metadata.get('grounding_chunks', []))} sources")
            return result
            
        except Exception as e:
            thread_safe_print(f"Search failed: {e}")
            return None
    
    def extract_competitors_from_search(self, search_results: List[Dict[str, Any]], 
                                      company_context: Dict[str, Any],
                                      business_segment: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        Extract competitor information from grounded search results.
        Returns list of competitors with evidence scores.
        """
        thread_safe_print("Extracting competitors from search results...")
        
        # Combine all search results
        combined_text = ""
        all_sources = []
        
        for result in search_results:
            if result:
                combined_text += f"\n\nSEARCH RESULT:\n{result['response_text']}\n"
                sources = result['grounding_metadata'].get('grounding_chunks', [])
                all_sources.extend(sources)
        
        if not combined_text.strip():
            thread_safe_print("No search results to process")
            return []
        
        # Check if analyzing a holding company
        company_name = company_context.get('company_name', 'Unknown')
        is_holding_company = any(term in company_name.lower() for term in ['group', 'berhad', 'holdings', 'plc'])
        
        if is_holding_company:
            # For holding companies, include subsidiaries
            prompt = f"""Extract BOTH the company's subsidiaries AND their competitors from search results:

PARENT COMPANY: {company_name}
Industry: {company_context.get('industry', 'Unknown')}

BUSINESS SEGMENT: {business_segment.get('segment_name', 'Unknown')}
Geographic Focus: {business_segment.get('geographic_focus', 'Unknown')}
Products/Services: {business_segment.get('products_services', 'Unknown')}

SEARCH RESULTS:
{combined_text}

Instructions:
1. First identify any subsidiary of {company_name} operating in {business_segment.get('geographic_focus', 'this market')}
2. Then identify direct competitors to that subsidiary

Return JSON array - PUT THE SUBSIDIARY FIRST if found:
[
    {{
        "name": "Operating Subsidiary Name",
        "parent_company": "{company_name}",
        "evidence_score": 1.0,
        "presence_evidence": "Description of subsidiary relationship and market presence",
        "market_position": "market position based on evidence",
        "source_quality": "high/medium/low"
    }},
    {{
        "name": "Competitor Name",
        "parent_company": "Parent if applicable",
        "evidence_score": 0.85,
        "presence_evidence": "Evidence of competition in this market",
        "market_position": "market position",
        "source_quality": "high/medium/low"
    }}
]"""
        else:
            # For regular companies, standard competitor extraction
            prompt = f"""Extract direct competitors from these search results for the specified market cell:

COMPANY CONTEXT:
Company: {company_name}
Industry: {company_context.get('industry', 'Unknown')}
Business Model: {company_context.get('business_model', 'Unknown')}

BUSINESS SEGMENT: {business_segment.get('segment_name', 'Unknown')}
Geographic Focus: {business_segment.get('geographic_focus', 'Unknown')}
Products/Services: {business_segment.get('products_services', 'Unknown')}

SEARCH RESULTS:
{combined_text}

Extract direct competitors that compete in this specific business segment. Apply these criteria:
- Must be mentioned as competitors, rivals, or market players
- Must operate in the same geography and customer segment
- Must offer similar products/services
- Exclude the target company itself
- Focus on evidence quality and market presence

Return JSON array with this exact format:
[
    {{
        "name": "Competitor Company Name",
        "parent_company": "Parent if subsidiary, else same as name",
        "evidence_score": 0.85,
        "presence_evidence": "brief description of evidence found",
        "market_position": "market leader/challenger/niche player",
        "source_quality": "high/medium/low based on source credibility"
- 0.9-1.0: Multiple high-quality sources, clear market presence evidence
- 0.7-0.8: Good sources, solid evidence of competition
- 0.5-0.6: Moderate evidence, some competitive mentions
- 0.3-0.4: Weak evidence, limited competitive context
- 0.0-0.2: Very weak or speculative evidence

Focus on finding 3-7 direct competitors with the strongest evidence."""
        
        try:
            response_text = retry_with_backoff(
                lambda: self.analysis_model.generate_content(prompt).text
            )
            
            # Extract JSON array from response
            json_match = re.search(r'\[.*?\]', response_text, re.DOTALL)
            if json_match:
                competitors = json.loads(json_match.group())
                
                # Validate and clean competitors
                valid_competitors = []
                for comp in competitors:
                    if (isinstance(comp, dict) and 
                        comp.get('name') and 
                        comp.get('name').strip() and
                        comp.get('name').lower() != company_context.get('company_name', '').lower()):
                        
                        # Ensure required fields
                        comp['parent_company'] = comp.get('parent_company') or comp['name']
                        comp['evidence_score'] = max(0.0, min(1.0, comp.get('evidence_score', 0.5)))
                        comp['presence_evidence'] = comp.get('presence_evidence', 'Mentioned in search results')
                        comp['market_position'] = comp.get('market_position', 'Unknown')
                        comp['source_quality'] = comp.get('source_quality', 'medium')
                        
                        valid_competitors.append(comp)

                # Resolve entities (subsidiaries/brands/JVs → parent) per segment
                valid_competitors = resolve_entities(valid_competitors, business_segment)
                
                # Sort by evidence score (highest first) and limit to top performers
                valid_competitors.sort(key=lambda x: x['evidence_score'], reverse=True)
                
                thread_safe_print(f"Extracted {len(valid_competitors)} competitors")
                return valid_competitors[:7]  # Max 7 competitors per market cell
                
            else:
                raise ValueError("No valid JSON array found in response")
                
        except Exception as e:
            thread_safe_print(f"Error extracting competitors: {e}")
            return []
    
    def discover_peers_for_segment(self, company_context: Dict[str, Any], 
                                  business_segment: Dict[str, Any],
                                  segment_id: int,
                                  max_competitors: int = 5) -> List[Dict[str, Any]]:
        """
        Complete peer discovery pipeline for a single business segment.
        Returns list of discovered competitors with database IDs.
        """
        segment_name = business_segment.get('segment_name', 'Unknown')
        thread_safe_print(f"\nDiscovering peers for segment: {segment_name}")
        
        # Step 1: Generate segment-aware search queries
        search_queries = self.generate_competitor_search_queries(company_context, business_segment)
        
        # Step 2: Execute grounded searches
        search_results = []
        for query in search_queries:
            result = self.search_competitors_grounded(
                search_query=query,
                company_name=company_context['company_name'],
                market_cell_key=segment_name
            )
            if result:
                search_results.append(result)
        
        if not search_results:
            thread_safe_print("No search results obtained")
            return []
        
        # Step 3: Extract competitors from search results
        competitors = self.extract_competitors_from_search(search_results, company_context, business_segment)
        
        if not competitors:
            thread_safe_print("No competitors extracted from search results")
            return []
        
        # Step 4: Select top competitors and save to database
        selected_competitors = competitors[:max_competitors]
        competitor_records = []
        
        for comp in selected_competitors:
            competitor_id = self.db.insert_competitor(
                market_cell_id=segment_id,  # Using segment_id with current schema
                name=comp['name'],
                parent_company=comp['parent_company'],
                evidence_score=comp['evidence_score'],
                presence_evidence=comp['presence_evidence']
            )
            
            comp['competitor_id'] = competitor_id
            competitor_records.append(comp)
        
        thread_safe_print(f"Discovered {len(competitor_records)} competitors for segment: {segment_name}")
        return competitor_records
    
    def discover_all_peers(self, company_id: int, max_competitors_per_cell: int = 3) -> Dict[int, List[Dict[str, Any]]]:
        """
        Discover competitors for all market cells of a company.
        Returns dict mapping market_cell_id to list of competitors.
        """
        thread_safe_print(f"\nStarting peer discovery for company ID: {company_id}")
        
        # Get company context and market cells
        company_row = self.db.get_connection().execute(
            "SELECT * FROM companies WHERE id = ?", (company_id,)
        ).fetchone()
        
        if not company_row:
            thread_safe_print(f"Company not found: {company_id}")
            return {}
        
        company_context = json.loads(company_row['context_json'])
        market_cells = self.db.get_market_cells_for_company(company_id)
        
        if not market_cells:
            thread_safe_print(f"No market cells found for company: {company_id}")
            return {}
        
        # Discover peers for each market cell
        all_competitors = {}
        
        for market_cell_row in market_cells:
            market_cell_dict = {
                'product_service': market_cell_row['product_service'],
                'geography': market_cell_row['geography'],
                'customer_segment': market_cell_row['customer_segment'],
                'materiality_score': market_cell_row['materiality_score']
            }
            
            competitors = self.discover_peers_for_segment(
                company_context=company_context,
                market_cell=market_cell_dict,
                market_cell_id=market_cell_row['id'],
                max_competitors=max_competitors_per_cell
            )
            
            all_competitors[market_cell_row['id']] = competitors
        
        # Clean up expired cache entries
        self.db.cleanup_expired_cache()
        
        thread_safe_print(f"\nPeer discovery complete. Found competitors for {len(all_competitors)} market cells")
        return all_competitors
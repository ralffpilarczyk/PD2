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
import google.generativeai as genai
from google.genai import types
from .entity_resolution import resolve_entities


class PeerDiscovery:
    """Discovers competitors using grounded web search"""
    
    def __init__(self, db: CompetitiveDatabase, model_name: str = "gemini-2.5-flash"):
        """Initialize with database and grounding-capable model"""
        self.db = db
        self.model_name = model_name
        # Always use 2.5-flash for grounding (required for google_search tool)
        self.grounding_model = genai.GenerativeModel("gemini-2.5-flash")
        self.analysis_model = genai.GenerativeModel(model_name)
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
                                         market_cell: Dict[str, Any]) -> List[str]:
        """
        Generate intelligent, context-aware search queries for finding competitors.
        Uses company understanding to create targeted searches.
        """
        thread_safe_print(f"Generating search queries for market cell: {market_cell['product_service']} x {market_cell['geography']} x {market_cell['customer_segment']}")
        
        prompt = f"""Based on this company context, generate targeted search queries for direct competitors in the specified market cell:

COMPANY CONTEXT:
{json.dumps(company_context, indent=2)}

MARKET CELL: {market_cell['product_service']} × {market_cell['geography']} × {market_cell['customer_segment']}

Generate 2-3 highly specific search queries that will find direct competitors:
- Include industry-specific terminology from the company context
- Use geographic qualifiers that match the market cell
- Include business model descriptors (B2B/B2C/enterprise/consumer)
- Add recent time qualifiers (2024, 2023)
- Use competitive keywords extracted from company documents

Examples of good queries:
- "Singapore digital banking fintech competitors DBS UOB 2024"
- "European B2B payment processing competitors Stripe Adyen"
- "Southeast Asia e-commerce logistics competitors Shopee Lazada"

Examples of poor queries:
- "competitors" (too generic)
- "global technology companies" (too broad)
- "fintech" (no geographic or segment focus)

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
            # Return fallback queries
            company_name = company_context.get('company_name', 'company')
            industry = company_context.get('industry', 'industry')
            geography = market_cell.get('geography', 'market')
            
            return [
                f"{company_name} competitors {geography} {industry} 2024",
                f"{industry} companies {geography} competition 2024",
                f"leading {industry} players {geography} market share"
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
            tools = [types.Tool(google_search=types.GoogleSearch())]
            response = retry_with_backoff(
                lambda: self.grounding_model.generate_content(
                    f"Find direct competitors for this search: {search_query}. Focus on company names, market presence evidence, and competitive positioning.",
                    tools=tools
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
                                      market_cell: Dict[str, Any]) -> List[Dict[str, Any]]:
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
        
        prompt = f"""Extract direct competitors from these search results for the specified market cell:

COMPANY CONTEXT:
Company: {company_context.get('company_name', 'Unknown')}
Industry: {company_context.get('industry', 'Unknown')}
Business Model: {company_context.get('business_model', 'Unknown')}

MARKET CELL: {market_cell['product_service']} × {market_cell['geography']} × {market_cell['customer_segment']}

SEARCH RESULTS:
{combined_text}

Extract direct competitors that compete in this specific market cell. Apply these criteria:
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
    }}
]

Evidence scoring guide:
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

                # Resolve entities (subsidiaries/brands/JVs → parent) per market cell
                valid_competitors = resolve_entities(valid_competitors, market_cell)
                
                # Sort by evidence score (highest first) and limit to top performers
                valid_competitors.sort(key=lambda x: x['evidence_score'], reverse=True)
                
                thread_safe_print(f"Extracted {len(valid_competitors)} competitors")
                return valid_competitors[:7]  # Max 7 competitors per market cell
                
            else:
                raise ValueError("No valid JSON array found in response")
                
        except Exception as e:
            thread_safe_print(f"Error extracting competitors: {e}")
            return []
    
    def discover_peers_for_market_cell(self, company_context: Dict[str, Any], 
                                     market_cell: Dict[str, Any],
                                     market_cell_id: int,
                                     max_competitors: int = 3) -> List[Dict[str, Any]]:
        """
        Complete peer discovery pipeline for a single market cell.
        Returns list of discovered competitors with database IDs.
        """
        market_cell_key = f"{market_cell['product_service']}_{market_cell['geography']}_{market_cell['customer_segment']}"
        thread_safe_print(f"\nDiscovering peers for market cell: {market_cell_key}")
        
        # Step 1: Generate search queries
        search_queries = self.generate_competitor_search_queries(company_context, market_cell)
        
        # Step 2: Execute grounded searches
        search_results = []
        for query in search_queries:
            result = self.search_competitors_grounded(
                search_query=query,
                company_name=company_context['company_name'],
                market_cell_key=market_cell_key
            )
            if result:
                search_results.append(result)
        
        if not search_results:
            thread_safe_print("No search results obtained")
            return []
        
        # Step 3: Extract competitors from search results
        competitors = self.extract_competitors_from_search(search_results, company_context, market_cell)
        
        if not competitors:
            thread_safe_print("No competitors extracted from search results")
            return []
        
        # Step 4: Select top competitors and save to database
        selected_competitors = competitors[:max_competitors]
        competitor_records = []
        
        for comp in selected_competitors:
            competitor_id = self.db.insert_competitor(
                market_cell_id=market_cell_id,
                name=comp['name'],
                parent_company=comp['parent_company'],
                evidence_score=comp['evidence_score'],
                presence_evidence=comp['presence_evidence']
            )
            
            comp['competitor_id'] = competitor_id
            competitor_records.append(comp)
        
        thread_safe_print(f"Discovered {len(competitor_records)} competitors for {market_cell_key}")
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
            
            competitors = self.discover_peers_for_market_cell(
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
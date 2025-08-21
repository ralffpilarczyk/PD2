"""
Business Segment Extraction and Company Context Analysis.
Extracts reported business segments from financial documents and ranks by significance.
"""

import json
import re
from typing import Dict, List, Optional, Tuple, Any
from ..utils import thread_safe_print, retry_with_backoff
from .database import CompetitiveDatabase
import google.generativeai as genai


class SegmentExtractor:
    """Extracts business segments and company context from financial documents"""
    
    def __init__(self, db: CompetitiveDatabase, model_name: str = "gemini-2.5-flash"):
        """Initialize with database connection and model"""
        self.db = db
        self.model_name = model_name
        self.model = genai.GenerativeModel(model_name)
    
    def extract_company_context(self, document_content: str, company_name: str = None) -> Dict[str, Any]:
        """
        Extract structured company context from documents using LLM analysis.
        Returns comprehensive company understanding for competitive analysis.
        """
        thread_safe_print(f"Extracting company context and segment information from documents...")
        
        prompt = f"""Based on these company documents, extract comprehensive company understanding:

DOCUMENT CONTENT:
{document_content[:30000]}

Extract structured information covering:
- Company identification and corporate structure
- Industry classification and business model
- Geographic presence and market focus  
- Product/service portfolio and customer base
- Financial indicators and company stage
- Keywords optimal for competitive research

Return structured JSON with this exact format:
{{
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
}}

Focus on factual information from the documents. Be specific about geographic markets and business segments."""
        
        try:
            response_text = retry_with_backoff(
                lambda: self.model.generate_content(prompt).text
            )
            
            # Extract JSON from response
            json_match = re.search(r'\{.*\}', response_text, re.DOTALL)
            if json_match:
                context = json.loads(json_match.group())
                # Embed original document text for later segment extraction
                context['__document_text__'] = document_content
                
                # Use provided company name if context extraction missed it
                if company_name and not context.get('company_name'):
                    context['company_name'] = company_name
                
                thread_safe_print(f"Extracted context for {context.get('company_name', 'company')}")
                return context
            else:
                raise ValueError("No valid JSON found in response")
                
        except Exception as e:
            thread_safe_print(f"Error extracting company context: {e}")
            # Return minimal context with provided company name
            return {
                "company_name": company_name or "Unknown Company",
                "ticker": None,
                "other_companies": [],
                "industry": "Unknown",
                "sub_sectors": [],
                "business_model": "Unknown",
                "headquarters": "Unknown",
                "primary_markets": [],
                "geography": "Unknown",
                "products_services": "Unknown",
                "target_customers": "Unknown",
                "revenue_model": "Unknown",
                "stage": "Unknown",
                "financial_highlights": "Unknown",
                "competitive_keywords": [],
                "market_keywords": [],
                "__document_text__": document_content
            }
    
    def extract_business_segments(self, company_context: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        Extract actual business segments as reported by the company.
        Returns list of segments sorted by financial significance.
        """
        thread_safe_print("Extracting business segments from financial reports...")
        
        # Extract the actual reported segments from documents
        document_text = company_context.get('__document_text__', '')
        if not document_text:
            thread_safe_print("No document text available for segment extraction")
            return self._fallback_segment_discovery(company_context)
        
        prompt = f"""Extract the ACTUAL BUSINESS SEGMENTS as reported by the company in their financial documents.

DOCUMENT CONTENT:
{document_text[:50000]}  # Use actual document text

Look for:
1. Segment reporting sections
2. Operating segments or business units  
3. Revenue/profit breakdown by segment
4. Geographic segment information
5. Product line segments

Extract the company's ACTUAL reported segments, not hypothetical ones.

Return JSON array with segments sorted by financial significance:
[
    {{
        "segment_name": "exact name as reported by company",
        "description": "what this segment includes",
        "revenue_contribution": "amount or % if disclosed",
        "profit_contribution": "operating profit or EBITDA if disclosed", 
        "growth_rate": "YoY growth if disclosed",
        "key_metrics": "other KPIs mentioned (subscribers, ARPU, etc.)",
        "geographic_focus": "countries/regions if segment is geographic",
        "products_services": "main offerings in this segment",
        "significance_score": 0.9
    }}
]

CRITICAL: 
- Use the EXACT segment names the company uses
- Include financial data if available
- Sort by revenue contribution (largest first)
- Only include segments actually reported, not inferred
- If no clear segments found, return company's main business as single segment"""

        try:
            # Use more aggressive retry for segment extraction as it's critical
            response_text = retry_with_backoff(
                lambda: self.model.generate_content(
                    prompt,
                    generation_config={"temperature": 0.3, "max_output_tokens": 4000}
                ).text,
                max_retries=5,  # More retries for critical segment extraction
                base_delay=2.0  # Start with longer delay
            )
            
            # Extract JSON array from response
            json_match = re.search(r'\[.*\]', response_text, re.DOTALL)
            if json_match:
                segments = json.loads(json_match.group())
                
                # Calculate significance scores based on available data
                validated_segments = []
                for segment in segments:
                    if 'segment_name' in segment:
                        # Calculate significance using both revenue and profit contributions
                        if 'significance_score' not in segment:
                            segment['significance_score'] = self._calculate_segment_significance(segment)
                        
                        segment['significance_score'] = max(0.0, min(1.0, segment['significance_score']))
                        validated_segments.append(segment)
                
                # Sort by significance using intelligent business judgment
                validated_segments = self._sort_segments_by_significance(validated_segments)
                
                # Limit to top 8 segments if too many
                if len(validated_segments) > 8:
                    thread_safe_print(f"Limiting to top 8 segments from {len(validated_segments)} total")
                    validated_segments = validated_segments[:8]
                
                thread_safe_print(f"Extracted {len(validated_segments)} business segments")
                return validated_segments
            else:
                raise ValueError("No valid JSON array found in response")
                
        except Exception as e:
            thread_safe_print(f"Error extracting business segments: {e}")
            return self._fallback_segment_discovery(company_context)
    
    def _calculate_segment_significance(self, segment: Dict[str, Any]) -> float:
        """
        Calculate segment significance using both revenue and profit contributions.
        Applies business judgment for unconsolidated affiliates.
        """
        revenue_str = str(segment.get('revenue_contribution', ''))
        profit_str = str(segment.get('profit_contribution', ''))
        
        # Extract percentage values
        revenue_pct = 0.0
        profit_pct = 0.0
        
        if '%' in revenue_str:
            try:
                revenue_pct = float(re.findall(r'[\d.]+', revenue_str)[0]) / 100.0
            except:
                pass
        
        if '%' in profit_str:
            try:
                profit_pct = float(re.findall(r'[\d.]+', profit_str)[0]) / 100.0
            except:
                pass
        
        # Business judgment for significance - principle-based approach
        if revenue_pct > 0 and profit_pct > 0:
            # Have both metrics - use weighted average favoring revenue
            # Revenue is more indicative of scale, profit of strategic importance
            return (revenue_pct * 0.7) + (profit_pct * 0.3)
        elif revenue_pct > 0:
            # Consolidated entity - revenue is the primary indicator
            return revenue_pct
        elif profit_pct > 0:
            # Unconsolidated affiliate - use profit as proxy for significance
            # Don't artificially inflate; profit contribution IS the significance
            # Just ensure it's not unfairly ranked below tiny consolidated segments
            return profit_pct * 0.9  # Slight discount since we lack full visibility
        else:
            # Check for other indicators of importance
            segment_name = segment.get('segment_name', '').lower()
            description = segment.get('description', '').lower()
            
            # Look for financial amounts even without percentages
            if 'billion' in description or 'million' in description:
                return 0.25
            
            # Strategic segments often mentioned without percentages
            if any(term in segment_name + description for term in 
                   ['strategic', 'growth', 'digital', 'innovation', 'emerging']):
                return 0.15  # Lower weight for unquantified strategic segments
            
            # Default for segments without clear metrics
            return 0.1
    
    def _sort_segments_by_significance(self, segments: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Sort segments using intelligent business judgment.
        Considers both revenue and profit contributions.
        """
        def segment_sort_key(segment):
            score = segment.get('significance_score', 0)
            
            # Boost score for segments with explicit financial data
            if segment.get('revenue_contribution') and '%' in str(segment.get('revenue_contribution')):
                score += 0.01  # Small boost for having revenue data
            
            # Check if it's an unconsolidated affiliate
            if 'unconsolidated' in segment.get('description', '').lower() or \
               'equity' in segment.get('description', '').lower() or \
               'associate' in segment.get('segment_name', '').lower():
                # Don't penalize unconsolidated entities
                if segment.get('profit_contribution') and '%' in str(segment.get('profit_contribution')):
                    score += 0.005  # Small boost for having profit data
            
            return score
        
        # Sort with business judgment
        sorted_segments = sorted(segments, key=segment_sort_key, reverse=True)
        
        # Log the sorting for transparency
        thread_safe_print("Segments sorted by significance:")
        for i, seg in enumerate(sorted_segments[:5], 1):  # Show top 5
            rev = seg.get('revenue_contribution', 'N/A')
            prof = seg.get('profit_contribution', 'N/A') 
            thread_safe_print(f"  {i}. {seg['segment_name']}: Score={seg['significance_score']:.2f}, Rev={rev}, Profit={prof}")
        
        return sorted_segments
    
    def _fallback_segment_discovery(self, company_context: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Intelligent fallback using company context to infer segments"""
        thread_safe_print("Using intelligent fallback segment discovery")
        
        segments = []
        
        # Look for subsidiary companies mentioned in context
        subsidiaries = company_context.get('other_companies', [])
        primary_markets = company_context.get('primary_markets', [])
        
        if subsidiaries and primary_markets:
            # Create segments using LLM to intelligently map subsidiaries to markets
            thread_safe_print(f"Found {len(subsidiaries)} subsidiaries - inferring their markets")
            
            # Use LLM to infer market mappings
            mapping_prompt = f"""Given these subsidiaries and markets, infer which market each subsidiary likely operates in:

Subsidiaries: {', '.join(subsidiaries[:8])}
Available Markets: {', '.join(primary_markets)}

Use contextual clues in the names and common telecom industry knowledge.
Return a simple JSON mapping: {{"subsidiary_name": "market"}}
Example: {{"CelcomDigi": "Malaysia", "XL Axiata": "Indonesia"}}"""

            try:
                mapping_response = retry_with_backoff(
                    lambda: self.model.generate_content(
                        mapping_prompt,
                        generation_config={"temperature": 0.1, "max_output_tokens": 500}
                    ).text
                )
                
                # Extract JSON mapping
                import json
                json_match = re.search(r'\{.*\}', mapping_response, re.DOTALL)
                if json_match:
                    market_mappings = json.loads(json_match.group())
                else:
                    market_mappings = {}
            except:
                market_mappings = {}
            
            for i, subsidiary in enumerate(subsidiaries[:8]):
                # Get market from LLM mapping or use first primary market
                geographic_focus = market_mappings.get(subsidiary, primary_markets[0] if primary_markets else 'Regional')
                
                # Infer product focus from subsidiary name patterns
                subsidiary_lower = subsidiary.lower()
                if any(term in subsidiary_lower for term in ['boost', 'bank', 'pay', 'wallet', 'fintech']):
                    products = "Digital financial services and banking"
                elif any(term in subsidiary_lower for term in ['tower', 'infra', 'edotco']):
                    products = "Telecommunications infrastructure services"
                elif 'merger' in subsidiary_lower or 'future' in subsidiary_lower:
                    products = "Planned telecommunications entity"
                else:
                    products = "Mobile and broadband telecommunications services"
                
                segments.append({
                    "segment_name": subsidiary,
                    "description": f"Operating entity focused on {geographic_focus} market",
                    "revenue_contribution": "Not disclosed",
                    "profit_contribution": "Not disclosed",
                    "growth_rate": "Not disclosed",
                    "key_metrics": "",
                    "geographic_focus": geographic_focus,
                    "products_services": products,
                    "significance_score": 0.5 - (i * 0.05)
                })
        
        # If no subsidiaries, create geographic segments from primary markets
        elif primary_markets:
            thread_safe_print(f"Creating segments from {len(primary_markets)} primary markets")
            for i, market in enumerate(primary_markets[:5]):  # Top 5 markets
                segments.append({
                    "segment_name": f"{market} Operations",
                    "description": f"Business operations in {market}",
                    "revenue_contribution": "Not disclosed",
                    "profit_contribution": "Not disclosed", 
                    "growth_rate": "Not disclosed",
                    "key_metrics": "",
                    "geographic_focus": market,
                    "products_services": company_context.get('products_services', ''),
                    "significance_score": 0.6 - (i * 0.1)
                })
        
        # Last resort - single segment
        if not segments:
            thread_safe_print("Creating single default segment")
            segments = [{
                "segment_name": "Core Business",
                "description": company_context.get('products_services', 'Main operations'),
                "revenue_contribution": "100%",
                "profit_contribution": "100%",
                "growth_rate": "Not disclosed",
                "key_metrics": company_context.get('financial_highlights', ''),
                "geographic_focus": ', '.join(primary_markets[:3]) if primary_markets else 'Global',
                "products_services": company_context.get('products_services', ''),
                "significance_score": 1.0
            }]
        
        return segments
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
            response_text = retry_with_backoff(
                lambda: self.model.generate_content(
                    prompt,
                    generation_config={"temperature": 0.3, "max_output_tokens": 4000}
                ).text
            )
            
            # Extract JSON array from response
            json_match = re.search(r'\[.*\]', response_text, re.DOTALL)
            if json_match:
                segments = json.loads(json_match.group())
                
                # Calculate significance scores based on available data
                validated_segments = []
                for segment in segments:
                    if 'segment_name' in segment:
                        # Calculate significance based on revenue contribution
                        if 'significance_score' not in segment:
                            # Try to parse revenue contribution
                            revenue_str = str(segment.get('revenue_contribution', ''))
                            if '%' in revenue_str:
                                try:
                                    pct = float(re.findall(r'[\d.]+', revenue_str)[0])
                                    segment['significance_score'] = pct / 100.0
                                except:
                                    segment['significance_score'] = 0.5
                            else:
                                segment['significance_score'] = 0.5
                        
                        segment['significance_score'] = max(0.0, min(1.0, segment['significance_score']))
                        validated_segments.append(segment)
                
                # Sort by significance (revenue contribution)
                validated_segments.sort(key=lambda x: x.get('significance_score', 0), reverse=True)
                
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
    
    def _fallback_segment_discovery(self, company_context: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Fallback when segment extraction fails - create single segment from context"""
        thread_safe_print("Using fallback segment discovery")
        return [{
            "segment_name": company_context.get('company_name', 'Core Business'),
            "description": company_context.get('products_services', 'Main business operations'),
            "revenue_contribution": "100%",
            "profit_contribution": "Unknown",
            "growth_rate": "Unknown",
            "key_metrics": company_context.get('financial_highlights', ''),
            "geographic_focus": ', '.join(company_context.get('primary_markets', ['Global'])[:3]),
            "products_services": company_context.get('products_services', ''),
            "significance_score": 1.0
        }]
"""
Market Cell Discovery and Company Context Extraction.
Analyzes company documents to understand business structure and define competitive market cells.
"""

import json
import re
from typing import Dict, List, Optional, Tuple, Any
from ..utils import thread_safe_print, retry_with_backoff
from .database import CompetitiveDatabase
import google.generativeai as genai


class MarketMapper:
    """Discovers market cells and extracts company context from documents"""
    
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
        thread_safe_print(f"Extracting company context from documents...")
        
        prompt = f"""Based on these company documents, extract comprehensive company understanding:

DOCUMENT CONTENT:
{document_content}

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
                "market_keywords": []
            }
    
    def discover_market_cells(self, company_context: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        Discover market cells based on company context using intelligent segmentation.
        Returns list of market cells with materiality scores.
        """
        thread_safe_print("Discovering market cells from company context...")
        
        prompt = f"""Based on this company context, identify material market cells for competitive analysis:

COMPANY CONTEXT:
{json.dumps(company_context, indent=2)}

Create market cells using this framework:
- Market Cell = (Product/Service) × (Geography) × (Customer Segment)
- Include cells that represent ≥20% of core business metrics (revenue, users, etc.)
- Use the company's own reporting structure and business divisions
- Apply business judgment: "Would an executive think of these as separate businesses?"

Generate 3-8 market cells that capture the company's competitive landscape.

Return JSON array with this exact format:
[
    {{
        "product_service": "specific product or service offering",
        "geography": "geographic market (country, region, or global)",
        "customer_segment": "target customer type (enterprise, consumer, SMB, etc.)",
        "materiality_score": 0.85,
        "rationale": "why this is a material market cell",
        "estimated_revenue_share": "approximate % of company revenue if known"
    }}
]

Focus on:
1. True business divisions, not artificial segmentation
2. Geographic markets where the company actually competes
3. Distinct customer segments with different competitive dynamics
4. Materiality - only include significant business areas

Examples of good market cells:
- "Mobile Services × Germany × Consumer"
- "Enterprise Software × North America × Large Enterprises"
- "Digital Banking × Singapore × Individual Customers"

Examples of poor market cells:
- "Technology × Global × Everyone" (too broad)
- "Product A × City X × Niche Y" (too narrow, <20% materiality)"""

        try:
            response_text = retry_with_backoff(
                lambda: self.model.generate_content(prompt).text
            )
            
            # Extract JSON array from response
            json_match = re.search(r'\[.*\]', response_text, re.DOTALL)
            if json_match:
                market_cells = json.loads(json_match.group())
                
                # Validate and clean market cells
                validated_cells = []
                for cell in market_cells:
                    if all(key in cell for key in ['product_service', 'geography', 'customer_segment']):
                        # Ensure materiality score is present and reasonable
                        if 'materiality_score' not in cell:
                            cell['materiality_score'] = 0.5
                        cell['materiality_score'] = max(0.0, min(1.0, cell['materiality_score']))
                        validated_cells.append(cell)
                
                # Apply consolidation if too many cells
                if len(validated_cells) > 8:
                    validated_cells = self._consolidate_market_cells(validated_cells)
                
                thread_safe_print(f"Discovered {len(validated_cells)} market cells")
                return validated_cells
            else:
                raise ValueError("No valid JSON array found in response")
                
        except Exception as e:
            thread_safe_print(f"Error discovering market cells: {e}")
            # Return default market cell based on context
            return [{
                "product_service": company_context.get('products_services', 'Core Business'),
                "geography": company_context.get('primary_markets', ['Global'])[0] if company_context.get('primary_markets') else 'Global',
                "customer_segment": company_context.get('target_customers', 'General Market'),
                "materiality_score": 1.0,
                "rationale": "Default market cell based on available company information",
                "estimated_revenue_share": "Unknown"
            }]
    
    def _consolidate_market_cells(self, market_cells: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Consolidate market cells if there are too many (>8).
        Apply business logic to group by largest dimension.
        """
        thread_safe_print(f"Consolidating {len(market_cells)} market cells to maximum 8...")
        
        # Group by dominant dimension (geography/product/customer) if too many
        if len(market_cells) <= 8:
            return market_cells
        
        # Determine dominant dimension by frequency and summed materiality
        def _group_by_key(key: str):
            groups = {}
            for cell in market_cells:
                k = cell.get(key, 'Unknown')
                groups.setdefault(k, []).append(cell)
            return groups
        
        geo_groups = _group_by_key('geography')
        prod_groups = _group_by_key('product_service')
        cust_groups = _group_by_key('customer_segment')
        
        def _score(groups):
            return sum(len(v) for v in groups.values()) + sum(sum(c.get('materiality_score', 0) for c in v) for v in groups.values())
        
        # Choose grouping with highest score as dominant dimension
        dominant = max([
            ('geography', geo_groups, _score(geo_groups)),
            ('product_service', prod_groups, _score(prod_groups)),
            ('customer_segment', cust_groups, _score(cust_groups))
        ], key=lambda x: x[2])
        _, groups, _ = dominant
        
        # Aggregate into consolidated cells per group, dropping groups <10% materiality
        consolidated = []
        total_materiality = sum(cell.get('materiality_score', 0) for cell in market_cells)
        for group_key, cells in groups.items():
            materiality_sum = sum(c.get('materiality_score', 0) for c in cells)
            if total_materiality and (materiality_sum / total_materiality) < 0.10:
                continue
            # Create consolidated representation
            representative = max(cells, key=lambda c: c.get('materiality_score', 0))
            consolidated.append({
                'product_service': representative.get('product_service', 'Consolidated'),
                'geography': representative.get('geography', 'Consolidated'),
                'customer_segment': representative.get('customer_segment', 'Consolidated'),
                'materiality_score': materiality_sum,
                'rationale': f"Consolidated group '{group_key}' with {len(cells)} cells"
            })
        
        # Sort and limit to 8
        consolidated.sort(key=lambda x: x['materiality_score'], reverse=True)
        consolidated = consolidated[:8]
        
        # Normalize materiality scores
        total_score = sum(cell['materiality_score'] for cell in consolidated)
        if total_score > 0:
            for cell in consolidated:
                cell['materiality_score'] = cell['materiality_score'] / total_score
        
        thread_safe_print(f"Consolidated to {len(consolidated)} market cells")
        return consolidated
    
    def validate_market_cells(self, market_cells: List[Dict[str, Any]], 
                             company_context: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        Validate market cells using business logic and company context.
        Apply the principle: "Would a business executive think of these as separate businesses?"
        """
        thread_safe_print("Validating market cells with business logic...")
        
        validated_cells = []
        for cell in market_cells:
            # Check minimum materiality threshold
            if cell.get('materiality_score', 0) >= 0.2:  # 20% threshold
                
                # Ensure all required fields are present and meaningful
                if (cell.get('product_service') and 
                    cell.get('geography') and 
                    cell.get('customer_segment') and
                    cell.get('product_service').lower() not in ['unknown', 'n/a', ''] and
                    cell.get('geography').lower() not in ['unknown', 'n/a', ''] and
                    cell.get('customer_segment').lower() not in ['unknown', 'n/a', '']):
                    
                    validated_cells.append(cell)
                else:
                    thread_safe_print(f"Rejected market cell due to missing/invalid fields: {cell}")
            else:
                thread_safe_print(f"Rejected market cell due to low materiality ({cell.get('materiality_score', 0):.2f}): {cell.get('product_service', 'Unknown')}")
        
        # Ensure at least one market cell exists
        if not validated_cells:
            thread_safe_print("No valid market cells found, creating default...")
            validated_cells = [{
                "product_service": company_context.get('products_services', 'Core Business'),
                "geography": "Global",
                "customer_segment": "General Market",
                "materiality_score": 1.0,
                "rationale": "Default market cell - company operates as single business unit",
                "estimated_revenue_share": "100%"
            }]
        
        thread_safe_print(f"Validated {len(validated_cells)} market cells")
        return validated_cells
    
    def save_company_and_market_cells(self, company_context: Dict[str, Any], 
                                    market_cells: List[Dict[str, Any]]) -> Tuple[int, List[int]]:
        """
        Save company context and market cells to database.
        Returns (company_id, list_of_market_cell_ids).
        """
        thread_safe_print("Saving company and market cells to database...")
        
        # Insert company
        company_id = self.db.insert_company(
            name=company_context['company_name'],
            context=company_context
        )
        
        # Insert market cells
        market_cell_ids = []
        for cell in market_cells:
            market_cell_id = self.db.insert_market_cell(
                company_id=company_id,
                product_service=cell['product_service'],
                geography=cell['geography'],
                customer_segment=cell['customer_segment'],
                materiality_score=cell['materiality_score']
            )
            market_cell_ids.append(market_cell_id)
        
        thread_safe_print(f"Saved company (ID: {company_id}) and {len(market_cell_ids)} market cells")
        return company_id, market_cell_ids
    
    def analyze_company_from_documents(self, document_content: str, 
                                     company_name: str = None) -> Tuple[Dict[str, Any], List[Dict[str, Any]]]:
        """
        Complete analysis pipeline: extract context → discover market cells → validate.
        Returns (company_context, validated_market_cells).
        """
        thread_safe_print("Starting company analysis from documents...")
        
        # Step 1: Extract company context
        company_context = self.extract_company_context(document_content, company_name)
        
        # Step 2: Discover market cells
        market_cells = self.discover_market_cells(company_context)
        
        # Step 3: Validate market cells
        validated_cells = self.validate_market_cells(market_cells, company_context)
        
        # Step 4: Save to database
        company_id, market_cell_ids = self.save_company_and_market_cells(company_context, validated_cells)
        
        thread_safe_print(f"Company analysis complete for {company_context['company_name']}")
        return company_context, validated_cells
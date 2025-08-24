"""
Fact Extractor - Extract financial metrics from markdown files
"""

import re
from decimal import Decimal
from canonicalizer import canonicalize_number, generate_fact_id, format_for_prolog

class FactExtractor:
    """Extract financial facts from markdown documents"""
    
    def __init__(self):
        self.patterns = {
            # Two patterns for EBITDA - try both
            'ebitda_table': r'EBITDA.*?\|\s*([\d,]+(?:\.\d+)?)\s*\|',  # Simple EBITDA table
            'ebitda_complex': r'Earnings before interest.*?\|\s*[\d,]+\s*\|.*?\|\s*([\d,]+)\s*\|$',  # Complex table, take last column
            # Pattern for revenue
            'revenue': r'(?:External operating revenue|Total operating revenue)\s*\|?\s*([\d,]+)',
            # Pattern for operating costs
            'costs': r'Other operating costs\s*\|?\s*\(([\d,]+)\)',
            # Pattern for segment profit
            'profit': r'Segment profit.*?\s*\|?\s*([\d,]+)',
        }
    
    def extract_facts(self, md_content, company_name, file_name):
        """
        Extract facts from markdown content
        
        Args:
            md_content: Markdown file content
            company_name: Name of the company (e.g., 'Axiata')
            file_name: Source file name
            
        Returns:
            List of fact dictionaries
        """
        facts = []
        
        # Determine period from filename
        period_label, period_date = self._extract_period(file_name, md_content)
        
        # Extract EBITDA - try both patterns
        ebitda_facts = self._extract_metric(
            md_content, 'ebitda_table', company_name, 
            period_label, period_date, file_name
        )
        if not ebitda_facts:
            ebitda_facts = self._extract_metric(
                md_content, 'ebitda_complex', company_name,
                period_label, period_date, file_name
            )
        # Rename key to just 'ebitda'
        for fact in ebitda_facts:
            fact['key'] = 'ebitda'
        facts.extend(ebitda_facts)
        
        # Extract Revenue
        revenue_facts = self._extract_metric(
            md_content, 'revenue', company_name,
            period_label, period_date, file_name
        )
        facts.extend(revenue_facts)
        
        # Calculate margins if we have both EBITDA and revenue
        margin_facts = self._calculate_margins(
            ebitda_facts, revenue_facts, company_name,
            period_label, period_date, file_name
        )
        facts.extend(margin_facts)
        
        # Extract operating costs (for future theorems)
        cost_facts = self._extract_metric(
            md_content, 'costs', company_name,
            period_label, period_date, file_name
        )
        facts.extend(cost_facts)
        
        return facts
    
    def _extract_period(self, file_name, content):
        """Extract period information from filename and content"""
        
        # Try to extract from filename first
        if '25Q1' in file_name:
            return 'Q1 2025', '2025-03-31'
        elif '24Q4' in file_name or '24 FS' in file_name:
            return 'FY 2024', '2024-12-31'
        elif '24Q1' in file_name:
            return 'Q1 2024', '2024-03-31'
        
        # Try to find period in content
        period_match = re.search(r'(?:period|quarter) ended (\d{1,2})\s+(\w+)\s+(\d{4})', content, re.I)
        if period_match:
            day = period_match.group(1).zfill(2)
            month_str = period_match.group(2)
            year = period_match.group(3)
            
            # Convert month name to number
            months = {
                'january': '01', 'february': '02', 'march': '03',
                'april': '04', 'may': '05', 'june': '06',
                'july': '07', 'august': '08', 'september': '09',
                'october': '10', 'november': '11', 'december': '12'
            }
            month = months.get(month_str.lower(), '12')
            
            # Determine quarter
            quarter = (int(month) - 1) // 3 + 1
            period_label = f"Q{quarter} {year}"
            period_date = f"{year}-{month}-{day}"
            
            return period_label, period_date
        
        # Default
        return 'Unknown', '2024-12-31'
    
    def _extract_metric(self, content, metric_key, company, period_label, period_date, source_doc):
        """Extract a specific metric using regex patterns"""
        
        facts = []
        pattern = self.patterns.get(metric_key)
        
        if not pattern:
            return facts
        
        matches = re.finditer(pattern, content, re.IGNORECASE | re.MULTILINE)
        
        for match_num, match in enumerate(matches):
            # Usually first number is current period, second is comparison
            if len(match.groups()) >= 1:
                value_str = match.group(1)
                value = canonicalize_number(value_str)
                
                if value is not None:
                    # Keep in original units (thousands) for consistency
                    
                    fact = {
                        'company': company,
                        'key': metric_key,
                        'value': value,
                        'confidence': 0.95,  # High confidence for directly extracted values
                        'period_label': period_label,
                        'period_date': period_date,
                        'source_doc': source_doc,
                        'source_page': match_num + 1,  # Approximate page
                        'method': 'regex_extract'
                    }
                    fact['fact_id'] = generate_fact_id(
                        company, metric_key, period_label, str(value)
                    )
                    facts.append(fact)
                    break  # Take first match only
        
        return facts
    
    def _calculate_margins(self, ebitda_facts, revenue_facts, company, period_label, period_date, source_doc):
        """Calculate margin metrics from EBITDA and revenue"""
        
        facts = []
        
        # Find matching EBITDA and revenue for same period
        for ebitda_fact in ebitda_facts:
            for revenue_fact in revenue_facts:
                if ebitda_fact['period_label'] == revenue_fact['period_label']:
                    # Calculate EBITDA margin
                    if revenue_fact['value'] > 0:
                        margin = ebitda_fact['value'] / revenue_fact['value']
                        
                        fact = {
                            'company': company,
                            'key': 'ebitda_margin',
                            'value': margin,
                            'confidence': min(ebitda_fact['confidence'], revenue_fact['confidence']) * 0.95,
                            'period_label': period_label,
                            'period_date': period_date,
                            'source_doc': source_doc,
                            'source_page': 999,  # Calculated
                            'method': 'calculated'
                        }
                        fact['fact_id'] = generate_fact_id(
                            company, 'ebitda_margin', period_label, str(margin)
                        )
                        facts.append(fact)
        
        return facts
    
    def write_facts_to_prolog(self, facts, output_file):
        """Write facts to Prolog file format"""
        
        with open(output_file, 'w') as f:
            f.write("% Facts extracted from financial statements\n\n")
            
            # Sort facts for deterministic output
            sorted_facts = sorted(facts, key=lambda x: (
                x['company'], x['key'], x['period_date'], x['source_page']
            ))
            
            for fact in sorted_facts:
                # base(Company, Key, Value, Confidence, Source, Period, Method, FactID)
                f.write(f"base('{fact['company']}', ")
                f.write(f"{fact['key']}, ")
                f.write(f"{format_for_prolog(fact['value'])}, ")
                f.write(f"{fact['confidence']}, ")
                f.write(f"src('{fact['source_doc']}', {fact['source_page']}, 'hash'), ")
                f.write(f"period('{fact['period_label']}', '{fact['period_date']}'), ")
                f.write(f"method('{fact['method']}'), ")
                f.write(f"'{fact['fact_id']}').\n")
            
            f.write("\n% End of facts\n")


# Test the extractor
if __name__ == "__main__":
    import os
    
    extractor = FactExtractor()
    
    # Test with actual Axiata file if available
    test_file = "../SourceFiles/Axiata 25Q1 FS_m.md"
    
    if os.path.exists(test_file):
        print(f"Testing with {test_file}")
        with open(test_file, 'r') as f:
            content = f.read()
        
        facts = extractor.extract_facts(content, 'Axiata', 'Axiata 25Q1 FS_m.md')
        
        print(f"\nExtracted {len(facts)} facts:")
        for fact in facts:
            print(f"  {fact['key']}: {fact['value']:.4f} ({fact['period_label']})")
    else:
        print(f"Test file not found: {test_file}")
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
            # Pattern for revenue - enhanced for bilingual
            'revenue': r'(?:Revenue|External operating revenue|Total operating revenue|Mobile Revenue|Total Revenue)\s*\|?\s*([\d,]+)',
            # Pattern for operating costs
            'costs': r'Other operating costs\s*\|?\s*\(([\d,]+)\)',
            # Pattern for segment profit
            'profit': r'Segment profit.*?\s*\|?\s*([\d,]+)',
            # Balance Sheet Items - enhanced for Indonesian/English bilingual
            'total_assets': r'(?:Total assets|Jumlah aset|Total aset)\s*\|?\s*([\d,]+)',
            'total_equity': r'(?:Total equity|Jumlah ekuitas|Total ekuitas)\s*\|?\s*([\d,]+)',
            'current_assets': r'(?:Total current assets|Jumlah aset lancar)\s*\|?\s*([\d,]+)',
            'current_liabilities': r'(?:Total current liabilities|Jumlah liabilitas jangka pendek)\s*\|?\s*([\d,]+)',
            'total_debt': r'(?:Total debt|Borrowings.*total|Pinjaman)\s*\|?\s*([\d,]+)',
            'borrowings': r'(?:Pinjaman|Borrowings?)\s*(?:jangka|term)?\s*\|?\s*([\d,]+)',
            'non_current_assets': r'(?:Total non-current assets|Jumlah aset tidak lancar)\s*\|?\s*([\d,]+)',
            'cash': r'(?:Cash and cash equivalents|Kas dan setara kas|CASH AND CASH EQUIVALENTS AT THE END)\s*\|?\s*([\d,]+)',
            'inventory': r'(?:Inventory|Inventories|Persediaan)\s*\|?\s*([\d,]+)',
            # Income Statement Items - enhanced
            'net_income': r'(?:Net profit|Net income|Profit for the (?:financial )?(?:period|year)|Profit attributable|Laba bersih|PAT)\s*\|?\s*([\d,]+)',
            'operating_income': r'(?:Operating income|Operating profit|EBIT[^D]|Laba operasi)\s*\|?\s*([\d,]+)',
            'interest_expense': r'(?:Interest expense|Finance costs?|Beban bunga)\s*\|?\s*\(?([\d,]+)\)?',
            'tax_expense': r'(?:Tax expense|Income tax expense|Taxation|Pajak penghasilan)\s*\|?\s*\(?([\d,]+)\)?',
            'depreciation': r'(?:Depreciation and amortisation|D&A|Depresiasi dan amortisasi)\s*\|?\s*([\d,]+)',
            # Cash Flow Items
            'operating_cash_flow': r'(?:CASH FLOWS FROM OPERATING ACTIVITIES|Cash flows? from operating|Net cash from operating)\s*\|?\s*([\d,]+)',
            'capex': r'(?:Purchase of PPE|Capital expenditure|Purchase of property, plant and equipment|Capex)\s*\|?\s*\(?([\d,]+)\)?',
            'intangible_capex': r'(?:Acquisition of intangible assets|Purchase of intangible)\s*\|?\s*\(?([\d,]+)\)?',
            'free_cash_flow': r'(?:Free cash flow|FCF)\s*\|?\s*([\d,]+)',
            # Working Capital Components  
            'trade_receivables': r'Trade and other receivables[^|]*\|\s*([\d,]+)',
            'trade_payables': r'(?:CURRENT LIABILITIES[\s\S]*?)Trade and other payables[^|]*\|\s*([\d,]+)',
            'inventories': r'Inventories[^|]*\|\s*([\d,]+)',
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
        
        # Extract all balance sheet items
        for metric in ['total_assets', 'total_equity', 'current_assets', 
                      'current_liabilities', 'total_debt', 'borrowings',
                      'non_current_assets', 'cash', 'inventory']:
            metric_facts = self._extract_metric(
                md_content, metric, company_name,
                period_label, period_date, file_name
            )
            facts.extend(metric_facts)
        
        # Extract income statement items
        for metric in ['net_income', 'operating_income', 'interest_expense',
                      'tax_expense', 'depreciation']:
            metric_facts = self._extract_metric(
                md_content, metric, company_name,
                period_label, period_date, file_name
            )
            facts.extend(metric_facts)
        
        # Extract cash flow items
        for metric in ['operating_cash_flow', 'capex', 'intangible_capex', 'free_cash_flow']:
            metric_facts = self._extract_metric(
                md_content, metric, company_name,
                period_label, period_date, file_name
            )
            facts.extend(metric_facts)
        
        # Extract working capital components
        for metric in ['trade_receivables', 'trade_payables', 'inventories']:
            metric_facts = self._extract_metric(
                md_content, metric, company_name,
                period_label, period_date, file_name
            )
            facts.extend(metric_facts)
        
        # Calculate total_assets if we have components
        total_assets_facts = self._calculate_total_assets(
            facts, company_name, period_label, period_date, file_name
        )
        facts.extend(total_assets_facts)
        
        # Use borrowings as total_debt if total_debt not found
        total_debt_facts = self._calculate_total_debt(
            facts, company_name, period_label, period_date, file_name
        )
        facts.extend(total_debt_facts)
        
        # Calculate advanced metrics
        advanced_facts = self._calculate_advanced_metrics(
            facts, company_name, period_label, period_date, file_name
        )
        facts.extend(advanced_facts)
        
        return facts
    
    def _extract_period(self, file_name, content):
        """Extract period information from document content"""
        
        # Search for common period patterns in the document content
        patterns = [
            # "for the period/quarter/year ended DD Month YYYY"
            r'(?:for the |)(?:period|quarter|year) ended (\d{1,2})\s+(\w+)\s+(\d{4})',
            # "Quarter ended Month DD, YYYY"
            r'(?:period|quarter|year) ended (\w+)\s+(\d{1,2}),?\s+(\d{4})',
            # "DD Month YYYY" in context of financial period
            r'(?:as of|as at)\s+(\d{1,2})\s+(\w+)\s+(\d{4})',
            # "Month DD, YYYY" in context of financial period
            r'(?:as of|as at)\s+(\w+)\s+(\d{1,2}),?\s+(\d{4})',
            # Three months ended format
            r'(?:three|six|nine|twelve) months ended\s+(\d{1,2})\s+(\w+)\s+(\d{4})',
            r'(?:three|six|nine|twelve) months ended\s+(\w+)\s+(\d{1,2}),?\s+(\d{4})'
        ]
        
        for pattern in patterns:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                groups = match.groups()
                
                # Parse based on group order
                if groups[0].isdigit():  # DD Month YYYY format
                    day = groups[0].zfill(2)
                    month_str = groups[1]
                    year = groups[2]
                else:  # Month DD YYYY format
                    month_str = groups[0]
                    day = groups[1].zfill(2)
                    year = groups[2]
                
                # Convert month name to number
                months = {
                    'january': '01', 'february': '02', 'march': '03',
                    'april': '04', 'may': '05', 'june': '06',
                    'july': '07', 'august': '08', 'september': '09',
                    'october': '10', 'november': '11', 'december': '12',
                    'jan': '01', 'feb': '02', 'mar': '03', 'apr': '04',
                    'may': '05', 'jun': '06', 'jul': '07', 'aug': '08',
                    'sep': '09', 'oct': '10', 'nov': '11', 'dec': '12'
                }
                
                month = months.get(month_str.lower())
                if not month:
                    continue
                
                # Determine if it's a quarter or full year
                month_num = int(month)
                if month_num in [3, 6, 9, 12]:
                    if month_num == 12 and day == '31':
                        period_label = f"FY {year}"
                    else:
                        quarter = month_num // 3
                        period_label = f"Q{quarter} {year}"
                else:
                    # Default to month-based period
                    period_label = f"{month_str.title()} {year}"
                
                period_date = f"{year}-{month}-{day}"
                return period_label, period_date
        
        # If no period found in content, return unknown
        return 'Unknown Period', None
    
    def _extract_metric(self, content, metric_key, company, period_label, period_date, source_doc):
        """Extract a specific metric using regex patterns"""
        
        facts = []
        pattern = self.patterns.get(metric_key)
        
        if not pattern:
            return facts
        
        matches = re.finditer(pattern, content, re.IGNORECASE | re.MULTILINE)
        
        for match_num, match in enumerate(matches):
            # Special handling for borrowings (sum of two groups)
            if metric_key == 'borrowings' and len(match.groups()) >= 2:
                current_str = match.group(1)
                non_current_str = match.group(2)
                current = canonicalize_number(current_str)
                non_current = canonicalize_number(non_current_str)
                
                if current is not None and non_current is not None:
                    value = current + non_current  # Total borrowings
                    
                    fact = {
                        'company': company,
                        'key': metric_key,
                        'value': value,
                        'confidence': 0.95,
                        'period_label': period_label,
                        'period_date': period_date or 'unknown',
                        'source_doc': source_doc,
                        'source_page': match_num + 1,
                        'method': 'regex_extract'
                    }
                    fact['fact_id'] = generate_fact_id(
                        company, metric_key, period_label, str(value)
                    )
                    facts.append(fact)
                    break
            # Regular extraction for other metrics
            elif len(match.groups()) >= 1:
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
                        'period_date': period_date or 'unknown',
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
                            'period_date': period_date or 'unknown',
                            'source_doc': source_doc,
                            'source_page': 999,  # Calculated
                            'method': 'calculated'
                        }
                        fact['fact_id'] = generate_fact_id(
                            company, 'ebitda_margin', period_label, str(margin)
                        )
                        facts.append(fact)
        
        return facts
    
    def _calculate_total_assets(self, all_facts, company, period_label, period_date, source_doc):
        """Calculate total assets from current + non-current if not directly available"""
        
        facts = []
        
        # Check if we already have total_assets
        has_total_assets = any(f['key'] == 'total_assets' for f in all_facts)
        if has_total_assets:
            return facts
        
        # Find current_assets and non_current_assets for this period
        current_assets = None
        non_current_assets = None
        
        for fact in all_facts:
            if fact['company'] == company and fact['period_label'] == period_label:
                if fact['key'] == 'current_assets':
                    current_assets = fact
                elif fact['key'] == 'non_current_assets':
                    non_current_assets = fact
        
        # Calculate total_assets if we have both components
        if current_assets and non_current_assets:
            total_value = current_assets['value'] + non_current_assets['value']
            
            fact = {
                'company': company,
                'key': 'total_assets',
                'value': total_value,
                'confidence': min(current_assets['confidence'], non_current_assets['confidence']) * 0.95,
                'period_label': period_label,
                'period_date': period_date or 'unknown',
                'source_doc': source_doc,
                'source_page': 999,  # Calculated
                'method': 'calculated'
            }
            fact['fact_id'] = generate_fact_id(
                company, 'total_assets', period_label, str(total_value)
            )
            facts.append(fact)
        
        return facts
    
    def _calculate_total_debt(self, all_facts, company, period_label, period_date, source_doc):
        """Use borrowings as total_debt if total_debt not found"""
        
        facts = []
        
        # Check if we already have total_debt
        has_total_debt = any(f['key'] == 'total_debt' for f in all_facts)
        if has_total_debt:
            return facts
        
        # Find borrowings for this period
        borrowings = None
        for fact in all_facts:
            if fact['company'] == company and fact['period_label'] == period_label:
                if fact['key'] == 'borrowings':
                    borrowings = fact
                    break
        
        # Use borrowings as total_debt
        if borrowings:
            fact = {
                'company': company,
                'key': 'total_debt',
                'value': borrowings['value'],
                'confidence': borrowings['confidence'] * 0.9,  # Slightly lower confidence
                'period_label': period_label,
                'period_date': period_date or 'unknown',
                'source_doc': source_doc,
                'source_page': 999,  # Calculated
                'method': 'calculated'
            }
            fact['fact_id'] = generate_fact_id(
                company, 'total_debt', period_label, str(borrowings['value'])
            )
            facts.append(fact)
        
        return facts
    
    def _calculate_advanced_metrics(self, all_facts, company, period_label, period_date, source_doc):
        """Calculate advanced financial metrics"""
        
        facts = []
        
        # Create metric lookup for this period
        period_metrics = {}
        for fact in all_facts:
            if fact['company'] == company and fact['period_label'] == period_label:
                period_metrics[fact['key']] = fact
        
        # 1. Calculate NOPAT (Net Operating Profit After Tax)
        if 'operating_income' in period_metrics and 'tax_expense' in period_metrics and 'net_income' in period_metrics:
            operating_income = period_metrics['operating_income']['value']
            tax_expense = period_metrics['tax_expense']['value']
            net_income = period_metrics['net_income']['value']
            
            # Estimate tax rate from net income and tax expense
            if operating_income > 0:
                # Approximate tax rate
                effective_tax_rate = tax_expense / (net_income + tax_expense) if (net_income + tax_expense) > 0 else 0.25
                nopat = operating_income * (1 - effective_tax_rate)
                
                fact = {
                    'company': company,
                    'key': 'nopat',
                    'value': nopat,
                    'confidence': 0.8,
                    'period_label': period_label,
                    'period_date': period_date or 'unknown',
                    'source_doc': source_doc,
                    'source_page': 999,
                    'method': 'calculated'
                }
                fact['fact_id'] = generate_fact_id(company, 'nopat', period_label, str(nopat))
                facts.append(fact)
        
        # 2. Calculate Invested Capital (Total Debt + Total Equity - Cash)
        if 'total_debt' in period_metrics and 'total_equity' in period_metrics:
            total_debt = period_metrics['total_debt']['value']
            total_equity = period_metrics['total_equity']['value']
            cash = period_metrics.get('cash', {}).get('value', 0)
            
            invested_capital = total_debt + total_equity - cash
            
            fact = {
                'company': company,
                'key': 'invested_capital',
                'value': invested_capital,
                'confidence': 0.85,
                'period_label': period_label,
                'period_date': period_date or 'unknown',
                'source_doc': source_doc,
                'source_page': 999,
                'method': 'calculated'
            }
            fact['fact_id'] = generate_fact_id(company, 'invested_capital', period_label, str(invested_capital))
            facts.append(fact)
        
        # 3. Calculate Total Capex (tangible + intangible)
        if 'capex' in period_metrics:
            total_capex = period_metrics['capex']['value']
            if 'intangible_capex' in period_metrics:
                total_capex += period_metrics['intangible_capex']['value']
            
            fact = {
                'company': company,
                'key': 'total_capex',
                'value': total_capex,
                'confidence': 0.9,
                'period_label': period_label,
                'period_date': period_date or 'unknown',
                'source_doc': source_doc,
                'source_page': 999,
                'method': 'calculated'
            }
            fact['fact_id'] = generate_fact_id(company, 'total_capex', period_label, str(total_capex))
            facts.append(fact)
        
        # 4. Calculate FCF if we have operating cash flow and capex
        if 'operating_cash_flow' in period_metrics and ('capex' in period_metrics or 'total_capex' in period_metrics):
            ocf = period_metrics['operating_cash_flow']['value']
            capex = period_metrics.get('total_capex', period_metrics.get('capex', {})).get('value', 0)
            
            fcf = ocf - capex
            
            fact = {
                'company': company,
                'key': 'fcf',
                'value': fcf,
                'confidence': 0.9,
                'period_label': period_label,
                'period_date': period_date or 'unknown',
                'source_doc': source_doc,
                'source_page': 999,
                'method': 'calculated'
            }
            fact['fact_id'] = generate_fact_id(company, 'fcf', period_label, str(fcf))
            facts.append(fact)
        
        # 5. Calculate Working Capital Days (DSO, DIO, DPO)
        if 'revenue' in period_metrics and 'trade_receivables' in period_metrics:
            revenue = period_metrics['revenue']['value']
            receivables = period_metrics['trade_receivables']['value']
            
            # DSO = (Receivables / Revenue) * 90 days (for quarterly)
            dso = (receivables / revenue) * 90 if revenue > 0 else 0
            
            fact = {
                'company': company,
                'key': 'dso',
                'value': dso,
                'confidence': 0.85,
                'period_label': period_label,
                'period_date': period_date or 'unknown',
                'source_doc': source_doc,
                'source_page': 999,
                'method': 'calculated'
            }
            fact['fact_id'] = generate_fact_id(company, 'dso', period_label, str(dso))
            facts.append(fact)
        
        if 'costs' in period_metrics and 'inventories' in period_metrics:
            cogs = period_metrics['costs']['value']
            inventory = period_metrics['inventories']['value']
            
            # DIO = (Inventory / COGS) * 90 days
            dio = (inventory / cogs) * 90 if cogs > 0 else 0
            
            fact = {
                'company': company,
                'key': 'dio',
                'value': dio,
                'confidence': 0.85,
                'period_label': period_label,
                'period_date': period_date or 'unknown',
                'source_doc': source_doc,
                'source_page': 999,
                'method': 'calculated'
            }
            fact['fact_id'] = generate_fact_id(company, 'dio', period_label, str(dio))
            facts.append(fact)
        
        if 'costs' in period_metrics and 'trade_payables' in period_metrics:
            cogs = period_metrics['costs']['value']
            payables = period_metrics['trade_payables']['value']
            
            # DPO = (Payables / COGS) * 90 days
            dpo = (payables / cogs) * 90 if cogs > 0 else 0
            
            fact = {
                'company': company,
                'key': 'dpo',
                'value': dpo,
                'confidence': 0.85,
                'period_label': period_label,
                'period_date': period_date or 'unknown',
                'source_doc': source_doc,
                'source_page': 999,
                'method': 'calculated'
            }
            fact['fact_id'] = generate_fact_id(company, 'dpo', period_label, str(dpo))
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
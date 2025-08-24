"""Ultimate fact extractor with unit normalization and smart deduplication"""

import re
from typing import List, Dict, Optional, Tuple
import hashlib

def canonicalize_number(value_str: str) -> Optional[float]:
    """Convert various number formats to float"""
    if not value_str:
        return None
    
    try:
        # Remove common formatting
        cleaned = value_str.replace(',', '').replace('(', '-').replace(')', '').strip()
        
        # Handle special cases
        if cleaned == '-' or cleaned == '':
            return None
            
        # Convert to float
        value = float(cleaned)
        
        return value
    except:
        return None

def generate_fact_id(company: str, key: str, period: str, value: str) -> str:
    """Generate unique fact ID"""
    fact_string = f"{company}:{key}:{period}:{value}"
    return hashlib.md5(fact_string.encode()).hexdigest()[:16]

class FactExtractorV4:
    """Ultimate fact extractor with intelligent unit handling"""
    
    def __init__(self):
        # Define metrics with their unit context
        self.metric_configs = {
            'revenue': {
                'patterns': [
                    # Pattern with explicit column headers to determine units
                    (r'Revenue\s*\|\s*\d+\s*\|\s*([\d,]+)', 'contextual'),
                    (r'Total\s+Revenue\s*\|\s*([\d,]+)', 'contextual'),
                    (r'Mobile Revenue.*?\|\s*([\d,]+)', 'contextual'),
                ],
                'expected_range_millions': (100, 100000),  # Expected range in millions
                'keywords': ['revenue', 'sales', 'turnover'],
                'exclude': ['growth', 'margin', '%', 'ratio', 'per']
            },
            'total_assets': {
                'patterns': [
                    (r'Total assets\s*\|\s*([\d,]+)', 'contextual'),
                    (r'Jumlah aset\s*\|\s*([\d,]+)', 'contextual'),
                ],
                'expected_range_millions': (1000, 500000),
                'keywords': ['assets'],
                'exclude': ['growth', 'turnover', 'return', 'classified']
            },
            'total_equity': {
                'patterns': [
                    (r'Total equity\s*\|\s*([\d,]+)', 'contextual'),
                    (r'Total shareholders.*?\|\s*([\d,]+)', 'contextual'),
                    (r'Jumlah ekuitas\s*\|\s*([\d,]+)', 'contextual'),
                ],
                'expected_range_millions': (500, 200000),
                'keywords': ['equity'],
                'exclude': ['ratio', 'return', '%']
            },
            'operating_cash_flow': {
                'patterns': [
                    # Skip note column by looking for larger numbers
                    (r'(?:Total )?[Cc]ash flows? from.*?operating activities\s*\|[^|]*\|\s*([\d,]+)', 'contextual'),
                    (r'Net cash from operating\s*\|[^|]*\|\s*([\d,]+)', 'contextual'),
                ],
                'expected_range_millions': (-10000, 50000),
                'keywords': ['cash', 'operating'],
                'exclude': ['margin', 'conversion', '%']
            },
            'capex': {
                'patterns': [
                    (r'Capital expenditure\s*\|\s*([\d,]+)', 'contextual'),
                    (r'Purchase of PPE.*?\|\s*\(?([\d,]+)\)?', 'contextual'),
                    (r'Acquisition of property.*?\|\s*\(?([\d,]+)\)?', 'contextual'),
                ],
                'expected_range_millions': (100, 20000),
                'keywords': ['capex', 'capital', 'expenditure'],
                'exclude': ['intensity', '%', 'ratio', 'guidance']
            },
            'ebitda': {
                'patterns': [
                    (r'EBITDA\s*\|\s*([\d,]+)', 'contextual'),
                    (r'Earnings before interest.*?\|\s*([\d,]+)', 'contextual'),
                ],
                'expected_range_millions': (-5000, 50000),
                'keywords': ['ebitda'],
                'exclude': ['margin', '%', 'ratio', 'coverage']
            },
            'net_income': {
                'patterns': [
                    (r'(?:Net profit|Net income|PAT)\s*\|\s*([\d,]+)', 'contextual'),
                    (r'Profit for the (?:year|period)\s*\|\s*([\d,]+)', 'contextual'),
                    (r'Profit attributable to.*?owners\s*\|\s*([\d,]+)', 'contextual'),
                ],
                'expected_range_millions': (-5000, 20000),
                'keywords': ['profit', 'income', 'pat'],
                'exclude': ['margin', '%', 'per share', 'before', 'gross', 'operating']
            },
            'total_debt': {
                'patterns': [
                    (r'Total (?:debt|borrowings)\s*\|\s*([\d,]+)', 'contextual'),
                    (r'Borrowings.*?[Tt]otal\s*\|\s*([\d,]+)', 'contextual'),
                ],
                'expected_range_millions': (0, 100000),
                'keywords': ['debt', 'borrowings'],
                'exclude': ['ratio', 'service', 'cost', 'current', 'non-current']
            },
            'cash': {
                'patterns': [
                    (r'Cash and cash equivalents\s*\|\s*([\d,]+)', 'contextual'),
                    (r'Kas dan setara kas\s*\|\s*([\d,]+)', 'contextual'),
                    (r'deposits, cash and bank\s*\|\s*([\d,]+)', 'contextual'),
                ],
                'expected_range_millions': (10, 20000),
                'keywords': ['cash', 'deposits'],
                'exclude': ['flow', 'generated', 'used', 'from', 'in']
            }
        }
    
    def extract_facts(self, md_content: str, company_name: str, file_name: str) -> List[Dict]:
        """Extract facts with intelligent unit detection"""
        
        facts = []
        
        # Detect document units (thousands vs millions)
        units_multiplier = self._detect_units(md_content)
        
        # Extract period
        period_label, period_date = self._extract_period(file_name, md_content)
        
        # Extract each metric
        for metric_name, config in self.metric_configs.items():
            metric_facts = self._extract_metric_with_units(
                md_content, metric_name, config, units_multiplier,
                company_name, period_label, period_date, file_name
            )
            facts.extend(metric_facts)
        
        # Calculate derived metrics
        derived_facts = self._calculate_derived_metrics(
            facts, company_name, period_label, period_date, file_name
        )
        facts.extend(derived_facts)
        
        # Smart deduplication
        unique_facts = self._smart_deduplicate(facts)
        
        return unique_facts
    
    def _detect_units(self, content: str) -> float:
        """Detect if financial statements are in thousands or millions"""
        
        # Common patterns indicating units
        thousands_patterns = [
            r"RM'000",  # Malaysian format
            r"Rs'000",  # Indian/Sri Lankan format  
            r"USD'000",  # US format
            r'\(\'000\)',
            r'(?:expressed|stated|presented) in thousands',
            r'dalam ribuan',
            r'in thousand[s]? of',
            r'000s?\s*(?:RM|USD|Rs)',
        ]
        
        millions_patterns = [
            r"RM'million",
            r"Rs million",
            r'(?:expressed|stated|presented) in millions',
            r'dalam jutaan',
            r'in million[s]? of',
            r'(?<!\'000\s)million[s]?\s*(?:RM|USD|Rs)',  # Avoid matching '000 millions
        ]
        
        # Check first 10000 characters for unit indicators (tables might be later)
        header = content[:10000]
        
        # Look for definitive indicators in table headers
        for pattern in thousands_patterns:
            if re.search(pattern, header, re.IGNORECASE):
                return 0.001  # Convert thousands to millions
        
        for pattern in millions_patterns:
            if re.search(pattern, header, re.IGNORECASE):
                return 1.0  # Already in millions
        
        # Default assumption: already in millions (safer for large companies)
        return 1.0
    
    def _extract_metric_with_units(self, content: str, metric_name: str, config: Dict,
                                   units_multiplier: float, company: str, 
                                   period_label: str, period_date: str, 
                                   source_doc: str) -> List[Dict]:
        """Extract metric and normalize units"""
        
        facts = []
        lines = content.split('\n')
        
        for line_num, line in enumerate(lines):
            # Skip excluded patterns
            skip = False
            for exclude in config.get('exclude', []):
                if re.search(exclude, line, re.IGNORECASE):
                    skip = True
                    break
            
            if skip:
                continue
            
            # Check if line contains relevant keywords
            relevant = False
            for keyword in config.get('keywords', []):
                if keyword.lower() in line.lower():
                    relevant = True
                    break
            
            if not relevant and config.get('keywords'):
                continue
            
            # Try each pattern
            for pattern, unit_type in config['patterns']:
                match = re.search(pattern, line, re.IGNORECASE)
                if match:
                    value_str = match.group(1)
                    raw_value = canonicalize_number(value_str)
                    
                    if raw_value is not None:
                        # Apply units conversion
                        value_in_millions = raw_value * units_multiplier
                        
                        # Check if value is in expected range
                        min_val, max_val = config['expected_range_millions']
                        
                        # If way outside range, might be wrong units
                        if value_in_millions < min_val / 100 or value_in_millions > max_val * 100:
                            # Try alternative unit interpretation
                            if units_multiplier == 0.001:  # Currently thousands
                                # Maybe it's already in millions
                                alt_value = raw_value
                            else:  # Currently millions
                                # Maybe it's in thousands
                                alt_value = raw_value * 0.001
                            
                            # Check if alternative is better
                            if min_val <= alt_value <= max_val:
                                value_in_millions = alt_value
                        
                        # Final validation
                        if min_val / 10 <= value_in_millions <= max_val * 10:
                            fact = {
                                'company': company,
                                'key': metric_name,
                                'value': value_in_millions,  # Always store in millions
                                'confidence': 0.95,
                                'period_label': period_label,
                                'period_date': period_date or 'unknown',
                                'source_doc': source_doc,
                                'source_page': line_num // 50 + 1,
                                'method': 'regex_extract',
                                'raw_value': raw_value,
                                'units_applied': units_multiplier
                            }
                            fact['fact_id'] = generate_fact_id(
                                company, metric_name, period_label, str(value_in_millions)
                            )
                            facts.append(fact)
                            
                            # Only take first good match per metric
                            return facts
        
        return facts
    
    def _smart_deduplicate(self, facts: List[Dict]) -> List[Dict]:
        """Smart deduplication keeping the most reasonable values"""
        
        fact_groups = {}
        
        # Group by company, metric, and period
        for fact in facts:
            key = (fact['company'], fact['key'], fact['period_label'])
            if key not in fact_groups:
                fact_groups[key] = []
            fact_groups[key].append(fact)
        
        deduped = []
        
        for key, group in fact_groups.items():
            if len(group) == 1:
                deduped.append(group[0])
            else:
                # Multiple values for same metric/period - choose best
                metric_name = key[1]
                
                # For calculated metrics, prefer calculated
                calculated = [f for f in group if f['method'] == 'calculated']
                if calculated:
                    deduped.append(calculated[0])
                    continue
                
                # For ratios/margins, prefer values between 0 and 1
                if any(x in metric_name for x in ['margin', 'ratio', 'roe', 'roa', 'roic']):
                    reasonable = [f for f in group if 0 <= f['value'] <= 1]
                    if reasonable:
                        # Pick highest confidence
                        deduped.append(max(reasonable, key=lambda x: x['confidence']))
                        continue
                
                # For absolute values, prefer larger (likely correct units)
                # But not absurdly large
                if metric_name in ['revenue', 'total_assets', 'total_equity']:
                    # Filter out tiny values (likely wrong units)
                    significant = [f for f in group if f['value'] > 100]
                    if significant:
                        # But also filter out absurdly large
                        reasonable = [f for f in significant if f['value'] < 10000000]
                        if reasonable:
                            deduped.append(max(reasonable, key=lambda x: x['value']))
                        else:
                            deduped.append(min(significant, key=lambda x: x['value']))
                        continue
                
                # Default: highest confidence, then larger value
                deduped.append(max(group, key=lambda x: (x['confidence'], abs(x['value']))))
        
        return deduped
    
    def _extract_period(self, file_name: str, content: str) -> Tuple[str, Optional[str]]:
        """Extract period from content"""
        
        # Patterns for period extraction
        patterns = [
            (r'(?:year|period) ended (\d{1,2})\s+(\w+)\s+(\d{4})', 'date'),
            (r'as (?:at|of) (\d{1,2})\s+(\w+)\s+(\d{4})', 'date'),
            (r'(\d{4})\s+(?:Annual|Full Year)', 'year'),
            (r'FY\s*(\d{4})', 'year'),
            (r'Q([1-4])\s*(\d{4})', 'quarter'),
        ]
        
        for pattern, ptype in patterns:
            match = re.search(pattern, content[:5000], re.IGNORECASE)
            if match:
                if ptype == 'date':
                    day = match.group(1).zfill(2)
                    month_str = match.group(2).lower()
                    year = match.group(3)
                    
                    months = {
                        'jan': '01', 'feb': '02', 'mar': '03', 'apr': '04',
                        'may': '05', 'jun': '06', 'jul': '07', 'aug': '08',
                        'sep': '09', 'oct': '10', 'nov': '11', 'dec': '12'
                    }
                    
                    month = months.get(month_str[:3], '12')
                    period_date = f'{year}-{month}-{day}'
                    
                    if month == '12':
                        period_label = f'FY {year}'
                    elif month == '03':
                        period_label = f'Q1 {year}'
                    elif month == '06':
                        period_label = f'Q2 {year}'
                    elif month == '09':
                        period_label = f'Q3 {year}'
                    else:
                        period_label = f'Period {year}'
                    
                    return period_label, period_date
                
                elif ptype == 'year':
                    year = match.group(1)
                    return f'FY {year}', f'{year}-12-31'
                
                elif ptype == 'quarter':
                    quarter = match.group(1)
                    year = match.group(2)
                    month = str(int(quarter) * 3).zfill(2)
                    return f'Q{quarter} {year}', f'{year}-{month}-31'
        
        # Fallback
        if '2025' in file_name or '25Q' in file_name:
            return 'Q1 2025', '2025-03-31'
        elif '2024' in file_name or '24' in file_name:
            return 'FY 2024', '2024-12-31'
        
        return 'Unknown Period', None
    
    def _calculate_derived_metrics(self, facts: List[Dict], company: str,
                                   period_label: str, period_date: str,
                                   source_doc: str) -> List[Dict]:
        """Calculate ratios and derived metrics"""
        
        derived = []
        
        # Group facts by period
        period_facts = {}
        for fact in facts:
            if fact['period_label'] == period_label:
                period_facts[fact['key']] = fact['value']
        
        # Calculate FCF properly
        if 'operating_cash_flow' in period_facts and 'capex' in period_facts:
            ocf = period_facts['operating_cash_flow']
            capex = period_facts['capex']
            fcf = ocf - capex  # Capex is positive, so subtract
            
            derived.append({
                'company': company,
                'key': 'free_cash_flow',
                'value': fcf,
                'confidence': 0.9,
                'period_label': period_label,
                'period_date': period_date or 'unknown',
                'source_doc': source_doc,
                'source_page': 999,
                'method': 'calculated',
                'formula': f'OCF ({ocf:.1f}) - CapEx ({capex:.1f})',
                'fact_id': generate_fact_id(company, 'free_cash_flow', period_label, str(fcf))
            })
        
        # Calculate margins as percentages
        if 'ebitda' in period_facts and 'revenue' in period_facts:
            if period_facts['revenue'] > 0:
                margin = period_facts['ebitda'] / period_facts['revenue']
                if 0 <= margin <= 1:  # Sanity check
                    derived.append({
                        'company': company,
                        'key': 'ebitda_margin',
                        'value': margin,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date or 'unknown',
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'formula': f"EBITDA ({period_facts['ebitda']:.1f}) / Revenue ({period_facts['revenue']:.1f})",
                        'fact_id': generate_fact_id(company, 'ebitda_margin', period_label, str(margin))
                    })
        
        # Calculate ROE
        if 'net_income' in period_facts and 'total_equity' in period_facts:
            if period_facts['total_equity'] > 0:
                roe = period_facts['net_income'] / period_facts['total_equity']
                if -1 <= roe <= 2:  # Sanity check
                    derived.append({
                        'company': company,
                        'key': 'roe',
                        'value': roe,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date or 'unknown',
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'formula': f"Net Income ({period_facts['net_income']:.1f}) / Equity ({period_facts['total_equity']:.1f})",
                        'fact_id': generate_fact_id(company, 'roe', period_label, str(roe))
                    })
        
        # Calculate leverage
        if 'total_assets' in period_facts and 'total_equity' in period_facts:
            if period_facts['total_equity'] > 0:
                leverage = period_facts['total_assets'] / period_facts['total_equity']
                if 1 <= leverage <= 50:  # Sanity check
                    derived.append({
                        'company': company,
                        'key': 'financial_leverage',
                        'value': leverage,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date or 'unknown',
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'formula': f"Assets ({period_facts['total_assets']:.1f}) / Equity ({period_facts['total_equity']:.1f})",
                        'fact_id': generate_fact_id(company, 'financial_leverage', period_label, str(leverage))
                    })
        
        # Calculate D/E ratio
        if 'total_debt' in period_facts and 'total_equity' in period_facts:
            if period_facts['total_equity'] > 0 and period_facts['total_debt'] > 100:  # Debt must be significant
                de_ratio = period_facts['total_debt'] / period_facts['total_equity']
                if 0 <= de_ratio <= 20:  # Sanity check
                    derived.append({
                        'company': company,
                        'key': 'debt_to_equity',
                        'value': de_ratio,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date or 'unknown',
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'formula': f"Debt ({period_facts['total_debt']:.1f}) / Equity ({period_facts['total_equity']:.1f})",
                        'fact_id': generate_fact_id(company, 'debt_to_equity', period_label, str(de_ratio))
                    })
        
        return derived
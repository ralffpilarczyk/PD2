"""Enhanced fact extractor with better table parsing and validation"""

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
        
        # Validate reasonable ranges
        if abs(value) < 0.0001 and value != 0:  # Too small, likely a ratio extracted wrong
            return None
        if abs(value) > 1e15:  # Too large, likely garbage
            return None
            
        return value
    except:
        return None

def generate_fact_id(company: str, key: str, period: str, value: str) -> str:
    """Generate unique fact ID"""
    fact_string = f"{company}:{key}:{period}:{value}"
    return hashlib.md5(fact_string.encode()).hexdigest()[:16]

class FactExtractorV3:
    """Enhanced fact extractor with intelligent table parsing"""
    
    def __init__(self):
        # Define metric configurations with expected ranges
        self.metric_configs = {
            'revenue': {
                'patterns': [
                    r'(?:^|\|)\s*(?:Total\s+)?Revenue\s*\|\s*([\d,]+(?:\.\d+)?)\s*\|',  # Table row
                    r'Revenue\s+(?:was\s+)?(?:RM|USD|Rs)?\s*([\d,]+(?:\.\d+)?)\s*(?:million|billion)?',  # Narrative
                    r'Total\s+(?:operating\s+)?revenue[^|]*\|\s*([\d,]+(?:\.\d+)?)',  # With description
                ],
                'min_value': 100,  # Minimum reasonable revenue
                'max_value': 1e9,  # Maximum reasonable revenue (in millions)
                'skip_if_contains': ['growth', 'margin', '%', 'ratio', 'per']
            },
            'ebitda': {
                'patterns': [
                    r'EBITDA\s*\|\s*([\d,]+(?:\.\d+)?)\s*\|',  # Simple table
                    r'EBITDA[^|]{0,30}\|\s*([\d,]+(?:\.\d+)?)',  # With some text
                    r'Earnings\s+before[^|]+\|\s*([\d,]+(?:\.\d+)?)',  # Full name
                ],
                'min_value': -1e6,
                'max_value': 1e8,
                'skip_if_contains': ['margin', '%', 'ratio', 'coverage']
            },
            'total_assets': {
                'patterns': [
                    r'(?:Total\s+assets|Jumlah\s+aset)\s*\|\s*([\d,]+(?:\.\d+)?)\s*\|',
                    r'Total\s+assets[^|]*\|\s*([\d,]+(?:\.\d+)?)',
                ],
                'min_value': 1000,
                'max_value': 1e10,
                'skip_if_contains': ['growth', 'turnover', 'return']
            },
            'total_equity': {
                'patterns': [
                    r'(?:Total\s+equity|Total\s+shareholders[^|]+|Jumlah\s+ekuitas)\s*\|\s*([\d,]+(?:\.\d+)?)\s*\|',
                    r'Total\s+equity[^|]*\|\s*([\d,]+(?:\.\d+)?)',
                ],
                'min_value': 100,
                'max_value': 1e9,
                'skip_if_contains': ['ratio', 'return', '%']
            },
            'net_income': {
                'patterns': [
                    r'(?:Net\s+profit|Net\s+income|PAT|Profit\s+for\s+the\s+(?:year|period))\s*\|\s*([\d,]+(?:\.\d+)?)\s*\|',
                    r'Profit\s+attributable\s+to[^|]+\|\s*([\d,]+(?:\.\d+)?)',
                ],
                'min_value': -1e6,
                'max_value': 1e8,
                'skip_if_contains': ['margin', '%', 'per\s+share']
            },
            'operating_income': {
                'patterns': [
                    r'(?:Operating\s+(?:income|profit)|EBIT(?!DA))\s*\|\s*([\d,]+(?:\.\d+)?)\s*\|',
                    r'EBIT\s*\|\s*([\d,]+(?:\.\d+)?)\s*\|',
                ],
                'min_value': -1e6,
                'max_value': 1e8,
                'skip_if_contains': ['margin', '%', 'coverage']
            },
            'cash': {
                'patterns': [
                    r'(?:Cash\s+and\s+cash\s+equivalents|Kas\s+dan\s+setara\s+kas)\s*\|\s*([\d,]+(?:\.\d+)?)\s*\|',
                    r'Total\s+cash[^|]*\|\s*([\d,]+(?:\.\d+)?)',
                ],
                'min_value': 0,
                'max_value': 1e8,
                'skip_if_contains': ['flow', 'generated', 'used']
            },
            'total_debt': {
                'patterns': [
                    r'(?:Total\s+(?:debt|borrowings)|Borrowings\s+-\s+Total)\s*\|\s*([\d,]+(?:\.\d+)?)\s*\|',
                    r'Total\s+(?:financial\s+)?debt[^|]*\|\s*([\d,]+(?:\.\d+)?)',
                ],
                'min_value': 0,
                'max_value': 1e9,
                'skip_if_contains': ['ratio', 'service', 'cost']
            },
            'capex': {
                'patterns': [
                    r'(?:Capital\s+expenditure|Capex|Purchase\s+of\s+PPE)\s*\|?\s*\(?([\d,]+(?:\.\d+)?)\)?',
                    r'Acquisition\s+of\s+property[^|]*\|?\s*\(?([\d,]+(?:\.\d+)?)\)?',
                ],
                'min_value': 0,
                'max_value': 1e8,
                'skip_if_contains': ['intensity', '%', 'ratio', 'guidance']
            },
            'operating_cash_flow': {
                'patterns': [
                    r'(?:Cash\s+from\s+operating|Operating\s+cash\s+flow|Net\s+cash\s+from\s+operating)[^|]*\|\s*([\d,]+(?:\.\d+)?)',
                    r'CASH\s+FLOWS?\s+FROM\s+OPERATING[^|]*\|\s*([\d,]+(?:\.\d+)?)',
                ],
                'min_value': -1e7,
                'max_value': 1e8,
                'skip_if_contains': ['margin', 'conversion', '%']
            }
        }
    
    def extract_facts(self, md_content: str, company_name: str, file_name: str) -> List[Dict]:
        """Extract facts from markdown content with validation"""
        
        facts = []
        
        # Extract period information
        period_label, period_date = self._extract_period(file_name, md_content)
        
        # Extract each metric
        for metric_name, config in self.metric_configs.items():
            metric_facts = self._extract_metric_validated(
                md_content, metric_name, config, 
                company_name, period_label, period_date, file_name
            )
            facts.extend(metric_facts)
        
        # Calculate derived metrics
        derived_facts = self._calculate_derived_metrics(
            facts, company_name, period_label, period_date, file_name
        )
        facts.extend(derived_facts)
        
        # Remove duplicates
        unique_facts = self._deduplicate_facts(facts)
        
        return unique_facts
    
    def _extract_metric_validated(self, content: str, metric_name: str, config: Dict,
                                  company: str, period_label: str, period_date: str, 
                                  source_doc: str) -> List[Dict]:
        """Extract metric with validation"""
        
        facts = []
        
        # Check each pattern
        for pattern in config['patterns']:
            # Skip lines containing exclusion words
            lines = content.split('\n')
            for line in lines:
                # Check if line should be skipped
                skip = False
                for skip_word in config.get('skip_if_contains', []):
                    if re.search(skip_word, line, re.IGNORECASE):
                        skip = True
                        break
                
                if skip:
                    continue
                
                # Try to match pattern
                match = re.search(pattern, line, re.IGNORECASE)
                if match:
                    value_str = match.group(1)
                    value = canonicalize_number(value_str)
                    
                    if value is not None:
                        # Validate value range
                        if value < config.get('min_value', -1e10) or value > config.get('max_value', 1e10):
                            continue
                        
                        # Skip suspiciously small whole numbers (likely indices)
                        if metric_name == 'revenue' and value < 10:
                            continue
                        
                        fact = {
                            'company': company,
                            'key': metric_name,
                            'value': value,
                            'confidence': 0.95,
                            'period_label': period_label,
                            'period_date': period_date or 'unknown',
                            'source_doc': source_doc,
                            'source_page': 1,
                            'method': 'regex_extract'
                        }
                        fact['fact_id'] = generate_fact_id(
                            company, metric_name, period_label, str(value)
                        )
                        facts.append(fact)
                        
                        # Only take first good match
                        if facts:
                            return facts
        
        return facts
    
    def _extract_period(self, file_name: str, content: str) -> Tuple[str, Optional[str]]:
        """Extract period from content"""
        
        # Common patterns for period extraction
        patterns = [
            (r'(?:year|period)\s+ended\s+(\d{1,2})\s+(\w+)\s+(\d{4})', 'full'),
            (r'(?:as\s+(?:at|of))\s+(\d{1,2})\s+(\w+)\s+(\d{4})', 'full'),
            (r'(?:quarter|Q[1-4])\s+(\d{4})', 'quarter'),
            (r'FY\s*(\d{4})', 'year'),
            (r'(\d{4})\s+(?:Annual|Full\s+Year)', 'year'),
        ]
        
        for pattern, ptype in patterns:
            match = re.search(pattern, content[:5000], re.IGNORECASE)  # Check first 5000 chars
            if match:
                if ptype == 'full':
                    day = match.group(1).zfill(2)
                    month_str = match.group(2)
                    year = match.group(3)
                    
                    months = {
                        'january': '01', 'february': '02', 'march': '03',
                        'april': '04', 'may': '05', 'june': '06',
                        'july': '07', 'august': '08', 'september': '09',
                        'october': '10', 'november': '11', 'december': '12'
                    }
                    
                    month = months.get(month_str.lower()[:3], '12')
                    
                    # Determine period type
                    if month in ['03', '06', '09', '12']:
                        if month == '03':
                            period_label = f'Q1 {year}'
                        elif month == '06':
                            period_label = f'Q2 {year}'
                        elif month == '09':
                            period_label = f'Q3 {year}'
                        else:
                            period_label = f'FY {year}'
                    else:
                        period_label = f'Period {month}/{year}'
                    
                    period_date = f'{year}-{month}-{day}'
                    return period_label, period_date
                
                elif ptype == 'quarter':
                    year = match.group(1)
                    # Try to find quarter number
                    q_match = re.search(r'Q([1-4])', content[:5000])
                    if q_match:
                        quarter = q_match.group(1)
                        period_label = f'Q{quarter} {year}'
                        # Estimate date
                        month = str(int(quarter) * 3).zfill(2)
                        period_date = f'{year}-{month}-31'
                    else:
                        period_label = f'FY {year}'
                        period_date = f'{year}-12-31'
                    return period_label, period_date
                
                elif ptype == 'year':
                    year = match.group(1)
                    period_label = f'FY {year}'
                    period_date = f'{year}-12-31'
                    return period_label, period_date
        
        # Default fallback
        if '2025' in file_name or '25Q' in file_name:
            return 'Q1 2025', '2025-03-31'
        elif '2024' in file_name or '24' in file_name:
            return 'FY 2024', '2024-12-31'
        
        return 'Unknown Period', None
    
    def _calculate_derived_metrics(self, facts: List[Dict], company: str, 
                                   period_label: str, period_date: str, 
                                   source_doc: str) -> List[Dict]:
        """Calculate financial ratios and derived metrics"""
        
        derived = []
        
        # Group facts by metric
        metrics = {}
        for fact in facts:
            if fact['period_label'] == period_label:
                metrics[fact['key']] = fact['value']
        
        # Calculate key ratios
        if 'total_assets' in metrics and 'total_equity' in metrics:
            if metrics['total_equity'] > 0:
                leverage = metrics['total_assets'] / metrics['total_equity']
                if 1 < leverage < 50:  # Reasonable leverage range
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
                        'fact_id': generate_fact_id(company, 'financial_leverage', period_label, str(leverage))
                    })
        
        if 'net_income' in metrics and 'total_equity' in metrics:
            if metrics['total_equity'] > 0:
                roe = metrics['net_income'] / metrics['total_equity']
                if -1 < roe < 2:  # Reasonable ROE range
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
                        'fact_id': generate_fact_id(company, 'roe', period_label, str(roe))
                    })
        
        if 'net_income' in metrics and 'total_assets' in metrics:
            if metrics['total_assets'] > 0:
                roa = metrics['net_income'] / metrics['total_assets']
                if -0.5 < roa < 0.5:  # Reasonable ROA range
                    derived.append({
                        'company': company,
                        'key': 'roa',
                        'value': roa,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date or 'unknown',
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'fact_id': generate_fact_id(company, 'roa', period_label, str(roa))
                    })
        
        if 'ebitda' in metrics and 'revenue' in metrics:
            if metrics['revenue'] > 0:
                margin = metrics['ebitda'] / metrics['revenue']
                if 0 < margin < 1:  # Must be between 0 and 100%
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
                        'fact_id': generate_fact_id(company, 'ebitda_margin', period_label, str(margin))
                    })
        
        if 'total_debt' in metrics and 'total_equity' in metrics:
            if metrics['total_equity'] > 0:
                debt_equity = metrics['total_debt'] / metrics['total_equity']
                if 0 <= debt_equity < 20:  # Reasonable D/E range
                    derived.append({
                        'company': company,
                        'key': 'debt_to_equity',
                        'value': debt_equity,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date or 'unknown',
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'fact_id': generate_fact_id(company, 'debt_to_equity', period_label, str(debt_equity))
                    })
        
        return derived
    
    def _deduplicate_facts(self, facts: List[Dict]) -> List[Dict]:
        """Remove duplicate facts, keeping highest confidence"""
        
        fact_map = {}
        
        for fact in facts:
            key = (fact['company'], fact['key'], fact['period_label'])
            
            if key not in fact_map:
                fact_map[key] = fact
            else:
                # Keep fact with higher confidence or more reasonable value
                existing = fact_map[key]
                
                # Prefer calculated over extracted for margins
                if 'margin' in fact['key'] and fact['method'] == 'calculated':
                    fact_map[key] = fact
                # Otherwise keep higher confidence
                elif fact['confidence'] > existing['confidence']:
                    fact_map[key] = fact
                # If same confidence, prefer larger value (likely in correct units)
                elif fact['confidence'] == existing['confidence']:
                    if abs(fact['value']) > abs(existing['value']):
                        fact_map[key] = fact
        
        return list(fact_map.values())
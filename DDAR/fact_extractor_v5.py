"""Table-aware fact extractor with period/entity recognition"""

import re
from typing import List, Dict, Optional, Tuple, Any
import hashlib
from dataclasses import dataclass

@dataclass
class TableColumn:
    """Represents a column in a financial table"""
    index: int
    entity: str  # 'group' or 'company' or specific company name
    period: str  # '2024', '2023', 'Q1 2025', etc.
    is_note: bool = False
    
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

class FactExtractorV5:
    """Table-aware fact extractor with proper period handling"""
    
    def __init__(self):
        self.metric_patterns = {
            'revenue': [
                r'\|\s*Revenue\s+',  # Simple Revenue with padding
                r'\|\s*(?:Total |Operating |External operating |Mobile )?Revenue\s+',
            ],
            'operating_cash_flow': [
                r'\|\s*(?:Total )?[Cc]ash flows?.*?operating activities',
                r'\|\s*Net cash.*?operating',
            ],
            'total_assets': [
                r'\|\s*Total assets\s+',
                r'\|\s*Jumlah aset\s+',
            ],
            'total_equity': [
                r'\|\s*Total (?:equity|shareholders)',
                r'\|\s*Jumlah ekuitas\s+',
            ],
            'net_income': [
                r'\|\s*(?:Net profit|Net income|PAT)\s+',
                r'\|\s*Profit for the (?:year|period)\s+',
                r'\|\s*Profit attributable to.*?owners\s+',
            ],
            'ebitda': [
                r'\|\s*EBITDA\s+',
                r'\|\s*Earnings before interest\s+',
            ],
            'capex': [
                r'\|\s*Capital expenditure\s+',
                r'\|\s*Purchase of (?:PPE|property)\s+',
                r'\|\s*Acquisition of property\s+',
            ],
            'total_debt': [
                r'\|\s*Total (?:debt|borrowings)\s+',
                r'\|\s*Borrowings.*?[Tt]otal\s+',
            ],
            'cash': [
                r'\|\s*Cash and cash equivalents\s+',
                r'\|\s*(?:deposits, )?cash and bank\s+',
            ]
        }
        
        self.expected_ranges = {
            'revenue': (100, 100000),
            'total_assets': (1000, 500000),
            'total_equity': (500, 200000),
            'operating_cash_flow': (-10000, 50000),
            'net_income': (-5000, 20000),
            'ebitda': (-5000, 50000),
            'capex': (100, 20000),
            'total_debt': (100, 100000),
            'cash': (10, 20000),
        }
    
    def extract_facts(self, md_content: str, company_name: str, file_name: str) -> List[Dict]:
        """Extract facts with proper period awareness"""
        
        facts = []
        
        # Detect document units
        units_multiplier = self._detect_units(md_content)
        
        # Split into lines for table parsing
        lines = md_content.split('\n')
        
        # Process tables
        i = 0
        while i < len(lines):
            # Look for table headers
            if self._is_table_header(lines[i]):
                # Parse the table structure
                columns = self._parse_table_header(lines[i])
                if columns:
                    # Extract facts from this table
                    table_facts = self._extract_from_table(
                        lines, i, columns, company_name, 
                        file_name, units_multiplier
                    )
                    facts.extend(table_facts)
            i += 1
        
        # Calculate derived metrics
        derived_facts = self._calculate_derived_metrics(facts, company_name, file_name)
        facts.extend(derived_facts)
        
        # Deduplicate by keeping latest period
        unique_facts = self._deduplicate_by_period(facts)
        
        return unique_facts
    
    def _detect_units(self, content: str) -> float:
        """Detect if values are in thousands or millions"""
        
        thousands_patterns = [
            r"RM'000",
            r"Rs'000", 
            r"USD'000",
            r'\(\'000\)',
            r'in thousands',
        ]
        
        # Check first 10000 chars
        header = content[:10000]
        
        for pattern in thousands_patterns:
            if re.search(pattern, header, re.IGNORECASE):
                return 0.001  # Convert to millions
        
        return 1.0  # Assume millions
    
    def _is_table_header(self, line: str) -> bool:
        """Check if line is a table header with periods"""
        
        # Must have pipes for table structure
        if '|' not in line:
            return False
        
        # Look for year patterns in header (including with <br> tags)
        year_patterns = [
            r'20\d{2}',  # Years like 2024, 2023
            r'Q[1-4]\s*20\d{2}',  # Quarters like Q1 2024
            r'FY\s*20\d{2}',  # Fiscal years
        ]
        
        # Check if line has multiple year references
        year_count = 0
        for pattern in year_patterns:
            matches = re.findall(pattern, line)
            year_count += len(matches)
        
        # Need at least 2 years and should have currency indicator
        if year_count >= 2 and ("RM'000" in line or 'Rs' in line or 'USD' in line or 'Note' in line):
            return True
        
        return False
    
    def _parse_table_header(self, header_line: str) -> List[TableColumn]:
        """Parse table header to understand column structure"""
        
        columns = []
        
        # Clean up <br> tags
        header_line = header_line.replace('<br>', ' ')
        
        # Split by pipe
        parts = header_line.split('|')
        
        for i, part in enumerate(parts):
            part = part.strip()
            
            # Skip empty parts or very long text (likely row headers)
            if not part or len(part) > 100:
                continue
            
            # Check if it's a note column
            if re.match(r'^Note?s?$', part, re.IGNORECASE):
                columns.append(TableColumn(i, 'note', 'note', True))
                continue
            
            # Extract year/period from formats like "2024 RM'000"
            year_match = re.search(r'(20\d{2})', part)
            quarter_match = re.search(r'Q([1-4])\s*(20\d{2})', part)
            
            if not year_match and not quarter_match:
                continue
            
            # Determine entity (Group vs Company)
            # In typical statements, first occurrence is Group, later is Company
            # But we need to be smarter about this
            entity = 'group'  # Default to group
            
            # Get the year
            if quarter_match:
                period = f'Q{quarter_match.group(1)} {quarter_match.group(2)}'
            elif year_match:
                year = year_match.group(1)
                period = f'{year}'
                
                # Check if this is a duplicate year (likely Company column)
                existing_years = [col.period for col in columns if not col.is_note]
                if period in existing_years:
                    entity = 'company'
            else:
                continue
            
            columns.append(TableColumn(i, entity, period, False))
        
        return columns
    
    def _extract_from_table(self, lines: List[str], start_idx: int, 
                            columns: List[TableColumn], company_name: str,
                            file_name: str, units_multiplier: float) -> List[Dict]:
        """Extract facts from a specific table"""
        
        facts = []
        
        # Process lines after header
        for i in range(start_idx + 1, min(start_idx + 100, len(lines))):
            line = lines[i]
            
            # Stop at empty line or new table
            if not line.strip() or self._is_table_header(line):
                break
            
            # Check if line contains a metric we're interested in
            metric_name = self._identify_metric(line)
            if not metric_name:
                continue
            
            # Split line by pipes
            values = line.split('|')
            
            # Extract values for each column
            for col in columns:
                if col.is_note or col.index >= len(values):
                    continue
                
                # Only extract from latest period and group (not company-only)
                if col.entity == 'company':
                    continue
                
                value_str = values[col.index].strip()
                raw_value = canonicalize_number(value_str)
                
                if raw_value is not None:
                    # Apply units
                    value_in_millions = raw_value * units_multiplier
                    
                    # Validate range
                    min_val, max_val = self.expected_ranges.get(metric_name, (-1e10, 1e10))
                    
                    # Skip if outside reasonable range
                    if value_in_millions < min_val / 100 or value_in_millions > max_val * 100:
                        continue
                    
                    # Determine period label
                    if 'Q' in col.period:
                        period_label = col.period
                        # Estimate date
                        quarter = int(col.period[1])
                        year = col.period.split()[1]
                        month = str(quarter * 3).zfill(2)
                        period_date = f'{year}-{month}-31'
                    else:
                        period_label = f'FY {col.period}'
                        period_date = f'{col.period}-12-31'
                    
                    fact = {
                        'company': company_name,
                        'key': metric_name,
                        'value': value_in_millions,
                        'confidence': 0.95,
                        'period_label': period_label,
                        'period_date': period_date,
                        'source_doc': file_name,
                        'source_page': i // 50 + 1,
                        'method': 'table_extract',
                        'column_info': f'{col.entity}_{col.period}'
                    }
                    fact['fact_id'] = generate_fact_id(
                        company_name, metric_name, period_label, str(value_in_millions)
                    )
                    facts.append(fact)
        
        return facts
    
    def _identify_metric(self, line: str) -> Optional[str]:
        """Identify which metric this line contains"""
        
        for metric_name, patterns in self.metric_patterns.items():
            for pattern in patterns:
                if re.search(pattern, line, re.IGNORECASE):
                    # Check for exclusions
                    if any(excl in line.lower() for excl in ['growth', 'margin', '%', 'ratio', 'per']):
                        if metric_name not in ['ebitda', 'net_income']:  # These are ok with some exclusions
                            continue
                    return metric_name
        
        return None
    
    def _deduplicate_by_period(self, facts: List[Dict]) -> List[Dict]:
        """Keep only latest period for each metric"""
        
        # Group by company and metric
        fact_groups = {}
        for fact in facts:
            key = (fact['company'], fact['key'])
            if key not in fact_groups:
                fact_groups[key] = []
            fact_groups[key].append(fact)
        
        deduped = []
        
        for key, group in fact_groups.items():
            if len(group) == 1:
                deduped.append(group[0])
            else:
                # Sort by period and take latest
                # Priority: 2025 > 2024, Q2 > Q1, etc.
                def period_sort_key(fact):
                    period = fact['period_label']
                    if 'Q' in period:
                        # Extract year and quarter
                        match = re.match(r'Q(\d)\s+(\d{4})', period)
                        if match:
                            return (int(match.group(2)), int(match.group(1)))
                    elif 'FY' in period:
                        # Extract year
                        match = re.search(r'(\d{4})', period)
                        if match:
                            return (int(match.group(1)), 4)  # FY treated as Q4
                    return (0, 0)
                
                # Sort and take latest
                sorted_facts = sorted(group, key=period_sort_key, reverse=True)
                deduped.append(sorted_facts[0])
        
        return deduped
    
    def _calculate_derived_metrics(self, facts: List[Dict], company: str, 
                                   source_doc: str) -> List[Dict]:
        """Calculate ratios for each period"""
        
        derived = []
        
        # Group facts by period
        periods = {}
        for fact in facts:
            period = fact['period_label']
            if period not in periods:
                periods[period] = {}
            periods[period][fact['key']] = fact
        
        # Calculate metrics for each period
        for period_label, period_facts in periods.items():
            metrics = {k: v['value'] for k, v in period_facts.items()}
            
            # Get period date from any fact
            period_date = next(iter(period_facts.values()))['period_date']
            
            # Calculate FCF
            if 'operating_cash_flow' in metrics and 'capex' in metrics:
                fcf = metrics['operating_cash_flow'] - metrics['capex']
                derived.append({
                    'company': company,
                    'key': 'free_cash_flow',
                    'value': fcf,
                    'confidence': 0.9,
                    'period_label': period_label,
                    'period_date': period_date,
                    'source_doc': source_doc,
                    'source_page': 999,
                    'method': 'calculated',
                    'fact_id': generate_fact_id(company, 'free_cash_flow', period_label, str(fcf))
                })
            
            # Calculate margins
            if 'ebitda' in metrics and 'revenue' in metrics and metrics['revenue'] > 0:
                margin = metrics['ebitda'] / metrics['revenue']
                if 0 <= margin <= 1:
                    derived.append({
                        'company': company,
                        'key': 'ebitda_margin',
                        'value': margin,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date,
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'fact_id': generate_fact_id(company, 'ebitda_margin', period_label, str(margin))
                    })
            
            # Calculate ROE
            if 'net_income' in metrics and 'total_equity' in metrics and metrics['total_equity'] > 0:
                roe = metrics['net_income'] / metrics['total_equity']
                if -1 <= roe <= 2:
                    derived.append({
                        'company': company,
                        'key': 'roe',
                        'value': roe,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date,
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'fact_id': generate_fact_id(company, 'roe', period_label, str(roe))
                    })
        
        return derived
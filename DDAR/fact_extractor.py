"""Table-aware fact extractor with period/entity recognition"""

import re
import hashlib
from typing import List, Dict, Optional, Tuple, Any
from dataclasses import dataclass

from config import METRIC_RANGES
from utils import safe_divide, validate_metric_range

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
        # Check if it's in parentheses (negative)
        is_negative = '(' in value_str and ')' in value_str
        
        # Remove common formatting
        cleaned = value_str.replace(',', '').replace('(', '').replace(')', '').strip()
        
        # Handle special cases
        if cleaned == '-' or cleaned == '':
            return None
            
        # Convert to float
        value = float(cleaned)
        
        # Apply negative sign if needed
        if is_negative:
            value = -value
            
        return value
    except:
        return None

def generate_fact_id(company: str, key: str, period: str, value: str) -> str:
    """Generate unique fact ID using SHA256"""
    # Normalize inputs for consistent hashing
    company = str(company).lower().strip()
    key = str(key).lower().strip()
    period = str(period).lower().strip()
    value = str(value)
    
    # Create composite key
    fact_string = f"{company}:{key}:{period}:{value}"
    
    # Generate SHA256 hash
    from hashlib import sha256
    return sha256(fact_string.encode()).hexdigest()[:16]

class FactExtractorV5:
    """
    Table-aware fact extractor with proper period handling.
    
    This class extracts financial facts from markdown-converted financial documents,
    handling complex table structures with multiple periods and entities.
    """
    
    def __init__(self) -> None:
        self.metric_patterns = {
            'revenue': [
                r'\|\s*Revenue\s+',  # Simple Revenue with padding
                r'\|\s*(?:Total |Operating |External operating |Mobile )?[Rr]evenue\s+',
                r'\|\s*(?:Total )?[Ss]ales\s+',
                r'\|\s*[Tt]urnover\s+',
            ],
            'operating_cash_flow': [
                r'\|\s*(?:Total )?[Cc]ash flows?.*?operating activities',
                r'\|\s*Net cash.*?operating',
                r'\|\s*Operating cash flow\s+',
                r'\|\s*Cash.*?from.*?operations\s+',
            ],
            'total_assets': [
                r'\|\s*Total assets\s+',
                r'\|\s*Jumlah aset\s+',
                r'\|\s*Assets.*?[Tt]otal\s+',
            ],
            'total_equity': [
                r'\|\s*Total (?:equity|shareholders)',
                r'\|\s*Jumlah ekuitas\s+',
                r'\|\s*Shareholders.*?equity\s+',
                r'\|\s*Equity.*?[Tt]otal\s+',
            ],
            'net_income': [
                r'\|\s*(?:Net profit|Net income|PAT)\s+',
                r'\|\s*Profit(?:/\(Loss\))?.*?for the (?:financial )?year\s+',
                r'\|\s*Profit.*?attributable to.*?owners\s+',
                r'\|\s*Net earnings\s+',
                r'\|\s*Total comprehensive income.*?for\s+',
            ],
            'ebitda': [
                r'\|\s*EBITDA\s+',
                r'\|\s*Earnings before interest.*?tax\s+',
                r'\|\s*Operating profit before.*?depreciation\s+',
                r'\|\s*EBITDA \(normalised\)\s+',
            ],
            'ebit': [
                r'\|\s*EBIT\s+',
                r'\|\s*Operating (?:profit|income)\s+',
                r'\|\s*Earnings before interest and tax\s+',
                r'\|\s*(?:Profit|Income) from operations\s+',
                r'\|\s*Profit.*?before.*?taxation\s+',
            ],
            'capex': [
                r'\|\s*Capital expenditure\s+',
                r'\|\s*Purchase of (?:PPE|property)\s+',
                r'\|\s*Acquisition of property\s+',
                r'\|\s*Capex\s+',
                r'\|\s*Investment in.*?equipment\s+',
            ],
            'total_debt': [
                r'\|\s*Total (?:debt|borrowings)\s+',
                r'\|\s*Borrowings.*?[Tt]otal\s+',
                r'\|\s*(?:Long|Short).*?term.*?borrowings.*?[Tt]otal\s+',
                r'\|\s*Interest.*?bearing.*?liabilities\s+',
            ],
            'cash': [
                r'\|\s*Cash and cash equivalents\s+',
                r'\|\s*(?:deposits, )?cash and bank\s+',
                r'\|\s*Cash at bank\s+',
                r'\|\s*Liquid assets\s+',
            ],
            'working_capital': [
                r'\|\s*(?:Net )?[Ww]orking capital\s+',
                r'\|\s*Current assets.*?less.*?current liabilities\s+',
            ],
            'current_assets': [
                r'\|\s*(?:Total )?[Cc]urrent assets\s+',
                r'\|\s*Assets.*?[Cc]urrent.*?[Tt]otal\s+',
            ],
            'current_liabilities': [
                r'\|\s*(?:Total )?[Cc]urrent liabilities\s+',
                r'\|\s*Liabilities.*?[Cc]urrent.*?[Tt]otal\s+',
            ],
            'inventory': [
                r'\|\s*Inventor(?:y|ies)\s+',
                r'\|\s*Stock\s+',
                r'\|\s*Merchandise\s+',
            ],
            'receivables': [
                r'\|\s*(?:Trade )?[Rr]eceivables\s+',
                r'\|\s*Accounts receivable\s+',
                r'\|\s*Trade and other receivables\s+',
            ],
            'payables': [
                r'\|\s*(?:Trade )?[Pp]ayables\s+',
                r'\|\s*Accounts payable\s+',
                r'\|\s*Trade and other payables\s+',
            ],
            'depreciation': [
                r'\|\s*Depreciation(?:\s+and amortisation)?\s+',
                r'\|\s*Depreciation expense\s+',
            ],
            'interest_expense': [
                r'\|\s*(?:Finance |Interest )(?:cost|expense)s?\s+',
                r'\|\s*Interest paid\s+',
            ],
            'tax_expense': [
                r'\|\s*(?:Income )?[Tt]ax(?:ation| expense)?\s+',
                r'\|\s*Current tax\s+',
                r'\|\s*Taxation\s+',
            ],
            'finance_costs': [
                r'\|\s*Finance costs?\s+',
                r'\|\s*Interest expense\s+',
            ],
            'depreciation': [
                r'\|\s*.*?[Dd]epreciation.*?amortisation\s+',
                r'\|\s*Depreciation expense\s+',
            ]
        }
        
        self.expected_ranges = METRIC_RANGES
    
    def extract_facts(self, md_content: str, company_name: str, file_name: str) -> List[Dict[str, Any]]:
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
        if year_count >= 2 and ("RM'000" in line or 'Rs' in line or 'USD' in line or 'Note' in line or '<br>' in line):
            return True
        
        return False
    
    def _parse_table_header(self, header_line: str) -> List[TableColumn]:
        """Parse table header to understand column structure"""
        
        columns = []
        
        # Clean up <br> tags
        header_line = header_line.replace('<br>', ' ')
        
        # Split by pipe
        parts = header_line.split('|')
        
        # Track if we've seen Group/Company context
        seen_group = False
        seen_company = False
        
        for i, part in enumerate(parts):
            part = part.strip()
            
            # Skip empty parts or very long text (likely row headers)
            if not part or len(part) > 100:
                continue
            
            # Check if it's a note column
            if re.match(r'^Note?s?$', part, re.IGNORECASE):
                columns.append(TableColumn(i, 'note', 'note', True))
                continue
            
            # Extract year/period from formats like "2024 RM'000" or "2024<br>RM'000"
            year_match = re.search(r'(20\d{2})', part)
            quarter_match = re.search(r'Q([1-4])\s*(20\d{2})', part)
            
            if not year_match and not quarter_match:
                # Check if this is a Group/Company header
                if 'group' in part.lower():
                    seen_group = True
                elif 'company' in part.lower():
                    seen_company = True
                continue
            
            # Determine entity based on position and context
            entity = 'group'  # Default to group
            
            # If we have explicit markers, use them
            if 'company' in part.lower():
                entity = 'company'
            elif 'group' in part.lower():
                entity = 'group'
            else:
                # Use position-based heuristic for income statement format
                # Usually: Note | Group 2024 | Group 2023 | Company 2024 | Company 2023
                year_columns = [c for c in columns if not c.is_note]
                if len(year_columns) >= 2:
                    entity = 'company'
            
            # Get the year
            if quarter_match:
                period = f'Q{quarter_match.group(1)} {quarter_match.group(2)}'
            elif year_match:
                year = year_match.group(1)
                period = f'{year}'
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
        """Keep only latest period for each metric, removing duplicates"""
        
        # Group by company, metric, and period
        fact_groups = {}
        for fact in facts:
            # Use period in key to keep different periods
            key = (fact['company'], fact['key'], fact.get('period_label', 'unknown'))
            
            # Skip if we already have this exact metric/period combo with higher confidence
            if key in fact_groups:
                existing_conf = fact_groups[key].get('confidence', 0)
                new_conf = fact.get('confidence', 0)
                if new_conf <= existing_conf:
                    continue
            
            fact_groups[key] = fact
        
        # Now group by company/metric only to get latest period
        metric_groups = {}
        for (company, metric, period), fact in fact_groups.items():
            key = (company, metric)
            if key not in metric_groups:
                metric_groups[key] = []
            metric_groups[key].append(fact)
        
        deduped = []
        for key, group in metric_groups.items():
            if len(group) == 1:
                deduped.append(group[0])
            else:
                # Sort by period and take latest
                def period_sort_key(fact):
                    period = fact.get('period_label', '')
                    if 'Q' in period:
                        match = re.match(r'Q(\d)\s+(\d{4})', period)
                        if match:
                            return (int(match.group(2)), int(match.group(1)))
                    elif 'FY' in period or period.isdigit():
                        match = re.search(r'(\d{4})', period)
                        if match:
                            return (int(match.group(1)), 4)
                    return (0, 0)
                
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
            if 'ebitda' in metrics and 'revenue' in metrics:
                margin = safe_divide(metrics['ebitda'], metrics['revenue'])
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
            
            # Calculate operating margin
            if 'ebit' in metrics and 'revenue' in metrics:
                op_margin = safe_divide(metrics['ebit'], metrics['revenue'])
                if -0.5 <= op_margin <= 1:
                    derived.append({
                        'company': company,
                        'key': 'operating_margin',
                        'value': op_margin,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date,
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'fact_id': generate_fact_id(company, 'operating_margin', period_label, str(op_margin))
                    })
            
            # Calculate net margin
            if 'net_income' in metrics and 'revenue' in metrics:
                net_margin = safe_divide(metrics['net_income'], metrics['revenue'])
                if -0.5 <= net_margin <= 0.5:
                    derived.append({
                        'company': company,
                        'key': 'net_margin',
                        'value': net_margin,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date,
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'fact_id': generate_fact_id(company, 'net_margin', period_label, str(net_margin))
                    })
            
            # Calculate ROE
            if 'net_income' in metrics and 'total_equity' in metrics:
                roe = safe_divide(metrics['net_income'], metrics['total_equity'])
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
            
            # Calculate ROA
            if 'net_income' in metrics and 'total_assets' in metrics:
                roa = safe_divide(metrics['net_income'], metrics['total_assets'])
                if -0.5 <= roa <= 0.5:
                    derived.append({
                        'company': company,
                        'key': 'roa',
                        'value': roa,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date,
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'fact_id': generate_fact_id(company, 'roa', period_label, str(roa))
                    })
            
            # Calculate D/E ratio
            if 'total_debt' in metrics and 'total_equity' in metrics:
                de_ratio = safe_divide(metrics['total_debt'], metrics['total_equity'])
                if 0 <= de_ratio <= 10:
                    derived.append({
                        'company': company,
                        'key': 'debt_to_equity',
                        'value': de_ratio,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date,
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'fact_id': generate_fact_id(company, 'debt_to_equity', period_label, str(de_ratio))
                    })
            
            # Calculate interest coverage
            if 'ebit' in metrics and 'interest_expense' in metrics:
                coverage = safe_divide(metrics['ebit'], metrics['interest_expense'])
                if 0 <= coverage <= 100:
                    derived.append({
                        'company': company,
                        'key': 'interest_coverage',
                        'value': coverage,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date,
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'fact_id': generate_fact_id(company, 'interest_coverage', period_label, str(coverage))
                    })
            
            # Calculate current ratio
            if 'current_assets' in metrics and 'current_liabilities' in metrics:
                current_ratio = safe_divide(metrics['current_assets'], metrics['current_liabilities'])
                if 0 <= current_ratio <= 10:
                    derived.append({
                        'company': company,
                        'key': 'current_ratio',
                        'value': current_ratio,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date,
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'fact_id': generate_fact_id(company, 'current_ratio', period_label, str(current_ratio))
                    })
            
            # Calculate asset turnover
            if 'revenue' in metrics and 'total_assets' in metrics:
                asset_turnover = safe_divide(metrics['revenue'], metrics['total_assets'])
                if 0 <= asset_turnover <= 10:
                    derived.append({
                        'company': company,
                        'key': 'asset_turnover',
                        'value': asset_turnover,
                        'confidence': 0.85,
                        'period_label': period_label,
                        'period_date': period_date,
                        'source_doc': source_doc,
                        'source_page': 999,
                        'method': 'calculated',
                        'fact_id': generate_fact_id(company, 'asset_turnover', period_label, str(asset_turnover))
                    })
            
            # Calculate ROIC if possible
            if 'ebit' in metrics and 'tax_expense' in metrics and 'total_assets' in metrics and 'current_liabilities' in metrics:
                if metrics['ebit'] > 0:
                    tax_rate = safe_divide(metrics.get('tax_expense', 0), metrics['ebit'], 0.25)
                    nopat = metrics['ebit'] * (1 - tax_rate)
                    invested_capital = metrics['total_assets'] - metrics['current_liabilities']
                    roic = safe_divide(nopat, invested_capital)
                    if -0.5 <= roic <= 1:
                        derived.append({
                            'company': company,
                            'key': 'roic',
                            'value': roic,
                            'confidence': 0.8,
                            'period_label': period_label,
                            'period_date': period_date,
                            'source_doc': source_doc,
                            'source_page': 999,
                            'method': 'calculated',
                            'fact_id': generate_fact_id(company, 'roic', period_label, str(roic))
                        })
        
        return derived
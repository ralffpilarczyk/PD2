"""
Calculation Engine - Calculate derived financial metrics from base facts
"""

from decimal import Decimal
from typing import List, Dict, Optional, Tuple
from canonicalizer import generate_fact_id

class CalculationEngine:
    """Calculate derived financial metrics when components are available"""
    
    def __init__(self):
        self.calculated_metrics = []
        self.missing_calculations = {}
    
    def calculate_all(self, facts: List[Dict]) -> List[Dict]:
        """Calculate all possible derived metrics from available facts"""
        
        self.calculated_metrics = []
        self.missing_calculations = {}
        
        # Organize facts by company and period for easier calculation
        facts_by_company_period = self._organize_facts(facts)
        
        # Try to calculate various metrics for each company/period combination
        for (company, period_label, period_date), period_facts in facts_by_company_period.items():
            # Convert to dict for easier access, preferring smaller values for duplicates
            # (to handle unit inconsistencies between documents)
            metrics = {}
            for f in period_facts:
                key = f['key']
                if key not in metrics or f['value'] < metrics[key]['value']:
                    metrics[key] = f
            
            # Financial ratios and metrics
            self._calculate_roe(company, period_label, period_date, metrics)
            self._calculate_roa(company, period_label, period_date, metrics)
            self._calculate_roic(company, period_label, period_date, metrics)
            self._calculate_margins(company, period_label, period_date, metrics)
            self._calculate_leverage_ratios(company, period_label, period_date, metrics)
            self._calculate_liquidity_ratios(company, period_label, period_date, metrics)
            self._calculate_efficiency_ratios(company, period_label, period_date, metrics)
            self._calculate_coverage_ratios(company, period_label, period_date, metrics)
            self._calculate_per_share_metrics(company, period_label, period_date, metrics)
            self._calculate_dupont_components(company, period_label, period_date, metrics)
        
        return self.calculated_metrics
    
    def _organize_facts(self, facts: List[Dict]) -> Dict:
        """Organize facts by company and period"""
        organized = {}
        
        for fact in facts:
            key = (fact['company'], fact['period_label'], fact['period_date'])
            if key not in organized:
                organized[key] = []
            organized[key].append(fact)
        
        return organized
    
    def _add_calculated_fact(
        self, company: str, key: str, value: Decimal,
        period_label: str, period_date: str,
        source_facts: List[str], formula: str
    ):
        """Add a calculated fact to results"""
        
        if value is None or value == 0:
            return
        
        fact = {
            'company': company,
            'key': key,
            'value': float(value),
            'confidence': 0.85,  # Calculated facts have slightly lower confidence
            'period_label': period_label,
            'period_date': period_date,
            'source_doc': 'calculated',
            'source_page': 0,
            'method': 'calculated',
            'formula': formula,
            'source_facts': source_facts
        }
        fact['fact_id'] = generate_fact_id(company, key, period_label, str(value))
        
        self.calculated_metrics.append(fact)
    
    def _calculate_roe(self, company: str, period_label: str, period_date: str, metrics: Dict):
        """Calculate Return on Equity = Net Income / Total Equity"""
        
        if 'net_income' in metrics and 'total_equity' in metrics:
            net_income = metrics['net_income']['value']
            total_equity = metrics['total_equity']['value']
            
            if total_equity > 0:
                roe = net_income / total_equity
                self._add_calculated_fact(
                    company, 'roe', roe, period_label, period_date,
                    [metrics['net_income']['fact_id'], metrics['total_equity']['fact_id']],
                    'Net Income / Total Equity'
                )
            else:
                self.missing_calculations['roe'] = "Total equity is zero or negative"
        else:
            missing = []
            if 'net_income' not in metrics:
                missing.append('net_income')
            if 'total_equity' not in metrics:
                missing.append('total_equity')
            self.missing_calculations['roe'] = f"Missing: {', '.join(missing)}"
    
    def _calculate_roa(self, company: str, period_label: str, period_date: str, metrics: Dict):
        """Calculate Return on Assets = Net Income / Total Assets"""
        
        if 'net_income' in metrics and 'total_assets' in metrics:
            net_income = metrics['net_income']['value']
            total_assets = metrics['total_assets']['value']
            
            if total_assets > 0:
                roa = net_income / total_assets
                self._add_calculated_fact(
                    company, 'roa', roa, period_label, period_date,
                    [metrics['net_income']['fact_id'], metrics['total_assets']['fact_id']],
                    'Net Income / Total Assets'
                )
    
    def _calculate_roic(self, company: str, period_label: str, period_date: str, metrics: Dict):
        """Calculate Return on Invested Capital = NOPAT / Invested Capital"""
        
        # Try to calculate NOPAT (Net Operating Profit After Tax)
        nopat = None
        nopat_sources = []
        
        if 'operating_income' in metrics:
            operating_income = metrics['operating_income']['value']
            
            # Try to get tax rate
            tax_rate = self._calculate_tax_rate(metrics)
            
            if tax_rate is not None:
                nopat = operating_income * Decimal(str(1 - tax_rate))
                nopat_sources = [metrics['operating_income']['fact_id']]
        
        # Calculate invested capital
        invested_capital = None
        ic_sources = []
        
        if 'total_assets' in metrics and 'current_liabilities' in metrics:
            total_assets = metrics['total_assets']['value']
            current_liabilities = metrics['current_liabilities']['value']
            invested_capital = total_assets - current_liabilities
            ic_sources = [metrics['total_assets']['fact_id'], metrics['current_liabilities']['fact_id']]
        elif 'total_equity' in metrics and 'total_debt' in metrics:
            total_equity = metrics['total_equity']['value']
            total_debt = metrics['total_debt']['value']
            invested_capital = total_equity + total_debt
            ic_sources = [metrics['total_equity']['fact_id'], metrics['total_debt']['fact_id']]
        
        # Calculate ROIC if we have both components
        if nopat is not None and invested_capital is not None and invested_capital > 0:
            roic = nopat / invested_capital
            self._add_calculated_fact(
                company, 'roic', roic, period_label, period_date,
                nopat_sources + ic_sources,
                'NOPAT / Invested Capital'
            )
        else:
            if nopat is None:
                self.missing_calculations['roic'] = "Cannot calculate NOPAT (missing operating income or tax rate)"
            elif invested_capital is None:
                self.missing_calculations['roic'] = "Cannot calculate invested capital"
    
    def _calculate_tax_rate(self, metrics: Dict) -> Optional[float]:
        """Calculate effective tax rate from available data"""
        
        if 'tax_provision' in metrics and 'operating_income' in metrics:
            tax = metrics['tax_provision']['value']
            ebt = metrics['operating_income']['value']
            if 'interest_expense' in metrics:
                ebt -= metrics['interest_expense']['value']
            
            if ebt > 0:
                return float(tax / ebt)
        
        # Default tax rate assumption if cannot calculate
        return None
    
    def _calculate_margins(self, company: str, period_label: str, period_date: str, metrics: Dict):
        """Calculate various margin metrics"""
        
        revenue = metrics.get('revenue', {}).get('value')
        
        if not revenue or revenue <= 0:
            return
        
        # Gross Margin
        if 'cost_of_revenue' in metrics:
            cogs = metrics['cost_of_revenue']['value']
            gross_profit = revenue - cogs
            gross_margin = gross_profit / revenue
            self._add_calculated_fact(
                company, 'gross_margin', gross_margin, period_label, period_date,
                [metrics['revenue']['fact_id'], metrics['cost_of_revenue']['fact_id']],
                '(Revenue - COGS) / Revenue'
            )
        
        # Operating Margin
        if 'operating_income' in metrics:
            operating_income = metrics['operating_income']['value']
            operating_margin = operating_income / revenue
            self._add_calculated_fact(
                company, 'operating_margin', operating_margin, period_label, period_date,
                [metrics['operating_income']['fact_id'], metrics['revenue']['fact_id']],
                'Operating Income / Revenue'
            )
        
        # Net Margin
        if 'net_income' in metrics:
            net_income = metrics['net_income']['value']
            net_margin = net_income / revenue
            self._add_calculated_fact(
                company, 'net_margin', net_margin, period_label, period_date,
                [metrics['net_income']['fact_id'], metrics['revenue']['fact_id']],
                'Net Income / Revenue'
            )
        
        # EBITDA Margin (already calculated in extractor, but recalculate for consistency)
        if 'ebitda' in metrics:
            ebitda = metrics['ebitda']['value']
            ebitda_margin = ebitda / revenue
            self._add_calculated_fact(
                company, 'ebitda_margin', ebitda_margin, period_label, period_date,
                [metrics['ebitda']['fact_id'], metrics['revenue']['fact_id']],
                'EBITDA / Revenue'
            )
    
    def _calculate_leverage_ratios(self, company: str, period_label: str, period_date: str, metrics: Dict):
        """Calculate leverage and solvency ratios"""
        
        # Debt to Equity
        if 'total_debt' in metrics and 'total_equity' in metrics:
            total_debt = metrics['total_debt']['value']
            total_equity = metrics['total_equity']['value']
            
            if total_equity > 0:
                debt_to_equity = total_debt / total_equity
                self._add_calculated_fact(
                    company, 'debt_to_equity', debt_to_equity, period_label, period_date,
                    [metrics['total_debt']['fact_id'], metrics['total_equity']['fact_id']],
                    'Total Debt / Total Equity'
                )
        
        # Debt to Assets
        if 'total_debt' in metrics and 'total_assets' in metrics:
            total_debt = metrics['total_debt']['value']
            total_assets = metrics['total_assets']['value']
            
            if total_assets > 0:
                debt_to_assets = total_debt / total_assets
                self._add_calculated_fact(
                    company, 'debt_to_assets', debt_to_assets, period_label, period_date,
                    [metrics['total_debt']['fact_id'], metrics['total_assets']['fact_id']],
                    'Total Debt / Total Assets'
                )
    
    def _calculate_liquidity_ratios(self, company: str, period_label: str, period_date: str, metrics: Dict):
        """Calculate liquidity ratios"""
        
        # Current Ratio
        if 'current_assets' in metrics and 'current_liabilities' in metrics:
            current_assets = metrics['current_assets']['value']
            current_liabilities = metrics['current_liabilities']['value']
            
            if current_liabilities > 0:
                current_ratio = current_assets / current_liabilities
                self._add_calculated_fact(
                    company, 'current_ratio', current_ratio, period_label, period_date,
                    [metrics['current_assets']['fact_id'], metrics['current_liabilities']['fact_id']],
                    'Current Assets / Current Liabilities'
                )
        
        # Quick Ratio (approximation without inventory data)
        if 'cash' in metrics and 'current_liabilities' in metrics:
            cash = metrics['cash']['value']
            current_liabilities = metrics['current_liabilities']['value']
            
            if current_liabilities > 0:
                # This is a rough approximation
                quick_ratio = cash / current_liabilities
                self._add_calculated_fact(
                    company, 'cash_ratio', quick_ratio, period_label, period_date,
                    [metrics['cash']['fact_id'], metrics['current_liabilities']['fact_id']],
                    'Cash / Current Liabilities'
                )
    
    def _calculate_efficiency_ratios(self, company: str, period_label: str, period_date: str, metrics: Dict):
        """Calculate efficiency ratios"""
        
        # Asset Turnover
        if 'revenue' in metrics and 'total_assets' in metrics:
            revenue = metrics['revenue']['value']
            total_assets = metrics['total_assets']['value']
            
            if total_assets > 0:
                asset_turnover = revenue / total_assets
                self._add_calculated_fact(
                    company, 'asset_turnover', asset_turnover, period_label, period_date,
                    [metrics['revenue']['fact_id'], metrics['total_assets']['fact_id']],
                    'Revenue / Total Assets'
                )
        
        # Capital Intensity (CapEx / Revenue)
        if 'capex' in metrics and 'revenue' in metrics:
            capex = metrics['capex']['value']
            revenue = metrics['revenue']['value']
            
            if revenue > 0:
                capital_intensity = capex / revenue
                self._add_calculated_fact(
                    company, 'capital_intensity', capital_intensity, period_label, period_date,
                    [metrics['capex']['fact_id'], metrics['revenue']['fact_id']],
                    'CapEx / Revenue'
                )
    
    def _calculate_coverage_ratios(self, company: str, period_label: str, period_date: str, metrics: Dict):
        """Calculate coverage ratios"""
        
        # Interest Coverage
        if 'operating_income' in metrics and 'interest_expense' in metrics:
            operating_income = metrics['operating_income']['value']
            interest_expense = metrics['interest_expense']['value']
            
            if interest_expense > 0:
                interest_coverage = operating_income / interest_expense
                self._add_calculated_fact(
                    company, 'interest_coverage', interest_coverage, period_label, period_date,
                    [metrics['operating_income']['fact_id'], metrics['interest_expense']['fact_id']],
                    'Operating Income / Interest Expense'
                )
        elif 'ebitda' in metrics and 'interest_expense' in metrics:
            ebitda = metrics['ebitda']['value']
            interest_expense = metrics['interest_expense']['value']
            
            if interest_expense > 0:
                interest_coverage = ebitda / interest_expense
                self._add_calculated_fact(
                    company, 'ebitda_interest_coverage', interest_coverage, period_label, period_date,
                    [metrics['ebitda']['fact_id'], metrics['interest_expense']['fact_id']],
                    'EBITDA / Interest Expense'
                )
    
    def _calculate_per_share_metrics(self, company: str, period_label: str, period_date: str, metrics: Dict):
        """Calculate per-share metrics if share count available"""
        
        shares = metrics.get('shares_outstanding', {}).get('value')
        
        if not shares or shares <= 0:
            return
        
        # EPS (Earnings Per Share)
        if 'net_income' in metrics:
            net_income = metrics['net_income']['value']
            eps = net_income / shares
            self._add_calculated_fact(
                company, 'eps', eps, period_label, period_date,
                [metrics['net_income']['fact_id'], metrics['shares_outstanding']['fact_id']],
                'Net Income / Shares Outstanding'
            )
        
        # Book Value Per Share
        if 'total_equity' in metrics:
            total_equity = metrics['total_equity']['value']
            bvps = total_equity / shares
            self._add_calculated_fact(
                company, 'book_value_per_share', bvps, period_label, period_date,
                [metrics['total_equity']['fact_id'], metrics['shares_outstanding']['fact_id']],
                'Total Equity / Shares Outstanding'
            )
    
    def _calculate_dupont_components(self, company: str, period_label: str, period_date: str, metrics: Dict):
        """Calculate DuPont components (NPM, AT, FL) for theorem integration"""
        
        # Net Profit Margin (NPM) = Net Income / Revenue
        if 'net_income' in metrics and 'revenue' in metrics:
            net_income = metrics['net_income']['value']
            revenue = metrics['revenue']['value']
            
            if revenue > 0:
                npm = net_income / revenue
                self._add_calculated_fact(
                    company, 'npm', npm, period_label, period_date,
                    [metrics['net_income']['fact_id'], metrics['revenue']['fact_id']],
                    'Net Income / Revenue'
                )
        
        # Asset Turnover (AT) = Revenue / Total Assets
        if 'revenue' in metrics and 'total_assets' in metrics:
            revenue = metrics['revenue']['value']
            total_assets = metrics['total_assets']['value']
            
            if total_assets > 0:
                at = revenue / total_assets
                self._add_calculated_fact(
                    company, 'at', at, period_label, period_date,
                    [metrics['revenue']['fact_id'], metrics['total_assets']['fact_id']],
                    'Revenue / Total Assets'
                )
        
        # Financial Leverage (FL) = Total Assets / Total Equity
        if 'total_assets' in metrics and 'total_equity' in metrics:
            total_assets = metrics['total_assets']['value']
            total_equity = metrics['total_equity']['value']
            
            if total_equity > 0:
                fl = total_assets / total_equity
                self._add_calculated_fact(
                    company, 'fl', fl, period_label, period_date,
                    [metrics['total_assets']['fact_id'], metrics['total_equity']['fact_id']],
                    'Total Assets / Total Equity'
                )
    
    def report_missing_calculations(self, facts: List[Dict]) -> Dict[str, str]:
        """Report which calculations couldn't be performed and why"""
        return self.missing_calculations


# Test the calculation engine
if __name__ == "__main__":
    engine = CalculationEngine()
    
    # Test with sample facts
    test_facts = [
        {'company': 'TestCo', 'key': 'net_income', 'value': 1000, 'period_label': 'Q1 2024', 
         'period_date': '2024-03-31', 'fact_id': 'test1'},
        {'company': 'TestCo', 'key': 'total_equity', 'value': 10000, 'period_label': 'Q1 2024',
         'period_date': '2024-03-31', 'fact_id': 'test2'},
        {'company': 'TestCo', 'key': 'total_assets', 'value': 15000, 'period_label': 'Q1 2024',
         'period_date': '2024-03-31', 'fact_id': 'test3'},
        {'company': 'TestCo', 'key': 'revenue', 'value': 5000, 'period_label': 'Q1 2024',
         'period_date': '2024-03-31', 'fact_id': 'test4'},
    ]
    
    calculated = engine.calculate_all(test_facts)
    
    print("Calculated metrics:")
    for fact in calculated:
        print(f"  {fact['key']}: {fact['value']:.4f} ({fact['formula']})")
    
    missing = engine.report_missing_calculations(test_facts)
    if missing:
        print("\nMissing calculations:")
        for key, reason in missing.items():
            print(f"  {key}: {reason}")
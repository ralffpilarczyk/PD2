"""
Data Availability Tracker - Track which theorems can run with available data
"""

from typing import List, Dict, Set

class DataAvailabilityTracker:
    """Track data availability and theorem applicability"""
    
    def __init__(self):
        # Define theorem requirements
        self.theorem_requirements = {
            # Value creation theorems
            'value_creation_roic': ['roic', 'wacc'],
            'value_creation_roe': ['roe', 'wacc'],
            'value_destruction_roic': ['roic', 'wacc'],
            'value_destruction_roe': ['roe', 'wacc'],
            
            # Operational lever theorems
            'margin_pressure': ['ebitda_margin', 'operating_margin', 'gross_margin', 'net_margin'],
            'capital_intensity_concern': ['capital_intensity'],
            
            # Valuation driver theorems
            'roe_valuation_driver': ['roe'],
            'margin_valuation_driver': ['ebitda_margin', 'operating_margin', 'gross_margin'],
            'growth_valuation_driver': ['revenue_growth'],
            
            # Financial health theorems
            'leverage_analysis': ['debt_to_equity', 'debt_to_assets'],
            'liquidity_analysis': ['current_ratio', 'cash_ratio'],
            'coverage_analysis': ['interest_coverage', 'ebitda_interest_coverage'],
            
            # Efficiency theorems
            'asset_efficiency': ['asset_turnover', 'roa'],
            'capital_efficiency': ['roic', 'roe'],
            
            # Profitability theorems
            'profitability_analysis': ['net_margin', 'operating_margin', 'gross_margin'],
            'return_analysis': ['roe', 'roa', 'roic'],
        }
        
        # Define which metrics need trends (multiple periods)
        self.trend_requirements = {
            'margin_pressure',
            'roe_valuation_driver',
            'margin_valuation_driver',
            'growth_valuation_driver',
        }
    
    def check_all_theorems(self, facts: List[Dict]) -> Dict:
        """Check which theorems can run with available facts"""
        
        # Get available metrics by company and period
        available_metrics = self._get_available_metrics(facts)
        
        # Check each theorem
        runnable = []
        missing_data = {}
        
        for theorem, requirements in self.theorem_requirements.items():
            can_run, missing = self._check_theorem(theorem, requirements, available_metrics)
            
            if can_run:
                runnable.append(theorem)
            else:
                missing_data[theorem] = missing
        
        # Summary statistics
        total = len(self.theorem_requirements)
        can_run = len(runnable)
        
        return {
            'total': total,
            'can_run': can_run,
            'cannot_run': total - can_run,
            'runnable': runnable,
            'missing_data': missing_data,
            'available_metrics': available_metrics,
            'coverage_percentage': (can_run / total * 100) if total > 0 else 0
        }
    
    def _get_available_metrics(self, facts: List[Dict]) -> Dict[str, Dict[str, Set[str]]]:
        """Get available metrics organized by company and metric"""
        
        available = {}
        
        for fact in facts:
            company = fact['company']
            metric = fact['key']
            period = fact['period_label']
            
            if company not in available:
                available[company] = {}
            
            if metric not in available[company]:
                available[company][metric] = set()
            
            available[company][metric].add(period)
        
        return available
    
    def _check_theorem(self, theorem: str, requirements: List[str], available: Dict) -> tuple:
        """Check if a specific theorem can run"""
        
        # For now, check if any company has at least one of the required metrics
        # In practice, theorems might need all requirements or just one
        
        can_run = False
        missing = set(requirements)
        
        for company, metrics in available.items():
            # Check if this company has any of the required metrics
            company_has = set(metrics.keys())
            required_set = set(requirements)
            
            # For OR requirements (any one metric is enough)
            if company_has & required_set:  # Intersection
                can_run = True
                missing -= company_has
                
                # For trend theorems, check if we have multiple periods
                if theorem in self.trend_requirements:
                    # Need at least 2 periods for trend analysis
                    has_trend = False
                    for metric in requirements:
                        if metric in metrics and len(metrics[metric]) >= 2:
                            has_trend = True
                            break
                    
                    if not has_trend:
                        can_run = False
                        missing.add('multiple_periods_needed')
        
        return can_run, list(missing)
    
    def suggest_calculations(self, facts: List[Dict]) -> Dict[str, List[str]]:
        """Suggest what metrics could be calculated from available data"""
        
        suggestions = {}
        available_metrics = self._get_available_metrics(facts)
        
        for company, metrics in available_metrics.items():
            company_suggestions = []
            
            # ROE calculation
            if 'net_income' in metrics and 'total_equity' in metrics:
                if 'roe' not in metrics:
                    company_suggestions.append('ROE (Net Income / Total Equity)')
            
            # ROA calculation
            if 'net_income' in metrics and 'total_assets' in metrics:
                if 'roa' not in metrics:
                    company_suggestions.append('ROA (Net Income / Total Assets)')
            
            # Margin calculations
            if 'revenue' in metrics:
                if 'ebitda' in metrics and 'ebitda_margin' not in metrics:
                    company_suggestions.append('EBITDA Margin (EBITDA / Revenue)')
                if 'net_income' in metrics and 'net_margin' not in metrics:
                    company_suggestions.append('Net Margin (Net Income / Revenue)')
            
            # Leverage calculations
            if 'total_debt' in metrics and 'total_equity' in metrics:
                if 'debt_to_equity' not in metrics:
                    company_suggestions.append('Debt-to-Equity (Total Debt / Total Equity)')
            
            if company_suggestions:
                suggestions[company] = company_suggestions
        
        return suggestions
    
    def generate_completeness_report(self, facts: List[Dict]) -> str:
        """Generate a text report on data completeness"""
        
        availability = self.check_all_theorems(facts)
        suggestions = self.suggest_calculations(facts)
        
        report = []
        report.append("DATA COMPLETENESS REPORT")
        report.append("=" * 60)
        
        # Overall statistics
        report.append(f"\nTheorem Coverage: {availability['can_run']}/{availability['total']} "
                     f"({availability['coverage_percentage']:.1f}%)")
        
        # Available metrics by company
        report.append("\nAvailable Metrics by Company:")
        for company, metrics in availability['available_metrics'].items():
            report.append(f"\n{company}:")
            for metric in sorted(metrics.keys())[:10]:  # Show first 10
                periods = sorted(metrics[metric])
                report.append(f"  - {metric}: {', '.join(periods)}")
        
        # Runnable theorems
        if availability['runnable']:
            report.append(f"\nRunnable Theorems ({len(availability['runnable'])}):")
            for theorem in availability['runnable'][:10]:
                report.append(f"  ✓ {theorem}")
        
        # Missing data for key theorems
        if availability['missing_data']:
            report.append(f"\nKey Theorems Missing Data:")
            for theorem, missing in list(availability['missing_data'].items())[:5]:
                if 'wacc' in missing:  # Highlight WACC-dependent theorems
                    report.append(f"  ✗ {theorem}: needs {', '.join(missing)} [WACC required]")
                else:
                    report.append(f"  ✗ {theorem}: needs {', '.join(missing)}")
        
        # Calculation suggestions
        if suggestions:
            report.append("\nSuggested Calculations:")
            for company, calcs in suggestions.items():
                report.append(f"\n{company}:")
                for calc in calcs[:5]:
                    report.append(f"  - {calc}")
        
        return "\n".join(report)


# Test the tracker
if __name__ == "__main__":
    tracker = DataAvailabilityTracker()
    
    # Test with sample facts
    test_facts = [
        {'company': 'TestCo', 'key': 'revenue', 'value': 1000, 'period_label': 'Q1 2024'},
        {'company': 'TestCo', 'key': 'ebitda', 'value': 300, 'period_label': 'Q1 2024'},
        {'company': 'TestCo', 'key': 'ebitda_margin', 'value': 0.3, 'period_label': 'Q1 2024'},
        {'company': 'TestCo', 'key': 'ebitda_margin', 'value': 0.32, 'period_label': 'Q2 2024'},
        {'company': 'TestCo', 'key': 'roe', 'value': 0.15, 'period_label': 'Q1 2024'},
    ]
    
    availability = tracker.check_all_theorems(test_facts)
    
    print(f"Theorem availability: {availability['can_run']}/{availability['total']}")
    print(f"\nRunnable theorems: {availability['runnable']}")
    print(f"\nMissing data (sample): {list(availability['missing_data'].items())[:3]}")
    
    print("\n" + "=" * 60)
    print(tracker.generate_completeness_report(test_facts))
"""
DDAR-SampleGPT Adapter Layer
Bridges our fact extraction with their theorem engine
"""

import re
from decimal import Decimal
from typing import Dict, List, Tuple, Optional
from datetime import datetime
import hashlib

class DDARAdapter:
    """Converts DDAR facts to SampleGPT format and manages integration"""
    
    def __init__(self):
        # Metric name mappings (our keys -> their canonical names)
        self.metric_map = {
            'ebitda': 'ebitda',
            'revenue': 'revenue',
            'operating_income': 'ebit',
            'net_income': 'net_income',
            'total_assets': 'total_assets',
            'total_equity': 'equity_value',
            'total_debt': 'total_debt',
            'current_assets': 'current_assets',
            'current_liabilities': 'current_liabilities',
            'cash': 'cash',
            'interest_expense': 'interest_expense',
            'tax_provision': 'tax_expense',
            'depreciation': 'depreciation',
            'capex': 'capex',
            'operating_cash_flow': 'operating_cash_flow',
            'free_cash_flow': 'fcf',
            'shares_outstanding': 'shares_outstanding',
            # Calculated metrics
            'roe': 'roe',
            'roa': 'roa', 
            'roic': 'roic',
            'ebitda_margin': 'ebitda_margin',
            'operating_margin': 'operating_margin',
            'net_margin': 'npm',
            'debt_to_equity': 'debt_to_equity',
            'current_ratio': 'current_ratio',
            'asset_turnover': 'at',
            'capital_intensity': 'capital_intensity'
        }
        
        # Unit detection patterns
        self.unit_patterns = {
            'margin': 'ratio',
            'ratio': 'ratio',
            'rate': 'ratio',
            'days': 'days',
            'years': 'years',
            'shares': 'count'
        }
        
        # Source type mapping
        self.source_map = {
            'calculated': 'model',
            'Axiata 24 AR_m.md': 'annual_report',
            'Axiata 24 FS_m.md': 'k10',
            'Axiata 25Q1 FS_m.md': 'q10',
            'Axiata 25Q1 PR_m.md': 'press',
            'Axiata 25Q1 Pres_m.md': 'investor_presentation'
        }
    
    def convert_period(self, period_label: str, period_date: str) -> str:
        """Convert DDAR period format to SampleGPT format"""
        # 'FY 2024' -> 'fy(2024)'
        # 'Q1 2025' -> 'q(2025,1)'
        
        if 'FY' in period_label:
            year = re.search(r'\d{4}', period_label)
            if year:
                return f"fy({year.group()})"
        elif 'Q' in period_label:
            match = re.search(r'Q(\d)\s+(\d{4})', period_label)
            if match:
                return f"q({match.group(2)},{match.group(1)})"
        
        # Fallback to date-based
        if period_date:
            date_parts = period_date.split('-')
            if len(date_parts) == 3:
                return f"date({date_parts[0]},{date_parts[1]},{date_parts[2]})"
        
        return 'unknown'
    
    def detect_unit_and_scale(self, metric_key: str, value: float) -> Tuple[str, str]:
        """Detect the unit type and scale from metric name and value magnitude"""
        
        # Check metric name for unit hints
        for pattern, unit in self.unit_patterns.items():
            if pattern in metric_key.lower():
                return unit, 'ones'
        
        # For financial values, detect scale based on magnitude
        if abs(value) < 1:
            # Likely a ratio/percentage already normalized
            return 'ratio', 'ones'
        elif abs(value) < 100:
            # Could be billions or a small number
            if 'margin' in metric_key or 'ratio' in metric_key:
                return 'ratio', 'ones'
            else:
                return 'currency', 'billions'
        elif abs(value) < 100000:
            # Likely millions
            return 'currency', 'millions'
        elif abs(value) < 100000000:
            # Likely thousands
            return 'currency', 'thousands'
        else:
            # Likely ones (already in base units)
            return 'currency', 'ones'
    
    def map_metric_name(self, ddar_key: str) -> str:
        """Map DDAR metric key to SampleGPT canonical name"""
        return self.metric_map.get(ddar_key, ddar_key)
    
    def map_source_type(self, source_doc: str) -> str:
        """Map DDAR source document to SampleGPT source type"""
        return self.source_map.get(source_doc, 'model')
    
    def convert_fact_to_assert(self, fact: Dict) -> str:
        """Convert a DDAR fact to SampleGPT assert_norm_fact statement"""
        
        company = fact['company'].lower().replace(' ', '_')
        metric = self.map_metric_name(fact['key'])
        value = fact['value']
        
        # Detect unit and scale
        unit, scale = self.detect_unit_and_scale(fact['key'], value)
        
        # Convert period
        period = self.convert_period(
            fact.get('period_label', ''),
            fact.get('period_date', '')
        )
        
        # Map source type
        source_type = self.map_source_type(fact.get('source_doc', 'unknown'))
        
        # Get confidence
        confidence = fact.get('confidence', 0.85)
        
        # Generate assert statement
        return f"assert_norm_fact({company}, {metric}, {value}, {unit}, {scale}, {period}, {source_type}, {confidence})"
    
    def generate_prolog_facts_file(self, facts: List[Dict], output_file: str):
        """Generate a Prolog file with normalized facts for SampleGPT engine"""
        
        with open(output_file, 'w') as f:
            f.write("%%% DDAR Facts simplified for essential theorems\n")
            f.write("%%% Generated by DDAR Adapter\n\n")
            f.write(":- discontiguous fact/3.\n")
            f.write(":- discontiguous fact_period/3.\n")
            f.write(":- discontiguous fact_confidence/3.\n\n")
            
            # Group facts by company for organization
            by_company = {}
            for fact in facts:
                company = fact['company']
                if company not in by_company:
                    by_company[company] = []
                by_company[company].append(fact)
            
            # Write facts for each company
            for company, company_facts in by_company.items():
                f.write(f"\n%%% Facts for {company}\n")
                company_lower = company.lower().replace(' ', '_')
                
                # Sort by period for readability
                sorted_facts = sorted(company_facts, 
                                     key=lambda x: (x.get('period_date', ''), x.get('key', '')))
                
                for fact in sorted_facts:
                    metric = self.map_metric_name(fact['key'])
                    value = fact['value']
                    period = self.convert_period(
                        fact.get('period_label', ''),
                        fact.get('period_date', '')
                    )
                    confidence = fact.get('confidence', 0.85)
                    
                    # Simple fact format for essential theorems
                    f.write(f"fact({company_lower}, {metric}, {value}).\n")
                    f.write(f"fact_period({company_lower}, {metric}, {period}).\n")
                    f.write(f"fact_confidence({company_lower}, {metric}, {confidence}).\n\n")
            
            f.write("\n%%% End of facts\n")
    
    def generate_bridge_rules(self, facts: List[Dict], output_file: str):
        """Generate bridge rules to select best facts for theorem engine"""
        
        with open(output_file, 'w') as f:
            f.write("%%% Bridge rules - simplified for essential theorems\n\n")
            
            # Since we're using simple fact/3 format, just provide a pass-through
            f.write("% Bridge is not needed for simple facts\n")
            f.write("bridge_all_facts :- true.\n\n")
            
            # Add calculation rules for DuPont and other key ratios
            f.write("%%% Calculation rules for derived metrics\n\n")
            f.write("calculate_dupont_inputs(Company, Period) :-\n")
            f.write("    best_fact(Company, revenue, Period, Rev, _, _, _),\n")
            f.write("    best_fact(Company, net_income, Period, NI, _, _, _),\n")
            f.write("    best_fact(Company, total_assets, Period, TA, _, _, _),\n")
            f.write("    best_fact(Company, equity_value, Period, EQ, _, _, _),\n")
            f.write("    NPM is NI / Rev,\n")
            f.write("    AT is Rev / TA,\n")
            f.write("    FL is TA / EQ,\n")
            f.write("    assertz(fact(Company, npm, NPM)),\n")
            f.write("    assertz(fact(Company, at, AT)),\n")
            f.write("    assertz(fact(Company, fl, FL)).\n\n")
            
            f.write("calculate_roic(Company, Period) :-\n")
            f.write("    best_fact(Company, ebit, Period, EBIT, _, _, _),\n")
            f.write("    best_fact(Company, tax_expense, Period, Tax, _, _, _),\n")
            f.write("    best_fact(Company, invested_capital, Period, IC, _, _, _),\n")
            f.write("    EBIT > 0, IC > 0,\n")
            f.write("    TaxRate is Tax / EBIT,\n")
            f.write("    NOPAT is EBIT * (1 - TaxRate),\n")
            f.write("    ROIC is NOPAT / IC,\n")
            f.write("    assertz(fact(Company, nopat, NOPAT)),\n")
            f.write("    assertz(fact(Company, roic, ROIC)).\n")
    
    def create_integrated_engine(self, theorem_files: List[str], output_file: str):
        """Create integrated Prolog engine combining our facts with their theorems"""
        
        with open(output_file, 'w') as f:
            f.write("%%% Integrated DDAR-SampleGPT Engine\n")
            f.write("%%% Combines DDAR facts with SampleGPT theorems\n\n")
            
            # Load the theorem files
            for theorem_file in theorem_files:
                f.write(f":- consult('{theorem_file}').\n")
            
            f.write("\n% Load converted facts\n")
            f.write(":- consult('ddar_facts_norm.pl').\n")
            f.write(":- consult('ddar_bridge.pl').\n\n")
            
            # Main analysis predicate
            f.write("%%% Main analysis entry point\n")
            f.write("analyze_company(Company, Results) :-\n")
            f.write("    % Bridge best facts\n")
            f.write("    bridge_all_facts,\n")
            f.write("    % Calculate derived metrics\n")
            f.write("    (calculate_dupont_inputs(Company, _) ; true),\n")
            f.write("    (calculate_roic(Company, _) ; true),\n")
            f.write("    % Collect results from essential theorems\n")
            f.write("    findall(t001(C), apply_t001(Company, C), T001Results),\n")
            f.write("    findall(t002(C), apply_t002(Company, C), T002Results),\n")
            f.write("    findall(t011(C), apply_t011(Company, C), T011Results),\n")
            f.write("    % Package results\n")
            f.write("    Results = results([\n")
            f.write("        value_creation(T001Results),\n")
            f.write("        dupont(T002Results),\n")
            f.write("        fcf(T011Results)\n")
            f.write("    ]).\n")


# Test the adapter
if __name__ == "__main__":
    adapter = DDARAdapter()
    
    # Sample DDAR fact
    test_fact = {
        'company': 'TestCompany',
        'key': 'revenue',
        'value': 22335.0,
        'confidence': 0.95,
        'period_label': 'FY 2024',
        'period_date': '2024-12-31',
        'source_doc': 'TestCompany 24 AR_m.md',
        'source_page': 6074,
        'method': 'regex_extract',
        'fact_id': 'abc123'
    }
    
    # Test conversion
    assert_stmt = adapter.convert_fact_to_assert(test_fact)
    print(f"Converted fact: {assert_stmt}")
    
    # Test period conversion
    print(f"Period conversion: {adapter.convert_period('FY 2024', '2024-12-31')}")
    print(f"Period conversion: {adapter.convert_period('Q1 2025', '2025-03-31')}")
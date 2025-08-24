#!/usr/bin/env python3
"""
DDAR Main Application - Interactive file selector and orchestrator
Similar to PD2.py interface for familiar user experience
"""

import os
import sys
import json
import subprocess
import tempfile
from datetime import datetime
from pathlib import Path
from typing import List, Dict, Tuple, Any

# Import our modules
from fact_extractor_v4 import FactExtractorV4 as FactExtractor
from calculation_engine import CalculationEngine
from data_availability import DataAvailabilityTracker
from report_generator import ReportGenerator
from enhanced_report_generator import EnhancedReportGenerator
from ddar_adapter import DDARAdapter
from conclusion_engine import ConclusionEngine, TheoremContext, create_chain_visualization

class DDARApplication:
    """Main DDAR application with interactive interface"""
    
    def __init__(self):
        self.source_dir = Path("../SourceFiles")
        self.output_dir = Path("output")
        self.output_dir.mkdir(exist_ok=True)
        (self.output_dir / "reports").mkdir(exist_ok=True)
        (self.output_dir / "facts").mkdir(exist_ok=True)
        
        self.extractor = FactExtractor()
        self.calculator = CalculationEngine()
        self.availability_tracker = DataAvailabilityTracker()
        self.report_generator = ReportGenerator()
        self.enhanced_report_generator = EnhancedReportGenerator()
        self.adapter = DDARAdapter()
        self.conclusion_engine = ConclusionEngine()
        
        # Theorem conclusion mappings
        self.conclusion_mappings = {
            'high_operating_efficiency': 'Strong Operational Efficiency',
            'moderate_operating_efficiency': 'Moderate Operational Efficiency',
            'low_operating_efficiency': 'Weak Operational Efficiency',
            'high_asset_efficiency': 'Excellent Asset Utilization',
            'moderate_asset_efficiency': 'Moderate Asset Utilization',
            'low_asset_efficiency': 'Poor Asset Utilization',
            'strong_liquidity': 'Strong Liquidity Position',
            'adequate_liquidity': 'Adequate Liquidity',
            'weak_liquidity': 'Liquidity Concerns',
            'strong_liquidity_cr_only': 'Strong Current Ratio',
            'adequate_liquidity_cr_only': 'Adequate Current Ratio',
            'weak_liquidity_cr_only': 'Weak Current Ratio',
            'strong_solvency': 'Strong Financial Solvency',
            'adequate_solvency': 'Adequate Solvency',
            'weak_solvency': 'Solvency Concerns',
            'improving_margins': 'Improving Profit Margins',
            'deteriorating_margins': 'Deteriorating Profit Margins',
            'stable_margins': 'Stable Profit Margins',
            'creates_value': 'Value Creation Confirmed',
            'destroys_value': 'Value Destruction Detected',
            'value_neutral': 'Value Neutral',
            'identity_applied': 'Financial Identity Calculated',
            'positive_eva': 'Positive Economic Value Added',
            'negative_eva': 'Negative Economic Value Added',
            'zero_eva': 'Zero Economic Value Added'
        }
        
    def display_header(self):
        """Display application header"""
        print("\n" + "=" * 70)
        print("DDAR - Deductive Database & Reasoning System")
        print("Financial Statement Analysis with Traceable Reasoning")
        print("=" * 70)
        
    def get_available_files(self) -> List[Tuple[Path, float, str]]:
        """Get list of available .md files with metadata"""
        files = []
        if self.source_dir.exists():
            for file_path in sorted(self.source_dir.glob("*.md")):
                size_mb = file_path.stat().st_size / (1024 * 1024)
                mod_time = datetime.fromtimestamp(file_path.stat().st_mtime)
                files.append((file_path, size_mb, mod_time.strftime("%Y-%m-%d")))
        return files
    
    def select_files(self) -> List[Path]:
        """Interactive file selection interface"""
        files = self.get_available_files()
        
        if not files:
            print(f"\nNo .md files found in {self.source_dir}")
            return []
        
        print(f"\nAvailable source files in {self.source_dir}:")
        print("-" * 60)
        
        # Display files with numbers
        for i, (file_path, size_mb, mod_date) in enumerate(files, 1):
            company = self.extract_company_name(file_path.name)
            print(f"{i:2}. {file_path.name:40} ({size_mb:.1f} MB) [{mod_date}]")
            if company:
                print(f"    Company: {company}")
        
        print("\nOptions:")
        print("  - Enter numbers separated by commas (e.g., 1,3,5)")
        print("  - Enter 'all' to select all files")
        print("  - Enter 'q' to quit")
        
        while True:
            selection = input("\nSelect files to analyze: ").strip().lower()
            
            if selection == 'q':
                print("Exiting...")
                sys.exit(0)
            
            if selection == 'all':
                selected = [f[0] for f in files]
                break
            
            try:
                # Parse comma-separated numbers
                indices = [int(x.strip()) - 1 for x in selection.split(',')]
                
                # Validate indices
                if all(0 <= i < len(files) for i in indices):
                    selected = [files[i][0] for i in indices]
                    break
                else:
                    print("Invalid selection. Please enter valid file numbers.")
            except (ValueError, IndexError):
                print("Invalid input. Please enter numbers separated by commas or 'all'.")
        
        # Confirm selection
        print(f"\nSelected {len(selected)} file(s):")
        for file_path in selected:
            print(f"  - {file_path.name}")
        
        return selected
    
    def extract_company_name(self, filename: str) -> str:
        """Extract company name from filename"""
        # Common patterns: "Company 24Q1 FS_m.md", "Company 24 AR_m.md"
        parts = filename.replace("_m.md", "").replace(".md", "").split()
        
        # Try to identify company name (before year/quarter indicators)
        company_parts = []
        for part in parts:
            if any(char.isdigit() for char in part):
                break
            company_parts.append(part)
        
        return " ".join(company_parts) if company_parts else "Unknown"
    
    def parse_prolog_conclusion(self, conclusion_str: str, theorem_name: str) -> Dict:
        """Parse Prolog conclusion into human-readable format"""
        import re
        
        # Check for new sensitivity analysis format
        if 'sensitivity' in conclusion_str or 'analysis' in conclusion_str:
            return self.parse_sensitivity_conclusion(conclusion_str, theorem_name)
        
        # Parse traditional Prolog term: predicate(arg1, arg2, ...)
        match = re.match(r'(\w+)\((.*)\)', conclusion_str)
        if not match:
            return {
                'recommendation': conclusion_str,
                'reasoning': 'Analysis completed',
                'values': []
            }
        
        predicate = match.group(1)
        args_str = match.group(2)
        
        # Parse arguments
        values = []
        if args_str:
            # Handle nested parentheses and commas
            args = []
            current_arg = ''
            paren_depth = 0
            for char in args_str:
                if char == '(' :
                    paren_depth += 1
                    current_arg += char
                elif char == ')':
                    paren_depth -= 1
                    current_arg += char
                elif char == ',' and paren_depth == 0:
                    args.append(current_arg.strip())
                    current_arg = ''
                else:
                    current_arg += char
            if current_arg:
                args.append(current_arg.strip())
            
            # Convert to float if numeric
            for arg in args:
                try:
                    values.append(float(arg))
                except ValueError:
                    values.append(arg)
        
        # Get human-readable recommendation
        recommendation = self.conclusion_mappings.get(predicate, predicate.replace('_', ' ').title())
        
        # Generate reasoning based on theorem and values
        reasoning = self.generate_reasoning(theorem_name, predicate, values)
        
        return {
            'recommendation': recommendation,
            'reasoning': reasoning,
            'values': values,
            'predicate': predicate
        }
    
    def generate_reasoning(self, theorem_name: str, predicate: str, values: List[Any]) -> str:
        """Generate reasoning explanation for a conclusion"""
        
        reasoning_templates = {
            'Operating Efficiency': {
                'high_operating_efficiency': 'Operating margin of {:.1%} indicates strong operational performance',
                'moderate_operating_efficiency': 'Operating margin of {:.1%} shows moderate operational efficiency',
                'low_operating_efficiency': 'Operating margin of {:.1%} suggests operational improvement needed'
            },
            'Asset Efficiency': {
                'high_asset_efficiency': 'Asset efficiency score of {:.2f} demonstrates excellent asset utilization',
                'moderate_asset_efficiency': 'Asset efficiency score of {:.2f} indicates moderate asset productivity',
                'low_asset_efficiency': 'Asset efficiency score of {:.2f} suggests underutilized assets'
            },
            'Liquidity Assessment': {
                'strong_liquidity': 'Current ratio of {:.2f} and quick ratio of {:.2f} indicate strong liquidity',
                'adequate_liquidity': 'Current ratio of {:.2f} and quick ratio of {:.2f} show adequate liquidity',
                'weak_liquidity': 'Current ratio of {:.2f} and quick ratio of {:.2f} suggest liquidity concerns',
                'strong_liquidity_cr_only': 'Current ratio of {:.2f} indicates strong short-term liquidity',
                'adequate_liquidity_cr_only': 'Current ratio of {:.2f} shows adequate short-term liquidity',
                'weak_liquidity_cr_only': 'Current ratio of {:.2f} suggests potential liquidity issues'
            },
            'Solvency Assessment': {
                'strong_solvency': 'Debt-to-equity of {:.2f} and interest coverage of {:.1f}x demonstrate solid solvency',
                'adequate_solvency': 'Debt-to-equity of {:.2f} and interest coverage of {:.1f}x show adequate solvency',
                'weak_solvency': 'Debt-to-equity of {:.2f} and interest coverage of {:.1f}x indicate solvency risks'
            },
            'Margin Trend Analysis': {
                'improving_margins': 'Profit margins showing {0} trend over analysis period',
                'deteriorating_margins': 'Profit margins showing {0} trend requiring attention',
                'stable_margins': 'Profit margins remain {0} across the analysis period'
            }
        }
        
        # Get template for this theorem and predicate
        theorem_templates = reasoning_templates.get(theorem_name, {})
        template = theorem_templates.get(predicate, f'{theorem_name} analysis completed')
        
        # Format with values
        try:
            if values:
                reasoning = template.format(*values)
            else:
                reasoning = template
        except (IndexError, ValueError):
            reasoning = f'{theorem_name}: {predicate.replace("_", " ")}'
        
        return reasoning
    
    def parse_sensitivity_conclusion(self, conclusion_str: str, theorem_name: str) -> Dict:
        """Parse sensitivity analysis conclusions"""
        import re
        
        # Extract current value and improvement paths
        if 'margin_sensitivity' in conclusion_str:
            # Extract current margin
            current_match = re.search(r'current\(([\d.]+)\)', conclusion_str)
            current = float(current_match.group(1)) if current_match else 0
            
            # Extract improvement paths
            revenue_match = re.search(r'revenue_growth\(([-\d.]+)\)', conclusion_str)
            cost_match = re.search(r'cost_reduction\(([-\d.]+)\)', conclusion_str)
            
            revenue_growth = float(revenue_match.group(1)) if revenue_match else 0
            cost_reduction = float(cost_match.group(1)) if cost_match else 0
            
            return {
                'recommendation': f'Margin Improvement Analysis',
                'reasoning': f'To improve margin by 25% (from {current:.1%} to {current*1.25:.1%}): '
                           f'Either grow revenue by {revenue_growth:.1f}% OR reduce costs by {cost_reduction:.1f}%',
                'values': [current, revenue_growth, cost_reduction]
            }
        
        elif 'asset_efficiency_analysis' in conclusion_str:
            # Extract current score
            score_match = re.search(r'current_score\(([\d.]+)\)', conclusion_str)
            score = float(score_match.group(1)) if score_match else 0
            
            # Extract improvement paths
            roa_match = re.search(r'improve_roa\(([\d.]+)\)', conclusion_str)
            revenue_match = re.search(r'increase_revenue\(([-\d.]+)\)', conclusion_str)
            
            roa_improvement = float(roa_match.group(1)) if roa_match else 0
            revenue_increase = float(revenue_match.group(1)) if revenue_match else 0
            
            return {
                'recommendation': f'Asset Efficiency Analysis',
                'reasoning': f'Current efficiency score: {score:.2f}. To improve by 30%: '
                           f'Either improve ROA by {roa_improvement:.0f}% OR increase revenue by {revenue_increase:.1f}%',
                'values': [score, roa_improvement, revenue_increase]
            }
        
        # Default fallback
        return {
            'recommendation': theorem_name,
            'reasoning': f'Sensitivity analysis completed: {conclusion_str[:100]}',
            'values': []
        }
    
    def extract_facts(self, files: List[Path]) -> Dict:
        """Extract facts from selected files"""
        print("\n" + "=" * 70)
        print("FACT EXTRACTION")
        print("-" * 70)
        
        all_facts = []
        companies = set()
        
        for file_path in files:
            print(f"\nProcessing: {file_path.name}")
            
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            company = self.extract_company_name(file_path.name)
            companies.add(company)
            
            # Extract facts
            facts = self.extractor.extract_facts(content, company, file_path.name)
            print(f"  Extracted {len(facts)} raw facts")
            
            # Show sample of extracted metrics
            metrics = {}
            for fact in facts:
                key = fact['key']
                if key not in metrics:
                    metrics[key] = []
                metrics[key].append(f"{fact['value']:.2f} ({fact['period_label']})")
            
            for key, values in sorted(metrics.items())[:5]:  # Show first 5 metrics
                print(f"    {key}: {', '.join(values[:2])}")  # Show first 2 periods
            
            all_facts.extend(facts)
        
        print(f"\nTotal facts extracted: {len(all_facts)}")
        print(f"Companies identified: {', '.join(sorted(companies))}")
        
        return {
            'facts': all_facts,
            'companies': list(companies),
            'files': [f.name for f in files]
        }
    
    def calculate_derived_metrics(self, facts: List[Dict]) -> List[Dict]:
        """Calculate derived metrics from base facts"""
        print("\n" + "=" * 70)
        print("CALCULATING DERIVED METRICS")
        print("-" * 70)
        
        derived = self.calculator.calculate_all(facts)
        
        # Report what was calculated
        calculated_metrics = {}
        for fact in derived:
            key = fact['key']
            if key not in calculated_metrics:
                calculated_metrics[key] = 0
            calculated_metrics[key] += 1
        
        if calculated_metrics:
            print("\nDerived metrics calculated:")
            for metric, count in sorted(calculated_metrics.items()):
                print(f"  - {metric}: {count} period(s)")
        else:
            print("\nNo derived metrics could be calculated (insufficient data)")
        
        # Report what couldn't be calculated
        missing = self.calculator.report_missing_calculations(facts)
        if missing:
            print("\nMetrics that couldn't be calculated:")
            for metric, reason in missing.items():
                print(f"  - {metric}: {reason}")
        
        return derived
    
    def check_data_availability(self, facts: List[Dict]) -> Dict:
        """Check which theorems can run with available data"""
        print("\n" + "=" * 70)
        print("DATA AVAILABILITY CHECK")
        print("-" * 70)
        
        availability = self.availability_tracker.check_all_theorems(facts)
        
        print(f"\nTheorems that can run: {availability['can_run']}/{availability['total']}")
        
        if availability['runnable']:
            print("\nRunnable theorems:")
            for theorem in availability['runnable'][:10]:  # Show first 10
                print(f"  ✓ {theorem}")
        
        if availability['missing_data']:
            print("\nTheorems missing data:")
            for theorem, missing in list(availability['missing_data'].items())[:5]:
                print(f"  ✗ {theorem}: needs {', '.join(missing)}")
        
        return availability
    
    def run_prolog_analysis(self, facts_file: str) -> List[Dict]:
        """Run Prolog theorem engine"""
        output_file = tempfile.mktemp(suffix='.json')
        
        try:
            cmd = [
                'swipl', '-q', '-s', 'engine.pl', '--',
                facts_file, 'theorems.pl', output_file
            ]
            
            result = subprocess.run(cmd, capture_output=True, text=True)
            
            if result.returncode != 0:
                print(f"Prolog error: {result.stderr}")
                return []
            
            conclusions = []
            if os.path.exists(output_file):
                with open(output_file, 'r') as f:
                    for line in f:
                        if line.strip():
                            try:
                                conclusions.append(json.loads(line))
                            except json.JSONDecodeError:
                                pass
            
            return conclusions
        
        finally:
            if os.path.exists(output_file):
                os.remove(output_file)
    
    def apply_theorems(self, facts: List[Dict]) -> List[Dict]:
        """Apply theorems using unified Prolog engine"""
        print("\n" + "=" * 70)
        print("APPLYING THEOREMS")
        print("-" * 70)
        
        # Store facts for use in intelligent conclusions
        self.current_facts = facts
        
        conclusions = self.apply_unified_theorems(facts)
        
        print(f"\nGenerated {len(conclusions)} conclusions")
        
        # Show summary by theorem type
        by_theorem = {}
        for c in conclusions:
            theorem = c.get('theorem', 'unknown')
            if theorem not in by_theorem:
                by_theorem[theorem] = 0
            by_theorem[theorem] += 1
        
        if by_theorem:
            print("\nConclusions by theorem:")
            for theorem, count in sorted(by_theorem.items()):
                print(f"  - {theorem}: {count}")
        
        return conclusions
    
    def apply_unified_theorems(self, facts: List[Dict]) -> List[Dict]:
        """Apply unified theorem engine with multi-pass sequential reasoning"""
        import re
        print("\nUsing unified multi-pass theorem engine...")
        
        # Get unique companies from facts
        companies = set()
        for fact in facts:
            if 'company' in fact:
                company_name = fact['company'].lower().replace(' ', '_')
                companies.add(company_name)
        
        if not companies:
            print("No companies found in facts")
            return []
        
        # Generate normalized facts file
        norm_facts_file = 'ddar_facts_norm.pl'
        self.adapter.generate_prolog_facts_file(facts, norm_facts_file)
        
        # Generate bridge rules
        bridge_file = 'ddar_bridge.pl'
        self.adapter.generate_bridge_rules(facts, bridge_file)
        
        # Create test runner for unified theorem engine
        engine_file = 'run_unified_theorems.pl'
        
        with open(engine_file, 'w') as f:
            f.write("%%% Run unified theorem engine on DDAR facts\n\n")
            f.write(":- consult('theorems/theorem_engine.pl').\n")
            f.write(":- consult('ddar_facts_norm.pl').\n")
            f.write(":- consult('ddar_bridge.pl').\n\n")
            
            # Add unified analysis runner (renamed to avoid conflict)
            f.write("run_unified_analysis :-\n")
            f.write("    bridge_all_facts,\n")
            f.write("    writeln('Unified Theorem Analysis'),\n")
            f.write("    format('~`=t~50|~n', []),\n")
            f.write("    writeln(''),\n")
            
            # Run both standard and iterative analysis for each company
            for company in sorted(companies):
                f.write(f"    analyze_company({company}, Results_{company}),\n")
                f.write(f"    writeln('RESULTS_FOR_{company.upper()}:'),\n")
                f.write(f"    writeln(Results_{company}),\n")
                f.write(f"    writeln(''),\n")
                f.write(f"    writeln('ITERATIVE_ANALYSIS_FOR_{company.upper()}:'),\n")
                f.write(f"    analyze_company_iterative({company}, IterativeResults_{company}),\n")
                f.write(f"    report_iterative_analysis(IterativeResults_{company}),\n")
            
            f.write("    halt.\n\n")
            f.write(":- initialization(run_unified_analysis).\n")
        
        try:
            # Run the theorems
            cmd = ['swipl', '-q', '-s', engine_file]
            result = subprocess.run(cmd, capture_output=True, text=True)
            
            print("\nTheorem execution output:")
            print("-" * 40)
            
            # Parse and display iterative analysis progress
            if result.stdout:
                lines = result.stdout.split('\n')
                iteration_count = 0
                sensitivity_count = 0
                
                for line in lines:
                    # Track iterations
                    if '=== Starting Iterative Analysis' in line:
                        print("\n" + "="*60)
                        print("ITERATIVE OPTIMIZATION LOOP STARTING")
                        print("="*60)
                    elif '--- Iteration' in line:
                        iteration_count += 1
                        print(f"\n🔄 ITERATION {iteration_count}")
                        print("-"*40)
                    elif 'Found' in line and 'sensitivity analyses' in line:
                        match = re.search(r'Found (\d+) sensitivity', line)
                        if match:
                            sensitivity_count = int(match.group(1))
                            print(f"📊 Analyzing {sensitivity_count} sensitivity paths")
                    elif 'Sensitivity for' in line:
                        theorem = line.split('Sensitivity for')[1].strip()
                        print(f"  ✓ {theorem}")
                    elif 'No more improvements available' in line:
                        print("\n⚠️  CONVERGENCE: No feasible improvements within constraints")
                    elif 'Selected improvements:' in line:
                        print("\n✅ Improvements selected for next iteration")
                    elif 'RESULTS_FOR' in line:
                        print("\n" + "="*60)
                        print("FINAL RESULTS")
                        print("="*60)
                        print(line)
                    elif result and 'result(' in line:
                        # Show results more clearly
                        results = re.findall(r'result\((\w+),(\w+),([^)]+)\)', line)
                        if results:
                            print("\nTheorems successfully applied:")
                            for theorem, metric, value in results:
                                try:
                                    val = float(value)
                                    if 'roic' in theorem:
                                        print(f"  • {theorem}: {metric} = {val:.4%}")
                                    elif 'fcf' in theorem:
                                        print(f"  • {theorem}: {metric} = ${val/1000000:.1f}M")
                                    elif 'ccc' in theorem or 'cycle' in theorem:
                                        print(f"  • {theorem}: {metric} = {val:.1f} days")
                                    else:
                                        print(f"  • {theorem}: {metric} = {val:.4f}")
                                except:
                                    print(f"  • {theorem}: {metric} = {value}")
                
                print(f"\n📈 Summary: {iteration_count} iterations completed, {sensitivity_count} paths analyzed")
            
            if result.stderr:
                # Only show actual errors, not warnings
                errors = [e for e in result.stderr.split('\n') if 'ERROR' in e]
                if errors:
                    print("\n❌ Errors:")
                    for error in errors:
                        print(f"  {error}")
            
            # Parse conclusions from output
            conclusions = []
            current_company = None
            
            # Look for result list in output
            results_pattern = r'\[result\((.*?)\)\]'
            
            for line in result.stdout.split('\n'):
                if 'Analyzing:' in line:
                    current_company = line.split('Analyzing:')[1].strip()
                elif 'RESULTS_FOR_' in line:
                    # Extract company name from RESULTS_FOR_AXIATA: format
                    company_match = re.search(r'RESULTS_FOR_(\w+):', line)
                    if company_match:
                        current_company = company_match.group(1).lower()
                
                # Check if line contains results list
                if 'result(' in line and '[' in line:
                    # Parse Prolog result tuples: result(theorem_name, metric, value)
                    result_matches = re.findall(r'result\(([^,]+),([^,]+),([^)]+)\)', line)
                    
                    # Use a set to avoid duplicates
                    seen_theorems = set()
                    
                    for theorem_name, metric, value in result_matches:
                        if (theorem_name, metric) not in seen_theorems:
                            seen_theorems.add((theorem_name, metric))
                            
                            # Map theorem names to descriptions
                            theorem_descriptions = {
                                'roic_decomp': 'ROIC Decomposition Analysis',
                                'roic_nopat': 'ROIC from NOPAT',
                                'fcf_simple': 'Free Cash Flow Analysis',
                                'fcf_conversion': 'FCF Conversion Efficiency',
                                'cfroi': 'Cash Flow Return on Investment',
                                'ccc': 'Cash Conversion Cycle',
                                'operating_cycle': 'Operating Cycle Analysis',
                                'dupont': 'DuPont ROE Analysis',
                                'roa_basic': 'Return on Assets',
                                'current_ratio': 'Current Ratio (Liquidity)',
                                'debt_to_equity': 'Debt to Equity Ratio',
                                'interest_coverage': 'Interest Coverage'
                            }
                            
                            # Generate intelligent conclusion with reasoning chain
                            try:
                                numeric_value = float(value)
                                
                                # Get all facts for this company and period
                                company_facts = {}
                                for fact in self.current_facts:
                                    if fact.get('company', '').lower() == (current_company or 'axiata').lower():
                                        try:
                                            company_facts[fact['key']] = float(fact['value'])
                                        except:
                                            pass
                                
                                # Create context for intelligent analysis
                                context = TheoremContext(
                                    company=current_company or 'axiata',
                                    theorem=theorem_name,
                                    metric=metric,
                                    value=numeric_value,
                                    all_facts=company_facts,
                                    period='latest'
                                )
                                
                                # Generate comprehensive conclusion
                                intelligent_result = self.conclusion_engine.generate_intelligent_conclusion(context)
                                
                                conclusions.append({
                                    'theorem': theorem_name.upper(),
                                    'name': theorem_descriptions.get(theorem_name, theorem_name),
                                    'company': current_company or 'axiata',
                                    'metric': metric,
                                    'value': value,
                                    'conclusion': intelligent_result['conclusion'],
                                    'recommendation': ' '.join(intelligent_result['recommendations'][:2]),  # Top 2 recommendations
                                    'reasoning': ' → '.join(intelligent_result['reasoning_chain'][:3]),  # Top 3 reasoning steps
                                    'numerical_insights': intelligent_result.get('numerical_insights', []),
                                    'implications': intelligent_result.get('implications', []),
                                    'confidence': intelligent_result['confidence'],
                                    'support_fact_ids': intelligent_result.get('evidence', [])
                                })
                            except Exception as e:
                                print(f"Error in intelligent conclusion: {e}")
                                # Fallback to simple conclusion
                                conclusion_text = self.generate_conclusion(theorem_name, metric, float(value))
                                conclusions.append({
                                    'theorem': theorem_name.upper(),
                                    'name': theorem_descriptions.get(theorem_name, theorem_name),
                                    'company': current_company or 'axiata',
                                    'metric': metric,
                                    'value': value,
                                    'conclusion': conclusion_text,
                                    'recommendation': self.generate_recommendation(theorem_name, metric, value),
                                    'reasoning': f"Based on {metric} calculation of {value}",
                                    'confidence': 0.70,
                                    'support_fact_ids': []
                                })
            
            return conclusions
            
        finally:
            # Cleanup (keep files for debugging)
            for f in [engine_file]:  # Only remove engine file
                if os.path.exists(f):
                    os.remove(f)
    
    def generate_report(self, data: Dict, conclusions: List[Dict], availability: Dict) -> str:
        """Generate comprehensive report"""
        print("\n" + "=" * 70)
        print("GENERATING REPORT")
        print("-" * 70)
        
        # Prepare report data
        report_data = {
            'timestamp': datetime.now().isoformat(),
            'companies': data['companies'],
            'files_analyzed': data['files'],
            'facts': data['facts'],
            'conclusions': conclusions,
            'data_availability': availability,
            'metadata': {
                'total_facts': len(data['facts']),
                'total_conclusions': len(conclusions),
                'theorems_applied': availability['can_run'],
                'theorems_skipped': availability['total'] - availability['can_run']
            }
        }
        
        # Generate HTML report
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        company_prefix = data['companies'][0] if data['companies'] else "Analysis"
        report_filename = f"{company_prefix}_DDAR_{timestamp}.html"
        report_path = self.output_dir / "reports" / report_filename
        
        # Use enhanced report generator
        self.enhanced_report_generator.generate_html_report(report_data, report_path)
        
        print(f"\nReport generated: {report_path}")
        
        # Also save raw JSON for further analysis
        json_path = report_path.with_suffix('.json')
        with open(json_path, 'w') as f:
            json.dump(report_data, f, indent=2, default=str)
        
        print(f"Raw data saved: {json_path}")
        
        return str(report_path)
    
    def generate_conclusion(self, theorem_name: str, metric: str, value: float) -> str:
        """Generate human-readable conclusion based on theorem results"""
        
        # ROIC conclusions
        if 'roic' in theorem_name.lower():
            if value < 0.05:
                return f"ROIC of {value:.2%} indicates poor capital efficiency - returns below typical cost of capital"
            elif value < 0.10:
                return f"ROIC of {value:.2%} suggests moderate capital efficiency - may be below WACC"
            elif value < 0.15:
                return f"ROIC of {value:.2%} shows good capital efficiency - likely exceeding cost of capital"
            else:
                return f"ROIC of {value:.2%} demonstrates excellent capital efficiency and value creation"
        
        # FCF conclusions
        elif 'fcf' in theorem_name.lower() and 'conversion' not in theorem_name.lower():
            if value < 0:
                return f"Negative FCF of {value:,.0f} indicates cash consumption - investment phase or operational issues"
            elif value < 1000000:
                return f"FCF of {value:,.0f} shows modest cash generation"
            else:
                return f"Strong FCF of {value:,.0f} provides flexibility for growth and returns"
        
        # FCF Conversion
        elif 'fcf_conversion' in theorem_name.lower():
            if value < 0.5:
                return f"FCF conversion of {value:.1f}x indicates weak cash generation relative to EBITDA"
            elif value < 1.0:
                return f"FCF conversion of {value:.1f}x shows moderate cash efficiency"
            else:
                return f"FCF conversion of {value:.1f}x demonstrates strong cash generation efficiency"
        
        # CFROI
        elif 'cfroi' in theorem_name.lower():
            if value < 0.08:
                return f"CFROI of {value:.1%} suggests poor cash returns on invested capital"
            elif value < 0.15:
                return f"CFROI of {value:.1%} indicates moderate cash generation returns"
            else:
                return f"CFROI of {value:.1%} shows strong cash-based returns"
        
        # Cash Conversion Cycle
        elif 'ccc' in theorem_name.lower():
            if value < 0:
                return f"Negative CCC of {value:.1f} days - receiving cash before paying suppliers (favorable)"
            elif value < 30:
                return f"CCC of {value:.1f} days indicates efficient working capital management"
            elif value < 60:
                return f"CCC of {value:.1f} days suggests moderate working capital efficiency"
            else:
                return f"CCC of {value:.1f} days indicates slow cash conversion - may pressure liquidity"
        
        # Operating Cycle
        elif 'operating_cycle' in theorem_name.lower():
            if value < 30:
                return f"Operating cycle of {value:.1f} days shows very efficient operations"
            elif value < 60:
                return f"Operating cycle of {value:.1f} days indicates good operational efficiency"
            else:
                return f"Operating cycle of {value:.1f} days suggests room for operational improvement"
        
        # Default
        else:
            return f"{metric}: {value:.4f}"
    
    def generate_recommendation(self, theorem_name: str, metric: str, value) -> str:
        """Generate actionable recommendations based on theorem results"""
        
        try:
            numeric_value = float(value)
        except:
            return "Review calculation methodology"
        
        # ROIC recommendations
        if 'roic' in theorem_name.lower():
            if numeric_value < 0.10:
                return "Focus on improving margins or asset turnover to enhance capital returns"
            else:
                return "Maintain capital discipline while seeking growth opportunities"
        
        # FCF recommendations
        elif 'fcf' in theorem_name.lower() and 'conversion' not in theorem_name.lower():
            if numeric_value < 0:
                return "Review capital allocation and working capital management"
            else:
                return "Consider strategic uses for excess cash generation"
        
        # CCC recommendations
        elif 'ccc' in theorem_name.lower():
            if numeric_value > 45:
                return "Optimize receivables collection and inventory turnover"
            else:
                return "Maintain working capital efficiency"
        
        # CFROI recommendations
        elif 'cfroi' in theorem_name.lower():
            if numeric_value < 0.10:
                return "Evaluate asset productivity and operational efficiency"
            else:
                return "Continue focus on cash generation optimization"
        
        return "Monitor trend and benchmark against peers"
    
    def run(self):
        """Main application flow"""
        self.display_header()
        
        # Step 1: Select files
        selected_files = self.select_files()
        if not selected_files:
            return
        
        # Step 2: Extract facts
        data = self.extract_facts(selected_files)
        
        # Step 3: Calculate derived metrics
        derived_facts = self.calculate_derived_metrics(data['facts'])
        data['facts'].extend(derived_facts)
        
        # Step 4: Check data availability
        availability = self.check_data_availability(data['facts'])
        
        # Step 5: Apply theorems
        conclusions = self.apply_theorems(data['facts'])
        
        # Step 6: Generate report
        report_path = self.generate_report(data, conclusions, availability)
        
        # Final summary
        print("\n" + "=" * 70)
        print("ANALYSIS COMPLETE")
        print("=" * 70)
        print(f"\n✓ Files analyzed: {len(selected_files)}")
        print(f"✓ Facts extracted: {len(data['facts'])}")
        print(f"✓ Conclusions generated: {len(conclusions)}")
        print(f"✓ Report saved: {report_path}")
        
        # Ask if user wants to open the report
        if input("\nOpen report in browser? (y/n): ").lower() == 'y':
            import webbrowser
            webbrowser.open(f"file://{os.path.abspath(report_path)}")


def main():
    """Entry point"""
    app = DDARApplication()
    try:
        app.run()
    except KeyboardInterrupt:
        print("\n\nAnalysis cancelled by user.")
        sys.exit(0)
    except Exception as e:
        print(f"\n\nError: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
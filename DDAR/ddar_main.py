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
from fact_extractor import FactExtractor
from calculation_engine import CalculationEngine
from data_availability import DataAvailabilityTracker
from report_generator import ReportGenerator
from ddar_adapter import DDARAdapter

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
        self.adapter = DDARAdapter()
        
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
            
            # Run analysis for each company
            for company in sorted(companies):
                f.write(f"    analyze_company({company}, _),\n")
            
            f.write("    halt.\n\n")
            f.write(":- initialization(run_unified_analysis).\n")
        
        try:
            # Run the theorems
            cmd = ['swipl', '-q', '-s', engine_file]
            result = subprocess.run(cmd, capture_output=True, text=True)
            
            print("\nTheorem execution output:")
            print("-" * 40)
            if result.stdout:
                print(result.stdout)
            if result.stderr:
                print("Errors:", result.stderr)
            
            # Parse conclusions from output
            conclusions = []
            current_company = None
            for line in result.stdout.split('\n'):
                if 'Analyzing:' in line:
                    current_company = line.split('Analyzing:')[1].strip()
                elif any(theorem in line for theorem in ['t001', 't002', 't003', 't004', 't011', 't015', 't020', 't021', 't030', 't031', 't040', 't041']):
                    # Parse theorem results
                    parts = line.strip().split(':')
                    if len(parts) >= 2:
                        theorem_id = parts[0].strip()
                        result_str = ':'.join(parts[1:]).strip()
                        
                        theorem_names = {
                            't001': 'Value Creation',
                            't002': 'DuPont Identity',
                            't003': 'Extended DuPont',
                            't004': 'Sustainable Growth',
                            't011': 'Free Cash Flow',
                            't015': 'ROIC Decomposition',
                            't020': 'Operating Efficiency',
                            't021': 'Asset Efficiency',
                            't030': 'Liquidity Assessment',
                            't031': 'Solvency Assessment',
                            't040': 'Margin Trend Analysis',
                            't041': 'Economic Value Added'
                        }
                        
                        if theorem_id in theorem_names:
                            # Parse the Prolog conclusion
                            parsed = self.parse_prolog_conclusion(result_str, theorem_names[theorem_id])
                            
                            conclusions.append({
                                'theorem': theorem_id.upper(),
                                'name': theorem_names[theorem_id],
                                'company': current_company,
                                'conclusion': result_str,  # Keep original for debugging
                                'recommendation': parsed['recommendation'],
                                'reasoning': parsed['reasoning'],
                                'confidence': 0.90,
                                'support_fact_ids': []  # TODO: Track actual fact IDs used
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
        
        self.report_generator.generate_html_report(report_data, report_path)
        
        print(f"\nReport generated: {report_path}")
        
        # Also save raw JSON for further analysis
        json_path = report_path.with_suffix('.json')
        with open(json_path, 'w') as f:
            json.dump(report_data, f, indent=2, default=str)
        
        print(f"Raw data saved: {json_path}")
        
        return str(report_path)
    
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
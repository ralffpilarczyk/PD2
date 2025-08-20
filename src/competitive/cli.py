"""
Standalone CLI for Competitive Analysis.
Provides command-line interface for testing and development.
"""

import os
import sys
import json
from pathlib import Path
from datetime import datetime
from typing import Optional

# Add the parent directory to the path so we can import from src
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from src.utils import thread_safe_print
from src.competitive.database import CompetitiveDatabase
from src.competitive.segment_extractor import SegmentExtractor
from src.competitive.peer_discovery import PeerDiscovery
from src.competitive.metric_engine import MetricEngine
# Removed competitive analyzer and strategy bundler - focusing on data collection only
from src.competitive.report_generator import CompetitiveReportGenerator
from src.competitive.pivot_exporter import PivotExporter

# Import Google Generative AI
try:
    import google.generativeai as genai
    from dotenv import load_dotenv
    load_dotenv()
    genai.configure(api_key=os.environ.get("GEMINI_API_KEY"))
except ImportError as e:
    print(f"Error importing required packages: {e}")
    print("Please ensure google-generativeai and python-dotenv are installed:")
    print("pip install google-generativeai python-dotenv")
    sys.exit(1)


class CompetitiveAnalysisCLI:
    """Command-line interface for competitive analysis"""
    
    def __init__(self):
        """Initialize CLI with database and analysis components"""
        # Create output directory with timestamp
        self.timestamp = datetime.now().strftime("%Y_%m_%d_%H_%M_%S")
        self.output_dir = Path(f"runs/run_{self.timestamp}/competitive")
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Initialize database
        db_path = self.output_dir / "competitive_analysis.db"
        self.db = CompetitiveDatabase(str(db_path))
        
        # Initialize analysis components
        self.segment_extractor = SegmentExtractor(self.db)
        self.peer_discovery = PeerDiscovery(self.db)
        self.metric_engine = MetricEngine(self.db)
        # Removed analyzer and bundler - focusing on pure data collection
        self.report_generator = CompetitiveReportGenerator(self.db, str(self.output_dir))
        self.pivot_exporter = PivotExporter(self.db, str(self.output_dir))
        
        thread_safe_print("Competitive Analysis CLI initialized")
        thread_safe_print(f"Output directory: {self.output_dir}")
        thread_safe_print(f"Database: {db_path}")
    
    def select_source_files(self) -> list:
        """Open a file chooser to select PDFs and/or Markdown files."""
        from tkinter import filedialog
        import tkinter as tk
        while True:
            root = tk.Tk()
            root.withdraw()
            print("\nSelect PDF files (for conversion) and/or Markdown/Text files (direct use)...")
            selected = filedialog.askopenfilenames(
                title="Select PDF and/or Markdown/Text Files",
                filetypes=[
                    ("PDF files", "*.pdf"),
                    ("Markdown files", "*.md"),
                    ("Text files", "*.txt"),
                    ("All files", "*.*"),
                ],
            )
            root.destroy()
            if not selected:
                print("No files selected. Please try again.")
                continue
            return list(selected)

    def load_documents_to_text(self, paths: list) -> str:
        """Read selected files into a single text blob. PDFs use pdfminer extraction."""
        contents = []
        for p in paths:
            try:
                lower = p.lower()
                if lower.endswith(".md") or lower.endswith(".txt"):
                    with open(p, "r", encoding="utf-8") as f:
                        contents.append(f"--- Document: {p} ---\n" + f.read())
                elif lower.endswith(".pdf"):
                    try:
                        from pdfminer.high_level import extract_text
                        text = extract_text(p) or ""
                        contents.append(f"--- PDF: {p} ---\n" + text)
                    except Exception as e:
                        print(f"Warning: PDF extraction failed for {p}: {e}")
                else:
                    print(f"Skipping unsupported file type: {p}")
            except Exception as e:
                print(f"Warning: Failed to read {p}: {e}")
        return "\n\n".join(contents).strip()
    
    def prompt_max_competitors(self) -> int:
        """Prompt user for maximum competitors per market cell"""
        print("\nHow many competitors per market cell?")
        print("Recommended: 3 (default)")
        print("Range: 1-7")
        
        while True:
            user_input = input("Enter number (or press Enter for default 3): ").strip()
            
            if not user_input:
                return 3
            
            try:
                num = int(user_input)
                if 1 <= num <= 7:
                    return num
                else:
                    print("Please enter a number between 1 and 7.")
            except ValueError:
                print("Please enter a valid number.")
    
    def prompt_analysis_scope(self) -> str:
        """Prompt user for analysis scope"""
        print("\nWhat scope of analysis would you like?")
        print("1. Discovery only: Market cells + competitor identification")
        print("2. Full data collection: Discovery + metrics collection (recommended)")
        print("")
        
        while True:
            choice = input("Choose option (1-2, or press Enter for full): ").strip()
            
            if not choice or choice == "2":
                return "full"
            elif choice == "1":
                return "discovery"
            else:
                print("Invalid choice. Please enter 1 or 2.")
    
    def analyze_company(self, company_name: str = None, document_content: str = "", 
                       max_competitors: int = 3, analysis_scope: str = "full") -> Optional[dict]:
        """
        Run complete competitive analysis for a company.
        Returns analysis results or None if failed.
        """
        try:
            thread_safe_print(f"\nStarting competitive data collection")
            
            # Initialize results variables
            all_competitors = {}
            metrics_summary = {}
            html_report_path = ""
            json_evidence_path = ""
            
            # Phase 1: Business Segment Extraction
            thread_safe_print("\nPhase 1: Company Analysis & Business Segment Extraction")
            company_context = self.segment_extractor.extract_company_context(
                document_content=document_content,
                company_name=company_name
            )
            business_segments = self.segment_extractor.extract_business_segments(company_context)
            
            if not business_segments:
                thread_safe_print("No business segments extracted. Analysis cannot continue.")
                return None
            
            # Display discovered segments for user visibility
            thread_safe_print(f"\nDiscovered {len(business_segments)} business segments:")
            for i, segment in enumerate(business_segments, 1):
                revenue = segment.get('revenue_contribution', 'N/A')
                profit = segment.get('profit_contribution', 'N/A')
                geography = segment.get('geographic_focus', 'N/A')
                thread_safe_print(f"  {i}. {segment['segment_name']}")
                thread_safe_print(f"     Revenue: {revenue}, Profit: {profit}, Geography: {geography}")
            thread_safe_print("")  # Blank line for readability
            
            # Save company and segments to database
            company_id = self.db.insert_company(company_context['company_name'], company_context)
            for segment in business_segments:
                self.db.insert_business_segment(company_id, segment)
            
            # Get company ID from database
            company_row = self.db.get_company_by_name(company_context['company_name'])
            if not company_row:
                thread_safe_print("Company not found in database after insertion.")
                return None
            
            company_id = company_row['id']
            
            # Phase 2: Peer Discovery
            thread_safe_print(f"\nPhase 2: Peer Discovery for {len(business_segments)} business segments")
            
            # For backward compatibility, create market_cells from segments
            market_cells = []
            for segment in business_segments:
                # Create a market cell entry for backward compatibility
                cell_id = self.db.insert_market_cell(
                    company_id=company_id,
                    product_service=segment.get('products_services', segment['segment_name']),
                    geography=segment.get('geographic_focus', 'Global'),
                    customer_segment='Mixed',  # Segments don't always specify this
                    materiality_score=segment.get('significance_score', 0.5)
                )
                market_cells.append({
                    'id': cell_id,
                    'product_service': segment.get('products_services', segment['segment_name']),
                    'geography': segment.get('geographic_focus', 'Global'),
                    'customer_segment': 'Mixed',
                    'materiality_score': segment.get('significance_score', 0.5)
                })
            
            all_competitors = {}
            for segment, cell in zip(business_segments, market_cells):
                competitors = self.peer_discovery.discover_peers_for_segment(
                    company_context=company_context,
                    business_segment=segment,
                    segment_id=cell['id'],  # Use market_cell_id for now
                    max_competitors=max_competitors
                )
                all_competitors[cell['id']] = competitors
            
            # Phase 2: Metric Collection & Normalization (if requested)
            # Additional variables already initialized at top
            
            # Phase 2: Metric Collection & Data Normalization  
            if analysis_scope == "full":
                thread_safe_print(f"\nPhase 2: Metric Collection & Data Normalization")
                metrics_results = self.metric_engine.process_and_save_all_metrics(company_id)
                
                if metrics_results.get('success'):
                    metrics_summary = metrics_results['summary']
                    thread_safe_print(f"Phase 2 completed successfully")
                else:
                    thread_safe_print(f"Phase 2 completed with limited data: {metrics_results.get('error', 'Unknown error')}")
                    metrics_summary = {"error": metrics_results.get('error')}
            
            # Generate HTML report with data tables (no analysis)
            if analysis_scope == "full" and metrics_summary:
                thread_safe_print(f"\nGenerating data collection report...")
                
                html_report_path = self.report_generator.generate_data_collection_report(
                    company_id=company_id
                )
                
                json_evidence_path = self.report_generator.generate_json_evidence_pack(
                    company_id=company_id
                )
                
                thread_safe_print(f"Data collection report generated")
            
            # Export pivot outputs (principles-based matrix for analysts)
            try:
                pivots_json_path = self.pivot_exporter.export_all_market_pivots(company_id)
            except Exception as e:
                thread_safe_print(f"Pivot export failed: {e}")
                pivots_json_path = ""

            # Compile results
            results = {
                'company_context': company_context,
                'business_segments': business_segments,
                'market_cells': market_cells,  # Keep for backward compatibility
                'competitors_by_segment': all_competitors,
                'competitors_by_market_cell': all_competitors,  # Keep for backward compatibility
                'metrics_summary': metrics_summary,
                'analysis_metadata': {
                    'timestamp': self.timestamp,
                    'company_id': company_id,
                    'analysis_scope': analysis_scope,
                    'total_market_cells': len(market_cells),
                    'total_competitors_found': sum(len(comps) for comps in all_competitors.values()),
                    'output_directory': str(self.output_dir),
                    'html_report_path': html_report_path,
                    'json_evidence_path': json_evidence_path,
                    'market_pivots_path': pivots_json_path
                }
            }
            
            # Save results to JSON
            results_file = self.output_dir / "analysis_results.json"
            with open(results_file, 'w', encoding='utf-8') as f:
                json.dump(results, f, indent=2, default=str)
            
            thread_safe_print(f"\nAnalysis complete. Results saved to: {results_file}")
            return results
            
        except Exception as e:
            thread_safe_print(f"Analysis failed: {e}")
            import traceback
            traceback.print_exc()
            return None
    
    def display_results_summary(self, results: dict):
        """Display a summary of analysis results"""
        if not results:
            return
        
        print("\n" + "="*60)
        print("COMPETITIVE ANALYSIS SUMMARY")
        print("="*60)
        
        # Company information
        company_context = results['company_context']
        print(f"\nCompany: {company_context['company_name']}")
        print(f"Industry: {company_context.get('industry', 'Unknown')}")
        print(f"Business Model: {company_context.get('business_model', 'Unknown')}")
        print(f"Geography: {company_context.get('geography', 'Unknown')}")
        
        # Business segments
        segments = results.get('business_segments', [])
        if segments:
            print(f"\nBusiness Segments ({len(segments)}):")
            for i, segment in enumerate(segments, 1):
                print(f"  {i}. {segment['segment_name']}")
                if segment.get('revenue_contribution'):
                    print(f"     Revenue: {segment['revenue_contribution']}")
                if segment.get('geographic_focus'):
                    print(f"     Geography: {segment['geographic_focus']}")
                print(f"     Significance: {segment.get('significance_score', 0):.2f}")
        else:
            # Fallback to market cells for backward compatibility
            market_cells = results.get('market_cells', [])
            print(f"\nMarket Cells ({len(market_cells)}):")
            for i, cell in enumerate(market_cells, 1):
                print(f"  {i}. {cell['product_service']} × {cell['geography']} × {cell['customer_segment']}")
                print(f"     Materiality: {cell['materiality_score']:.2f}")
        
        # Competitors
        competitors_by_cell = results['competitors_by_market_cell']
        print(f"\nCompetitors by Market Cell:")
        
        for cell_id, competitors in competitors_by_cell.items():
            # Find the corresponding market cell
            market_cell = None
            for cell in market_cells:
                if cell_id in [mc['id'] for mc in self.db.get_market_cells_for_company(results['analysis_metadata']['company_id'])]:
                    market_cell_row = self.db.get_connection().execute(
                        "SELECT * FROM market_cells WHERE id = ?", (cell_id,)
                    ).fetchone()
                    if market_cell_row:
                        market_cell = f"{market_cell_row['product_service']} × {market_cell_row['geography']} × {market_cell_row['customer_segment']}"
                        break
            
            if market_cell:
                print(f"\n  {market_cell}:")
                if competitors:
                    for comp in competitors:
                        print(f"    • {comp['name']} (Evidence: {comp['evidence_score']:.2f})")
                        print(f"      {comp['presence_evidence']}")
                else:
                    print(f"    • No competitors found")
        
        # Metrics summary (Phase 2)
        metrics_summary = results.get('metrics_summary')
        if metrics_summary:
            print(f"\nMetrics Collection Summary:")
            if 'error' in metrics_summary:
                print(f"  Error: {metrics_summary['error']}")
            else:
                print(f"  Metrics Attempted: {metrics_summary.get('metrics_attempted', 0)}")
                print(f"  Observations Saved: {metrics_summary.get('observations_saved', 0)}")
                print(f"  Success Rate: {metrics_summary.get('observations_saved', 0) / max(1, metrics_summary.get('metrics_attempted', 1)):.1%}")
                print(f"  Normalization Errors: {metrics_summary.get('normalization_errors', 0)}")
        
        # Competitive analysis summary (Phase 3)
        competitive_analyses = results.get('competitive_analyses', {})
        if competitive_analyses:
            print(f"\nCompetitive Analysis Summary:")
            total_analyzed = len(competitive_analyses)
            successful_analyses = sum(1 for analysis in competitive_analyses.values() 
                                    if analysis.get('competitive_analysis', {}).get('success'))
            print(f"  Market Cells Analyzed: {successful_analyses}/{total_analyzed}")
            
            # Overall competitive positions
            positions = []
            for analysis_data in competitive_analyses.values():
                comp_analysis = analysis_data.get('competitive_analysis', {})
                if comp_analysis.get('success'):
                    position = comp_analysis.get('overall_position', {}).get('position', '')
                    if position:
                        positions.append(position)
            
            if positions:
                most_common_position = max(set(positions), key=positions.count)
                print(f"  Most Common Position: {most_common_position}")
        
        # Strategy bundles summary (Phase 3)
        strategy_bundles = results.get('strategy_bundles', {})
        if strategy_bundles:
            total_bundles = sum(len(bundles) for bundles in strategy_bundles.values())
            print(f"\nStrategy Recommendations:")
            print(f"  Total Strategy Bundles: {total_bundles}")
            print(f"  Market Cells with Strategies: {len([k for k, v in strategy_bundles.items() if v])}")
        
        # Report paths (Phase 3)
        html_path = results.get('analysis_metadata', {}).get('html_report_path')
        json_path = results.get('analysis_metadata', {}).get('json_evidence_path')
        if html_path:
            print(f"\nReports Generated:")
            print(f"  HTML Report: {html_path}")
            if json_path:
                print(f"  JSON Evidence Pack: {json_path}")
        
        # Metadata
        metadata = results['analysis_metadata']
        print(f"\nAnalysis Metadata:")
        print(f"  Timestamp: {metadata['timestamp']}")
        print(f"  Analysis Scope: {metadata.get('analysis_scope', 'phase1')}")
        print(f"  Total Market Cells: {metadata['total_market_cells']}")
        print(f"  Total Competitors: {metadata['total_competitors_found']}")
        print(f"  Output Directory: {metadata['output_directory']}")
        
        # Database stats
        stats = self.db.get_database_stats()
        print(f"\nDatabase Statistics:")
        for table, count in stats.items():
            if count > 0:
                print(f"  {table}: {count} records")
    
    def run_interactive(self):
        """Run interactive CLI session"""
        print("Competitive Analysis CLI")
        print("=" * 40)
        
        try:
            # File upload like PD2
            paths = self.select_source_files()
            document_content = self.load_documents_to_text(paths)
            if not document_content:
                print("No readable content from selected files. Exiting.")
                return
            max_competitors = self.prompt_max_competitors()
            analysis_scope = self.prompt_analysis_scope()
            
            # Run analysis
            results = self.analyze_company(
                company_name=None,
                document_content=document_content,
                max_competitors=max_competitors,
                analysis_scope=analysis_scope
            )
            
            # Display results
            if results:
                self.display_results_summary(results)
                print(f"\nFull results saved to: {self.output_dir}")
            else:
                print("\nAnalysis failed. Check the output above for errors.")
                
        except KeyboardInterrupt:
            print("\n\nAnalysis interrupted by user. Goodbye!")
        except Exception as e:
            print(f"\nUnexpected error: {e}")
            import traceback
            traceback.print_exc()


def main():
    """Main entry point for CLI"""
    # Check for required environment variables
    if not os.environ.get("GEMINI_API_KEY"):
        print("Error: GEMINI_API_KEY environment variable not set.")
        print("Please set your Gemini API key in .env file or environment.")
        sys.exit(1)
    
    # Initialize and run CLI
    cli = CompetitiveAnalysisCLI()
    cli.run_interactive()


if __name__ == "__main__":
    main()
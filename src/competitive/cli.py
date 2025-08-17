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
from src.competitive.market_mapper import MarketMapper
from src.competitive.peer_discovery import PeerDiscovery
from src.competitive.metric_engine import MetricEngine

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
        self.market_mapper = MarketMapper(self.db)
        self.peer_discovery = PeerDiscovery(self.db)
        self.metric_engine = MetricEngine(self.db)
        
        thread_safe_print(f"Competitive Analysis CLI initialized")
        thread_safe_print(f"Output directory: {self.output_dir}")
        thread_safe_print(f"Database: {db_path}")
    
    def prompt_company_name(self) -> str:
        """Prompt user for company name"""
        while True:
            company_name = input("\nEnter company name for competitive analysis: ").strip()
            if company_name:
                return company_name
            print("Please enter a valid company name.")
    
    def prompt_document_input(self) -> str:
        """Prompt user for document input method and content"""
        print("\nHow would you like to provide company information?")
        print("1. Type/paste text directly")
        print("2. Load from file")
        print("3. Use company name only (minimal analysis)")
        
        while True:
            choice = input("Choose option (1-3): ").strip()
            
            if choice == "1":
                print("\nPaste company information (press Ctrl+D or Ctrl+Z when done):")
                lines = []
                try:
                    while True:
                        line = input()
                        lines.append(line)
                except EOFError:
                    pass
                
                content = "\n".join(lines).strip()
                if content:
                    return content
                else:
                    print("No content provided. Please try again.")
                    continue
                    
            elif choice == "2":
                file_path = input("Enter file path: ").strip()
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        content = f.read().strip()
                    if content:
                        print(f"Loaded {len(content)} characters from {file_path}")
                        return content
                    else:
                        print("File is empty. Please try again.")
                        continue
                except Exception as e:
                    print(f"Error reading file: {e}. Please try again.")
                    continue
                    
            elif choice == "3":
                return "Minimal analysis mode - using company name only."
                
            else:
                print("Invalid choice. Please enter 1, 2, or 3.")
    
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
        print("1. Phase 1 only: Company analysis + competitor discovery")
        print("2. Phase 2 full: Phase 1 + metric collection + data normalization")
        print("Recommended: Phase 2 full (default)")
        
        while True:
            choice = input("Choose option (1-2, or press Enter for Phase 2): ").strip()
            
            if not choice or choice == "2":
                return "phase2"
            elif choice == "1":
                return "phase1"
            else:
                print("Invalid choice. Please enter 1 or 2.")
    
    def analyze_company(self, company_name: str, document_content: str, 
                       max_competitors: int = 3, analysis_scope: str = "phase2") -> Optional[dict]:
        """
        Run complete competitive analysis for a company.
        Returns analysis results or None if failed.
        """
        try:
            thread_safe_print(f"\n🚀 Starting competitive analysis for: {company_name}")
            
            # Phase 1: Market Cell Discovery
            thread_safe_print("\n📊 Phase 1: Company Analysis & Market Cell Discovery")
            company_context, market_cells = self.market_mapper.analyze_company_from_documents(
                document_content=document_content,
                company_name=company_name
            )
            
            if not market_cells:
                thread_safe_print("❌ No market cells discovered. Analysis cannot continue.")
                return None
            
            # Get company ID from database
            company_row = self.db.get_company_by_name(company_context['company_name'])
            if not company_row:
                thread_safe_print("❌ Company not found in database after insertion.")
                return None
            
            company_id = company_row['id']
            
            # Phase 2: Peer Discovery
            thread_safe_print(f"\n🔍 Phase 2: Peer Discovery for {len(market_cells)} market cells")
            all_competitors = self.peer_discovery.discover_all_peers(
                company_id=company_id,
                max_competitors_per_cell=max_competitors
            )
            
            # Phase 2: Metric Collection & Normalization (if requested)
            metrics_summary = None
            if analysis_scope == "phase2":
                thread_safe_print(f"\n📊 Phase 2: Metric Collection & Data Normalization")
                metrics_results = self.metric_engine.process_and_save_all_metrics(company_id)
                
                if metrics_results.get('success'):
                    metrics_summary = metrics_results['summary']
                    thread_safe_print(f"✅ Phase 2 completed successfully!")
                else:
                    thread_safe_print(f"⚠ Phase 2 completed with limited data: {metrics_results.get('error', 'Unknown error')}")
                    metrics_summary = {"error": metrics_results.get('error')}
            
            # Compile results
            results = {
                'company_context': company_context,
                'market_cells': market_cells,
                'competitors_by_market_cell': all_competitors,
                'metrics_summary': metrics_summary,
                'analysis_metadata': {
                    'timestamp': self.timestamp,
                    'company_id': company_id,
                    'analysis_scope': analysis_scope,
                    'total_market_cells': len(market_cells),
                    'total_competitors_found': sum(len(comps) for comps in all_competitors.values()),
                    'output_directory': str(self.output_dir)
                }
            }
            
            # Save results to JSON
            results_file = self.output_dir / "analysis_results.json"
            with open(results_file, 'w', encoding='utf-8') as f:
                json.dump(results, f, indent=2, default=str)
            
            thread_safe_print(f"\n✅ Analysis complete! Results saved to: {results_file}")
            return results
            
        except Exception as e:
            thread_safe_print(f"❌ Analysis failed: {e}")
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
        
        # Market cells
        market_cells = results['market_cells']
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
        print("🔍 Competitive Analysis CLI")
        print("=" * 40)
        
        try:
            # Get user inputs
            company_name = self.prompt_company_name()
            document_content = self.prompt_document_input()
            max_competitors = self.prompt_max_competitors()
            analysis_scope = self.prompt_analysis_scope()
            
            # Run analysis
            results = self.analyze_company(
                company_name=company_name,
                document_content=document_content,
                max_competitors=max_competitors,
                analysis_scope=analysis_scope
            )
            
            # Display results
            if results:
                self.display_results_summary(results)
                print(f"\n📁 Full results saved to: {self.output_dir}")
            else:
                print("\n❌ Analysis failed. Check the output above for errors.")
                
        except KeyboardInterrupt:
            print("\n\n👋 Analysis interrupted by user. Goodbye!")
        except Exception as e:
            print(f"\n❌ Unexpected error: {e}")
            import traceback
            traceback.print_exc()


def main():
    """Main entry point for CLI"""
    # Check for required environment variables
    if not os.environ.get("GEMINI_API_KEY"):
        print("❌ Error: GEMINI_API_KEY environment variable not set.")
        print("Please set your Gemini API key in .env file or environment.")
        sys.exit(1)
    
    # Initialize and run CLI
    cli = CompetitiveAnalysisCLI()
    cli.run_interactive()


if __name__ == "__main__":
    main()
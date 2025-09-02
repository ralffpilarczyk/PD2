#!/usr/bin/env python3
"""
Minimal controller - loads only extraction and causal rules
No dimensions, polarity, blocking rules, or expected patterns
"""

import subprocess
import tempfile
import sys
import yaml
from pathlib import Path
from generic_extractor import GenericExtractor

class GenericController:
    def __init__(self, 
                 extraction_config: str = None,
                 causal_config: str = None):
        """Initialize with minimal configuration files"""
        
        # Default to config directory
        config_dir = Path(__file__).parent / "config"
        
        if extraction_config is None:
            extraction_config = config_dir / "extraction_rules.yaml"
        if causal_config is None:
            causal_config = config_dir / "causal_rules.yaml"
            
        self.extractor = GenericExtractor(extraction_config)
        self.engine_path = Path(__file__).parent / "generic_reasoner.pl"
        
        # Load causal rules configuration
        with open(causal_config, 'r') as f:
            self.causal_config = yaml.safe_load(f)
    
    def analyze_document(self, md_file: Path, entity: str = "target_entity"):
        """Minimal pipeline with gate-only derivation"""
        
        print(f"\n{'='*60}")
        print(f"MINIMAL REASONING ENGINE")
        print(f"{'='*60}")
        
        # Step 1: Extract facts
        print(f"\n1. Extracting facts from {md_file.name}...")
        
        with open(md_file, 'r') as f:
            content = f.read()
        
        facts = self.extractor.extract(content, entity)
        
        print(f"   Found {len(facts)} facts")
        
        if not facts:
            print("   No facts found. Exiting.")
            return
        
        # Skip complex analysis for large documents
        if len(facts) > 15:  # Increased threshold after fixing circular rule
            print(f"\n2. Document too complex ({len(facts)} facts) - showing summary only")
            print("\nObserved facts:")
            for fact in sorted(facts, key=lambda x: x['property']):
                print(f"  - {fact['property']} (conf: {fact.get('confidence', 0.8):.2f})")
            print("\n--- Summary ---")
            print(f"Observed: {len(facts)}")
            print("(Full reasoning skipped for performance)")
            return
        
        # Step 2: Create Prolog analysis
        print(f"\n2. Running reasoning engine...")
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as f:
            f.write(f"% Analysis of {md_file.name}\n")
            f.write(":- consult('generic_reasoner.pl').\n\n")
            f.write("run :-\n")
            f.write("    reset_analysis,\n")
            
            # Load causal rules (minimal form)
            for rule in self.causal_config.get('causal_rules', []):
                f.write(f"    load_rule('{rule['antecedent']}', "
                       f"'{rule['consequent']}', {rule['confidence']}),\n")
            
            # Assert observed facts in canonical form
            for fact in facts:
                f.write(f"    assertz(fact('{entity}', '{fact['property']}', "
                       f"observed, {fact.get('confidence', 0.8)})),\n")
            
            # Pass only essential settings
            min_conf = self.causal_config.get('settings', {}).get('min_confidence_to_derive', 0.5)
            max_depth = self.causal_config.get('settings', {}).get('max_depth', 5)
            
            f.write(f"    set_config(min_confidence({min_conf})),\n")
            f.write(f"    set_config(max_depth({max_depth})),\n")
            f.write(f"    analyze('{entity}').\n")
            
            temp_file = f.name
        
        # Step 3: Run reasoning
        result = subprocess.run(
            # Ensure Prolog runs our goal and always halts
            ['swipl', '-q', '-s', temp_file, '-g', 'run', '-t', 'halt'],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent,
            timeout=60  # 60 seconds for real documents
        )
        
        # Display results
        if result.stdout:
            print(result.stdout)
        if result.stderr:
            # Filter out singleton warnings for cleaner output
            errors = [line for line in result.stderr.split('\n') 
                     if 'Singleton' not in line and line.strip()]
            if errors:
                print(f"Errors: {' '.join(errors)}")
        
        # Cleanup
        Path(temp_file).unlink()
        
        print(f"\n{'='*60}")
        print("Analysis Complete")
        print(f"{'='*60}")

def main():
    """Test the minimal framework"""
    
    # Check if a file was provided
    if len(sys.argv) > 1:
        # Handle file paths with spaces
        if len(sys.argv) > 2 and not sys.argv[2].startswith('-'):
            # Likely a path with spaces that got split
            print("Error: File path appears to contain spaces.")
            print("Please quote the path:")
            print(f'  python {sys.argv[0]} "path with spaces.md"')
            sys.exit(1)
            
        test_file = Path(sys.argv[1])
        if not test_file.exists():
            test_file = Path(__file__).parent / "test_docs" / sys.argv[1]
        
        if test_file.exists():
            entity = sys.argv[2] if len(sys.argv) > 2 else "company"
            controller = GenericController()
            controller.analyze_document(test_file, entity)
            return
        else:
            print(f"Error: File '{sys.argv[1]}' not found")
            sys.exit(1)
    
    # Default test with simple properties
    test_doc = Path(__file__).parent / "test_docs" / "test_minimal.md"
    test_doc.parent.mkdir(exist_ok=True)
    
    with open(test_doc, 'w') as f:
        f.write("""# Minimal Test Company

## Market Position
We are the market leader with strong brand recognition.

## Challenges
However, regulation prevents us from raising prices.

## Technology
We have achieved technological supremacy in our field.
""")
    
    print("\nTesting minimal reasoning engine...")
    controller = GenericController()
    controller.analyze_document(test_doc, "test_co")

if __name__ == "__main__":
    main()
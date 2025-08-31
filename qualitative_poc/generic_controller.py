#!/usr/bin/env python3
"""
Generic controller using configuration-driven components
No hardcoded logic - everything from config
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
                 causal_config: str = None,
                 conflict_config: str = None):
        """Initialize with configuration files"""
        
        # Default to config directory
        config_dir = Path(__file__).parent / "config"
        
        if extraction_config is None:
            extraction_config = config_dir / "extraction_rules.yaml"
        if causal_config is None:
            causal_config = config_dir / "causal_rules.yaml"
        if conflict_config is None:
            conflict_config = config_dir / "conflict_resolution.yaml"
            
        self.extractor = GenericExtractor(extraction_config)
        self.engine_path = Path(__file__).parent / "generic_reasoner.pl"
        
        # Load causal rules configuration
        with open(causal_config, 'r') as f:
            self.causal_config = yaml.safe_load(f)
            
        # Load conflict resolution configuration
        try:
            with open(conflict_config, 'r') as f:
                self.conflict_config = yaml.safe_load(f)
        except:
            self.conflict_config = None
    
    def analyze_document(self, md_file: Path, entity: str = "target_entity", show_rationale: bool = True):
        """Complete pipeline using generic components"""
        
        print(f"\n{'='*60}")
        print(f"GENERIC QUALITATIVE REASONING FRAMEWORK")
        print(f"{'='*60}")
        
        # Step 1: Extract facts using generic extractor
        print(f"\n1. Extracting facts from {md_file.name}...")
        
        with open(md_file, 'r') as f:
            content = f.read()
        
        facts = self.extractor.extract(content, entity)
        
        print(f"   Found {len(facts)} facts:")
        for fact in facts:
            dim = fact.get('dimension', 'unknown')
            pol = fact.get('polarity', 'neutral')
            print(f"   - {fact['property']} ({fact['type']}, dim:{dim}/{pol})")
        
        if not facts:
            print("   No facts found. Exiting.")
            return
        
        # Step 2: Create Prolog analysis
        print(f"\n2. Preparing reasoning engine...")
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as f:
            f.write(f"% Analysis of {md_file.name}\n")
            f.write(":- consult('generic_reasoner.pl').\n\n")
            f.write("run :-\n")
            f.write("    reset_analysis,\n")
            
            # Load causal rules from config with all fields
            for rule in self.causal_config['causal_rules']:
                # Include blocking conditions if present
                blockers = rule.get('unless_blocked_by', [])
                blockers_str = str(blockers).replace("'", '"')
                rationale = rule.get('rationale', 'No rationale provided')
                
                f.write(f"    load_causal_rule_full('{rule['antecedent']}', "
                       f"'{rule['consequent']}', {rule['confidence']}, "
                       f"{blockers_str}, '{rationale}'),\n")
            
            # Load expected patterns from config
            for pattern in self.causal_config.get('expected_patterns', []):
                f.write(f"    load_expected_pattern('{pattern['property1']}', "
                       f"'{pattern['property2']}', {pattern['confidence']}, "
                       f"'{pattern['absence_implies']}'),\n")
            
            # Load blocking rules from config
            for blocker in self.causal_config.get('blocking_rules', []):
                blocks_str = str(blocker['blocks']).replace("'", '"')
                f.write(f"    load_blocking_rule('{blocker['blocker']}', "
                       f"{blocks_str}, '{blocker['strength']}', "
                       f"'{blocker.get('rationale', 'No rationale')}'),\n")
            
            # Load negation pairs and dimensions from extractor config
            for prop_name, prop_config in self.extractor.properties.items():
                if 'negates' in prop_config:
                    f.write(f"    load_negation_pair('{prop_name}', '{prop_config['negates']}'),\n")
                if 'negation' in prop_config:
                    f.write(f"    load_negation_pair('{prop_name}', '{prop_config['negation']}'),\n")
                # Load dimension metadata
                if 'dimension' in prop_config and 'polarity' in prop_config:
                    dim = prop_config['dimension']
                    pol = prop_config['polarity']
                    f.write(f"    assertz(property_dimension('{prop_name}', {dim}, {pol})),\n")
            
            # Load property metadata for derived facts from causal config
            if 'property_metadata' in self.causal_config:
                for prop_name, metadata in self.causal_config['property_metadata'].items():
                    if 'dimension' in metadata and 'polarity' in metadata:
                        dim = metadata['dimension']
                        pol = metadata['polarity']
                        f.write(f"    assertz(property_dimension('{prop_name}', {dim}, {pol})),\n")
            
            # Assert facts with confidence and priority
            for fact in facts:
                f.write(f"    assertz(has_property_full('{entity}', '{fact['property']}', "
                       f"{fact.get('confidence', 0.8)}, {fact.get('priority', 1)})),\n")
                # Also assert simple form for backward compatibility
                f.write(f"    assertz(has_property('{entity}', '{fact['property']}')),\n")
            
            # Pass configuration settings including confidence decay
            show_rationale_flag = 'true' if show_rationale else 'false'
            min_conf = self.causal_config['settings'].get('min_confidence_to_derive', 0.5)
            cascade = self.causal_config['settings'].get('cascade_blocking', True)
            cascade_flag = 'true' if cascade else 'false'
            decay = self.causal_config['settings'].get('confidence_decay_per_hop', 0.9)
            
            f.write(f"    set_config(min_confidence({min_conf})),\n")
            f.write(f"    set_config(cascade_blocking({cascade_flag})),\n")
            f.write(f"    set_config(show_rationale({show_rationale_flag})),\n")
            f.write(f"    set_config(confidence_decay({decay})),\n")
            f.write(f"    analyze_with_config('{entity}').\n")
            f.write("\n:- run, halt.\n")
            
            temp_file = f.name
        
        # Step 3: Run reasoning
        print(f"\n3. Running generic reasoning...")
        
        result = subprocess.run(
            ['swipl', '-q', '-s', temp_file],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent
        )
        
        # Display results
        print("\n4. Results:")
        print("-" * 40)
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
    """Test the generic framework"""
    
    # Check if a file was provided
    if len(sys.argv) > 1:
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
    
    # Default test with novel properties
    test_doc = Path(__file__).parent / "test_docs" / "test_novel.md"
    test_doc.parent.mkdir(exist_ok=True)
    
    with open(test_doc, 'w') as f:
        f.write("""# Novel Company Analysis

## Technology Position
We have first-mover advantage in quantum computing. Our technological supremacy 
is evident in recent benchmarks. We've achieved quantum supremacy in several 
key algorithms.

## Operational Challenges
Despite regulatory barriers, we maintain our technological edge. However, we 
lack economies of scale due to the nascent nature of the quantum market.

## Market Position
While we are not yet the market leader, our first-mover advantage positions 
us well for future dominance once the market matures.
""")
    
    print("\nTesting with NOVEL properties not in original hardcoded system...")
    controller = GenericController()
    controller.analyze_document(test_doc, "quantum_co")

if __name__ == "__main__":
    main()
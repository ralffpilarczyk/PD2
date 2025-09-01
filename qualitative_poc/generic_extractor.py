#!/usr/bin/env python3
"""
Minimal fact extractor - pattern matching only
No dimensions, polarity, or semantic conflict detection
"""

import re
import yaml
from pathlib import Path
from typing import List, Dict

class GenericExtractor:
    def __init__(self, config_path: str = None):
        """Load extraction rules only"""
        if config_path is None:
            config_path = Path(__file__).parent / "config" / "extraction_rules.yaml"
        
        with open(config_path, 'r') as f:
            config = yaml.safe_load(f)
        
        self.properties = config['properties']
        self.settings = config.get('settings', {})
        
        # Pre-compile patterns for performance
        self._compile_patterns()
    
    def _compile_patterns(self):
        """Pre-compile regex patterns"""
        self.compiled_patterns = {}
        
        for prop_name, prop_config in self.properties.items():
            self.compiled_patterns[prop_name] = []
            patterns = prop_config.get('patterns', [])
            
            for pattern in patterns:
                try:
                    # Simple case-insensitive matching
                    compiled = re.compile(pattern, re.IGNORECASE)
                    self.compiled_patterns[prop_name].append(compiled)
                except re.error as e:
                    print(f"Warning: Invalid regex '{pattern}' for '{prop_name}': {e}")
    
    def extract(self, text: str, entity: str = "target") -> List[Dict]:
        """Extract facts using pattern matching only"""
        
        facts = []
        properties_found = set()  # Track which properties we've already found
        
        # Find matches for each property
        for prop_name, patterns in self.compiled_patterns.items():
            if prop_name in properties_found:
                continue  # Skip if already found
                
            prop_config = self.properties[prop_name]
            best_confidence = 0
            found_match = False
            
            # Check all patterns for this property
            for pattern in patterns:
                if pattern.search(text):
                    found_match = True
                    # Use property's configured confidence
                    confidence = prop_config.get('confidence', 0.8)
                    best_confidence = max(best_confidence, confidence)
                    break  # Found a match, no need to check other patterns
            
            if found_match:
                # Check if this property negates another
                if 'negates' in prop_config:
                    # This is a negative property (e.g., regulation negates pricing_power)
                    # Emit the negated property as negative
                    negated_prop = prop_config['negates']
                    if negated_prop not in properties_found:
                        facts.append({
                            'entity': entity,
                            'property': f"no_{negated_prop}" if not negated_prop.startswith('no_') else negated_prop,
                            'type': 'negative',
                            'confidence': best_confidence,
                            'source': 'observed'
                        })
                        properties_found.add(negated_prop)
                        properties_found.add(f"no_{negated_prop}")
                else:
                    # Regular property
                    facts.append({
                        'entity': entity,
                        'property': prop_name,
                        'type': prop_config.get('type', 'positive'),
                        'confidence': best_confidence,
                        'source': 'observed'
                    })
                    properties_found.add(prop_name)
        
        # Apply minimum confidence threshold
        min_conf = self.settings.get('min_confidence_to_assert', 0.3)
        facts = [f for f in facts if f['confidence'] >= min_conf]
        
        # Sort by property name for deterministic output
        facts.sort(key=lambda x: x['property'])
        
        return facts

# Test if run directly
if __name__ == "__main__":
    extractor = GenericExtractor()
    
    # Test minimal extraction
    test_texts = [
        "We are the market leader with strong brand recognition.",
        "Regulation prevents us from raising prices.",
        "We have technological supremacy and network effects.",
        "Despite being market leader, we have no pricing power due to regulation."
    ]
    
    for i, text in enumerate(test_texts, 1):
        print(f"\nTest {i}: {text}")
        facts = extractor.extract(text)
        for fact in facts:
            print(f"  - {fact['property']} (type: {fact['type']}, conf: {fact['confidence']:.2f})")
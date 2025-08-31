#!/usr/bin/env python3
"""
Generic configuration-driven fact extractor
Fully authoritative from YAML configuration
"""

import re
import yaml
from pathlib import Path
from typing import List, Dict, Set, Tuple
from collections import defaultdict

class GenericExtractor:
    def __init__(self, config_path: str = None, dimensions_path: str = None):
        """Load extraction rules and semantic dimensions"""
        if config_path is None:
            config_path = Path(__file__).parent / "config" / "extraction_rules.yaml"
        if dimensions_path is None:
            dimensions_path = Path(__file__).parent / "config" / "semantic_dimensions.yaml"
        
        with open(config_path, 'r') as f:
            config = yaml.safe_load(f)
        
        with open(dimensions_path, 'r') as f:
            self.dimensions_config = yaml.safe_load(f)
        
        self.properties = config['properties']
        self.settings = config.get('settings', {})
        self.dimensions = self.dimensions_config.get('dimensions', {})
        self.contradiction_patterns = self.dimensions_config.get('contradiction_patterns', [])
        self.resolution_principles = self.dimensions_config.get('resolution_principles', {})
        
        # Pre-compile patterns for performance
        self._compile_patterns()
        
        # Build contradiction detection maps
        self._build_dimension_maps()
        self._build_negation_map()  # Keep for backward compatibility
    
    def _compile_patterns(self):
        """Pre-compile regex patterns with configured settings"""
        self.compiled_patterns = {}
        
        for prop_name, prop_config in self.properties.items():
            self.compiled_patterns[prop_name] = []
            patterns = prop_config.get('patterns', [])
            
            # Limit patterns per property for performance
            max_patterns = self.settings.get('max_patterns_per_property', 10)
            patterns = patterns[:max_patterns]
            
            for pattern in patterns:
                try:
                    # Add word boundaries if configured
                    if self.settings.get('word_boundaries', True):
                        # Check if boundaries already present
                        if not pattern.startswith(r'\b') and not pattern.startswith('^'):
                            pattern = r'\b' + pattern
                        if not pattern.endswith(r'\b') and not pattern.endswith('$'):
                            pattern = pattern + r'\b'
                    
                    # Set flags based on configuration
                    flags = re.IGNORECASE if not self.settings.get('case_sensitive', False) else 0
                    
                    compiled = re.compile(pattern, flags)
                    self.compiled_patterns[prop_name].append(compiled)
                    
                except re.error as e:
                    print(f"Warning: Invalid regex '{pattern}' for '{prop_name}': {e}")
    
    def _build_dimension_maps(self):
        """Build maps for dimension-based contradiction detection"""
        self.dimension_map = {}  # property -> dimension
        self.polarity_map = {}   # property -> polarity
        self.same_dimension = defaultdict(list)  # dimension -> [properties]
        
        for prop_name, prop_config in self.properties.items():
            if 'dimension' in prop_config:
                dim = prop_config['dimension']
                pol = prop_config.get('polarity', 'neutral')
                
                self.dimension_map[prop_name] = dim
                self.polarity_map[prop_name] = pol
                self.same_dimension[dim].append((prop_name, pol))
    
    def _build_negation_map(self):
        """Build negation map from dimensions and legacy config"""
        self.negation_map = {}
        
        # First, add dimension-based contradictions
        for dim, props in self.same_dimension.items():
            positives = [p for p, pol in props if pol == 'positive']
            negatives = [p for p, pol in props if pol == 'negative']
            
            # Each positive contradicts each negative in same dimension
            for pos in positives:
                for neg in negatives:
                    self.negation_map[pos] = neg
                    self.negation_map[neg] = pos
        
        # Then add any legacy configured negations (for backward compatibility)
        for prop_name, prop_config in self.properties.items():
            if 'negates' in prop_config:
                self.negation_map[prop_name] = prop_config['negates']
                self.negation_map[prop_config['negates']] = prop_name
    
    def extract(self, text: str, entity: str = "target") -> List[Dict]:
        """Extract facts using fully configured rules"""
        
        # Track all matches with metadata
        all_matches = defaultdict(list)
        
        # Find all pattern matches
        for prop_name, patterns in self.compiled_patterns.items():
            prop_config = self.properties[prop_name]
            
            for pattern in patterns:
                for match in pattern.finditer(text):
                    # Extract context window
                    context_window = prop_config.get('context_window', 
                                                     self.settings.get('context_window_default', 30))
                    
                    start = max(0, match.start() - context_window)
                    end = min(len(text), match.end() + context_window)
                    context = text[start:end]
                    
                    # Check for negation in context
                    is_negated = self._check_negation_in_context(
                        context, 
                        match.group(),
                        prop_config
                    )
                    
                    # Calculate confidence
                    confidence = self._calculate_confidence(
                        prop_config,
                        match,
                        is_negated
                    )
                    
                    all_matches[prop_name].append({
                        'position': match.start(),
                        'match_text': match.group(),
                        'context': context,
                        'is_negated': is_negated,
                        'confidence': confidence,
                        'priority': prop_config.get('priority', 1),
                        'config': prop_config
                    })
        
        # Apply suppression rules
        suppressed = self._apply_suppression(all_matches)
        
        # Resolve conflicts and create facts
        facts = self._create_facts(all_matches, suppressed, entity)
        
        # Apply blocking rules from constraints
        facts = self._apply_blocking_rules(facts)
        
        # Final conflict resolution
        facts = self._resolve_final_conflicts(facts)
        
        return facts
    
    def _check_negation_in_context(self, context: str, match_text: str, prop_config: Dict) -> bool:
        """Check if match is negated in its context"""
        
        # Get negation prefixes from settings
        neg_prefixes = self.settings.get('negation_prefixes', 
                                         ['no ', 'not ', 'lack of ', 'absence of ',
                                          'without ', 'unable to ', 'cannot ', 'prevent'])
        
        context_lower = context.lower()
        match_lower = match_text.lower()
        
        # Find match position in context
        match_pos = context_lower.find(match_lower)
        if match_pos == -1:
            return False
        
        # Check for negation before match
        prefix = context_lower[:match_pos]
        
        for neg in neg_prefixes:
            if neg in prefix:
                # Check distance - negation should be close
                neg_pos = prefix.rfind(neg)
                distance = match_pos - neg_pos - len(neg)
                
                # Negation within reasonable distance
                if distance < 20:
                    return True
        
        return False
    
    def _calculate_confidence(self, prop_config: Dict, match, is_negated: bool) -> float:
        """Calculate confidence score for a match"""
        
        base_confidence = prop_config.get('confidence', 0.8)
        
        if is_negated:
            # Direct negations often have higher confidence
            return min(1.0, base_confidence * 1.1)
        
        return base_confidence
    
    def _apply_suppression(self, all_matches: Dict) -> Set[str]:
        """Apply suppression rules - some properties suppress others"""
        
        suppressed = set()
        
        for prop_name, matches in all_matches.items():
            if not matches:
                continue
                
            prop_config = self.properties[prop_name]
            
            # Check if this property suppresses its negation
            if prop_config.get('suppresses_negation', False):
                negation = prop_config.get('negation')
                if negation:
                    # Check suppression window overlap
                    suppression_window = self.settings.get('suppression_window', 50)
                    
                    for match in matches:
                        # Suppress negation matches near this match
                        if negation in all_matches:
                            for neg_match in all_matches[negation]:
                                distance = abs(match['position'] - neg_match['position'])
                                if distance < suppression_window:
                                    suppressed.add(negation)
        
        return suppressed
    
    def _create_facts(self, all_matches: Dict, suppressed: Set[str], entity: str) -> List[Dict]:
        """Create facts from matches, handling suppression and conflicts"""
        
        facts = []
        
        for prop_name, matches in all_matches.items():
            # Skip suppressed properties
            if prop_name in suppressed:
                continue
            
            if not matches:
                continue
            
            prop_config = self.properties[prop_name]
            
            # Select best match (highest priority, then confidence)
            best_match = max(matches, key=lambda x: (x['priority'], x['confidence']))
            
            # Check minimum confidence threshold
            min_conf = self.settings.get('min_confidence_to_assert', 0.5)
            if best_match['confidence'] < min_conf:
                continue
            
            # Handle negated matches
            if best_match['is_negated']:
                # Find what this negates
                negated_prop = prop_config.get('negates')
                if negated_prop:
                    facts.append({
                        'entity': entity,
                        'property': negated_prop,
                        'type': 'negative',
                        'category': prop_config.get('category', 'general'),
                        'confidence': best_match['confidence'],
                        'priority': best_match['priority'] + 1,  # Negatives higher priority
                        'source': 'negation_detected'
                    })
            else:
                # Regular fact
                fact = {
                    'entity': entity,
                    'property': prop_name,
                    'type': prop_config.get('type', 'neutral'),
                    'category': prop_config.get('category', 'general'),
                    'dimension': prop_config.get('dimension', 'unknown'),
                    'polarity': prop_config.get('polarity', 'neutral'),
                    'confidence': best_match['confidence'],
                    'priority': best_match['priority'],
                    'source': 'pattern_match'
                }
                
                # Add blocking information if constraint type
                if prop_config.get('type') == 'constraint':
                    fact['blocks'] = prop_config.get('blocks', [])
                
                facts.append(fact)
        
        return facts
    
    def _apply_blocking_rules(self, facts: List[Dict]) -> List[Dict]:
        """Apply blocking rules from constraint-type facts"""
        
        # Find all constraint facts
        constraints = [f for f in facts if f.get('type') == 'constraint']
        
        # Collect all blocked properties
        blocked_props = set()
        for constraint in constraints:
            blocked_props.update(constraint.get('blocks', []))
        
        # Filter out blocked facts
        filtered = []
        for fact in facts:
            if fact['property'] not in blocked_props:
                filtered.append(fact)
        
        return filtered
    
    def _resolve_final_conflicts(self, facts: List[Dict]) -> List[Dict]:
        """Final conflict resolution using configured strategy"""
        
        strategy = self.settings.get('resolution_strategy', 'priority_based')
        reality_beats_theory = self.settings.get('reality_beats_theory', True)
        
        # Group facts by entity-property
        fact_groups = defaultdict(list)
        for fact in facts:
            key = (fact['entity'], fact['property'])
            fact_groups[key].append(fact)
        
        # Also check for negation conflicts
        resolved = {}
        
        for fact in facts:
            prop = fact['property']
            entity = fact['entity']
            key = (entity, prop)
            
            # Check for direct negation conflict
            if prop in self.negation_map:
                neg_prop = self.negation_map[prop]
                neg_key = (entity, neg_prop)
                
                if neg_key in resolved:
                    # Conflict! Apply resolution strategy
                    existing = resolved[neg_key]
                    
                    if self._should_override(fact, existing, strategy, reality_beats_theory):
                        # Remove negation, add this
                        del resolved[neg_key]
                        resolved[key] = fact
                    # else keep existing, skip this
                    continue
            
            # No conflict or won conflict
            if key not in resolved:
                resolved[key] = fact
            else:
                # Same property multiple times - keep best
                existing = resolved[key]
                if self._should_override(fact, existing, strategy, reality_beats_theory):
                    resolved[key] = fact
        
        return list(resolved.values())
    
    def _should_override(self, new_fact: Dict, existing: Dict, 
                        strategy: str, reality_beats_theory: bool) -> bool:
        """Determine if new fact should override existing"""
        
        # Reality beats theory - negative/constraint types win
        if reality_beats_theory:
            if new_fact.get('type') in ['negative', 'constraint'] and \
               existing.get('type') == 'positive':
                return True
            if existing.get('type') in ['negative', 'constraint'] and \
               new_fact.get('type') == 'positive':
                return False
        
        # Apply configured strategy
        if strategy == 'priority_based':
            if new_fact.get('priority', 1) > existing.get('priority', 1):
                return True
            elif new_fact.get('priority', 1) == existing.get('priority', 1):
                return new_fact.get('confidence', 0) > existing.get('confidence', 0)
        else:  # confidence_based
            return new_fact.get('confidence', 0) > existing.get('confidence', 0)
        
        return False
    
    def get_contradictions(self, facts: List[Dict]) -> List[Dict]:
        """Find contradictions with resolution principles"""
        
        contradictions = []
        props_by_entity = defaultdict(list)
        
        # Group facts by entity
        for fact in facts:
            props_by_entity[fact['entity']].append(fact)
        
        # Check each entity's properties
        for entity, entity_facts in props_by_entity.items():
            # Check dimension-based contradictions
            for f1 in entity_facts:
                for f2 in entity_facts:
                    if f1['property'] != f2['property']:
                        if self._are_contradictory(f1['property'], f2['property']):
                            winner, reason = self._resolve_contradiction(f1, f2)
                            contradictions.append({
                                'entity': entity,
                                'fact1': f1,
                                'fact2': f2,
                                'winner': winner,
                                'reason': reason
                            })
        
        return contradictions
    
    def _are_contradictory(self, prop1: str, prop2: str) -> bool:
        """Check if two properties contradict via dimensions or patterns"""
        
        # Check dimension-based contradiction
        if prop1 in self.dimension_map and prop2 in self.dimension_map:
            if self.dimension_map[prop1] == self.dimension_map[prop2]:
                if self.polarity_map[prop1] != self.polarity_map[prop2]:
                    return True
        
        # Check pattern-based contradiction
        for pattern in self.contradiction_patterns:
            if self._matches_pattern(prop1, prop2, pattern):
                return True
        
        # Check legacy negation map
        if prop1 in self.negation_map and self.negation_map[prop1] == prop2:
            return True
        
        return False
    
    def _matches_pattern(self, prop1: str, prop2: str, pattern: Dict) -> bool:
        """Check if properties match a contradiction pattern"""
        import re
        
        if 'pattern_positive' in pattern and 'pattern_negative' in pattern:
            # Check both directions
            pos_pattern = pattern['pattern_positive']
            neg_pattern = pattern['pattern_negative']
            
            # Replace \1 with actual capture groups
            if re.match(pos_pattern, prop1) and re.match(neg_pattern, prop2):
                return True
            if re.match(pos_pattern, prop2) and re.match(neg_pattern, prop1):
                return True
        
        return False
    
    def _resolve_contradiction(self, fact1: Dict, fact2: Dict) -> Tuple[Dict, str]:
        """Resolve contradiction using configured principles"""
        
        # Apply resolution principles in order
        principles = sorted(self.resolution_principles.items(), 
                          key=lambda x: int(x[0].split('_')[0]))
        
        for principle_key, principle in principles:
            # Check observation vs derivation
            if 'observation_beats' in principle_key:
                if fact1.get('source') == 'observation' and fact2.get('source') != 'observation':
                    return fact1, principle['description']
                if fact2.get('source') == 'observation' and fact1.get('source') != 'observation':
                    return fact2, principle['description']
            
            # Check negative beats positive
            if 'negative_beats' in principle_key:
                if fact1.get('type') == 'negative' and fact2.get('type') == 'positive':
                    return fact1, principle['description']
                if fact2.get('type') == 'negative' and fact1.get('type') == 'positive':
                    return fact2, principle['description']
            
            # Check confidence
            if 'confidence' in principle_key:
                conf_diff = abs(fact1.get('confidence', 0) - fact2.get('confidence', 0))
                threshold = self.dimensions_config.get('confidence_settings', {}).get('confidence_difference_threshold', 0.1)
                if conf_diff > threshold:
                    if fact1.get('confidence', 0) > fact2.get('confidence', 0):
                        return fact1, principle['description']
                    else:
                        return fact2, principle['description']
        
        # Default: higher priority wins
        if fact1.get('priority', 0) > fact2.get('priority', 0):
            return fact1, 'Higher priority'
        else:
            return fact2, 'Higher priority'

# Test if run directly
if __name__ == "__main__":
    extractor = GenericExtractor()
    
    # Test comprehensive extraction
    test_texts = [
        "We are the market leader but regulation prevents us from raising prices.",
        "Despite being the market leader, we have no pricing power due to commoditization.",
        "We have strong network effects and high switching costs.",
        "We lack economies of scale but have first-mover advantage."
    ]
    
    for i, text in enumerate(test_texts, 1):
        print(f"\nTest {i}: {text}")
        facts = extractor.extract(text)
        for fact in facts:
            print(f"  - {fact['property']} ({fact['type']}, conf: {fact['confidence']:.2f})")
        
        # Check for contradictions
        contradictions = extractor.get_contradictions(facts)
        if contradictions:
            print("  Contradictions detected:")
            for c in contradictions:
                print(f"    - {c[0]} vs {c[1]}")
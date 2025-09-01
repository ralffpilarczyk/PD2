# Qualitative Reasoning POC - Comprehensive Overview

## Executive Summary

This is a **generic, configuration-driven qualitative reasoning system** that performs pattern-based inference without numerical calculations. The system extracts facts from text documents, applies causal rules to derive new insights, resolves contradictions using universal principles, and provides comprehensive reporting of its reasoning process.

### Core Philosophy
- **Zero hardcoding** - All domain knowledge in YAML configurations
- **Principle-based resolution** - Universal rules for conflict resolution
- **Semantic understanding** - Properties organized by dimensions and polarity
- **Complete transparency** - Full provenance and explanation for all decisions

## Architecture Overview

### System Components

```
qualitative_poc/
├── generic_controller.py      # Main orchestrator (Python)
├── generic_extractor.py       # Text extraction engine (Python)
├── generic_reasoner.pl        # Reasoning engine (Prolog)
├── config/
│   ├── extraction_rules.yaml  # Property extraction patterns
│   ├── causal_rules.yaml      # Causal relationships & metadata
│   ├── semantic_dimensions.yaml # Semantic categories
│   └── conflict_resolution.yaml # Resolution principles
└── test_docs/                  # Test documents
```

### Data Flow

1. **Text Input** → `generic_extractor.py` extracts facts using regex patterns
2. **Facts** → `generic_controller.py` creates Prolog program with facts and rules
3. **Prolog Program** → `generic_reasoner.pl` performs multi-level derivation
4. **Reasoning** → Conflict resolution, cascade blocking, contradiction detection
5. **Output** → Comprehensive report with derivations, blocks, and explanations

## Configuration System

### 1. Extraction Rules (`extraction_rules.yaml`)

Defines how to extract properties from text with semantic metadata:

```yaml
properties:
  market_leader:
    patterns:
      - "market leader"
      - "leading market position"
      - "dominant player"
    dimension: position
    polarity: positive
    strength: 0.9
    negates: weak_position
    priority: 1
```

**Key fields:**
- `patterns`: Regex patterns to match in text
- `dimension`: Semantic category (position, scale, financial, technology, etc.)
- `polarity`: positive or negative within dimension
- `strength`: Confidence when extracted (0-1)
- `negates`: Direct negation relationship
- `priority`: Higher priority facts override lower ones

### 2. Causal Rules (`causal_rules.yaml`)

Defines causal relationships and derived properties:

```yaml
causal_rules:
  - antecedent: market_leader
    consequent: pricing_power
    confidence: 0.8
    unless_blocked_by: [regulation, commoditization]
    rationale: "Market leaders typically have pricing power"
    category: market_dynamics

property_metadata:
  pricing_power:
    dimension: pricing
    polarity: positive
    category: pricing
```

**Key sections:**
- `causal_rules`: A→B relationships with confidence and blockers
- `property_metadata`: Dimension/polarity for derived properties
- `expected_patterns`: Co-occurrence expectations for anomaly detection
- `blocking_rules`: Facts that prevent certain derivations
- `reasoning_chains`: Multi-hop derivation paths
- `settings`: Global configuration (thresholds, decay rates)

### 3. Semantic Dimensions (`semantic_dimensions.yaml`)

Defines semantic categories for contradiction detection:

```yaml
dimensions:
  position:
    description: "Market position and competitive standing"
    properties:
      positive:
        - market_leader
        - dominant_position
      negative:
        - weak_position
        - struggling_position
```

Properties in the same dimension with opposite polarity are contradictory.

## Core Algorithms

### 1. Fact Extraction (`generic_extractor.py`)

```python
def extract(self, text, entity="company"):
    facts = []
    for prop_name, config in self.properties.items():
        if self._matches_patterns(text, config['patterns']):
            # Check for contradictions within extracted facts
            if not self._contradicts_existing(prop_name, facts):
                facts.append({
                    'property': prop_name,
                    'confidence': config.get('strength', 0.8),
                    'dimension': config.get('dimension'),
                    'polarity': config.get('polarity')
                })
    return self._resolve_contradictions(facts)
```

**Key features:**
- Pattern matching with regex
- Immediate contradiction detection
- Dimension-based conflict resolution
- Priority-based override

### 2. Causal Derivation (`generic_reasoner.pl`)

The Prolog engine performs iterative deepening with confidence decay:

```prolog
derive_all_depths_with_conf(Entity, Seen, Depth, AllDerived) :-
    % Get current facts
    findall(F, has_property(Entity, F), ObservedFacts),
    findall(F, member(derived(F, _, _), Seen), DerivedFacts),
    append(ObservedFacts, DerivedFacts, CurrentFacts),
    
    % Derive new facts with confidence decay
    findall(derived(P, because(Source), Conf),
            (member(Source, CurrentFacts),
             causal_rule(Source, P, RuleConf),
             \+ member(P, CurrentFacts),
             calculate_derived_confidence(RuleConf, Depth, Conf),
             Conf >= MinConfidence),
            NewDerived),
    
    % Continue if new derivations found
    (NewDerived = [] -> AllDerived = Seen
    ; append(Seen, NewDerived, NewSeen),
      NextDepth is Depth + 1,
      derive_all_depths_with_conf(Entity, NewSeen, NextDepth, AllDerived)).
```

**Confidence calculation:**
```
EffectiveConfidence = RuleConfidence × (DecayRate ^ Depth)
```
Default decay rate: 0.9 per hop

### 3. Conflict Resolution (`resolve_with_cascade`)

Three types of conflicts detected and resolved:

1. **Negation conflicts**: `pricing_power` vs `no_pricing_power`
2. **Dimension conflicts**: Properties in same dimension, opposite polarity
3. **Observed-derived conflicts**: Observed facts override derived ones

**Resolution principles:**
1. Negative beats positive (reality over theory)
2. Observed beats derived (evidence over inference)
3. Higher confidence beats lower
4. Shorter path beats longer

### 4. Cascade Blocking

When a fact is blocked, all downstream derivations are also blocked:

```prolog
find_cascade_blocks(Possible, BlockedFacts, AllBlocked) :-
    % Find facts that depend on blocked facts
    findall(P, 
            (member(derived(P, because(Source), _), Possible),
             member(Source, BlockedFacts)),
            DependentFacts),
    % Recursively find all downstream blocks
    ...
```

Example cascade:
- `pricing_power` blocked by `no_pricing_power`
- → `high_margins` cascade blocked (depends on pricing_power)
- → `strong_cash_flow` cascade blocked (depends on high_margins)
- → `investment_capacity` cascade blocked (depends on strong_cash_flow)

## Reporting System

### Output Sections

1. **Direct Observations**
   - Facts extracted from text with confidence and priority

2. **Derived Insights**
   - Facts inferred through causal rules
   - Shows source, confidence, and rationale

3. **Valid Reasoning Chains**
   - Multi-hop derivation paths that succeeded
   - Format: `A → B → C → D`

4. **Contradiction Clusters Encountered**
   - All conflicts detected during derivation
   - Shows both parties and conflict type
   - Count of total conflicts resolved

5. **Contradictions Detected (Post-Resolution)**
   - Any remaining contradictions after resolution
   - Should normally be empty if resolution worked

6. **Unusual Patterns**
   - Expected co-occurrences that are missing
   - Explains why (blocked, cascade blocked, not derived)

7. **Blocked Derivations**
   - Facts blocked with blocker identity and principle
   - Example: `pricing_power blocked by no_pricing_power (negative beats positive)`

8. **Cascade Blocked**
   - Facts blocked due to upstream blocks
   - Shows dependency chain

9. **Blocked Chains**
   - Complete derivation paths that were prevented
   - Useful for understanding what could have been

10. **Summary Statistics**
    - Fact counts: observed, derived, blocked, cascade blocked
    - Top blocking reasons with counts
    - Rule metrics: total, applicable, applied, blocked

11. **Active Configuration**
    - Current settings (thresholds, flags)
    - Resolution strategy
    - Confidence decay rate

## Key Improvements from Critique

### From Hardcoded to Generic
- **Before**: Properties like "market_leader" were hardcoded in Python
- **After**: Everything driven by YAML configurations

### Complete Cascade Blocking
- **Before**: Only first-level blocks
- **After**: Full N-level cascade with complete downstream tracking

### Proper Contradiction Detection
- **Before**: Only checked negation pairs
- **After**: Semantic dimensions with universal resolution principles

### Accurate Reporting
- **Before**: Inflated counts (702 cascade blocks), wrong blocker attribution
- **After**: Deduplicated counts, correct blocker tracking, clear provenance

### Early Rule Gating
- **Before**: Derive everything then block
- **After**: Apply confidence and blocker checks during derivation

## Usage Example

```python
from generic_controller import GenericController
from pathlib import Path

# Initialize with configurations
controller = GenericController(
    extraction_config="config/extraction_rules.yaml",
    causal_config="config/causal_rules.yaml",
    conflict_config="config/conflict_resolution.yaml"
)

# Analyze a document
doc = Path("test_docs/company_analysis.md")
controller.analyze_document(doc, entity="acme_corp", show_rationale=True)
```

## Testing Approach

### Test Document Structure
Create markdown documents with various scenarios:
- Contradictory facts (weak_position + technological_supremacy)
- Missing expected patterns
- Complex cascade scenarios
- Dimension conflicts

### Validation Points
1. Contradictions properly detected and resolved
2. Cascades block all downstream facts
3. Metrics are consistent and accurate
4. Unusual patterns explained with precise causes
5. No duplicate facts or inflated counts

## Design Principles (from CLAUDE.md)

1. **Principle-Based Approaches**: Make wise choices through patterns, not hardcoded rules
2. **Generic Solutions**: Adapt to different contexts through intelligent pattern recognition
3. **Dynamic Adaptation**: Use current context to inform decisions rather than static assumptions

## Technical Stack

- **Python 3.x**: Orchestration and text extraction
- **SWI-Prolog**: Logic reasoning engine
- **YAML**: Configuration format
- **Regular Expressions**: Pattern matching

## Common Issues and Solutions

### Issue: "Warning: Singleton variables"
**Solution**: Use underscore for unused variables in Prolog predicates

### Issue: Duplicate facts in output
**Solution**: Use `setof` instead of `findall`, check before asserting

### Issue: Inflated cascade counts
**Solution**: Deduplicate with `sort()` before counting

### Issue: No facts being derived
**Solution**: Check confidence thresholds and blocker conditions

## Future Enhancements

1. **Learning System**: Capture successful patterns for reuse
2. **Explanation Generation**: Natural language explanations
3. **Confidence Calibration**: Auto-tune confidence based on outcomes
4. **Interactive Mode**: Allow user to query reasoning paths
5. **Visualization**: Graph representation of derivation networks

## Critical Implementation Details

### Dynamic Predicates (Prolog)
```prolog
:- dynamic has_property/2.
:- dynamic has_property_full/4.
:- dynamic causal_rule/3.
:- dynamic causal_rule_full/5.
:- dynamic blocked_fact/2.
:- dynamic cascade_blocked/3.
:- dynamic block_source/4.
:- dynamic contradiction_clusters/3.
```

### Resolution Order in `resolve_with_cascade`
1. Identify all conflicts (negation, dimension, observed-derived)
2. Record contradiction clusters for reporting
3. Track block sources with principles
4. Mark directly blocked facts
5. Find cascade blocks recursively
6. Filter out all blocked facts from final derivations

### Controller Pipeline (`generic_controller.py`)
```python
def analyze_document(self, md_file, entity, show_rationale):
    # 1. Extract facts from markdown
    facts = self.extractor.extract(content, entity)
    
    # 2. Create temporary Prolog file
    with tempfile.NamedTemporaryFile() as f:
        # Load reasoning engine
        f.write(":- consult('generic_reasoner.pl').\n")
        
        # Load causal rules with blockers
        for rule in self.causal_config['causal_rules']:
            f.write(f"load_causal_rule_full(...)")
        
        # Assert facts with confidence
        for fact in facts:
            f.write(f"assertz(has_property_full(...))")
        
        # Run analysis
        f.write(f"analyze_with_config('{entity}')")
    
    # 3. Execute Prolog and capture output
    result = subprocess.run(['swipl', '-q', '-s', temp_file])
```

### Confidence Decay Formula
```
derive_all_depths_with_conf:
  Depth 0: confidence = rule_confidence
  Depth 1: confidence = rule_confidence × 0.9
  Depth 2: confidence = rule_confidence × 0.81
  Depth N: confidence = rule_confidence × (0.9 ^ N)
```

### Block Source Tracking
Each blocked fact records:
- **Entity**: The entity being analyzed
- **Blocked**: The fact that was blocked
- **Blocker**: The fact that caused the block
- **Principle**: The resolution principle applied

## Summary

This POC demonstrates a fully generic, configuration-driven approach to qualitative reasoning that:
- Extracts facts from unstructured text
- Applies multi-level causal inference
- Resolves contradictions using universal principles
- Provides complete transparency in reasoning
- Maintains zero hardcoding in core logic

The system successfully transforms domain expertise into declarative configurations while maintaining sophisticated reasoning capabilities including cascade blocking, dimension-based contradiction detection, and comprehensive provenance tracking.
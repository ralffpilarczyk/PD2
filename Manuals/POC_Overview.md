# Qualitative Reasoning POC - Minimal Core Implementation

## Executive Summary

This is a **minimal, gate-only qualitative reasoning system** that performs pattern-based inference with strict derivation controls. The system has been simplified to its essential core: extract facts, apply causal rules with gate checks, and provide transparent reporting. All complex features (dimensions, polarity, post-hoc scanning) have been removed in favor of a trustworthy, deterministic engine.

### Core Philosophy  
- **Gate-only derivation** - Check constraints BEFORE asserting facts, not after
- **Single representation** - One fact format throughout the system
- **Event-based metrics** - Count from accept/reject logs, not rescanning
- **Zero complexity** - No cascades, dimensions, or semantic metadata

## Architecture Overview

### System Components

```
qualitative_poc/
├── generic_controller.py      # Main orchestrator (minimal)
├── generic_extractor.py       # Simple pattern matching
├── generic_reasoner.pl        # Gate-only reasoning engine
├── config/
│   ├── extraction_rules.yaml  # Property patterns only
│   └── causal_rules.yaml      # Simple A→B rules
└── test_docs/                  # Test documents
```

### Data Flow

1. **Text Input** → `generic_extractor.py` extracts facts using patterns
2. **Facts** → `generic_controller.py` creates Prolog program 
3. **Prolog Program** → `generic_reasoner.pl` performs gate-checked derivation
4. **Output** → Simple report with observed, derived, and rejected facts

## Configuration System

### 1. Extraction Rules (`extraction_rules.yaml`)

Simple pattern matching for property extraction:

```yaml
properties:
  market_leader:
    patterns:
      - "market leader"
      - "leading market position"
      - "dominant player"
    confidence: 0.9
```

**Only two fields:**
- `patterns`: Text patterns to match
- `confidence`: Extraction confidence (0-1)

### 2. Causal Rules (`causal_rules.yaml`)

Simple causal relationships:

```yaml
causal_rules:
  - antecedent: market_leader
    consequent: pricing_power
    confidence: 0.8

settings:
  min_confidence_to_derive: 0.5
  max_depth: 5
```

**Minimal structure:**
- `causal_rules`: Simple A→B with confidence
- `settings`: Min confidence and max derivation depth

**Important:** Avoid circular rules like A→B and B→A which cause infinite loops

### 3. No Semantic Dimensions

The simplified system has no semantic dimensions or categories. Contradiction detection is limited to explicit negation pairs (e.g., `pricing_power` vs `no_pricing_power`).

## Core Algorithms

### 1. Fact Extraction (`generic_extractor.py`)

```python
def extract(self, text, entity="company"):
    facts = []
    for prop_name, config in self.properties.items():
        if self._matches_patterns(text, config['patterns']):
            facts.append({
                'property': prop_name,
                'confidence': config.get('confidence', 0.8)
            })
    return facts
```

**Simplified to:**
- Pattern matching only
- No contradiction checking
- No conflict resolution
- Just property and confidence

### 2. Gate-Only Derivation (`generic_reasoner.pl`)

The Prolog engine uses gate checks BEFORE asserting facts:

```prolog
% Gate check: can we assert this derived fact?
can_assert_fact(Entity, Property, Source, Confidence) :-
    % Check 1: No duplicate
    \+ fact(Entity, Property, _, _),
    % Check 2: Confidence threshold
    get_min_confidence(MinConf),
    Confidence >= MinConf,
    % Check 3: No negation conflict
    \+ has_negation_conflict(Entity, Property, Source).

% Derive with gate checking
derive_at_depth(Entity, _, NewFactsAdded) :-
    findall(Added,
            (rule(Antecedent, Consequent, RuleConf),
             fact(Entity, Antecedent, _, _),
             \+ fact(Entity, Consequent, _, _),
             (can_assert_fact(Entity, Consequent, derived, RuleConf) ->
                 assertz(fact(Entity, Consequent, derived, RuleConf)),
                 assertz(accept(Entity, Antecedent, Consequent, RuleConf)),
                 Added = true
             ;
                 assertz(reject(Entity, Antecedent, Consequent, Reason)),
                 Added = false
             )),
            Results).
```

**Key principle:** Check constraints BEFORE asserting, not after

### 3. Negation Handling

Only explicit negation is handled:

```prolog
% Check if properties are negations
is_negation(Prop1, Prop2) :-
    atom_concat('no_', Base, Prop1),
    Prop2 = Base.

% Negative always blocks positive
should_block(NegProp, _, _, _) :-
    atom_concat('no_', _, NegProp), !.
% Observed blocks derived  
should_block(_, observed, _, derived).
```

**Simple rules:**
1. `no_X` always blocks `X`
2. Observed facts block derived facts
3. No other conflict resolution

### 4. No Cascade Blocking

The simplified system has no cascade blocking. Each fact is evaluated independently with gate checks. If a fact is rejected, it simply doesn't get asserted - there's no post-hoc cascade analysis.

## Reporting System

### Output Sections

1. **Observed Facts**
   - Facts extracted from text with confidence

2. **Derived Facts (kept)**
   - Facts successfully derived through gate checks
   - Shows source and confidence

3. **Rejected Derivations**
   - Facts that failed gate checks
   - Shows rejection reason (duplicate, low confidence, negation)

4. **Reasoning Chains** (limited)
   - Simple direct edges from observed to derived
   - Omitted for documents with >5 facts or >15 edges

5. **Summary Statistics**
   - Observed count
   - Derived count
   - Rejected count
   - Max derivation depth used
   - Rule metrics from event log

## Key Simplifications from Previous Version

### Gate-Only Derivation
- **Before**: Derive everything then block in post-processing
- **After**: Check constraints BEFORE asserting any fact

### Single Fact Representation
- **Before**: Multiple representations with metadata
- **After**: Single format: `fact(Entity, Property, Source, Confidence)`

### Event-Based Metrics
- **Before**: Rescan rules to count applications
- **After**: Count from accept/reject event log

### No Complex Features
- **Before**: Dimensions, polarity, cascades, semantic categories
- **After**: Just facts, rules, and simple negation

### Fixed Circular Rules
- **Issue**: Rules like A→B and B→A caused infinite loops
- **Fix**: Removed circular rule (commoditization ↔ no_pricing_power)

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

### Issue: Timeout with many facts
**Cause**: Circular rules creating infinite derivation loops
**Solution**: Remove circular rules from causal_rules.yaml

### Issue: "Goal (directive) failed" warning
**Cause**: Harmless - Prolog expects to halt but subprocess captures output
**Solution**: Ignore this warning, it doesn't affect results

### Issue: No facts being derived
**Solution**: Check min_confidence threshold in settings

### Issue: Performance with large documents  
**Solution**: Controller skips Prolog for >15 facts, shows summary only

## Intentionally Removed Features

These features were removed to create a minimal, trustworthy core:

1. **Semantic Dimensions**: No categorization of properties
2. **Cascade Blocking**: No multi-level dependency tracking  
3. **Complex Conflict Resolution**: Only simple negation handling
4. **Blocking Rules**: No conditional blocking based on other facts
5. **Expected Patterns**: No anomaly detection
6. **Priority System**: No override based on priority levels
7. **Confidence Decay**: No reduction over derivation depth

## Critical Implementation Details

### Dynamic Predicates (Prolog)
```prolog
:- dynamic fact/4.           % entity, property, source, confidence
:- dynamic rule/3.           % antecedent, consequent, confidence  
:- dynamic accept/4.         % entity, antecedent, consequent, confidence
:- dynamic reject/4.         % entity, antecedent, consequent, reason
:- dynamic edge/3.           % entity, antecedent, consequent
:- dynamic config_setting/1. % min_confidence, max_depth
```

### Gate Check Process
```prolog
can_assert_fact(Entity, Property, Source, Confidence) :-
    \+ fact(Entity, Property, _, _),           % No duplicate
    get_min_confidence(MinConf),
    Confidence >= MinConf,                      % Above threshold
    \+ has_negation_conflict(Entity, Property, Source). % No negation
```

### Controller Pipeline (`generic_controller.py`)
```python
def analyze_document(self, md_file, entity):
    # 1. Extract facts
    facts = self.extractor.extract(content, entity)
    
    # 2. Skip if too complex (>15 facts)
    if len(facts) > 15:
        print("Document too complex - summary only")
        return
    
    # 3. Create Prolog program
    with tempfile.NamedTemporaryFile() as f:
        f.write(":- consult('generic_reasoner.pl').\n")
        # Load rules
        for rule in causal_rules:
            f.write(f"load_rule('{rule['antecedent']}', ...")
        # Assert facts
        for fact in facts:
            f.write(f"assertz(fact('{entity}', ...")
        # Run analysis
        f.write(f"analyze('{entity}').")
    
    # 4. Execute with timeout
    subprocess.run(['swipl', '-q', '-s', temp_file], timeout=60)
```

## Summary

This minimal POC demonstrates a simplified qualitative reasoning system that:
- Extracts facts using simple pattern matching
- Applies gate-checked causal derivation
- Handles explicit negation conflicts only
- Provides transparent event-based metrics
- Avoids all complex features that caused issues

The system trades sophistication for reliability, creating a trustworthy core that:
- Never asserts facts that violate constraints
- Counts metrics from event logs, not rescanning
- Avoids circular rules that cause infinite loops
- Maintains single representation throughout
- Completes analysis in milliseconds for reasonable document sizes
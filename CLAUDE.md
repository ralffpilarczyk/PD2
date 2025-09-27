# CLAUDE.md

This file provides guidance to AI assistants when working with code in this repository.

## Project Overview
ProfileDash 2.0 is an intelligent document analysis system that processes PDF financial documents (annual reports, financial statements, investor presentations) and generates comprehensive company profiles with 32 different analytical sections using Google's Gemini API.

## Architecture
The project uses a clean modular architecture with all core components in the `src/` package:
- `PD2.py` - Main application entry point with UI and orchestration
- `src/core_analyzer.py` - Multi-step analysis pipeline with progressive refinement
- `src/insight_memory.py` - Learning system that captures analytical patterns
- `src/profile_generator.py` - HTML report generation with markdown processing
- `src/utils.py` - Shared utilities including rate limiting and markdown fixes
- `src/file_manager.py` - Handles file I/O and markdown preprocessing
- `src/profile_sections.py` - Defines all 32 analysis sections

## Key Commands
```bash
# Run the application
python PD2.py

# Install dependencies
pip install -r requirements.txt
```

## Development Guidelines
1. **API Configuration**: Ensure `GEMINI_API_KEY` is set in `.env` file
2. **Rate Limiting**: All Gemini API calls must use `retry_with_backoff` from `src/utils.py`
3. **Memory System**: Learning insights stored in `memory/learning_memory.json` - section-based, max 30 words, quality score 9-10
4. **Section 32**: Special handling - no word limits, pure data extraction
5. **Parallel Processing**: Section analysis uses ThreadPoolExecutor (1-5 workers configurable)
6. **PDF Conversion**:
   - **LLM-Enhanced Mode** (default): Uses Gemini API to improve table/chart extraction
   - **Basic Mode**: Standard Marker conversion without LLM
   - Worker count auto-adjusts: 2 for LLM mode (API-bound), 3-5 for basic (CPU-bound)
   - User can toggle between modes via UI prompt

## Code Style & Communication
1. **No Emojis**: Do not use emojis in code, comments, commit messages, or documentation
2. **Professional Tone**: Maintain clear, concise technical communication
3. **Comments**: Only add comments when explicitly requested

# Engineering Principles: Build Systems That Stay Correct

## Core Principles

### Prime directive: Consistency over velocity
• Never ship changes that create logical contradictions or leave the system in an ambiguous state.
• If a requirement forces a contradiction, stop and clarify before proceeding.

### Specify → Verify → Implement
• **Specify completely**: normal behavior, edge cases, conflict handling, and expected outputs.
• **Verify logically**: walk through a few concrete scenarios by hand; ensure no contradictions arise.
• **Implement once**: build the full, consistent solution rather than accreting fixes.

### Gate before commit
• Validate preconditions and conflicts before mutating state.
• If an action doesn't meet the gate, don't "do-and-fix"; reject with a single, clear reason and move on.

### Single source of truth
• Maintain one canonical representation of state; avoid parallel or redundant forms.
• Derive metrics and summaries from the same authoritative source (e.g., the event log), not from rescans or recomputation.

### Determinism by design
• Same inputs should yield the same ordering, counts, and outputs.
• Sort and format outputs deterministically; eliminate non-deterministic iteration and timing-dependent behavior.

### Bounded complexity
• Constrain loops, recursion, and search space (depth, breadth, time).
• Prefer linear-time designs over pairwise scans; avoid O(N²) patterns unless strictly necessary and justified.

### Minimal surface area
• Keep the smallest possible feature set that delivers the outcome.
• Remove dead code and feature toggles you don't intend to use; one representation is better than three.

### Invariants and integrity checks
• Define non-negotiable invariants (e.g., "applied ≤ attempted; no negative counts; no duplicates") and assert them.
• Fail fast on invariant violations; surface a clear error instead of producing untrustworthy outputs.

### Stop and ask when unclear
If a requirement is ambiguous, conflicts with existing behavior, or omits edge cases/expected outputs, pause and request clarification.

**Template for clarification:**
```
I need clarification before implementing:
1. "[Ambiguity] could mean [A] or [B]; which is intended?"
2. "This change touches [X, Y, Z]; should they be updated for consistency?"
3. "When [edge case] happens, should the system [option 1] or [option 2]?"
4. "For input [example], is [output] the expected result?"
```

### Tests that matter
Write small acceptance tests that reflect real user outcomes; include:
• A happy path
• A contradiction/conflict case
• A boundary/degenerate case

Ensure tests assert invariants and determinism, not just existence of output.

### Conflict handling: be explicit
• Treat explicit conflicts (e.g., mutually exclusive states) as contradictions; reject one with a clear reason.
• Treat "tensions" (plausible co-existence) as design questions; seek clarification rather than encoding assumptions.

### Observability and accountability
• Log meaningful, single-source events (attempted, accepted, rejected with reason).
• Build summaries from those events; avoid secondary scans that create drift or double counting.

## Domain-Specific: Reasoning and Derivation Systems

When working with systems that derive facts, apply rules, or build reasoning chains, these additional principles apply:

### Cascade Principle
When blocking or removing a fact, ALL downstream derivations must also be blocked/removed.
```python
# Example invariant:
# If fact A is blocked, and B was derived from A, then B must also be blocked
# If C was derived from B, then C must also be blocked (transitive)
```

### Contradiction Resolution Hierarchy
When facts contradict each other, resolve using this precedence:
1. **Explicit negations win**: "no_X" overrides "X"
2. **Observed facts win**: Directly stated facts override derived facts
3. **Shorter paths win**: Facts with shorter derivation chains override longer ones
4. **Recent wins**: More recently observed facts override older ones

### Semantic Consistency
Facts with opposite meanings in the same category are mutually exclusive:
• Cannot have both "market_leader" and "weak_position"
• Cannot have both "high_margins" and "margin_compression"
• Cannot derive both "strong_cash_flow" and "weak_cash_flow"

### Before Implementing Any Reasoning Change
1. **Trace cascade effects**: List all facts that would be affected downstream
2. **Check for contradictions**: Identify any mutual exclusions that would be violated
3. **Verify resolution strategy**: Ensure you know which fact wins in each conflict

### Test Cases for Reasoning Systems
Always test these scenarios:
• **Cascade test**: If A→B→C→D and A is blocked, verify B,C,D are also blocked
• **Contradiction test**: If both X and no_X are present, verify no_X wins
• **Semantic test**: If deriving opposites, verify only one survives
• **Consistency test**: Final output contains no logical contradictions

### When Logical Contradictions Are Detected
```
This implementation would create a logical contradiction:
- Current system state: [describe state]
- This change would create: [describe contradiction]
- Resolution options:
  1. Apply precedence rules: [which fact wins and why]
  2. Cascade removal: [what else must be removed]
  3. Redesign required: [if precedence unclear]

Which approach should I take?
```

## Red Flags Requiring Immediate Stop

If you observe any of these patterns, STOP and seek clarification:
• Implementing the same feature for the third time
• A fix for your previous fix
• Adding special cases after "complete" implementation
• Discovering logical contradictions during testing (should be caught in design)
• Circular dependencies in derivation
• Unbounded growth in derived facts
• Non-deterministic output from same input

## The Ultimate Test

Before committing any change, ask:
1. Will this work correctly the FIRST time for ALL cases?
2. Have I traced through edge cases manually?
3. Does this maintain ALL system invariants?

If any answer is "no" or "unsure", the specification is incomplete. Stop and clarify.

## Deep Analysis Methodology
When analyzing company data, always apply multi-layer analytical thinking:

1. **Surface vs. Reality**: Identify contradictions between management claims and actual data
2. **Calculate Hidden Metrics**: Derive ratios and relationships not explicitly provided
3. **Pattern Recognition**: Find correlations across time periods and different data sets
4. **Relevance Filter**: Every insight must pass "Does this matter to company prospects?"
5. **Logic Test**: Every insight must pass "Does this make business sense?"
6. **Data Density**: Maximum insights per word - eliminate all fluff and corporate language
7. **Contradiction Highlighting**: Always flag where management narrative diverges from data reality

**Quality Standard**: Would this insight change an investor's view of the company's prospects? If no, remove it.

## Analysis Pipeline
Each section goes through multiple refinement steps with progressive condensation:
1. Initial Draft - Comprehensive analysis with all relevant data
2. Completeness Check - Identifies missing critical information
3. Enhanced Draft - Incorporates missing elements
4. Deep Analysis & Polish - Condenses to ~500 words of essential insights
5. (Optional) Discovery Pipeline - Additional pattern finding for deeper insights
6. Learning Extraction - Captures reusable analytical methodologies

## Important Patterns
- Section definitions in `profile_sections.py` organized into 5 groups
- Thread-safe operations for parallel processing (using thread_safe_print from utils.py)
- Two-stage quality filtering for learning insights
- Exponential backoff for API rate limits (up to 3 retries)
- Professional HTML output with markdown-to-HTML conversion
- All print statements should use thread_safe_print to avoid garbled output in parallel mode

## Testing & Quality
No formal test suite exists. When making changes:
- Test with sample PDFs
- Monitor `runs/run_*/run_summary.txt` for errors
- Check `quality_metrics/` for tracking data
- Verify HTML output renders correctly

## Recent Improvements
- **LLM-Enhanced PDF Conversion**: Optional AI-powered extraction for better table/chart handling
- **Markdown Table Fixes**: Automatic correction of corrupted tables from PDF conversion
- **HTML Rendering**: Fixed table rendering and Section 32 code block issues
- **Workflow Optimization**: All user selections happen upfront before processing
- **Safety Features**: Output size limits prevent excessive LLM generation
- **Thread Safety**: All operations use thread-safe printing for parallel processing

## Output Structure
```
runs/
└── run_YYYY_MM_DD_HH_MM_SS/
    ├── section_1/
    │   ├── step_1_initial_draft.md
    │   ├── step_2_completeness_check.txt
    │   ├── step_3_enhanced_draft.md
    │   ├── step_4_final_section.md
    │   └── step_6_learning.json
    ├── [Company]_profile.md      # Combined markdown
    ├── [Company]_profile.html    # Final HTML output
    └── run_summary.txt          # Processing summary
```

## Known Limitations
- Depends on Marker library for PDF conversion (can have table corruption issues)
- PDF conversion limited to single worker due to PyTorch tensor memory constraints
- No formal test suite or type checking
- LLM quality scoring requires aggressive filtering (9-10 threshold)
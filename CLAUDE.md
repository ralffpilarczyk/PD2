# CLAUDE.md

This file provides guidance to AI assistants when working with code in this repository.

## Project Overview
ProfileDash 2.0 is an intelligent document analysis system that processes PDF financial documents (annual reports, financial statements, investor presentations) and generates comprehensive company profiles with 33 analytical sections using Google's Gemini API.

## Architecture Principles
The system follows a clean modular architecture with separation of concerns:
- **Orchestration Layer**: Main entry point handles user interaction and coordinates all components
- **Analysis Pipeline**: Multi-step refinement process with progressive condensation
- **Learning System**: Captures and reuses analytical patterns across runs
- **Output Generation**: Professional PDF reports with HTML intermediate format
- **Utilities Layer**: Shared functionality for thread safety, rate limiting, and data cleaning
- **File Management**: Handles I/O operations and markdown preprocessing
- **Section Definitions**: Declarative specification of all analysis requirements

## Key Commands
```bash
# Run the application
python PD2.py

# Install dependencies
pip install -r requirements.txt
```

## Development Guidelines

### Core System Principles
1. **API Operations**: All external API calls must implement exponential backoff and respect rate limits
2. **Learning System**: Insights must be high-quality (9-10 score), concise (max 30 words), and section-specific
3. **Special Cases**: Section 33 (Data Book) has no word limits - pure extraction mode
4. **Concurrency**: Use thread-safe operations for all parallel processing contexts
5. **PDF Processing**: Support both LLM-enhanced and basic conversion modes with appropriate worker counts
6. **Output Quality**: All analysis must pass the relevance filter: "Does this change an investor's view?"

### Processing Modes
- **LLM-Enhanced PDF Conversion**: Uses AI for better table/chart extraction (2 workers - API-bound)
- **Basic PDF Conversion**: Standard Marker library conversion (3-5 workers - CPU-bound)
- **Parallel Section Analysis**: Configurable worker count (1-8) based on API quotas
- **Two-Phase Scheduling**: Process sections 1-32 first, then section 33 separately

## Code Style & Communication
1. **No Emojis**: Do not use emojis in code, comments, commit messages, or documentation
2. **Professional Tone**: Maintain clear, concise technical communication
3. **Comments**: Only add comments when explicitly requested
4. **Git Commits**: Do NOT add "Co-Authored-By: Claude" or any AI attribution lines to commit messages

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

## Analysis Pipeline Architecture
Each section follows a progressive refinement approach with multiple stages:
1. **Initial Draft** - Comprehensive analysis capturing all relevant data (~1000 words)
2. **Completeness Check** - Validates against source documents, identifies gaps
3. **Enhanced Draft** - Incorporates missing elements from validation
4. **Deep Analysis & Polish** - Condenses to essential insights (~500 words) with investor relevance filter
5. **Discovery Pipeline** (Optional) - Six-stage deep pattern finding for quantitative insights
6. **Learning Extraction** - Captures reusable analytical methodologies for future runs

## Design Patterns & Conventions

### Parallel Processing Patterns
- **Thread Safety First**: All console output must use thread-safe mechanisms in parallel contexts
- **Worker Pool Management**: Thread pools for section analysis, process pools for PDF conversion
- **Resource Limiting**: Global semaphores control concurrent API calls to prevent quota exhaustion
- **Staged Scheduling**: Submit tasks with slight delays to prevent thundering herd

### Quality Assurance Patterns
- **Two-Stage Filtering**: Learning insights undergo dual quality checks before persistence
- **Output Validation**: Check for empty outputs, retry with fallback to previous steps
- **Table Constraints**: Enforce maximum dimensions (10 columns, 20 rows) to prevent bloat
- **Markdown Cleaning**: Automatic correction of corrupted tables from PDF conversion

### Error Handling Patterns
- **Exponential Backoff**: 3 retries with increasing delays (1s, 2s, 4s) and jitter
- **Graceful Degradation**: Failed sections don't block others, partial outputs are acceptable
- **Respect Server Signals**: Honor Retry-After headers from API responses
- **Cache Aggressively**: SHA-256 based caching prevents redundant PDF conversions

### Output Delivery Patterns
- **Two-Phase Generation**: Sections 1-32 first, then Section 33 separately
- **Progressive Refinement**: Multiple passes with increasing constraints
- **Format Chain**: Markdown → HTML → PDF with WeasyPrint
- **Professional Styling**: Consistent typography, pagination, footers with generation metadata

## Testing & Quality
No formal test suite exists. Testing approach:
- **Validation Method**: Test with sample PDFs representing different document types
- **Monitoring**: Check run summaries and quality metrics for errors and degradation
- **Output Verification**: Manually review PDF output for formatting and content quality
- **Regression Detection**: Compare outputs across runs for consistency

## Output Structure
```
runs/run_YYYY_MM_DD_HH_MM_SS/    # Timestamped run directory
├── section_*/                    # Individual section work products
│   ├── step_1_initial_draft.md
│   ├── step_2_completeness_check.txt
│   ├── step_3_enhanced_draft.md
│   ├── step_4_final_section.md
│   └── step_6_learning.json
├── [Company]_profile.md          # Combined markdown
└── run_summary.txt               # Processing log

ReportFiles/                      # Final deliverables
└── [Company]_YYMMDD_HHMM.pdf    # Professional PDF report
```

## Known Constraints & Limitations
- **PDF Conversion**: Marker library can corrupt tables during extraction
- **Process Isolation**: PDF conversion requires separate processes due to PyTorch memory constraints
- **Quality Filtering**: Learning system requires aggressive 9-10 threshold to prevent noise
- **No Formal Testing**: Manual validation only - no automated test suite
- **Memory Accumulation**: Learning insights grow unbounded over time
# CLAUDE.md

## Onboarding

When starting substantial work on this codebase, read:
1. README.md - project overview and usage
2. Manuals/*.md - detailed technical documentation

Trigger with: "familiarize yourself with the codebase" or similar.

## Working Style

1. **Brutally critical, constructive**: Point out flaws directly. Don't soften feedback or be diplomatic about problems. Every critique must come with a solution or path forward.

2. **Ask, don't assume**: When requirements are ambiguous or could be interpreted multiple ways, stop and ask. Don't fill gaps with assumptions.

3. **Consistency over velocity**: Never ship changes that create contradictions or leave the system in an ambiguous state. If a requirement forces a contradiction, stop and clarify.

4. **Specify, verify, implement**: Think through edge cases before coding. Walk through scenarios manually. Build the full solution once rather than accreting fixes.

## Code Quality Rules

1. **Universal over specific**: Code and prompts must work across all industries, sectors, and company types. Never hardcode company names, industry terms (e.g., "CPO", "FFB"), or sector-specific examples into logic or validation.

2. **Variables over magic numbers**: Never hardcode numbers into formulas. Define named constants or configurable parameters. If a threshold might need tuning, make it a variable.

3. **Minimal surface area**: Keep the smallest feature set that delivers the outcome. Remove dead code. One representation is better than three.

4. **Single source of truth**: Avoid parallel or redundant data structures. Derive summaries from authoritative sources, don't recompute.

5. **Fail fast**: Validate preconditions before mutating state. Surface clear errors instead of producing untrustworthy outputs.

6. **Determinism**: Same inputs must yield same outputs. Eliminate non-deterministic iteration and timing-dependent behavior.

## Communication Style

1. **No emojis**: Never use emojis in code, comments, commit messages, or documentation.
2. **Professional tone**: Clear, concise technical communication.
3. **Comments only when requested**: Don't add docstrings or comments unless explicitly asked.
4. **No AI attribution**: Do NOT add "Co-Authored-By: Claude" or similar to commits.

## Project: ProfileDash 2.2

A document analysis system that processes PDF financial documents and generates company profiles using Google's Gemini API.

**Two tools:**
- **PD2**: 34-section comprehensive profiles for M&A/investment analysis
- **OPP**: One-page profiles for quick screening

**Key commands:**
```bash
python PD2.py      # Run ProfileDash
python OPP.py      # Run OnePageProfile
```

## Analysis Quality Standards

Every insight must pass these filters:
1. **Investor relevance**: Would this change an investor's view? If no, remove it.
2. **Logic test**: Does this make business sense?
3. **Data density**: Maximum insights per word. Eliminate fluff and corporate language.
4. **Surface vs reality**: Flag where management narrative diverges from data.

## LLM Pipeline Design

1. **Hypothesis generation without document anchoring**: When generating hypotheses or independent analysis, strategically detach from source documents. Attaching documents during hypothesis generation causes the LLM to anchor to existing framing rather than reason independently. Generate hypotheses first, then test against documents.

2. **Separate testing from synthesis**: Testing (verifying evidence) and synthesis (drawing conclusions) are different cognitive tasks requiring different temperatures. Testing needs precision (low temperature). Synthesis needs judgment (medium temperature). Don't combine them in one step.

3. **Universal prompts with section-specific pointers**: When building multi-section analysis, keep the reasoning chain universal (same steps, same logic) but allow section-specific guidance through minimal pointers. Don't hardwire different algorithms per section - use one algorithm with parameterized inputs.

## Red Flags - Stop and Clarify

- Implementing the same feature for the third time
- A fix for your previous fix
- Adding special cases after "complete" implementation
- Discovering contradictions during testing (should be caught in design)
- Non-deterministic output from same input

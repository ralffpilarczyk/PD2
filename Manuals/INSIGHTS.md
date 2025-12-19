# Ground Truth Insight Pipeline - Implementation Plan

## Overview

This document specifies an enhanced analysis pipeline for Sections 1-32 that adds ground truth discovery, hypothesis testing, and insight integration to the existing 4-step descriptive pipeline.

**Goal**: Produce sections where insight is woven into description, not bolted on. Every sentence helps an investor understand how the company actually competes.

**Inspiration**: Section 33's hypothesis-driven pipeline, which develops genuine chain-of-thought by generating hypotheses without document anchoring, then testing them.

---

## Pipeline Architecture

### The 9-Step Pipeline

| Step | Name | Docs Attached | Temp | Input | Output |
|------|------|---------------|------|-------|--------|
| 1 | Draft | YES | 0.6 | Source docs + section specs | ~1000 words |
| 2 | Completeness | YES | 0.2 | Step 1 | ADD list (max 5 items) |
| 3 | Enhance | YES | 0.6 | Step 1 + ADD list | ~1000 words |
| 4 | Polish | YES | 0.6 | Step 3 | ~500 words |
| 5 | Ground Truth | YES | 0.6 | Source docs + ground truth pointer | 2-3 observations |
| 6 | Hypothesize | **NO** | 0.6 | Step 5 | Implications, predictions |
| 7 | Test | YES | 0.2 | Step 6 | Evidence + verdicts |
| 8 | Synthesize | YES | 0.6 | Step 7 | Insight paragraph (~150 words) |
| 9 | Integrate | YES | 0.6 | Step 4 + Step 8 | Final section ~500 words |

### Execution Flow

```
                    Source Documents
                           │
              ┌────────────┴────────────┐
              │                         │
              ▼                         ▼
    ┌─────────────────┐       ┌─────────────────┐
    │  DESCRIPTIVE    │       │  GROUND TRUTH   │
    │  TRACK          │       │  TRACK          │
    │                 │       │                 │
    │  Step 1: Draft  │       │  Step 5: Find   │
    │       ↓         │       │       ↓         │
    │  Step 2: Check  │       │  Step 6: Hypo   │  ← NO DOCS
    │       ↓         │       │       ↓         │
    │  Step 3: Enhance│       │  Step 7: Test   │
    │       ↓         │       │       ↓         │
    │  Step 4: Polish │       │  Step 8: Synth  │
    │                 │       │                 │
    └────────┬────────┘       └────────┬────────┘
             │                         │
             └───────────┬─────────────┘
                         │
                         ▼
               ┌─────────────────┐
               │  Step 9:        │
               │  Integrate      │
               │                 │
               │  Polish + Synth │
               │  → Final Section│
               └─────────────────┘
```

**Parallel Execution**: Steps 1-4 and Steps 5-8 run in parallel. Step 9 waits for both tracks to complete.

---

## Scope

- **Sections 1-32**: Use the 9-step pipeline
- **Section 33**: Keep existing 4-layer hypothesis pipeline (unchanged)
- **Section 34**: Keep existing single-pass Data Book extraction (unchanged)
- **Requirement**: Each section must have a ground truth pointer defined before it can run

---

## Ground Truth Pointers

Each section needs a "ground truth pointer" - a one-sentence prompt that tells the LLM what ground truth means for THAT section. These live in `profile_sections.py` as a new field.

### Sections 1-12 (Define Now)

| Section | Title | Ground Truth Pointer |
|---------|-------|---------------------|
| 1 | Operating Footprint | How does the physical and human structure enable or constrain how the company competes? |
| 2 | Products and Services | How does the company actually sell, deliver, price, and get paid for its products? |
| 3 | Key Customers | Why do customers buy from this company, stay, or leave? |
| 4 | Key Suppliers | What supplier dependencies create risk or advantage? |
| 5 | Key Competitors | How does the company actually win or lose against specific competitors? |
| 6 | Operational KPIs | What do the actual metrics reveal about unit economics and operational health? |
| 7 | Summary Financials (Consolidated) | What do the numbers reveal about earnings quality and cash generation? |
| 8 | Summary Financials (Segment) | Which segments actually drive value and which destroy it? |
| 9 | Balance Sheet | What does the capital structure reveal about financial flexibility and risk? |
| 10 | Top 10 Shareholders | What does ownership structure reveal about control, alignment, and exit dynamics? |
| 11 | M&A Agenda | What does transaction history reveal about strategic priorities and execution capability? |
| 12 | Key Decision Makers | Who actually controls decisions and what are their incentives? |

### Sections 13-32 (Define Before Implementation)

Ground truth pointers must be defined before these sections can use the new pipeline. Until defined, these sections are blocked from running.

---

## Prompts

### Step 5: Ground Truth Discovery

**Documents attached: YES | Temperature: 0.6**

```
Search the source documents about [Company]'s [SECTION TITLE].

GROUND TRUTH FOCUS: [Ground truth pointer from profile_sections.py]

Find 2-3 specific observations that reveal this ground truth. Look beyond the corporate narrative for:
- What the actual structure/data reveals about competitive reality
- How things actually work (mechanisms), even if not explicitly stated
- Where what the company DOES diverges from what it SAYS

For each observation:

OBSERVATION [N]:
GROUND TRUTH: [What the data/structure actually reveals - be specific, cite source]
vs. NARRATIVE: [What management says or implies, if different]
COMPETITIVE IMPLICATION: [What this means for how the company wins or loses]

If no meaningful ground truth observations exist for this section, state "No significant observations identified" and explain why.
```

### Step 6: Hypothesis Generation

**Documents attached: NO | Temperature: 0.6**

```
You identified these ground truth observations about [Company]'s [SECTION TITLE]:

[Insert Step 5 output]

Without re-reading the source documents, generate hypotheses for each observation:

OBSERVATION [N]:
IMPLICATION: What does this reveal about how the company actually operates?
ASSUMPTION: What would have to be true for this to make sense?
RISK: What's the downside if this isn't sustainable or isn't what it appears?
PREDICTION: What else should we see in the documents if this interpretation is correct?
```

### Step 7: Hypothesis Testing

**Documents attached: YES | Temperature: 0.2**

```
Test these hypotheses about [Company]'s [SECTION TITLE]:

[Insert Step 6 output]

For each prediction, search the source documents:

OBSERVATION [N]:
PREDICTION: [from Step 6]
SUPPORTING EVIDENCE: [Quote specific passages with page/section references, or "None found"]
DISCONFIRMING EVIDENCE: [What would refute this? Did you find it? Quote if yes.]
VERDICT: Supported / Refuted / Unclear
```

### Step 8: Synthesis

**Documents attached: YES | Temperature: 0.6**

```
Based on ground truth analysis of [Company]'s [SECTION TITLE]:

OBSERVATIONS:
[Insert Step 5 output]

TEST RESULTS:
[Insert Step 7 output]

Write an insight synthesis (150 words maximum):

What does the ground truth reveal about how this company actually competes in this area?

Rules:
- Take a position - don't hedge
- Focus on what an investor needs to understand
- Reference specific evidence that supports your position
- Note any critical uncertainties that remain
```

### Step 9: Integration

**Documents attached: YES | Temperature: 0.6**

```
You have two inputs for [Company]'s [SECTION TITLE]:

POLISHED DESCRIPTION:
[Step 4 output]

GROUND TRUTH INSIGHTS:
[Step 8 output]

Rewrite the section so the ground truth insights are woven into the description.

Rules:
- The insights should strengthen the description, not appear as a separate addendum
- Where ground truth contradicts or deepens the narrative, the ground truth wins
- Preserve important data points and tables from the description
- Keep to ~500 words maximum
- Every sentence should help an investor understand how this company actually competes
- Do not add a separate "Analytical Notes" section - integrate fully
```

---

## Implementation Tasks

### Phase 1: Data Model Changes

**File: `src/profile_sections.py`**

1. Add `ground_truth_pointer` field to each section dictionary
2. Populate pointers for Sections 1-12 (per table above)
3. Leave Sections 13-32 pointers as `None` (will block execution)

```python
{
    "number": 1,
    "title": "Operating Footprint",
    "specs": "...",  # existing
    "ground_truth_pointer": "How does the physical and human structure enable or constrain how the company competes?"  # NEW
}
```

### Phase 2: Core Analyzer Changes

**File: `src/core_analyzer.py`**

1. Add new methods:
   - `ground_truth_discovery(section, company_name)` - Step 5
   - `hypothesis_generation(step5_output, section)` - Step 6
   - `hypothesis_testing(step6_output, section, company_name)` - Step 7
   - `insight_synthesis(step5_output, step7_output, section, company_name)` - Step 8
   - `insight_integration(step4_output, step8_output, section, company_name)` - Step 9

2. Add validation: Check `ground_truth_pointer` is not None before running Steps 5-9

3. Ensure Step 6 does NOT attach documents (critical for breaking anchoring)

### Phase 3: Orchestration Changes

**File: `PD2.py`**

1. Modify `analyze_section()` to run two parallel tracks:
   - Track A: Steps 1-4 (existing)
   - Track B: Steps 5-8 (new)

2. Add Step 9 that waits for both tracks and integrates

3. Update `WorkerDisplay` to show new steps:
   - "Sec. N → Draft" (Step 1)
   - "Sec. N → Refine" (Steps 2-3)
   - "Sec. N → Polish" (Step 4)
   - "Sec. N → Ground" (Step 5)
   - "Sec. N → Hypo" (Step 6)
   - "Sec. N → Test" (Step 7)
   - "Sec. N → Synth" (Step 8)
   - "Sec. N → Integrate" (Step 9)

4. Handle blocked sections (no ground truth pointer):
   - Log warning
   - Skip section entirely
   - Report in run summary

### Phase 4: File Output Changes

**Directory structure per section:**

```
runs/run_TIMESTAMP/section_N/
├── step_1_draft.md
├── step_2_completeness.txt
├── step_3_enhanced.md
├── step_4_polished.md
├── step_5_ground_truth.md      # NEW
├── step_6_hypotheses.md        # NEW
├── step_7_test_results.md      # NEW
├── step_8_synthesis.md         # NEW
└── step_9_final_section.md     # NEW (replaces step_4 as final)
```

### Phase 5: Testing

1. Run on 2-3 test companies with Sections 1-12 only
2. Compare Step 4 output vs Step 9 output
3. Evaluate: Are insights woven in? Is ground truth visible?
4. Iterate on prompts as needed

### Phase 6: Expand Ground Truth Pointers

1. Define pointers for Sections 13-19 (Strategy/SWOT)
2. Test on same companies
3. Define pointers for Sections 20-32 (Sellside/Buyside)
4. Full pipeline operational

---

## API Cost Impact

**Current pipeline (Sections 1-32):**
- 4 steps × 32 sections = 128 API calls

**New pipeline (Sections 1-32):**
- 9 steps × 32 sections = 288 API calls

**Increase**: +160 calls (+125%)

**Note**: Steps 1-4 and 5-8 run in parallel, so wall-clock time increase is less than 2x.

---

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| Step 9 loses descriptive content | Prompt explicitly requires preserving data points and tables |
| Step 6 anchors to documents anyway | Enforce NO document attachment in code, not just prompt |
| Ground truth pointers too vague | Start with Sections 1-12, iterate before expanding |
| Parallel execution race conditions | Step 9 explicitly waits for both tracks via futures |
| Empty ground truth observations | Allow "No significant observations" - don't force insights where none exist |

---

## Success Criteria

A section passes if:
1. Ground truth insights are woven into description (not separate addendum)
2. Mechanisms are explained, not just described (HOW, not just WHAT)
3. Corporate buzzwords are replaced with specific operational reality
4. An investor reading the section understands how the company actually competes
5. Where narrative and ground truth conflict, ground truth wins

---

## Files to Modify

| File | Changes |
|------|---------|
| `src/profile_sections.py` | Add `ground_truth_pointer` field to all sections |
| `src/core_analyzer.py` | Add Steps 5-9 methods, validation logic |
| `PD2.py` | Parallel track execution, Step 9 integration, WorkerDisplay updates |

---

## Next Steps

1. Review and approve this plan
2. Define ground truth pointers for Sections 1-12
3. Implement Phase 1 (data model)
4. Implement Phase 2 (core analyzer)
5. Implement Phase 3 (orchestration)
6. Test on sample companies
7. Iterate on prompts
8. Expand to Sections 13-32

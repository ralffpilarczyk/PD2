# Complete Section Analysis Workflow

## Pre-Step: Document Loading
- PDFs converted to markdown → stored in `full_context` (entire document corpus)

---

## Step 1: Initial Draft
**LLM Call #1** (`create_initial_draft`)
- **Input:**
  - Section requirements/specs
  - Relevant memory (analytical methodologies from previous runs)
  - Full document context (`full_context`)
- **Temperature:** 0.6 (medium)
- **Output:** ~1000 word initial draft
- **Saved as:** `step_1_initial_draft.md`

---

## Step 2: Completeness Check
**LLM Call #2** (`completeness_check`)
- **Input:**
  - Section requirements/specs
  - Full document context (`full_context`)
  - Current draft from Step 1
- **Temperature:** 0.2 (low)
- **Output:** ADD list of missing data items
- **Saved as:** `step_2_add_list.txt`

---

## Step 3: Apply Completeness
**LLM Call #3** (`apply_completeness_only`)
- **Input:**
  - Current draft from Step 1
  - ADD list from Step 2
  - Full document context (to look up the missing data)
- **Temperature:** 0.6 (medium)
- **Output:** Enhanced draft with added data
- **Saved as:** `step_3_improved_draft.md`
- **Known issue:** Sometimes includes meta-commentary

---

## Step 4: Deep Analysis (Discovery Pipeline - 6 sub-calls)

### Sub-call 4.1: Extract All Data
- **Input:** Enhanced draft from Step 3
- **Temperature:** 0.2 (low)
- **Output:** 
  ```
  CLAIMS AND STATEMENTS:
  - [C1] "efficiency efforts"
  - [C2] "investing in employee development"
  
  QUANTIFIABLE FACTS:
  - [F1] employees: 3,360 (2024) [-13.54% from 2023]
  - [F2] energy_consumption: 1,691 TJ (2024) [+13.11% from 2022]
  ```
- **Saved as:** `discovery_pipeline/stage_1_extracted_data.txt`

### Sub-call 4.2: Calculate Material Relationships
- **Input:** Extracted data from 4.1
- **Temperature:** 0.6 (medium)
- **Output:**
  ```
  CALCULATED RELATIONSHIPS:
  - [R1] Employee reduction 13.54% while subscriber growth continued
  - [R2] Energy consumption increased 13.11% despite "efficiency" claims
  
  CLAIM-FACT MATCHES:
  - [M1] Claim [C1] "efficiency efforts" CONTRADICTED by Fact [F2] rising energy
  ```
- **Saved as:** `discovery_pipeline/stage_2_relationships.txt`

### Sub-call 4.3: Identify Anomalies
- **Input:** Relationships from 4.2
- **Temperature:** 0.6 (medium)
- **Output:**
  ```
  TOP ANOMALIES:
  [A1] Pattern: Staff cut 13.54% while operations grew
       Unusualness: 8/10
       Why surprising: Typically growth requires more staff
  
  [A2] Pattern: Energy consumption up 13% during "efficiency" period
       Unusualness: 7/10
       Why surprising: Contradicts efficiency narrative
  ```
- **Saved as:** `discovery_pipeline/stage_3_anomalies.txt`

### Sub-call 4.4: Investigate Anomalies
- **Input:** 
  - Anomalies from 4.3
  - Original enhanced draft from Step 3
- **Temperature:** 0.6 (medium)
- **Output:**
  ```
  INVESTIGATION RESULTS:
  [A1] Investigation:
  - Root cause: Automation and digital transformation
  - Related patterns: Digital adoption metrics up
  - Management position: "Strategic efficiency drive"
  - Significance: Major cost structure improvement
  ```
- **Saved as:** `discovery_pipeline/stage_4_investigations.txt`

### Sub-call 4.5: Assess Impact
- **Input:**
  - Investigations from 4.4
  - Section context
- **Temperature:** 0.6 (medium)
- **Output:**
  ```
  IMPACT ASSESSMENT:
  [A1] Business Impact:
  - Quantified impact: 4.06% opex reduction
  - Investment thesis impact: HIGH - structural margin improvement
  - Structural implication: Advantage - scalability improved
  - Materiality: Adds 200-300bps to EBITDA margin
  ```
- **Saved as:** `discovery_pipeline/stage_5_impacts.txt`

### Sub-call 4.6: Generate Final Insight
- **Input:**
  - Impact assessments from 4.5
  - Section requirements
- **Temperature:** 0.6 (medium)
- **Output:** Final <500 word synthesis
- **Saved as:** `discovery_pipeline/stage_6_final_insight.md` & `step_4_final_section.md`

---

## Step 5: Learning Extraction
**LLM Call #10** (`extract_learning`)
- **Input:** Final output from Step 4
- **Temperature:** 0.2 (low)
- **Output:** JSON with analytical methodologies (not company-specific findings)
- **Saved as:** `step_5_learning.json`

---

## Post-Processing
- Memory review (if not in test mode) to incorporate new methodologies
- HTML generation combining all sections

---

## Key Issues Identified

1. **Step 3 contamination** - Sometimes adds meta-commentary instead of clean output
2. **Context redundancy** - Full document context passed multiple times
3. **Information loss** - Each stage only sees output from previous stage, not original data
4. **Temperature choices** - All discovery stages use 0.6, might need variation

## Summary
The complete workflow uses 10 LLM calls per section:
- 3 calls for initial draft and enhancement (Steps 1-3)
- 6 calls for discovery pipeline (Step 4)
- 1 call for learning extraction (Step 5)

The discovery pipeline is finding good insights but with 10 LLM calls per section, there's room for optimization.
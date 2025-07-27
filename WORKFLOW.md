# Complete Section Analysis Workflow

## Pre-Step: Document Loading
- PDFs converted to markdown → stored in `full_context` (entire document corpus)
- User selects sections to analyze and chooses discovery pipeline option

---

## Base Workflow (Discovery Pipeline OFF)

### Step 1: Initial Draft
**LLM Call #1** (`create_initial_draft`)
- **Input:**
  - Section requirements/specs
  - Relevant memory (analytical methodologies from previous runs)
  - Full document context (`full_context`)
- **Temperature:** 0.6 (medium)
- **Word Target:** ~1000 words
- **Special Handling:** Section 32 uses specialized data-only prompt
- **Output:** Initial draft with facts and analysis
- **Saved as:** `step_1_initial_draft.md`

### Step 2: Completeness Check
**LLM Call #2** (`completeness_check`)
- **Input:**
  - Section requirements/specs
  - Full document context (`full_context`)
  - Current draft from Step 1
- **Temperature:** 0.2 (low)
- **Output:** ADD list of missing data items (max 5 suggestions)
- **Saved as:** `step_2_add_list.txt`
- **Note:** Skipped for Section 32

### Step 3: Apply Completeness
**LLM Call #3** (`apply_completeness_only`)
- **Input:**
  - Current draft from Step 1
  - ADD list from Step 2
  - Full document context (to look up the missing data)
- **Temperature:** 0.6 (medium)
- **Output:** Enhanced draft with added data (~1000+ words)
- **Saved as:** `step_3_improved_draft.md`
- **Note:** Skipped for Section 32

### Step 4: Deep Analysis and Polish
**LLM Call #4** (`original_deep_analysis`)
- **Input:**
  - Enhanced draft from Step 3
  - Section requirements/specs
- **Temperature:** 0.6 (medium)
- **Word Target:** Maximum 500 words
- **Focus:** Condense to essential elements while preserving:
  - Tables (especially for financial sections)
  - Section-relevant content
  - Value creation insights
- **Output:** Polished, condensed analysis
- **Saved as:** `step_4_final_section.md`
- **Note:** Skipped for Section 32

### Step 6: Learning Extraction
**LLM Call #5** (`extract_learning`)
- **Input:** Final output from Step 4
- **Temperature:** 0.2 (low)
- **Output:** JSON with analytical methodologies
- **Saved as:** `step_6_learning.json`
- **Note:** Skipped for Section 32

**Total LLM Calls: 5 per section (1 for Section 32)**

---

## Enhanced Workflow (Discovery Pipeline ON)

### Steps 1-4: Same as Base Workflow
- Initial Draft → Completeness Check → Apply Additions → Deep Analysis
- Results in `step_4_final_section.md`

### Step 5: Discovery Pipeline Augmentation
Runs 6-stage discovery pipeline on Step 3 draft, then augments Step 4 output

#### Stage 1: Extract All Data
**LLM Call #5** (`_extract_all_data`)
- **Input:** Step 3 improved draft
- **Temperature:** 0.2 (low)
- **Output:** Claims and quantifiable facts
- **Saved as:** `discovery_pipeline/stage_1_extracted_data.txt`

#### Stage 2: Calculate Material Relationships
**LLM Call #6** (`_calculate_material_relationships`)
- **Input:** Extracted data from Stage 1
- **Temperature:** 0.6 (medium)
- **Focus:** Relationships with >10% changes, claim validation
- **Output:** Calculated relationships and claim-fact matches
- **Saved as:** `discovery_pipeline/stage_2_relationships.txt`

#### Stage 3: Identify Anomalies
**LLM Call #7** (`_identify_anomalies`)
- **Input:** Relationships from Stage 2
- **Temperature:** 0.6 (medium)
- **Focus:** Top 2-3 patterns affecting value creation
- **Output:** Anomalies with value impact assessment
- **Saved as:** `discovery_pipeline/stage_3_anomalies.txt`

#### Stage 4: Investigate Anomalies
**LLM Call #8** (`_investigate_anomalies`)
- **Input:** 
  - Anomalies from Stage 3
  - Original Step 3 draft
- **Temperature:** 0.6 (medium)
- **Output:** Root causes and business implications
- **Saved as:** `discovery_pipeline/stage_4_investigations.txt`

#### Stage 5: Assess Impact
**LLM Call #9** (`_assess_impact`)
- **Input:**
  - Investigations from Stage 4
  - Section context
- **Temperature:** 0.6 (medium)
- **Output:** Quantified business impact assessments
- **Saved as:** `discovery_pipeline/stage_5_impacts.txt`

#### Stage 6: Generate Discovery Insight
**LLM Call #10** (`_generate_insight_comprehensive`)
- **Input:**
  - All previous discovery stages
  - Section requirements
- **Temperature:** 0.6 (medium)
- **Word Target:** Maximum 500 words
- **Output:** Fact-dense value-focused analysis
- **Saved as:** `discovery_pipeline/stage_6_final_insight.md`

#### Final Augmentation (2 sub-calls)

**LLM Call #11** (`_analyze_insights_for_augmentation`)
- **Input:**
  - Step 4 output (base)
  - Discovery insights
  - Section context
- **Temperature:** 0.2 (low)
- **Purpose:** Pre-filter insights for materiality
- **Criteria:**
  - NOT already in base output
  - Would affect investment decisions
  - Non-obvious value drivers/risks
  - Can be stated in one sentence
- **Output:** Critical insights with exact insertion points

**LLM Call #12** (`_intelligent_augmentation`)
- **Input:**
  - Step 4 output (base output to preserve)
  - Discovery insights from Stage 6
  - Pre-analyzed critical insights from Call #11
  - Section context
- **Temperature:** 0.6 (medium)
- **Rules:**
  - Preserve 100% of Step 4 content
  - Maximum 2-3 insertions total
  - Single sentence insertions only
  - Insert immediately after related content
  - Use transition phrases
  - Total additions under 100 words
- **Output:** Augmented analysis with critical insights surgically inserted
- **Saved as:** `step_5_discovery_augmented.md`

### Step 6: Learning Extraction
**LLM Call #13** (`extract_learning`)
- **Input:** Final augmented output
- **Temperature:** 0.2 (low)
- **Output:** JSON with analytical methodologies
- **Saved as:** `step_6_learning.json`

**Total LLM Calls: 13 per section with discovery pipeline**

---

## Augmentation Quality Control

The discovery pipeline augmentation uses a two-stage process to ensure only critical insights are added:

### Stage 1: Pre-Analysis Filter
- Identifies which discovery insights are worth adding
- Must pass ALL criteria:
  - Not already covered in base output
  - Would materially affect investment decisions  
  - Reveals non-obvious value drivers or risks
  - Can be stated in one impactful sentence

### Stage 2: Surgical Insertion
- Maximum 2-3 insertions per section
- Single sentences only
- Inserted immediately after related content
- Must quantify business impact
- Example: "Notably, this 13.5% headcount reduction coincided with 18% revenue/employee growth, suggesting successful automation worth 200bps in margin expansion."

### What Gets Rejected
- Academic observations
- Obvious patterns
- Insights without quantified business impact
- Anything that doesn't affect cash generation or competitive position

---

## Post-Processing
1. **Memory Review** (if not in test mode)
   - Reviews all learning JSON files
   - Updates `learning_memory.json` with high-quality insights
   
2. **Profile Generation**
   - Collects final outputs (prioritizes discovery augmented if available)
   - Applies footnote management (max 5 per section)
   - Fixes malformed markdown tables
   - Generates HTML with cover page and TOC

---

## File Structure Per Section
```
section_XX/
├── step_1_initial_draft.md
├── step_2_add_list.txt (skipped for Section 32)
├── step_3_improved_draft.md (skipped for Section 32)
├── step_4_final_section.md
├── step_5_discovery_augmented.md (only if discovery ON)
├── step_6_learning.json
└── discovery_pipeline/ (only if discovery ON)
    ├── stage_1_extracted_data.txt
    ├── stage_2_relationships.txt
    ├── stage_3_anomalies.txt
    ├── stage_4_investigations.txt
    ├── stage_5_impacts.txt
    └── stage_6_final_insight.md
```

---

## Key Features

### Parallel Processing
- Supports 1-3 workers for concurrent section analysis
- Automatic retry with exponential backoff for rate limits
- Section context included in rate limit messages

### Quality Controls
- Footnote management (max 5 per section, sequential numbering)
- Malformed table detection and correction
- Word count targets at each stage
- Section requirement adherence

### Special Handling
- **Section 32 (Data Appendix)**: 
  - Skips Steps 2-4
  - Skips Step 5 (Discovery Pipeline) - never runs discovery on data appendix
  - Limited to 15-20 most important tables
  - No analysis or insights, data only
  - Only runs Step 1 (data extraction) and Step 6 (learning extraction)

### Memory System
- Captures analytical methodologies (not company-specific data)
- Two-stage quality filtering
- Section-based storage with quality scores

---

## Summary
- **Base workflow**: 5 LLM calls per section
- **With discovery**: 13 LLM calls per section (includes pre-analysis)
- **Discovery advantage**: Finds non-obvious patterns and anomalies
- **Augmentation approach**: 
  - Two-stage process: analyze then augment
  - Maximum 2-3 insertions per section
  - Single sentences only, surgically placed
  - Must pass materiality test
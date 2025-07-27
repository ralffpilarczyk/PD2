import google.generativeai as genai
from typing import Dict
import re
import os
from datetime import datetime
from .utils import retry_with_backoff
from .profile_sections import sections


class CoreAnalyzer:
    """Handles the core analysis pipeline - drafts, critiques, and learning extraction"""
    
    # Progressive word limit configuration
    INITIAL_WORDS = 1000      # Initial draft + completeness critique target
    INSIGHT_WORDS = 500       # Insight critique target - distill key patterns
    POLISH_WORDS = 300        # Polish critique target - final concise output
    SECTION_32_EXEMPT = 32    # Section number that is exempt from word limits
    
    # Temperature configuration for different cognitive phases
    LOW_TEMP = 0.2           # Systematic, methodical analysis (completeness)
    MEDIUM_TEMP = 0.6        # Balanced refinement (polish, initial draft)
    HIGH_TEMP = 0.9          # Creative breakthrough thinking (insights) - reduced to avoid safety blocks
    
    def __init__(self, full_context: str, use_discovery_pipeline: bool = False, run_timestamp: str = None):
        """Initialize core analyzer with document context
        
        Args:
            full_context: The full document context to analyze
            use_discovery_pipeline: Whether to use the 6-stage discovery pipeline (default: False)
            run_timestamp: Optional run timestamp for organizing outputs
        """
        self.full_context = full_context
        
        # Toggle for discovery pipeline vs original deep analysis
        self.use_discovery_pipeline = use_discovery_pipeline
        
        # Run timestamp for organizing outputs
        self.run_timestamp = run_timestamp or datetime.now().strftime('%Y_%m_%d_%H_%M_%S')
        
        # Create models with different temperatures for different cognitive phases
        self.model_low_temp = genai.GenerativeModel(
            'gemini-2.5-flash',
            generation_config=genai.types.GenerationConfig(temperature=self.LOW_TEMP)
        )
        self.model_medium_temp = genai.GenerativeModel(
            'gemini-2.5-flash', 
            generation_config=genai.types.GenerationConfig(temperature=self.MEDIUM_TEMP)
        )
        self.model_high_temp = genai.GenerativeModel(
            'gemini-2.5-flash',
            generation_config=genai.types.GenerationConfig(temperature=self.HIGH_TEMP)
        )
    
    def create_initial_draft(self, section: Dict, relevant_memory: str) -> str:
        """Step 1: Create a disciplined initial draft."""
        
        if section['number'] == self.SECTION_32_EXEMPT:
            # This prompt is highly specific and should remain as is.
            prompt = f"""You are a data organizer. Your only job is to create a comprehensive data appendix.

SECTION {section['number']}: {section['title']}

REQUIREMENTS:
{section['specs']}

DOCUMENTS:
{self.full_context}

APPENDIX OUTPUT REQUIREMENTS:
1. Extract the MOST IMPORTANT tables and structured data from the documents.
2. Prioritize: Financial statements, segment data, operational metrics, key ratios
3. Organize tables logically under clear Markdown headers.
4. Format tables correctly in Markdown.
5. Include clear source references for each table using [1], [2], [3] footnote format.

SIZE CONSTRAINTS:
- Focus on the 15-20 most important tables
- For very large tables, include key sections or summaries
- Ensure output remains under 150KB total

CRITICAL RESTRICTIONS:
- DO NOT write any analysis, insights, commentary, or explanatory text.
- DO NOT write executive summaries or narrative sections.
- The output must contain ONLY: Markdown headers for table groups, the tables themselves, and footnotes.

This is a data-only section. Any narrative text will be removed.
"""
        else:
            memory_instructions = f"\n\nANALYTICAL METHODOLOGY (apply these proven techniques):\n{relevant_memory}" if relevant_memory.strip() else ""
            
            prompt = f"""You are an expert business analyst creating a DISCIPLINED initial draft.

SECTION {section['number']}: {section['title']}

REQUIREMENTS:
{section['specs']}{memory_instructions}

DOCUMENTS:
{self.full_context}

DRAFTING INSTRUCTIONS:
1.  **Word Count:** Target a DRAFT of approximately **{self.INITIAL_WORDS} words**. This is a draft, but it must be focused.
2.  **Scope Adherence:** Adhere STRICTLY to the section requirements. Do NOT include information that belongs in other sections.
3.  **Analytical Methods:** If analytical methodology is provided above, apply those proven techniques to this analysis.
4.  **Data First:** Extract all relevant data points first, with precise citations.
5.  **Concise Analysis:** Provide brief analysis connecting the data points. Avoid long, speculative paragraphs.
6.  **Footnote Discipline:** Use exactly 5 footnotes maximum. Select the 5 most important data points to cite. Use [1], [2], [3], [4], [5] format. No letters in footnotes.
7.  **Tables:** Include at least one small, well-formatted Markdown table with the most critical data.

Your goal is to create a strong, fact-based draft that applies proven analytical techniques and is well-structured within the target word count.
"""
        
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context=section['number']
        )

    def deep_analysis_and_polish(self, section: Dict, comprehensive_draft: str) -> str:
        """Step 4: Apply deep analysis methodology and polish to final output."""
        
        word_count = len(comprehensive_draft.split())
        
        # Skip for Section 32
        if section['number'] == self.SECTION_32_EXEMPT:
            return comprehensive_draft
        
        # Always use original approach for Step 4
        # Discovery pipeline will be used as augmentation in PD2.py if enabled
        return self.original_deep_analysis(section, comprehensive_draft)
    
    def original_deep_analysis(self, section: Dict, comprehensive_draft: str) -> str:
        """Original deep analysis approach - condense while preserving section-relevant content."""
        word_count = len(comprehensive_draft.split())
        
        prompt = f"""You are an expert analyst condensing content to its most essential elements.

COMPREHENSIVE DRAFT:
---
{comprehensive_draft}
---

SECTION {section['number']}: {section['title']}
SECTION REQUIREMENTS:
{section['specs']}

CURRENT WORD COUNT: {word_count} words
FINAL TARGET: **Maximum 500 words - absolutely no exceptions.**

CONDENSING INSTRUCTIONS:

1. **FOLLOW THE SECTION REQUIREMENTS EXACTLY** - The section specs above define what belongs in this section. Follow them precisely.

2. **RELEVANCE FILTER** - Keep only content that meets BOTH criteria:
   - Directly relevant to the section topic and requirements
   - Affects company prospects or value creation

3. **TABLE PRESERVATION**:
   - If the draft contains tables, KEEP at least one summary table
   - For financial sections, tables are essential - preserve key metrics in table format
   - If no tables exist but data would be clearer in a table, create one

4. **DATA DENSITY**:
   - Preserve specific numbers, percentages, and trends
   - Keep year-over-year comparisons and growth rates
   - Maintain factual density while cutting descriptive text

5. **SMART CONDENSING**:
   - Remove generic statements and obvious observations
   - Cut repetitive points - state each insight once
   - Eliminate elaborate explanations - let the data speak
   - Focus on what's surprising, notable, or value-affecting

WHAT TO PRESERVE:
- Tables (especially for financial data)
- Specific metrics and quantified trends
- Key comparisons and benchmarks
- Unusual patterns or anomalies
- Content that directly fulfills section requirements

WHAT TO REMOVE:
- Generic framework language (competitive moat, etc.) unless specifically relevant
- Repetitive points
- Obvious statements
- Elaborate explanations of simple facts

CONSTRAINTS:
- Maximum 500 words total
- Maximum 5 footnotes [1], [2], [3], [4], [5]
- At least one table where appropriate
- Every sentence must add value

Generate the condensed version that respects the section's specific focus."""

        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context=section['number']
        )
    
    def discovery_pipeline_analysis(self, section: Dict, comprehensive_draft: str) -> str:
        """6-stage discovery pipeline for deep insight generation."""
        print(f"Starting discovery pipeline for Section {section['number']}")
        
        # Create directory for pipeline outputs
        pipeline_dir = f"runs/run_{self.run_timestamp}/section_{section['number']}/discovery_pipeline"
        os.makedirs(pipeline_dir, exist_ok=True)
        
        # Stage 1: Extract all data
        print(f"  Stage 1: Extracting data...")
        extracted_data = self._extract_all_data(comprehensive_draft, section['number'])
        self._save_stage_output(pipeline_dir, "stage_1_extracted_data.txt", extracted_data)
        
        # Stage 2: Calculate material relationships
        print(f"  Stage 2: Calculating relationships...")
        relationships = self._calculate_material_relationships(extracted_data, section['number'])
        self._save_stage_output(pipeline_dir, "stage_2_relationships.txt", relationships)
        
        # Stage 3: Identify anomalies
        print(f"  Stage 3: Identifying anomalies...")
        anomalies = self._identify_anomalies(relationships, section['number'])
        self._save_stage_output(pipeline_dir, "stage_3_anomalies.txt", anomalies)
        
        # Stage 4: Investigate top anomalies
        print(f"  Stage 4: Investigating anomalies...")
        investigations = self._investigate_anomalies(anomalies, comprehensive_draft, section['number'])
        self._save_stage_output(pipeline_dir, "stage_4_investigations.txt", investigations)
        
        # Stage 5: Assess impact
        print(f"  Stage 5: Assessing impact...")
        impacts = self._assess_impact(investigations, section)
        self._save_stage_output(pipeline_dir, "stage_5_impacts.txt", impacts)
        
        # Stage 6: Generate final insight with comprehensive data
        print(f"  Stage 6: Generating final insight with full fact register...")
        comprehensive_data = {
            'extracted_facts': extracted_data,
            'relationships': relationships,
            'anomalies': anomalies,
            'investigations': investigations,
            'impacts': impacts,
            'original_draft': comprehensive_draft  # Add original draft with footnotes
        }
        final_insight = self._generate_insight_comprehensive(comprehensive_data, section)
        self._save_stage_output(pipeline_dir, "stage_6_final_insight.md", final_insight)
        
        print(f"Discovery pipeline complete for Section {section['number']}")
        return final_insight
    
    def augment_with_discovery(self, section: Dict, step3_draft: str, step4_output: str) -> str:
        """Augment Step 4 output with insights from discovery pipeline."""
        print(f"Running discovery pipeline augmentation for Section {section['number']}")
        
        # Run discovery pipeline on Step 3 draft to find insights
        discovery_insights = self.discovery_pipeline_analysis(section, step3_draft)
        
        # Now augment Step 4 with any missing insights
        augmented_output = self._intelligent_augmentation(step4_output, discovery_insights, section)
        
        return augmented_output
    
    def _intelligent_augmentation(self, base_output: str, discovery_insights: str, section: Dict) -> str:
        """Intelligently merge discovery insights into base output without removing content."""
        
        # First, analyze what insights are worth adding
        insight_analysis = self._analyze_insights_for_augmentation(base_output, discovery_insights, section)
        
        # Then perform targeted augmentation
        prompt = f"""You are an expert editor performing SURGICAL insertion of critical business insights.

BASE OUTPUT (Step 4 - PRESERVE 100%):
---
{base_output}
---

DISCOVERY INSIGHTS:
---
{discovery_insights}
---

INSIGHTS TO ADD (from analysis):
---
{insight_analysis}
---

SECTION: {section['title']}

CRITICAL INSERTION RULES:

1. **MATERIALITY TEST**: Only add insights that would change an investment decision
   - Must reveal hidden value drivers or risks
   - Must quantify impact on margins, returns, or growth
   - Must expose contradictions with material implications

2. **PLACEMENT PRECISION**:
   - Insert IMMEDIATELY after the most related sentence in base output
   - Example: If base mentions "headcount reduced 10%", insert efficiency insight right after
   - Never append to end of paragraphs - find the exact contextual location
   - Use transition phrases: "Notably," "However," "This coincides with," "Despite this,"

3. **BUSINESS RELEVANCE FILTER**:
   - No academic observations
   - No obvious patterns
   - Only counterintuitive findings with business impact
   - Must answer: "So what? How does this affect cash generation or competitive position?"

4. **INSERTION FORMAT**:
   - One sentence maximum per insight
   - Include the specific calculation or anomaly
   - State the business implication clearly
   - Example: "Notably, this 13.5% headcount reduction coincided with 18% revenue/employee growth, suggesting successful automation worth 200bps in margin expansion."

5. **BRUTAL SELECTION**:
   - Maximum 2-3 insertions total
   - If discovery found 10 insights, pick only the 2-3 that most affect value
   - Better to add nothing than to add marginal insights

CONSTRAINTS:
- Base output must remain 100% intact
- Insertions must be single sentences at precise locations
- No new paragraphs or sections
- Maintain existing footnotes
- Total additions under 100 words

Generate the augmented output with ONLY critical insights surgically inserted at optimal locations."""

        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context=section['number']
        )
    
    def _analyze_insights_for_augmentation(self, base_output: str, discovery_insights: str, section: Dict) -> str:
        """Pre-analyze discovery insights to identify which are worth adding."""
        analysis_prompt = f"""Analyze which discovery insights are CRITICAL enough to add to the base output.

BASE OUTPUT (current analysis):
---
{base_output}
---

DISCOVERY INSIGHTS (potential additions):
---
{discovery_insights}
---

SECTION CONTEXT: {section['title']}

Identify ONLY insights that meet ALL these criteria:
1. NOT already covered in the base output (even partially)
2. Would materially affect investment decisions
3. Reveals non-obvious value drivers or risks
4. Can be stated in one impactful sentence

For each qualifying insight:
- State the insight concisely
- Identify the EXACT sentence in base output after which it should be inserted
- Explain why it's material enough to include

If NO insights meet all criteria, state "No critical insights to add."

Be extremely selective - only truly game-changing insights."""

        return retry_with_backoff(
            lambda: self.model_low_temp.generate_content(analysis_prompt).text,
            context=section['number']
        )
    
    def _save_stage_output(self, directory: str, filename: str, content: str):
        """Save intermediate stage output for debugging."""
        filepath = os.path.join(directory, filename)
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(content)
    
    def _extract_all_data(self, draft: str, section_num: int = None) -> str:
        """Stage 1: Extract claims, statements, and quantifiable facts."""
        prompt = f"""Extract all quantifiable information from this document.
Include claims, statements, and measurable facts.
Note: Source documents may have footnotes like [31], [32] but ignore the numbers - just extract the facts.

DOCUMENT:
---
{draft}
---

Output format:
CLAIMS AND STATEMENTS:
- [C1] "exact quote or paraphrase of claim"
- [C2] "another claim"

QUANTIFIABLE FACTS:
- [F1] metric_name: value unit (time_period) [change if mentioned]
- [F2] another_metric: value unit (time_period)

Example:
- [F1] Total Mobile Subscriptions: 12,753,000 (2Q24)
- [F2] Blended Data Usage: 33.2 GB/month (1Q25)

Extract EVERYTHING quantifiable. Make no judgments about relevance.
Do NOT include footnote numbers from the source - just extract the facts themselves."""
        
        return retry_with_backoff(
            lambda: self.model_low_temp.generate_content(prompt).text,
            context=section_num
        )
    
    def _calculate_material_relationships(self, extracted_data: str, section_num: int = None) -> str:
        """Stage 2: Calculate relationships that are likely to be meaningful."""
        prompt = f"""Calculate relationships using these criteria:

EXTRACTED DATA:
---
{extracted_data}
---

Calculate relationships using these filters:

1. MAGNITUDE FILTER: Only examine relationships where:
   - Any metric changed >10% period-over-period
   - Any metric differs >20% from others in its category
   - Any absolute value represents >5% of a total

2. CLAIM TESTING: For every claim made, calculate:
   - Metrics that would validate/invalidate the claim
   - Ratios that test the claim's credibility

3. TEMPORAL PATTERNS: Calculate relationships for:
   - Metrics showing opposing trends
   - Metrics that should logically move together but don't
   - Sudden changes in established patterns

4. CATEGORY COHERENCE: Within logical groupings:
   - Compare metrics that should correlate
   - Calculate efficiency ratios within categories
   - Identify outliers within peer groups

Skip relationships between:
- Metrics with <5% changes
- Unrelated categories (unless specific reason)
- Administrative or minor operational metrics

Output format:
CALCULATED RELATIONSHIPS:
- [R1] metric_a / metric_b = value (interpretation)
- [R2] metric_c changed X% while metric_d changed Y% (interpretation)

CLAIM-FACT MATCHES:
- [M1] Claim [C#] is SUPPORTED/CONTRADICTED by Fact [F#] because...

Focus on relationships that could affect business prospects."""
        
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context=section_num
        )
    
    def _identify_anomalies(self, relationships: str, section_num: int = None) -> str:
        """Stage 3: Identify top 2-3 anomalous patterns with value creation focus."""
        prompt = f"""Given these relationships:
---
{relationships}
---

Identify the TOP 2-3 patterns that could most affect VALUE CREATION through:
- Cash generation capacity
- Return on capital employed
- Competitive positioning
- Business model scalability

Selection criteria for anomalies:
1. Patterns that affect margins (revenue/cost relationships)
2. Capital efficiency contradictions (asset utilization anomalies)
3. Scalability indicators (unit economics changes)
4. Competitive moat factors (market position shifts)

For each anomaly:
- Assign an ID [A1], [A2], [A3]
- Describe the unexpected pattern with numbers
- Estimate potential value impact (how it affects cash flows or returns)
- Explain why this matters to the business model

Output format:
TOP ANOMALIES:
[A1] Pattern: [description with specific numbers]
     Value Impact: [estimated effect on margins, returns, or growth]
     Why it matters: [connection to cash generation or competitive position]

[A2] Pattern: [description with specific numbers]
     Value Impact: [estimated effect on margins, returns, or growth]
     Why it matters: [connection to cash generation or competitive position]

Select ONLY anomalies that demonstrably affect value creation. Ignore patterns that don't impact cash flows, returns, or competitive advantage."""
        
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context=section_num
        )
    
    def _investigate_anomalies(self, anomalies: str, original_draft: str, section_num: int = None) -> str:
        """Stage 4: Deep investigation of anomalies."""
        prompt = f"""Investigate these anomalies by searching the original document:

ANOMALIES TO INVESTIGATE:
---
{anomalies}
---

ORIGINAL DOCUMENT:
---
{original_draft}
---

For each anomaly, search for:
1. Any explanation for why this anomaly exists
2. Related data that might explain the pattern
3. Similar patterns in other metrics
4. Management commentary about this area
5. Additional context that makes the anomaly more/less significant

Output format:
INVESTIGATION RESULTS:
[A1] Investigation:
- Root cause: [finding]
- Related patterns: [other similar anomalies]
- Management position: [what they say about this]
- Significance: [why this matters]

[A2] Investigation:
[similar structure]

Goal: Understand the business implications of each anomaly."""
        
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context=section_num
        )
    
    def _assess_impact(self, investigations: str, section: Dict) -> str:
        """Stage 5: Assess impact on company prospects."""
        prompt = f"""Assess the impact of these findings on company prospects:

INVESTIGATION RESULTS:
---
{investigations}
---

SECTION CONTEXT: {section['title']}

For each finding:
1. Quantify the potential impact on company performance
2. Assess whether this would change an investor's thesis
3. Determine if this reveals structural advantages or vulnerabilities
4. Calculate materiality (% impact on relevant metrics)

Output format:
IMPACT ASSESSMENT:
[A1] Business Impact:
- Quantified impact: [specific metrics and magnitude]
- Investment thesis impact: [HIGH/MEDIUM/LOW with explanation]
- Structural implication: [advantage/vulnerability/neutral]
- Materiality: [calculation]

[A2] Business Impact:
[similar structure]

Focus on impacts that would influence investment decisions."""
        
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context=section['number']
        )
    
    def _generate_insight_comprehensive(self, comprehensive_data: dict, section: Dict) -> str:
        """Stage 6: Generate final insight synthesis with complete fact register."""
        prompt = f"""Convert these comprehensive findings into a fact-dense investment-grade analysis.

COMPLETE DISCOVERY DATA:

EXTRACTED FACTS:
---
{comprehensive_data['extracted_facts']}
---

CALCULATED RELATIONSHIPS:
---
{comprehensive_data['relationships']}
---

DISCOVERED ANOMALIES:
---
{comprehensive_data['anomalies']}
---

INVESTIGATION RESULTS:
---
{comprehensive_data['investigations']}
---

IMPACT ASSESSMENTS:
---
{comprehensive_data['impacts']}
---

ORIGINAL DRAFT WITH FOOTNOTES (for citation reference):
---
{comprehensive_data.get('original_draft', '')}
---

SECTION: {section['title']}
REQUIREMENTS: {section['specs']}

CREATE A FACT-DENSE, VALUE-FOCUSED ANALYSIS:

VALUE CREATION FILTER:
Every insight must demonstrate quantified impact on:
- Cash generation (margins, working capital, capex efficiency)
- Return on capital (asset productivity, capital intensity)
- Competitive advantage (market share, pricing power, cost position)
- Business scalability (unit economics, operational leverage)

If an anomaly doesn't affect these value drivers, exclude it entirely.

REQUIREMENTS:

1. **Lead with value impact**: Start each paragraph with calculations that affect cash flows or returns
   Example: "Capex fell 49% (RM675M→RM344M) while revenue grew 3.6%, improving FCF by RM330M"

2. **Quantify value creation**: Show exactly how anomalies affect margins, returns, or growth
   Example: "13.54% headcount reduction saves RM50M annually, adding 200bps to EBITDA margin"

3. **Connect to business model**: Every contradiction must link to fundamental value equation
   Example: "Energy cost rising 13% while revenue/employee up 18% still yields net 5% productivity gain"

4. **Ignore non-value factors**: Exclude any pattern that doesn't affect cash generation or competitive position
   No gender ratios, training hours, or CSR metrics unless they quantifiably impact returns

5. **Maintain fact density**: 3-5 value-relevant numbers per paragraph

6. **Direct language**: Write for a hedge fund analyst who only cares about returns

STRUCTURE:
- Opening: Core operational metrics that drive the business model
- Body: 2-3 insights showing how anomalies affect value creation
- Each paragraph must calculate impact on margins, returns, or competitive position
- Maximum 500 words total

EXAMPLE PATTERN:
"Maxis operates X sites with Y employees generating Z output. [Anomaly 1 with calculations showing contradiction]. This reveals [specific business risk/advantage with quantified impact]. [Anomaly 2 with different calculations]. Together these patterns indicate [synthesized conclusion with numbers]."

FOOTNOTE REQUIREMENTS:
- Use exactly 5 footnotes maximum for the entire section
- Select ONLY the 5 most important/material data points to cite
- Use sequential numbering: [1], [2], [3], [4], [5]
- No letters in footnotes (not [a], [b] or [1a], [1b])
- Place footnote section at the end with source citations from the original draft

Generate the final fact-dense analytical output with proper footnote citations."""
        
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context=section['number']
        )
    
    def _generate_insight(self, impacts: str, section: Dict) -> str:
        """Stage 6: Generate final insight synthesis (legacy version)."""
        prompt = f"""Convert these analyzed findings into a concise investment-grade insight:

IMPACT ASSESSMENTS:
---
{impacts}
---

SECTION: {section['title']}
REQUIREMENTS: {section['specs']}

Create a final analysis that:
1. Starts with one sentence stating what the company does/has in this area
2. Presents 2-3 discovered insights based on the anomaly analysis
3. Quantifies all statements with specific numbers
4. Explains business implications clearly
5. Uses zero corporate jargon
6. Stays under 500 words total

Structure the output naturally - don't use rigid headings, but ensure you cover:
- The surprising relationships discovered
- What these reveal about the business
- Why they matter to investors
- Specific risks or advantages uncovered

Every sentence must contain actionable intelligence.
Use exactly 5 footnotes maximum. Sequential numbering only: [1], [2], [3], [4], [5]. No letters.

Generate the final analytical output."""
        
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context=section['number']
        )

    def apply_critique(self, section: Dict, current_draft: str, critique: str, critique_type: str) -> str:
        """Applies a critique to a draft to generate a revised version."""
        
        word_target = self.POLISH_WORDS
        
        prompt = f"""You are an intelligent editor that flawlessly applies instructions to a text.

ORIGINAL DRAFT:
---
{current_draft}
---

INSTRUCTIONS TO APPLY:
---
{critique}
---

Based on the instructions, generate the new, revised version of the text.

CRITICAL FINAL CHECKS:
- **Word Count:** The final output MUST be under {word_target} words.
- **Footnote Rules:** Maximum 5 footnotes. Sequential numbering only: [1], [2], [3], [4], [5]. No letters. If draft has more than 5, keep only the 5 most important.
- **Scope:** All out-of-scope content mentioned in the instructions must be removed.

Produce only the final, revised Markdown text.
"""
        # Medium temperature to apply edits intelligently without going off-track.
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context=section['number']
        )

    def extract_learning(self, section: Dict, final_output: str) -> str:
        """Extract analytical methodologies and frameworks that can improve future analysis."""
        
        prompt = f"""You are a methodology curator. Extract analytical techniques and frameworks from this analysis that could improve future analysis of ANY company.

SECTION TYPE: {section['title']}

COMPLETED ANALYSIS:
---
{final_output}
---

Extract transferable analytical methodologies, not company-specific findings.

METHODOLOGY EXTRACTION:
1. **Analytical Techniques**: What calculation methods, ratio analysis, or data relationships proved most revealing?
2. **Red Flag Patterns**: What general warning signs or contradiction patterns were identified?  
3. **Data Quality Checks**: What validation methods or cross-references were most effective?

OUTPUT FORMAT (JSON):
{{
  "analytical_techniques": [
    "When analyzing [context], calculate [specific ratio/metric] to reveal [insight type]",
    "Look for [pattern] in [data type] to identify [risk/opportunity]"
  ],
  "red_flag_patterns": [
    "Warning sign: [general pattern] often indicates [business risk]",
    "Contradiction: [claim type] vs [data type] reveals [underlying issue]"
  ],
  "data_validation": [
    "Cross-check [data source A] against [data source B] to verify [accuracy]",
    "Calculate [derived metric] to validate [reported claim]"
  ]
}}

RULES:
- Focus on methodology, not specific company details
- Each technique should apply across multiple companies/industries
- Be specific about analytical steps and calculations
- Maximum 2 items per category - focus on most valuable methods

Extract only the most transferable and valuable analytical approaches."""
        # Low temperature for structured, precise data extraction.
        return retry_with_backoff(
            lambda: self.model_low_temp.generate_content(prompt).text,
            context=section['number']
        )
    
    def completeness_check(self, section: Dict, draft: str) -> str:
        """Step 2: Check what's missing from the draft by comparing against source documents."""
        
        word_count = len(draft.split())
        
        # Skip for Section 32
        if section['number'] == self.SECTION_32_EXEMPT:
            return "No additions needed - data appendix section."
        
        prompt = f"""You are a meticulous completeness auditor focused on SECTION RELEVANCE. Your job is to identify missing data that belongs specifically in this section.

SECTION {section['number']}: {section['title']}

REQUIREMENTS:
{section['specs']}

SOURCE DOCUMENTS:
{self.full_context}

CURRENT DRAFT:
---
{draft}
---

WORD COUNT: {word_count} words

CRITICAL: Only suggest additions that directly fulfill the section requirements above. Do NOT suggest data that belongs in other sections.

MATERIALITY FILTER:
Each addition must pass: "Does this data point directly address the section requirements AND would it matter to company prospects?"

ADD LIST FORMAT:
- [CRITICAL] Specific missing data that directly fulfills section requirements (include exact source)
- [IMPORTANT] Relevant data that would improve section completeness (include exact source)
- [USEFUL] Supporting data that belongs specifically in this section (include exact source)

STRICT RULES:
1. Only suggest data that explicitly belongs in THIS section per the requirements
2. Be EXTREMELY specific - include exact data point and source location  
3. Each item must directly address the section scope
4. Focus on quantified, factual data only
5. Maximum 5 suggestions - focus on the most important gaps only

Output ONLY the ADD list. No preamble or explanation."""
        
        return retry_with_backoff(
            lambda: self.model_low_temp.generate_content(prompt).text,
            context=section['number']
        )
    
    def scope_check(self, section: Dict, draft: str) -> str:
        """Step 3: Check what should be removed from the draft (out of scope)."""
        
        # Skip for Section 32
        if section['number'] == self.SECTION_32_EXEMPT:
            return "No removals needed - data appendix section."
        
        prompt = f"""You are a conservative scope enforcer. Your job is to identify content that CLEARLY does NOT belong in this section.

SECTION {section['number']}: {section['title']}

REQUIREMENTS:
{section['specs']}

OTHER SECTIONS AVAILABLE:
{self._get_other_sections_summary()}

CURRENT DRAFT:
---
{draft}
---

Analyze the draft and create a CONSERVATIVE REMOVE list:

REMOVE LIST FORMAT:
- Paragraph [X]: "[First 10 words...]" - Belongs in Section [Y] (reason)
- Lines [X-Y]: "[First 10 words...]" - Out of scope (reason)
- Table about [topic]: Belongs in Section [Z]

CONSERVATIVE RULES:
1. ONLY flag content that is CLEARLY out of scope for this section
2. When in doubt, KEEP the content in this section
3. Employee data, network assets, facilities ARE core to Operating Footprint - do not remove
4. Only remove obvious financial metrics (revenue, EBITDA, margins) or clearly unrelated content
5. Be specific about location (paragraph number or line range)
6. Include the first few words to ensure correct identification
7. Explain WHERE it belongs or WHY it's clearly out of scope

BIAS: If content could reasonably belong in this section, do NOT remove it.

Output ONLY the REMOVE list. No preamble or explanation."""
        
        return retry_with_backoff(
            lambda: self.model_low_temp.generate_content(prompt).text,
            context=section['number']
        )
    
    def apply_completeness_only(self, section: Dict, current_draft: str, add_list: str) -> str:
        """Apply only the ADD list to create an improved draft."""
        
        prompt = f"""You are a precise editor. Add missing content to create a more complete draft.

CURRENT DRAFT:
---
{current_draft}
---

ADD THESE ITEMS:
---
{add_list}
---

SOURCE DOCUMENTS (for looking up ADD items):
{self.full_context}

INSTRUCTIONS:
1. Add ALL items from the ADD list using the exact data from source documents
2. Maintain narrative flow - integrate additions smoothly into appropriate sections
3. Preserve all existing content - do not remove anything
4. Keep the same professional tone and formatting
5. If an ADD item duplicates existing content, enhance rather than duplicate
6. FOOTNOTE RULES: After adding content, ensure total footnotes do not exceed 5. Renumber sequentially [1] through [5]. No letters in footnotes.

CRITICAL: Output ONLY the enhanced draft markdown content. Do not include any explanations, commentary, or descriptions of what you are doing. No preamble, no postamble - just the final enhanced draft."""
        
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context=section['number']
        )
    
    def _get_other_sections_summary(self) -> str:
        """Get a summary of all other sections for scope checking."""
        summary_lines = []
        for s in sections:
            summary_lines.append(f"Section {s['number']}: {s['title']}")
        return "\n".join(summary_lines) 
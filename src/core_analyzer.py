import google.generativeai as genai
from typing import Dict
import re
import os
from datetime import datetime
from .utils import retry_with_backoff, thread_safe_print, validate_and_fix_tables
from .profile_sections import sections


class CoreAnalyzer:
    """Handles the core analysis pipeline - drafts, critiques, and learning extraction"""
    
    # Progressive word limit configuration
    INITIAL_WORDS = 1000      # Initial draft + completeness critique target
    INSIGHT_WORDS = 500       # Insight critique target - distill key patterns
    POLISH_WORDS = 300        # Polish critique target - final concise output
    SECTION_32_EXEMPT = 34    # Section number that is exempt from word limits (Data Book)
    
    # Temperature configuration for different cognitive phases
    LOW_TEMP = 0.2           # Systematic, methodical analysis (completeness)
    MEDIUM_TEMP = 0.6        # Balanced refinement (polish, initial draft)
    HIGH_TEMP = 0.9          # Creative breakthrough thinking (insights) - reduced to avoid safety blocks

    def __init__(self, run_timestamp: str = None, model_name: str = 'gemini-3-flash-preview',
                 cached_model_low=None, cached_model_medium=None, cached_model_high=None,
                 pdf_parts=None):
        """Initialize core analyzer with cached models for document context

        Args:
            run_timestamp: Optional run timestamp for organizing outputs
            model_name: The Gemini model to use (e.g., 'gemini-3-flash-preview')
            cached_model_low: Cached model with temp 0.2 (optional)
            cached_model_medium: Cached model with temp 0.6 (optional)
            cached_model_high: Cached model with temp 0.9 (optional)
            pdf_parts: List of uploaded file references for fallback (optional)
        """
        # Run timestamp for organizing outputs
        self.run_timestamp = run_timestamp or datetime.now().strftime('%Y_%m_%d_%H_%M_%S')

        # Selected model name
        self.model_name = model_name

        # Store pdf_parts for fallback when cache is not available
        self.pdf_parts = pdf_parts or []

        # Store cached models (documents already in cache)
        self.cached_model_low_temp = cached_model_low
        self.cached_model_medium_temp = cached_model_medium
        self.cached_model_high_temp = cached_model_high

        # Create regular models for fallback OR for operations that don't need docs
        self.model_low_temp = genai.GenerativeModel(
            self.model_name,
            generation_config=genai.types.GenerationConfig(temperature=self.LOW_TEMP)
        )
        self.model_medium_temp = genai.GenerativeModel(
            self.model_name,
            generation_config=genai.types.GenerationConfig(temperature=self.MEDIUM_TEMP)
        )
        self.model_high_temp = genai.GenerativeModel(
            self.model_name,
            generation_config=genai.types.GenerationConfig(temperature=self.HIGH_TEMP)
        )
    
    def create_initial_draft(self, section: Dict) -> str:
        """Step 1: Create a disciplined initial draft."""

        if section['number'] == self.SECTION_32_EXEMPT:
            # This prompt is highly specific and should remain as is.
            prompt = f"""You are a data organizer. Your only job is to create a comprehensive data appendix.

SECTION {section['number']}: {section['title']}

REQUIREMENTS:
{section['specs']}

APPENDIX OUTPUT REQUIREMENTS:
1. Extract the MOST IMPORTANT tables and structured data from the documents.
2. Prioritize: Financial statements, segment data, operational metrics, key ratios
3. Organize tables logically under clear Markdown headers.
4. Format tables correctly in Markdown.
5. Include clear source references for each table using Markdown footnotes [^1], [^2], [^3] format.

SIZE CONSTRAINTS:
- Focus on the 15-20 most important tables
- For very large tables, include key sections or summaries
- Ensure output remains under 500KB total

CRITICAL RESTRICTIONS:
- DO NOT write any analysis, insights, commentary, or explanatory text.
- DO NOT write executive summaries or narrative sections.
- The output must contain ONLY: Markdown headers for table groups, the tables themselves, and footnotes.
- DO NOT wrap your output in code blocks (no ```html or ```markdown)
- DO NOT use HTML tags like <div>, <table>, etc. - use ONLY Markdown syntax
- Tables must use Markdown pipe syntax: | Header | Header | with | :--- | :--- | separators

This is a data-only section. Any narrative text will be removed.
"""
        elif section['number'] == 35:
            # Section 35: Unit Economics Analysis - custom prompt
            prompt = f"""You are writing a unit economics analysis for a private equity investment memo.

SECTION {section['number']}: {section['title']}

{section['specs']}

OUTPUT FORMAT:
- Pure prose, no tables
- No bullet points in final output (use prose paragraphs)
- Target 800-1000 words
- Every claim must have a number
- Cite sources with footnotes [^1], [^2] etc.

Structure your output as:
1. Core business identification (1 paragraph)
2. UOP identification and justification (1 paragraph)
3. Cash flow per UOP calculation (1 paragraph with math shown)
4. Decomposition tree (multiple paragraphs walking through each node)
5. Ancillary businesses (1 paragraph if applicable)
"""
        else:
            prompt = f"""You are an expert business analyst creating a DISCIPLINED initial draft.

SECTION {section['number']}: {section['title']}

REQUIREMENTS:
{section['specs']}

DRAFTING INSTRUCTIONS:
1.  **Word Count:** Target a DRAFT of approximately **{self.INITIAL_WORDS} words**. This is a draft, but it must be focused.
2.  **Scope Adherence:** Adhere STRICTLY to the section requirements. Do NOT include information that belongs in other sections.
3.  **Data First:** Extract all relevant data points first, with precise citations.
4.  **Insightful Analysis:** Provide brief but insightful analysis connecting the data points. Look for non-obvious patterns and implications. Avoid long, speculative paragraphs.
5.  **Footnote Discipline:** Use exactly 5 footnotes maximum. Select the 5 most important data points to cite. Use Markdown footnotes [^1], [^2], [^3], [^4], [^5] with definitions at the end of the section. No letters in footnotes.
6.  **Tables:** Include at least one small, well-formatted Markdown table with the most critical data.

CRITICAL TABLE FORMATTING RULES:
- NEVER create table cells with more than 200 characters
- NEVER create table separator rows with more than 50 dashes/colons total
- MAXIMUM 10 columns per table - if more data needed, split into multiple tables
- MAXIMUM 20 rows per table - if more data needed, summarize or create multiple tables
- If you encounter corrupted tables in source documents (with thousands of dashes or massive cells), DO NOT reproduce them
- Create clean, simple tables with proper markdown syntax: | Header 1 | Header 2 | followed by | :--- | :--- |
- Each table row must have the same number of columns
- If source data is corrupted, extract the meaningful content and present it in a clean format
- For large datasets, prioritize the most important/recent data within the 20-row limit
- ALWAYS put a blank line before and after tables
- NEVER put line breaks inside table cells - keep all cell content on one line
- If a cell needs a pipe character |, escape it as \|
- Table titles like "**Revenue Table**" must have a blank line before the table starts

CRITICAL LIST FORMATTING RULES:
- ALWAYS put a blank line before starting a bulleted or numbered list
- Lists should use consistent markers: either all -, all *, or all numbered
- Don't mix list styles within the same list

Your goal is to create a strong, fact-based draft that applies proven analytical techniques and is well-structured within the target word count.
"""

        # Use cached model if available, else fallback to pdf_parts
        if self.cached_model_medium_temp:
            result = retry_with_backoff(
                lambda: self.cached_model_medium_temp.generate_content([prompt]).text,
                context=section['number']            )
        else:
            result = retry_with_backoff(
                lambda: self.model_medium_temp.generate_content(self.pdf_parts + [prompt]).text,
                context=section['number']            )
        
        # Validate and fix tables BEFORE size check
        result = validate_and_fix_tables(result)
        
        # Safety check: If the initial draft is too large, truncate
        max_chars = 500000 if section['number'] == self.SECTION_32_EXEMPT else 100000
        if len(result) > max_chars:
            thread_safe_print(f"WARNING: Section {section['number']} initial draft exceeded {max_chars} chars ({len(result)} chars). Truncating...")
            # Find a natural break point
            truncate_point = int(max_chars * 0.9)
            last_paragraph = result.rfind('\n\n', truncate_point - 2000, truncate_point)
            if last_paragraph > 0:
                result = result[:last_paragraph] + "\n\n[Content truncated due to excessive length]"
            else:
                result = result[:truncate_point] + "\n\n[Content truncated due to excessive length]"
        
        return result

    def deep_analysis_and_polish(self, section: Dict, comprehensive_draft: str) -> str:
        """Step 4: Apply deep analysis methodology and polish to final output."""
        
        word_count = len(comprehensive_draft.split())
        
        # Skip for Section 32
        if section['number'] == self.SECTION_32_EXEMPT:
            return comprehensive_draft
        
        # Always use original approach for Step 4
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
   - MAXIMUM 10 columns and 20 rows per table - split if needed
   - Fix any malformed tables from the draft
   - ALWAYS put a blank line before and after tables
   - Table titles like "**Revenue Table**" must have a blank line before the table
   - NEVER put line breaks inside table cells
   - Escape pipe characters in cells as \|

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
- Maximum 5 Markdown footnotes [^1] .. [^5], with definitions at the end of the section
- At least one table where appropriate
- Every sentence must add value

Generate the condensed version that respects the section's specific focus."""

        result = retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context=section['number']        )
        
        # Validate and fix tables
        result = validate_and_fix_tables(result)
        
        # Check for empty or very short output (indicating failure)
        if not result or len(result.strip()) < 100:
            thread_safe_print(f"WARNING: Section {section['number']} Step 4 produced empty or minimal output. Using previous draft.")
            # Return the input draft as fallback
            return comprehensive_draft
        
        # Safety check: If the final output is too large, truncate
        max_chars = 10000  # ~500 words * 6 chars/word * 3.3 safety factor
        if len(result) > max_chars:
            thread_safe_print(f"WARNING: Section {section['number']} final output exceeded {max_chars} chars ({len(result)} chars). Truncating...")
            # Find a natural break point
            truncate_point = int(max_chars * 0.9)
            last_paragraph = result.rfind('\n\n', truncate_point - 1000, truncate_point)
            if last_paragraph > 0:
                result = result[:last_paragraph] + "\n\n[Content truncated due to excessive length]"
            else:
                result = result[:truncate_point] + "\n\n[Content truncated due to excessive length]"
        
        return result
    
    def extract_learning(self, section: Dict, final_output: str) -> str:
        """Extract analytical principles that sharpen and deepen future analysis."""

        prompt = f"""Extract analytical principles from this analysis that help sharpen and deepen future company analysis.

SECTION TYPE: {section['title']}

COMPLETED ANALYSIS:
---
{final_output}
---

Extract 2-4 analytical PRINCIPLES - NOT company-specific findings, NOT vague wisdom, NOT just red flags.

WHAT TO EXTRACT:

Analytical principles that guide HOW to analyze more deeply:
- **Comparative techniques** - what to compare against what to reveal hidden truths
- **Decomposition approaches** - how to break down aggregates to expose real drivers
- **Verification methods** - how to test claims using different data sources
- **Relationship patterns** - what metrics or trends to correlate for deeper insight

GOOD EXAMPLES (analytical principles):
- "Compare stated strategy against actual capital allocation to reveal true management priorities"
- "Decompose aggregate growth into organic versus inorganic components to assess core business health"
- "Calculate implied operational metrics from management claims to test their plausibility"
- "Trace revenue recognition through to cash collection to verify business quality"
- "Compare segment economics to corporate average to identify where value is actually created"
- "Map capital deployment to subsequent margin changes to evaluate management effectiveness"

BAD EXAMPLES (too vague or too specific):
- "To see the future, analyze the growth segments" (too vague)
- "Revenue concentration above 30% means customer controls pricing" (specific red flag, not analytical principle)
- "Actions speak louder than words" (corporate poetry)

OUTPUT FORMAT (JSON):
{{
  "principles": [
    "First analytical principle (12-20 words)",
    "Second analytical principle (12-20 words)"
  ]
}}

RULES:
- NO company names, NO sector names, NO specific company numbers
- 12-20 words per principle
- Focus on analytical APPROACHES that sharpen analysis
- Must apply to MOST companies (80%+), not just specific situations like:
  * Parent-subsidiary structures or transfer pricing scenarios
  * Multi-entity groups or conglomerates
  * Captive service centers
  * Complex organizational structures
- Avoid techniques that only work in rare or niche situations

Extract principles that guide deeper analytical thinking."""
        # Low temperature for structured, precise data extraction.
        return retry_with_backoff(
            lambda: self.model_low_temp.generate_content(prompt).text,
            context=section['number']        )
    
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

        # Use cached model if available, else fallback to pdf_parts
        if self.cached_model_low_temp:
            return retry_with_backoff(
                lambda: self.cached_model_low_temp.generate_content([prompt]).text,
                context=section['number']            )
        else:
            return retry_with_backoff(
                lambda: self.model_low_temp.generate_content(self.pdf_parts + [prompt]).text,
                context=section['number']            )


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

INSTRUCTIONS:
1. Add ALL items from the ADD list using the exact data from source documents
2. Maintain narrative flow - integrate additions smoothly into appropriate sections
3. Preserve all existing content - do not remove anything
4. Keep the same professional tone and formatting
5. If an ADD item duplicates existing content, enhance rather than duplicate
6. FOOTNOTE RULES: After adding content, ensure total footnotes do not exceed 5. Use Markdown footnotes [^1]..[^5] with definitions at the end of the section.

CRITICAL TABLE RULES:
- If the current draft contains malformed tables (excessive dashes, huge cells), FIX THEM
- Create proper markdown tables with clean formatting
- No table cell should exceed 200 characters
- MAXIMUM 10 columns per table
- MAXIMUM 20 rows per table
- Table separator rows should use alignment markers: | :--- | :--- | :--- |
- Extract meaningful data from corrupted tables and present cleanly
- Split large tables into multiple smaller tables if needed
- ALWAYS put a blank line before and after tables
- Table titles like "**Revenue Table**" must have a blank line before the table
- NEVER put line breaks inside table cells
- Escape pipe characters in cells as \|

CRITICAL LIST RULES:
- ALWAYS put a blank line before starting any list
- Use consistent list markers throughout

CRITICAL: Output ONLY the enhanced draft markdown content. Do not include any explanations, commentary, or descriptions of what you are doing. No preamble, no postamble - just the final enhanced draft."""

        # Use cached model if available, else fallback to pdf_parts
        if self.cached_model_medium_temp:
            result = retry_with_backoff(
                lambda: self.cached_model_medium_temp.generate_content([prompt]).text,
                context=section['number']            )
        else:
            result = retry_with_backoff(
                lambda: self.model_medium_temp.generate_content(self.pdf_parts + [prompt]).text,
                context=section['number']            )

        # Validate and fix tables
        result = validate_and_fix_tables(result)
        
        # Check for empty or very short output (indicating failure)
        if not result or len(result.strip()) < 100:
            thread_safe_print(f"WARNING: Section {section['number']} Step 3 produced empty or minimal output. Using original draft.")
            # Return the original draft as fallback
            return current_draft
        
        # Safety check: If the improved draft is too large, truncate
        max_chars = 50000
        if len(result) > max_chars:
            thread_safe_print(f"WARNING: Section {section['number']} Step 3 output exceeded {max_chars} chars ({len(result)} chars). Truncating...")
            truncate_point = int(max_chars * 0.9)
            last_paragraph = result.rfind('\n\n', truncate_point - 2000, truncate_point)
            if last_paragraph > 0:
                result = result[:last_paragraph] + "\n\n[Content truncated due to excessive length]"
            else:
                result = result[:truncate_point] + "\n\n[Content truncated due to excessive length]"

        return result

    # =========================================================================
    # Steps 5-9: Ground Truth Insight Pipeline (Sections 1-32 when enabled)
    # =========================================================================

    def ground_truth_discovery(self, section: dict, company_name: str, file_manager) -> str:
        """Step 5: Discover ground truth observations using the section's ground_truth_pointer.

        Temperature: MEDIUM (0.6)
        Documents: YES (cached model or pdf_parts)
        Output: 2-3 observations with GROUND TRUTH / vs. NARRATIVE / COMPETITIVE IMPLICATION
        """
        ground_truth_pointer = section.get('ground_truth_pointer', '')
        if not ground_truth_pointer:
            return "No ground truth pointer defined for this section."

        prompt = f"""Search the source documents about {company_name}'s {section['title']}.

GROUND TRUTH FOCUS: {ground_truth_pointer}

Find 2-3 specific observations that reveal this ground truth. Look beyond the corporate narrative for:
- What the actual structure/data reveals about competitive reality
- How things actually work (mechanisms), even if not explicitly stated
- Where what the company DOES diverges from what it SAYS

For each observation:

OBSERVATION [N]:
GROUND TRUTH: [What the data/structure actually reveals - be specific, cite source]
vs. NARRATIVE: [What management says or implies, if different]
COMPETITIVE IMPLICATION: [What this means for how the company wins or loses]

REQUIREMENT: Each observation must be material to the company's prospects. If it wouldn't change an investor's view, don't include it.

If no meaningful ground truth observations exist for this section, state "No significant observations identified" and explain why.
"""

        # Use cached model if available, else fallback to pdf_parts
        if self.cached_model_medium_temp:
            result = retry_with_backoff(
                lambda: self.cached_model_medium_temp.generate_content([prompt]).text,
                context=f"Section {section['number']} Step 5"
            )
        else:
            result = retry_with_backoff(
                lambda: self.model_medium_temp.generate_content(self.pdf_parts + [prompt]).text,
                context=f"Section {section['number']} Step 5"
            )

        file_manager.save_step_output(section['number'], 'step_5_ground_truth.md', result)
        return result

    def hypothesis_generation(self, step5_output: str, section: dict, company_name: str, file_manager) -> str:
        """Step 6: Generate hypotheses from ground truth observations WITHOUT document anchoring.

        Temperature: MEDIUM (0.6)
        Documents: NO (critical - prevents anchoring to existing framing)
        Output: For each observation: IMPLICATION / ASSUMPTION / RISK / PREDICTION
        """
        prompt = f"""You identified these ground truth observations about {company_name}'s {section['title']}:

{step5_output}

Without re-reading the source documents, generate hypotheses for each observation:

OBSERVATION [N]:
IMPLICATION: What does this reveal about how the company actually operates?
ASSUMPTION: What would have to be true for this to make sense?
RISK: What's the downside if this isn't sustainable or isn't what it appears?
PREDICTION: What else should we see in the documents if this interpretation is correct?
"""

        # CRITICAL: Use model WITHOUT documents - prevents anchoring to existing framing
        result = retry_with_backoff(
            lambda: self.model_medium_temp.generate_content([prompt]).text,
            context=f"Section {section['number']} Step 6"
        )

        file_manager.save_step_output(section['number'], 'step_6_hypotheses.md', result)
        return result

    def hypothesis_testing(self, step6_output: str, section: dict, company_name: str, file_manager) -> str:
        """Step 7: Test hypotheses against document evidence.

        Temperature: LOW (0.2) - precision for evidence gathering
        Documents: YES (cached model or pdf_parts)
        Output: For each prediction: SUPPORTING EVIDENCE / DISCONFIRMING EVIDENCE / VERDICT
        """
        prompt = f"""Test these hypotheses about {company_name}'s {section['title']}:

{step6_output}

For each prediction, search the source documents:

OBSERVATION [N]:
PREDICTION: [from Step 6]
SUPPORTING EVIDENCE: [Quote specific passages with page/section references, or "None found"]
DISCONFIRMING EVIDENCE: [What would refute this? Did you find it? Quote if yes.]
VERDICT: Supported / Refuted / Unclear
"""

        # Use cached model if available, else fallback to pdf_parts
        if self.cached_model_low_temp:
            result = retry_with_backoff(
                lambda: self.cached_model_low_temp.generate_content([prompt]).text,
                context=f"Section {section['number']} Step 7"
            )
        else:
            result = retry_with_backoff(
                lambda: self.model_low_temp.generate_content(self.pdf_parts + [prompt]).text,
                context=f"Section {section['number']} Step 7"
            )

        file_manager.save_step_output(section['number'], 'step_7_test_results.md', result)
        return result

    def insight_synthesis(self, step5_output: str, step7_output: str, section: dict, company_name: str, file_manager) -> str:
        """Step 8: Synthesize CONFIRMED findings from hypothesis testing.

        Temperature: MEDIUM (0.6)
        Documents: YES
        Output: 150-word factual summary of confirmed hypotheses only
        """
        prompt = f"""Summarize the CONFIRMED findings from this hypothesis testing on {company_name}'s {section['title']}.

TEST RESULTS:
{step7_output}

INSTRUCTIONS:
1. Include ONLY hypotheses with verdict "Supported" or "Strongly Supported"
2. Exclude anything marked "Refuted", "Unclear", or "Indeterminate"
3. For each confirmed finding, state:
   - What was confirmed (the prediction that held up)
   - The specific evidence that confirmed it (quote or cite)
4. If NO hypotheses were confirmed, state "No hypotheses were confirmed by document evidence."

Write a factual summary (150 words maximum). No speculation. No interpretation beyond what was explicitly confirmed.
"""

        # Use cached model if available, else fallback to pdf_parts
        if self.cached_model_medium_temp:
            result = retry_with_backoff(
                lambda: self.cached_model_medium_temp.generate_content([prompt]).text,
                context=f"Section {section['number']} Step 8"
            )
        else:
            result = retry_with_backoff(
                lambda: self.model_medium_temp.generate_content(self.pdf_parts + [prompt]).text,
                context=f"Section {section['number']} Step 8"
            )

        file_manager.save_step_output(section['number'], 'step_8_synthesis.md', result)
        return result

    def insight_integration(self, step4_output: str, step8_output: str, section: dict, company_name: str, file_manager) -> str:
        """Step 9: Integrate insights into the polished description.

        Temperature: MEDIUM (0.6)
        Documents: YES
        Output: ~500 words with insights woven in (not bolted on)
        """
        prompt = f"""You have two inputs for {company_name}'s {section['title']}:

POLISHED DESCRIPTION:
{step4_output}

GROUND TRUTH INSIGHTS:
{step8_output}

Rewrite the section so the ground truth insights are woven into the description.

Rules:
- The insights should strengthen the description, not appear as a separate addendum
- Where ground truth contradicts or deepens the narrative, the ground truth wins
- Preserve important data points and tables from the description
- Keep to ~500 words maximum
- Every sentence should help an investor understand how this company actually competes
- Do not add a separate "Analytical Notes" section - integrate fully
"""

        # Use cached model if available, else fallback to pdf_parts
        if self.cached_model_medium_temp:
            result = retry_with_backoff(
                lambda: self.cached_model_medium_temp.generate_content([prompt]).text,
                context=f"Section {section['number']} Step 9"
            )
        else:
            result = retry_with_backoff(
                lambda: self.model_medium_temp.generate_content(self.pdf_parts + [prompt]).text,
                context=f"Section {section['number']} Step 9"
            )

        # Validate and fix tables
        result = validate_and_fix_tables(result)

        file_manager.save_step_output(section['number'], 'step_9_integrated.md', result)
        return result

    # =========================================================================
    # Section 33: Financial Pattern Analysis - 4-Layer Hypothesis-Driven Pipeline
    # =========================================================================

    SECTION_33_PATTERN_ANALYSIS = 33

    def _section33_layer1_identify_patterns(self, company_name: str) -> str:
        """
        Layer 1: Identify 3 material financial/operational patterns worth investigating.
        Attach: Source document
        Returns: Raw text with 3 patterns (to be parsed)
        """
        prompt = f"""You are analysing the financial statements, operational KPIs, and disclosures of {company_name}.

Identify patterns where quantitative metrics that should move together are diverging disproportionately. These can be financial metrics, operational KPIs, or combinations of both.

FIRST: Identify the SINGLE most material pattern - the divergence that most affects the company's prospects. This is your primary pattern.

THEN: Identify TWO additional patterns that are INDEPENDENT of the first. These must have different root causes - not different symptoms of the same issue.

INDEPENDENCE TEST: If Pattern 2 or 3 would disappear if Pattern 1 were resolved, they are not independent. Find different patterns.

REQUIREMENTS FOR EACH PATTERN:
1. BOTH sides must be QUANTITATIVE with specific numbers from the documents
2. The divergence must be SIGNIFICANT (>10% gap or multi-period trend)
3. The pattern must be MATERIAL TO THE COMPANY'S PROSPECTS - does it affect future cash flows, operational sustainability, or competitive position? If no, it's not material.

GOOD PATTERN EXAMPLES:
- Revenue +15% while Production Volume -12% (price masking volume decline)
- Capex 450M vs Depreciation 900M (underinvestment vs asset consumption)
- Reported Profit +20% while Operating Cash Flow -5% (earnings quality question)
- Headcount -15% while Output +8% (productivity or quality risk)
- Customer Count +20% while Revenue per Customer -25% (mix deterioration)

BAD PATTERN EXAMPLES (DO NOT USE):
- "Strong governance despite industry challenges" (qualitative, not quantitative)
- "Revenue growth vs Volume decline" AND "Profit growth vs Volume decline" (same root cause - not independent)
- "Management claims vs Reality" (not measurable divergence)
- "Net Profit vs OCI swing" (accounting volatility that doesn't affect operations or prospects)

For each pattern provide:

1. PATTERN: Concise name describing the two diverging metrics
   METRIC A: [Metric name]: [Value] ([Period]) - Direction: [Up/Down X%]
   METRIC B: [Metric name]: [Value] ([Period]) - Direction: [Up/Down X%]
   PERSISTENCE: [Number] periods
   WHY MATERIAL: [One sentence on why this affects the company's prospects]

2. [Same format]

3. [Same format]

CRITICAL: If you cannot find three genuinely independent quantitative divergences that are material to prospects, report only what you find. Do not pad with accounting noise or patterns that share root causes with Pattern 1.
"""
        # Use cached model if available, else fallback to pdf_parts
        if self.cached_model_medium_temp:
            return retry_with_backoff(
                lambda: self.cached_model_medium_temp.generate_content([prompt]).text,
                context="Section 33 Layer 1"            )
        else:
            return retry_with_backoff(
                lambda: self.model_medium_temp.generate_content(self.pdf_parts + [prompt]).text,
                context="Section 33 Layer 1"            )

    def _section33_layer2_generate_hypotheses(self, company_name: str, pattern_text: str) -> str:
        """
        Layer 2: Generate 3 candidate explanations for a pattern.
        Attach: Nothing (intentional - broad hypothesis generation)
        Returns: Raw text with 3 explanations (to be parsed)
        """
        prompt = f"""You have identified this pattern in {company_name}'s financials:

{pattern_text}

Generate exactly three candidate explanations spanning benign to concerning.

CRITICAL: At least one hypothesis must challenge the validity or interpretation of the metrics themselves, not just explain the divergence. Consider: Is there an accounting explanation? A consolidation scope change? A timing distortion? A measurement issue?

For each explanation, provide:

NAME: Concise name
MECHANISM: How this would produce the observed pattern
PREDICTION A: First testable prediction - something else we should observe if this explanation is true
PREDICTION B: Second testable prediction
PREDICTION C: Third testable prediction

Be specific to this company's context. Avoid generic explanations.

Present as EXPLANATION 1, EXPLANATION 2, EXPLANATION 3.
"""
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context="Section 33 Layer 2"        )

    def _section33_layer3_test_hypothesis(self, company_name: str, pattern_text: str, explanation_text: str) -> str:
        """
        Layer 3: Test a hypothesis against document evidence.
        Attach: Source document
        Returns: Raw text with verdict
        """
        prompt = f"""You are testing a hypothesis about {company_name}.

PATTERN:
{pattern_text}

HYPOTHESIS:
{explanation_text}

Search the document for evidence bearing on each prediction.

For each prediction, report:
- EVIDENCE FOUND: Quote relevant passages with page/note references. If nothing relevant exists, state "No evidence found in document."
- DISCONFIRMING EVIDENCE: What evidence would REFUTE this prediction? Actively search for it and report what you find.
- DIRECTION: Supports / Refutes / Ambiguous
- STRENGTH: Strong / Moderate / Weak / No evidence

Do not infer or fabricate evidence. Only cite what is explicitly stated in the document.

OVERALL VERDICT: Rate as strongly supported / supported / indeterminate / weakly refuted / strongly refuted. Explain in two sentences, weighing both supporting and disconfirming evidence.
"""
        # Use cached model if available, else fallback to pdf_parts
        if self.cached_model_low_temp:
            return retry_with_backoff(
                lambda: self.cached_model_low_temp.generate_content([prompt]).text,
                context="Section 33 Layer 3"            )
        else:
            return retry_with_backoff(
                lambda: self.model_low_temp.generate_content(self.pdf_parts + [prompt]).text,
                context="Section 33 Layer 3"            )

    def _section33_layer4_synthesize(self, company_name: str, pattern_text: str,
                                      explanations_and_verdicts: str) -> str:
        """
        Layer 4: Write analytical summary for a pattern.
        Attach: Source document
        Returns: Max 200 word analytical summary
        """
        prompt = f"""You have investigated a pattern in {company_name}'s financials.

PATTERN:
{pattern_text}

HYPOTHESES TESTED AND VERDICTS:
{explanations_and_verdicts}

Write an analytical summary for a senior banker.

THESIS: One paragraph stating what is actually happening and why. Take a position.

KEY EVIDENCE: Three to five specific figures from the document that support your thesis. ALWAYS use the most recent data available. Include currency/units and time periods.

SUSTAINABILITY: Is this trajectory sustainable? What breaks it and when?

VALUATION DRIVERS: What is this pattern doing to the key value drivers (earnings quality, growth sustainability, capital efficiency, risk profile)? Be specific about WHAT is affected.

NARRATIVE GAP: State what management has NOT explained. Identify the specific gap between their narrative and the data.

Maximum 200 words. Direct prose. No bullet points. No reference to hypotheses or methodology. No explicit diligence questions - the analysis should make them obvious.
"""
        # Use cached model if available, else fallback to pdf_parts
        if self.cached_model_medium_temp:
            return retry_with_backoff(
                lambda: self.cached_model_medium_temp.generate_content([prompt]).text,
                context="Section 33 Layer 4"            )
        else:
            return retry_with_backoff(
                lambda: self.model_medium_temp.generate_content(self.pdf_parts + [prompt]).text,
                context="Section 33 Layer 4"            )

    def _section33_parse_patterns(self, layer1_output: str) -> list:
        """Parse Layer 1 output into list of pattern dicts."""
        patterns = []

        # Split by pattern numbers - handles multiple formats:
        # - Plain: 1. or 1)
        # - Markdown headers: ### 1. or ## 1.
        # - Bold: **1.** or **1)**
        pattern_blocks = re.split(r'\n(?=(?:#{1,3}\s*)?\*{0,2}\d[\.\)]\*{0,2}\s)', layer1_output)

        for block in pattern_blocks:
            block = block.strip()
            if not block or not re.match(r'^(?:#{1,3}\s*)?\*{0,2}\d[\.\)]', block):
                continue

            pattern = {"raw": block}

            # Extract PATTERN name
            pattern_match = re.search(r'PATTERN:\s*(.+?)(?:\n|$)', block, re.IGNORECASE)
            if pattern_match:
                pattern["name"] = pattern_match.group(1).strip()
            else:
                # Try to get first line after number (handles markdown prefixes)
                first_line = block.split('\n')[0]
                pattern["name"] = re.sub(r'^(?:#{1,3}\s*)?\*{0,2}\d[\.\)]\*{0,2}\s*', '', first_line).strip()

            # Extract OBSERVATION
            obs_match = re.search(r'OBSERVATION:\s*(.+?)(?=\n[A-Z]+:|$)', block, re.IGNORECASE | re.DOTALL)
            if obs_match:
                pattern["observation"] = obs_match.group(1).strip()

            patterns.append(pattern)

        # Ensure we have exactly 3 patterns
        while len(patterns) < 3:
            patterns.append({"raw": f"Pattern {len(patterns)+1}: Unable to identify", "name": f"Pattern {len(patterns)+1}"})

        return patterns[:3]

    def _section33_validate_pattern_diversity(self, patterns: list) -> list:
        """
        Check for redundant patterns and warn if detected.
        Uses text similarity rather than hardcoded keywords.
        """
        if len(patterns) < 2:
            return patterns

        def extract_significant_words(text: str) -> set:
            """Extract significant words (excluding common stopwords)."""
            stopwords = {'the', 'a', 'an', 'is', 'are', 'was', 'were', 'be', 'been',
                         'being', 'have', 'has', 'had', 'do', 'does', 'did', 'will',
                         'would', 'could', 'should', 'may', 'might', 'must', 'shall',
                         'can', 'need', 'dare', 'ought', 'used', 'to', 'of', 'in',
                         'for', 'on', 'with', 'at', 'by', 'from', 'as', 'into',
                         'through', 'during', 'before', 'after', 'above', 'below',
                         'between', 'under', 'again', 'further', 'then', 'once',
                         'here', 'there', 'when', 'where', 'why', 'how', 'all',
                         'each', 'few', 'more', 'most', 'other', 'some', 'such',
                         'no', 'nor', 'not', 'only', 'own', 'same', 'so', 'than',
                         'too', 'very', 'just', 'and', 'but', 'if', 'or', 'because',
                         'until', 'while', 'this', 'that', 'these', 'those', 'what',
                         'which', 'who', 'whom', 'its', 'it', 'they', 'them', 'their',
                         'we', 'us', 'our', 'you', 'your', 'he', 'him', 'his', 'she',
                         'her', 'vs', 'per', 'also', 'however', 'therefore', 'thus'}
            words = re.findall(r'\b[a-z]{3,}\b', text.lower())
            return {w for w in words if w not in stopwords}

        # Extract significant words from each pattern
        pattern_words = [extract_significant_words(p.get("raw", "")) for p in patterns]

        # Check for high overlap (Jaccard similarity)
        for i in range(len(patterns)):
            for j in range(i+1, len(patterns)):
                if not pattern_words[i] or not pattern_words[j]:
                    continue
                intersection = pattern_words[i] & pattern_words[j]
                union = pattern_words[i] | pattern_words[j]
                similarity = len(intersection) / len(union) if union else 0

                if similarity > 0.4:  # >40% word overlap suggests redundancy
                    shared_sample = ', '.join(sorted(intersection)[:5])
                    thread_safe_print(f"    WARNING: Patterns {i+1} and {j+1} may be redundant ({similarity:.0%} overlap: {shared_sample}...)")

        return patterns

    def _section33_parse_explanations(self, layer2_output: str) -> list:
        """Parse Layer 2 output into list of explanation dicts."""
        explanations = []

        # Split by EXPLANATION markers (handles plain text and markdown bold **EXPLANATION**)
        exp_blocks = re.split(r'\n(?=\*{0,2}EXPLANATION\s*\d)', layer2_output, flags=re.IGNORECASE)

        for block in exp_blocks:
            block = block.strip()
            if not block or not re.match(r'\*{0,2}EXPLANATION\s*\d', block, re.IGNORECASE):
                continue

            explanation = {"raw": block}

            # Extract NAME (handles **NAME**: and *NAME*: formats)
            name_match = re.search(r'\*{0,2}NAME\*{0,2}:\s*(.+?)(?:\n|$)', block, re.IGNORECASE)
            if name_match:
                name = name_match.group(1).strip()
                name = re.sub(r'^\*+|\*+$', '', name).strip()  # Remove markdown artifacts
                explanation["name"] = name
            else:
                explanation["name"] = f"Explanation {len(explanations)+1}"

            explanations.append(explanation)

        # Ensure we have exactly 3 explanations
        while len(explanations) < 3:
            explanations.append({"raw": f"Explanation {len(explanations)+1}: Unable to generate", "name": f"Explanation {len(explanations)+1}"})

        return explanations[:3]

    def _section33_extract_verdict(self, layer3_output: str) -> str:
        """Extract the overall verdict from Layer 3 output."""
        # Handle **OVERALL VERDICT**: format
        verdict_match = re.search(r'\*{0,2}OVERALL VERDICT\*{0,2}:\s*(.+?)(?:\n\n|$)', layer3_output, re.IGNORECASE | re.DOTALL)
        if verdict_match:
            return verdict_match.group(1).strip()
        return "Verdict: Unable to determine"

    def analyze_section_33(self, company_name: str, file_manager, worker_display=None) -> str:
        """
        Orchestrate the 4-layer pipeline for Section 33: Financial Pattern Analysis.

        Execution flow:
        1. Layer 1: 1 call -> 3 patterns
        2. Layer 2: 3 parallel calls -> 9 explanations (3 per pattern)
        3. Layer 3: 9 parallel calls -> 9 verdicts
        4. Layer 4: 3 parallel calls -> 3 summaries

        Total: 16 API calls

        Returns: Formatted markdown with 3 pattern subsections
        """
        from concurrent.futures import ThreadPoolExecutor, as_completed

        section_dir = f"runs/run_{self.run_timestamp}/section_33"
        os.makedirs(section_dir, exist_ok=True)

        # ===================
        # LAYER 1: Identify Patterns
        # ===================
        if worker_display:
            worker_display.update(33, "Layer 1")
        thread_safe_print("  Layer 1: Identifying financial patterns...")

        layer1_output = self._section33_layer1_identify_patterns(company_name)

        # Save Layer 1 output
        file_manager.save_step_output(33, "layer1_patterns.txt", layer1_output)

        # Parse and validate patterns
        patterns = self._section33_parse_patterns(layer1_output)
        patterns = self._section33_validate_pattern_diversity(patterns)

        # Display pattern names
        for i, p in enumerate(patterns):
            name = p.get("name", "Unknown")
            name = re.sub(r'^\*+|\*+$', '', name).strip()  # Remove markdown artifacts
            thread_safe_print(f"    [{i+1}] {name[:50]}")

        # ===================
        # LAYER 2: Generate Hypotheses (3 calls, sequential)
        # ===================
        thread_safe_print("  Layer 2: Generating hypotheses...")

        all_explanations = {}  # pattern_idx -> list of explanations

        for pattern_idx, pattern in enumerate(patterns):
            if worker_display:
                worker_display.update(33, f"L2 [{pattern_idx+1}/3]")
            try:
                layer2_output = self._section33_layer2_generate_hypotheses(
                    company_name,
                    pattern["raw"]
                )
                # Save Layer 2 output
                file_manager.save_step_output(33, f"layer2_pattern{pattern_idx+1}_hypotheses.txt", layer2_output)
                # Parse explanations
                explanations = self._section33_parse_explanations(layer2_output)
                all_explanations[pattern_idx] = explanations
                # Show pattern name and hypothesis names (clean markdown artifacts)
                pattern_name = pattern.get("name", f"Pattern {pattern_idx+1}")
                pattern_name = re.sub(r'^\*+|\*+$', '', pattern_name).strip()[:30]
                def clean_hyp_name(n):
                    n = re.sub(r'^\*+|\*+$', '', n).strip()
                    return n[:20]
                hyp_names = " | ".join([clean_hyp_name(e.get("name", "?")) for e in explanations])
                thread_safe_print(f"    [{pattern_idx+1}] {pattern_name}: {hyp_names}")
            except Exception as e:
                thread_safe_print(f"    [{pattern_idx+1}] Error - {e}")
                all_explanations[pattern_idx] = [{"raw": "Error generating hypothesis", "name": "Error"}] * 3

        # ===================
        # LAYER 3: Test Hypotheses (9 calls, sequential to avoid rate limits)
        # ===================
        thread_safe_print("  Layer 3: Testing hypotheses against evidence...")

        all_verdicts = {}  # (pattern_idx, exp_idx) -> verdict

        call_count = 0
        for pattern_idx in range(3):
            for exp_idx in range(3):
                call_count += 1
                if worker_display:
                    worker_display.update(33, f"L3 [{call_count}/9]")

                pattern = patterns[pattern_idx]
                explanation = all_explanations.get(pattern_idx, [{"raw": "N/A"}] * 3)[exp_idx]

                try:
                    layer3_output = self._section33_layer3_test_hypothesis(
                        company_name,
                        pattern["raw"],
                        explanation["raw"]
                    )
                    # Save Layer 3 output
                    file_manager.save_step_output(33, f"layer3_pattern{pattern_idx+1}_hyp{exp_idx+1}_verdict.txt", layer3_output)
                    # Extract verdict
                    verdict = self._section33_extract_verdict(layer3_output)
                    all_verdicts[(pattern_idx, exp_idx)] = verdict
                    # Extract just the verdict category
                    verdict_category = "Unknown"
                    verdict_lower = verdict.lower()
                    if "strongly supported" in verdict_lower:
                        verdict_category = "Strongly Supported"
                    elif "strongly refuted" in verdict_lower:
                        verdict_category = "Strongly Refuted"
                    elif "supported" in verdict_lower:
                        verdict_category = "Supported"
                    elif "refuted" in verdict_lower:
                        verdict_category = "Refuted"
                    elif "indeterminate" in verdict_lower:
                        verdict_category = "Indeterminate"
                    exp_name = explanation.get("name", "Hyp")[:20]
                    thread_safe_print(f"    [{call_count}/9] P{pattern_idx+1}-{exp_name}: {verdict_category}")
                except Exception as e:
                    thread_safe_print(f"    Pattern {pattern_idx+1} Hyp {exp_idx+1}: Error - {e}")
                    all_verdicts[(pattern_idx, exp_idx)] = "Error: Unable to test"

        thread_safe_print(f"    Completed {len(all_verdicts)} hypothesis tests")

        # ===================
        # LAYER 4: Synthesize (3 calls, sequential to avoid rate limits)
        # ===================
        thread_safe_print("  Layer 4: Synthesizing analytical summaries...")

        all_summaries = {}  # pattern_idx -> summary

        for pattern_idx in range(3):
            if worker_display:
                worker_display.update(33, f"L4 [{pattern_idx+1}/3]")

            pattern = patterns[pattern_idx]

            # Build explanations and verdicts text
            exp_and_verdicts = []
            for exp_idx in range(3):
                explanation = all_explanations.get(pattern_idx, [{"name": "N/A"}] * 3)[exp_idx]
                verdict = all_verdicts.get((pattern_idx, exp_idx), "No verdict")
                exp_and_verdicts.append(f"{explanation.get('name', 'Explanation')}: {verdict}")

            exp_verdicts_text = "\n".join(exp_and_verdicts)

            try:
                summary = self._section33_layer4_synthesize(
                    company_name,
                    pattern["raw"],
                    exp_verdicts_text
                )
                # Save Layer 4 output
                file_manager.save_step_output(33, f"layer4_pattern{pattern_idx+1}_synthesis.md", summary)
                all_summaries[pattern_idx] = summary
                pattern_name = pattern.get("name", f"Pattern {pattern_idx+1}")[:40]
                thread_safe_print(f"    [{pattern_idx+1}] {pattern_name}: Complete")
            except Exception as e:
                thread_safe_print(f"    Pattern {pattern_idx+1}: Error - {e}")
                all_summaries[pattern_idx] = f"Error generating synthesis: {e}"

        # ===================
        # COMBINE INTO FINAL OUTPUT
        # ===================
        if worker_display:
            worker_display.update(33, "Final")

        final_output = "## Section 33: Financial Pattern Analysis\n\n"

        for pattern_idx in range(3):
            pattern = patterns[pattern_idx]
            # Clean pattern name - remove any markdown artifacts
            pattern_name = pattern.get("name", f"Pattern {pattern_idx+1}")
            pattern_name = re.sub(r'^\*+|\*+$', '', pattern_name).strip()  # Remove leading/trailing asterisks
            pattern_name = re.sub(r'\*\*$', '', pattern_name).strip()  # Remove trailing bold markers
            summary = all_summaries.get(pattern_idx, "Summary not available")

            final_output += f"### Pattern {pattern_idx+1}: {pattern_name}\n\n"
            final_output += summary.strip() + "\n\n"

        # Save final section output
        file_manager.save_step_output(33, "step_4_final_section.md", final_output)

        thread_safe_print("  Section 33 complete: 16 API calls executed")

        return final_output

    # =========================================================================
    # Section 35: Unit Economics Analysis - Custom Refinement Prompts (2-5)
    # Pipeline: 1 (specs) -> 2 (critique+temporal) -> 3 (integrated synthesis)
    #           -> 4 (peer benchmark) -> 5 (final assembly)
    # =========================================================================

    def _section35_prompt2_critique_and_temporal(self, company_name: str, prompt1_output: str) -> str:
        """
        Prompt 2: Self-critique, targeted deepening, AND temporal extraction.
        Temperature: LOW (0.2)
        Documents: YES
        """
        prompt = f"""Here is your unit economics analysis for {company_name}:

{prompt1_output}

Perform TWO tasks:

## TASK 1: SELF-CRITIQUE AND DEEPENING

Review your work:

1. Which branches are flagged Inferred or Speculative?
2. Which branches stopped at only 1-2 levels when deeper decomposition might be possible?
3. Are there any logical gaps or inconsistencies?

For each weak branch:
- Go back to the source documents
- Try to find additional evidence
- Push deeper if evidence exists
- If you still cannot find evidence, state this clearly and leave the branch as is

Do not re-do the solid parts. Only refine the weak spots.

## TASK 2: TEMPORAL EXTRACTION

For EVERY quantifiable driver in the tree, extract temporal data where available in the source documents.

Create a TEMPORAL DATA TABLE at the end of your output:

| Driver | LTM | Most Recent FY | Prior FY | Change Explanation |
|--------|-----|----------------|----------|-------------------|

For each driver:

1. **LTM (Last Twelve Months)**: Value for the most recent 12-month period if disclosed. If not available, write "Not disclosed".

2. **Most Recent FY**: Value for the most recently completed fiscal year. Cite the source (document, page).

3. **Prior FY**: Value for the fiscal year before that. Cite the source.

4. **Change Explanation**: For changes >5% year-over-year:
   - Quote management commentary if available, marked as [Management]
   - Otherwise infer the likely cause from operational context, marked as [Inferred]

RULES:
- Only include drivers that have at least TWO data points across the three periods
- If a driver has only one historical value, note it but do not fabricate history
- Always cite specific document references (page number, document name)
- For percentage-based drivers (utilisation, yield), report percentage values
- For absolute drivers (price, volume), report values with units
- Do not fabricate or estimate values not in the documents

OUTPUT FORMAT:
1. First, the refined unit economics tree from Task 1
2. Then, a clearly labelled TEMPORAL DATA TABLE with all quantifiable drivers
"""
        if self.cached_model_low_temp:
            return retry_with_backoff(
                lambda: self.cached_model_low_temp.generate_content([prompt]).text,
                context="Section 35 Prompt 2"
            )
        else:
            return retry_with_backoff(
                lambda: self.model_low_temp.generate_content(self.pdf_parts + [prompt]).text,
                context="Section 35 Prompt 2"
            )

    def _section35_prompt3_integrated_synthesis(self, company_name: str, prompt2_output: str) -> str:
        """
        Prompt 3: Integrated prose synthesis with temporal context.
        Temperature: MEDIUM (0.6)
        Documents: NO (synthesis from prior output only - temporal data already extracted)
        """
        prompt = f"""Here is the refined unit economics analysis with temporal data for {company_name}:

{prompt2_output}

Write a unit economics narrative for a private equity investment memo. PROSE ONLY - NO TABLES.

STRUCTURE (all in prose paragraphs):

1. THE UNIT (1 paragraph)
   What is the Unit of Production? Describe it concretely - what does one unit actually represent in this business? Why is this the right unit for analysis? This must be crystal clear before any numbers are discussed.

2. HEADLINE ECONOMICS (1 paragraph)
   State cash flow per UOP. Is it positive or negative? How has it changed from prior year? One-line verdict: improving, stable, or deteriorating.

3. NODE-BY-NODE EVOLUTION (main body - multiple paragraphs)
   Walk through EACH driver in the decomposition tree. For each node:
   - State what the driver is and its current value
   - State its prior year value and the change (absolute and %)
   - Explain WHY it changed (management commentary or inferred)
   - State whether the trend is continuing, reversing, or stabilising

   Structure this as a logical walk through the tree: start with revenue per UOP, decompose into its components, then costs per UOP, decompose into its components. Each node gets its own treatment.

   EXAMPLE OF GOOD NODE DESCRIPTION:
   "Revenue per tower currently stands at $42,000, up 9% from $38,500 in FY23. Management attributes this to pricing adjustments on legacy contracts as they come up for renewal. The LTM figure of $43,200 suggests acceleration into FY25."

   EXAMPLE OF BAD NODE DESCRIPTION (DO NOT DO THIS):
   "Revenue per tower is $42,000."

4. DATA GAPS (final paragraph)
   Which nodes lack temporal data? State in prose, not bullets.

RULES:
- PURE PROSE - absolutely no tables, no bullet points
- Every sentence must contain a number
- Maximum 500 words total
- Write like a PE associate: direct, data-dense, no fluff

FORBIDDEN:
- Tables of any kind
- Bullet points
- Accounting margins (gross margin %, EBITDA margin %)
- Financial ratios (ROE, debt/equity)
- Valuation commentary
"""
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content([prompt]).text,
            context="Section 35 Prompt 3"
        )

    def _section35_prompt4_peer_benchmark(self, company_name: str, prompt2_output: str) -> str:
        """
        Prompt 4: Peer benchmarking (optional - may return empty if no peer data).
        Temperature: LOW (0.2)
        Documents: YES
        """
        prompt = f"""Here is the unit economics analysis for {company_name}:

{prompt2_output}

Benchmark the unit economics drivers against peers. PROSE ONLY - NO TABLES.

FOCUS ON THESE OPERATIONAL DRIVERS (if peer data available):
- Revenue per UOP (absolute $)
- Cost per UOP (absolute $)
- Cash flow per UOP (absolute $)
- Utilisation rate / capacity factor / occupancy
- Productivity metrics (output per employee, units per hour)

FORBIDDEN:
- Tables of any kind
- Financial ratios (ROE, ROA, debt/equity)
- Accounting margins as percentages (gross margin %, EBITDA margin %)
- Revenue growth rates
- Valuation multiples

If peer or industry benchmark data is available in the source documents, write 2-3 paragraphs:

1. Where does {company_name} lag peers on unit economics drivers? By how much? What would closing each gap add to cash flow per UOP?

2. Where does {company_name} lead? Is the advantage structural or temporary?

If peer data is NOT available, state in one sentence which operational drivers per UOP would be most valuable to benchmark.

Do not fabricate peer data. Maximum 150 words.
"""
        # Use cached model if available, else fallback to pdf_parts
        if self.cached_model_low_temp:
            return retry_with_backoff(
                lambda: self.cached_model_low_temp.generate_content([prompt]).text,
                context="Section 35 Prompt 4"
            )
        else:
            return retry_with_backoff(
                lambda: self.model_low_temp.generate_content(self.pdf_parts + [prompt]).text,
                context="Section 35 Prompt 4"
            )

    def _section35_prompt5_final_assembly(self, company_name: str,
                                           prompt3_output: str,
                                           prompt4_output: str) -> str:
        """
        Prompt 5: Final assembly.
        Temperature: LOW (0.2)
        Documents: NO (assembly from prior outputs only)
        """
        prompt = f"""Assemble the final Unit Economics Analysis section for {company_name}.

INPUTS:

UNIT ECONOMICS NARRATIVE:
{prompt3_output}

PEER BENCHMARKING:
{prompt4_output}

OUTPUT STRUCTURE (prose only, no tables):

## Unit Economics Analysis

[Combine the narrative and peer comparison into flowing prose. The structure should be:

1. First paragraph: What is the UOP and why it's the right unit for this business
2. Second paragraph: Headline cash flow per UOP and whether it's improving/deteriorating
3. Following paragraphs: Node-by-node evolution of each driver with temporal context
4. Final paragraph: Peer comparison (if available) or data gaps

Total length: 500-600 words maximum.]

RULES:
- PURE PROSE - no tables, no bullet points, no headers beyond "## Unit Economics Analysis"
- Every sentence must contain a number
- Write like a PE investment memo: direct, data-dense

FORBIDDEN - Remove if present:
- All tables
- Bullet points
- "Investment Implications" section
- Accounting margins (%, not absolute $)
- Financial ratios
- Valuation commentary
"""
        return retry_with_backoff(
            lambda: self.model_low_temp.generate_content([prompt]).text,
            context="Section 35 Prompt 5"
        )

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
    SECTION_32_EXEMPT = 33    # Section number that is exempt from word limits (Data Book)
    
    # Temperature configuration for different cognitive phases
    LOW_TEMP = 0.2           # Systematic, methodical analysis (completeness)
    MEDIUM_TEMP = 0.6        # Balanced refinement (polish, initial draft)
    HIGH_TEMP = 0.9          # Creative breakthrough thinking (insights) - reduced to avoid safety blocks
    
    def __init__(self, full_context: str, run_timestamp: str = None, model_name: str = 'gemini-2.5-flash'):
        """Initialize core analyzer with document context
        
        Args:
            full_context: The full document context to analyze
            run_timestamp: Optional run timestamp for organizing outputs
            model_name: The Gemini model to use (e.g., 'gemini-2.5-flash' or 'gemini-2.5-flash-lite')
        """
        self.full_context = full_context
        
        # Run timestamp for organizing outputs
        self.run_timestamp = run_timestamp or datetime.now().strftime('%Y_%m_%d_%H_%M_%S')
        
        # Selected model name
        self.model_name = model_name
        
        # Create models with different temperatures for different cognitive phases
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
5.  **Insightful Analysis:** Provide brief but insightful analysis connecting the data points. Look for non-obvious patterns and implications. Avoid long, speculative paragraphs.
6.  **Footnote Discipline:** Use exactly 5 footnotes maximum. Select the 5 most important data points to cite. Use Markdown footnotes [^1], [^2], [^3], [^4], [^5] with definitions at the end of the section. No letters in footnotes.
7.  **Tables:** Include at least one small, well-formatted Markdown table with the most critical data.

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
        
        result = retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context=section['number']
        )
        
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
            context=section['number']
        )
        
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
        """Extract wise analytical principles that can improve future analysis."""

        prompt = f"""You are extracting WISE ANALYTICAL PRINCIPLES that apply to ANY company in ANY sector.

Think like a seasoned investor advising a junior analyst: What fundamental truths guide effective analysis?

SECTION TYPE: {section['title']}

COMPLETED ANALYSIS:
---
{final_output}
---

Extract 2-4 timeless PRINCIPLES from this analysis - NOT company-specific findings or technical methodologies.

PRINCIPLE EXTRACTION GUIDANCE:

Principles should be:
- **Fundamental truths** about analysis, not specific calculation methods
- **Memorable and quotable** - something an analyst would remember and apply
- **Focus on WHAT matters**, not HOW to calculate it
- **8-15 words maximum** - brevity forces clarity
- **Universally applicable** across ALL sectors

GOOD EXAMPLES (the style to emulate):
- "Actions speak louder than words - watch where capital actually goes"
- "Cash flow quality reveals more truth than reported profits"
- "Verify management's narrative against primary source data"
- "Customer concentration creates fragility in revenue continuity"
- "Cost growth outpacing revenue growth signals broken unit economics"

BAD EXAMPLES (avoid these patterns):
- "When analyzing capital allocation, correlate major investment events with subsequent changes in segmental growth..." (Too technical, too long)
- "Calculate cash conversion ratios (Operating Cash Flow / EBITDA) to assess earnings quality..." (Too procedural)
- "Cross-check operational data against financial data to test for logical consistency..." (Focuses on HOW not WHAT)

OUTPUT FORMAT (JSON):
{{
  "principles": [
    "First wise principle (8-15 words)",
    "Second wise principle (8-15 words)"
  ]
}}

RULES:
- Focus on what matters, not how to calculate it
- Each principle should apply across multiple companies/industries
- Maximum 4 principles - focus on most valuable insights
- No company names, sector names, or specific numbers

Extract only the most transferable and memorable analytical principles."""
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

SOURCE DOCUMENTS (WARNING: May contain corrupted tables - ignore malformed formatting):
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
        
        result = retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text,
            context=section['number']
        )
        
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
    
 
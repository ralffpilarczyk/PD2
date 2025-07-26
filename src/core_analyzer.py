import google.generativeai as genai
from typing import Dict
import re
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
    
    def __init__(self, full_context: str):
        """Initialize core analyzer with document context"""
        self.full_context = full_context
        
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
1. Extract ALL tables and structured data from the documents.
2. Organize tables logically under clear Markdown headers.
3. Format tables correctly in Markdown.
4. Include clear source references for each table using [1], [2], [3] footnote format.

CRITICAL RESTRICTIONS:
- DO NOT write any analysis, insights, commentary, or explanatory text.
- DO NOT write executive summaries or narrative sections.
- The output must contain ONLY: Markdown headers for table groups, the tables themselves, and footnotes.

This is a data-only section. Any narrative text will be removed.
"""
        else:
            prompt = f"""You are an expert business analyst creating a DISCIPLINED initial draft.

SECTION {section['number']}: {section['title']}

REQUIREMENTS:
{section['specs']}

{relevant_memory}

DOCUMENTS:
{self.full_context}

DRAFTING INSTRUCTIONS:
1.  **Word Count:** Target a DRAFT of approximately **{self.INITIAL_WORDS} words**. This is a draft, but it must be focused.
2.  **Scope Adherence:** Adhere STRICTLY to the section requirements. Do NOT include information that belongs in other sections.
3.  **Data First:** Extract all relevant data points first, with precise citations.
4.  **Concise Analysis:** Provide brief analysis connecting the data points. Avoid long, speculative paragraphs.
5.  **Footnote Discipline:** Use footnotes sparingly (max 5-8). Cite only specific numbers or direct quotes. Use [1], [2], [3] format.
6.  **Tables:** Include at least one small, well-formatted Markdown table with the most critical data.

Your goal is to create a strong, fact-based draft that is well-structured and within the target word count, requiring only polishing, not a rewrite.
"""
        
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text
        )

    def polish_critique(self, section: Dict, draft: str) -> str:
        """Step 2: Generate aggressive polish instructions to finalize the draft."""
        
        word_count = len(draft.split())
        
        # This function should not be called for Section 32, but as a safeguard:
        if section['number'] == self.SECTION_32_EXEMPT:
            return "This section is a data appendix. No polishing is required. Ensure it contains only tables, headers, and footnotes."

        prompt = f"""You are a ruthless editor. Your job is to make this draft final.

ANALYSIS DRAFT TO BE POLISHED:
---
{draft}
---

CURRENT WORD COUNT: {word_count} words
FINAL TARGET WORD COUNT: **Strictly under {self.POLISH_WORDS} words.**

MANDATORY INSTRUCTIONS FOR FINAL VERSION:
1.  **CUT WORD COUNT:** Be aggressive. Remove redundant phrases, verbose sentences, and less critical points to get under the {self.POLISH_WORDS}-word target.
2.  **ENFORCE SCOPE:** Delete ANY analysis that is not directly required by the section specs. For example, if you see an "Approach" or "Methodology" section, REMOVE it.
3.  **FIX FOOTNOTES:**
    - Ensure there are no more than 5 unique footnotes.
    - Consolidate citations where possible.
    - **Renumber all footnotes to be sequential, starting from [1].** For example, if the draft has `[1], [2], [5]`, the final version MUST be `[1], [2], [3]`.
4.  **STANDARDIZE FORMATTING:** Ensure all tables are clean Markdown and all footnotes are in `[1]` format.
5.  **IMPROVE CLARITY:** Rewrite any confusing sentences for clarity and impact.

Provide a set of clear, actionable instructions that the `apply_critique` function will use to generate the final, polished text. The goal is a concise, high-impact final section that meets all constraints.
"""
        # Use low temperature for systematic and strict editing instructions.
        return retry_with_backoff(
            lambda: self.model_low_temp.generate_content(prompt).text
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
- **Footnote Renumbering:** Ensure all footnotes are sequential (e.g., [1], [2], [3]).
- **Scope:** All out-of-scope content mentioned in the instructions must be removed.

Produce only the final, revised Markdown text.
"""
        # Medium temperature to apply edits intelligently without going off-track.
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text
        )

    def extract_learning(self, section: Dict, final_output: str) -> str:
        """Extracts key insights and learnings from the final polished section."""
        
        prompt = f"""Given the final, polished analysis for this section, extract the most critical insights and learnings.

FINAL ANALYSIS:
---
{final_output}
---

Based on this final text, identify:
1.  **Three Key Findings:** What are the three most important, non-obvious conclusions from the analysis?
2.  **Two Critical Red Flags:** What are the two biggest risks or areas of concern identified?
3.  **One Major Disconnect:** What is the most significant contradiction between what the company says and what the data shows?

Present these learnings in a structured JSON format. For example:
{{
  "key_findings": [
    "Finding 1...",
    "Finding 2...",
    "Finding 3..."
  ],
  "red_flags": [
    "Risk 1...",
    "Risk 2..."
  ],
  "major_disconnect": "The major disconnect is..."
}}

This output will be used to inform future analysis, so be concise and specific.
"""
        # Low temperature for structured, precise data extraction.
        return retry_with_backoff(
            lambda: self.model_low_temp.generate_content(prompt).text
        )
    
    def completeness_check(self, section: Dict, draft: str) -> str:
        """Step 2: Check what's missing from the draft by comparing against source documents."""
        
        word_count = len(draft.split())
        
        # Skip for Section 32
        if section['number'] == self.SECTION_32_EXEMPT:
            return "No additions needed - data appendix section."
        
        prompt = f"""You are a meticulous completeness auditor. Your job is to identify EXACTLY what is missing from this draft by comparing it against the source documents.

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

Compare the draft against both the requirements AND the source documents. Create a specific ADD list of data that exists in the sources but is missing from the draft:

ADD LIST FORMAT:
- [CRITICAL] Specific item missing (e.g., "Employee count by region table for 2023-2024 from Annual Report p.47")
- [IMPORTANT] Specific item missing (e.g., "Capacity utilization 72% Q3 2024 from Earnings Call p.12")  
- [USEFUL] Specific item missing (e.g., "Breakdown of leased vs owned facilities from 10-K p.89")

RULES:
1. Be EXTREMELY specific - include the exact data point and source location
2. Only list items that actually exist in the source documents
3. Prioritize items that would most improve the analysis quality
4. Each item should be implementable in 1-3 sentences or a small table
5. Focus on factual data, not interpretations

Output ONLY the ADD list. No preamble or explanation."""
        
        return retry_with_backoff(
            lambda: self.model_low_temp.generate_content(prompt).text
        )
    
    def scope_check(self, section: Dict, draft: str) -> str:
        """Step 3: Check what should be removed from the draft (out of scope)."""
        
        # Skip for Section 32
        if section['number'] == self.SECTION_32_EXEMPT:
            return "No removals needed - data appendix section."
        
        prompt = f"""You are a strict scope enforcer. Your job is to identify content that does NOT belong in this section.

SECTION {section['number']}: {section['title']}

REQUIREMENTS:
{section['specs']}

OTHER SECTIONS AVAILABLE:
{self._get_other_sections_summary()}

CURRENT DRAFT:
---
{draft}
---

Analyze the draft and create a specific REMOVE list:

REMOVE LIST FORMAT:
- Paragraph [X]: "[First 10 words...]" - Belongs in Section [Y] (reason)
- Lines [X-Y]: "[First 10 words...]" - Out of scope (reason)
- Table about [topic]: Belongs in Section [Z]

RULES:
1. Identify content that belongs in OTHER sections
2. Identify content that is out of scope entirely
3. Be specific about location (paragraph number or line range)
4. Include the first few words to ensure correct identification
5. Explain WHERE it belongs or WHY it's out of scope

Output ONLY the REMOVE list. No preamble or explanation."""
        
        return retry_with_backoff(
            lambda: self.model_low_temp.generate_content(prompt).text
        )
    
    def apply_completeness_and_scope(self, section: Dict, current_draft: str, add_list: str, remove_list: str) -> str:
        """Apply the ADD and REMOVE lists to create an improved draft."""
        
        prompt = f"""You are a precise editor. Apply the following changes to create an improved draft.

CURRENT DRAFT:
---
{current_draft}
---

ADD THESE ITEMS:
---
{add_list}
---

REMOVE THESE ITEMS:
---
{remove_list}
---

SOURCE DOCUMENTS (for looking up ADD items):
{self.full_context}

INSTRUCTIONS:
1. Add ALL items from the ADD list using the exact data from source documents
2. Remove ALL items from the REMOVE list
3. When adding items, cite the source (e.g., [1], [2])
4. Maintain narrative flow - integrate additions smoothly
5. Preserve all existing content not marked for removal
6. Keep the same professional tone and formatting

Generate the complete revised draft with all changes applied."""
        
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text
        )
    
    def _get_other_sections_summary(self) -> str:
        """Get a summary of all other sections for scope checking."""
        summary_lines = []
        for s in sections:
            summary_lines.append(f"Section {s['number']}: {s['title']}")
        return "\n".join(summary_lines) 
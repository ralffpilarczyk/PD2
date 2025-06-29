import google.generativeai as genai
from typing import Dict
import re


class CoreAnalyzer:
    """Handles the core analysis pipeline - drafts, critiques, and learning extraction"""
    
    # Progressive word limit configuration
    INITIAL_WORDS = 1000      # Initial draft + completeness critique target
    INSIGHT_WORDS = 700       # Insight critique target - distill key patterns
    POLISH_WORDS = 500        # Polish critique target - final concise output
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
        """Step 1: Create initial comprehensive draft"""
        
        # Check if this is Section 32 (Appendix) - different approach needed
        if section['number'] == self.SECTION_32_EXEMPT:
            prompt = f"""You are a data organizer. Create a comprehensive data appendix for this section.

SECTION {section['number']}: {section['title']}

REQUIREMENTS:
{section['specs']}

DOCUMENTS:
{self.full_context}

Create a clean, well-organized appendix that:
1. Extracts ALL tables and structured data from the documents
2. Organizes tables logically (operational → financial → governance → miscellaneous)
3. Formats tables properly in Markdown
4. Includes clear source references for each table
5. NO ANALYSIS - just organize the raw data tables

CRITICAL: Do NOT provide any analysis, insights, or commentary. This is purely a data reference tool.
Focus on comprehensive data extraction and logical organization.
Output in clean Markdown format with clear section headers and properly formatted tables.
"""
        else:
            prompt = f"""You are an expert business analyst. Create a comprehensive analysis for this section.

SECTION {section['number']}: {section['title']}

REQUIREMENTS:
{section['specs']}

{relevant_memory}

CRITICAL STANCE - Management documents are inherently biased toward positive presentation:
- Challenge optimistic claims with data
- Look for what's NOT being said  
- Question timing of positive announcements
- Identify potential red flags or risks being downplayed
- Identify areas of disconnect between what the numbers say and what the text says

DOCUMENTS:
{self.full_context}

Create a thorough analysis that addresses all requirements. Focus on:
1. Extracting all relevant data points with precise citations
2. Identifying patterns and relationships in the data
3. Providing business insights and implications
4. Maintaining analytical rigor throughout

MANDATORY TABLE REQUIREMENT:
Include at least a small, well-formatted table that presents the most relevant numbers for this section. The table should:
- Include proper headers and source references
- Be formatted clearly in Markdown table syntax
- Focus on the most important numerical data that supports your analysis

CONSTRAINT: Target {self.INITIAL_WORDS} words (except Section {self.SECTION_32_EXEMPT} which organizes data without word limit).
Be comprehensive in this initial phase - capture all relevant facts and insights.
Output in clean Markdown format with proper headers, tables, and bullet points.
"""
        
        # Use medium temperature for initial draft - balanced comprehensive analysis
        response = self.model_medium_temp.generate_content(prompt)
        return response.text
    
    def completeness_critique(self, section: Dict, draft: str) -> str:
        """Step 2: Critique completeness against requirements"""
        
        # Count words in current draft
        word_count = len(draft.split())
        
        # Check if this is Section 32 (Appendix) - different requirements
        if section['number'] == self.SECTION_32_EXEMPT:
            prompt = f"""Critique this appendix for DATA COMPLETENESS against the specific requirements.

APPENDIX REQUIREMENTS:
{section['specs']}

DRAFT APPENDIX:
{draft}

CURRENT WORD COUNT: {word_count} words
NOTE: This is Section {self.SECTION_32_EXEMPT} (Appendix) - NO WORD LIMIT applies.

APPENDIX-SPECIFIC EVALUATION:
1. DATA COMPLETENESS: Are all tables and data from the source documents included?
2. ORGANIZATION: Are tables logically grouped (financials, operational, governance, etc.)?
3. FORMAT: Are tables properly formatted in Markdown?
4. SOURCES: Are table sources clearly noted?
5. UNWANTED CONTENT: Are there analytical narratives that should be removed?

For each gap identified, be specific about:
- What tables or data are missing
- How to improve the organization structure
- Any analytical content that should be removed

Focus on making this a comprehensive data reference appendix."""
        else:
            prompt = f"""Critique this analysis for COMPLETENESS against the specific requirements.

ORIGINAL REQUIREMENTS:
{section['specs']}

DRAFT ANALYSIS:
{draft}

CURRENT WORD COUNT: {word_count} words
TARGET: {self.INITIAL_WORDS} words (COMPLETENESS PHASE - capture everything important)

COMPLETENESS FOCUS - This is the comprehensive phase:
- Prioritize capturing ALL required elements over brevity
- Better to be slightly over word count than miss critical requirements
- Next phases will distill and refine the content

Evaluate systematically:
1. REQUIRED DATA POINTS: Are all specified metrics, time periods, and data elements included?
2. COVERAGE GAPS: What specific requirements are missing or inadequately addressed?
3. SOURCE CITATIONS: Are all claims properly sourced with document references?
4. TIME PERIODS: Are the proposed historical data requirements met?
5. FORMAT REQUIREMENTS: Are tables, bullet points, etc. used as specified?
6. TABLE REQUIREMENT: Is there at least one small, well-formatted table with the most relevant numbers for this section?
7. COMPREHENSIVE CAPTURE: What additional relevant insights or data points would strengthen the analysis?

For each gap identified, be specific about:
- What exactly is missing
- Where it should be found in the source documents  
- How critical this gap is to meeting requirements
- What additional relevant insights could be captured

TABLE FOCUS: Include at least one small table with the most relevant numbers for this section. Keep it concise but focused on the key metrics that matter most for this specific section.

COMPREHENSIVE GOAL: This completeness phase should capture all material facts and requirements. 
Subsequent phases will handle distillation and refinement. Focus on thoroughness over brevity."""
        
        # Use low temperature for completeness critique - systematic, methodical analysis
        response = self.model_low_temp.generate_content(prompt)
        return response.text
    
    def insight_critique(self, section: Dict, draft: str) -> str:
        """Step 3: Deep insight critique using What-Why-So What framework"""
        
        # Count words in current draft
        word_count = len(draft.split())
        
        # Check if this is Section 32 (Appendix) - no analytical insights needed
        if section['number'] == self.SECTION_32_EXEMPT:
            prompt = f"""Critique this appendix for DATA ORGANIZATION and PRESENTATION.

APPENDIX TO CRITIQUE:
{draft}

CURRENT WORD COUNT: {word_count} words
NOTE: This is Section {self.SECTION_32_EXEMPT} (Appendix) - NO ANALYTICAL INSIGHTS needed.

APPENDIX-SPECIFIC FOCUS:
This should be a clean data reference tool, not an analytical document.

Evaluate:
1. PURE DATA FOCUS: Remove any analytical narratives, insights, or "What-Why-So What" content
2. TABLE ORGANIZATION: Are tables logically sequenced (operational → financial → governance)?
3. DATA INTEGRITY: Are all important tables from source documents included?
4. CLEAN FORMAT: Are tables properly formatted in Markdown with clear headers?
5. REFERENCE UTILITY: Is this easy to use as a data lookup tool?

Focus on:
- Removing all analytical content and keeping only raw data tables
- Improving table formatting and organization
- Making this a comprehensive data reference appendix
- Ensuring tables are complete and well-sourced

The goal is a clean, organized collection of data tables - not analysis."""
        else:
            prompt = f"""Conduct a deep analytical critique of this business analysis using investigative thinking.

ANALYSIS TO CRITIQUE:
{draft}

CURRENT WORD COUNT: {word_count} words
TARGET: {self.INSIGHT_WORDS} words (INSIGHT PHASE - distill to key analytical patterns)

DISTILLATION FOCUS - This is the analytical thinking phase:
- Move from comprehensive capture to focused insight extraction
- Identify the most important analytical patterns and relationships
- Cut lower-value content to make room for deeper insights
- Next phase will further distill to final concise output

CRITICAL PERSPECTIVE - Remember management documents are biased:
- What positive claims lack supporting evidence?
- What negative information might be minimized or omitted?
- Where do the numbers contradict the narrative?
- What context is missing that could change the interpretation?

Apply the What-Why-So What framework systematically:

1. WHAT (Pattern Detection):
   - Scan for mathematical/logical anomalies (revenue up, margins down, unit economics change, etc.)
   - Identify temporal inconsistencies (guidance vs results?)
   - Spot cross-metric relationships that need explanation
   - Find strategic disconnects (says vs does?)
   - Look for numbers that contradict management narrative

2. WHY (Investigation):
   - For each unusual pattern: WHY did this happen? What's the business logic?
   - Are there hidden relationships not explored?
   - What explanations are missing that an experienced analyst would ask?
   - What cross-document inconsistencies need reconciliation?
   - What isn't management telling us?

3. SO WHAT (Implication):
   - For each unexplained pattern: What business risk or opportunity does this create?
   - What are the investment/strategic implications?
   - What would a skeptical investor want to know?
   - What material insights are missing?
   - What red flags should buyers investigate further?

Focus on logical gaps that would make an experienced business analyst say "wait, that doesn't make sense" or "you need to explain why..."

INSIGHT DISTILLATION APPROACH:
- Focus on the 2-3 most material analytical insights that truly matter
- Cut routine observations to make room for breakthrough findings
- Prioritize insights that change how you view the business
- Eliminate repetitive or less impactful content to reach {self.INSIGHT_WORDS} words

TABLE EVALUATION: Does the analysis include at least one small, focused table with the MOST RELEVANT numbers for this section? Evaluate:
- What specific metrics should be tabulated for maximum clarity and relevance
- How time periods should be organized (columns for years/quarters)
- What comparisons would be most valuable in table format
- If multiple tables exist, which one is most essential for this section's purpose

Identify specific analytical improvements needed while targeting {self.INSIGHT_WORDS} words."""
        
        # Use high temperature for insight critique - creative breakthrough thinking
        response = self.model_high_temp.generate_content(prompt)
        return response.text
    
    def polish_critique(self, section: Dict, draft: str) -> str:
        """Step 4: Critique for clarity and conciseness"""
        
        # Count words in current draft
        word_count = len(draft.split())
        
        # Check if this section is exempt from word limits
        if section['number'] == self.SECTION_32_EXEMPT:
            prompt = f"""Critique this appendix for CLARITY and ORGANIZATION.

APPENDIX ANALYSIS:
{draft}

CURRENT WORD COUNT: {word_count} words
NOTE: This is Section {self.SECTION_32_EXEMPT} (Appendix) - NO WORD LIMIT applies.

APPENDIX-SPECIFIC REQUIREMENTS:
1. PRESERVE ALL TABLES: Tables and data should be maintained, not removed
2. ORGANIZE LOGICALLY: Ensure proper structure and flow between sections
3. CLARITY: Improve readability without losing data
4. REMOVE EXECUTIVE SUMMARY: Appendices should not have executive summaries

Evaluate:
1. ORGANIZATION: Are tables and data logically grouped?
2. COMPLETENESS: Are all important data tables included?
3. CLARITY: Is the structure easy to navigate?
4. FORMAT: Are tables properly formatted and readable?

Focus on:
- Improving organization and flow
- Ensuring all tables are properly formatted
- Removing any executive summary or narrative sections
- Making the appendix a clean data reference tool"""
        else:
            prompt = f"""Critique this analysis for CLARITY and EFFICIENCY.

ANALYSIS:
{draft}

CURRENT WORD COUNT: {word_count} words
TARGET: {self.POLISH_WORDS} words (FINAL POLISH PHASE - maximum impact per word)

DISTILLATION FOCUS - This is the final refinement phase:
- Ruthlessly prioritize only the most impactful insights
- Every sentence must deliver significant analytical value
- Cut redundant content while preserving core insights
- Optimize for clarity and conciseness

CRITICAL REQUIREMENTS:
1. STRICT LENGTH: Must reach {self.POLISH_WORDS} words - provide specific cuts to achieve this
2. PRIORITIZE: Keep only the most important insights and data points
3. CUT: Redundant explanations, excessive examples, verbose phrasing
4. PRESERVE: Key data points, sources, and breakthrough insights

Evaluate:
1. LENGTH: Specific guidance to reach {self.POLISH_WORDS} words - what must be cut?
2. IMPACT: Are the highest-value insights prominently featured?
3. EFFICIENCY: What redundant content can be removed without losing analytical power?
4. CLARITY: Is the logic flow clear and well-structured?
5. TABLE FOCUS: One small, essential table with the most critical numbers for this section

Focus on:
- AGGRESSIVE CUTTING to reach {self.POLISH_WORDS} words exactly
- Removing verbose explanations while keeping insights
- Prioritizing breakthrough findings over routine observations
- Maximum analytical impact per word
- One small, focused table with the most essential metrics

Provide specific guidance on cuts needed to reach {self.POLISH_WORDS} words."""
        
        # Use medium temperature for polish critique - balanced refinement
        response = self.model_medium_temp.generate_content(prompt)
        return response.text
    
    def apply_critique(self, section: Dict, current_draft: str, critique: str, critique_type: str) -> str:
        """Apply a specific critique to improve the current draft"""
        
        # Check if this is the exempt section (no word limit)
        if section['number'] == self.SECTION_32_EXEMPT:
            word_constraint = ""
        else:
            # Determine word target based on critique stage
            if critique_type == "completeness":
                target_words = self.INITIAL_WORDS
                phase_guidance = "COMPLETENESS PHASE: Be comprehensive - capture all required elements."
            elif critique_type == "insight":
                target_words = self.INSIGHT_WORDS
                phase_guidance = "INSIGHT PHASE: Distill to key analytical patterns and relationships."
            elif critique_type == "polish":
                target_words = self.POLISH_WORDS
                phase_guidance = "POLISH PHASE: Final distillation - maximum impact per word."
            else:
                target_words = self.POLISH_WORDS  # Default fallback
                phase_guidance = "FINAL PHASE: Concise and impactful."
            
            word_constraint = f"\nCONSTRAINT: Target {target_words} words - {phase_guidance}"
            word_constraint += "\nTABLE REQUIREMENT: Ensure at least a small, well-formatted table with key numbers is included."
        
        prompt = f"""REVISE (do not rewrite) this analysis by applying the {critique_type} critique feedback.

CURRENT ANALYSIS TO REVISE:
{current_draft}

{critique_type.upper()} CRITIQUE FEEDBACK:
{critique}

SOURCE DOCUMENTS (only for implementing specific feedback):
{self.full_context}

CRITICAL REVISION APPROACH:
- START with the current analysis above as your foundation
- KEEP all valuable content, insights, and structure from the current analysis
- ONLY make targeted improvements based on the specific critique feedback
- DO NOT start over or create an entirely new analysis

CRITICAL STANCE - Maintain skeptical analysis:
- Challenge management claims with data
- Highlight disconnects between numbers and narrative
- Identify what's missing or downplayed

Instructions:
1. Address all specific issues raised in the critique
2. PRESERVE the existing analysis structure and all valuable content
3. ADD missing elements identified in the critique - use source documents ONLY to find specific data points mentioned in the critique
4. ENHANCE the analysis based on feedback - improve what exists rather than replacing it
5. For {critique_type} phase: Balance enhancement with appropriate distillation{word_constraint}

REVISION APPROACH: This is about improving and enhancing the existing analysis, not creating a new one. The source documents are provided only to help you implement specific critique feedback that requires additional data.

Output the REVISED analysis in clean Markdown format."""
        
        # Select appropriate temperature model based on critique type
        if critique_type == "completeness":
            # Low temperature for systematic implementation of completeness feedback
            model = self.model_low_temp
        elif critique_type == "insight":
            # High temperature for creative implementation of insight feedback
            model = self.model_high_temp
        elif critique_type == "polish":
            # Medium temperature for balanced implementation of polish feedback
            model = self.model_medium_temp
        else:
            # Default to medium temperature
            model = self.model_medium_temp
        
        response = model.generate_content(prompt)
        return response.text
    
    def extract_learning(self, section: Dict, final_output: str) -> str:
        """Step 5: Extract analytical instruction insights for memory"""
        
        section_num = section['number']
        section_title = section['title']
        section_specs = section['specs']
        
        prompt = f"""Extract ANALYTICAL INSTRUCTION patterns from this Section {section_num} analysis.

SECTION {section_num}: {section_title}

EXISTING SECTION REQUIREMENTS (ANALYZE FOR OVERLAP):
{section_specs}

ANALYSIS OUTPUT THAT WAS PRODUCED:
{final_output}

OVERLAP IDENTIFICATION GUIDE:
The section specs above already require certain analytical approaches. You must identify these and NOT repeat them as "insights."

EXAMPLES OF OVERLAP TO AVOID:
If section specs say "note any anomalies" → DON'T generate "Look for anomalies in the data"
If section specs say "benchmark against industry" → DON'T generate "Compare with industry benchmarks"  
If section specs say "analyze trends over 3 years" → DON'T generate "Examine historical trends"

WHAT QUALIFIES AS ADDITIVE (Non-overlapping):
1. SPECIFIC TECHNIQUES not mentioned in specs:
   - Particular ratio calculations (incl unit economics)or mathematical approaches
   - Specific cross-referencing methods between data sources
   - Advanced investigative techniques that go well beyond basic requirements

2. RELATIONSHIP PATTERNS discovered during analysis:
   - Specific correlations that proved material (e.g., "When X increases but Y decreases, investigate Z")
   - Cause-effect relationships that weren't obvious from section specs
   - Counter-intuitive connections that revealed hidden insights

3. ADVANCED VALIDATION METHODS:
   - Specific ways to verify management claims beyond what specs require
   - Find areas of disconnect between what the numbers say and what the text says
   - Particular red flags or warning signs to watch for
   - Advanced quality checks not covered in basic requirements
   - Consider scenarios where one of the most important company's strengths could turn into a weakness

CROSS-INDUSTRY UNIVERSALITY REQUIREMENT:
Every instruction must be applicable across different industries and company types:
- Avoid industry-specific jargon (e.g., "manufacturing capacity" → "operational capacity")
- Use universal business concepts (revenue, margins, employees, customers, unit economics,etc.)
- Focus on universal analytical techniques, not sector-specific approaches
- Instructions should work for tech companies, manufacturing, services, retail, financial services,etc.

UNIVERSALITY TEST FOR EACH INSTRUCTION:
1. Would this technique apply to a tech company? A manufacturer? A retailer? A bank?
2. Does this use universal business metrics rather than industry-specific ones?
3. Is the analytical approach broadly applicable across sectors?
4. Does this avoid company-specific or industry-specific terminology?

WORD LIMIT ENFORCEMENT:
- Maximum 30 words per instruction (strictly enforced)
- Be extremely concise - every word must add value
- Remove unnecessary articles, prepositions where possible
- Focus on core technique and insight

OVERLAP TEST FOR EACH POTENTIAL INSTRUCTION:
Before including any instruction, ask:
1. Does the section spec already ask for this general approach?
2. Is this just a restatement of existing requirements?
3. Does this add a specific technique or insight beyond the basic specs?
4. Would an analyst following the section specs already do this?

Only include instructions that pass this overlap test.

QUALITY CALIBRATION FOR ADDITIVE INSTRUCTIONS:
- 10/10: Specific advanced techniques that provide deep and material insights well beyond what's apparent (completely new approaches)
- 9/10: Non-obvious investigative methods that consistently reveal material insights
- 8/10: Specific enhancements to basic analytical approaches  
- 6-7/10: Useful specific techniques that supplement basic requirements
- 1-5/10: Anything that overlaps with or restates section requirements

EXAMPLES OF PROPER ADDITIVE INSTRUCTIONS:
GOOD (Additive + Universal): "When efficiency claims made, calculate labor costs/revenue quarterly - increases during 'efficiency' expose false claims"
- Universal metrics (labor costs, revenue), applicable across industries, 16 words

BAD (Overlap): "When analyzing efficiency, look for disconnects between claims and data"  
- This just restates basic analytical skepticism already required

GOOD (Additive + Universal): "When customer concentration mentioned, calculate top-3 revenue share - exceeding 50% indicates dangerous dependency"
- Universal concept (customer concentration), specific threshold, applicable across sectors, 14 words

BAD (Not Universal): "When production capacity discussed, analyze utilization rates against order backlog"
- Too manufacturing-specific, wouldn't apply to service companies

FORMAT REQUIREMENT:
- instruction: "When [universal trigger], [specific technique] - [universal insight]"
- section_number: {section_num}
- quality_score: [1-10, most should be 4-7, only breakthrough techniques get 9-10]
- word_count: [must be ≤30 words]

CRITICAL FILTERS:
1. Must be SPECIFIC (not generic analytical advice)
2. Must be ADDITIVE (goes beyond section requirements)  
3. Must be UNIVERSAL (applies across industries and company types)
4. Must be CONCISE (≤30 words maximum)
5. Must be ACTIONABLE (provides concrete technique)
6. Must be VALIDATED (emerged from this actual analysis)

OUTPUT FORMAT:
ANALYTICAL_INSTRUCTIONS:
- instruction: "[specific universal technique ≤30 words]"
  section_number: {section_num}
  quality_score: [1-10]
  word_count: [actual word count, must be ≤30]

Expect to generate 0-2 instructions total - most potential instructions will fail the overlap, universality, or word limit tests.
Each instruction must be universally applicable across industries and sectors.
"""
        
        # Use medium temperature for learning extraction - balanced analysis
        response = self.model_medium_temp.generate_content(prompt)  
        return response.text 
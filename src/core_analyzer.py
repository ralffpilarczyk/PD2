import google.generativeai as genai
from typing import Dict
import re
from .utils import retry_with_backoff


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
- Avoid redundant columns (e.g., don't include "Most Recent Date" if dates are clear from column headers)
- Be efficient - only include rows and columns that add meaningful information

FOOTNOTE RULES:
- Maximum 5-8 footnotes per section
- Only footnote: specific numbers from sources, direct quotes, non-obvious claims
- DO NOT footnote: general observations, industry facts, your own analysis
- Format: <sup>(1)</sup> inline, list footnotes at section end
- No square bracket citations

CONSTRAINT: Target {self.INITIAL_WORDS} words (except Section {self.SECTION_32_EXEMPT} which organizes data without word limit).
Be comprehensive in this initial phase - capture all relevant facts and insights.
Output in clean Markdown format with proper headers, tables, and bullet points.
"""
        
        # Use medium temperature for initial draft - balanced comprehensive analysis
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text
        )
    
    def completeness_critique(self, section: Dict, draft: str) -> str:
        """Step 2: Critique for completeness against section specifications with materiality assessment"""
        
        word_count = len(draft.split())
        
        # Check if this section is exempt from word limits
        if section['number'] == self.SECTION_32_EXEMPT:
            prompt = f"""Critique this appendix for COMPLETENESS of essential data.

APPENDIX ANALYSIS:
{draft}

CURRENT WORD COUNT: {word_count} words
NOTE: This is Section {self.SECTION_32_EXEMPT} (Appendix) - NO WORD LIMIT applies.

Focus on ensuring all critical data tables and reference information are included."""
        else:
            prompt = f"""Critique this analysis for COMPLETENESS against the section specifications.

ORIGINAL SECTION SPECIFICATIONS (MUST STAY WITHIN SCOPE):
{section['specs']}

CRITICAL SCOPE BOUNDARY: Your critique must only suggest completeness improvements that ALIGN with the section specifications above.

ANALYSIS TO CRITIQUE:
{draft}

CURRENT WORD COUNT: {word_count} words
TARGET: {self.INITIAL_WORDS} words (INITIAL PHASE - comprehensive capture)

COMPLETENESS ASSESSMENT:
Identify specific elements required by the section specifications that are missing or incomplete.

MATERIALITY ASSESSMENT:
Review each fact/claim in the draft. Consider marking items where ANY of these apply:
- Different outcome would be material to company's prospects → [MARK: MATERIAL_IMPACT]
- Trend reversal would trigger strategic shift → [MARK: TREND_REVERSAL]
- Root cause unknown but outcome material → [MARK: ROOT_CAUSE_MISSING]

For each mark, provide brief explanation in format:
[MARK: MATERIAL_IMPACT] "Debt restructuring - could affect access to growth capital"

CRITICAL EVALUATION AREAS:
1. REQUIRED ELEMENTS: What specific elements from the section specifications are missing?
2. DATA GAPS: What critical data points or metrics are absent?
3. SOURCE VERIFICATION: Are claims properly supported and sourced?
4. MATERIALITY: Which facts have material impact on company prospects?
5. ANALYTICAL DEPTH: Where does analysis need deeper exploration within section scope?

Focus exclusively on:
- Missing elements specifically required by the section specifications
- Inadequate coverage of specified analytical requirements
- Gaps in required data, metrics, or evidence
- Materiality assessment to focus on what truly matters
- Areas needing more analytical depth within the defined scope

DO NOT suggest:
- Adding content that belongs in other sections
- Removing content that the section specifications require
- Analysis beyond the scope defined in the section specifications

Provide specific, actionable guidance for achieving completeness within the section scope while identifying material facts that warrant focus."""

        # Use low temperature for systematic completeness evaluation
        return retry_with_backoff(
            lambda: self.model_low_temp.generate_content(prompt).text
        )
    
    def insight_critique(self, section: Dict, draft: str) -> str:
        """Step 3: Critique for analytical depth and breakthrough insight"""
        
        word_count = len(draft.split())
        
        # Check if this is the exempt section
        if section['number'] == self.SECTION_32_EXEMPT:
            prompt = f"""This is Section {self.SECTION_32_EXEMPT} (Appendix) - focus on data organization rather than insights.

APPENDIX:
{draft}

Evaluate organization and completeness of data tables only."""
        else:
            prompt = f"""Critique this analysis for STRATEGIC INSIGHT and breakthrough thinking.

ORIGINAL SECTION SPECIFICATIONS (MUST STAY WITHIN SCOPE):
{section['specs']}

CRITICAL SCOPE BOUNDARY: Your critique must only suggest analytical approaches that ALIGN with the section specifications above.
DO NOT suggest insights or analysis that belongs in other sections.

ANALYSIS TO CRITIQUE:
{draft}

CURRENT WORD COUNT: {word_count} words
TARGET: {self.INSIGHT_WORDS} words (INSIGHT PHASE - distill key patterns)

INSIGHT DEPTH ASSESSMENT - This is the analytical breakthrough phase:
Look for opportunities to transform facts into strategic insights WITHIN the section scope.
Focus on non-obvious patterns, relationships, and implications that reveal competitive dynamics or material risks.

Critical evaluation areas:
1. ANALYTICAL DEPTH: Does the analysis explain WHY things are happening, not just WHAT?
2. NON-OBVIOUS CONNECTIONS: Are there hidden relationships between data points within this section?
3. COMPETITIVE IMPLICATIONS: What does this data reveal about competitive positioning or dynamics?
4. STRATEGIC RISKS/OPPORTUNITIES: What are the forward-looking implications within this section's domain?
5. MANAGEMENT VS REALITY: Are there disconnects between management claims and actual data?

SOURCE ANALYSIS DEPTH:
{self.full_context}

INSIGHT FOCUS AREAS (within section scope):
- What patterns emerge when connecting different data points in this section?
- What competitive dynamics or market forces are revealed by the data?
- What strategic vulnerabilities or advantages become apparent?
- Where do the numbers tell a different story than management's narrative?
- What are the second-order effects or implications of the trends shown?
- What comparisons would be most valuable in table format FOR THIS SECTION'S PURPOSE
- If multiple tables exist, which one is most essential for this section's purpose

Identify specific analytical improvements needed while targeting {self.INSIGHT_WORDS} words and staying strictly within the section specifications."""
        
        # Use high temperature for insight critique - creative breakthrough thinking
        return retry_with_backoff(
            lambda: self.model_high_temp.generate_content(prompt).text
        )

    def insight_testing(self, section: Dict, draft: str) -> str:
        """Step 4A: Systematic testing of insights using the 3-question framework"""
        
        # Check if this is the exempt section
        if section['number'] == self.SECTION_32_EXEMPT:
            return "Insight testing not applicable for appendix section."
        
        prompt = f"""Test each strategic insight in this analysis using systematic boundary examination.

ORIGINAL SECTION SPECIFICATIONS (MUST STAY WITHIN SCOPE):
{section['specs']}

ANALYSIS TO TEST:
{draft}

INSIGHT TESTING FRAMEWORK:
For each statement that reveals hidden relationships or strategic implications within this section:

Answer these three questions explicitly:
1. "What breaks this?" (identify the boundary condition where this insight fails)
2. "What must stay true for this to continue?" (identify critical dependencies)
3. "Who loses if this succeeds?" (identify second-order effects and competitive impacts)

OUTPUT FORMAT REQUIRED:
For each testable insight, use this exact structure:

[INSIGHT]: [State the specific insight being tested]
- Breaks when: [Specific condition (ideally underpinned by numbers) that would invalidate this insight]
- Depends on: [Critical factors that must remain true]
- Losers: [Who gets negatively impacted if this insight plays out]

TESTING CRITERIA:
- Focus only on insights with strategic or competitive implications within this section's scope
- Skip obvious facts or standard industry observations
- Examine insights that could materially impact company prospects
- Look for insights about competitive positioning, market dynamics, or operational leverage

EXAMPLE FORMAT:
[INSIGHT]: Subscription model drives predictable revenue growth
- Breaks when: Market penetration reaches 40% of addressable market due to saturation
- Depends on: Customer acquisition cost staying below £50 and monthly churn under 5%
- Losers: Traditional retail channels seeing 20% volume decline

Test only insights that reveal important non-obvious relationships or competitive dynamics within this section's scope."""

        # Use medium temperature for systematic testing
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text
        )

    def insight_marking(self, section: Dict, draft: str, insight_testing_results: str) -> str:
        """Step 4B: Evaluate and mark insights based on testing results"""
        
        # Check if this is the exempt section
        if section['number'] == self.SECTION_32_EXEMPT:
            return "Insight marking not applicable for appendix section."
        
        prompt = f"""Evaluate the insight testing results to identify which insights merit expansion.

ORIGINAL SECTION SPECIFICATIONS (MUST STAY WITHIN SCOPE):
{section['specs']}

CURRENT ANALYSIS:
{draft}

INSIGHT TESTING RESULTS:
{insight_testing_results}

INSIGHT EVALUATION:
Based on the test answers from the insight testing, mark each insight:

- If testing reveals non-obvious vulnerabilities, dependencies, or impacts → [MARK: KEEP_EXPAND]
- If testing shows obvious, generic, or well-known patterns → [MARK: REMOVE_WEAK]

MARKING CRITERIA:
[MARK: KEEP_EXPAND] for insights where testing reveals:
- Non-obvious boundary conditions or failure points
- Specific dependencies that aren't immediately apparent
- Unexpected competitive or market impacts
- Material risks or opportunities not obvious from surface data

[MARK: REMOVE_WEAK] for insights where testing shows:
- Obvious or predictable patterns
- Generic industry trends without company-specific implications
- Dependencies that are self-evident
- Impacts that are standard competitive effects

OUTPUT FORMAT:
For each tested insight, provide:
[MARK: KEEP_EXPAND] - [Brief reasoning why this insight has non-obvious value]
OR
[MARK: REMOVE_WEAK] - [Brief reasoning why this insight is too obvious]

EXAMPLE:
[MARK: KEEP_EXPAND] - Market ceiling at 40% not obvious from current 15% penetration, specific cost thresholds reveal scalability limits
[MARK: REMOVE_WEAK] - Standard retail displacement pattern, no unique competitive insight

Focus on preserving insights that reveal hidden competitive dynamics or material risks within this section's scope."""

        # Use medium temperature for balanced evaluation
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text
        )
    
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

ORIGINAL SECTION SPECIFICATIONS (MUST STAY WITHIN SCOPE):
{section['specs']}

CRITICAL SCOPE BOUNDARY: Your critique must only suggest improvements that ALIGN with the section specifications above.
DO NOT suggest removing content that the section specifications require or adding content beyond the specified scope.

ANALYSIS:
{draft}

CURRENT WORD COUNT: {word_count} words
TARGET: {self.POLISH_WORDS} words (FINAL POLISH PHASE - maximum impact per word)

DISTILLATION FOCUS - This is the final refinement phase:
- Ruthlessly prioritize only the most impactful insights WITHIN the section specifications
- Every sentence must deliver significant analytical value FOR THIS SECTION'S PURPOSE
- Cut redundant content while preserving core insights required by the section specifications
- Optimize for clarity and conciseness WITHIN the specified scope

CRITICAL REQUIREMENTS:
1. SECTION SCOPE COMPLIANCE: Ensure all required elements from section specifications are preserved
2. STRICT LENGTH: Must reach {self.POLISH_WORDS} words - provide specific cuts to achieve this
3. PRIORITIZE: Keep only the most important insights and data points REQUIRED by the section specifications
4. CUT: Redundant explanations, excessive examples, verbose phrasing
5. PRESERVE: Key data points, sources, and breakthrough insights that align with section requirements

Evaluate:
1. SCOPE COMPLIANCE: Does the analysis still cover all requirements from the section specifications?
2. LENGTH: Specific guidance to reach {self.POLISH_WORDS} words - what must be cut?
3. IMPACT: Are the highest-value insights required by this section prominently featured?
4. EFFICIENCY: What redundant content can be removed without losing analytical power WITHIN the section scope?
5. CLARITY: Is the logic flow clear and well-structured?
6. TABLE FOCUS: One small, essential table with the most critical numbers for this section

SCOPE PROTECTION:
- Do not suggest removing content that directly fulfills section specifications
- Do not suggest adding content that belongs in other sections
- Focus cuts on redundancy, not required section elements

Focus on:
- AGGRESSIVE CUTTING to reach {self.POLISH_WORDS} words exactly WHILE preserving section requirements
- Removing verbose explanations while keeping insights required by the section specifications
- Prioritizing breakthrough findings that align with the section's analytical purpose
- Maximum analytical impact per word WITHIN the section scope
- One small, focused table with the most essential metrics for this section

Provide specific guidance on cuts needed to reach {self.POLISH_WORDS} words while maintaining compliance with section specifications."""
        
        # Use medium temperature for polish critique - balanced refinement
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text
        )
    
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
            elif critique_type == "insight_testing":
                target_words = self.INSIGHT_WORDS
                phase_guidance = "INSIGHT TESTING PHASE: Apply systematic testing framework to validate insights."
            elif critique_type == "insight_marking":
                target_words = self.INSIGHT_WORDS
                phase_guidance = "INSIGHT MARKING PHASE: Expand strong insights, remove weak ones based on testing."
            elif critique_type == "polish":
                target_words = self.POLISH_WORDS
                phase_guidance = "POLISH PHASE: Final distillation - maximum impact per word."
            else:
                target_words = self.POLISH_WORDS  # Default fallback
                phase_guidance = "FINAL PHASE: Concise and impactful."
            
            word_constraint = f"\nCONSTRAINT: Target {target_words} words - {phase_guidance}"
            word_constraint += "\nTABLE REQUIREMENT: Ensure at least a small, well-formatted table with key numbers is included. Avoid redundant columns."
            word_constraint += "\nFOOTNOTE RULES: Maximum 5-8 footnotes per section. Only footnote:"
            word_constraint += "\n- Specific numbers from source documents"
            word_constraint += "\n- Direct quotes from sources"
            word_constraint += "\n- Non-obvious claims requiring verification"
            word_constraint += "\nDO NOT footnote general observations, industry facts, or your own analysis."
            word_constraint += "\nFORMAT: Use <sup>(1)</sup> inline, list footnotes at section end. No brackets."
            
            # Add systematic word reduction strategy
            word_constraint += "\nWORD REDUCTION STRATEGY:"
            word_constraint += "\nTo fit within target word count:"
            word_constraint += "\n1. FIRST eliminate redundant information (repetitive statements, overlapping explanations)"
            word_constraint += "\n2. THEN remove least value-add content (preserve core insights required by section specifications)"
            
            # Add marking system guidance for relevant critique types
            if critique_type in ["completeness", "insight_marking"]:
                word_constraint += "\nMARKING SYSTEM GUIDANCE:"
                word_constraint += "\n- Use [MARK: TYPE] tags to identify facts based on materiality assessment"
                word_constraint += "\n- For [MARK: MATERIAL_IMPACT] items: Add scenarios and implications"
                word_constraint += "\n- For [MARK: TREND_REVERSAL] items: Add inflection triggers and impact"
                word_constraint += "\n- For [MARK: ROOT_CAUSE_MISSING] items: Investigate underlying drivers"
                word_constraint += "\n- For [MARK: KEEP_EXPAND] items: Develop insight further with testing results"
                word_constraint += "\n- For [MARK: REMOVE_WEAK] items: Remove or significantly reduce emphasis"
        
        # Special handling for insight testing and marking
        if critique_type == "insight_testing":
            prompt = f"""APPLY the insight testing framework to this analysis.

ORIGINAL SECTION SPECIFICATIONS (CRITICAL - MUST COMPLY):
{section['specs']}

CURRENT ANALYSIS TO ENHANCE:
{current_draft}

INSIGHT TESTING FRAMEWORK:
{critique}

ENHANCEMENT APPROACH:
- KEEP all existing structure and content from the current analysis
- ENHANCE insights by adding boundary testing (what breaks, what depends, who loses)
- FOCUS on importantinsights that reveal non-obvious competitive dynamics or material risks
- REMOVE or de-emphasize insights that tested as obvious or weak
- MAINTAIN word count target while improving analytical depth{word_constraint}
- MEASURE the importance of each insight on the company's prospects and the impact of the insight on the company's prospects

CRITICAL INSTRUCTIONS:
1. Apply the 3-question framework to each strategic insight
2. Expand insights that passed testing with boundary conditions, dependencies, and impacts
3. Reduce emphasis on insights that failed testing
4. Ensure all enhancements align with section specifications
5. Maintain the existing analysis structure as foundation

Output the ENHANCED analysis with insight testing applied."""
        
        elif critique_type == "insight_marking":
            prompt = f"""APPLY the insight marking evaluation to this analysis.

ORIGINAL SECTION SPECIFICATIONS (CRITICAL - MUST COMPLY):
{section['specs']}

CURRENT ANALYSIS TO REFINE:
{current_draft}

INSIGHT MARKING EVALUATION:
{critique}

REFINEMENT APPROACH:
- EXPAND insights marked as [MARK: KEEP_EXPAND] with additional analysis
- REDUCE or REMOVE insights marked as [MARK: REMOVE_WEAK] 
- MAINTAIN section compliance and word count target
- FOCUS on insights that create competitive advantage or reveal hidden risks{word_constraint}

CRITICAL INSTRUCTIONS:
1. Significantly expand [MARK: KEEP_EXPAND] insights with deeper analysis
2. Remove or significantly reduce [MARK: REMOVE_WEAK] insights
3. Ensure all changes align with section specifications
4. Maintain the existing analysis structure as foundation

Output the REFINED analysis with insight marking applied."""
        
        else:
            # Standard critique application
            prompt = f"""REVISE (do not rewrite) this analysis by applying the {critique_type} critique feedback.

ORIGINAL SECTION SPECIFICATIONS (CRITICAL - MUST COMPLY):
{section['specs']}

CRITICAL SCOPE BOUNDARY
- You MUST stay strictly within the section specifications above
- Do NOT add content that belongs in other sections
- Do NOT remove content that the section specifications require
- Do NOT change the analytical focus beyond what the section specifications define
- The critique feedback should only be applied if it aligns with the section specifications

CURRENT ANALYSIS TO REVISE:
{current_draft}

{critique_type.upper()} CRITIQUE FEEDBACK:
{critique}

SOURCE DOCUMENTS (only for implementing specific feedback):
{self.full_context}

CRITICAL REVISION APPROACH:
- START with the current analysis above as your foundation
- KEEP all valuable content, insights, and structure from the current analysis
- ONLY make targeted improvements based on the critique feedback that ALIGN with section specifications
- REJECT any critique suggestions that would violate the section specifications
- DO NOT start over or create an entirely new analysis

SECTION SPECIFICATION COMPLIANCE CHECK:
Before implementing any critique suggestion, ask:
1. Does this change align with the section specifications?
2. Am I staying within the defined scope of this section?
3. Am I preserving all required elements from the section specifications?
4. Would this change cause the section to cover topics that belong elsewhere?

CRITICAL STANCE - Maintain skeptical analysis:
- Challenge management claims with data
- Highlight disconnects between numbers and narrative
- Identify what's missing or downplayed
- BUT only within the scope defined by the section specifications

Instructions:
1. Address critique issues that align with the section specifications
2. PRESERVE the existing analysis structure and all valuable content required by section specifications
3. ADD missing elements identified in the critique - use source documents ONLY to find specific data points mentioned in the critique that align with section specifications
4. ENHANCE the analysis based on feedback that supports the section specifications
5. IGNORE or REJECT critique suggestions that would violate the section specifications
6. For {critique_type} phase: Balance enhancement with appropriate distillation{word_constraint}

REVISION APPROACH: This is about improving and enhancing the existing analysis WITHIN the section specifications, not creating a new one. The source documents are provided only to help you implement specific critique feedback that aligns with the section requirements.

Output the REVISED analysis in clean Markdown format."""
        
        # Select appropriate temperature model based on critique type
        if critique_type == "completeness":
            # Low temperature for systematic implementation of completeness feedback
            model = self.model_low_temp
        elif critique_type in ["insight", "insight_testing", "insight_marking"]:
            # High temperature for creative implementation of insight feedback
            model = self.model_high_temp
        elif critique_type == "polish":
            # Medium temperature for balanced implementation of polish feedback
            model = self.model_medium_temp
        else:
            # Default to medium temperature
            model = self.model_medium_temp
        
        return retry_with_backoff(
            lambda: model.generate_content(prompt).text
        )
    
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
        return retry_with_backoff(
            lambda: self.model_medium_temp.generate_content(prompt).text
        ) 
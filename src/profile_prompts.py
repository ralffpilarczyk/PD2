"""
Prompt templates for OnePageProfile (OPP.py)
"""

from src.opp_sections import get_section_boundaries

# Common critical rules for all OPP sections
OPP_CRITICAL_RULES = """
Critical rules:
• Keep each sentence as concise as possible. no corporate poetry, no consulting buzz words, but chairman-friendly language
• Ideally, each sentence should have at least one number
• Do not make up information
• A precise sentence which may not perfectly suit the purpose is much more valuable than a beautifully tailored sentence which is not grounded
• Format each bullet as: **Title** (1-2 words): Rest of sentence. Example: **Revenues**: In 2024 revenues were USD81m, up 8.5% YoY.
• If there is no meaningful data to write a sentence as instructed, skip it silently - never write a bullet without specifics
• MATERIALITY RULE: When multiple numbers could describe the same concept, always use the MOST MATERIAL figure.
• RECENCY VALIDATION: If data for the same metric for more than one period, ALWAYS use the most recent one and state the fiscal period explicitly: "3,291 employees (FY2025)" not just "3,291 employees".
• ZERO INFERENCE RULE: Do not infer unstated metrics. If a metric is not in the source, skip it.
• Every metric must include proper units (%, USD, units, etc.) AND the time/time period it refers to (e.g., in parentheses)
• Never repeat the same fact twice across bullets - eliminate all redundancy
• Avoid vague qualifiers like "integrated", "leading", "comprehensive" unless quantified with proof
• Every sentence must be clear enough that a reader unfamiliar with the sector can understand it
• Plain US SEC compliant style English: Define all technical terms, industry jargon, and sector-specific language for a general business audience. Assume the reader is a smart generalist, not an industry insider.

Prose requirements:
• Every bullet must be a complete grammatical sentence with subject and verb
• No sentence fragments or comma-separated lists without connecting words
• Use proper articles (a, the) and conjunctions (and, but, while)
• Readable as standalone prose, not telegraphic shorthand
"""

# Shared formatting rules for section outputs
SECTION_FORMATTING_RULES = """
FORMATTING RULES:
• Keep bullet format with **bold** keywords (1-2 most important words per bullet)
• Each bullet is one sentence maximum
• ALWAYS put a blank line before starting any list
• Use consistent markdown formatting
• Do NOT include page numbers, footnotes, or source citations
• Output ONLY the section content - start directly with bullets
• No preamble, introduction, or explanatory text
"""

# Subtitle-specific requirements
SUBTITLE_GENERATION_RULES = """
SUBTITLE REQUIREMENTS:
• Maximum 8 words
• Must be an INVESTMENT THESIS, not a corporate tagline
• No marketing hype, no buzzwords, no superlatives without proof
• Plain US SEC compliant style English
• Full sentence in prose, not cryptic half-sentences
• No period at the end
• Professional M&A banker tone - help banker decide if deal merits review
"""

# Section boundary enforcement (template - use with format())
BOUNDARY_ENFORCEMENT_INTRO = """
SECTION BOUNDARIES - STAY FOCUSED:
This is the "{section_title}" section. DO NOT include content that belongs in other sections:
{boundaries}
"""

# Data source hierarchy (for combined research + document context)
DATA_SOURCE_HIERARCHY = """
SOURCE HIERARCHY (when multiple sources available):
PRIMARY SOURCES (highest trust):
• Uploaded company documents (annual reports, filings, investor presentations)
• Official company financial statements and disclosures
• These are authoritative for the company's own data - use over any conflicting secondary source

SECONDARY SOURCES (contextual):
• Web research, press articles, analyst reports
• Useful for: competitor info, market context, industry trends, recent news
• NOT authoritative for the company's own financials or operations
• If secondary source conflicts with primary source, USE PRIMARY and ignore secondary

CONFLICT RESOLUTION:
• Primary source data always wins for company-specific facts
• Most recent data wins when comparing same-type sources
• Note material discrepancies only if they suggest a red flag (e.g., company claims vs market perception)
"""

# Condensing and prioritization guidance
CONDENSING_PRIORITY_RULES = """
WHAT TO PRESERVE:
• Content that addresses the original requirements
• Bullets with numbers and specific facts - prioritize these over generic statements
• Specific metrics and quantified trends
• Year-over-year comparisons, growth rates, margins, percent contributions
• Key comparisons and benchmarks
• Unusual patterns or red flags
• Strategic disconnects or opportunities

WHAT TO REMOVE:
• Generic framework language unless specifically relevant
• Generic statements and obvious observations
• Repetitive points - state each insight once
• Repetition of facts already stated in other bullets (even if worded differently)
• Obvious statements without data
• Elaborate explanations - let data speak
• Content without specific numbers or hard facts
• Page numbers, footnotes, source citations
"""

COMPANY_NAME_EXTRACTION_PROMPT = """Extract the primary company name from these documents.

Look for the company name in:
- Document titles and headers
- Letterheads and footers
- About sections
- First few pages

Return ONLY the company name, nothing else. Keep it short (2-50 characters).
If unclear, return "Company Profile"."""


def get_title_subtitle_prompt(company_name: str) -> str:
    """Generate prompt for creating title and subtitle"""
    return f"""You are creating a company profile title and subtitle for M&A bankers.

CRITICAL RULES:
- Title should be the company name: {company_name}
- Subtitle should be a key message about the company for potential investors or buyers

{SUBTITLE_GENERATION_RULES}

TEST: Would this subtitle help a banker decide if deal merits further review?

OUTPUT FORMAT:
# {company_name}
Your investment-focused subtitle (up to 8 words) here

Now generate the title and subtitle based on the documents provided."""


def get_subtitle_refinement_prompt(current_title_subtitle: str, sections_context: str) -> str:
    """Generate prompt for refining subtitle after iterations

    Args:
        current_title_subtitle: Current title and subtitle text
        sections_context: Context from refined profile sections

    Returns:
        Prompt string for subtitle refinement
    """
    return f"""You are refining a company profile subtitle for M&A bankers.

CURRENT TITLE AND SUBTITLE:
{current_title_subtitle}

CONTEXT FROM REFINED PROFILE:
{sections_context if sections_context else "Not available for first refinement"}

TASK: Refine the subtitle ONLY (keep title unchanged) to be more:
1. **Concise**: Up to 8 words maximum, remove filler
2. **Investment-focused**: What makes this company attractive to acquirers?
3. **Specific**: Use metrics/market position if available in context
4. **Factual**: No marketing hype, no superlatives without proof

{SUBTITLE_GENERATION_RULES}

ADDITIONAL RULES:
• Every word must earn its place
• If you cannot make it more specific/dense, return it unchanged

OUTPUT FORMAT:
# [Title - unchanged]
[Your refined subtitle - up to 8 words]

Generate the refined version now."""


def get_section_generation_prompt(section: dict) -> str:
    """Generate the initial section generation prompt

    Args:
        section: Section dictionary from opp_sections.py

    Returns:
        Prompt string for generating this section
    """
    return f"""You are creating a company profile section for M&A bankers. Analyze the provided context and generate content for the "{section['title']}" section.

SECTION REQUIREMENTS:
{section['specs']}

{DATA_SOURCE_HIERARCHY}

{OPP_CRITICAL_RULES}

DENSITY PRIORITY:
• Pack multiple related facts into single sentences where sensible
• Every word must earn its place - no filler, no obvious observations
• Prioritize sentences with 2+ quantified facts over single-fact sentences

{BOUNDARY_ENFORCEMENT_INTRO.format(section_title=section['title'], boundaries=get_section_boundaries(section['number']))}

OUTPUT FORMAT:
## {section['title']}
[Your bullet points here, following the requirements above]

CRITICAL: Do NOT include page numbers, footnotes, or source citations in your output.

FACT-CHECKING BEFORE YOU WRITE:
• Verify every number appears in primary source documents (within reasonable rounding)
• Use the MOST RECENT fiscal period if multiple periods exist
• Use the CORRECT category if similar metrics exist
• Use local currency as stated in source
• Skip any bullet if you cannot find specific data - NEVER estimate
• If web research conflicts with uploaded documents, USE the uploaded document data

Now generate this section based on the context provided."""


def get_section_completeness_check_prompt(section: dict, section_content: str) -> str:
    """Generate completeness check prompt for a section

    Args:
        section: Section dictionary from opp_sections.py
        section_content: Current section content to check

    Returns:
        Prompt string for completeness check
    """
    return f"""You are a meticulous completeness auditor evaluating a company profile section for M&A purposes. Your job is to identify critical gaps in both DATA and INVESTOR PERSPECTIVE.

SECTION: {section['title']}

ORIGINAL REQUIREMENTS FOR THIS SECTION:
{section['specs']}

{DATA_SOURCE_HIERARCHY}

{OPP_CRITICAL_RULES}

CURRENT SECTION CONTENT:
---
{section_content}
---

SOURCE DOCUMENTS:
{{source_documents}}

YOUR DUAL ASSESSMENT:

PART A: DATA COMPLETENESS
Check if the section is missing specific data points from source documents that are required by the ORIGINAL REQUIREMENTS above.

PART B: INVESTOR PERSPECTIVE COMPLETENESS
Consider adding statements (but only if they are not already present) underpinned by numbers and supported by source documents if they meet one of the following criteria:
- They fulfill an item in the ORIGINAL REQUIREMENTS that was skipped
- They are fundamental to a reader's understanding of the company as an investment or acquisition target
- They are fundamental to the company's prospects

PART C: TEMPORAL COMPLETENESS
Identify any metrics or data points in the current content using data older than the most recent fiscal period available in source documents. Flag for replacement with latest data.

CRITICAL CONSTRAINT:
ONLY suggest additions where the source documents contain relevant data to support the addition. Do NOT suggest perspective gaps if the source documents lack the necessary data.

VALIDATION CHECKLIST - Flag these errors if found in current content:
• [WRONG NUMBER]: Draft number doesn't match source (within 5%)
• [OUTDATED]: Draft uses old data when more recent data exists
• [CATEGORY ERROR]: Draft confuses similar metrics 
• [WRONG MATERIALITY]: Draft emphasizes immaterial figure over material one
• [FABRICATED]: Draft includes specific metrics not in source

{BOUNDARY_ENFORCEMENT_INTRO.format(section_title=section['title'], boundaries=get_section_boundaries(section['number']))}

MATERIALITY FILTER:
Each addition must pass TWO tests:
1. "Is this data/perspective CRITICAL to evaluating this company as an investment or acquisition target?"
2. "Do the source documents contain sufficient data to support this addition?"

ADD LIST FORMAT:
- [CRITICAL] Missing item that would materially affect investment decision
- [IMPORTANT] Relevant item that would improve completeness
- [USEFUL] Supporting item that adds investor value
- [OUTDATED] Replace [old metric/year] with [newer metric/year] from source

STRICT RULES:
1. Be EXTREMELY specific - include exact data points (numbers, percentages, names)
2. Only suggest items where source documents contain the data - do NOT suggest items if data is missing
3. Focus on quantified, factual data where available
4. Maximum 5 suggestions - focus on the most critical gaps only
5. If the section is complete, state "No critical gaps identified"
6. Do NOT include page numbers, footnotes, or source citations in your suggestions

Output ONLY the ADD list for this section. No preamble or explanation."""


def get_section_enhancement_prompt(section: dict, section_content: str, add_list: str) -> str:
    """Generate enhancement prompt for a section

    Args:
        section: Section dictionary from opp_sections.py
        section_content: Current section content
        add_list: ADD list from completeness check

    Returns:
        Prompt string for enhancement
    """
    return f"""You are a precise editor. Add missing content to create a more complete section for M&A evaluation.

SECTION: {section['title']}

CURRENT SECTION CONTENT:
---
{section_content}
---

ADD THESE ITEMS:
---
{add_list}
---

SOURCE DOCUMENTS (for looking up ADD items):
{{source_documents}}

{DATA_SOURCE_HIERARCHY}

{OPP_CRITICAL_RULES}

INSTRUCTIONS:
0. {BOUNDARY_ENFORCEMENT_INTRO.format(section_title=section['title'], boundaries=get_section_boundaries(section['number']))}
1. Add ALL items from the ADD list ONLY if the source documents contain supporting data
2. PRIORITY: Lead with most material numbers (RM 15B receivables > RM 94M equipment)
3. RECENCY: Replace outdated data with most recent period from source
4. CORRECTIONS: Fix category errors flagged in ADD list with correct specific figures
5. If an ADD item cannot be supported by source document data, skip it silently - do NOT add placeholder text like "metrics not disclosed" or "data unavailable"
3. Use exact data from source documents with numbers and specifics
4. Maintain narrative flow - integrate additions smoothly
5. Preserve all existing content - do not remove anything UNLESS the ADD list provides more recent or more relevant data that should replace outdated information
6. Keep the same format: bullets with bold keywords using **word** syntax
7. If an ADD item duplicates existing content, enhance rather than duplicate

INTEGRATION STRATEGY FOR DENSITY:
• When adding new facts, fuse them with existing content where possible
• Create multi-fact sentences rather than adding new bullets
• Example: Instead of adding separate bullet "Revenue grew 12%" to existing "Malaysia 35%, Indonesia 28%",
  combine: "Malaysia generates 35% of revenue and Indonesia 28%, with combined growth of 12% YoY"

{SECTION_FORMATTING_RULES}

CRITICAL:
- Do NOT add unhelpful statements about missing data
- Do NOT include page numbers, footnotes, or source citations
- Only add content that provides real investor value with specific facts/numbers
- Output ONLY the enhanced section content (## Section Name + bullets). No preamble, no postamble."""


def get_section_density_enhancement_prompt(section: dict, section_content: str) -> str:
    """Generate density-focused enhancement prompt when no gaps are found

    Used in iterations 2+ when completeness check finds no missing content,
    but we still want to increase information density through compression and fusion.

    Args:
        section: Section dictionary from opp_sections.py
        section_content: Current section content to enhance for density

    Returns:
        Prompt string for density enhancement
    """
    return f"""You are a precision editor increasing information density for M&A evaluation.

SECTION: {section['title']}

CURRENT SECTION CONTENT:
---
{section_content}
---

SOURCE DOCUMENTS (for additional quantified details):
{{source_documents}}

{OPP_CRITICAL_RULES}

TASK:
The content is complete but can be denser. Increase information density without adding new topics by:

1. **FUSION**: Combine related facts from multiple bullets into single multi-fact sentences
   - Example: Two bullets "Malaysia 35% revenue" + "Indonesia 28%" → "Malaysia generates 35% of revenue and Indonesia 28%"

2. **QUANTIFICATION**: Replace qualitative statements with quantified facts from source documents
   - Example: "Strong growth" → "Revenue grew 12% YoY to USD81m"

3. **COMPRESSION**: Remove verbose phrasing while preserving all facts
   - Example: "The company has a presence in" → "Operates in"

4. **ENRICHMENT**: Add specific numbers, percentages, or dates where currently missing
   - Only if data exists in source documents
   - Example: "Several countries" → "9 countries across Asia"

CONSTRAINTS:
• Maintain all existing facts - do not drop information
• Keep section boundaries - stay focused on "{section['title']}"
• Skip enrichment if source documents lack supporting data

{SECTION_FORMATTING_RULES}

OUTPUT FORMAT:
## {section['title']}
[Enhanced bullets with higher information density]

Generate the density-enhanced version now."""


def get_section_deduplication_prompt(section: dict, section_content: str, previous_sections: list) -> str:
    """Generate deduplication prompt to remove content overlapping with previous sections

    Args:
        section: Section dictionary from opp_sections.py
        section_content: Current section content to deduplicate
        previous_sections: List of dicts with 'title' and 'content' from already-processed sections

    Returns:
        Prompt string for deduplication
    """
    if not previous_sections:
        # No previous sections to check against
        return section_content

    previous_content = "\n\n".join([
        f"## {s['title']}\n{s['content']}"
        for s in previous_sections
    ])

    return f"""Remove redundant content from this section that overlaps with already-finalized sections.

CURRENT SECTION TO DEDUPLICATE:
{section_content}

ALREADY-FINALIZED SECTIONS (these take precedence):
{previous_content}

{OPP_CRITICAL_RULES}

TASK:
Remove any bullet points from the current section that cover topics already addressed in the finalized sections above.

KEEP bullet points that:
- Cover unique information not mentioned in finalized sections
- Provide a different angle or focus appropriate to "{section['title']}"
- Add material new insights even if touching on similar topics

REMOVE bullet points that:
- Duplicate facts or insights already stated in finalized sections
- Overlap significantly with finalized section content
- Repeat information without adding material new value

{SECTION_FORMATTING_RULES}

CRITICAL RULES:
- Maintain the section header: ## {section['title']}
- If a bullet is only partially redundant, keep the unique parts and remove the redundant parts
- If ALL bullets are redundant, return just the section header with a note: "Content integrated into other sections"
- No preamble, no explanation, no commentary

Generate the deduplicated version now."""


def get_section_cleanup_prompt(target_section: dict, all_sections: list) -> str:
    """Generate cleanup prompt to sort content based on relevance to section specs

    Args:
        target_section: Dict with 'number', 'title', 'specs', 'content' for the target section
        all_sections: List of dicts with 'number', 'title', 'specs', 'content' for ALL 4 sections

    Returns:
        Prompt string for cleanup
    """
    # Build the current content from all sections
    all_content = "\n\n".join([
        f"## Section {s['number']}: {s['title']}\n{s['content']}"
        for s in all_sections
    ])

    # Build the section specs reference
    all_specs = "\n\n".join([
        f"SECTION {s['number']} ({s['title']}) SPECS:\n{s['specs']}"
        for s in all_sections
    ])

    return f"""Sort content based on which section it belongs to MOST.

TARGET SECTION: Section {target_section['number']} - {target_section['title']}

CURRENT CONTENT ACROSS ALL SECTIONS:
{all_content}

SECTION SPECIFICATIONS:
{all_specs}

{OPP_CRITICAL_RULES}

TASK:
Review ALL bullets across ALL sections and identify which ones belong MOST to Section {target_section['number']} ({target_section['title']}).

For each bullet across all sections, ask:
- Is this MOST relevant to Section {target_section['number']} specs?
- Or is it MORE relevant to another section's specs?

INCLUDE bullets that:
- Are MOST relevant to Section {target_section['number']} specs, regardless of where they currently are
- Best fit the requirements and focus of this section
- Align with this section's purpose and scope

EXCLUDE bullets that:
- Are MORE relevant to another section's specs
- Better fit another section's requirements
- Are not relevant to any section (rare but possible)

{SECTION_FORMATTING_RULES}

CRITICAL RULES:
- Include the section header: ## {target_section['title']}
- Return ONLY bullets that belong MOST to this section
- Duplicates are OK - the polish step will handle them
- If no bullets belong to this section, return just the header
- No preamble, no explanation, no commentary

Generate the cleaned section now."""


def get_section_polish_prompt(section: dict, section_content: str, word_limit: int) -> str:
    """Generate the polish prompt for a specific section

    Args:
        section: Section dictionary from opp_sections.py
        section_content: Current section content
        word_limit: Maximum words for polished section

    Returns:
        Prompt string for polishing
    """
    word_count = len(section_content.split())

    return f"""You are an M&A Managing Director condensing content to its most essential elements for M&A evaluation by the chairman of the potential buyer.

Polish this text accordingly. The polished text must be:
- **Concise**: Aim for {word_limit} words.
- **Coherent**: Always full sentences, no cryptic half-sentences, consider the order of the sentences and the flow of the text.
- **Highly relevant**: Each statement must matter to the investment decision.
- **Fact-based**: Each statement underpinned with numbers or, absent that, hard facts.

SECTION: {section['title']}

ORIGINAL REQUIREMENTS FOR THIS SECTION:
{section['specs']}

{OPP_CRITICAL_RULES}

CURRENT CONTENT:
---
{section_content}
---

CURRENT WORD COUNT: {word_count} words
FINAL TARGET: **Aim for {word_limit} words** (exceeding by up to 20 words is acceptable if needed for deeply insightful coverage).

CONDENSING INSTRUCTIONS:

1. **PRESERVE STRUCTURE** - The original requirements above define what this section should contain. Preserve coverage of those elements while condensing. Re-ordering to create a coherent flow is allowed.

2. **RELEVANCE FILTER** - Keep only content that meets BOTH criteria:
   - Addresses the original requirements for this section
   - Critical to investment/acquisition decision-making (targeted to the chairman of the potential buyer)

3. **DATA DENSITY**:
   - Preserve specific numbers, percentages, and trends
   - Keep year-over-year comparisons and growth rates and margins and percent contributions
   - Maintain factual density while cutting descriptive text
   - When multiple metrics exist for same item, ALWAYS prefer the most recent fiscal period

4. **SMART CONDENSING**:
   - Remove generic statements and obvious observations
   - Cut repetitive points - state each insight once
   - Eliminate elaborate explanations - let the data speak and order it so it tells a story 
   - Focus on what's surprising, notable, or value-affecting
   - Deliver deep insights and consider the flow of the text to get there, but do not add language for the sake of it
   - Combine related facts in one multi-fact sentence to the extent possible and sensible. Example: "Malaysia generates 35% of revenue and Indonesia 28%, with combined growth of 12% YoY" (3 facts, 1 sentence)
   - Be brutal yet constructive when condensing

{CONDENSING_PRIORITY_RULES}

{SECTION_FORMATTING_RULES}

CONSTRAINTS:
- Maximum {word_limit} words total (count everything)
- Maintain bullet format with **bold** syntax
- Preserve coverage of original requirements
- Every sentence must add investment decision value

CRITICAL OUTPUT RULES:
- Do NOT include any preamble, introduction, or explanatory text like "Here is the condensed..." or "This section..."
- RETAIN MATERIALITY: Don't delete the most important facts (largest numbers, recent data, differentiators) to hit word count
- PRESERVE REQUIRED ELEMENTS: Check original requirements above - ensure all required elements remain covered after condensing
- Do NOT repeat the section name as a header, bold text, or label (the section name is already in the document header)
- Output ONLY the bullet points for this section
- Start directly with the first bullet point (starting with *)
- No commentary about what you are doing - just output the bullets
- No corporate poetry, no consulting buzz words, but chairman-friendly prose
- No statements about missing data or need to analyse further, but factual observations

Generate the condensed version of this section only."""


def get_opp_ground_truth_prompt(section: dict, company_name: str) -> str:
    """Generate prompt for Step 6 - Ground Truth Discovery

    This step identifies verifiable facts in source documents that could
    reveal insights beyond the company's narrative. Uses the section's
    ground_truth_pointer to guide discovery.

    Args:
        section: Section dictionary from opp_sections.py (includes ground_truth_pointer)
        company_name: Company name for context

    Returns:
        Prompt string for ground truth discovery
    """
    return f"""You are an investigative analyst examining source documents for {company_name}.

SECTION CONTEXT: {section['title']}

GROUND TRUTH QUESTION:
{section.get('ground_truth_pointer', 'What verifiable facts reveal insights beyond the stated narrative?')}

YOUR TASK:
Search the source documents for 2-3 VERIFIABLE observations that address the ground truth question above.

WHAT MAKES A GOOD OBSERVATION:
1. **Verifiable**: Based on specific numbers, dates, or documented facts in the source
2. **Non-obvious**: Goes beyond what management explicitly states
3. **Measurable**: Can be tested or validated against other data
4. **Investment-relevant**: Would matter to a potential buyer or investor

EXAMPLES OF GOOD OBSERVATIONS:
- "Revenue grew 15% but headcount grew 35%, suggesting margin pressure ahead"
- "Three board members resigned within 6 months of the acquisition"
- "Largest customer represents 40% of revenue despite claims of diversification"
- "Capex has declined 60% over 3 years while maintenance costs rose 25%"

EXAMPLES OF BAD OBSERVATIONS:
- "The company has strong market position" (unverifiable claim)
- "Management seems committed to growth" (opinion, not fact)
- "The industry is competitive" (generic, obvious)

SEARCH STRATEGY:
- Look for disconnects between stated strategy and actual resource allocation
- Compare narrative claims against actual numbers
- Identify patterns in timing, concentration, or trends
- Note what is NOT mentioned that should be

OUTPUT FORMAT:
List exactly 2-3 observations, each as:
[OBSERVATION N]: One sentence stating the verifiable fact
[SOURCE DATA]: The specific numbers/dates from documents supporting this
[SIGNIFICANCE]: One sentence on why this matters for investment evaluation

Be precise and factual. Do not speculate beyond what documents show."""


def get_opp_hypothesis_prompt(step6_output: str, section: dict, company_name: str) -> str:
    """Generate prompt for Step 7 - Hypothesis Generation

    CRITICAL: This step deliberately has NO document attachment.
    The LLM must reason independently from the Step 6 observations
    to generate hypotheses without anchoring to source framing.

    Args:
        step6_output: Output from Step 6 (ground truth observations)
        section: Section dictionary from opp_sections.py
        company_name: Company name for context

    Returns:
        Prompt string for hypothesis generation
    """
    return f"""You are a strategic analyst generating hypotheses for {company_name}.

SECTION CONTEXT: {section['title']}

OBSERVATIONS FROM DOCUMENT ANALYSIS:
{step6_output}

YOUR TASK:
Based ONLY on the observations above (you have no access to source documents),
generate 2-3 testable hypotheses about what these observations might mean.

WHAT MAKES A GOOD HYPOTHESIS:
1. **Testable**: Can be confirmed or refuted with additional data
2. **Specific**: Makes a clear prediction, not a vague possibility
3. **Investment-relevant**: If true, would affect valuation or deal thesis
4. **Non-obvious**: Goes beyond restating the observation

HYPOTHESIS STRUCTURE:
For each observation, ask:
- "If this pattern continues, what happens next?"
- "What underlying cause could explain this?"
- "What would a skeptical buyer want to verify?"

EXAMPLES OF GOOD HYPOTHESES:
- "The revenue/headcount disconnect suggests the company is building capacity for a product launch within 12 months"
- "Board departures after acquisition indicate integration failure or strategic disagreement"
- "Declining capex with rising maintenance suggests deferred investment that will require catch-up spend"

EXAMPLES OF BAD HYPOTHESES:
- "Things could get better or worse" (not specific)
- "Management might be concerned" (not testable)
- "The company faces challenges" (too vague)

OUTPUT FORMAT:
For each hypothesis:
[HYPOTHESIS N]: One clear, testable statement
[IMPLICATION]: What this means for investment decision if true
[TEST]: What evidence would confirm or refute this

Generate 2-3 hypotheses. Be specific and actionable."""


def get_opp_test_prompt(step7_output: str, section: dict, company_name: str) -> str:
    """Generate prompt for Step 8 - Hypothesis Testing

    This step has document access restored to test hypotheses from Step 7.
    Uses low temperature for precision in evidence evaluation.

    Args:
        step7_output: Output from Step 7 (hypotheses)
        section: Section dictionary from opp_sections.py
        company_name: Company name for context

    Returns:
        Prompt string for hypothesis testing
    """
    return f"""You are an evidence evaluator testing hypotheses for {company_name}.

SECTION CONTEXT: {section['title']}

HYPOTHESES TO TEST:
{step7_output}

SOURCE DOCUMENTS:
{{source_documents}}

YOUR TASK:
For each hypothesis above, search the source documents for evidence that
either CONFIRMS or REFUTES the hypothesis. Be rigorous and objective.

EVALUATION STANDARDS:
1. **CONFIRMED**: Multiple data points support the hypothesis
2. **PARTIALLY CONFIRMED**: Some evidence supports, but gaps remain
3. **INCONCLUSIVE**: Insufficient evidence either way
4. **REFUTED**: Evidence contradicts the hypothesis

EVIDENCE RULES:
- Cite specific numbers, dates, and facts from documents
- Distinguish between strong evidence (quantified, explicit) and weak evidence (implied, qualitative)
- Note when evidence is absent (document silent on topic)
- Do not infer beyond what documents explicitly state

OUTPUT FORMAT:
For each hypothesis:
[HYPOTHESIS N]: Restate the hypothesis briefly
[EVIDENCE FOR]: Specific supporting data from documents (with numbers)
[EVIDENCE AGAINST]: Specific contradicting data from documents (with numbers)
[VERDICT]: CONFIRMED / PARTIALLY CONFIRMED / INCONCLUSIVE / REFUTED
[CONFIDENCE]: HIGH / MEDIUM / LOW (based on evidence quality)

Be objective. If evidence is weak or absent, say so. Do not force conclusions."""


def get_opp_synthesis_prompt(step8_output: str, section: dict, company_name: str) -> str:
    """Generate prompt for Step 9 - Insight Synthesis

    This step synthesizes tested hypotheses into actionable insights.
    Produces the "Insights-only" content for the second PPTX variant.

    Args:
        step8_output: Output from Step 8 (test results with verdicts)
        section: Section dictionary from opp_sections.py
        company_name: Company name for context

    Returns:
        Prompt string for insight synthesis
    """
    return f"""You are a senior M&A advisor synthesizing insights for {company_name}.

SECTION CONTEXT: {section['title']}

HYPOTHESIS TEST RESULTS:
{step8_output}

SOURCE DOCUMENTS:
{{source_documents}}

YOUR TASK:
Synthesize the confirmed and partially confirmed hypotheses into 2-4 actionable
insights for investment evaluation. Focus on what the evidence REVEALS, not just
what it DESCRIBES.

SYNTHESIS PRINCIPLES:
1. **Only confirmed insights**: Use only CONFIRMED or PARTIALLY CONFIRMED hypotheses
2. **Investment impact**: Each insight must affect valuation, risk, or deal thesis
3. **Evidence-backed**: Ground each insight in specific data from the tests
4. **Forward-looking**: What does this mean for future performance or integration?

INSIGHT QUALITY STANDARDS:
- Each insight must be something a buyer would want to know BEFORE making an offer
- Avoid restating obvious facts - synthesize to reveal implications
- Connect dots between multiple observations where relevant
- Quantify impact where possible ("This represents ~15% margin risk")

IF NO HYPOTHESES WERE CONFIRMED:
State: "No hypotheses were confirmed with sufficient evidence."
Then provide 1-2 observations about what the evidence gaps themselves reveal.

OUTPUT FORMAT:
## {section['title']} - Insights

* **[Insight keyword]**: [One sentence insight with supporting data]
* **[Insight keyword]**: [One sentence insight with supporting data]
[Maximum 4 bullets]

CRITICAL RULES:
- Each bullet is one complete sentence
- Bold the first 1-2 words as the insight keyword
- Include at least one number per bullet where possible
- No generic statements - every insight must be specific to this company
- Keep total output under 100 words"""


def get_opp_integration_prompt(step5_output: str, step9_output: str, section: dict, company_name: str) -> str:
    """Generate prompt for Step 10 - Integration

    This step weaves insights into the vanilla description, producing
    content where insight enriches description rather than being bolted on.

    Args:
        step5_output: Output from Step 5 (vanilla polished content)
        step9_output: Output from Step 9 (synthesized insights)
        section: Section dictionary from opp_sections.py
        company_name: Company name for context

    Returns:
        Prompt string for integration
    """
    return f"""You are integrating insights into a company profile for {company_name}.

SECTION: {section['title']}

VANILLA CONTENT (Step 5 - descriptive, factual):
{step5_output}

INSIGHTS (Step 9 - analytical, forward-looking):
{step9_output}

SOURCE DOCUMENTS:
{{source_documents}}

YOUR TASK:
Integrate insights into the vanilla content by EXTENDING existing bullets or adding new insight bullets. The vanilla content is the foundation - preserve it and enrich it.

SACRED RULES (non-negotiable):
1. **Order is sacred**: Existing bullets keep their exact sequence from Step 5
2. **Headers are sacred**: Each bullet's original **Keyword** stays unchanged
3. **Facts are sacred**: All numbers, stats, and quantified data from Step 5 must remain
4. **Extend, don't rewrite**: Append insight to end of a bullet, don't restructure the bullet

INTEGRATION APPROACH:

OPTION A - Extend an existing bullet (preferred):
Append insight implication to the end of the bullet, keeping all original content intact.

  Step 5:  * **Revenue**: USD 81m, up 15% YoY
  Step 10: * **Revenue**: USD 81m, up 15% YoY, though 35% headcount growth suggests margin pressure ahead

  Step 5:  * **Customers**: Top 3 represent 65% of revenue
  Step 10: * **Customers**: Top 3 represent 65% of revenue, up from 45% three years ago, creating concentration risk

OPTION B - Add new insight bullet (when insight doesn't fit existing bullets):
Insert a new bullet immediately after the topically related bullet.

  Step 5:  * **Customers**: Top 3 represent 65% of revenue
  Step 10: * **Customers**: Top 3 represent 65% of revenue
           * **Concentration risk**: Customer share doubled in 3 years, limiting acquirer negotiating power

WHAT NOT TO DO:
- Don't reorder bullets - sequence from Step 5 is fixed
- Don't change **Keyword** headers - they must match Step 5 exactly
- Don't remove numbers or stats to make room for insight text
- Don't combine two Step 5 bullets into one
- Don't add a separate "Key Insights" or "Strategic Observations" section
- Don't use filler phrases like "Notably," "Importantly," or "It's worth noting"

OUTPUT FORMAT:
## {section['title']}

* **[Original Keyword from Step 5]**: [Original content, optionally extended with insight]
* **[Original Keyword from Step 5]**: [Original content, optionally extended with insight]
[Continue preserving all Step 5 bullets in order, with optional new insight bullets inserted after related topics]

TARGET: ~150 words (extend bullets rather than cutting facts to fit)

{OPP_CRITICAL_RULES}

Generate the integrated section now."""


def get_opp_cleanup2_prompt(target_section: dict, all_sections: list) -> str:
    """Generate cleanup prompt for Step 11 - Cleanup 2

    Similar to Step 4 but for integrated content. Redistributes content
    based on section relevance after insights have been woven in.

    Args:
        target_section: Dict with 'number', 'title', 'specs', 'content' for target section
        all_sections: List of dicts with 'number', 'title', 'specs', 'content' for ALL 4 sections

    Returns:
        Prompt string for cleanup
    """
    # Get section boundaries for target section
    section_boundaries = get_section_boundaries(target_section['number'])

    # Build context showing other sections' content
    other_sections_content = "\n\n".join([
        f"## Section {s['number']}: {s['title']}\n{s['content']}"
        for s in all_sections if s['number'] != target_section['number']
    ])

    return f"""Review this integrated section for {target_section['title']}.

CURRENT CONTENT:
{target_section['content']}

OTHER SECTIONS (for context - do not duplicate their content):
{other_sections_content}

SECTION BOUNDARIES - content that belongs in OTHER sections:
{section_boundaries}

{OPP_CRITICAL_RULES}

TASK:
Review and clean up this section:
1. Remove any content that clearly belongs in another section (per boundaries above)
2. Keep content that is most relevant to THIS section's specs
3. Preserve all integrated insights and data points
4. Do not add new content - only redistribute or remove

INTEGRATION-AWARE RULES:
- Insight-enriched bullets should stay with their descriptive base topic
- If a bullet combines facts from different section domains, assign to the PRIMARY topic
- Preserve the insight integration - do not split woven content

{SECTION_FORMATTING_RULES}

CRITICAL RULES:
- Include the section header: ## {target_section['title']}
- Return ONLY bullets that belong to this section
- If no bullets belong to this section, return just the header
- No preamble, no explanation, no commentary

Output the cleaned section content."""


def get_opp_polish2_prompt(section: dict, section_content: str, step9_insights: str, word_limit: int) -> str:
    """Generate the polish prompt for Step 12 - Polish 2

    Compresses integrated content through semantic fusion, ensuring insight
    MEANINGS survive even as word count is reduced.

    Args:
        section: Section dictionary from opp_sections.py
        section_content: Current integrated section content from Step 11
        step9_insights: Output from Step 9 (insight meanings that must survive)
        word_limit: Maximum words for polished section (typically 100-110)

    Returns:
        Prompt string for final polish
    """
    word_count = len(section_content.split())

    return f"""You are compressing integrated content through semantic fusion, not cutting.

SECTION: {section['title']}

INTEGRATED CONTENT (from Steps 10-11):
---
{section_content}
---

INSIGHT MEANINGS THAT MUST SURVIVE (from Step 9):
---
{step9_insights}
---

CURRENT WORD COUNT: {word_count} words
TARGET: ~{word_limit} words

YOUR TASK:
Compress this content so the MEANINGS from the Step 9 insights survive in the final output.
The exact words don't matter. The implications do.

SEMANTIC FUSION (what we want):
Two inputs:
- Fact: "The firm targets RM 100 million for its AI-powered mandate"
- Insight: "This is a marketing pilot rather than a proprietary moat"

Good fusion (18 words, preserves both meanings):
"The RM 100 million AI mandate (0.31% of AUM) is a marketing pilot, not a proprietary moat"

Bad cutting (15 words, loses the insight):
"The firm targets RM 100 million for AI-powered mandates using third-party technology"

The second version keeps the fact but genericizes the insight into nothing.

COMPRESSION PRINCIPLES:
1. **Insight implications are the payload** - facts are just context for insights
2. **Fuse, don't cut** - combine two sentences into one that captures both meanings
3. **Specificity over generality** - "marketing pilot not a moat" beats "uses third-party technology"
4. **Cut descriptive padding first** - "the firm operates as" can become just the subject
5. **Preserve causal language** - keep "because", "suggesting", "indicating", "despite"

WHAT TO PRESERVE (in priority order):
1. Insight implications from Step 9 (highest priority)
2. Specific numbers that support those insights
3. Causal connections and "so what" language
4. Comparative context (vs competitors, vs prior periods)

WHAT TO CUT FIRST:
1. Definitional phrases ("—the total market value of investments—")
2. Redundant context already implied by the insight
3. Generic descriptive padding without analytical value
4. Duplicate facts stated in different bullets

VALIDATION BEFORE OUTPUT:
Check each insight meaning from Step 9. Can you find its essence in your output?
If an insight meaning is missing, you're cutting the wrong things. Fuse it back in.

{OPP_CRITICAL_RULES}

{SECTION_FORMATTING_RULES}

CONSTRAINTS:
- Target ~{word_limit} words (up to 15 words over is acceptable for insight-rich content)
- Maintain bullet format with **bold** syntax
- Every bullet should carry both fact AND implication where possible

CRITICAL OUTPUT RULES:
- Do NOT include any preamble or explanatory text
- Start directly with the first bullet point (*)
- No commentary - just output the semantically compressed bullets

Generate the semantically fused section now."""

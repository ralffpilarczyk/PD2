"""
Prompt templates for OnePageProfile (OPP.py)
"""

# Dynamic import: try custom sections first, fall back to default
try:
    from src.opp_sections_custom import get_section_boundaries
except ImportError:
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
• ALWAYS use the most recent data available in the source documents - prioritize latest fiscal periods over historical data
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
- Subtitle should be a key message about the company for potential investors or buyers, in up to 8 words as a prose statement (not a cryptic assembly of keywords), no period at the end
- The subtitle must be an INVESTMENT THESIS, not a corporate tagline, no buzz words, no superlatives without proof
- Professional M&A banker tone - no marketing hype, no superlatives without proof
- Plain US SEC compliant style English

TEST: Would this subtitle help a banker decide if deal merits further review?

OUTPUT FORMAT:
# {company_name}
Your investment-focused subtitle (up to 8 words) here

Now generate the title and subtitle based on the documents provided."""


def get_section_generation_prompt(section: dict) -> str:
    """Generate the initial section generation prompt

    Args:
        section: Section dictionary from opp_sections.py

    Returns:
        Prompt string for generating this section
    """
    return f"""You are creating a company profile section for M&A bankers. Analyze the provided documents and generate content for the "{section['title']}" section.

SECTION REQUIREMENTS:
{section['specs']}

{OPP_CRITICAL_RULES}

DENSITY PRIORITY:
• Pack multiple related facts into single sentences where sensible
• Every word must earn its place - no filler, no obvious observations
• Prioritize sentences with 2+ quantified facts over single-fact sentences

SECTION BOUNDARIES - STAY FOCUSED:
This is the "{section['title']}" section. DO NOT include content that belongs in other sections:
{get_section_boundaries(section['number'])}

OUTPUT FORMAT:
## {section['title']}
[Your bullet points here, following the requirements above]

CRITICAL: Do NOT include page numbers, footnotes, or source citations in your output.

Now generate this section based on the documents provided."""


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

SECTION BOUNDARIES - STAY IN SCOPE:
This completeness check is for the "{section['title']}" section ONLY.
Do NOT suggest additions that belong in other sections:
{get_section_boundaries(section['number'])}

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

{OPP_CRITICAL_RULES}

INSTRUCTIONS:
0. RESPECT SECTION BOUNDARIES: Only add content relevant to "{section['title']}".
   Do NOT add content belonging in other sections:
   {get_section_boundaries(section['number'])}
1. Add ALL items from the ADD list ONLY if the source documents contain supporting data
2. If an ADD item cannot be supported by source document data, skip it silently - do NOT add placeholder text like "metrics not disclosed" or "data unavailable"
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

FORMATTING RULES:
- Keep bullet format with **bold** keywords (1-2 most important words per bullet)
- Each bullet is one sentence maximum
- ALWAYS put a blank line before starting any list
- Use consistent markdown formatting

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
• Preserve bullet format with **bold** keywords
• Each bullet must remain a complete grammatical sentence
• Skip enrichment if source documents lack supporting data

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

CRITICAL RULES:
- Maintain the section header: ## {section['title']}
- Keep the bullet format with **bold** keywords
- If a bullet is only partially redundant, keep the unique parts and remove the redundant parts
- If ALL bullets are redundant, return just the section header with a note: "Content integrated into other sections"
- Output ONLY the deduplicated section content (## Section Name + remaining bullets)
- No preamble, no explanation, no commentary

Generate the deduplicated version now."""


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
   - Eliminate elaborate explanations - let the data speak
   - Focus on what's surprising, notable, or value-affecting
   - Deliver deep insights
   - Combine related facts in one multi-fact sentence to the extent possible and sensible. Example: "Malaysia generates 35% of revenue and Indonesia 28%, with combined growth of 12% YoY" (3 facts, 1 sentence)
   - Be brutal yet constructive when condensing
   
5. **FORMAT PRESERVATION**:
   - Keep bullet format with **bold** keywords (1-2 words per bullet)
   - Each bullet is one sentence
   - Prioritize bullets with numbers and specific facts

WHAT TO PRESERVE:
- Content that addresses the original requirements
- Specific metrics and quantified trends
- Key comparisons and benchmarks
- Unusual patterns or red flags
- Strategic disconnects or opportunities

WHAT TO REMOVE:
- Generic framework language unless specifically relevant
- Repetitive points
- Repetition of facts already stated in other bullets (even if worded differently)
- Obvious statements without data
- Elaborate explanations of simple facts
- Content that doesn't address the original section requirements
- Page numbers, footnotes, or source citations

CONSTRAINTS:
- Maximum {word_limit} words total (count everything)
- Maintain bullet format with **bold** syntax
- Preserve coverage of original requirements
- Every sentence must add investment decision value

CRITICAL OUTPUT RULES:
- Do NOT include any preamble, introduction, or explanatory text like "Here is the condensed..." or "This section..."
- Do NOT repeat the section name as a header, bold text, or label (the section name is already in the document header)
- Output ONLY the bullet points for this section
- Start directly with the first bullet point (starting with *)
- No commentary about what you are doing - just output the bullets
- No corporate poetry, no consulting buzz words, but chairman-friendly language
- No statements about missing data or need to analyse further, but factual observations

Generate the condensed version of this section only."""

"""
Prompt templates for OnePageProfile (OPP.py)
"""

# Section requirements - single source of truth for what each section should contain
SECTION_REQUIREMENTS = {
    "Company Overview": [
        "one sentence where the company is based (city and country) and what its primary business is (key subsector, not high level like 'technology company')",
        "one sentence on the company's primary operating footprint, i.e. where its people are based, with numbers",
        "one sentence on the company's asset base, i.e. where its key assets are based, and if they are owned or leased",
        "one sentence on the mix of products and services and their value proposition, indicating what's most important",
        "one sentence on the mix of geography, indicating what's most important",
        "one sentence on the mix of customers, indicating what's most important, and highlight key customers, if any, and customer relationships, e.g. long term relationships, exclusivity, etc.",
        "one sentence on the company's key suppliers and concentration risk, but only if there is a critical exposure"
    ],
    "Competitive Positioning": [
        "one sentence on the company's position in the value chain, i.e. what is the company's core competency and how does it compare to competitors",
        "one sentence on the company's 3 most important strategic objectives",
        "one sentence on market structure and the company's competitive positioning",
        "one sentence on market position and market share by products and services",
        "one sentence on market position and market share by geography",
        "one sentence on market position and market share by customer segment",
        "in case the data is missing, then use the company's narrative"
    ],
    "Financial KPIs": [
        "one sentence on most recent revenue and recent growth",
        "one sentence on EBITDA or, absent that, operating profit or, absent that, profit in absolute terms, as well as recent growth or margins, whatever is more important for the reader",
        "for each segment, one sentence on the segment financials, with numbers, and the percentage contribution to revenues and profits, if possible",
        "one sentence on balance sheet highlights, i.e. what's the single most important thing about the company's balance sheet, sometimes that's about net debt, sometimes about impairments, you need to judge",
        "one sentence on the single most important MD&A highlight, including numbers"
    ],
    "Strategic Considerations": [
        "one sentence on material recent strategic initiatives or transactions, including dates and numbers",
        "one sentence on the company's history of partnerships, including dates and numbers",
        "one sentence on the key decision makers at the management level and their strategic agenda",
        "one sentence on the key decision makers at the shareholder level and their strategic agenda towards the company, including if they have indicated willingness to sell or if they have any relevant track record in the sector",
        "one sentence on a strategic observation that would be highly interesting to a potential buyer, including strategic disconnects that warrant further analysis before proceeding with an acquisition",
        "one sentence on what is the key attraction of the company to a potential buyer, but this needs to sound deeply sophisticated and be backed up by numbers",
        "again, if you don't have the data then stick with the company's narrative"
    ]
}


COMPANY_NAME_EXTRACTION_PROMPT = """Extract the primary company name from these documents.

Look for the company name in:
- Document titles and headers
- Letterheads and footers
- About sections
- First few pages

Return ONLY the company name, nothing else. Keep it short (2-50 characters).
If unclear, return "Company Profile"."""


def get_profile_generation_prompt(company_name: str) -> str:
    """Generate the main profile generation prompt with company name"""

    # Format section requirements for the prompt
    sections_formatted = ""
    for section_name, requirements in SECTION_REQUIREMENTS.items():
        sections_formatted += f"\n## {section_name}\n"
        for req in requirements:
            sections_formatted += f"[{req}]\n"

    return f"""You are creating a company profile page for M&A bankers. Analyze the provided documents and generate a structured profile following these EXACT instructions.

CRITICAL RULES:
- Keep each sentence as short as possible, no longer than 20 words
- Ideally, each sentence should have at least one number
- If there is no meaningful data to write a sentence as instructed, skip it silently
- Do not make up information
- A precise sentence which may not perfectly suit the purpose is better than a beautifully tailored sentence which is not grounded
- Bold the 1-2 most important words in each bullet point using **word** syntax

OUTPUT FORMAT:

# {company_name}
[Subtitle: key message about the company for the potential investor or potential buyer, in 4-8 words]
{sections_formatted}
Now generate the profile based on the documents provided."""


def get_completeness_check_prompt(initial_profile: str) -> str:
    """Generate completeness check prompt with section requirements"""

    # Format section requirements for the prompt
    requirements_formatted = ""
    for section_name, requirements in SECTION_REQUIREMENTS.items():
        requirements_formatted += f"\n{section_name}:\n"
        for req in requirements:
            requirements_formatted += f"  - {req}\n"

    return f"""You are a meticulous completeness auditor evaluating a company profile for M&A purposes. Your job is to identify critical gaps in both DATA and INVESTOR PERSPECTIVE.

ORIGINAL REQUIREMENTS FOR THE PROFILE:
{requirements_formatted}

INITIAL PROFILE:
---
{initial_profile}
---

SOURCE DOCUMENTS:
{{source_documents}}

YOUR DUAL ASSESSMENT:

PART A: DATA COMPLETENESS
Check if the profile is missing specific data points from source documents that are required by the ORIGINAL REQUIREMENTS above.

PART B: INVESTOR PERSPECTIVE COMPLETENESS
For each section, consider adding statements (but only if they are not already present) underpinned by numbers and supported by source documents if they meet one of the following criteria:
- They fulfill an item in the ORIGINAL REQUIREMENTS that was skipped
- They are fundamental to a reader's understanding of the company as an investment or acquisition target
- They are fundamental to the company's competitive positioning
- They are fundamental to the company's prospects

CRITICAL CONSTRAINT:
ONLY suggest additions where the source documents contain relevant data to support the addition. Do NOT suggest perspective gaps if the source documents lack the necessary data.

MATERIALITY FILTER:
Each addition must pass TWO tests:
1. "Is this data/perspective CRITICAL to evaluating this company as an investment or acquisition target?"
2. "Do the source documents contain sufficient data to support this addition?"

ADD LIST FORMAT:
- [CRITICAL] Missing item that would materially affect investment decision (include exact source location with page/section reference)
- [IMPORTANT] Relevant item that would improve completeness (include exact source location with page/section reference)
- [USEFUL] Supporting item that adds investor value (include exact source location with page/section reference)

STRICT RULES:
1. Be EXTREMELY specific - include exact data points and source locations (page numbers, section names)
2. Only suggest items where source documents contain the data - do NOT suggest items if data is missing
3. Focus on quantified, factual data where available
4. Maximum 5 suggestions per section - focus on the most critical gaps only
5. If the profile is complete, state "No critical gaps identified"

Output ONLY the ADD list. No preamble or explanation."""


def get_enhancement_prompt(initial_profile: str, add_list: str) -> str:
    """Generate the enhancement prompt"""
    return f"""You are a precise editor. Add missing content to create a more complete profile for M&A evaluation.

CURRENT PROFILE:
---
{initial_profile}
---

ADD THESE ITEMS:
---
{add_list}
---

SOURCE DOCUMENTS (for looking up ADD items):
{{source_documents}}

INSTRUCTIONS:
1. Add ALL items from the ADD list ONLY if the source documents contain supporting data
2. If an ADD item cannot be supported by source document data, skip it silently - do NOT add placeholder text like "metrics not disclosed" or "data unavailable"
3. Use exact data from source documents with numbers and specifics
4. Maintain narrative flow - integrate additions smoothly into appropriate sections
5. Preserve all existing content - do not remove anything
6. Keep the same format: bullets with bold keywords using **word** syntax
7. If an ADD item duplicates existing content, enhance rather than duplicate
8. Maintain short sentences (max 20 words) with numbers where possible

FORMATTING RULES:
- Keep bullet format with **bold** keywords (1-2 most important words per bullet)
- Each bullet is one sentence maximum
- Maintain section structure: Company Overview, Competitive Positioning, Financial KPIs, Strategic Considerations
- ALWAYS put a blank line before starting any list
- Use consistent markdown formatting

CRITICAL:
- Do NOT add unhelpful statements about missing data
- Only add content that provides real investor value with specific facts/numbers
- Output ONLY the enhanced profile markdown content. No preamble, no postamble."""


def get_polish_prompt(section_name: str, section_content: str, word_limit: int) -> str:
    """Generate the polish prompt for a specific section"""
    word_count = len(section_content.split())

    if word_limit == 0:  # Title and subtitle - no word limit
        return f"""You are an expert editor polishing a company profile title and subtitle for M&A presentation.

CURRENT CONTENT:
---
{section_content}
---

CURRENT WORD COUNT: {word_count} words

POLISHING INSTRUCTIONS:
1. Ensure the company name is clear and accurate
2. Refine the subtitle to be compelling and investor-focused (4-8 words)
3. The subtitle should capture the key investment thesis or strategic positioning
4. Keep it concise and impactful

Output ONLY the polished title and subtitle. No explanations."""

    else:  # Content sections with word limits
        # Format section requirements from SECTION_REQUIREMENTS dictionary
        section_reqs = ""
        if section_name in SECTION_REQUIREMENTS:
            for req in SECTION_REQUIREMENTS[section_name]:
                section_reqs += f"- {req}\n"

        return f"""You are an expert analyst condensing content to its most essential elements for M&A evaluation.

SECTION: {section_name}

ORIGINAL REQUIREMENTS FOR THIS SECTION:
{section_reqs}

CURRENT CONTENT:
---
{section_content}
---

CURRENT WORD COUNT: {word_count} words
FINAL TARGET: **Maximum {word_limit} words - absolutely no exceptions.**

CONDENSING INSTRUCTIONS:

1. **PRESERVE STRUCTURE** - The original requirements (title and sentences)above define what this section should contain. Preserve coverage of those elements while condensing.

2. **RELEVANCE FILTER** - Keep only content that meets BOTH criteria:
   - Addresses the original requirements for this section (section title or sentences)
   - Critical to investment/acquisition decision-making

3. **DATA DENSITY**:
   - Preserve specific numbers, percentages, and trends
   - Keep year-over-year comparisons and growth rates and margins and percent contributions
   - Maintain factual density while cutting descriptive text

4. **SMART CONDENSING**:
   - Remove generic statements and obvious observations
   - Cut repetitive points - state each insight once
   - Eliminate elaborate explanations - let the data speak
   - Focus on what's surprising, notable, or value-affecting

5. **FORMAT PRESERVATION**:
   - Keep bullet format with **bold** keywords (1-2 words per bullet)
   - Each bullet is one sentence (max 20 words)
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
- Obvious statements without data
- Elaborate explanations of simple facts
- Content that doesn't address the original section requirements

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

Generate the condensed version of this section only."""

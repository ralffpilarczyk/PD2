"""
TEMPLATE for Custom OPP Section Definitions

==================================================================================
INSTRUCTIONS FOR CREATING YOUR CUSTOM PROFILE
==================================================================================

1. COPY this entire file to: src/opp_sections_custom.py
   Command: cp src/opp_sections_template.py src/opp_sections_custom.py

2. MODIFY the 4 sections below to match your specific requirements
   - Change section titles to your desired names
   - Update specs with your custom bullet point requirements
   - Keep the structure intact (see CRITICAL RULES below)

3. UPDATE get_section_boundaries() function to match your sections
   - Define what content belongs in each section
   - Prevents content bleeding between sections

4. SAVE and run OPP, selecting "2 - Custom Profile"

==================================================================================
CRITICAL RULES - DO NOT VIOLATE THESE
==================================================================================

✓ MUST have exactly 4 sections (for 2×2 PowerPoint layout)
✓ MUST number sections 1, 2, 3, 4 in order
✓ Each section MUST have these keys: "number", "title", "specs"
✓ Keep specs as multi-line strings with \\n line breaks
✓ Do NOT remove the get_section_boundaries() function
✗ Do NOT add or remove sections (must be exactly 4)
✗ Do NOT change section numbers to anything other than 1-4

==================================================================================
WRITING EFFECTIVE SECTION REQUIREMENTS
==================================================================================

GOOD PATTERNS:
• "One sentence on [topic] including [specific metrics]"
  → Enforces conciseness and quantification

• "One sentence on [topic] with time period context (e.g., 2024, Q1 2025)"
  → Ensures temporal clarity

• "One sentence on [topic], if available in source documents"
  → Makes optional requirements clear

• "One sentence on [topic] with numbers and percentages"
  → Forces data-driven content

BAD PATTERNS:
✗ "Describe the company" - too vague
✗ "Provide analysis" - no specific requirements
✗ "Include relevant information" - unclear what's relevant

TIPS:
- Be specific about what data you want (numbers, percentages, dates)
- Specify format preferences (absolute values vs ratios, currency units)
- Use "One sentence on..." to enforce brevity
- Include examples of what you're looking for
- Distinguish mandatory vs optional elements

==================================================================================
"""

# ==============================================================================
# SECTION DEFINITIONS
# ==============================================================================

sections = [
    {
        "number": 1,
        "title": "Company Overview",  # ← CHANGE THIS to your section 1 name

        # ↓ CHANGE EVERYTHING BELOW to your section 1 requirements
        "specs": """Provide a comprehensive overview of the company's business.

Include the following elements:
• One sentence where the company is based (city and country) and what its primary business is - specify the EXACT subsector with precision (e.g., 'pharmaceutical distribution', not 'healthcare'; 'SaaS CRM software', not 'technology')
• One sentence on the company's primary operating footprint, i.e. where its people are based, with numbers
• One sentence on the company's asset base, i.e. where its key assets are based, and if they are owned or leased
• One sentence on the key products and services and their value proposition
• One sentence on the key geographies where the company is active
• One sentence on the key customers, including spread or concentration risk
• One sentence on how the company sells (e.g. direct or indirect) and how it contracts with customers (e.g. long term contracts, purchase orders, etc.) and how it maintains relationships with customers (e.g. loyalty programs, etc.)
• One sentence on the company's key suppliers, but only if there is significant concentration risk

EXAMPLES OF CUSTOMIZATION:
- Add industry-specific requirements (e.g., "licensing status" for pharma)
- Focus on asset-light vs asset-heavy characteristics
- Emphasize regulatory context if relevant to your sector
- Specify customer segments you care about (B2B vs B2C, enterprise vs SMB)
"""
    },

    {
        "number": 2,
        "title": "Competitive Positioning",  # ← CHANGE THIS to your section 2 name

        # ↓ CHANGE EVERYTHING BELOW to your section 2 requirements
        "specs": """Analyze the company's competitive position and strategic focus.

Include the following elements:
• One sentence on the company's position in the value chain, i.e. what is the company's core capability and how does it compare to competitors
• One sentence on the company's 2 most important strategic objectives
• One sentence on market structure and the company's competitive positioning
• One sentence on market position (e.g. #1 in market) and market share by products and services
• One sentence on market position (e.g. #3 in geography) and market share by geography
• One sentence on market position (e.g. #2 in customer segment) and market share by customer segment
• One sentence on on the 2 most important accolades and awards the company has received
• In case the data is missing, then use the company's narrative

EXAMPLES OF CUSTOMIZATION:
- Add "barriers to entry" or "switching costs" analysis
- Request "technology moat" assessment for tech companies
- Ask for "brand strength indicators" in consumer businesses
- Require "regulatory advantages" in regulated industries
- Focus on "network effects" for platform businesses
"""
    },

    {
        "number": 3,
        "title": "Financial KPIs",  # ← CHANGE THIS to your section 3 name

        # ↓ CHANGE EVERYTHING BELOW to your section 3 requirements
        "specs": """Summarize the company's key financial metrics and trends.

Include the following elements:
• One sentence on the company's most important revenue drivers, e.g. prices or volumes or ARPU, depending on sector, and any important trends
• One sentence on most recent revenue and recent growth
• One sentence on the company's most important cost drivers, e.g. cost of goods sold or operating expenses or other costs, and any important trends
• One sentence on EBITDA or, absent that, operating profit or, absent that, profit - include absolute values (with currency units), margins (as %), and growth rates (as %) with proper labels
• For each segment, one sentence on the segment financials, with numbers, and the percentage contribution to revenues and profits, if possible
• One sentence on the company's capital expenditure and any important trends
• One sentence on balance sheet highlights, i.e. what's the single most important thing about the company's balance sheet, sometimes that's about net debt, sometimes about impairments, you need to judge
• One sentence on unit economics and the 2 most important financial performance drivers, including numbers

EXAMPLES OF CUSTOMIZATION:
- Request specific metrics for your industry (e.g., "Same Store Sales Growth" for retail)
- Add "Rule of 40" for SaaS (growth rate + profit margin)
- Request "Inventory Turns" for manufacturing
- Ask for "Customer Acquisition Cost" and "Lifetime Value" for subscription businesses
- Require "Net Interest Margin" for financial services
- Focus on "Free Cash Flow" metrics if that's your priority
"""
    },

    {
        "number": 4,
        "title": "Strategic Considerations",  # ← CHANGE THIS to your section 4 name

        # ↓ CHANGE EVERYTHING BELOW to your section 4 requirements
        "specs": """Summarise key strategic considerations for the company as an investment or acquisition target.

Include the following elements:
• One sentence on the company's 2 most important strategic initiatives or transactions, including dates and numbers
• One sentence on the company's history of partnerships, including dates and numbers
• One sentence on the key decision makers at the management level and their strategic agenda
• One sentence on the key shareholders and their ownership in the company (in %), and their key decision makers at the shareholder level and their strategic agenda towards the company, any indications of their willingness to sell, or any relevant track record
• One sentence on 2 strategic observations connecting to deal implications - explain WHY each matters for valuation, synergies, or integration risk, not just WHAT you observe
• One sentence on the key acquisition rationale quantifying the value creation opportunity - specify who would buy this (strategic vs financial) and why with numbers
• If you don't have the data then stick with the company's narrative

EXAMPLES OF CUSTOMIZATION:
- Add "ESG risks or opportunities" if relevant to your investment thesis
- Request "talent retention considerations" for people-intensive businesses
- Ask for "IP portfolio strength" in innovation-driven sectors
- Require "regulatory approval pathway" for M&A in regulated industries
- Focus on "cultural fit indicators" if you prioritize integration ease
- Add "exit comps" - what similar companies sold for recently
"""
    }
]


# ==============================================================================
# SECTION BOUNDARIES FUNCTION
# ==============================================================================

def get_section_boundaries(section_num: int) -> str:
    """Define what content belongs in OTHER sections (to prevent bleeding)

    This function tells the LLM what content DOES NOT belong in the current section.
    Update this to match your custom section titles and topics.

    Args:
        section_num: Section number (1-4)

    Returns:
        String describing what belongs in other sections

    EXAMPLE:
    If Section 1 is "Company Overview" and you're defining boundaries for it,
    list what belongs in Sections 2, 3, 4 instead.
    """

    # ↓ CHANGE THESE to match your custom section topics
    boundaries = {
        1: """- Section 2: value chain position, strategic objectives, market rankings/share, competitive positioning, accolades/awards
- Section 3: revenue/cost drivers and trends, revenue/EBITDA numbers/growth, margins, segment financials, capex, balance sheet, MD&A highlights
- Section 4: strategic initiatives/transactions, partnerships, management/shareholder agendas, strategic observations, investment thesis""",

        2: """- Section 1: company location, operating/asset footprint, key products and services, product/service value propositions, customer/supplier relationships
- Section 3: revenue/cost drivers and trends, revenue/EBITDA numbers/growth, margins, segment financials, capex, balance sheet, MD&A highlights
- Section 4: strategic initiatives/transactions, partnerships, management/shareholder agendas, strategic observations, investment thesis""",

        3: """- Section 1: company location, operating/asset footprint, key products and services, product/service value propositions, customer/supplier relationships
- Section 2: value chain position, strategic objectives, market rankings/share, competitive positioning, accolades/awards
- Section 4: strategic initiatives/transactions, partnerships, management/shareholder agendas, strategic observations, investment thesis""",

        4: """- Section 1: company location, operating/asset footprint, key products and services, product/service value propositions, customer/supplier relationships
- Section 2: value chain position, strategic objectives, market rankings/share, competitive positioning, accolades/ awards
- Section 3: revenue/cost drivers and trends, revenue/EBITDA numbers/growth, margins, segment financials, capex, balance sheet, MD&A highlights"""
    }

    return boundaries.get(section_num, "")


# ==============================================================================
# VALIDATION HELPER (Do not modify)
# ==============================================================================

def validate_custom_sections():
    """Validate that custom sections follow required structure

    This function is called automatically when you select Custom Profile mode.
    It prevents common mistakes that would cause OPP to fail.
    """
    errors = []

    # Check section count
    if len(sections) != 4:
        errors.append(f"Must have exactly 4 sections, found {len(sections)}")

    # Check each section structure
    required_keys = {"number", "title", "specs"}
    for i, section in enumerate(sections):
        # Check required keys
        missing = required_keys - set(section.keys())
        if missing:
            errors.append(f"Section {i}: missing required keys: {missing}")

        # Check section number
        if "number" in section and section["number"] != i + 1:
            errors.append(f"Section {i}: number must be {i+1}, found {section['number']}")

        # Check title is not empty
        if "title" in section and not section["title"].strip():
            errors.append(f"Section {i}: title cannot be empty")

        # Check specs is not empty
        if "specs" in section and not section["specs"].strip():
            errors.append(f"Section {i}: specs cannot be empty")

    return errors


# ==============================================================================
# TESTING YOUR CUSTOM SECTIONS
# ==============================================================================

if __name__ == "__main__":
    print("=" * 80)
    print("CUSTOM SECTION VALIDATION")
    print("=" * 80)

    errors = validate_custom_sections()

    if errors:
        print("\n❌ VALIDATION FAILED\n")
        for error in errors:
            print(f"  • {error}")
        print("\nPlease fix these errors before running OPP.")
    else:
        print("\n✓ VALIDATION PASSED\n")
        print("Your custom sections are properly structured!")
        print("\nSections defined:")
        for section in sections:
            print(f"  {section['number']}. {section['title']}")
        print("\nYou can now run OPP and select '2 - Custom Profile'")

    print("=" * 80)

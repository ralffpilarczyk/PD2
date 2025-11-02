"""
Custom OPP Section Definitions

==================================================================================
INSTRUCTIONS FOR CREATING YOUR CUSTOM PROFILE
==================================================================================

This file contains the default OnePageProfile sections as a working example.
To create your custom profile:

1. EDIT the two marked sections below:
   - EDITABLE SECTION 1: Modify the 4 section definitions
   - EDITABLE SECTION 2: Update the section boundaries to match

2. SAVE and run OPP, selecting "2 - Custom Profile"

3. To RESET to defaults: git checkout src/opp_sections_custom.py

==================================================================================
CRITICAL RULES
==================================================================================

✓ MUST have exactly 4 sections (for 2×2 PowerPoint layout)
✓ MUST number sections 1, 2, 3, 4 in order
✓ Each section MUST have: "number", "title", "specs"
✓ Keep specs as multi-line strings with \\n line breaks

✗ Do NOT add or remove sections (must be exactly 4)
✗ Do NOT change section numbers
✗ Do NOT remove the get_section_boundaries() function

==================================================================================
CUSTOMIZATION TIPS
==================================================================================

For SECTION DEFINITIONS (sections list):
- Change "title" to your desired section name
- Modify "specs" to define what content you want in each section
- Use "One sentence on..." pattern to enforce conciseness
- Include "with numbers" or "including metrics" to ensure quantification
- Add "if available" to make elements optional

For SECTION BOUNDARIES (get_section_boundaries function):
- Update the boundaries dict to describe what belongs in OTHER sections
- Keep descriptions concise (max ~20 words per section)
- This prevents content bleeding between sections

==================================================================================
WHAT TO EDIT
==================================================================================

Below you'll find TWO sections marked with:
  ⚠️  EDITABLE SECTION

These are the ONLY parts you should modify. Everything else should remain as-is.

==================================================================================
"""


# ═══════════════════════════════════════════════════════════════════════════════
# ⚠️  EDITABLE SECTION 1: SECTION DEFINITIONS (4 sections)
# ═══════════════════════════════════════════════════════════════════════════════
#
# Modify the 4 sections below to customize your profile.
# Change "title" and "specs" to match your requirements.
# Keep structure: each section must have "number", "title", "specs"
#
# ═══════════════════════════════════════════════════════════════════════════════

sections = [
    {
        "number": 1,
        "title": "Company Overview",
        "specs": "Provide a comprehensive overview of the company's business.\\n"
                "\\n"
                "Include the following elements:\\n"
                "• One sentence where the company is based (city and country) and what its primary business is - specify the EXACT subsector with precision (e.g., 'pharmaceutical distribution', not 'healthcare'; 'SaaS CRM software', not 'technology')\\n"
                "• One sentence on the company's primary operating footprint, i.e. where its people are based, with numbers\\n"
                "• One sentence on the company's asset base, i.e. where its key assets are based, and if they are owned or leased\\n"
                "• One sentence on the key products and services and their value proposition\\n"
                "• One sentence on the key geographies where the company is active\\n"
                "• One sentence on the key customers, including spread or concentration risk\\n"
                "• One sentence on how the company sells (e.g. direct or indirect) and how it contracts with customers (e.g. long term contracts, purchase orders, etc.) and how it maintains relationships with customers (e.g. loyalty programs, etc.)\\n"
                "• One sentence on the company's key suppliers, but only if there is significant concentration risk\\n"
    },
    {
        "number": 2,
        "title": "Competitive Positioning",
        "specs": "Analyze the company's competitive position and strategic focus.\\n"
                "\\n"
                "Include the following elements:\\n"
                "• One sentence on the company's position in the value chain, i.e. what is the company's core capability and how does it compare to competitors\\n"
                "• One sentence on the company's 2 most important strategic objectives\\n"
                "• One sentence on market structure and the company's competitive positioning\\n"
                "• One sentence on market position (e.g. #1 in market) and market share by products and services\\n"
                "• One sentence on market position (e.g. #3 in geography) and market share by geography\\n"
                "• One sentence on market position (e.g. #2 in customer segment) and market share by customer segment\\n"
                "• One sentence on on the 2 most important accolades and awards the company has received\\n"
                "• In case the data is missing, then use the company's narrative\\n"
    },
    {
        "number": 3,
        "title": "Financial KPIs",
        "specs": "Summarize the company's key financial metrics and trends.\\n"
                "\\n"
                "Include the following elements:\\n"
                "• One sentence on the company's most important revenue drivers, e.g. prices or volumes or ARPU, depending on sector, and any important trends\\n"
                "• One sentence on most recent revenue and recent growth\\n"
                "• One sentence on the company's most important cost drivers, e.g. cost of goods sold or operating expenses or other costs, and any important trends\\n"
                "• One sentence on EBITDA or, absent that, operating profit or, absent that, profit - include absolute values (with currency units), margins (as %), and growth rates (as %) with proper labels\\n"
                "• For each segment, one sentence on the segment financials, with numbers, and the percentage contribution to revenues and profits, if possible\\n"
                "• One sentence on the company's capital expenditure and any important trends\\n"
                "• One sentence on balance sheet highlights, i.e. what's the single most important thing about the company's balance sheet, sometimes that's about net debt, sometimes about impairments, you need to judge\\n"
                "• One sentence on unit economics and the 2 most important financial performance drivers, including numbers\\n"
    },
    {
        "number": 4,
        "title": "Strategic Considerations",
        "specs": "Summarise key strategic considerations for the company as an investment or acquisition target.\\n"
                "\\n"
                "Include the following elements:\\n"
                "• One sentence on the company's 2 most important strategic initiatives or transactions, including dates and numbers\\n"
                "• One sentence on the company's history of partnerships, including dates and numbers\\n"
                "• One sentence on the key decision makers at the management level and their strategic agenda\\n"
                "• One sentence on the key shareholders and their ownership in the company (in %), and their key decision makers at the shareholder level and their strategic agenda towards the company, any indications of their willingness to sell, or any relevant track record\\n"
                "• One sentence on 2 strategic observations connecting to deal implications - explain WHY each matters for valuation, synergies, or integration risk, not just WHAT you observe\\n"
                "• One sentence on the key acquisition rationale quantifying the value creation opportunity - specify who would buy this (strategic vs financial) and why with numbers\\n"
                "• If you don't have the data then stick with the company's narrative\\n"
    }
]


# ═══════════════════════════════════════════════════════════════════════════════
# ⚠️  EDITABLE SECTION 2: SECTION BOUNDARIES
# ═══════════════════════════════════════════════════════════════════════════════
#
# Update the boundaries dict below to match your custom section topics.
# Each entry describes what content belongs in OTHER sections (not the current one).
# This prevents content bleeding between sections.
#
# ═══════════════════════════════════════════════════════════════════════════════

def get_section_boundaries(section_num: int) -> str:
    """Generate section boundary warnings based on actual section specs

    Args:
        section_num: Section number (1-4)

    Returns:
        Formatted string listing what content belongs in OTHER sections (max 20 words per section)
    """
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

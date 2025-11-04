"""
Section definitions for OnePageProfile
Contains specifications for all 4 profile sections
"""

sections = [
    {
        "number": 1,
        "title": "Company Overview",
        "specs": "Provide a comprehensive overview of the company's business.\\n"
                "\\n"
                "Include the following elements:\\n"
                "• One sentence describing where the company is based (city and country) and what its primary business is - specify the EXACT subsector with precision (e.g., 'pharmaceutical distribution', not 'healthcare'; 'SaaS CRM software', not 'technology')\\n"
                "• One sentence describing the company's primary operating footprint, i.e. where its people are based, with numbers\\n"
                "• One sentence describing the company's asset base, i.e. where its key assets are based, and if they are owned or leased\\n"
                "• One sentence describing the key products and services and their value proposition\\n"
                "• One sentence describing the key geographies where the company is active\\n"
                "• One sentence describing the key customers, including spread or concentration risk\\n"
                "• One sentence describing how the company sells (e.g. direct or indirect) and how it contracts with customers (e.g. long term contracts, purchase orders, etc.) and how it maintains relationships with customers (e.g. loyalty programs, etc.)\\n"
                "• One sentence describing the company's key suppliers, but only if there is significant concentration risk\\n"
                "• In case the company is listed on a stock exchange, then one sentence on the market capitalization in local currency. in case the company is not listed on a stock exchange, then skip this sentence.\\n"
    },
    {
        "number": 2,
        "title": "Competitive Positioning",
        "specs": "Analyze the company's competitive position and strategic focus.\\n"
                "\\n"
                "Include the following elements:\\n"
                "• One sentence describing the company's position in the value chain, i.e. what is the company's core capability and how does it compare to competitors\\n"
                "• One sentence describing the company's 2 most important strategic objectives\\n"
                "• One sentence describing market structure and the company's competitive positioning\\n"
                "• One sentence describing market position (e.g. #1 in market) and market share by products and services\\n"
                "• One sentence describing market position (e.g. #3 in geography) and market share by geography\\n"
                "• One sentence describing market position (e.g. #2 in customer segment) and market share by customer segment\\n"
                "• One sentence describing the 2 most important accolades and awards the company has received\\n"
                "• In case the data is missing, then use the company's narrative\\n"
    },
    {
        "number": 3,
        "title": "Financial KPIs",
        "specs": "Summarize the company's key financial metrics and trends.\\n"
                "\\n"
                "Include the following elements:\\n"
                "• One sentence describing the company's most important revenue drivers, e.g. prices or volumes or ARPU, depending on sector, and any important trends\\n"
                "• One sentence describing most recent revenue and recent growth\\n"
                "• One sentence describing the company's most important cost drivers, e.g. cost of goods sold or operating expenses or other costs, and any important trends\\n"
                "• One sentence describing EBITDA or, absent that, operating profit or, absent that, profit - include absolute values (with currency units), margins (as %), and growth rates (as %) with proper labels\\n"
                "• For each segment, one sentence describing the segment financials, with numbers, and the percentage contribution to revenues and profits, if possible\\n"
                "• One sentence describing the company's capital expenditure and any important trends\\n"
                "• One sentence describing balance sheet highlights, i.e. what's the single most important thing about the company's balance sheet, sometimes that's about net debt, sometimes about impairments, you need to judge\\n"
                "• One sentence describing unit economics and the 2 most important financial performance drivers, including numbers\\n"
    },
    {
        "number": 4,
        "title": "Strategic Considerations",
        "specs": "Summarise key strategic considerations for the company as an investment or acquisition target.\\n"
                "\\n"
                "Include the following elements:\\n"
                "• One sentence describing the company's 2 most important strategic initiatives or transactions, including dates and numbers\\n"
                "• One sentence describing the company's history of partnerships, including dates and numbers\\n"
                "• One sentence describing the key decision makers at the management level and their strategic agenda\\n"
                "• One sentence describing all shareholders who own more than 20% of the company and their ownership in the company (in %), and their key decision makers at the shareholder level and their strategic agenda towards the company, any indications of their willingness to sell, or any relevant track record\\n"
                "• One sentence describing 2 strategic observations connecting to deal implications - explain WHY each matters for valuation, synergies, or integration risk, not just WHAT you observe\\n"
                "• One sentence describing the key acquisition rationale quantifying the value creation opportunity - specify who would buy this (strategic vs financial) and why with numbers\\n"
                "• If you don't have the data then stick with the company's narrative\\n"
    }
]


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

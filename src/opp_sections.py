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
                "• One sentence where the company is based (city and country) and what its primary business is (key subsector, not high level like 'technology company')\\n"
                "• One sentence on the company's primary operating footprint, i.e. where its people are based, with numbers\\n"
                "• One sentence on the company's asset base, i.e. where its key assets are based, and if they are owned or leased\\n"
                "• One sentence on the mix of products and services and their value proposition, indicating what's most important\\n"
                "• One sentence on the mix of geography, indicating what's most important\\n"
                "• One sentence on the mix of customers, indicating what's most important, and highlight key customers, if any, and customer relationships, e.g. long term relationships, exclusivity, etc.\\n"
                "• One sentence on the company's key suppliers and concentration risk, but only if there is a critical exposure\\n"
                "\\n"
                "Critical rules:\\n"
                "• Keep each sentence as short as possible, no longer than 20 words\\n"
                "• Ideally, each sentence should have at least one number\\n"
                "• If there is no meaningful data to write a sentence as instructed, skip it silently\\n"
                "• Do not make up information\\n"
                "• A precise sentence which may not perfectly suit the purpose is better than a beautifully tailored sentence which is not grounded\\n"
                "• Bold the 1-2 most important words in each bullet point using **word** syntax"
    },
    {
        "number": 2,
        "title": "Competitive Positioning",
        "specs": "Analyze the company's competitive position and strategic focus.\\n"
                "\\n"
                "Include the following elements:\\n"
                "• One sentence on the company's position in the value chain, i.e. what is the company's core competency and how does it compare to competitors\\n"
                "• One sentence on the company's 3 most important strategic objectives\\n"
                "• One sentence on market structure and the company's competitive positioning\\n"
                "• One sentence on market position and market share by products and services\\n"
                "• One sentence on market position and market share by geography\\n"
                "• One sentence on market position and market share by customer segment\\n"
                "• In case the data is missing, then use the company's narrative\\n"
                "\\n"
                "Critical rules:\\n"
                "• Keep each sentence as short as possible, no longer than 20 words\\n"
                "• Ideally, each sentence should have at least one number\\n"
                "• If there is no meaningful data to write a sentence as instructed, skip it silently\\n"
                "• Do not make up information\\n"
                "• A precise sentence which may not perfectly suit the purpose is better than a beautifully tailored sentence which is not grounded\\n"
                "• Bold the 1-2 most important words in each bullet point using **word** syntax"
    },
    {
        "number": 3,
        "title": "Financial KPIs",
        "specs": "Summarize the company's key financial metrics and trends.\\n"
                "\\n"
                "Include the following elements:\\n"
                "• One sentence on most recent revenue and recent growth\\n"
                "• One sentence on EBITDA or, absent that, operating profit or, absent that, profit in absolute terms, as well as recent growth or margins, whatever is more important for the reader\\n"
                "• For each segment, one sentence on the segment financials, with numbers, and the percentage contribution to revenues and profits, if possible\\n"
                "• One sentence on balance sheet highlights, i.e. what's the single most important thing about the company's balance sheet, sometimes that's about net debt, sometimes about impairments, you need to judge\\n"
                "• One sentence on the single most important MD&A highlight, including numbers\\n"
                "\\n"
                "Critical rules:\\n"
                "• Keep each sentence as short as possible, no longer than 20 words\\n"
                "• Ideally, each sentence should have at least one number\\n"
                "• If there is no meaningful data to write a sentence as instructed, skip it silently\\n"
                "• Do not make up information\\n"
                "• A precise sentence which may not perfectly suit the purpose is better than a beautifully tailored sentence which is not grounded\\n"
                "• Bold the 1-2 most important words in each bullet point using **word** syntax"
    },
    {
        "number": 4,
        "title": "Strategic Considerations",
        "specs": "Identify key strategic factors and considerations for potential investors or acquirers.\\n"
                "\\n"
                "Include the following elements:\\n"
                "• One sentence on material recent strategic initiatives or transactions, including dates and numbers\\n"
                "• One sentence on the company's history of partnerships, including dates and numbers\\n"
                "• One sentence on the key decision makers at the management level and their strategic agenda\\n"
                "• One sentence on the key decision makers at the shareholder level and their strategic agenda towards the company, including if they have indicated willingness to sell or if they have any relevant track record in the sector\\n"
                "• One sentence on a strategic observation that would be highly interesting to a potential buyer, including strategic disconnects that warrant further analysis before proceeding with an acquisition\\n"
                "• One sentence on what is the key attraction of the company to a potential buyer, but this needs to sound deeply sophisticated and be backed up by numbers\\n"
                "• Again, if you don't have the data then stick with the company's narrative\\n"
                "\\n"
                "Critical rules:\\n"
                "• Keep each sentence as short as possible, no longer than 20 words\\n"
                "• Ideally, each sentence should have at least one number\\n"
                "• If there is no meaningful data to write a sentence as instructed, skip it silently\\n"
                "• Do not make up information\\n"
                "• A precise sentence which may not perfectly suit the purpose is better than a beautifully tailored sentence which is not grounded\\n"
                "• Bold the 1-2 most important words in each bullet point using **word** syntax"
    }
]

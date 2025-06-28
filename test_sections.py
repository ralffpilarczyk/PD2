"""
Test Section definitions 
Contains descriptions of select test sections
"""

sections = [

    {
        "number": 1,
        "title": "Operating Footprint",
        "specs": "Extract and analyze the company's operational presence and scale. Focus on understanding how the company's physical and human resources are distributed and how they support its business model.\\n"
                "\\n"
                "Key areas to consider:\\n"
                "a. Employee distribution across locations and functions\\n"
                "b. Main operating assets, their strategic importance and their ownership (owned or leased)\\n"
                "c. Geographic footprint and alignment with business strategy\\n"
                "d. Any significant changes or trends in the operating footprint\\n"
                "e. How the operating structure supports competitive positioning\\n"
                "\\n"
                "Extract operating data points such as capacity, square footage, number of customers, etc., but NOT financial data\\n"
                "Look for insights that connect the operating footprint to business performance and strategic objectives.\\n"
                "For each data point, extract historical values for the last 3 years and the 5 most recent interim periods. "
                "Present data points in table format to the extent reasonable. If not, use bullet points.\\n"
                "For each data point, always try to identify the most recent value and its date, because that's usually the most important one.\\n"
                "For each data point, always reference the specific point in time or time period it relates to.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "For each data point, note any anomalies, any disconnects between the data point and management's commentary, and any industry benchmarks mentioned."
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point."
    },
    {
        "number": 7,
        "title": "Summary Financials (Consolidated)",
        "specs": "Extract and analyze the company's consolidated financial performance, both annually and interim periods.\\n"
                "\\n"
                "Key areas to consider:\\n"
                "a. Financial Metrics:\\n"
                "   - Revenue\\n"
                "   - EBITDA and margin\\n"
                "   - Operating Income and margin\\n"
                "   - Net Income and margin\\n"
                "   - Capex and % of revenue\\n"
                "   - Cash conversion (EBITDA / (EBITDA/Capex))\\n"
                "\\n"
                "b. Time Periods:\\n"
                "   - Last 3 financial years\\n"
                "   - 5 most recent interim periods\\n"
                "   - Any forecasts or guidance\\n"
                "\\n"
                "c. Additional Data:\\n"
                "   - GAAP and non-GAAP measures\\n"
                "   - One-time items\\n"
                "   - Industry comparisons\\n"
                "\\n"
                "Analysis Requirements:\\n"
                "a. Trend Analysis:\\n"
                "   - Revenue growth\\n"
                "   - Margin evolution\\n"
                "   - Cash flow generation\\n"
                "\\n"
                "b. Performance Assessment:\\n"
                "   - Achievement vs. guidance\\n"
                "   - Industry comparison\\n"
                "   - Key drivers of performance\\n"
                "\\n"
                "Output Format:\\n"
                "a. Financial Summary:\\n"
                "   - Key metrics in table format, if possible\\n" 
                "   - Key recent trends\\n"
                "   - Margin analysis\\n"
                "   - Cash flow assessment\\n"
                "\\n"
                "b. MDNA highlighting key recent trends:\\n"
                "   - 2 key achievements\\n"
                "   - 2 key challenges\\n"
                "   - 2 areas of disconnect between management statements and actual performance\\n"
                "\\n"
                "For each data point, extract historical values for the last 3 years and the 5 most recent interim periods. "
                "Present data points in table format to the extent reasonable. If not, use bullet points.\\n"
                "For each data point, always try to identify the most recent value and its date, because that's usually the most important one.\\n"
                "For each data point, always reference the specific point in time or time period it relates to.\\n"
                "For each data point, note any anomalies, any disconnects between the data point and management's commentary, and any industry benchmarks mentioned."
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point."
    },

    {
        "number": 13,
        "title": "Strategic Objectives and Corporate Strategies",
        "specs": "Extract and analyze the company's strategic goals and corporate strategy.\\n"
                "\\n"
                "Key areas to consider:\\n"
                "a. Strategic objectives:\\n"
                "   - List the top 3 strategic objectives.\\n"
                "   - Strategic objectives are defined as goals going forward.\\n"
                "   - Note the timeframes and horizons.\\n"
                "   - Note the success metrics and KPIs.\\n"
                "   - Note the resource allocation.\\n"
                "\\n"
                "b. Corporate strategies:\\n"
                "   - List the top 3 corporate strategies.\\n"
                "   - Corporate strategies are defined as using existing assets or existing capabilities to compete now.\\n"
                "   - Note there is a clear distinction between strategic objectives (goals going forward) and corporate strategies (using existing assets/capabilities to compete now).\\n"
                "   - Strategy is not about plans or investments or aspirations. A strategy has to be feasible, by definition. This is critically important.\\n"
                "   - List the required capabilities for each strategy.\\n"
                "   - List the competitive and historical context for each strategy.\\n"
                "   - List any disconnects between the company's strategic objectives (its goals) and its corporate strategies (how it is competing now).\\n"
                "\\n"
                "Presented in structured format with clear separation between objectives and strategies.\\n"
                "All data points must reference the specific point in time or time period they relate to.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point."
    },
    {
        "number": 21,
        "title": "Sellside Positioning - Competitive Positioning",
        "specs": "Describe the 3 most important competitive advantages that materially impact the Company's economic performance over the next 12 months.\\n"
                "Focus on specific, measurable advantages relative to named competitors in key markets and segments.\\n"
                "Include both quantitative measures (market share, growth rates, margins, pricing power) and qualitative advantages (brand strength, customer relationships) supported by hard data.\n"
                "Demonstrate how each advantage translates to superior financial prospects relative to industry averages and specific competitors.\\n"
                "Competitive positioning and competitive advantage are about the company's relative strengths, not market trends or future plans.\\n"
                "Support all claims with specific numeric data including comparative metrics, retention rates, pricing premiums, and relative growth rates.\\n"
                "All data points must reference the specific point in time or time period they relate to.\\n"   
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.", 
    },
    {
        "number": 29,
        "title": "Buyside Due Diligence - Operating Performance",
        "specs": "Describe the 3 most important operating metrics that materially impact the Company's economic performance over the last 24 months. \\n"
            "Prioritize metrics related to market share, volumes, unit pricing, revenue per user, unit margins, \\n"
            "customer acquisition costs, customer churn/retention, asset utilization, and unit economics. Not financial metrics. \\n"
            "For each metric:\n"
            "  - Provide a clear definition of the metric and its relevance.\\n"
            "  - Present historical data for the last 24 months, showing trends (preferably quarterly data if available).\\n"
            "  - Benchmark against key competitors and/or industry averages, where data is available. Quantify any outperformance or underperformance. \\n"
            "  - Analyze the drivers of the metric's performance.  Explain why the metric has changed (e.g., due to market conditions, competitive actions, internal initiatives).\\n"
            "  - Assess the sustainability of the current performance trend.  Are there factors that could cause the metric to improve or deteriorate? \\n"
            "  - Quantify the financial impact of the metric's performance (e.g., contribution to revenue growth, margin expansion/compression). \\n"
            "  - Formulate 1-2 specific, data-driven due diligence questions that a potential buyer should ask. \\n"
            "Provide an introductory paragraph summarizing the overall operating performance based on the chosen metrics. \\n"
            "All data points must reference the specific point in time or time period they relate to. \\n"
            "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report). \\n"
            "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.",
    },
    {
        "number": 32,
        "title": "Appendix",
        "specs": """
You will be provided with a large block of text containing all tables extracted from multiple source documents, formatted in Markdown.
Your task is to act as an expert data organizer and format this information into a single, clean, comprehensive, and logically structured HTML appendix.

**CRITICAL INSTRUCTIONS:**

1.  **DO NOT OMIT ANY DATA.** Your primary goal is to present **ALL** the numerical data provided in the input markdown. Do not summarize, aggregate, or drop any figures. If you see a number, it must appear in your output.

2.  **Organize Logically:** Structure the appendix in a sequence similar to what would be found in a professional financial prospectus. The ideal order is:
    a. **Operational KPIs:** Key Performance Indicators, non-financial metrics (e.g., subscribers, units, market share).
    b. **Consolidated Financials:** The main financial statements (Income Statement, Balance Sheet, Cash Flow).
    c. **Supporting Financials:** Detailed breakdowns that support the main statements (e.g., debt schedules, revenue recognition details).
    d. **Segment Reporting:** Financials broken down by business segment or geography.
    e. **Shareholder & Governance Data:** Top shareholders, board information, etc.
    f. **All other miscellaneous data tables.**

3.  **Combine Time Periods Intelligently:** Where possible, present annual and quarterly data for the same metric in a single table to show progression and allow for easy comparison. For example, a row for "Revenue" might have columns for FY2022, FY2023, Q1 2023, and Q1 2024.

4.  **Prioritize Clarity over Combination:** If combining time periods creates a confusing or inaccurate table (e.g., the metrics are not truly like-for-like, or the headers become too complex), it is **better to create separate, clear tables**. Never drop data for the sake of combining. When in doubt, show tables separately.

5.  **Use Standard HTML:** Adhere strictly to the provided HTML formatting rules (e.g., `<table class="data-table">`, `<thead>`, `<tbody>`, etc.). Use `<h3>` and `<h4>` to create a clear hierarchy based on the organizational structure above.

6.  **Use Source Information:** The input markdown will contain page numbers and table titles from the original documents. Use these to inform your headings and to help you group related tables.

Your output must be a single, well-formed block of HTML representing the entire appendix content, starting with `<div class="section" id="section-32">` and ending with `</div>`.
        """
    }
]
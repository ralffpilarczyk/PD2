"""
Section definitions for ProfileDash
Contains descriptions of all profile sections
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
        "number": 2,
        "title": "Products and Services",
        "specs": "Extract and analyze the company's product and service offerings. Focus on understanding the value proposition of each product or service from the customer's perspective.\\n"
                "\\n"
                "Key areas to consider:\\n"
                "a. List of main product/service categories\\n"
                "b. Individual products within each category\\n"
                "c. Value proposition for each product\\n"
                "d. Market positioning and competitive differentiation\\n"
                "e. Product lifecycle stage\\n"
                "f. Any data points such as market share, growth rates, etc., but NOT financial data\\n"
                "\\n"
                "For each data point, extract historical values for the last 3 years and the 5 most recent interim periods. "
                "Present data points in table format to the extent reasonable. If not, use bullet points.\\n"
                "For each data point, always try to identify the most recent value and its date, because that's usually the most important one.\\n"
                "For each data point, always reference the specific point in time or time period it relates to.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "For each data point, note any anomalies, any disconnects between the data point and management's commentary, and any industry benchmarks mentioned."
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point."
    },
    {
        "number": 3,
        "title": "Key Customers",
        "specs": "Extract and analyze the company's customer relationships. Focus on understanding the key customers and their contribution to the company's revenue.\\n"
                "\\n"
                "Key areas to consider:\\n"
                "a. List of largest customers\\n"
                "b. Position in the value chain: whether customer is an OEM, Tier 1, or Tier 2 supplier\\n"
                "c. Relationship strength and duration\\n"
                "d. Products purchased by each key customer and associated metrics\\n"
                "e. Customer segmentation by industry and geography\\n"
                "f. Customer positioning (premium, mid-market, value segments)\\n"
                "g. Revenue contribution and customer concentration\\n"
                "h. Purchase patterns and contract terms\\n"
                "\\n"
                "For each data point, extract historical values for the last 3 years and the 5 most recent interim periods. "
                "Present data points in table format to the extent reasonable. If not, use bullet points.\\n"
                "For each data point, always try to identify the most recent value and its date, because that's usually the most important one.\\n"
                "For each data point, always reference the specific point in time or time period it relates to.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "For each data point, note any anomalies, any disconnects between the data point and management's commentary, and any industry benchmarks mentioned."
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point."
    },
    {
        "number": 4,
        "title": "Key Suppliers",
        "specs": "Extract and analyze the company's supplier relationships.\\n"
                "\\n"
                "Key areas to consider:\\n"
                "a. List of largest suppliers and supplier concentration\\n"
                "b. Materials or components provided by each key supplier\\n"
                "c. Supplier's position in the value chain (B2B, B2C, B2B2C) and margin capture\\n"
                "d. COGS contribution\\n"
                "e. Relationship duration\\n"
                "f. Supplier segmentation by industry and geography\\n"
                "h. Pricing trends\\n"
                "k. Supplier performance metrics (quality, on-time delivery)\\n"
                "\\n"
                "For each data point, extract historical values for the last 3 years and the 5 most recent interim periods. "
                "Present data points in table format to the extent reasonable. If not, use bullet points.\\n"
                "For each data point, always try to identify the most recent value and its date, because that's usually the most important one.\\n"
                "For each data point, always reference the specific point in time or time period it relates to.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "For each data point, note any anomalies, any disconnects between the data point and management's commentary, and any industry benchmarks mentioned."
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point."
    },
    {
        "number": 5,
        "title": "Key Competitors",
        "specs": "Extract and analyze the company's competitive landscape.\\n"
                "\\n"
                "Key areas to consider:\\n"
                "a. List of key competitors and their recent strategic moves\\n"
                "b. Market structure and competitive dynamics\\n"
                "c. Product comparisons and areas of competition\\n"
                "d. Market share trends and competitive strategies\\n"
                "e. Current positioning and recent changes\\n"
                "\\n"
                "For each data point, extract historical values for the last 3 years and the 5 most recent interim periods. "
                "Present data points in table format to the extent reasonable. If not, use bullet points.\\n"
                "For each data point, always try to identify the most recent value and its date, because that's usually the most important one.\\n"
                "For each data point, always reference the specific point in time or time period it relates to.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "For each data point, note any anomalies, any disconnects between the data point and management's commentary, and any industry benchmarks mentioned."
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point."
    },
    {
        "number": 6,
        "title": "Operational KPIs",
        "specs": "Extract and analyze operational KPIs (not financial KPIs)that directly impact cash flow generation.\\n"
                "\\n"
                "Key areas to consider:\\n"
                "a. Market share metrics and trends (by product, region, customer segment)\\n"
                "b. Volume metrics and trends (units sold, customers served)\\n"
                "c. Pricing metrics and trends (average selling price, price trends)\\n"
                "d. Customer metrics and trends (retention, acquisition cost, lifetime value)\\n"
                "e. Operational efficiency metrics and trends (capacity utilization, cycle times)\\n"
                "f. Unit economics and business impact\\n"
                "\\n"
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
        "number": 8,
        "title": "Summary Financials (Segment)",
        "specs": "Extract and analyze financial performance by business segment.\\n"
                "\\n"
                "Key areas to consider:\\n"
                "a. Segment Data:\\n"
                "   - Segment names and definitions\\n"
                "   - Revenue by segment\\n"
                "   - Operating income by segment\\n"
                "   - EBITDA by segment\\n"
                "   - Assets by segment\\n"
                "\\n"
                "b. Time Periods:\\n"
                "   - Last 3 financial years\\n"
                "   - 5 most recent quarters\\n"
                "   - Any segment-specific forecasts or guidance\\n"
                "\\n"
                "c. Additional Data:\\n"
                "   - One-time items by segment\\n"
                "   - Segment-specific metrics\\n"
                "   - Inter-segment transactions\\n"
                "\\n"
                "Analysis Requirements:\\n"
                "1. Segment Analysis:\\n"
                "   - Growth trends by segment\\n"
                "   - Margin evolution by segment\\n"
                "   - Asset efficiency by segment\\n"
                "\\n"
                "b. Contribution Analysis:\\n"
                "   - Segment performance comparison\\n"
                "   - Segment contribution to overall results\\n"
                "   - Strategic importance by segment\\n"
                "\\n"
                "Output Format:\\n"
                 "a. Financial Summary by segment:\\n"
                "   - Key metrics by segment in table format, if possible\\n" 
                "   - Key recent trends by segment\\n"
                "   - Margin analysis by segment\\n"
                "   - Cash flow assessment by segment\\n"
                "\\n"
                "b. MDNA highlighting key recent trends by segment:\\n"
                "   - 2 key achievements by segment\\n"
                "   - 2 key challenges by segment\\n"
                "   - 2 areas of disconnect between management statements and actual performance by segment\\n"
                "\\n"
                "For each data point, extract historical values for the last 3 years and the 5 most recent interim periods. "
                "Present data points in table format to the extent reasonable. If not, use bullet points.\\n"
                "For each data point, always try to identify the most recent value and its date, because that's usually the most important one.\\n"
                "For each data point, always reference the specific point in time or time period it relates to.\\n"
                "For each data point, note any anomalies, any disconnects between the data point and management's commentary, and any industry benchmarks mentioned."
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point."
    },
    {
        "number": 9,
        "title": "Balance Sheet (Most Recent)",
        "specs": "Extract and analyze the company's most recent balance sheet.\\n"
                "\\n"
                "Key areas to consider:\\n"
                "a. Asset Data:\\n"
                "   - Cash & Equivalents\\n"
                "   - Current Assets\\n"
                "   - PP&E\\n"
                "   - Intangibles\\n"
                "   - Investments\\n"
                "   - Other assets\\n"
                "   - Total Assets\\n"
                "\\n"
                "b. Liability Data:\\n"
                "   - Current Liabilities\\n"
                "   - Non-Current Liabilities\\n"
                "   - Total Liabilities\\n"
                "   - Equity\\n"
                "   - Minority Interest\\n"
                "   - Total Liabilities and Equity\\n"
                "\\n"
                "c. Additional Data:\\n"
                "   - Gross Debt\\n"
                "   - Net Debt\\n"
                "   - Off-balance sheet items\\n"
                "   - Working capital and working capitalmetrics\\n"
                "\\n"
                "Output Format:\\n"
                "a. Balance Sheet metrics in table format, if possible:\\n"
                "   - Key asset categories\\n"
                "   - Key liability categories\\n"
                "   - Key equity categories\\n"
                "   - Net debt build up\\n"
                "   - Working capital build up\\n"
                "b. MDNA highlighting key recent trends:\\n"
                "   - 2 key achievements\\n"
                "   - 2 key challenges\\n"
                "   - 2 observations on leverage and covenants\\n"
                "   - 2 observations on working capital\\n"
                "   - 2 areas of disconnect between management statements and actual performance\\n"
                "\\n"
                "For each data point, extract historical values for the last 3 years and the 5 most recent interim periods. "
                "Present data points in table format to the extent reasonable. If not, use bullet points.\\n"
                "For each data point, always try to identify the most recent value and its date, because that's usually the most important one.\\n"
                "For each data point, always reference the specific point in time or time period it relates to.\\n"
                "For each data point, note any anomalies, any disconnects between the data point and management's commentary, and any industry benchmarks mentioned."
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point"
    },
    {
        "number": 10,
        "title": "Top 10 Shareholders",
        "specs": "Extract and analyze the company's ownership structure.\\n"
                "\\n"
                "Key areas to consider:\\n"
                "a. List of top 10 shareholders and each of their percentage ownership\\n"
                "b. Recent transactions and changes in ownership positions\\n"
                "c. Share class details and voting rights\\n"
                "d. Beneficial ownership information and related shareholder groups (e.g. founders, management team, family, etc.) and their percentage ownership\\n"
                "e. Insider holdings and recent changes in insider ownership\\n"
                "f. Shareholder activism history\\n"
                "g. Voting power distribution and control implications\\n"
                "h. Shareholder agreements and key terms\\n"
                "\\n"
                "For each data point, extract historical values for the last 3 years and the 5 most recent interim periods. "
                "Present data points in table format to the extent reasonable. If not, use bullet points.\\n"
                "For each data point, always try to identify the most recent value and its date, because that's usually the most important one.\\n"
                "For each data point, always reference the specific point in time or time period it relates to.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "For each data point, note any anomalies, any disconnects between the data point and management's commentary, and any industry benchmarks mentioned."
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point"
    },
    {
        "number": 11,
        "title": "M&A Agenda and Material Corporate Activity",
        "specs": "Summarise the company's M&A agenda as follows:\\n"
                "1. M&A objectives and strategy - including strategic rationale, target criteria (size, geography, capabilities), and how M&A fits into overall growth plans\\n"
                "2. Statements towards acquisitions, divestitures, monetisations or restructurings - including management commentary on appetite and priorities\\n"
                "3. Specific deals currently underway and transaction status - active discussions, LOIs, due diligence, regulatory approvals, or recently completed/failed deals\\n"
                "4. M&A capacity and constraints - financial capacity, leverage limits, regulatory considerations\\n"
                "5. Track record - historical M&A performance and integration outcomes where disclosed\\n"
                "\\n"
                "In addition, extract and analyze significant corporate events and transactions over the last 2 years.\\n"
                "Present information in a concise narrative format with specific quotes and data points where available.\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point."
                "\\n"
                "Key areas to consider:\\n"
                "a. Strategic reviews\\n"
                "b. Material financings and capital raises\\n"
                "c. Material investments and divestitures\\n"
                "d. Material pending transactions\\n"
                "e. Material failed transactions\\n"
                "\\n"
                "For each material corporate activity, include the strategic rationale, commercial terms, and deal structure.\\n"
                "Present all material corporate activities in chronological order.\\n"
                "All events must reference the specific point in time or time period they relate to.\\n"
                "For each material corporate activity, note any anomalies, any disconnects between the data point and management's commentary, and any industry benchmarks mentioned.\\n"
                "\\n"
    },
    {
        "number": 12,
        "title": "Key Decision Makers",
        "specs": "Start with a list of the key senior executives and the a list of the Board of Directors.\\n"
                "Include titles, roles, backgrounds, and relevant experience at other companies.\\n"
                "Detail compensation structures including fixed, variable, shares, options, and incentive arrangements.\\n"
                "Track recent leadership changes and their impact.\\n"
                "Include committee memberships and roles for board members.\\n"
                "Highlight board independence status.\\n"
                "Note relationships between board members and executives.\\n"
                "Outline decision-making authority and reporting relationships where available.\\n"
                "All data points must reference the specific point in time or time period they relate to.\\n"
                "Presented in table or bullet point format.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point."
    },
    {
        "number": 13,
        "title": "Deep Dive Discoveries",
        "specs": "Extract risks or contradictions not immediately apparent. Focus on identifying information that could materially impact the company's prospects.\\n"
                "\\n"
                "Key areas to consider:\\n"
                "a. Heroic Assumptions: Identify up to 3 material assumptions made by management that require achievements well outside historical norms, where material means impacting the prospects of the company\\n"
                "b. Disconnects: Identify up to 3 material disconnects between what management is saying and what the company is actually doing, where material means impacting the prospects of the company\\n"
                "c. Buried Disclosures: Identify up to 3 material observations that are hidden away in a single place, maybe just a footnote, but which could materially impact the prospects of the company\\n"
                "\\n"
                "For each observation:\\n"
                "- Present as a single clear sentence explaining the issue and why it is material to the company's prospects\\n"
                "\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each observation."
    },
    {
        "number": 14,
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
        "number": 15,
        "title": "Strategic Constraints",
        "specs": "List the 3 most important constraints for the company to achieve its strategic objectives.\\n"
                "Focus on constraints that make it very difficult to achieve the previously defined strategic objectives.\\n"
                "Quantify the impact of constraints on business performance where possible.\\n"
                "Include mitigation efforts or plans related to these constraints.\\n"
                "Compare how competitors deal with similar constraints.\\n"
                "Note evolution of constraints over time where material.\\n"
                "Include consideration of emerging future constraints.\\n"
                "Categorize constraints appropriately based on available data\\n"
                "All data points must reference the specific point in time or time period they relate to.\\n"
                "Presented in table or bullet point format.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point."
    },
    {
        "number": 16,
        "title": "Strengths",
        "specs": "Describe the 3 most relevant strengths that enable the company to compete.\\n"
                "Focus on existing strengths (not future plans or initiatives), and existing competitive moats.\\n"
                "Prioritize strengths by how much they enable the company to compete.\\n"
                "Cover competitive positioning, skills, assets, capabilities, expertise, speed of execution, insights, intellectual property, and licenses.\\n"
                "Focus on strengths most relevant to the company's most important business segments.\\n"
                "Strengths are about the company's current capabilities, not market trends or future plans.\\n"
                "Include how the company developed key strengths.\\n"
                "Provide competitive benchmarking where data is available.\\n"
                "Quantify strengths with metrics where possible.\\n"
                "Show how each strength directly contributes to competitive advantage.\\n"
                "Include how the company is currently leveraging these strengths.\\n"
                "Mention emerging strengths only if they might become critical in the near term.\\n"
                "This is about the company's strengths versus competitors, not market trends.\\n"
                "All data points must reference the specific point in time or time period they relate to.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always underpin observations with numbers, facts, data, or comparisons.\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.",
 
    },
    {
        "number": 17,
        "title": "Weaknesses",
        "specs": "Describe the 3 most relevant weaknesses that prevent the Company from serving its customers or competing with its main competitors\\n"
                "Focus on company-specific weaknesses versus competitors (not market threats)\\n"
                "Prioritize weaknesses that are material to the company's ability to compete today\\n"
                "Weaknesses are about the company's current capabilities, not market trends or future challenges.\\n"
                "Explain underlying causes and development of material weaknesses\\n"
                "Include competitive benchmarking where data is available\\n"
                "Quantify impact of weaknesses on business performance\\n"
                "Include meaningful mitigation efforts already underway\\n"
                "Note emerging weaknesses that may become material in the near term\\n"
                "All data points must reference the specific point in time or time period they relate to\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always underpin observations with numbers, facts, data, or comparisons\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point",
 
    },
    {
        "number": 18,
        "title": "Opportunities",
        "specs": "Describe 3 specific opportunities that are achievable within 12-24 months and could materially impact the Company's performance\\n"
                "Focus only on opportunities where the Company has existing capabilities to capture value\\n"
                "For each opportunity, quantify potential revenue, profit, and ROI where possible\\n"
                "Clearly link each opportunity to the Company's existing strengths and capabilities\\n"
                "An opportunity is not an initiative by the company, but a situation backdrop that can realistically be captured by the company to improve value creation or competitive positioning.\\n"
                "Include implementation timeframes, required investments, and expected returns\\n"
                "Prioritize opportunities based on potential financial impact and feasibility\\n"
                "Avoid speculative opportunities that require significant capability development\\n"
                "All data points must reference the specific point in time or time period they relate to\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always underpin observations with numbers, facts, data, or comparisons\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point",
 
    },
    {
        "number": 19,
        "title": "Threats",
        "specs": "Identify and analyze the 3 most significant threats to the Company's performance within the next 12-24 months.\\n"
                "Prioritize threats based on potential financial impact and likelihood of occurrence.\\n"
                "Include competitive threats, technological disruptions, regulatory changes, and other external factors.\\n"
                "A threat is not a misstep of the company, but a situation backdrop that makes it challenging for the company to compete or create value.\\n"
                "Quantify the potential negative impact of each threat on revenue, margins, and profitability where possible.\\n"
                "Assess likelihood, timeframe, and early warning indicators for each threat.\\n"
                "Describe the Company's current mitigation efforts and their effectiveness.\\n"
                "Identify key competitors' strategies that may impact the Company's market position.\\n"
                "Highlight disconnects between the Company's stated strategies and actual implementation.\\n"
                "All data points must reference the specific point in time or time period they relate to.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always underpin observations with numbers, facts, data, or comparisons.\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.",
    },
    {
        "number": 20,
        "title": "Sellside Positioning - Macro",
        "specs": "Describe 3 most important macro trends which support the Company's performance and prospects.\\n"
                "Focus on economic indicators, not industry dynamics, because that's a separate question later. Positive trends only.\\n"
                "Include relevant macro indicators such as economic growth, interest rates, labor costs, supply chain indicators, and global trade.\\n"
                "Macro trends are about the company's environment, not the company's capabilities or strategies.\\n"
                "Present data covering both recent trends (last 24 months) and future outlook (next 12 months).\\n"
                "Explain how each trend specifically benefits the Company and underpin all observations with quantitative data.\\n"
                "All data points must reference the specific point in time or time period they relate to.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always underpin observations with numbers, facts, data, or comparisons.\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.", 
    },
    {
        "number": 21,
        "title": "Sellside Positioning - Industry",
        "specs": "Describe 3 most important industry trends which support the Company's performance and prospects.\\n"
                "Focus on industry indicators, not macro indicators. Positive trends only.\\n"
                "Include demand, supply, pricing, and industry growth drivers relevant to the Company.\\n"
                "Industry trends are about the company's environment, not the company's capabilities or strategies.\\n"
                "Present data covering both recent performance (last 24 months) and future projections (next 12 months).\\n"
                "Explain how each trend specifically benefits the Company and underpin all observations with facts, data and numbers.\\n"
                "All data points must reference the specific point in time or time period they relate to.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always underpin observations with numbers, facts, data, or comparisons.\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.",
    },
    {
        "number": 22,
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
        "number": 23,
        "title": "Sellside Positioning - Operating Performance",
        "specs": "Describe the 3 most important operating performance metrics over the last 24 months that directly impact the Company's economic wellbeing.\\n"
                "Focus on measurable KPIs such as market share evolution, volumes sold, pricing trends, revenue per customer or unit economics that show the Company in the best possible light.\\n"
                "Highlight areas of particular strength and present data that demonstrates exceptional operating performance.\\n"
                "Operating performance is separate from financial performance which will be covered separately later. \\n"
                "This should be the greatest hits of the Company's operating KPIs.\\n"
                "Support all claims with specific numeric data points and trends.\\n"
                "All data points must reference the specific point in time or time period they relate to.\\n"   
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.",
    },
    {
        "number": 24,
        "title": "Sellside Positioning - Financial Performance",
        "specs": "Describe the 3 most important financial achievements of the Company over the last 24 months.\\n"
                "Focus on metrics that demonstrate exceptional financial performance, particularly those related to cash flow generation.\\n"
                "Present quarterly data where available to highlight positive trends.\\n"
                "Include industry and peer comparisons that demonstrate the Company's outperformance.\\n"
                "This should be the greatest hits of the Company's financial KPIs.\\n"
                "Support all claims with specific financial data and metrics.\\n"
                "All data points must reference the specific point in time or time period they relate to.\\n"   
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.",
    },
    {
        "number": 25,
        "title": "Sellside Positioning - Management",
        "specs": "Describe 3 facts about the Company's management team and Board that highlight their strengths and capabilities.\\n"
                "Focus on both individual executives and the management team as a whole.\\n"
                "Quantify management capabilities and how they contribute to the Company's success.\\n"
                "Include relevant background and experience that directly contributes to their ability to lead the Company.\\n"
                "Highlight successful execution of specific strategic initiatives, supported by concrete data points.\\n"
                "All observations must be underpinned by facts and numbers.\\n"
                "All data points must reference the specific point in time or time period they relate to.\\n"   
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.",
    },
    {
        "number": 26,
        "title": "Sellside Positioning - Potential Investor Concerns and Mitigants",
        "specs": "Describe the 5 most important potential investor concerns when considering investing in the Company.\\n"
                "Focus on fundamental business concerns and valuation issues that could impact investor returns.\\n"
                "For each concern, provide 2-3 bullet points explaining the issue, underpinned by specific data points.\\n"
                "Follow each concern with 1-2 compelling mitigants that are already in place or represent structural advantages (not future actions).\\n"
                "Quantify all concerns and mitigants with concrete data points and metrics.\\n"
                "Focus on company-specific issues rather than broader industry trends.\\n"
                "Present the most effective and credible arguments to deflect each concern.\\n"
                "All data points must reference the specific point in time or time period they relate to.\\n"   
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.",
    },
    {
        "number": 27,
        "title": "Buyside Due Diligence - Macro",
        "specs": "Describe the 3 most important macroeconomic trends that could materially impact the Company's economic performance over the next 12 months.\\n"
                "Focus on downside risks, not upside opportunities. Provide a detailed analysis of potential negative impacts.\\n"
                "For each trend, provide a quantitative assessment of the Company's sensitivity to these factors, \\n"
                "including impacts on margins, revenue, and cash flow. Include benchmarking against competitors' sensitivity to the \\n"
                "same macroeconomic factors where material differences in relative performance exist.\\n"
                "For each key trend, formulate 1-2 detailed, data-driven due diligence questions that a potential buyer should ask.\\n"
                "Validate all claims with specific historical data points and quantitative examples.\\n"
                "All data points must reference the specific point in time or time period they relate to.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.",
    },
    {
        "number": 28,
        "title": "Buyside Due Diligence - Industry",
        "specs": "Describe the 3 most important industry trends that could materially impact the Company's economic performance over the next 12-24 months.\\n"
                "Focus on downside risks, not upside opportunities, including technology shifts, competitive dynamics, and regulatory changes.\\n"
                "Provide a detailed analysis of potential negative impacts. \\"
                "For each trend, provide a quantitative assessment of the Company's sensitivity to these factors, \\n"
                "including impacts on margins, revenue, and cash flow. Include benchmarking against competitors' sensitivity to the \\n"
                "same industry factors where material differences in relative performance exist.\\n"
                "For each key trend, formulate 1-2 detailed, data-driven due diligence questions that a potential buyer should ask.\\n"
                "Validate all claims with specific historical data points and quantitative examples.\\n"
                "All data points must reference the specific point in time or time period they relate to.\\n"
                "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.",
    },
    {
        "number": 29,
        "title": "Buyside Due Diligence - Competitive Positioning",
        "specs": "Describe the Company's 3 most important quantifiable competitive weaknesses versus key competitors and industry benchmarks.\\n"
                 "Analyze market share trends over the past 24 months and describe whether the Company's position is deteriorating.\\n"
                 "Measure product/service differentiation using objective metrics, pricing power through realization rates and premiums, and customer loyalty through retention and satisfaction scores.\\n"
                 "Quantify cost position and operational efficiency relative to key competitors using margin analysis and productivity metrics.\\n"
                 "For each topic, formulate 1-2 specific due diligence questions to validate competitive claims with concrete data.\\n"
                 "Triangulate all competitive positioning assertions using measurable data from multiple sources including customer metrics, win/loss analysis, and third-party benchmarking.\\n"
                 "All data points must reference the specific point in time or time period they relate to.\\n"
                 "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report).\\n"
                 "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.",
    },
    {
        "number": 30,
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
        "number": 31,
        "title": "Buyside Due Diligence - Financial Performance",
        "specs": "Describe the 3 most important financial metrics and then identify and quantify material risks to these metrics and the Company's economic well-being over the next 12-24 months. \\n"
            "Analyse the underlying drivers, trends, and potential vulnerabilities. \\n"
            "For each metric:\\n"
            "  - Provide a clear definition and explain its relevance to the Company's financial health. \\n"
            "  - Present historical data for the last 24 months (preferably quarterly), showing trends and comparing to relevant benchmarks (industry averages, key competitors) where available. \\n"
            "  - Analyze the quality of the metric.  Are there any accounting treatments, one-time items, or unusual adjustments that could distort the true picture? \\n"
            "  - Assess sustainability.  Is the current performance likely to continue, improve, or deteriorate? What are the key drivers and risks? \\n"
            "  - Quantify the potential financial impact of any identified risks or vulnerabilities (e.g., impact on revenue, EBITDA, cash flow, valuation). \\n"
            "  - For each of the 3 metrics, formulate 1-2 specific, data-driven due diligence questions that a potential buyer should ask. \\n"
            "Include an introductory paragraph summarizing the overall financial risk profile based on the chosen metrics. \\n"
            "Conduct a forensic accounting review to the extent possible based on the provided documents, to search for unusual accounting policies. \\n"
            "All data points must reference the specific point in time or time period they relate to. \\n"
            "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report). \\n"
            "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.",
    },
    {
        "number": 32,
        "title": "Buyside Due Diligence - Management",
        "specs": "Describe the key members of the management team and the Board of Directors, focusing on individual track records, experience, and potential risks.\n"
            "For each key individual (primarily C-suite and key Board members):\n"
            "  - Provide a brief overview of their current role and tenure at the Company.\n"
            "  - Analyze their track record, both at the Company and in previous roles. Include:\n"
            "    - Quantifiable achievements (e.g., revenue growth, cost reductions, successful product launches) over the last 24 months.\n"
            "    - Any identified failures or setbacks.\n"
            "    - Relevant experience and expertise, particularly as it relates to the Company's strategic priorities.\n"
            "    - Identification of any potential 'red flags' (e.g., frequent job changes, involvement in controversies, lack of relevant experience).\n"
            "  - Assess potential retention risks based on information available in the provided documents (e.g., recent departures, changes in compensation structure, negative sentiment expressed in internal communications).\n"
            "  - For each decision maker formulate 1 specific, data-driven due diligence questions that a potential buyer should ask. \n"
            "All data points must reference the specific point in time or time period they relate to. \\n"
            "More recent data points (e.g. from subsequent interim financials) are more important than older data points (e.g. from the preceding annual report). \\n"
            "Always include precise footnotes with exact sources, document references, page numbers, and sections for each data point.",
    },

    {
        "number": 33,
        "title": "Appendix: Data Book",
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
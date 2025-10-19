I would like to use Gemini to create company profile pages as per the format set out below.
Step 1: User chooses Gemini 2.5 Flash or 2.5 Pro, similar to PD2
Step 2: User uploads a source document.
Step 3: OnePageProfile (OPP.py) then applies the instructions below to create a profile.

The source documents will be PDFs and there is no conversion required, unlike PD2. We will feed the PDFs into Gemini as they are. The difference to PD2 is that OPP will require only one PDF for a run. PD2 required lots of files. the UI for OPP.py should allow to upload several files, but I expect that we will usually only use one file. Same tkinter file picker as PD2. in case the use uploads several files then handle them in the same context window, similar to PD2.

Web search is not required as part of the routine.

Target users are front line M&A bankers. This tool is meant to be an automation tool to produce boilerplate profile pages with a high quality level of insights. the inputs are bespoke profiles e.g. from PD2 or from Deep Research. the Outputs should be thoughtful client ready pages. I assume we will produce 5-10 profiles per week.

The purpose of the OPP.py is to produce text based output that I can hand to our graphics team to create a powerpoint slide which consists of the following:
- Title
- Subttitle
- 4 text boxes (2x2 format), each with a box title and then a few bullet points (each bullet is one sentence) underneath. Each sentence in each bullet has the 1-2 most important words in bold fonts, the LLM should decide which ones they are, I am ok with that.

The final output should be in markdown. At least initially. This is an intermediate product and then my graphics team has to simply copy paste into PPT. We will turn this into PPT later, but not now.

The output files should be saved into the folder ProfileFiles and follow the same naming convention as the ones in the folder ReportFiles, including date and time. Add the folder ProfileFiles to .gitignore. OPP.py needs to extract the company name, same as for PD2. For the filename and for the page title.

Overarching principle: Please keep the code as simple as possible.

Ideally we leverage functions from PD2 to keep it simple. you decide what functions we can import. Import as required, e.g. utils in src.

As far as word count instructions are concerned, let's trust the LLM.

As for the page to be produced, here is what we need and how we should go about it.
Content generation instructions are in single square brackets. 
Keep each sentence as short as possible, no longer than 15 words. Ideally, each sentence should have at least one number. If there is no meaningful data in the source files to write the sentence as instructed, then don't write it. Do not make up stuff, just because I ask for it. A precise sentence which may not perfectly suit the purpose is better than a beutifully tailored sentence which is not grounded. in case you don't have adequate data to produce an adequate sentence, then skip it altogether and omit silently.

Output as follows:
[Title: Company name]
[Subtitle: key message about the company for the potential investor or potential buyer, in 4-8 words]

[Box 1 top left] Company Overview
[one sentence where the company is based and what its primary business is]
[one sentence on the company's primary operating footprint, i.e. where its people or assets (whatever is more important) are based, with numbers]
[one sentence on the mix of products and services, indicating what's most important]
[one sentence on the mix of geography, indicating what's most important]
[one sentence on the mix of customers, indicating what's most important]
[one sentence on the key suppliers and concentration risk, if any]

[Box 2 top right] Operational KPIs and Competitive Posiioning
[3 sentences with the 3 most important operational KPIs, only the most recent ones]
[one sentence on market position and market share by products and services]
[one sentence on market position and market share by geography]
[one sentence on market position and market share by customer segment]
[in case the data is missing, then use the company's narrative]

[Box 3 bottom left] Financial KPIs
[one sentence on most recent revenue and recent growth]
[one sentence on EBITDA or, absent that, operating profit or, absent that, profit, as well as recent growth or margins, whatever is more important for the reader]
[one sentence on financials by segment, as many numbers as possible]
[one sentence on balance sheet highlights, i.e. what's the single most important thing about the company's balance sheet, sometimes that's about net debt, sometimes about impariments, you need to judge]
[one sentence on the single most important MDNA highlight]

[Box 4 Bottom right] Strategic Considerations
[one sentence on material recent strategic initiatives or transactions]
[one sentence on the company's history of partnerships]
[one sentence on the key decision makers at the management level and their strategic agenda]
[one sentence on the key decision makers at the shareholder level and their strategic agenda towards the company, including if they have indicated willingness to sell]
[one sentence on a strategic observation that would be highly interesting to a potential buyer, including strategic disconnects that warrant further analysis before proceeding with an acquisition]
[one sentence on what is the key attraction of the company to a potential buyer, but this needs to sound deeply sophisticated and be backed up by numbers]
[again, if you don't have the data then stick with the company's narrative]

these instructions in square brackets need to go verbatim into the prompt instructions.

In the event the data is incomplete, produce the profile anyway, simply omit those sentences that couldnt be written silently.
As for the UI and the processing output, keep the terminal output similar to PD2.

The initial version of OPP.py is a single shot, fully automoated profile page generator. Eventually, I would like to introduce the checking and refining routines we used in PD2, but I don't need a human in the loop.

No branding header on the output page, for now.

OPP.py should create a similar run log as PD2, because we will eventually introduce the refinement loops.

As for the model temperature, let's start with 0.5.
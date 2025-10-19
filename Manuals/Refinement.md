I would like OPP.py to run refinement loops, similar to PD2.

One the initial run has been finished, OPP.py should enter into the first refinement loop as follows:

Completeness Check
- Temperature: 0.2 (low - precise/systematic)
- What it does: Compares the draft against source documents to identify gaps
- Output: An "ADD list" with max 5 specific missing items
- Does NOT add or rewrite anything - just produces a checklist

Send the LLM the following instructions:
Based on the following instruction

<insert initial instruction prompt here>

and based on the following initial output

<insert the md file from the initial profile result>

and based on the following original input

<insert original sourcefiles here>

do evaluate if the output is correct and if it represents the target company appropriately. If amendments are needed, suggest them as follows

The prompt instructs:
"identify missing data that belongs specifically in this section"
"Be EXTREMELY specific - include exact data point and source location"
"Output ONLY the ADD list. No preamble or explanation."

Format of ADD list:
- [CRITICAL] Specific missing data with exact source
- [IMPORTANT] Relevant data with exact source
- [USEFUL] Supporting data with exact source

Step 3: Enhanced Draft 

  - Temperature: 0.6 (medium)
  - What it does: ADDS specific content while preserving everything
  - This is ADDITIVE, not a rewrite

  Key instructions in the prompt:
  "Add ALL items from the ADD list using the exact data from source documents"
  "Preserve all existing content - do not remove anything"
  "Maintain narrative flow - integrate additions smoothly into appropriate sections"
  "If an ADD item duplicates existing content, enhance rather than duplicate"

  So the LLM:
  1. Takes the current draft
  2. Reads the ADD list
  3. Looks up the data in source documents
  4. Weaves in the missing content at appropriate places
  5. Keeps everything else intact

  ---
Step 4: Deep Analysis & Polish

  - Temperature: 0.6 (medium)
  - What it does: COMPLETE REWRITE from scratch 
  - This is a full rewrite with aggressive condensation

  Key instructions:
  "You are condensing content to its most essential elements"
  "FINAL TARGET: Maximum 500 words in each of the 4 sections - absolutely no exceptions"
  "Keep only content that meets BOTH criteria:
    - Directly relevant to the section topic and requirements
    - Affects company prospects or value creation"

  What gets removed:
  - Generic statements and obvious observations
  - Repetitive points
  - Elaborate explanations
  - Anything that doesn't pass the relevance filter

  What gets preserved:
  - Specific metrics and trends
  - Key comparisons
  - Unusual patterns or anomalies

  Have a look at PD2 to understand the flow of prompts.
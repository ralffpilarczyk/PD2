import os
import google.generativeai as genai
from typing import List, Dict, Tuple
from dotenv import load_dotenv
from datetime import datetime

# Load environment variables
load_dotenv()

# Configure Gemini
genai.configure(api_key=os.environ.get("GEMINI_API_KEY"))

# Import test sections
from src.profile_sections import sections

class MultiSectionTester:
    def __init__(self, markdown_files: List[str]):
        """Initialize with list of markdown file paths"""
        self.full_context = self._load_markdown_files(markdown_files)
        self.model = genai.GenerativeModel('gemini-2.5-flash')
        
        # Create output directory
        self.output_dir = f"output_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        os.makedirs(self.output_dir, exist_ok=True)
    
    def _load_markdown_files(self, file_paths: List[str]) -> str:
        """Load and concatenate markdown files"""
        contents = []
        for path in file_paths:
            with open(path, 'r', encoding='utf-8') as f:
                contents.append(f"--- Document: {os.path.basename(path)} ---\n{f.read()}\n")
        return "\n\n".join(contents)
    
    def _save_output(self, filename: str, content: str):
        """Save output to file"""
        filepath = os.path.join(self.output_dir, filename)
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(content)
    
    def single_agent_approach(self, section: Dict) -> str:
        """Single agent approach for any section"""
        prompt = f"""You are an expert analyst. Complete this section precisely.

Section {section['number']}: {section['title']}

Instructions:
{section['specs']}

Documents:
{self.full_context}

Output in HTML format starting with <div class="section" id="section-{section['number']}">"""
        
        response = self.model.generate_content(prompt)
        return response.text
    
    def multi_agent_improved_v1(self, section: Dict) -> str:
        """Fix 1: Explicit preservation instructions"""
        # Agent 1: Extract with preservation
        extract_prompt = f"""Extract data for Section {section['number']}: {section['title']}

CRITICAL: Include source references for EVERY data point
Format each fact as: [FACT] | [SOURCE: doc, page]

Documents:
{self.full_context}

PRESERVE: All numbers, dates, sources, analytical insights"""
        
        extract_response = self.model.generate_content(extract_prompt)
        extracted = extract_response.text
        self._save_output(f"section_{section['number']}/v1_extracted.txt", extracted)
        
        # Agent 2: Analyze with preservation
        analyze_prompt = f"""Analyze using these facts:
{extracted}

Task: {section['specs']}

PRESERVE: All numbers, dates, sources from the extracted data
Include [SOURCE] after each claim"""
        
        analyze_response = self.model.generate_content(analyze_prompt)
        analysis = analyze_response.text
        self._save_output(f"section_{section['number']}/v1_analysis.txt", analysis)
        
        # Agent 3: Edit with preservation
        edit_prompt = f"""Polish this analysis:
{analysis}

FORBIDDEN: Removing numbers, sources, or analytical conclusions
ALLOWED: Removing only redundant phrases like "it appears", "furthermore"
PRESERVE: All facts, figures, and citations

Output HTML starting with <div class="section" id="section-{section['number']}">"""
        
        edit_response = self.model.generate_content(edit_prompt)
        return edit_response.text
    
    def multi_agent_improved_v2(self, section: Dict) -> str:
        """Fix 2: Chain-of-custody approach with tagged format"""
        # Agent 1: Tagged extraction
        extract_prompt = f"""Extract data for Section {section['number']}: {section['title']}

Use this EXACT format for every fact:
[FACT: specific data point] | [SOURCE: document name, page X] | [DATE: when applicable]

Example:
[FACT: Revenue grew 10.5% to S$114.9M] | [SOURCE: AR 2024, p.4] | [DATE: FY2024]

Documents:
{self.full_context}"""
        
        extract_response = self.model.generate_content(extract_prompt)
        extracted = extract_response.text
        self._save_output(f"section_{section['number']}/v2_tagged_extract.txt", extracted)
        
        # Agent 2: Analyze maintaining tags
        analyze_prompt = f"""Using tagged facts below, create analysis for:
{section['specs']}

Tagged facts:
{extracted}

RULES:
- Use facts exactly as tagged
- Maintain [SOURCE] tags inline
- Connect facts logically

Example output:
Revenue grew significantly [FACT: 10.5%] | [SOURCE: AR 2024, p.4] due to...
"""
        
        analyze_response = self.model.generate_content(analyze_prompt)
        analysis = analyze_response.text
        self._save_output(f"section_{section['number']}/v2_tagged_analysis.txt", analysis)
        
        # Agent 3: Format preserving tags
        format_prompt = f"""Convert tagged analysis to clean HTML:
{analysis}

Rules:
- Convert [SOURCE: X, p.Y] to <sup>[X, p.Y]</sup>
- Keep ALL facts and sources
- Remove tags but preserve content

Output HTML starting with <div class="section" id="section-{section['number']}">"""
        
        format_response = self.model.generate_content(format_prompt)
        return format_response.text
    
    def single_specialized_agent(self, section: Dict) -> str:
        """Fix 3: Single specialized agent per section type"""
        
        if section['number'] == 1:  # Operating Footprint
            prompt = f"""You are an operations analyst. In ONE focused pass:

1. Extract employee distribution, assets, geographic data
2. Analyze trends and strategic implications  
3. Write concisely with sources

For EACH data point: [metric] ([period]) Source: [doc, page]

{section['specs']}

Documents:
{self.full_context}

Output structured HTML under 500 words."""

        elif section['number'] == 7:  # Financials
            prompt = f"""You are a financial analyst. In ONE pass:

1. Create consolidated financial tables (3 years + 5 quarters)
2. Calculate margins and growth rates
3. Add trend analysis (2 achievements, 2 challenges)

Format: Clean HTML tables with period headers
Include source footnotes

{section['specs']}

Documents:
{self.full_context}"""

        elif section['number'] == 13:  # Strategic
            prompt = f"""You are a strategy consultant. In ONE pass:

1. Identify top 3 objectives (future goals) with timeframes/KPIs
2. Identify top 3 strategies (current methods) with capabilities
3. Note any disconnects

Clear separation between objectives vs strategies

{section['specs']}

Documents:
{self.full_context}"""

        elif section['number'] == 21:  # Competitive
            prompt = f"""You are a competitive analyst. Identify 3 advantages:

For EACH advantage (150 words max):
- Clear statement
- 2-3 supporting metrics with dates
- Why it beats competitors (even if inferring)
- Financial impact

Start: "The company's 3 competitive advantages are:"

{section['specs']}

Documents:
{self.full_context}"""

        elif section['number'] == 29:  # Due Diligence
            prompt = f"""You are a due diligence specialist. Analyze 3 operating metrics:

For EACH metric:
- Definition (1 sentence)
- Historical data with trends (bullet points)
- Drivers and sustainability analysis (3 sentences)
- 2 specific DD questions

Focus: market share, volumes, pricing, unit economics, retention
Exclude: financial metrics

{section['specs']}

Documents:
{self.full_context}"""

        else:  # Section 32 - Appendix
            prompt = f"""Organize all tables into HTML appendix:

{section['specs']}

Documents:
{self.full_context}"""
        
        response = self.model.generate_content(prompt)
        return response.text
    
    def alternative_approach_1(self, section: Dict) -> str:
        """Alternative: Iterative refinement with quality gates"""
        # First pass: Complete draft
        draft_prompt = f"""Create initial draft for Section {section['number']}: {section['title']}

{section['specs']}

Documents:
{self.full_context}

Don't worry about length - focus on completeness."""
        
        draft_response = self.model.generate_content(draft_prompt)
        draft = draft_response.text
        
        # Quality check
        quality_prompt = f"""Rate this draft on:
1. Completeness (all requirements met)
2. Accuracy (facts correct with sources)
3. Clarity (well-structured)
4. Conciseness (no fluff)

Draft:
{draft}

For each dimension, provide:
- Score (1-10)
- Specific issues to fix"""
        
        quality_response = self.model.generate_content(quality_prompt)
        quality_check = quality_response.text
        self._save_output(f"section_{section['number']}/quality_check.txt", quality_check)
        
        # Targeted revision
        revision_prompt = f"""Revise this draft addressing these specific issues:
{quality_check}

Original draft:
{draft}

Maintain all accurate content while fixing identified problems.
Output final HTML."""
        
        revision_response = self.model.generate_content(revision_prompt)
        return revision_response.text
    
    def alternative_approach_2(self, section: Dict) -> str:
        """Alternative: Constraint-based generation"""
        constraints = {
            1: "Max 400 words. Must include employee counts, asset details, geographic revenue.",
            7: "Tables required. Max 100 words of commentary. All metrics for 3 years.",
            13: "Bullet points only. Clear headers for objectives vs strategies.",
            21: "Exactly 3 advantages. 150 words each. Must compare to competitors.",
            29: "Exactly 3 metrics. Structured format. 2 questions per metric.",
            32: "Tables only. No commentary. All data preserved."
        }
        
        prompt = f"""Generate Section {section['number']}: {section['title']}

HARD CONSTRAINTS:
{constraints.get(section['number'], 'Be concise and complete.')}

QUALITY REQUIREMENTS:
- Every claim needs a source
- Numbers must include dates/periods
- No generic statements

{section['specs']}

Documents:
{self.full_context}

Penalty for violating constraints: Response will be rejected."""
        
        response = self.model.generate_content(prompt)
        return response.text
    
    def test_all_approaches(self, section_num: int):
        """Test all approaches on one section"""
        section = next(s for s in sections if s['number'] == section_num)
        section_dir = os.path.join(self.output_dir, f"section_{section_num}")
        os.makedirs(section_dir, exist_ok=True)
        
        approaches = [
            ("single_agent", self.single_agent_approach),
            ("multi_v1_preservation", self.multi_agent_improved_v1),
            ("multi_v2_chain_custody", self.multi_agent_improved_v2),
            ("specialized_agent", self.single_specialized_agent),
            ("iterative_refinement", self.alternative_approach_1),
            ("constraint_based", self.alternative_approach_2)
        ]
        
        results = {}
        for name, method in approaches:
            print(f"Running {name}...")
            try:
                output = method(section)
                self._save_output(f"section_{section_num}/{name}.html", output)
                results[name] = {
                    "words": len(output.split()),
                    "has_sources": "<sup>" in output or "Source:" in output,
                    "has_tables": "<table" in output
                }
            except Exception as e:
                print(f"Error in {name}: {e}")
                results[name] = {"error": str(e)}
        
        # Summary
        summary = f"Section {section_num} Test Results\n\n"
        for name, metrics in results.items():
            summary += f"{name}: {metrics}\n"
        
        self._save_output(f"section_{section_num}/comparison.txt", summary)
        print(summary)
        return results


# Usage
if __name__ == "__main__":
    from tkinter import filedialog
    import tkinter as tk
    
    root = tk.Tk()
    root.withdraw()
    
    print("Select markdown files")
    markdown_files = filedialog.askopenfilenames(
        title="Select Markdown Files",
        filetypes=[("Markdown files", "*.md"), ("All files", "*.*")]
    )
    
    if not markdown_files:
        print("No files selected.")
    else:
        tester = MultiSectionTester(list(markdown_files))
        
        available_sections = [s['number'] for s in sections]
        print(f"\nAvailable sections: {available_sections}")
        
        choice = input("Enter section number or 'all' for all sections: ").strip()
        
        if choice.lower() == 'all':
            print("Running all approaches on all sections...")
            for section in sections:
                tester.test_all_approaches(section['number'])
        else:
            try:
                section_num = int(choice)
                if section_num in available_sections:
                    tester.test_all_approaches(section_num)
                else:
                    print(f"Invalid section number. Available sections: {available_sections}")
            except ValueError:
                print("Invalid input. Enter a section number or 'all'")
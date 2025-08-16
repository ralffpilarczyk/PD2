import os
import glob
from typing import Dict, List
from datetime import datetime
from pathlib import Path
import google.generativeai as genai
from .utils import retry_with_backoff, thread_safe_print, clean_markdown_tables, validate_and_fix_tables
from .profile_sections import sections
import markdown
from markdown.extensions import tables


class ProfileGenerator:
    """Handles generation of professional HTML profiles"""
    
    def __init__(self, run_timestamp: str, model_name: str = 'gemini-2.5-flash'):
        """Initialize profile generator"""
        self.run_timestamp = run_timestamp
        self.model_name = model_name
        # Use medium temperature model for company name extraction
        self.model = genai.GenerativeModel(
            model_name,
            generation_config=genai.types.GenerationConfig(temperature=0.6)
        )
    
    def generate_html_profile(self, results: Dict, section_numbers: List[int], full_context: str, sections_param: List[Dict]):
        """Generate complete markdown and HTML profile documents from existing section files"""
        
        # Extract company name
        company_name = self._extract_company_name(full_context)
        thread_safe_print(f"Generating combined profile for: {company_name}")
        
        # Collect markdown from existing section files
        combined_markdown, processed_sections = self._collect_section_markdown()
        
        if not combined_markdown:
            thread_safe_print("No section markdown files found!")
            return None
        
        # Clean company name for filename
        clean_company_name = company_name.replace(' ', '_').replace('.', '').replace(',', '').replace('/', '_')
        
        # Save combined markdown file
        md_filename = f"{clean_company_name}_profile.md"
        md_path = f"runs/run_{self.run_timestamp}/{md_filename}"
        
        with open(md_path, 'w', encoding='utf-8') as f:
            f.write(combined_markdown)
        thread_safe_print(f"Combined markdown saved: {md_path}")
        
        # Generate HTML from combined markdown
        html_path = self._generate_html_from_markdown(combined_markdown, processed_sections, company_name, clean_company_name)
        
        return html_path
    
    def _collect_section_markdown(self):
        """Collect all section markdown files from the current run directory"""
        combined_markdown = ""
        processed_sections = []
        
        run_dir = f"runs/run_{self.run_timestamp}"
        
        # Look for section directories (section_01, section_02, etc.)
        section_dirs = glob.glob(f"{run_dir}/section_*")
        # Sort numerically by section number
        section_dirs = sorted(section_dirs, key=lambda x: int(os.path.basename(x).split('_')[1]))
        
        for section_dir in section_dirs:
            section_num = int(os.path.basename(section_dir).split('_')[1])
            section_title = self._get_section_title(section_num)
            
            # Look for markdown files in the section directory
            md_files = glob.glob(f"{section_dir}/*.md")
            
            if md_files:
                # Find the final section file - prioritize discovery augmented, then step_4
                final_file = None
                for md_file in md_files:
                    if 'step_5_discovery_augmented.md' in md_file:
                        final_file = md_file
                        break
                    elif 'step_4_final_section.md' in md_file:
                        final_file = md_file
                        # Don't break - keep looking for discovery augmented
                    elif 'final_section.md' in md_file and not final_file:
                        final_file = md_file
                
                # If no final file found, use the first one
                md_file = final_file if final_file else md_files[0]
                
                thread_safe_print(f"Reading section {section_num}: {section_title} - {md_file}")
                
                with open(md_file, 'r', encoding='utf-8') as f:
                    content = f.read()

                # Skip near-empty content to reduce blank sections
                if not content or len(content.strip()) < 50:
                    thread_safe_print(f"  → Skipping section {section_num} due to empty content in {os.path.basename(md_file)}")
                    continue
                
                # Clean corrupted tables first
                content = clean_markdown_tables(content)

                # Clean up problematic wrappers and normalize formatting
                content = self._clean_markdown_content(content)

                # Final table validation/normalization (single utility)
                content = validate_and_fix_tables(content)
                    
                # Add section with proper title and anchor
                combined_markdown += f'\n\n<a id="section_{section_num}"></a>\n\n# Section {section_num}: {section_title}\n\n'
                combined_markdown += content
                combined_markdown += f"\n\n---\n\n"
                
                # Track processed sections for TOC
                processed_sections.append((section_num, section_title))
        
        # Apply footnote label scoping to avoid cross-section conflicts
        combined_markdown = self._manage_footnotes(combined_markdown)
        
        return combined_markdown, processed_sections
    
    def _clean_markdown_content(self, content):
        """Clean up problematic markdown code block wrappers and malformed tables from section content"""
        content = content.strip()
        
        # Special handling for Section 32 - check if content is wrapped in HTML code blocks
        if content.startswith('```html\n') and content.endswith('\n```'):
            # Remove the HTML code block wrapper for Section 32
            content = content[8:-4]  # Remove ```html\n at start and \n``` at end
            content = content.strip()
            thread_safe_print("  → Removed HTML code block wrapper (Section 32)")
        elif content.startswith('```html'):
            # Handle case without newline after ```html
            content = content[7:]  # Remove ```html at start
            if content.endswith('```'):
                content = content[:-3]  # Remove ``` at end
            content = content.strip()
            thread_safe_print("  → Removed HTML code block wrapper (Section 32)")
        
        # Check if content is wrapped in markdown code block
        elif content.startswith('```markdown\n') and content.endswith('\n```'):
            # Remove the wrapper
            content = content[12:-4]  # Remove ```markdown\n at start and \n``` at end
            content = content.strip()
            thread_safe_print("  → Cleaned problematic markdown code block wrapper")
        elif content.startswith('```markdown'):
            # Handle case without newline after ```markdown
            content = content[11:]  # Remove ```markdown at start
            if content.endswith('```'):
                content = content[:-3]  # Remove ``` at end
            content = content.strip()
            thread_safe_print("  → Cleaned problematic markdown code block wrapper")
        
        # Table structure fixes are handled centrally by validate_and_fix_tables
        import re
        
        # Remove duplicate section titles that LLM sometimes generates
        # Look for patterns like "## SECTION 1: Title" or "# SECTION 1: Title" at the start
        lines = content.split('\n')
        if lines and (lines[0].startswith('## SECTION ') or lines[0].startswith('# SECTION ') or lines[0].startswith('SECTION ')):
            lines = lines[1:]  # Remove the first line
            content = '\n'.join(lines).strip()
            thread_safe_print("  → Removed duplicate section title")
        
        # Ensure blank lines before tables for proper markdown parsing
        lines = content.split('\n')
        fixed_lines = []
        for i, line in enumerate(lines):
            # Check if current line is a table (starts with |)
            if line.strip().startswith('|') and i > 0:
                prev_line = lines[i-1].strip()
                # If previous line exists and isn't empty, add blank line
                if prev_line and not prev_line.startswith('|'):
                    if not fixed_lines or fixed_lines[-1].strip():  # Avoid multiple blank lines
                        fixed_lines.append('')
                        thread_safe_print("  → Added blank line before table")
            
            # Check if current line starts a list (-, *, or numbered)
            if re.match(r'^\s*(?:[-*]|\d+\.)', line) and i > 0:
                prev_line = lines[i-1].strip()
                # If previous line exists and isn't empty or part of a list
                if prev_line and not re.match(r'^\s*(?:[-*]|\d+\.)', prev_line):
                    if not fixed_lines or fixed_lines[-1].strip():  # Avoid multiple blank lines
                        fixed_lines.append('')
                        thread_safe_print("  → Added blank line before list")
            
            fixed_lines.append(line)
        
        content = '\n'.join(fixed_lines)
            
        return content
    
    def _manage_footnotes(self, markdown_content):
        """Scope markdown footnote labels per section to avoid cross-section collisions.

        Transforms [^1] and corresponding definitions [^1]: ... into
        [^s{section}_{1}] per section. Does not renumber globally.
        """
        import re

        parts = re.split(r'(<a id="section_(\d+)"></a>)', markdown_content)
        processed = []

        i = 0
        while i < len(parts):
            part = parts[i]
            if not part:
                i += 1
                continue

            # Section anchor
            if part.startswith('<a id="section_'):
                processed.append(part)
                # Next part should be section number captured by split; skip it in output
                # parts: [text, anchor, num, content, anchor, num, content, ...]
                # After anchor and num, the next part is the content
                i += 1  # move to captured group with section number
                if i < len(parts):
                    section_num = parts[i]
                else:
                    section_num = None
                i += 1
                if i < len(parts):
                    section_content = parts[i]
                else:
                    section_content = ''

                if section_num:
                    prefix = f"s{section_num}_"
                    # Map [^n] -> [^{prefix}{n}] in references
                    section_content = re.sub(r"\[\^(\d+)\]", lambda m: f"[^{prefix}{m.group(1)}]", section_content)
                    # Map definitions ^[n]: -> ^[prefix+n]:
                    section_content = re.sub(r"^\[\^(\d+)\]:", lambda m: f"[^{prefix}{m.group(1)}]:", section_content, flags=re.MULTILINE)

                processed.append(section_content)
            else:
                # Preamble or trailing content
                processed.append(part)
            i += 1

        return ''.join(processed)
    
    def _get_section_title(self, section_num):
        """Get the proper section title from section definitions"""
        for section in sections:
            if section['number'] == section_num:
                return section['title']
        return f"Section {section_num}"
    
    def _generate_html_from_markdown(self, combined_markdown, processed_sections, company_name, clean_company_name):
        """Generate HTML file from combined markdown"""
        
        # Generate cover page
        cover_html = self._generate_cover_page(processed_sections, company_name)
        
        # Convert content to HTML
        content_html = self._markdown_to_html(combined_markdown)
        css_styles = self._get_css_styles()
        
        # Get current date
        generation_date = datetime.now().strftime('%B %d, %Y')
        
        # Create complete HTML document with cover page
        full_html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{company_name} - ProfileDash 2.0</title>
    <style>
{css_styles}
    </style>
</head>
<body>
{cover_html}
    <div class="profile-content">
        <div class="section">
            <div class="section-content">
{content_html}
            </div>
        </div>
    </div>
</body>
</html>"""
        
        # Save HTML file
        html_filename = f"{clean_company_name}_profile.html"
        html_path = f"runs/run_{self.run_timestamp}/{html_filename}"
        
        with open(html_path, 'w', encoding='utf-8') as f:
            f.write(full_html)
        
        thread_safe_print(f"Combined HTML profile saved: {html_path}")
        thread_safe_print(f"Processed {len(processed_sections)} sections")
        
        return html_path
    
    def _generate_cover_page(self, processed_sections, company_name):
        """Generate cover page with table of contents"""
        
        # Get current date
        generation_date = datetime.now().strftime('%B %d, %Y')
        
        # Human-readable model label for cover note
        model_label_map = {
            'gemini-2.5-flash': 'Gemini 2.5 Flash',
            'gemini-2.5-flash-lite': 'Gemini 2.5 Flash-Lite',
            'gemini-2.0-flash': 'Gemini 2.0 Flash',
        }
        model_label = model_label_map.get(self.model_name, self.model_name)
        
        # Group sections by category (like the real system does)
        groups = {
            "Company Profile": [s for s in processed_sections if 1 <= s[0] <= 12],
            "Strategy and SWOT": [s for s in processed_sections if 13 <= s[0] <= 18], 
            "Sellside Positioning": [s for s in processed_sections if 19 <= s[0] <= 25],
            "Buyside Due Diligence": [s for s in processed_sections if 26 <= s[0] <= 31],
            "Data Book": [s for s in processed_sections if s[0] == 32]
        }
        
        cover_html = f'''
        <div class="cover-page">
            <h1 class="company-name">{company_name}</h1>
            <h2 class="product-name">ProfileDash 2.0</h2>
            <div class="generation-info">
                Profile generated via {model_label} on {generation_date}<br>
                Under MIT License
            </div>
            
            <div class="toc-section">
                <h2>Table of Contents</h2>
        '''
        
        for group_name, group_sections in groups.items():
            if group_sections:
                cover_html += f'<div class="toc-group"><strong>{group_name}</strong></div>\n'
                for section_num, section_title in sorted(group_sections):
                    cover_html += f'<div class="toc-item"><a href="#section_{section_num}">Section {section_num}: {section_title}</a></div>\n'
                cover_html += "<br>\n"
        
        cover_html += '''
            </div>
        </div>
        <div class="page-break"></div>
        '''
        
        return cover_html

    def _extract_company_name(self, full_context: str) -> str:
        """Extract company name from document context via LLM"""
        try:
            prompt = f"""Extract the primary company name from these documents.

DOCUMENTS:
{full_context[:3000]}  

Look for the main company being analyzed. This could be in:
- Document titles
- Headers and letterheads  
- "About [Company]" sections
- Financial statement headers
- Management discussion sections

Return ONLY the company name, nothing else. If multiple companies mentioned, return the PRIMARY company being analyzed.

If unclear, return "Company Profile"

Examples:
- "Apple Inc." → "Apple Inc."
- "Microsoft Corporation" → "Microsoft Corporation"  
- "Tesla, Inc." → "Tesla, Inc."
"""
            
            company_name = retry_with_backoff(
                lambda: self.model.generate_content(prompt).text.strip()
            )
            
            # Basic validation
            if len(company_name) > 100 or len(company_name) < 2:
                return "Company Profile"
            
            return company_name
            
        except Exception as e:
            thread_safe_print(f"Company name extraction failed: {e}")
            return "Company Profile"
    
    def _markdown_to_html(self, markdown_content: str) -> str:
        """Convert markdown content to HTML using proper markdown parser"""
        # Note: Removed 'nl2br' extension as it interferes with table rendering
        md = markdown.Markdown(extensions=['tables', 'fenced_code', 'extra', 'footnotes'])
        return md.convert(markdown_content)
    
    def _get_css_styles(self) -> str:
        """Return CSS styles for professional document formatting"""
        return '''
        body {
            font-family: 'Georgia', 'Times New Roman', serif;
            line-height: 1.6;
            margin: 0;
            padding: 0;
            color: #333;
        }
        
        .cover-page {
            min-height: 100vh;
            display: flex;
            flex-direction: column;
            justify-content: flex-start;
            align-items: center;
            text-align: center;
            padding: 60px 40px 40px 40px;
            page-break-after: always;
        }
        
        .company-name {
            font-size: 3.2em;
            font-weight: bold;
            color: #1a365d;
            margin-top: 80px;
            margin-bottom: 30px;
            letter-spacing: 2px;
            line-height: 1.1;
        }
        
        .product-name {
            font-size: 1.8em;
            font-weight: bold;
            color: #2d5a87;
            margin-bottom: 40px;
        }
        
        .generation-info {
            font-size: 1em;
            color: #666;
            margin-bottom: 60px;
            line-height: 1.4;
        }
        
        .toc-section {
            text-align: left;
            max-width: 600px;
            width: 100%;
            flex-grow: 1;
        }
        
        .toc-section h2 {
            font-size: 1.5em;
            color: #1a365d;
            border-bottom: 2px solid #2d5a87;
            padding-bottom: 10px;
            margin-bottom: 30px;
        }
        
        .toc-group {
            font-size: 1.1em;
            color: #2d5a87;
            margin: 15px 0 8px 0;
        }
        
        .toc-item {
            margin: 5px 0 5px 20px;
            font-size: 0.95em;
        }
        
        .toc-item a {
            color: #333;
            text-decoration: none;
            border-bottom: 1px dotted #666;
        }
        
        .toc-item a:hover {
            color: #2d5a87;
            border-bottom: 1px solid #2d5a87;
        }
        
        .page-break {
            page-break-before: always;
        }
        
        .profile-content {
            padding: 40px;
            max-width: 1000px;
            margin: 0 auto;
        }
        
        .section {
            margin-bottom: 40px;
        }
        
        .section-content {
            font-size: 1em;
            line-height: 1.7;
        }
        
        .data-table, table {
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
            font-size: 0.9em;
        }
        
        .data-table td, table td, table th {
            border: 1px solid #ddd;
            padding: 8px 12px;
            text-align: left;
        }
        
        .data-table tr:first-child td, table thead th, table tr:first-child td {
            background-color: #f8f9fa;
            font-weight: bold;
            color: #2d5a87;
        }
        
        .data-table tr:nth-child(even), table tr:nth-child(even) {
            background-color: #f8f9fa;
        }
        
        strong {
            color: #2d5a87;
            font-weight: bold;
        }
        
        p {
            margin: 10px 0;
            font-size: 1em;
            line-height: 1.6;
        }
        

        
        h1 {
            color: #1a365d;
            font-size: 1.8em;
            margin-top: 30px;
            margin-bottom: 20px;
            border-bottom: 3px solid #2d5a87;
            padding-bottom: 10px;
        }
        
        h2 {
            color: #2d5a87;
            font-size: 1.3em;
            margin-top: 25px;
            margin-bottom: 15px;
        }
        
        h3 {
            color: #4a5568;
            font-size: 1.1em;
            margin-top: 20px;
            margin-bottom: 10px;
        }
        
        @media print {
            .page-break {
                page-break-before: always;
            }
            
            .cover-page {
                page-break-after: always;
                min-height: auto;
                height: auto;
            }
            
            .company-name {
                margin-top: 40px;
            }
        }
        ''' 
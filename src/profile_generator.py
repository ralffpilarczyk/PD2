import os
import glob
from typing import Dict, List
from datetime import datetime
from pathlib import Path
import google.generativeai as genai
from .utils import retry_with_backoff, thread_safe_print, clean_markdown_tables, validate_and_fix_tables
from .profile_sections import sections
from .pdf_generator import generate_pdf_from_html
from . import __version__
import markdown
from markdown.extensions import tables


class ProfileGenerator:
    """Handles generation of professional HTML profiles"""
    
    def __init__(self, run_timestamp: str, model_name: str = 'gemini-3-flash-preview'):
        """Initialize profile generator"""
        self.run_timestamp = run_timestamp
        self.model_name = model_name
        # Use medium temperature model for company name extraction
        self.model = genai.GenerativeModel(
            model_name,
            generation_config=genai.types.GenerationConfig(temperature=0.6)
        )
    
    def generate_html_profile(self, results: Dict, section_numbers: List[int], company_name: str, sections_param: List[Dict], pdf_variant: str = None):
        """Generate complete markdown and HTML profile documents from existing section files

        Args:
            results: Dictionary of section results (unused for file collection)
            section_numbers: List of section numbers to include
            company_name: Company name for titles and filenames
            sections_param: Section definitions
            pdf_variant: Optional variant type - None (default), "vanilla", "insights", or "integrated"
        """

        # Company name is now passed directly (extracted using cached model in PD2.py)

        # Collect markdown from existing section files
        combined_markdown, processed_sections = self._collect_section_markdown(pdf_variant=pdf_variant)
        
        if not combined_markdown:
            thread_safe_print("⚠ No sections found!")
            return None
        
        # Clean company name for filename
        clean_company_name = company_name.replace(' ', '_').replace('.', '').replace(',', '').replace('/', '_')

        # Save combined markdown file (include variant in filename if specified)
        variant_suffix = f"_{pdf_variant}" if pdf_variant else ""
        md_filename = f"{clean_company_name}{variant_suffix}_profile.md"
        md_path = f"runs/run_{self.run_timestamp}/{md_filename}"

        with open(md_path, 'w', encoding='utf-8') as f:
            f.write(combined_markdown)

        # Generate HTML from combined markdown
        html_path = self._generate_html_from_markdown(combined_markdown, processed_sections, company_name, clean_company_name, pdf_variant=pdf_variant)
        
        return html_path
    
    def _collect_section_markdown(self, pdf_variant: str = None):
        """Collect all section markdown files from the current run directory

        Args:
            pdf_variant: Optional variant type - None (default), "vanilla", "insights", or "integrated"
        """
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
                # Select file based on variant type
                final_file = None

                if pdf_variant == "vanilla":
                    # Vanilla: Use Step 4 output only (standard polished description)
                    for md_file in md_files:
                        if 'step_4_final_section.md' in md_file:
                            final_file = md_file
                            break
                        elif 'final_section.md' in md_file and not final_file:
                            final_file = md_file

                elif pdf_variant == "insights":
                    # Insights: Use Step 8 synthesis only (ground truth insights)
                    for md_file in md_files:
                        if 'step_8_synthesis.md' in md_file:
                            final_file = md_file
                            break
                    # Fallback to step_4 if no insights available
                    if not final_file:
                        for md_file in md_files:
                            if 'step_4_final_section.md' in md_file:
                                final_file = md_file
                                break

                elif pdf_variant == "integrated":
                    # Integrated: Use Step 9 integrated output (insights woven in)
                    for md_file in md_files:
                        if 'step_9_integrated.md' in md_file:
                            final_file = md_file
                            break
                    # Fallback to step_4 if no integrated available
                    if not final_file:
                        for md_file in md_files:
                            if 'step_4_final_section.md' in md_file:
                                final_file = md_file
                                break

                else:
                    # Default: Use step_4 output (standard polished description)
                    for md_file in md_files:
                        if 'step_4_final_section.md' in md_file:
                            final_file = md_file
                            break
                        elif 'final_section.md' in md_file and not final_file:
                            final_file = md_file

                # If no final file found, use the first one
                md_file = final_file if final_file else md_files[0]

                # Silent collection - no output per section
                with open(md_file, 'r', encoding='utf-8') as f:
                    content = f.read()

                # Skip near-empty content to reduce blank sections
                if not content or len(content.strip()) < 50:
                    thread_safe_print(f"  → Skipping section {section_num} (empty)")
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
        elif content.startswith('```html'):
            # Handle case without newline after ```html
            content = content[7:]  # Remove ```html at start
            if content.endswith('```'):
                content = content[:-3]  # Remove ``` at end
            content = content.strip()
        
        # Check if content is wrapped in markdown code block
        elif content.startswith('```markdown\n') and content.endswith('\n```'):
            # Remove the wrapper
            content = content[12:-4]  # Remove ```markdown\n at start and \n``` at end
            content = content.strip()
        elif content.startswith('```markdown'):
            # Handle case without newline after ```markdown
            content = content[11:]  # Remove ```markdown at start
            if content.endswith('```'):
                content = content[:-3]  # Remove ``` at end
            content = content.strip()
        
        # Table structure fixes are handled centrally by validate_and_fix_tables
        import re
        
        # Remove duplicate section titles that LLM sometimes generates
        # Look for patterns like "## SECTION 1: Title" or "# SECTION 1: Title" at the start
        lines = content.split('\n')
        if lines and (lines[0].startswith('## SECTION ') or lines[0].startswith('# SECTION ') or lines[0].startswith('SECTION ')):
            lines = lines[1:]  # Remove the first line
            content = '\n'.join(lines).strip()
        
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

            # Check if current line starts a list (-, *, or numbered)
            if re.match(r'^\s*(?:[-*]|\d+\.)', line) and i > 0:
                prev_line = lines[i-1].strip()
                # If previous line exists and isn't empty or part of a list
                if prev_line and not re.match(r'^\s*(?:[-*]|\d+\.)', prev_line):
                    if not fixed_lines or fixed_lines[-1].strip():  # Avoid multiple blank lines
                        fixed_lines.append('')
            
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
    
    def _generate_html_from_markdown(self, combined_markdown, processed_sections, company_name, clean_company_name, pdf_variant: str = None):
        """Generate HTML file from combined markdown

        Args:
            combined_markdown: Combined markdown content
            processed_sections: List of (section_num, section_title) tuples
            company_name: Company name for titles
            clean_company_name: Cleaned company name for filenames
            pdf_variant: Optional variant type for filename
        """

        # Generate cover page (include variant in subtitle if specified)
        cover_html = self._generate_cover_page(processed_sections, company_name, pdf_variant=pdf_variant)

        # Convert content to HTML
        content_html = self._markdown_to_html(combined_markdown)

        # Get current date in both formats
        generation_date = datetime.now().strftime('%B %d, %Y')
        footer_date = datetime.now().strftime('%d-%b-%y')

        css_styles = self._get_css_styles(footer_date)
        
        # Create complete HTML document with cover page
        full_html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{company_name} - ProfileDash {__version__}</title>
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
        
        # Create ReportFiles directory if it doesn't exist
        report_dir = Path("ReportFiles")
        report_dir.mkdir(parents=True, exist_ok=True)
        
        # Convert run timestamp format from YYYY_MM_DD_HH_MM_SS to YYMMDD_HHMM
        # Example: 2025_09_05_16_23_45 -> 250905_1623
        timestamp_parts = self.run_timestamp.split('_')
        if len(timestamp_parts) >= 5:
            year = timestamp_parts[0][2:]  # Last 2 digits of year
            month = timestamp_parts[1]
            day = timestamp_parts[2]
            hour = timestamp_parts[3]
            minute = timestamp_parts[4]
            compact_timestamp = f"{year}{month}{day}_{hour}{minute}"
        else:
            # Fallback if timestamp format is unexpected
            compact_timestamp = datetime.now().strftime('%y%m%d_%H%M')
        
        # Save HTML file to ReportFiles with new naming format (include variant if specified)
        variant_suffix = f"_{pdf_variant}" if pdf_variant else ""
        html_filename = f"{clean_company_name}{variant_suffix}_{compact_timestamp}.html"
        html_path = report_dir / html_filename
        
        with open(html_path, 'w', encoding='utf-8') as f:
            f.write(full_html)

        thread_safe_print(f"✓ HTML: {html_path.name}")

        # Generate PDF from HTML
        pdf_path = generate_pdf_from_html(str(html_path))
        if pdf_path:
            thread_safe_print(f"✓ PDF: {Path(pdf_path).name}")

        return str(html_path)
    
    def _generate_cover_page(self, processed_sections, company_name, pdf_variant: str = None):
        """Generate cover page with table of contents

        Args:
            processed_sections: List of (section_num, section_title) tuples
            company_name: Company name
            pdf_variant: Optional variant type for subtitle
        """

        # Get current date
        generation_date = datetime.now().strftime('%B %d, %Y')

        # Human-readable model label for cover note
        model_label_map = {
            'gemini-3-flash-preview': 'Gemini 3 Flash Preview',
            'gemini-3-pro-preview': 'Gemini 3 Pro Preview',
        }
        model_label = model_label_map.get(self.model_name, self.model_name)

        # Variant label for subtitle
        variant_labels = {
            'vanilla': ' (Vanilla)',
            'insights': ' (Insights Only)',
            'integrated': ' (Integrated)'
        }
        variant_label = variant_labels.get(pdf_variant, '')

        # Group sections by category (like the real system does)
        groups = {
            "Company Profile": [s for s in processed_sections if 1 <= s[0] <= 13],
            "Strategy and SWOT": [s for s in processed_sections if 14 <= s[0] <= 19],
            "Sellside Positioning": [s for s in processed_sections if 20 <= s[0] <= 26],
            "Buyside Due Diligence": [s for s in processed_sections if 27 <= s[0] <= 32],
            "Financial Pattern Analysis": [s for s in processed_sections if s[0] == 33],
            "Data Book": [s for s in processed_sections if s[0] == 34]
        }

        cover_html = f'''
        <div class="cover-page">
            <h1 class="company-name">{company_name}</h1>
            <h2 class="product-name">ProfileDash {__version__}{variant_label}</h2>
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
            thread_safe_print(f"⚠ Could not extract company name")
            return "Company Profile"
    
    def _markdown_to_html(self, markdown_content: str) -> str:
        """Convert markdown content to HTML using proper markdown parser"""
        # Note: Removed 'nl2br' extension as it interferes with table rendering
        md = markdown.Markdown(extensions=['tables', 'fenced_code', 'extra', 'footnotes'])
        return md.convert(markdown_content)
    
    def _get_css_styles(self, footer_date: str) -> str:
        """Return CSS styles for professional document formatting"""
        return f'''
    @page {{
        size: A4;
        margin: 2.5cm 2cm 3.5cm 2cm;

        @bottom-left {{
            content: "Generated by ProfileDash {__version__} on {footer_date}";
            font-family: 'Georgia', 'Times New Roman', serif;
            font-size: 7px;
            color: #2d5a87;
        }}

        @bottom-right {{
            content: "Page " counter(page) " of " counter(pages);
            font-family: 'Georgia', 'Times New Roman', serif;
            font-size: 7px;
            color: #2d5a87;
        }}
    }}

    body {{
        font-family: 'Georgia', 'Times New Roman', serif;
        font-size: 12px;
        line-height: 1.5;
        margin: 0;
        padding: 0;
        color: #333;
    }}

    .cover-page {{
        height: 100%;
        display: flex;
        flex-direction: column;
        justify-content: flex-start;
        align-items: center;
        text-align: center;
        padding: 40px 30px 30px 30px;
        page-break-after: always;
    }}

    .company-name {{
        font-size: 28px;
        font-weight: bold;
        color: #1a365d;
        margin-top: 50px;
        margin-bottom: 20px;
        letter-spacing: 0.5px;
        line-height: 1.2;
    }}

    .product-name {{
        font-size: 18px;
        font-weight: bold;
        color: #2d5a87;
        margin-bottom: 25px;
    }}

    .generation-info {{
        font-size: 11px;
        color: #666;
        margin-bottom: 30px;
        line-height: 1.4;
    }}

    .toc-section {{
        text-align: left;
        max-width: 600px;
        width: 100%;
        flex-grow: 1;
    }}

    .toc-section h2 {{
        font-size: 16px;
        color: #1a365d;
        border-bottom: 2px solid #2d5a87;
        padding-bottom: 6px;
        margin-bottom: 16px;
    }}

    .toc-group {{
        font-size: 13px;
        color: #2d5a87;
        margin: 12px 0 6px 0;
    }}

    .toc-item {{
        margin: 4px 0 4px 16px;
        font-size: 11px;
    }}

    .toc-item a {{
        color: #333;
        text-decoration: none;
        border-bottom: 1px dotted #666;
    }}

    .toc-item a:hover {{
        color: #2d5a87;
        border-bottom: 1px solid #2d5a87;
    }}

    .page-break {{
        page-break-before: always;
    }}

    .profile-content {{
        padding: 20px 0;
        max-width: 100%;
        margin: 0;
    }}

    .section {{
        margin-bottom: 40px;
    }}

    .section-content {{
        font-size: 1em;
        line-height: 1.7;
    }}

    .data-table, table {{
        width: 100%;
        border-collapse: collapse;
        margin: 16px 0;
        font-size: 10px;
    }}

    .data-table td, table td, table th {{
        border: 1px solid #ddd;
        padding: 8px 12px;
        text-align: left;
    }}

    .data-table tr:first-child td, table thead th, table tr:first-child td {{
        background-color: #f8f9fa;
        font-weight: bold;
        color: #2d5a87;
    }}

    .data-table tr:nth-child(even), table tr:nth-child(even) {{
        background-color: #f8f9fa;
    }}

    strong {{
        color: #2d5a87;
        font-weight: bold;
    }}

    p {{
        margin: 10px 0;
        font-size: 1em;
        line-height: 1.6;
    }}



    h1 {{
        color: #1a365d;
        font-size: 18px;
        margin-top: 20px;
        margin-bottom: 12px;
        border-bottom: 2px solid #2d5a87;
        padding-bottom: 6px;
    }}

    h2 {{
        color: #2d5a87;
        font-size: 15px;
        margin-top: 16px;
        margin-bottom: 10px;
    }}

    h3 {{
        color: #4a5568;
        font-size: 13px;
        margin-top: 14px;
        margin-bottom: 8px;
    }}

    @media print {{
        .page-break {{
            page-break-before: always;
        }}

        .cover-page {{
            page-break-after: always;
        }}
    }}
    ''' 
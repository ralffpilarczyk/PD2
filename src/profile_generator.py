import os
from typing import Dict, List
from datetime import datetime
from pathlib import Path
import google.generativeai as genai


class ProfileGenerator:
    """Handles generation of professional HTML profiles"""
    
    def __init__(self, run_timestamp: str):
        """Initialize profile generator"""
        self.run_timestamp = run_timestamp
        # Use medium temperature model for company name extraction
        self.model = genai.GenerativeModel(
            'gemini-2.5-flash',
            generation_config=genai.types.GenerationConfig(temperature=0.6)
        )
    
    def generate_html_profile(self, results: Dict, section_numbers: List[int], full_context: str, sections: List[Dict]):
        """Generate complete HTML profile document"""
        
        # Extract company name
        company_name = self._extract_company_name(full_context)
        print(f"Generating profile for: {company_name}")
        
        # Get current date
        generation_date = datetime.now().strftime('%B %d, %Y')
        
        # Generate table of contents
        toc_html = self._generate_section_toc(section_numbers, sections)
        
        # Generate section content
        content_html = ""
        for section_num in sorted(section_numbers):
            if section_num in results:
                section_info = next(s for s in sections if s['number'] == section_num)
                content_html += f'''
<div class="section" id="section_{section_num}">
    <h1>Section {section_num}: {section_info["title"]}</h1>
    <div class="section-content">
        {self._markdown_to_html(results[section_num])}
    </div>
</div>
<div class="page-break"></div>
'''
        
        # Complete HTML document
        html_content = f'''<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{company_name} - ProfileDash 2.0</title>
    <style>
        {self._get_css_styles()}
    </style>
</head>
<body>
    <!-- Cover Page -->
    <div class="cover-page">
        <div class="company-name">{company_name}</div>
        <div class="product-name">ProfileDash 2.0</div>
        <div class="generation-info">
            Profile generated via Gemini 2.5 Flash on {generation_date}<br>
            Under MIT License
        </div>
        
        <div class="toc-section">
            <h2>Table of Contents</h2>
            {toc_html}
        </div>
    </div>
    
    <!-- Page Break before content -->
    <div class="page-break"></div>
    
    <!-- Profile Content -->
    <div class="profile-content">
        {content_html}
    </div>
</body>
</html>'''
        
        # Save HTML file
        html_filename = f"{company_name.replace(' ', '_').replace('.', '').replace(',', '')}_Profile.html"
        html_path = f"runs/run_{self.run_timestamp}/{html_filename}"
        
        with open(html_path, 'w', encoding='utf-8') as f:
            f.write(html_content)
        
        print(f"✅ HTML profile saved: {html_path}")
        return html_path
    
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
            
            response = self.model.generate_content(prompt)
            company_name = response.text.strip()
            
            # Basic validation
            if len(company_name) > 100 or len(company_name) < 2:
                return "Company Profile"
            
            return company_name
            
        except Exception as e:
            print(f"Company name extraction failed: {e}")
            return "Company Profile"
    
    def _generate_section_toc(self, section_numbers: List[int], sections: List[Dict]) -> str:
        """Generate table of contents for selected sections"""
        toc_html = ""
        
        # Group sections by category
        groups = {
            "Company Profile": [s for s in section_numbers if 1 <= s <= 14],
            "SWOT Analysis": [s for s in section_numbers if 15 <= s <= 18], 
            "Sellside Positioning": [s for s in section_numbers if 19 <= s <= 25],
            "Buyside Due Diligence": [s for s in section_numbers if 26 <= s <= 31],
            "Data Book": [s for s in section_numbers if s == 32]
        }
        
        for group_name, group_sections in groups.items():
            if group_sections:
                toc_html += f'<div class="toc-group"><strong>{group_name}</strong></div>\n'
                for section_num in sorted(group_sections):
                    section_info = next(s for s in sections if s['number'] == section_num)
                    toc_html += f'<div class="toc-item"><a href="#section_{section_num}">Section {section_num}: {section_info["title"]}</a></div>\n'
                toc_html += "<br>\n"
        
        return toc_html
    
    def _markdown_to_html(self, markdown_content: str) -> str:
        """Convert markdown content to HTML"""
        # Basic markdown to HTML conversion
        html = markdown_content
        
        # Headers
        html = html.replace('### ', '<h3>').replace('\n# ', '</h3>\n<h1>').replace('\n## ', '</h1>\n<h2>').replace('\n### ', '</h2>\n<h3>')
        
        # Add closing tags for headers at end of lines
        lines = html.split('\n')
        processed_lines = []
        
        for line in lines:
            if line.startswith('<h1>') and not line.endswith('</h1>'):
                line = line + '</h1>'
            elif line.startswith('<h2>') and not line.endswith('</h2>'):
                line = line + '</h2>'
            elif line.startswith('<h3>') and not line.endswith('</h3>'):
                line = line + '</h3>'
            
            # Bold text
            if '**' in line:
                line = line.replace('**', '<strong>', 1).replace('**', '</strong>', 1)
            
            # Tables - basic support
            if '|' in line and line.count('|') >= 2:
                if '---' in line:
                    line = ''  # Remove separator line
                else:
                    cells = [cell.strip() for cell in line.split('|')[1:-1]]
                    if processed_lines and '<table>' not in processed_lines[-1]:
                        processed_lines.append('<table class="data-table">')
                    line = '<tr>' + ''.join(f'<td>{cell}</td>' for cell in cells) + '</tr>'
            elif processed_lines and '<table>' in processed_lines[-1] and '|' not in line:
                processed_lines.append('</table>')
            
            processed_lines.append(line)
        
        # Close any open table
        if '<table>' in processed_lines[-1]:
            processed_lines.append('</table>')
        
        return '<br>'.join(processed_lines)
    
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
            height: 100vh;
            display: flex;
            flex-direction: column;
            justify-content: center;
            align-items: center;
            text-align: center;
            padding: 40px;
            page-break-after: always;
        }
        
        .company-name {
            font-size: 3.5em;
            font-weight: bold;
            color: #1a365d;
            margin-bottom: 30px;
            letter-spacing: 2px;
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
        
        .section h1 {
            color: #1a365d;
            font-size: 1.8em;
            border-bottom: 3px solid #2d5a87;
            padding-bottom: 10px;
            margin-bottom: 25px;
        }
        
        .section-content {
            font-size: 1em;
            line-height: 1.7;
        }
        
        .section-content h2 {
            color: #2d5a87;
            font-size: 1.3em;
            margin-top: 25px;
            margin-bottom: 15px;
        }
        
        .section-content h3 {
            color: #4a5568;
            font-size: 1.1em;
            margin-top: 20px;
            margin-bottom: 10px;
        }
        
        .data-table {
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
            font-size: 0.9em;
        }
        
        .data-table td {
            border: 1px solid #ddd;
            padding: 8px 12px;
            text-align: left;
        }
        
        .data-table tr:first-child td {
            background-color: #f8f9fa;
            font-weight: bold;
            color: #2d5a87;
        }
        
        .data-table tr:nth-child(even) {
            background-color: #f8f9fa;
        }
        
        strong {
            color: #2d5a87;
        }
        
        @media print {
            .page-break {
                page-break-before: always;
            }
            
            .cover-page {
                page-break-after: always;
            }
        }
        ''' 
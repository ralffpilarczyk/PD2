"""
Report Generator - Generate comprehensive reports with reasoning chains and footnotes
"""

from datetime import datetime
from pathlib import Path
from typing import List, Dict, Any, Optional
import json

class ReportGenerator:
    """Generate HTML and Markdown reports with full audit trails"""
    
    def __init__(self):
        self.footnote_counter = 0
        self.footnotes = []
    
    def generate_html_report(self, data: Dict, output_path: Path) -> None:
        """Generate HTML report with reasoning chains and footnotes"""
        
        self.footnote_counter = 0
        self.footnotes = []
        
        html = self._generate_html_structure(data)
        
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(html)
    
    def _generate_html_structure(self, data: Dict) -> str:
        """Generate complete HTML structure"""
        
        companies = ", ".join(data.get('companies', ['Unknown']))
        timestamp = datetime.fromisoformat(data['timestamp']).strftime("%B %d, %Y at %I:%M %p")
        
        html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>DDAR Analysis Report - {companies}</title>
    <style>
        {self._get_css_styles()}
    </style>
</head>
<body>
    <div class="container">
        <header>
            <h1>DDAR Analysis Report</h1>
            <div class="header-info">
                <p><strong>Company:</strong> {companies}</p>
                <p><strong>Generated:</strong> {timestamp}</p>
                <p><strong>Files Analyzed:</strong> {len(data.get('files_analyzed', []))}</p>
            </div>
        </header>
        
        {self._generate_executive_summary(data)}
        {self._generate_data_availability_section(data)}
        {self._generate_findings_section(data)}
        {self._generate_reasoning_chains_section(data)}
        {self._generate_footnotes_section()}
        {self._generate_appendix(data)}
    </div>
    
    <script>
        {self._get_javascript()}
    </script>
</body>
</html>"""
        
        return html
    
    def _get_css_styles(self) -> str:
        """Get CSS styles for the report"""
        
        return """
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
            line-height: 1.6;
            color: #333;
            background: #f5f5f5;
            margin: 0;
            padding: 0;
        }
        
        .container {
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
            background: white;
            box-shadow: 0 0 20px rgba(0,0,0,0.1);
        }
        
        header {
            border-bottom: 3px solid #2c3e50;
            padding-bottom: 20px;
            margin-bottom: 30px;
        }
        
        h1 {
            color: #2c3e50;
            margin: 0 0 10px 0;
        }
        
        h2 {
            color: #34495e;
            border-bottom: 2px solid #ecf0f1;
            padding-bottom: 10px;
            margin-top: 30px;
        }
        
        h3 {
            color: #555;
            margin-top: 20px;
        }
        
        .header-info {
            display: flex;
            gap: 30px;
            color: #666;
        }
        
        .summary-box {
            background: #ecf0f1;
            padding: 20px;
            border-radius: 8px;
            margin: 20px 0;
        }
        
        .metrics-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 15px;
            margin: 20px 0;
        }
        
        .metric-card {
            background: #fff;
            border: 1px solid #ddd;
            padding: 15px;
            border-radius: 5px;
        }
        
        .metric-value {
            font-size: 24px;
            font-weight: bold;
            color: #2c3e50;
        }
        
        .metric-label {
            color: #666;
            font-size: 14px;
        }
        
        .finding {
            background: #f9f9f9;
            border-left: 4px solid #3498db;
            padding: 15px;
            margin: 15px 0;
        }
        
        .finding-header {
            font-weight: bold;
            color: #2c3e50;
            margin-bottom: 10px;
        }
        
        .confidence {
            display: inline-block;
            padding: 2px 8px;
            border-radius: 3px;
            font-size: 12px;
            font-weight: bold;
        }
        
        .confidence-high {
            background: #27ae60;
            color: white;
        }
        
        .confidence-medium {
            background: #f39c12;
            color: white;
        }
        
        .confidence-low {
            background: #e74c3c;
            color: white;
        }
        
        .reasoning-chain {
            background: #fff;
            border: 1px solid #ddd;
            padding: 15px;
            margin: 15px 0;
            border-radius: 5px;
        }
        
        .chain-step {
            margin-left: 20px;
            padding: 5px 0;
            border-left: 2px solid #3498db;
            padding-left: 15px;
        }
        
        .footnote-ref {
            color: #3498db;
            cursor: pointer;
            font-size: 0.8em;
            vertical-align: super;
        }
        
        .footnote {
            font-size: 0.9em;
            color: #666;
            margin: 5px 0;
            padding-left: 20px;
        }
        
        .data-table {
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
        }
        
        .data-table th, .data-table td {
            padding: 10px;
            text-align: left;
            border-bottom: 1px solid #ddd;
        }
        
        .data-table th {
            background: #f5f5f5;
            font-weight: bold;
        }
        
        .available { color: #27ae60; }
        .missing { color: #e74c3c; }
        
        .collapsible {
            background-color: #f1f1f1;
            color: #444;
            cursor: pointer;
            padding: 18px;
            width: 100%;
            border: none;
            text-align: left;
            outline: none;
            font-size: 15px;
            margin-top: 10px;
        }
        
        .active, .collapsible:hover {
            background-color: #ddd;
        }
        
        .content {
            padding: 0 18px;
            display: none;
            overflow: hidden;
            background-color: #f9f9f9;
        }
        """
    
    def _get_javascript(self) -> str:
        """Get JavaScript for interactive features"""
        
        return """
        // Collapsible sections
        var coll = document.getElementsByClassName("collapsible");
        for (var i = 0; i < coll.length; i++) {
            coll[i].addEventListener("click", function() {
                this.classList.toggle("active");
                var content = this.nextElementSibling;
                if (content.style.display === "block") {
                    content.style.display = "none";
                } else {
                    content.style.display = "block";
                }
            });
        }
        
        // Footnote tooltips
        var footnoteRefs = document.getElementsByClassName("footnote-ref");
        for (var i = 0; i < footnoteRefs.length; i++) {
            footnoteRefs[i].addEventListener("click", function() {
                var footnoteId = this.getAttribute("data-footnote");
                var footnote = document.getElementById(footnoteId);
                if (footnote) {
                    footnote.scrollIntoView({ behavior: 'smooth' });
                    footnote.style.backgroundColor = '#ffffcc';
                    setTimeout(function() {
                        footnote.style.backgroundColor = '';
                    }, 2000);
                }
            });
        }
        """
    
    def _generate_executive_summary(self, data: Dict) -> str:
        """Generate executive summary section"""
        
        total_facts = data['metadata']['total_facts']
        total_conclusions = data['metadata']['total_conclusions']
        theorems_applied = data['metadata']['theorems_applied']
        theorems_skipped = data['metadata']['theorems_skipped']
        
        html = """
        <section id="executive-summary">
            <h2>Executive Summary</h2>
            <div class="summary-box">
                <div class="metrics-grid">
                    <div class="metric-card">
                        <div class="metric-value">{}</div>
                        <div class="metric-label">Facts Extracted</div>
                    </div>
                    <div class="metric-card">
                        <div class="metric-value">{}</div>
                        <div class="metric-label">Conclusions Generated</div>
                    </div>
                    <div class="metric-card">
                        <div class="metric-value">{}</div>
                        <div class="metric-label">Theorems Applied</div>
                    </div>
                    <div class="metric-card">
                        <div class="metric-value">{}</div>
                        <div class="metric-label">Theorems Skipped</div>
                    </div>
                </div>
            </div>
        """.format(total_facts, total_conclusions, theorems_applied, theorems_skipped)
        
        # Key findings summary
        if data.get('conclusions'):
            html += "<h3>Key Findings</h3><ul>"
            
            # Group conclusions by confidence
            high_conf = [c for c in data['conclusions'] if c.get('confidence', 0) >= 0.9]
            medium_conf = [c for c in data['conclusions'] if 0.7 <= c.get('confidence', 0) < 0.9]
            
            for conclusion in high_conf[:3]:  # Top 3 high confidence
                html += f"<li>{conclusion.get('recommendation', 'Unknown')} "
                html += f"<span class='confidence confidence-high'>High Confidence</span></li>"
            
            for conclusion in medium_conf[:2]:  # Top 2 medium confidence
                html += f"<li>{conclusion.get('recommendation', 'Unknown')} "
                html += f"<span class='confidence confidence-medium'>Medium Confidence</span></li>"
            
            html += "</ul>"
        
        html += "</section>"
        return html
    
    def _generate_data_availability_section(self, data: Dict) -> str:
        """Generate data availability section"""
        
        availability = data.get('data_availability', {})
        
        html = """
        <section id="data-availability">
            <h2>Data Availability</h2>
        """
        
        # Coverage percentage
        coverage = availability.get('coverage_percentage', 0)
        html += f"""
        <div class="summary-box">
            <p><strong>Theorem Coverage:</strong> {availability.get('can_run', 0)} of {availability.get('total', 0)} theorems can run ({coverage:.1f}%)</p>
        </div>
        """
        
        # Available metrics
        if availability.get('available_metrics'):
            html += "<h3>Available Metrics by Company</h3>"
            html += "<table class='data-table'>"
            html += "<tr><th>Company</th><th>Metric</th><th>Periods Available</th></tr>"
            
            for company, metrics in availability['available_metrics'].items():
                for metric, periods in sorted(metrics.items())[:10]:
                    periods_str = ", ".join(sorted(periods)[:3])
                    if len(periods) > 3:
                        periods_str += f" (+{len(periods)-3} more)"
                    html += f"<tr><td>{company}</td><td>{metric}</td><td>{periods_str}</td></tr>"
            
            html += "</table>"
        
        # Missing data for key theorems
        if availability.get('missing_data'):
            html += "<h3>Theorems with Missing Data</h3>"
            html += "<ul>"
            
            for theorem, missing in list(availability['missing_data'].items())[:5]:
                missing_str = ", ".join(missing)
                html += f"<li><strong>{theorem}:</strong> needs <span class='missing'>{missing_str}</span></li>"
            
            html += "</ul>"
        
        html += "</section>"
        return html
    
    def _generate_findings_section(self, data: Dict) -> str:
        """Generate findings section with conclusions"""
        
        html = """
        <section id="findings">
            <h2>Analysis Findings</h2>
        """
        
        conclusions = data.get('conclusions', [])
        
        if not conclusions:
            html += "<p>No conclusions could be generated with available data.</p>"
        else:
            # Group by theorem type
            by_theorem = {}
            for c in conclusions:
                theorem = c.get('theorem', 'unknown')
                if theorem not in by_theorem:
                    by_theorem[theorem] = []
                by_theorem[theorem].append(c)
            
            for theorem, items in sorted(by_theorem.items()):
                html += f"<h3>{theorem.replace('_', ' ').title()}</h3>"
                
                for item in items:
                    confidence = item.get('confidence', 0)
                    conf_class = 'high' if confidence >= 0.9 else 'medium' if confidence >= 0.7 else 'low'
                    
                    footnote_id = self._add_footnote(item)
                    
                    html += f"""
                    <div class="finding">
                        <div class="finding-header">
                            {item.get('recommendation', 'Unknown')}
                            <span class="confidence confidence-{conf_class}">{confidence:.0%}</span>
                            <span class="footnote-ref" data-footnote="footnote-{footnote_id}">[{footnote_id}]</span>
                        </div>
                        <div>Reasoning: {item.get('reasoning', 'Not provided')}</div>
                        <div>Evidence: {', '.join(item.get('support_fact_ids', []))}</div>
                    </div>
                    """
        
        html += "</section>"
        return html
    
    def _generate_reasoning_chains_section(self, data: Dict) -> str:
        """Generate reasoning chains section"""
        
        html = """
        <section id="reasoning-chains">
            <h2>Reasoning Chains</h2>
            <p>Detailed traceability from facts to conclusions:</p>
        """
        
        # Show reasoning for top conclusions
        conclusions = data.get('conclusions', [])[:5]
        
        for i, conclusion in enumerate(conclusions, 1):
            html += f"""
            <button class="collapsible">Chain {i}: {conclusion.get('theorem', 'unknown')}</button>
            <div class="content">
                <div class="reasoning-chain">
            """
            
            # Build reasoning chain
            html += f"<div class='chain-step'><strong>Conclusion:</strong> {conclusion.get('recommendation')}</div>"
            html += f"<div class='chain-step'><strong>Theorem Applied:</strong> {conclusion.get('theorem')}</div>"
            html += f"<div class='chain-step'><strong>Reasoning:</strong> {conclusion.get('reasoning')}</div>"
            
            # Show supporting facts
            support_ids = conclusion.get('support_fact_ids', [])
            if support_ids:
                html += "<div class='chain-step'><strong>Supporting Facts:</strong><ul>"
                
                # Find the actual facts
                for fact_id in support_ids:
                    fact = self._find_fact_by_id(data.get('facts', []), fact_id)
                    if fact:
                        html += f"<li>{fact['key']}: {fact.get('value', 'N/A')} "
                        html += f"({fact.get('period_label', 'Unknown period')}) "
                        html += f"[Source: {fact.get('source_doc', 'Unknown')}, "
                        html += f"line {fact.get('source_page', 'Unknown')}]</li>"
                
                html += "</ul></div>"
            
            html += """
                </div>
            </div>
            """
        
        html += "</section>"
        return html
    
    def _generate_footnotes_section(self) -> str:
        """Generate footnotes section"""
        
        if not self.footnotes:
            return ""
        
        html = """
        <section id="footnotes">
            <h2>Footnotes</h2>
            <div class="footnotes-list">
        """
        
        for footnote in self.footnotes:
            html += f"""
            <div class="footnote" id="footnote-{footnote['id']}">
                [{footnote['id']}] {footnote['text']}
            </div>
            """
        
        html += """
            </div>
        </section>
        """
        
        return html
    
    def _generate_appendix(self, data: Dict) -> str:
        """Generate appendix with raw data"""
        
        html = """
        <section id="appendix">
            <h2>Appendix</h2>
            
            <button class="collapsible">Raw Facts Data</button>
            <div class="content">
                <p>All extracted facts in tabular format:</p>
                <table class="data-table">
                    <tr>
                        <th>Company</th>
                        <th>Metric</th>
                        <th>Value</th>
                        <th>Period</th>
                        <th>Source</th>
                        <th>Confidence</th>
                    </tr>
        """
        
        # Show sample of facts
        for fact in data.get('facts', [])[:20]:
            company = fact.get('company', 'Unknown')
            key = fact.get('key', 'Unknown')
            value = fact.get('value', 'N/A')
            if isinstance(value, (int, float)):
                value = f"{value:.4f}"
            period = fact.get('period_label', 'Unknown')
            source = fact.get('source_doc', 'Unknown')
            confidence = fact.get('confidence', 0)
            
            html += f"""
            <tr>
                <td>{company}</td>
                <td>{key}</td>
                <td>{value}</td>
                <td>{period}</td>
                <td>{source}</td>
                <td>{confidence:.0%}</td>
            </tr>
            """
        
        if len(data.get('facts', [])) > 20:
            html += f"""
            <tr>
                <td colspan="6" style="text-align: center;">
                    ... and {len(data.get('facts', [])) - 20} more facts
                </td>
            </tr>
            """
        
        html += """
                </table>
            </div>
        </section>
        """
        
        return html
    
    def _add_footnote(self, conclusion: Dict) -> int:
        """Add a footnote for a conclusion"""
        
        self.footnote_counter += 1
        
        # Build footnote text
        text = f"Theorem: {conclusion.get('theorem', 'unknown')}. "
        text += f"Confidence: {conclusion.get('confidence', 0):.0%}. "
        text += f"Based on {len(conclusion.get('support_fact_ids', []))} supporting facts. "
        text += f"Generated using DDAR reasoning engine."
        
        self.footnotes.append({
            'id': self.footnote_counter,
            'text': text
        })
        
        return self.footnote_counter
    
    def _find_fact_by_id(self, facts: List[Dict], fact_id: str) -> Optional[Dict]:
        """Find a fact by its ID"""
        
        for fact in facts:
            if fact.get('fact_id') == fact_id:
                return fact
        return None


# Test the report generator
if __name__ == "__main__":
    generator = ReportGenerator()
    
    # Test with sample data
    test_data = {
        'timestamp': datetime.now().isoformat(),
        'companies': ['TestCo'],
        'files_analyzed': ['test1.md', 'test2.md'],
        'facts': [
            {'company': 'TestCo', 'key': 'revenue', 'value': 1000000, 'period_label': 'Q1 2024',
             'source_doc': 'test1.md', 'source_page': 10, 'confidence': 0.95, 'fact_id': 'test1'},
        ],
        'conclusions': [
            {
                'theorem': 'value_creation',
                'recommendation': 'Value creation detected',
                'reasoning': 'ROE exceeds WACC by 5%',
                'confidence': 0.85,
                'support_fact_ids': ['test1']
            }
        ],
        'data_availability': {
            'total': 10,
            'can_run': 4,
            'coverage_percentage': 40,
            'available_metrics': {'TestCo': {'revenue': {'Q1 2024'}, 'ebitda': {'Q1 2024'}}},
            'missing_data': {'value_creation': ['wacc']}
        },
        'metadata': {
            'total_facts': 10,
            'total_conclusions': 1,
            'theorems_applied': 4,
            'theorems_skipped': 6
        }
    }
    
    output_path = Path('test_report.html')
    generator.generate_html_report(test_data, output_path)
    print(f"Test report generated: {output_path}")
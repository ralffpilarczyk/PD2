"""
Report Generator for Competitive Analysis.
Creates comprehensive HTML reports with PD2 styling and professional formatting.
"""

import json
import os
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Any
from ..utils import thread_safe_print
from .database import CompetitiveDatabase
import markdown
from markdown.extensions import tables


class CompetitiveReportGenerator:
    """Generates comprehensive HTML reports for competitive analysis"""
    
    def __init__(self, db: CompetitiveDatabase, output_dir: str):
        """Initialize with database and output directory"""
        self.db = db
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Initialize markdown processor with extensions
        self.md_processor = markdown.Markdown(
            extensions=['tables', 'toc', 'codehilite', 'fenced_code'],
            extension_configs={
                'toc': {'title': 'Table of Contents'},
                'codehilite': {'css_class': 'highlight'}
            }
        )
    
    def generate_comprehensive_report(self, company_id: int, 
                                    competitive_analyses: Dict[int, Dict[str, Any]],
                                    strategy_bundles: Dict[int, List[Dict[str, Any]]] = None) -> str:
        """
        Generate comprehensive competitive intelligence report.
        Returns path to generated HTML file.
        """
        thread_safe_print(f"📄 Generating comprehensive competitive analysis report...")
        
        # Get company information
        company_info = self._get_company_info(company_id)
        if not company_info:
            thread_safe_print("⚠ Company information not found")
            return ""
        
        # Generate report sections
        report_content = self._build_report_content(
            company_info=company_info,
            competitive_analyses=competitive_analyses,
            strategy_bundles=strategy_bundles or {}
        )
        
        # Convert to HTML with styling
        html_content = self._convert_to_html(report_content, company_info)
        
        # Save to file
        company_name = company_info.get('name', 'Unknown').replace(' ', '_').replace(',', '').replace('.', '')
        filename = f"{company_name}_competitive_analysis.html"
        html_path = self.output_dir / filename
        
        with open(html_path, 'w', encoding='utf-8') as f:
            f.write(html_content)
        
        thread_safe_print(f"✅ Report generated: {html_path}")
        return str(html_path)
    
    def _get_company_info(self, company_id: int) -> Optional[Dict[str, Any]]:
        """Get company information from database"""
        with self.db.get_connection() as conn:
            cursor = conn.execute("SELECT * FROM companies WHERE id = ?", (company_id,))
            row = cursor.fetchone()
            
            if row:
                company_info = dict(row)
                if company_info.get('context_json'):
                    company_info['context'] = json.loads(company_info['context_json'])
                return company_info
        
        return None
    
    def _build_report_content(self, company_info: Dict[str, Any],
                            competitive_analyses: Dict[int, Dict[str, Any]],
                            strategy_bundles: Dict[int, List[Dict[str, Any]]]) -> str:
        """Build the complete report content in markdown format"""
        
        context = company_info.get('context', {})
        company_name = company_info.get('name', 'Unknown Company')
        
        # Start building the report
        content = []
        
        # Title and Executive Summary
        content.append(f"# Competitive Intelligence Report")
        content.append(f"## {company_name}")
        content.append(f"*Generated on {datetime.now().strftime('%B %d, %Y')}*")
        content.append("\n---\n")
        
        # Executive Summary
        content.append("## Executive Summary")
        content.append(self._generate_executive_summary(company_info, competitive_analyses, strategy_bundles))
        
        # Company Overview
        content.append("## Company Overview")
        content.append(self._generate_company_overview(company_info))
        
        # Market Analysis
        content.append("## Market Analysis")
        content.append(self._generate_market_analysis(competitive_analyses))
        
        # Competitive Positioning
        content.append("## Competitive Positioning")
        content.append(self._generate_competitive_positioning(competitive_analyses))
        
        # Strategic Recommendations
        if strategy_bundles:
            content.append("## Strategic Recommendations")
            content.append(self._generate_strategic_recommendations(strategy_bundles, competitive_analyses))
        
        # Data Appendix
        content.append("## Data Appendix")
        content.append(self._generate_data_appendix(competitive_analyses))
        
        # Methodology and Sources
        content.append("## Methodology and Sources")
        content.append(self._generate_methodology_section(competitive_analyses))
        
        return "\n\n".join(content)
    
    def _generate_executive_summary(self, company_info: Dict[str, Any],
                                  competitive_analyses: Dict[int, Dict[str, Any]],
                                  strategy_bundles: Dict[int, List[Dict[str, Any]]]) -> str:
        """Generate executive summary section"""
        
        context = company_info.get('context', {})
        company_name = company_info.get('name', 'Unknown')
        
        summary_parts = []
        
        # Company context
        industry = context.get('industry', 'Unknown')
        business_model = context.get('business_model', 'Unknown')
        geography = context.get('geography', 'Unknown')
        
        summary_parts.append(f"**{company_name}** operates in the {industry} industry with a {business_model} business model, serving {geography} markets.")
        
        # Competitive analysis summary
        total_markets = len(competitive_analyses)
        successful_analyses = sum(1 for analysis in competitive_analyses.values() 
                                if analysis.get('competitive_analysis', {}).get('success'))
        
        summary_parts.append(f"This analysis covers **{total_markets} market cells** with comprehensive competitive intelligence for {successful_analyses} segments.")
        
        # Overall competitive position
        overall_positions = []
        key_strengths = []
        key_weaknesses = []
        
        for market_data in competitive_analyses.values():
            comp_analysis = market_data.get('competitive_analysis', {})
            if comp_analysis.get('success'):
                position = comp_analysis.get('overall_position', {}).get('position', '')
                if position:
                    overall_positions.append(position)
                
                strengths = comp_analysis.get('top_strengths', [])
                weaknesses = comp_analysis.get('top_weaknesses', [])
                
                key_strengths.extend([s.get('metric', '') for s in strengths[:2]])
                key_weaknesses.extend([w.get('metric', '') for w in weaknesses[:2]])
        
        # Most common position
        if overall_positions:
            most_common_position = max(set(overall_positions), key=overall_positions.count)
            summary_parts.append(f"**Competitive Position**: {most_common_position} across analyzed market segments.")
        
        # Key insights
        if key_strengths:
            unique_strengths = list(set(key_strengths))[:3]
            summary_parts.append(f"**Key Competitive Strengths**: {', '.join(unique_strengths)}")
        
        if key_weaknesses:
            unique_weaknesses = list(set(key_weaknesses))[:3]
            summary_parts.append(f"**Key Areas for Improvement**: {', '.join(unique_weaknesses)}")
        
        # Strategic recommendations summary
        if strategy_bundles:
            total_bundles = sum(len(bundles) for bundles in strategy_bundles.values())
            summary_parts.append(f"**Strategic Recommendations**: {total_bundles} strategic bundles proposed across market segments, focusing on competitive gap closure and advantage amplification.")
        
        return "\n\n".join(summary_parts)
    
    def _generate_company_overview(self, company_info: Dict[str, Any]) -> str:
        """Generate company overview section"""
        
        context = company_info.get('context', {})
        
        overview_parts = []
        
        # Basic company information
        overview_table = []
        overview_table.append("| Attribute | Details |")
        overview_table.append("|-----------|---------|")
        overview_table.append(f"| Company Name | {company_info.get('name', 'Unknown')} |")
        
        if context.get('ticker'):
            overview_table.append(f"| Ticker | {context['ticker']} |")
        
        overview_table.append(f"| Industry | {context.get('industry', 'Unknown')} |")
        overview_table.append(f"| Business Model | {context.get('business_model', 'Unknown')} |")
        overview_table.append(f"| Headquarters | {context.get('headquarters', 'Unknown')} |")
        overview_table.append(f"| Stage | {context.get('stage', 'Unknown')} |")
        
        overview_parts.append("\n".join(overview_table))
        
        # Business description
        if context.get('products_services'):
            overview_parts.append(f"**Products & Services**: {context['products_services']}")
        
        if context.get('target_customers'):
            overview_parts.append(f"**Target Customers**: {context['target_customers']}")
        
        if context.get('revenue_model'):
            overview_parts.append(f"**Revenue Model**: {context['revenue_model']}")
        
        # Geographic presence
        if context.get('primary_markets'):
            markets = context['primary_markets']
            if isinstance(markets, list):
                markets_str = ", ".join(markets)
            else:
                markets_str = str(markets)
            overview_parts.append(f"**Primary Markets**: {markets_str}")
        
        # Financial highlights
        if context.get('financial_highlights'):
            overview_parts.append(f"**Financial Highlights**: {context['financial_highlights']}")
        
        return "\n\n".join(overview_parts)
    
    def _generate_market_analysis(self, competitive_analyses: Dict[int, Dict[str, Any]]) -> str:
        """Generate market analysis section"""
        
        market_parts = []
        
        market_parts.append("This analysis examines competitive dynamics across the following market cells:")
        
        # Market cells table
        market_table = []
        market_table.append("| Market Cell | Product/Service | Geography | Customer Segment | Materiality | Competitive Position |")
        market_table.append("|-------------|-----------------|-----------|------------------|-------------|---------------------|")
        
        for market_id, market_data in competitive_analyses.items():
            market_info = market_data.get('market_cell_info', {})
            comp_analysis = market_data.get('competitive_analysis', {})
            
            product_service = market_info.get('product_service', 'Unknown')
            geography = market_info.get('geography', 'Unknown')
            customer_segment = market_info.get('customer_segment', 'Unknown')
            materiality = market_info.get('materiality_score', 0.0)
            
            position = 'Not Analyzed'
            if comp_analysis.get('success'):
                position = comp_analysis.get('overall_position', {}).get('position', 'Unknown')
            
            market_table.append(f"| Cell {market_id} | {product_service} | {geography} | {customer_segment} | {materiality:.2f} | {position} |")
        
        market_parts.append("\n".join(market_table))
        
        # Market insights
        market_parts.append("### Key Market Insights")
        
        successful_analyses = [data for data in competitive_analyses.values() 
                             if data.get('competitive_analysis', {}).get('success')]
        
        if successful_analyses:
            # Calculate average metrics
            total_metrics = sum(len(data['competitive_analysis'].get('scored_metrics', [])) 
                              for data in successful_analyses)
            avg_confidence = sum(data['competitive_analysis'].get('analysis_summary', {}).get('average_confidence', 0) 
                               for data in successful_analyses) / len(successful_analyses)
            
            market_parts.append(f"- **Total Metrics Analyzed**: {total_metrics}")
            market_parts.append(f"- **Average Data Confidence**: {avg_confidence:.2f}/1.0")
            market_parts.append(f"- **Market Cells with Strong Competitive Intelligence**: {len(successful_analyses)}/{len(competitive_analyses)}")
        
        return "\n\n".join(market_parts)
    
    def _generate_competitive_positioning(self, competitive_analyses: Dict[int, Dict[str, Any]]) -> str:
        """Generate competitive positioning section"""
        
        positioning_parts = []
        
        for market_id, market_data in competitive_analyses.items():
            market_info = market_data.get('market_cell_info', {})
            comp_analysis = market_data.get('competitive_analysis', {})
            
            if not comp_analysis.get('success'):
                continue
            
            market_name = f"{market_info.get('product_service', 'Unknown')} × {market_info.get('geography', 'Unknown')}"
            positioning_parts.append(f"### {market_name}")
            
            # Overall position
            overall_position = comp_analysis.get('overall_position', {})
            position = overall_position.get('position', 'Unknown')
            competitive_score = overall_position.get('competitive_score', 0.5)
            
            positioning_parts.append(f"**Overall Position**: {position} (Score: {competitive_score:.2f}/1.0)")
            
            # Top strengths
            strengths = comp_analysis.get('top_strengths', [])
            if strengths:
                positioning_parts.append("**Key Competitive Strengths**:")
                for i, strength in enumerate(strengths, 1):
                    metric = strength.get('metric', 'Unknown')
                    competitor = strength.get('competitor', 'Unknown')
                    diff = strength.get('differentiation', 0)
                    positioning_parts.append(f"{i}. {metric} - {diff:+.1%} vs {competitor}")
            
            # Top weaknesses
            weaknesses = comp_analysis.get('top_weaknesses', [])
            if weaknesses:
                positioning_parts.append("**Key Areas for Improvement**:")
                for i, weakness in enumerate(weaknesses, 1):
                    metric = weakness.get('metric', 'Unknown')
                    competitor = weakness.get('competitor', 'Unknown')
                    diff = weakness.get('differentiation', 0)
                    positioning_parts.append(f"{i}. {metric} - {diff:+.1%} vs {competitor}")
            
            # Metrics summary table
            scored_metrics = comp_analysis.get('scored_metrics', [])
            if scored_metrics:
                positioning_parts.append("**Detailed Metrics Analysis**:")
                
                metrics_table = []
                metrics_table.append("| Metric | Capability Family | Impact | Confidence | Addressability | Anchor Competitor |")
                metrics_table.append("|--------|------------------|--------|------------|----------------|------------------|")
                
                for metric in scored_metrics[:10]:  # Limit to top 10 metrics
                    name = metric.get('metric_name', 'Unknown')
                    family = metric.get('capability_family', 'Unknown')
                    impact = metric.get('impact', 0)
                    confidence = metric.get('confidence', 0)
                    addressability = metric.get('addressability', 0)
                    anchor = metric.get('anchor_competitor', 'Unknown')
                    
                    metrics_table.append(f"| {name} | {family} | {impact:.2f} | {confidence:.2f} | {addressability:.2f} | {anchor} |")
                
                positioning_parts.append("\n".join(metrics_table))
        
        return "\n\n".join(positioning_parts)
    
    def _generate_strategic_recommendations(self, strategy_bundles: Dict[int, List[Dict[str, Any]]],
                                          competitive_analyses: Dict[int, Dict[str, Any]]) -> str:
        """Generate strategic recommendations section"""
        
        strategy_parts = []
        
        for market_id, bundles in strategy_bundles.items():
            if not bundles:
                continue
            
            # Get market info
            market_data = competitive_analyses.get(market_id, {})
            market_info = market_data.get('market_cell_info', {})
            market_name = f"{market_info.get('product_service', 'Unknown')} × {market_info.get('geography', 'Unknown')}"
            
            strategy_parts.append(f"### {market_name}")
            
            for i, bundle in enumerate(bundles, 1):
                bundle_name = bundle.get('name', f'Strategy Bundle {i}')
                description = bundle.get('description', 'No description available')
                feasibility = bundle.get('feasibility_score', 0.5)
                timeline = bundle.get('estimated_timeline', 12)
                
                strategy_parts.append(f"#### {i}. {bundle_name}")
                strategy_parts.append(f"**Description**: {description}")
                strategy_parts.append(f"**Feasibility Score**: {feasibility:.2f}/1.0")
                strategy_parts.append(f"**Estimated Timeline**: {timeline} months")
                
                # Strategies in bundle
                strategies = bundle.get('strategies', [])
                if strategies:
                    strategy_parts.append("**Component Strategies**:")
                    for j, strategy in enumerate(strategies, 1):
                        strategy_name = strategy.get('name', f'Strategy {j}')
                        mechanism = strategy.get('mechanism', 'No mechanism specified')
                        strategy_parts.append(f"{j}. **{strategy_name}**: {mechanism}")
                
                # Resource requirements
                resource_summary = bundle.get('resource_summary', {})
                if resource_summary:
                    strategy_parts.append("**Resource Requirements**:")
                    for resource_type, requirement in resource_summary.items():
                        if requirement and requirement != f"Low {resource_type} requirements":
                            strategy_parts.append(f"- **{resource_type.upper()}**: {requirement}")
                
                # Risks
                risks = bundle.get('risk_assessment', [])
                if risks:
                    strategy_parts.append("**Key Risks**:")
                    for risk in risks[:3]:  # Top 3 risks
                        strategy_parts.append(f"- {risk}")
                
                # Conflicts
                conflicts = bundle.get('bundle_conflicts', [])
                if conflicts:
                    strategy_parts.append("**Potential Conflicts**:")
                    for conflict in conflicts:
                        severity = conflict.get('severity', 0)
                        description = conflict.get('description', 'Conflict detected')
                        strategy_parts.append(f"- {description} (Severity: {severity:.1f}/1.0)")
        
        return "\n\n".join(strategy_parts)
    
    def _generate_data_appendix(self, competitive_analyses: Dict[int, Dict[str, Any]]) -> str:
        """Generate data appendix section"""
        
        appendix_parts = []
        
        appendix_parts.append("This section provides detailed data tables and methodology notes for the competitive analysis.")
        
        # Data quality summary
        total_observations = 0
        high_confidence_obs = 0
        
        with self.db.get_connection() as conn:
            # Get data quality statistics
            cursor = conn.execute("""
                SELECT COUNT(*) as total, 
                       AVG(confidence_score) as avg_confidence,
                       COUNT(CASE WHEN confidence_score >= 0.7 THEN 1 END) as high_confidence
                FROM observations
            """)
            stats = cursor.fetchone()
            
            if stats:
                total_observations = stats['total']
                avg_confidence = stats['avg_confidence'] or 0
                high_confidence_obs = stats['high_confidence']
                
                appendix_parts.append("### Data Quality Summary")
                appendix_parts.append(f"- **Total Observations**: {total_observations}")
                appendix_parts.append(f"- **Average Confidence Score**: {avg_confidence:.2f}/1.0")
                appendix_parts.append(f"- **High Confidence Observations (≥0.7)**: {high_confidence_obs} ({high_confidence_obs/max(1,total_observations)*100:.1f}%)")
        
        # Comparability class breakdown (using existing column)
        with self.db.get_connection() as conn:
            cursor = conn.execute("""
                SELECT comparability_class, COUNT(*) as count
                FROM observations
                WHERE comparability_class IS NOT NULL
                GROUP BY comparability_class
                ORDER BY count DESC
            """)
            source_stats = cursor.fetchall()
            
            if source_stats:
                appendix_parts.append("### Data Comparability Breakdown")
                source_table = []
                source_table.append("| Comparability Class | Count | Percentage |")
                source_table.append("|---------------------|-------|------------|")
                
                total_sourced = sum(row['count'] for row in source_stats)
                for row in source_stats:
                    comp_class = row['comparability_class']
                    count = row['count']
                    percentage = (count / total_sourced) * 100
                    source_table.append(f"| {comp_class} | {count} | {percentage:.1f}% |")
                
                appendix_parts.append("\n".join(source_table))
        
        return "\n\n".join(appendix_parts)
    
    def _generate_methodology_section(self, competitive_analyses: Dict[int, Dict[str, Any]]) -> str:
        """Generate methodology and sources section"""
        
        methodology_parts = []
        
        methodology_parts.append("### Analysis Methodology")
        methodology_parts.append("This competitive intelligence report was generated using a systematic 4-dimension scoring framework:")
        
        methodology_parts.append("1. **Differentiation**: Quantitative performance gaps vs best-in-class competitors")
        methodology_parts.append("2. **Impact**: Business materiality based on value impact categories (margin, ROCE, growth, scalability)")
        methodology_parts.append("3. **Confidence**: Source quality assessment and data consistency analysis")
        methodology_parts.append("4. **Addressability**: 12-month actionability assessment based on capability family")
        
        methodology_parts.append("### Data Sources")
        methodology_parts.append("Data was collected using Google Search Grounding with the following source hierarchy:")
        methodology_parts.append("- **Primary Sources**: Earnings reports, regulatory filings, SEC submissions")
        methodology_parts.append("- **Secondary Sources**: Company disclosures, analyst reports, industry studies")
        methodology_parts.append("- **Tertiary Sources**: News articles, press releases, verified public statements")
        
        methodology_parts.append("### Data Normalization")
        methodology_parts.append("All financial data was normalized using:")
        methodology_parts.append("- **Currency Conversion**: European Central Bank daily exchange rates")
        methodology_parts.append("- **Period Alignment**: TTM preferred, with quarterly data annualized when necessary")
        methodology_parts.append("- **Scope Alignment**: Market cell specific data preferred, with fallback to segment/consolidated data")
        
        methodology_parts.append("### Limitations")
        methodology_parts.append("- Analysis based on publicly available information as of the report generation date")
        methodology_parts.append("- Data quality varies by competitor and metric availability")
        methodology_parts.append("- Strategic recommendations require validation against internal capabilities and constraints")
        
        return "\n\n".join(methodology_parts)
    
    def _convert_to_html(self, markdown_content: str, company_info: Dict[str, Any]) -> str:
        """Convert markdown content to HTML with PD2 styling"""
        
        # Convert markdown to HTML
        html_body = self.md_processor.convert(markdown_content)
        
        # Get company name for title
        company_name = company_info.get('name', 'Unknown Company')
        
        # Build complete HTML document with PD2-style CSS
        html_template = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Competitive Analysis - {company_name}</title>
    <style>
        {self._get_report_css()}
    </style>
</head>
<body>
    <div class="container">
        {html_body}
    </div>
</body>
</html>"""
        
        return html_template
    
    def _get_report_css(self) -> str:
        """Get CSS styling similar to PD2 reports"""
        return """
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', 'Helvetica Neue', Arial, sans-serif;
            line-height: 1.6;
            color: #333;
            max-width: none;
            margin: 0;
            padding: 0;
            background-color: #f8f9fa;
        }
        
        .container {
            max-width: 1200px;
            margin: 0 auto;
            padding: 40px;
            background-color: white;
            box-shadow: 0 0 10px rgba(0,0,0,0.1);
        }
        
        h1 {
            color: #2c3e50;
            border-bottom: 3px solid #3498db;
            padding-bottom: 10px;
            margin-bottom: 30px;
            font-size: 2.5em;
            font-weight: 700;
        }
        
        h2 {
            color: #34495e;
            border-bottom: 2px solid #ecf0f1;
            padding-bottom: 8px;
            margin-top: 40px;
            margin-bottom: 20px;
            font-size: 1.8em;
            font-weight: 600;
        }
        
        h3 {
            color: #2c3e50;
            margin-top: 30px;
            margin-bottom: 15px;
            font-size: 1.4em;
            font-weight: 600;
        }
        
        h4 {
            color: #34495e;
            margin-top: 25px;
            margin-bottom: 12px;
            font-size: 1.2em;
            font-weight: 600;
        }
        
        p {
            margin-bottom: 15px;
            text-align: justify;
        }
        
        table {
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
            box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        
        th {
            background-color: #3498db;
            color: white;
            padding: 12px 8px;
            text-align: left;
            font-weight: 600;
            border-bottom: 2px solid #2980b9;
        }
        
        td {
            padding: 10px 8px;
            border-bottom: 1px solid #ecf0f1;
        }
        
        tr:nth-child(even) {
            background-color: #f8f9fa;
        }
        
        tr:hover {
            background-color: #e8f4f8;
        }
        
        ul, ol {
            margin-bottom: 15px;
            padding-left: 25px;
        }
        
        li {
            margin-bottom: 8px;
        }
        
        strong {
            color: #2c3e50;
            font-weight: 600;
        }
        
        em {
            color: #7f8c8d;
        }
        
        blockquote {
            border-left: 4px solid #3498db;
            padding-left: 20px;
            margin: 20px 0;
            background-color: #f8f9fa;
            padding: 15px 20px;
            border-radius: 5px;
        }
        
        code {
            background-color: #f1f2f6;
            padding: 2px 6px;
            border-radius: 3px;
            font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
            font-size: 0.9em;
        }
        
        .toc {
            background-color: #f8f9fa;
            border: 1px solid #e9ecef;
            border-radius: 5px;
            padding: 20px;
            margin: 20px 0;
        }
        
        .toc ul {
            list-style-type: none;
            padding-left: 0;
        }
        
        .toc li {
            margin-bottom: 5px;
        }
        
        .toc a {
            text-decoration: none;
            color: #3498db;
        }
        
        .toc a:hover {
            text-decoration: underline;
        }
        
        hr {
            border: none;
            height: 2px;
            background-color: #ecf0f1;
            margin: 30px 0;
        }
        
        @media print {
            .container {
                box-shadow: none;
                max-width: none;
                padding: 20px;
            }
            
            h1, h2 {
                page-break-after: avoid;
            }
            
            table {
                page-break-inside: avoid;
            }
        }
        """
    
    def generate_json_evidence_pack(self, company_id: int,
                                  competitive_analyses: Dict[int, Dict[str, Any]],
                                  strategy_bundles: Dict[int, List[Dict[str, Any]]] = None) -> str:
        """
        Generate JSON evidence pack for PD2 integration.
        Returns path to generated JSON file.
        """
        thread_safe_print(f"📋 Generating JSON evidence pack...")
        
        # Get company information
        company_info = self._get_company_info(company_id)
        if not company_info:
            return ""
        
        # Build evidence pack structure
        evidence_pack = {
            "metadata": {
                "company_id": company_id,
                "company_name": company_info.get('name'),
                "generated_at": datetime.now().isoformat(),
                "analysis_version": "1.0",
                "total_market_cells": len(competitive_analyses)
            },
            "company_context": company_info.get('context', {}),
            "competitive_analyses": competitive_analyses,
            "strategy_bundles": strategy_bundles or {},
            "mini_tables": self._generate_mini_tables(competitive_analyses),
            "key_insights": self._extract_key_insights(competitive_analyses, strategy_bundles or {}),
            "data_quality": self._get_data_quality_summary()
        }
        
        # Save to JSON file
        company_name = company_info.get('name', 'Unknown').replace(' ', '_').replace(',', '').replace('.', '')
        filename = f"{company_name}_evidence_pack.json"
        json_path = self.output_dir / filename
        
        with open(json_path, 'w', encoding='utf-8') as f:
            json.dump(evidence_pack, f, indent=2, default=str)
        
        thread_safe_print(f"✅ Evidence pack generated: {json_path}")
        return str(json_path)
    
    def _generate_mini_tables(self, competitive_analyses: Dict[int, Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Generate mini-tables ready for PD2 section injection"""
        mini_tables = []
        
        for market_id, market_data in competitive_analyses.items():
            comp_analysis = market_data.get('competitive_analysis', {})
            if not comp_analysis.get('success'):
                continue
            
            market_info = market_data.get('market_cell_info', {})
            market_name = f"{market_info.get('product_service', 'Unknown')} × {market_info.get('geography', 'Unknown')}"
            
            # Create competitive positioning mini-table
            scored_metrics = comp_analysis.get('scored_metrics', [])
            if scored_metrics:
                table_data = {
                    "title": f"Competitive Positioning - {market_name}",
                    "type": "competitive_metrics",
                    "market_cell_id": market_id,
                    "headers": ["Metric", "Our Position", "Best Competitor", "Gap", "Confidence"],
                    "rows": []
                }
                
                for metric in scored_metrics[:5]:  # Top 5 metrics
                    metric_name = metric.get('metric_name', 'Unknown')
                    anchor_competitor = metric.get('anchor_competitor', 'Unknown')
                    
                    # Find our company's differentiation (simplified)
                    differentiation = metric.get('differentiation', {})
                    our_diff = next(iter(differentiation.values()), 0) if differentiation else 0
                    
                    gap_str = f"{our_diff:+.1%}" if our_diff != 0 else "Parity"
                    confidence = metric.get('confidence', 0)
                    
                    table_data["rows"].append([
                        metric_name,
                        "Competitive" if our_diff > 0 else "Behind" if our_diff < 0 else "Parity",
                        anchor_competitor,
                        gap_str,
                        f"{confidence:.2f}"
                    ])
                
                mini_tables.append(table_data)
        
        return mini_tables
    
    def _extract_key_insights(self, competitive_analyses: Dict[int, Dict[str, Any]],
                            strategy_bundles: Dict[int, List[Dict[str, Any]]]) -> List[str]:
        """Extract key insights for PD2 section enhancement"""
        insights = []
        
        # Competitive position insights
        positions = []
        for market_data in competitive_analyses.values():
            comp_analysis = market_data.get('competitive_analysis', {})
            if comp_analysis.get('success'):
                position = comp_analysis.get('overall_position', {}).get('position', '')
                if position:
                    positions.append(position)
        
        if positions:
            most_common = max(set(positions), key=positions.count)
            insights.append(f"Maintains {most_common.lower()} across primary market segments")
        
        # Strategic insights
        if strategy_bundles:
            total_strategies = sum(len(bundles) for bundles in strategy_bundles.values())
            insights.append(f"Strategic analysis identifies {total_strategies} actionable improvement initiatives")
        
        # Data quality insight
        total_analyses = len(competitive_analyses)
        successful_analyses = sum(1 for data in competitive_analyses.values() 
                                if data.get('competitive_analysis', {}).get('success'))
        
        if successful_analyses > 0:
            insights.append(f"High-quality competitive data available for {successful_analyses}/{total_analyses} market segments")
        
        return insights
    
    def _get_data_quality_summary(self) -> Dict[str, Any]:
        """Get overall data quality summary"""
        with self.db.get_connection() as conn:
            cursor = conn.execute("""
                SELECT 
                    COUNT(*) as total_observations,
                    AVG(confidence_score) as avg_confidence,
                    COUNT(CASE WHEN confidence_score >= 0.7 THEN 1 END) as high_confidence_count,
                    COUNT(DISTINCT competitor_id) as unique_competitors,
                    COUNT(DISTINCT metric_id) as unique_metrics
                FROM observations
            """)
            stats = cursor.fetchone()
            
            if stats:
                return {
                    "total_observations": stats['total_observations'],
                    "average_confidence": stats['avg_confidence'] or 0,
                    "high_confidence_percentage": (stats['high_confidence_count'] / max(1, stats['total_observations'])) * 100,
                    "unique_competitors": stats['unique_competitors'],
                    "unique_metrics": stats['unique_metrics']
                }
        
        return {}
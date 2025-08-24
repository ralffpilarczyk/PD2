"""
Advanced Conclusion Engine for DDAR
Generates detailed, numerical conclusions with chain of thought reasoning
"""

import json
from typing import Dict, List, Tuple, Optional, Any
from dataclasses import dataclass
from datetime import datetime
from config import DEFAULT_WACC

@dataclass
class TheoremContext:
    """Context for theorem analysis"""
    company: str
    theorem: str
    metric: str
    value: float
    all_facts: Dict[str, float]
    period: str
    
@dataclass
class ReasoningChain:
    """Represents a chain of logical reasoning"""
    steps: List[str]
    conclusion: str
    confidence: float
    evidence: List[str]
    
class ConclusionEngine:
    """Generate intelligent, numerical conclusions with reasoning chains"""
    
    def __init__(self):
        # Industry benchmarks and thresholds
        self.benchmarks = {
            'roic': {'poor': 0.05, 'moderate': 0.10, 'good': 0.15, 'excellent': 0.20, 'industry_avg': 0.12},
            'roe': {'poor': 0.08, 'moderate': 0.12, 'good': 0.15, 'excellent': 0.20, 'industry_avg': 0.14},
            'roa': {'poor': 0.03, 'moderate': 0.05, 'good': 0.08, 'excellent': 0.12, 'industry_avg': 0.06},
            'npm': {'poor': 0.05, 'moderate': 0.10, 'good': 0.15, 'excellent': 0.20, 'industry_avg': 0.12},
            'at': {'poor': 0.5, 'moderate': 0.8, 'good': 1.2, 'excellent': 1.5, 'industry_avg': 1.0},
            'current_ratio': {'poor': 0.8, 'moderate': 1.0, 'good': 1.5, 'excellent': 2.0, 'optimal': 1.5},
            'debt_to_equity': {'excellent': 0.3, 'good': 0.5, 'moderate': 1.0, 'poor': 2.0, 'industry_avg': 0.7},
            'interest_coverage': {'poor': 1.5, 'moderate': 2.5, 'good': 4.0, 'excellent': 6.0, 'industry_avg': 3.5},
            'ccc': {'excellent': 0, 'good': 30, 'moderate': 60, 'poor': 90, 'industry_avg': 45},
            'fcf_conversion': {'poor': 0.3, 'moderate': 0.5, 'good': 0.7, 'excellent': 0.9, 'industry_avg': 0.6},
            'cfroi': {'poor': 0.06, 'moderate': 0.10, 'good': 0.15, 'excellent': 0.20, 'industry_avg': 0.12},
            'wacc': DEFAULT_WACC  # Default, will be overridden from facts if available
        }
        
    def generate_intelligent_conclusion(self, context: TheoremContext) -> Dict[str, Any]:
        """Generate intelligent conclusion with numerical context and reasoning"""
        
        # Build reasoning chain
        chain = self._build_reasoning_chain(context)
        
        # Generate numerical insights
        numerical_insights = self._generate_numerical_insights(context)
        
        # Identify implications
        implications = self._identify_implications(context, chain)
        
        # Generate recommendations with specific targets
        recommendations = self._generate_targeted_recommendations(context, numerical_insights)
        
        # Calculate confidence based on data quality and logic strength
        confidence = self._calculate_confidence(context, chain)
        
        return {
            'conclusion': chain.conclusion,
            'reasoning_chain': chain.steps,
            'numerical_insights': numerical_insights,
            'implications': implications,
            'recommendations': recommendations,
            'confidence': confidence,
            'evidence': chain.evidence
        }
    
    def _build_reasoning_chain(self, context: TheoremContext) -> ReasoningChain:
        """Build logical reasoning chain for the theorem"""
        
        steps = []
        evidence = []
        
        if 'roic' in context.theorem.lower():
            # ROIC reasoning chain
            roic = context.value
            wacc = context.all_facts.get('wacc', self.benchmarks.get('wacc', DEFAULT_WACC))
            spread = roic - wacc
            
            # Get components if available
            npm = context.all_facts.get('npm', 0)
            at = context.all_facts.get('at', 0)
            
            steps.append(f"ROIC calculated at {roic:.2%}")
            
            if npm and at:
                steps.append(f"Decomposition: NPM ({npm:.1%}) × Asset Turnover ({at:.2f}x) = {roic:.2%}")
                
                # Identify bottleneck
                if npm < self.benchmarks['npm']['moderate']:
                    steps.append(f"Primary constraint: Net Profit Margin at {npm:.1%} is {(self.benchmarks['npm']['industry_avg']/npm - 1)*100:.0f}% below industry average")
                elif at < self.benchmarks['at']['moderate']:
                    steps.append(f"Primary constraint: Asset Turnover at {at:.2f}x is {(1 - at/self.benchmarks['at']['industry_avg'])*100:.0f}% below industry average")
            
            steps.append(f"Value Spread (ROIC - WACC): {spread:.2%}")
            
            if spread < 0:
                steps.append(f"Destroying value: Each $1 of capital destroys ${abs(spread):.2f} of value")
                conclusion = f"CRITICAL: ROIC of {roic:.2%} is {abs(spread/wacc)*100:.0f}% below cost of capital, destroying ${abs(spread)*context.all_facts.get('total_assets', 1000000)/1000000:.1f}M annually"
            elif spread < 0.03:
                steps.append(f"Marginal value creation: Barely covering cost of capital")
                conclusion = f"ROIC of {roic:.2%} marginally exceeds WACC by {spread:.2%}, creating minimal economic value"
            else:
                steps.append(f"Creating value: Each $1 of capital creates ${spread:.2f} of value")
                conclusion = f"Strong ROIC of {roic:.2%} exceeds WACC by {spread/wacc*100:.0f}%, creating ${spread*context.all_facts.get('total_assets', 1000000)/1000000:.1f}M in annual economic value"
            
            evidence = [f"npm:{npm:.2%}", f"at:{at:.2f}", f"wacc_assumed:{wacc:.2%}"]
            
        elif 'fcf' in context.theorem.lower() and 'conversion' not in context.theorem.lower():
            # FCF reasoning chain
            fcf = context.value
            ocf = context.all_facts.get('operating_cash_flow', 0)
            capex = context.all_facts.get('capex', 0)
            revenue = context.all_facts.get('revenue', 0)
            
            steps.append(f"Free Cash Flow: ${fcf/1000000:.1f}M")
            
            if ocf and capex:
                steps.append(f"Components: OCF ${ocf/1000000:.1f}M - CapEx ${capex/1000000:.1f}M = FCF ${fcf/1000000:.1f}M")
                capex_intensity = capex / ocf if ocf else float('inf')
                steps.append(f"CapEx intensity: {capex_intensity:.1f}x of operating cash flow")
                
                if capex_intensity > 1.5:
                    steps.append(f"WARNING: CapEx consuming {capex_intensity:.1f}x OCF - unsustainable without external funding")
            
            if revenue:
                fcf_margin = fcf / revenue
                steps.append(f"FCF margin: {fcf_margin:.1%} of revenue")
                
                if fcf_margin < -0.05:
                    conclusion = f"SEVERE: Negative FCF of ${fcf/1000000:.1f}M ({fcf_margin:.1%} of revenue) indicates cash burn rate requiring immediate attention"
                elif fcf_margin < 0:
                    conclusion = f"Negative FCF of ${fcf/1000000:.1f}M driven by CapEx of ${capex/1000000:.1f}M ({capex/ocf:.1f}x OCF)"
                elif fcf_margin < 0.05:
                    conclusion = f"Marginal FCF of ${fcf/1000000:.1f}M ({fcf_margin:.1%} of revenue) limits strategic flexibility"
                else:
                    conclusion = f"Healthy FCF of ${fcf/1000000:.1f}M ({fcf_margin:.1%} margin) supports growth and shareholder returns"
            else:
                conclusion = f"FCF of ${fcf/1000000:.1f}M with CapEx/OCF ratio of {capex/ocf:.1f}x"
            
            evidence = [f"ocf:${ocf/1000000:.1f}M", f"capex:${capex/1000000:.1f}M", f"revenue:${revenue/1000000:.1f}M"]
            
        elif 'ccc' in context.theorem.lower():
            # Cash Conversion Cycle reasoning
            ccc = context.value
            dso = context.all_facts.get('dso', 0)
            dio = context.all_facts.get('dio', 0)
            dpo = context.all_facts.get('dpo', 0)
            
            steps.append(f"Cash Conversion Cycle: {ccc:.1f} days")
            
            if dso and dio and dpo:
                steps.append(f"Components: DSO {dso:.1f} + DIO {dio:.1f} - DPO {dpo:.1f} = {ccc:.1f} days")
                
                # Identify optimization opportunities
                if dso > 45:
                    steps.append(f"Collection issue: DSO of {dso:.1f} days is {dso-30:.0f} days above best practice")
                if dio > 30:
                    steps.append(f"Inventory issue: DIO of {dio:.1f} days suggests excess inventory")
                if dpo < 45:
                    steps.append(f"Payment opportunity: DPO of {dpo:.1f} days could be extended")
            
            industry_avg = self.benchmarks['ccc']['industry_avg']
            variance = (ccc - industry_avg) / industry_avg
            
            if ccc < 0:
                conclusion = f"EXCELLENT: Negative CCC of {ccc:.1f} days - collecting cash {abs(ccc):.0f} days before paying suppliers"
            elif variance < -0.2:
                conclusion = f"Strong CCC of {ccc:.1f} days is {abs(variance)*100:.0f}% better than industry, providing working capital advantage"
            elif variance > 0.5:
                conclusion = f"Poor CCC of {ccc:.1f} days is {variance*100:.0f}% worse than industry, tying up ${ccc * context.all_facts.get('revenue', 0) / 365 / 1000000:.1f}M in working capital"
            else:
                conclusion = f"CCC of {ccc:.1f} days is {('above' if variance > 0 else 'below')} industry average by {abs(variance)*100:.0f}%"
            
            evidence = [f"dso:{dso:.1f}", f"dio:{dio:.1f}", f"dpo:{dpo:.1f}"]
            
        else:
            # Default reasoning
            steps.append(f"Calculated {context.metric}: {context.value:.4f}")
            conclusion = f"{context.metric}: {context.value:.4f}"
            evidence = []
        
        return ReasoningChain(steps=steps, conclusion=conclusion, confidence=0.85, evidence=evidence)
    
    def _generate_numerical_insights(self, context: TheoremContext) -> List[str]:
        """Generate numerical insights with comparisons and context"""
        
        insights = []
        
        # Get benchmark if available
        metric_key = context.metric.lower()
        if metric_key in self.benchmarks:
            benchmark = self.benchmarks[metric_key]
            industry_avg = benchmark.get('industry_avg', 0)
            
            if industry_avg:
                variance = (context.value - industry_avg) / industry_avg if industry_avg else 0
                if abs(variance) > 0.1:
                    insights.append(f"{context.metric} is {abs(variance)*100:.0f}% {'above' if variance > 0 else 'below'} industry average of {industry_avg:.2%}")
            
            # Performance tier
            if 'poor' in benchmark:
                if context.value < benchmark['poor']:
                    insights.append(f"Performance in bottom quartile (below {benchmark['poor']:.2%})")
                elif context.value < benchmark['moderate']:
                    insights.append(f"Below average performance (threshold: {benchmark['moderate']:.2%})")
                elif context.value < benchmark['good']:
                    insights.append(f"Moderate performance (next tier: {benchmark['good']:.2%})")
                elif context.value < benchmark['excellent']:
                    insights.append(f"Good performance (excellence at: {benchmark['excellent']:.2%})")
                else:
                    insights.append(f"Excellent performance (top tier above {benchmark['excellent']:.2%})")
        
        # Add context from related metrics
        if 'roic' in context.theorem.lower():
            if 'total_assets' in context.all_facts:
                economic_profit = context.value * context.all_facts['total_assets']
                insights.append(f"Implied economic profit: ${economic_profit/1000000:.1f}M annually")
        
        if 'fcf' in context.theorem.lower():
            if 'revenue' in context.all_facts:
                fcf_yield = context.value / context.all_facts['revenue']
                insights.append(f"FCF yield on revenue: {fcf_yield:.1%}")
            if 'ebitda' in context.all_facts and context.all_facts['ebitda'] > 0:
                fcf_to_ebitda = context.value / context.all_facts['ebitda']
                insights.append(f"FCF/EBITDA conversion: {fcf_to_ebitda:.1%}")
        
        return insights
    
    def _identify_implications(self, context: TheoremContext, chain: ReasoningChain) -> List[str]:
        """Identify business implications from the analysis"""
        
        implications = []
        
        if 'roic' in context.theorem.lower():
            if context.value < 0.05:
                implications.append("Urgent need to restructure operations or exit underperforming assets")
                implications.append("Current capital allocation destroying shareholder value")
            elif context.value < 0.10:
                implications.append("Returns likely below cost of capital - review strategic options")
                implications.append("Limited ability to fund growth internally")
            else:
                implications.append("Positive value creation supports organic growth")
                implications.append("Strong position for competitive advantage")
        
        elif 'fcf' in context.theorem.lower():
            if context.value < 0:
                implications.append("External funding required to sustain operations")
                implications.append("Limited financial flexibility for strategic initiatives")
                if 'debt_to_equity' in context.all_facts and context.all_facts['debt_to_equity'] > 1:
                    implications.append("High leverage combined with negative FCF increases financial risk")
            else:
                implications.append("Self-funding capability for growth investments")
                implications.append("Flexibility for dividends or share buybacks")
        
        elif 'ccc' in context.theorem.lower():
            if context.value > 60:
                working_capital_tied = context.value * context.all_facts.get('revenue', 0) / 365
                implications.append(f"Approximately ${working_capital_tied/1000000:.1f}M tied up in working capital")
                implications.append("Opportunity to release cash through working capital optimization")
            elif context.value < 0:
                implications.append("Negative float provides free financing from suppliers")
                implications.append("Strong negotiating position with supply chain")
        
        return implications
    
    def _generate_targeted_recommendations(self, context: TheoremContext, insights: List[str]) -> List[str]:
        """Generate specific, actionable recommendations with targets"""
        
        recommendations = []
        
        if 'roic' in context.theorem.lower():
            target_roic = max(self.benchmarks['wacc'] + 0.03, self.benchmarks['roic']['industry_avg'])
            gap = target_roic - context.value
            
            if gap > 0:
                # Calculate required improvements
                npm = context.all_facts.get('npm', 0.10)
                at = context.all_facts.get('at', 1.0)
                
                # Option 1: Improve margins
                required_npm = target_roic / at if at > 0 else npm
                npm_improvement = (required_npm / npm - 1) * 100 if npm > 0 else 100
                
                # Option 2: Improve asset turnover
                required_at = target_roic / npm if npm > 0 else at
                at_improvement = (required_at / at - 1) * 100 if at > 0 else 100
                
                if npm_improvement < at_improvement and npm_improvement < 50:
                    recommendations.append(f"Increase NPM from {npm:.1%} to {required_npm:.1%} ({npm_improvement:.0f}% improvement) to achieve {target_roic:.1%} ROIC")
                elif at_improvement < 100:
                    recommendations.append(f"Improve asset turnover from {at:.2f}x to {required_at:.2f}x ({at_improvement:.0f}% improvement) to achieve {target_roic:.1%} ROIC")
                else:
                    recommendations.append(f"Comprehensive improvement needed: Both margins and asset efficiency require enhancement")
            else:
                recommendations.append(f"Maintain ROIC above {target_roic:.1%} through operational excellence")
        
        elif 'fcf' in context.theorem.lower() and context.value < 0:
            ocf = context.all_facts.get('operating_cash_flow', 0)
            capex = context.all_facts.get('capex', 0)
            
            if capex > ocf * 0.7:
                capex_reduction = capex - (ocf * 0.7)
                recommendations.append(f"Reduce CapEx by ${capex_reduction/1000000:.1f}M ({capex_reduction/capex*100:.0f}%) to achieve positive FCF")
            
            if ocf < context.all_facts.get('ebitda', 0) * 0.7:
                recommendations.append(f"Improve working capital to increase OCF/EBITDA conversion above 70%")
        
        elif 'ccc' in context.theorem.lower() and context.value > 45:
            target_ccc = self.benchmarks['ccc']['industry_avg']
            days_to_improve = context.value - target_ccc
            cash_release = days_to_improve * context.all_facts.get('revenue', 0) / 365
            
            recommendations.append(f"Reduce CCC by {days_to_improve:.0f} days to release ${cash_release/1000000:.1f}M in working capital")
            
            # Specific levers
            dso = context.all_facts.get('dso', 0)
            if dso > 35:
                recommendations.append(f"Accelerate collections: Reduce DSO from {dso:.0f} to 35 days")
            
            dio = context.all_facts.get('dio', 0)
            if dio > 30:
                recommendations.append(f"Optimize inventory: Reduce DIO from {dio:.0f} to 30 days")
        
        return recommendations
    
    def _calculate_confidence(self, context: TheoremContext, chain: ReasoningChain) -> float:
        """Calculate confidence based on data completeness and logic strength"""
        
        base_confidence = 0.7
        
        # Adjust for data completeness
        required_metrics = {
            'roic': ['npm', 'at', 'total_assets'],
            'fcf': ['operating_cash_flow', 'capex', 'revenue'],
            'ccc': ['dso', 'dio', 'dpo']
        }
        
        for theorem_type, metrics in required_metrics.items():
            if theorem_type in context.theorem.lower():
                available = sum(1 for m in metrics if m in context.all_facts and context.all_facts[m])
                completeness = available / len(metrics)
                base_confidence += completeness * 0.2
                break
        
        # Adjust for value reasonableness
        if context.metric in self.benchmarks:
            benchmark = self.benchmarks[context.metric]
            if 'industry_avg' in benchmark:
                variance = abs(context.value - benchmark['industry_avg']) / benchmark['industry_avg']
                if variance > 2:  # More than 200% variance
                    base_confidence -= 0.1
        
        # Adjust for chain complexity
        if len(chain.steps) > 3:
            base_confidence += 0.05
        
        return min(max(base_confidence, 0.5), 0.95)

def create_chain_visualization(chains: List[ReasoningChain]) -> str:
    """Create visual representation of reasoning chains"""
    
    html = "<div class='reasoning-chains'>"
    
    for i, chain in enumerate(chains):
        html += f"<div class='chain-container'>"
        html += f"<h4>Reasoning Chain {i+1}</h4>"
        
        # Show steps as connected flow
        for j, step in enumerate(chain.steps):
            html += f"<div class='chain-step'>"
            html += f"<span class='step-number'>{j+1}</span>"
            html += f"<span class='step-content'>{step}</span>"
            if j < len(chain.steps) - 1:
                html += "<span class='step-arrow'>→</span>"
            html += "</div>"
        
        # Show conclusion
        html += f"<div class='chain-conclusion'>"
        html += f"<strong>Conclusion:</strong> {chain.conclusion}"
        html += f"<span class='confidence-badge'>{chain.confidence:.0%} confidence</span>"
        html += "</div>"
        
        # Show evidence
        if chain.evidence:
            html += f"<div class='chain-evidence'>"
            html += f"<strong>Evidence:</strong> {', '.join(chain.evidence)}"
            html += "</div>"
        
        html += "</div>"
    
    html += "</div>"
    
    return html
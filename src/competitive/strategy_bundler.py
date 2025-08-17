"""
Strategy Bundler with Deep Conflict Analysis.
Generates viable strategy bundles with resource allocation analysis and conflict detection.
"""

import json
import re
from typing import Dict, List, Optional, Tuple, Any
from ..utils import thread_safe_print, retry_with_backoff
from .database import CompetitiveDatabase
import google.generativeai as genai


class StrategyBundler:
    """Generates strategic recommendations with deep conflict analysis"""
    
    # Resource conflict matrix - which strategies compete for same resources
    RESOURCE_CONFLICTS = {
        "cost_reduction": ["automation", "process_optimization", "outsourcing"],
        "innovation_investment": ["r_and_d", "product_development", "technology_upgrade"],
        "market_expansion": ["geographic_expansion", "market_penetration", "customer_acquisition"],
        "operational_excellence": ["quality_improvement", "efficiency_programs", "lean_initiatives"],
        "customer_focus": ["service_enhancement", "personalization", "customer_experience"],
        "scale_building": ["capacity_expansion", "infrastructure_investment", "partnership"]
    }
    
    def __init__(self, db: CompetitiveDatabase, model_name: str = "gemini-2.5-flash"):
        """Initialize with database and strategy generation model"""
        self.db = db
        self.model_name = model_name
        self.strategy_model = genai.GenerativeModel(model_name)
    
    def generate_strategy_bundles(self, market_cell_id: int, competitive_analysis: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        Generate 2-3 viable strategy bundles for a market cell based on competitive analysis.
        Includes deep conflict detection and resource allocation analysis.
        """
        thread_safe_print(f"Generating strategy bundles for market cell {market_cell_id}")
        
        if not competitive_analysis.get('success'):
            thread_safe_print("Cannot generate strategies without competitive analysis")
            return []
        
        # Extract key insights for strategy generation
        strengths = competitive_analysis.get('top_strengths', [])
        weaknesses = competitive_analysis.get('top_weaknesses', [])
        overall_position = competitive_analysis.get('overall_position', {})
        scored_metrics = competitive_analysis.get('scored_metrics', [])
        
        # Get market cell context
        market_cell_info = self._get_market_cell_context(market_cell_id)
        
        # Generate initial strategy concepts
        strategy_concepts = self._generate_strategy_concepts(
            strengths=strengths,
            weaknesses=weaknesses,
            overall_position=overall_position,
            market_cell_info=market_cell_info,
            scored_metrics=scored_metrics
        )
        
        if not strategy_concepts:
            thread_safe_print("No strategy concepts generated")
            return []
        
        # Analyze resource conflicts and optimize bundles
        optimized_bundles = self._analyze_and_optimize_bundles(strategy_concepts, market_cell_info)
        
        # Save to database
        saved_bundles = self._save_strategy_bundles(market_cell_id, optimized_bundles)
        
        thread_safe_print(f"Generated {len(saved_bundles)} strategy bundles")
        return saved_bundles
    
    def _get_market_cell_context(self, market_cell_id: int) -> Dict[str, Any]:
        """Get market cell context and company information"""
        with self.db.get_connection() as conn:
            # Get market cell info
            market_cell_query = """
                SELECT mc.*, c.name as company_name, c.context_json
                FROM market_cells mc
                JOIN companies c ON mc.company_id = c.id
                WHERE mc.id = ?
            """
            cursor = conn.execute(market_cell_query, (market_cell_id,))
            market_cell_row = cursor.fetchone()
            
            if not market_cell_row:
                return {}
            
            return {
                "market_cell_id": market_cell_id,
                "product_service": market_cell_row['product_service'],
                "geography": market_cell_row['geography'],
                "customer_segment": market_cell_row['customer_segment'],
                "materiality_score": market_cell_row['materiality_score'],
                "company_name": market_cell_row['company_name'],
                "company_context": json.loads(market_cell_row['context_json']) if market_cell_row['context_json'] else {}
            }
    
    def _generate_strategy_concepts(self, strengths: List[Dict], weaknesses: List[Dict],
                                  overall_position: Dict[str, Any], market_cell_info: Dict[str, Any],
                                  scored_metrics: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Generate initial strategy concepts using LLM analysis"""
        
        prompt = f"""Generate strategic recommendations for this competitive situation:

MARKET CELL: {market_cell_info.get('product_service', 'Unknown')} × {market_cell_info.get('geography', 'Unknown')} × {market_cell_info.get('customer_segment', 'Unknown')}

COMPANY: {market_cell_info.get('company_name', 'Unknown')}
INDUSTRY: {market_cell_info.get('company_context', {}).get('industry', 'Unknown')}
BUSINESS MODEL: {market_cell_info.get('company_context', {}).get('business_model', 'Unknown')}

COMPETITIVE POSITION: {overall_position.get('position', 'Unknown')}
COMPETITIVE SCORE: {overall_position.get('competitive_score', 0.5):.2f}

TOP STRENGTHS:
{json.dumps(strengths, indent=2)}

TOP WEAKNESSES:
{json.dumps(weaknesses, indent=2)}

Generate 3-4 distinct strategy concepts that address the competitive situation:

[
    {{
        "name": "Strategy Name",
        "mechanism": "How to achieve the improvement - specific actions and approaches",
        "required_enablers": ["capability 1", "resource 2", "investment 3"],
        "addresses_weaknesses": ["specific weakness addressed"],
        "leverages_strengths": ["specific strength leveraged"],
        "expected_kpi_shifts": {{
            "conservative": "realistic impact in 12 months",
            "optimistic": "best-case scenario impact"
        }},
        "risks": ["primary risk 1", "secondary risk 2"],
        "timeline_months": 12,
        "resource_requirements": {{
            "capex": "capital investment needed",
            "opex": "operational spending required", 
            "people": "human resources needed"
        }},
        "strategic_category": "cost_reduction/innovation_investment/market_expansion/operational_excellence/customer_focus/scale_building"
    }
]

Focus on:
1. Addressing the most significant competitive weaknesses first
2. Leveraging existing strengths for competitive advantage
3. Realistic 12-month actionability 
4. Resource allocation considerations
5. Business-specific strategic logic

Strategic Categories:
- cost_reduction: Focus on operational efficiency and cost position
- innovation_investment: R&D, product development, technology advancement
- market_expansion: Geographic growth, market penetration, customer acquisition
- operational_excellence: Quality, process optimization, lean operations
- customer_focus: Service enhancement, customer experience, personalization
- scale_building: Capacity expansion, infrastructure, partnerships

Return valid JSON array only."""
        
        try:
            response_text = retry_with_backoff(
                lambda: self.strategy_model.generate_content(prompt).text
            )
            
            # Extract JSON array from response
            json_match = re.search(r'\[.*?\]', response_text, re.DOTALL)
            if json_match:
                strategy_concepts = json.loads(json_match.group())
                
                # Validate and clean strategy concepts
                valid_concepts = []
                for concept in strategy_concepts:
                    if (isinstance(concept, dict) and 
                        all(key in concept for key in ['name', 'mechanism', 'strategic_category'])):
                        
                        # Ensure required fields
                        concept['required_enablers'] = concept.get('required_enablers', [])
                        concept['addresses_weaknesses'] = concept.get('addresses_weaknesses', [])
                        concept['leverages_strengths'] = concept.get('leverages_strengths', [])
                        concept['expected_kpi_shifts'] = concept.get('expected_kpi_shifts', {})
                        concept['risks'] = concept.get('risks', [])
                        concept['timeline_months'] = concept.get('timeline_months', 12)
                        concept['resource_requirements'] = concept.get('resource_requirements', {})
                        
                        valid_concepts.append(concept)
                
                thread_safe_print(f"✓ Generated {len(valid_concepts)} strategy concepts")
                return valid_concepts
            else:
                raise ValueError("No valid JSON array found in response")
                
        except Exception as e:
            thread_safe_print(f"⚠ Error generating strategy concepts: {e}")
            return self._get_fallback_strategies(weaknesses, strengths)
    
    def _get_fallback_strategies(self, weaknesses: List[Dict], strengths: List[Dict]) -> List[Dict[str, Any]]:
        """Generate fallback strategies if LLM generation fails"""
        thread_safe_print("Using fallback strategy generation...")
        
        fallback_strategies = []
        
        # Strategy 1: Address top weakness
        if weaknesses:
            top_weakness = weaknesses[0]
            fallback_strategies.append({
                "name": f"Address {top_weakness['metric']} Gap",
                "mechanism": f"Focus on improving {top_weakness['metric']} through targeted initiatives",
                "strategic_category": "operational_excellence",
                "addresses_weaknesses": [top_weakness['metric']],
                "leverages_strengths": [],
                "timeline_months": 12,
                "resource_requirements": {"opex": "Moderate operational investment"},
                "risks": ["Implementation challenges", "Resource constraints"]
            })
        
        # Strategy 2: Leverage top strength
        if strengths:
            top_strength = strengths[0]
            fallback_strategies.append({
                "name": f"Leverage {top_strength['metric']} Advantage",
                "mechanism": f"Build on superior {top_strength['metric']} to gain market advantage",
                "strategic_category": "scale_building",
                "addresses_weaknesses": [],
                "leverages_strengths": [top_strength['metric']],
                "timeline_months": 9,
                "resource_requirements": {"capex": "Moderate investment to scale strength"},
                "risks": ["Market response", "Competitive reaction"]
            })
        
        return fallback_strategies
    
    def _analyze_and_optimize_bundles(self, strategy_concepts: List[Dict[str, Any]], 
                                    market_cell_info: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        Analyze resource conflicts and optimize strategy bundles.
        Applies deep thinking about resource allocation and strategic conflicts.
        """
        thread_safe_print("Analyzing resource conflicts and optimizing bundles...")
        
        # Group strategies by resource requirements and strategic category
        strategy_groups = self._group_strategies_by_resources(strategy_concepts)
        
        # Detect conflicts
        conflicts = self._detect_strategic_conflicts(strategy_concepts)
        
        # Generate optimized bundles
        optimized_bundles = []
        
        # Bundle 1: Focus on highest-impact, lowest-conflict strategies
        bundle1 = self._create_focused_bundle(strategy_concepts, conflicts, "high_impact_low_conflict")
        if bundle1:
            optimized_bundles.append(bundle1)
        
        # Bundle 2: Address weaknesses systematically
        bundle2 = self._create_focused_bundle(strategy_concepts, conflicts, "weakness_focused")
        if bundle2:
            optimized_bundles.append(bundle2)
        
        # Bundle 3: Leverage strengths for growth
        bundle3 = self._create_focused_bundle(strategy_concepts, conflicts, "strength_focused")
        if bundle3:
            optimized_bundles.append(bundle3)
        
        return optimized_bundles[:3]  # Return max 3 bundles
    
    def _group_strategies_by_resources(self, strategies: List[Dict[str, Any]]) -> Dict[str, List[Dict]]:
        """Group strategies by resource requirements"""
        resource_groups = {
            "high_capex": [],
            "high_opex": [],
            "people_intensive": [],
            "low_resource": []
        }
        
        for strategy in strategies:
            resources = strategy.get('resource_requirements', {})
            
            # Classify by resource intensity (simple heuristic)
            capex = str(resources.get('capex', '')).lower()
            opex = str(resources.get('opex', '')).lower()
            people = str(resources.get('people', '')).lower()
            
            if any(term in capex for term in ['high', 'significant', 'major']):
                resource_groups['high_capex'].append(strategy)
            elif any(term in opex for term in ['high', 'significant', 'major']):
                resource_groups['high_opex'].append(strategy)
            elif any(term in people for term in ['extensive', 'large', 'significant']):
                resource_groups['people_intensive'].append(strategy)
            else:
                resource_groups['low_resource'].append(strategy)
        
        return resource_groups
    
    def _detect_strategic_conflicts(self, strategies: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Detect conflicts between strategies using resource allocation analysis.
        Applies deep thinking about competing resource demands.
        """
        conflicts = []
        
        for i, strategy1 in enumerate(strategies):
            for j, strategy2 in enumerate(strategies[i+1:], i+1):
                conflict_analysis = self._analyze_strategy_conflict(strategy1, strategy2)
                if conflict_analysis['has_conflict']:
                    conflicts.append({
                        "strategy1": strategy1['name'],
                        "strategy2": strategy2['name'],
                        "conflict_type": conflict_analysis['conflict_type'],
                        "severity": conflict_analysis['severity'],
                        "description": conflict_analysis['description']
                    })
        
        return conflicts
    
    def _analyze_strategy_conflict(self, strategy1: Dict[str, Any], strategy2: Dict[str, Any]) -> Dict[str, Any]:
        """
        Analyze conflict between two strategies using business logic.
        Implements the deep analysis required by WEBSEARCH.md.
        """
        # Check strategic category conflicts
        cat1 = strategy1.get('strategic_category', '')
        cat2 = strategy2.get('strategic_category', '')
        
        # Known conflicting combinations
        category_conflicts = {
            ('cost_reduction', 'innovation_investment'): {
                'type': 'budget_allocation',
                'severity': 0.8,
                'description': 'Cost reduction and innovation investment compete for financial resources'
            },
            ('cost_reduction', 'customer_focus'): {
                'type': 'strategic_direction',
                'severity': 0.6,
                'description': 'Cost focus may conflict with customer experience investments'
            },
            ('market_expansion', 'operational_excellence'): {
                'type': 'management_attention',
                'severity': 0.5,
                'description': 'Geographic expansion and operational optimization require different management focus'
            },
            ('innovation_investment', 'scale_building'): {
                'type': 'resource_allocation',
                'severity': 0.4,
                'description': 'R&D investment and capacity building may compete for capital'
            }
        }
        
        # Check for direct category conflict
        conflict_key = tuple(sorted([cat1, cat2]))
        if conflict_key in category_conflicts:
            conflict_info = category_conflicts[conflict_key]
            return {
                'has_conflict': True,
                'conflict_type': conflict_info['type'],
                'severity': conflict_info['severity'],
                'description': conflict_info['description']
            }
        
        # Check resource requirement conflicts
        res1 = strategy1.get('resource_requirements', {})
        res2 = strategy2.get('resource_requirements', {})
        
        # Both require high capital investment
        if (any(term in str(res1.get('capex', '')).lower() for term in ['high', 'significant']) and
            any(term in str(res2.get('capex', '')).lower() for term in ['high', 'significant'])):
            return {
                'has_conflict': True,
                'conflict_type': 'capital_competition',
                'severity': 0.7,
                'description': 'Both strategies require significant capital investment simultaneously'
            }
        
        # Both require extensive people resources
        if (any(term in str(res1.get('people', '')).lower() for term in ['extensive', 'large']) and
            any(term in str(res2.get('people', '')).lower() for term in ['extensive', 'large'])):
            return {
                'has_conflict': True,
                'conflict_type': 'talent_competition',
                'severity': 0.6,
                'description': 'Both strategies require extensive human resources simultaneously'
            }
        
        # No significant conflict detected
        return {
            'has_conflict': False,
            'conflict_type': None,
            'severity': 0.0,
            'description': 'No significant resource or strategic conflicts detected'
        }
    
    def _create_focused_bundle(self, strategies: List[Dict[str, Any]], 
                             conflicts: List[Dict[str, Any]], bundle_type: str) -> Optional[Dict[str, Any]]:
        """Create a focused strategy bundle based on type and conflict analysis"""
        
        if bundle_type == "high_impact_low_conflict":
            # Select strategies with minimal conflicts
            selected_strategies = []
            conflict_matrix = {c['strategy1']: c['strategy2'] for c in conflicts if c['severity'] > 0.6}
            
            for strategy in strategies:
                strategy_name = strategy['name']
                has_high_conflict = (strategy_name in conflict_matrix or 
                                   any(strategy_name == c['strategy2'] for c in conflicts if c['severity'] > 0.6))
                
                if not has_high_conflict:
                    selected_strategies.append(strategy)
            
            bundle_name = "Balanced Growth Strategy"
            bundle_description = "Focus on high-impact initiatives with minimal resource conflicts"
            
        elif bundle_type == "weakness_focused":
            # Select strategies that address weaknesses
            selected_strategies = [s for s in strategies if s.get('addresses_weaknesses')]
            bundle_name = "Competitive Gap Closure"
            bundle_description = "Systematic approach to address competitive weaknesses"
            
        elif bundle_type == "strength_focused":
            # Select strategies that leverage strengths
            selected_strategies = [s for s in strategies if s.get('leverages_strengths')]
            bundle_name = "Advantage Amplification"
            bundle_description = "Build on existing competitive advantages"
            
        else:
            return None
        
        if not selected_strategies:
            return None
        
        # Limit to 2-3 strategies per bundle to maintain focus
        selected_strategies = selected_strategies[:3]
        
        # Analyze bundle-level conflicts
        bundle_conflicts = self._analyze_bundle_conflicts(selected_strategies, conflicts)
        
        return {
            "name": bundle_name,
            "description": bundle_description,
            "strategies": selected_strategies,
            "bundle_conflicts": bundle_conflicts,
            "total_strategies": len(selected_strategies),
            "estimated_timeline": max([s.get('timeline_months', 12) for s in selected_strategies]),
            "resource_summary": self._summarize_bundle_resources(selected_strategies),
            "risk_assessment": self._assess_bundle_risks(selected_strategies),
            "feasibility_score": self._calculate_bundle_feasibility(selected_strategies, bundle_conflicts)
        }
    
    def _analyze_bundle_conflicts(self, selected_strategies: List[Dict[str, Any]], 
                                all_conflicts: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Analyze conflicts within the selected bundle"""
        strategy_names = {s['name'] for s in selected_strategies}
        
        bundle_conflicts = []
        for conflict in all_conflicts:
            if (conflict['strategy1'] in strategy_names and 
                conflict['strategy2'] in strategy_names):
                bundle_conflicts.append(conflict)
        
        return bundle_conflicts
    
    def _summarize_bundle_resources(self, strategies: List[Dict[str, Any]]) -> Dict[str, str]:
        """Summarize resource requirements for the bundle"""
        capex_requirements = []
        opex_requirements = []
        people_requirements = []
        
        for strategy in strategies:
            resources = strategy.get('resource_requirements', {})
            if resources.get('capex'):
                capex_requirements.append(resources['capex'])
            if resources.get('opex'):
                opex_requirements.append(resources['opex'])
            if resources.get('people'):
                people_requirements.append(resources['people'])
        
        return {
            "capex": "; ".join(capex_requirements) if capex_requirements else "Low capital requirements",
            "opex": "; ".join(opex_requirements) if opex_requirements else "Minimal operational costs",
            "people": "; ".join(people_requirements) if people_requirements else "Limited additional headcount"
        }
    
    def _assess_bundle_risks(self, strategies: List[Dict[str, Any]]) -> List[str]:
        """Assess combined risks for the bundle"""
        all_risks = []
        for strategy in strategies:
            risks = strategy.get('risks', [])
            all_risks.extend(risks)
        
        # Remove duplicates and return unique risks
        unique_risks = list(set(all_risks))
        return unique_risks[:5]  # Limit to top 5 risks
    
    def _calculate_bundle_feasibility(self, strategies: List[Dict[str, Any]], 
                                    conflicts: List[Dict[str, Any]]) -> float:
        """Calculate feasibility score for the bundle (0.0-1.0)"""
        base_feasibility = 0.8  # Start with high feasibility
        
        # Reduce for conflicts
        conflict_penalty = sum(c.get('severity', 0) for c in conflicts) * 0.1
        base_feasibility -= conflict_penalty
        
        # Reduce for resource intensity
        high_resource_strategies = sum(1 for s in strategies 
                                     if any(term in str(s.get('resource_requirements', {})).lower() 
                                           for term in ['high', 'significant', 'major']))
        
        resource_penalty = high_resource_strategies * 0.1
        base_feasibility -= resource_penalty
        
        # Bonus for complementary strategies
        if len(strategies) >= 2:
            base_feasibility += 0.1
        
        return max(0.0, min(1.0, base_feasibility))
    
    def _save_strategy_bundles(self, market_cell_id: int, bundles: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Save strategy bundles to database and return with IDs"""
        saved_bundles = []
        
        with self.db.get_connection() as conn:
            for bundle in bundles:
                cursor = conn.execute("""
                    INSERT INTO strategy_bundles 
                    (market_cell_id, name, mechanism, required_enablers, expected_kpi_shifts,
                     risks, timeline_months, resource_requirements, conflict_analysis)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (
                    market_cell_id,
                    bundle['name'],
                    bundle.get('description', ''),
                    json.dumps(list({en for s in bundle.get('strategies', []) for en in s.get('required_enablers', [])})),
                    json.dumps({k: v for k, v in bundle.get('resource_summary', {}).items()}),
                    json.dumps(bundle.get('risk_assessment', [])),
                    bundle.get('estimated_timeline', 12),
                    json.dumps(bundle.get('resource_summary', {})),
                    json.dumps(bundle.get('bundle_conflicts', []))
                ))
                
                bundle['id'] = cursor.lastrowid
                saved_bundles.append(bundle)
        
        return saved_bundles
    
    def generate_all_strategy_bundles(self, company_id: int, 
                                    competitive_analyses: Dict[int, Dict[str, Any]]) -> Dict[int, List[Dict[str, Any]]]:
        """
        Generate strategy bundles for all market cells with competitive analysis.
        Returns strategy bundles by market cell ID.
        """
        thread_safe_print(f"\nGenerating strategy bundles for company {company_id}")
        
        all_bundles = {}
        
        for market_cell_id, analysis_data in competitive_analyses.items():
            if analysis_data.get('competitive_analysis', {}).get('success'):
                competitive_analysis = analysis_data['competitive_analysis']
                market_cell_name = analysis_data.get('market_cell_name', f'Market Cell {market_cell_id}')
                
                thread_safe_print(f"\nGenerating strategies for: {market_cell_name}")
                
                bundles = self.generate_strategy_bundles(market_cell_id, competitive_analysis)
                all_bundles[market_cell_id] = bundles
                
                thread_safe_print(f"  Generated {len(bundles)} strategy bundles")
        
        thread_safe_print(f"\nStrategy generation complete for {len(all_bundles)} market cells")
        return all_bundles
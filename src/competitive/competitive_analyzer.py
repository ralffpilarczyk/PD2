"""
Competitive Analysis and Scoring Framework.
Implements the 4-dimension scoring system and competitive position analysis.
"""

import json
import statistics
from typing import Dict, List, Optional, Tuple, Any
from ..utils import thread_safe_print, retry_with_backoff
from .database import CompetitiveDatabase
import google.generativeai as genai


class CompetitiveAnalyzer:
    """Analyzes competitive positioning using multi-dimensional scoring"""
    
    # Value impact mapping for scoring
    VALUE_IMPACT_WEIGHTS = {
        "margin": 1.0,      # Direct margin impact
        "roce": 0.9,        # Return on capital employed
        "growth": 0.8,      # Revenue/customer growth
        "scalability": 0.7  # Operational leverage
    }
    
    def __init__(self, db: CompetitiveDatabase, model_name: str = "gemini-2.5-flash"):
        """Initialize with database and analysis model"""
        self.db = db
        self.model_name = model_name
        self.analysis_model = genai.GenerativeModel(model_name)
    
    def calculate_competitive_scores(self, market_cell_id: int, company_id: int) -> Dict[str, Any]:
        """
        Calculate competitive scores for all metrics in a market cell.
        Returns comprehensive scoring analysis.
        """
        thread_safe_print(f"Calculating competitive scores for market cell {market_cell_id}")
        
        # Get observations for this market cell
        observations = self._get_market_cell_observations(market_cell_id, company_id)
        
        if not observations:
            thread_safe_print(f"No observations found for market cell {market_cell_id}")
            return {"success": False, "error": "No observations found"}
        
        # Group observations by metric
        metrics_data = self._group_observations_by_metric(observations)
        
        # Calculate scores for each metric
        scored_metrics = []
        for metric_id, metric_observations in metrics_data.items():
            metric_scores = self._score_metric_competitively(
                metric_id=metric_id,
                observations=metric_observations,
                company_id=company_id
            )
            if metric_scores:
                scored_metrics.append(metric_scores)
        
        if not scored_metrics:
            thread_safe_print(f"No metrics could be scored for market cell {market_cell_id}")
            return {"success": False, "error": "No metrics could be scored"}
        
        # Identify top strengths and weaknesses
        strengths, weaknesses = self._identify_strengths_weaknesses(scored_metrics)
        
        # Calculate overall competitive position
        overall_position = self._calculate_overall_position(scored_metrics)

        thread_safe_print(f"Competitive scoring complete: {len(scored_metrics)} metrics analyzed")
        
        return {
            "success": True,
            "market_cell_id": market_cell_id,
            "scored_metrics": scored_metrics,
            "top_strengths": strengths,
            "top_weaknesses": weaknesses,
            "overall_position": overall_position,
            "analysis_summary": {
                "metrics_analyzed": len(scored_metrics),
                "average_confidence": statistics.mean([m["confidence"] for m in scored_metrics]),
                "competitive_gaps_identified": len(weaknesses),
                "competitive_advantages": len(strengths)
            }
        }
    
    def _get_market_cell_observations(self, market_cell_id: int, company_id: int) -> List[Dict[str, Any]]:
        """Get all observations for a market cell with metadata"""
        with self.db.get_connection() as conn:
            query = """
                SELECT 
                    o.*,
                    m.name as metric_name,
                    m.definition as metric_definition,
                    m.capability_family,
                    m.value_impact_category,
                    m.directionality,
                    c.name as competitor_name,
                    c.parent_company,
                    c.evidence_score
                FROM observations o
                JOIN metrics m ON o.metric_id = m.id
                JOIN competitors c ON o.competitor_id = c.id
                WHERE o.market_cell_id = ?
                ORDER BY m.name, c.name
            """
            cursor = conn.execute(query, (market_cell_id,))
            return [dict(row) for row in cursor.fetchall()]
    
    def _group_observations_by_metric(self, observations: List[Dict[str, Any]]) -> Dict[int, List[Dict[str, Any]]]:
        """Group observations by metric ID"""
        grouped = {}
        for obs in observations:
            metric_id = obs['metric_id']
            if metric_id not in grouped:
                grouped[metric_id] = []
            grouped[metric_id].append(obs)
        return grouped
    
    def _score_metric_competitively(self, metric_id: int, observations: List[Dict[str, Any]], 
                                  company_id: int) -> Optional[Dict[str, Any]]:
        """
        Score a single metric using the 4-dimension framework:
        1. Differentiation: How different vs anchor
        2. Impact: Business materiality 
        3. Confidence: Data reliability
        4. Addressability: 12-month actionability
        """
        if not observations:
            return None
        
        # Get metric metadata
        metric_info = observations[0]  # All observations have same metric info
        metric_name = metric_info['metric_name']

        thread_safe_print(f"  Scoring metric: {metric_name}")
        
        try:
            # Step 1: Determine anchor (best-in-class by default)
            anchor_value, anchor_competitor = self._determine_anchor(observations)
            
            # Step 2: Calculate differentiation scores
            differentiation_scores = self._calculate_differentiation(observations, anchor_value, metric_info)
            
            # Step 3: Assess business impact
            impact_score = self._assess_business_impact(metric_info)
            
            # Step 4: Calculate confidence score
            confidence_score = self._calculate_confidence_score(observations)
            
            # Step 5: Assess addressability
            addressability_score = self._assess_addressability(metric_info, observations)
            
            # Step 6: Calculate composite priority score
            composite_score = self._calculate_composite_score(
                differentiation_scores, impact_score, confidence_score, addressability_score
            )
            
            return {
                "metric_id": metric_id,
                "metric_name": metric_name,
                "metric_definition": metric_info['metric_definition'],
                "capability_family": metric_info['capability_family'],
                "anchor_value": anchor_value,
                "anchor_competitor": anchor_competitor,
                "differentiation": differentiation_scores,
                "impact": impact_score,
                "confidence": confidence_score,
                "addressability": addressability_score,
                "composite_score": composite_score,
                "competitor_values": [
                    {
                        "competitor": obs['competitor_name'],
                        "value": obs['normalized_value'],
                        "confidence": obs['confidence_score']
                    } for obs in observations
                ],
                "directionality": metric_info['directionality']
            }
            
        except Exception as e:
            thread_safe_print(f"Error scoring {metric_name}: {e}")
            return None
    
    def _determine_anchor(self, observations: List[Dict[str, Any]]) -> Tuple[float, str]:
        """Determine anchor value (best-in-class by default)"""
        directionality = observations[0]['directionality']
        
        # Filter for high-confidence observations
        reliable_obs = [obs for obs in observations if obs['confidence_score'] >= 0.5]
        
        if not reliable_obs:
            reliable_obs = observations  # Use all if none are reliable
        
        if directionality == "higher_better":
            best_obs = max(reliable_obs, key=lambda x: x['normalized_value'])
        elif directionality == "lower_better":
            best_obs = min(reliable_obs, key=lambda x: x['normalized_value'])
        else:  # neutral - use median
            sorted_obs = sorted(reliable_obs, key=lambda x: x['normalized_value'])
            best_obs = sorted_obs[len(sorted_obs) // 2]
        
        return best_obs['normalized_value'], best_obs['competitor_name']
    
    def _calculate_differentiation(self, observations: List[Dict[str, Any]], 
                                 anchor_value: float, metric_info: Dict[str, Any]) -> Dict[str, float]:
        """Calculate differentiation scores for each competitor"""
        directionality = metric_info['directionality']
        differentiation_scores = {}
        
        for obs in observations:
            competitor = obs['competitor_name']
            value = obs['normalized_value']
            
            if anchor_value == 0:
                # Avoid division by zero
                diff_score = 0.0
            else:
                # Calculate percentage difference from anchor
                if directionality == "higher_better":
                    diff_score = (value - anchor_value) / abs(anchor_value)
                elif directionality == "lower_better":
                    diff_score = (anchor_value - value) / abs(anchor_value)
                else:  # neutral
                    diff_score = abs(value - anchor_value) / abs(anchor_value)
            
            differentiation_scores[competitor] = diff_score
        
        return differentiation_scores
    
    def _assess_business_impact(self, metric_info: Dict[str, Any]) -> float:
        """Assess business impact based on value impact category"""
        value_category = metric_info.get('value_impact_category', 'scalability')
        base_weight = self.VALUE_IMPACT_WEIGHTS.get(value_category, 0.5)
        
        # Boost score for certain capability families
        capability_family = metric_info.get('capability_family', '')
        
        if capability_family in ['customer_economics', 'scale_footprint']:
            base_weight *= 1.2  # High impact families
        elif capability_family in ['cost_position']:
            base_weight *= 1.1  # Medium-high impact
        
        return min(1.0, base_weight)  # Cap at 1.0
    
    def _calculate_confidence_score(self, observations: List[Dict[str, Any]]) -> float:
        """Calculate overall confidence based on source quality and consistency"""
        if not observations:
            return 0.0
        
        # Average confidence from individual observations
        avg_confidence = statistics.mean([obs['confidence_score'] for obs in observations])
        
        # Adjust for data consistency (lower variance = higher confidence)
        values = [obs['normalized_value'] for obs in observations if obs['normalized_value'] > 0]
        
        if len(values) > 1:
            # Calculate coefficient of variation
            mean_val = statistics.mean(values)
            std_dev = statistics.stdev(values)
            
            if mean_val > 0:
                cv = std_dev / mean_val
                consistency_bonus = max(0, 1.0 - cv)  # Lower CV = higher bonus
                avg_confidence = min(1.0, avg_confidence * (1 + consistency_bonus * 0.2))
        
        return avg_confidence
    
    def _assess_addressability(self, metric_info: Dict[str, Any], 
                             observations: List[Dict[str, Any]]) -> float:
        """Assess how addressable this metric is within 12 months"""
        capability_family = metric_info.get('capability_family', '')
        
        # Base addressability scores by capability family
        addressability_map = {
            'cost_position': 0.8,        # Usually addressable through operational changes
            'customer_economics': 0.7,   # Moderate timeline for customer-focused changes
            'quality_coverage': 0.6,     # Service quality takes time to change
            'innovation_regulatory': 0.3, # Long-term investments, regulatory delays
            'asset_intensity': 0.4,      # Capital-intensive changes take time
            'scale_footprint': 0.5       # Market position changes are medium-term
        }
        
        base_score = addressability_map.get(capability_family, 0.5)
        
        # Adjust based on metric complexity (simple assumption)
        metric_name = metric_info.get('metric_name', '').lower()
        
        if any(term in metric_name for term in ['cost', 'efficiency', 'productivity']):
            base_score *= 1.2  # Operational metrics more addressable
        elif any(term in metric_name for term in ['market share', 'brand', 'network']):
            base_score *= 0.8  # Strategic metrics less addressable short-term
        
        return min(1.0, base_score)
    
    def _calculate_composite_score(self, differentiation_scores: Dict[str, float],
                                 impact_score: float, confidence_score: float,
                                 addressability_score: float) -> Dict[str, float]:
        """Calculate composite priority scores for each competitor"""
        composite_scores = {}
        
        for competitor, diff_score in differentiation_scores.items():
            # Composite formula: weighted combination of all dimensions
            composite = (
                abs(diff_score) * 0.4 +      # Differentiation weight: 40%
                impact_score * 0.3 +          # Impact weight: 30%
                confidence_score * 0.2 +      # Confidence weight: 20%
                addressability_score * 0.1    # Addressability weight: 10%
            )
            
            composite_scores[competitor] = composite
        
        return composite_scores
    
    def _identify_strengths_weaknesses(self, scored_metrics: List[Dict[str, Any]]) -> Tuple[List[Dict], List[Dict]]:
        """Identify top 3 strengths and weaknesses based on scoring"""
        strengths = []
        weaknesses = []
        
        for metric in scored_metrics:
            metric_name = metric['metric_name']
            differentiation = metric['differentiation']
            composite_scores = metric['composite_score']
            directionality = metric['directionality']
            
            # Find best and worst performers for this metric
            if directionality == "higher_better":
                # Positive differentiation = strength, negative = weakness
                for competitor, diff_score in differentiation.items():
                    composite = composite_scores.get(competitor, 0)
                    
                    if diff_score > 0.1 and composite > 0.6:  # Significant strength
                        strengths.append({
                            "metric": metric_name,
                            "competitor": competitor,
                            "differentiation": diff_score,
                            "composite_score": composite,
                            "type": "strength"
                        })
                    elif diff_score < -0.1 and composite > 0.5:  # Significant weakness
                        weaknesses.append({
                            "metric": metric_name,
                            "competitor": competitor,
                            "differentiation": diff_score,
                            "composite_score": composite,
                            "type": "weakness"
                        })
        
        # Sort by composite score and take top 3
        strengths.sort(key=lambda x: x['composite_score'], reverse=True)
        weaknesses.sort(key=lambda x: x['composite_score'], reverse=True)
        
        return strengths[:3], weaknesses[:3]
    
    def _calculate_overall_position(self, scored_metrics: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Calculate overall competitive position summary"""
        if not scored_metrics:
            return {"error": "No metrics to analyze"}
        
        # Calculate averages
        avg_impact = statistics.mean([m['impact'] for m in scored_metrics])
        avg_confidence = statistics.mean([m['confidence'] for m in scored_metrics])
        avg_addressability = statistics.mean([m['addressability'] for m in scored_metrics])
        
        # Calculate competitive position vs anchor
        total_differentiation = 0
        metric_count = 0
        
        for metric in scored_metrics:
            for competitor, diff_score in metric['differentiation'].items():
                total_differentiation += diff_score
                metric_count += 1
        
        avg_differentiation = total_differentiation / max(1, metric_count)
        
        # Determine competitive position
        if avg_differentiation > 0.15:
            position = "Strong Competitive Position"
        elif avg_differentiation > 0.05:
            position = "Moderate Competitive Advantage"
        elif avg_differentiation > -0.05:
            position = "Competitive Parity"
        elif avg_differentiation > -0.15:
            position = "Moderate Competitive Disadvantage"
        else:
            position = "Weak Competitive Position"
        
        return {
            "position": position,
            "avg_differentiation": avg_differentiation,
            "avg_impact": avg_impact,
            "avg_confidence": avg_confidence,
            "avg_addressability": avg_addressability,
            "metrics_analyzed": len(scored_metrics),
            "competitive_score": (avg_differentiation + 1) / 2  # Normalize to 0-1
        }
    
    def analyze_all_market_cells(self, company_id: int) -> Dict[int, Dict[str, Any]]:
        """
        Run competitive analysis for all market cells of a company.
        Returns analysis results by market cell ID.
        """
        thread_safe_print(f"\nStarting competitive analysis for company {company_id}")
        
        # Get all market cells for the company
        market_cells = self.db.get_market_cells_for_company(company_id)
        
        if not market_cells:
            thread_safe_print(f"No market cells found for company {company_id}")
            return {}
        
        analysis_results = {}
        
        for market_cell_row in market_cells:
            market_cell_id = market_cell_row['id']
            market_cell_name = f"{market_cell_row['product_service']} × {market_cell_row['geography']} × {market_cell_row['customer_segment']}"
            
            thread_safe_print(f"\nAnalyzing market cell: {market_cell_name}")
            
            # Calculate competitive scores
            scores = self.calculate_competitive_scores(market_cell_id, company_id)
            
            if scores.get('success'):
                analysis_results[market_cell_id] = {
                    "market_cell_name": market_cell_name,
                    "market_cell_info": dict(market_cell_row),
                    "competitive_analysis": scores
                }
                
                # Log summary
                position = scores['overall_position'].get('position', 'Unknown')
                strengths_count = len(scores.get('top_strengths', []))
                weaknesses_count = len(scores.get('top_weaknesses', []))
                
                thread_safe_print(f"  {position}")
                thread_safe_print(f"  {strengths_count} key strengths, {weaknesses_count} key weaknesses")
            else:
                thread_safe_print(f"  Analysis failed: {scores.get('error', 'Unknown error')}")
        
        thread_safe_print(f"\nCompetitive analysis complete for {len(analysis_results)} market cells")
        return analysis_results
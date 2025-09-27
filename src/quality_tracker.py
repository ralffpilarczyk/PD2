import re
from typing import Dict

class QualityTracker:
    """Handles quality metrics calculation and tracking"""
    
    def __init__(self):
        """Initialize quality tracker"""
        self.quality_scores = {}
    
    def calculate_section_metrics(self, section_num: int, output: str):
        """Calculate insight depth and efficiency metrics for a section"""
        # Count insights (rough heuristic based on analytical language)
        insight_patterns = [
            r'indicates?',
            r'suggests?',
            r'reveals?',
            r'due to',
            r'because',
            r'driven by',
            r'results? from',
            r'implies?',
            r'therefore',
            r'consequently'
        ]
        
        insight_count = 0
        for pattern in insight_patterns:
            insight_count += len(re.findall(pattern, output, re.IGNORECASE))
        
        output_length = len(output.split())
        depth_ratio = insight_count / output_length if output_length > 0 else 0

        # Simple numeric density: numbers and percentages per 100 words
        numeric_tokens = re.findall(r'(?:\d+[\d,\.]*%?)', output)
        numeric_density = (len(numeric_tokens) / output_length * 100) if output_length > 0 else 0

        # Simple table presence/count: count lines that look like table rows
        table_rows = sum(1 for line in output.split('\n') if '|' in line and line.strip().startswith('|'))
        has_table = table_rows > 0
        
        self.quality_scores[f"section_{section_num}"] = {
            "insights_count": insight_count,
            "output_length": output_length,
            "depth_ratio": depth_ratio,
            "numeric_density_per_100w": numeric_density,
            "has_table": has_table,
            "table_row_count": table_rows
        }
    
    def get_quality_scores(self) -> Dict:
        """Get current quality scores"""
        return self.quality_scores.copy()
    
 
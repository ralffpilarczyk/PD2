"""
Utility functions for DDAR application
"""
import re
from typing import Optional, Union, Any
from pathlib import Path

def extract_company_name(filename: str) -> str:
    """Extract company name from filename."""
    # Remove extension and path
    base = Path(filename).stem
    
    # Common patterns
    patterns = [
        r'^([A-Za-z]+(?:\s+[A-Za-z]+)*)',  # Leading words
        r'^([A-Za-z]+)',  # First word
    ]
    
    for pattern in patterns:
        match = re.match(pattern, base)
        if match:
            return match.group(1).strip()
    
    # Fallback: use first word
    parts = base.split()
    return parts[0] if parts else "Unknown"

def safe_divide(numerator: Union[int, float], denominator: Union[int, float], 
                default: float = 0.0) -> float:
    """Safely divide two numbers, returning default if division by zero."""
    try:
        if denominator == 0:
            return default
        return float(numerator) / float(denominator)
    except (TypeError, ValueError, ZeroDivisionError):
        return default

def format_number(value: Union[int, float], decimals: int = 2, 
                  as_millions: bool = False) -> str:
    """Format a number for display."""
    try:
        num = float(value)
        if as_millions:
            num = num / 1_000_000
            return f"${num:,.{decimals}f}M"
        return f"{num:,.{decimals}f}"
    except (TypeError, ValueError):
        return str(value)

def format_percentage(value: Union[int, float], decimals: int = 1) -> str:
    """Format a number as percentage."""
    try:
        return f"{float(value) * 100:.{decimals}f}%"
    except (TypeError, ValueError):
        return str(value)

def clean_metric_name(metric: str) -> str:
    """Clean and standardize metric names."""
    replacements = {
        'npm': 'net_margin',
        'at': 'asset_turnover',
        'fl': 'financial_leverage',
        'de_ratio': 'debt_to_equity',
        'coverage_val': 'interest_coverage',
        'margin_val': 'margin'
    }
    
    cleaned = metric.lower().strip()
    return replacements.get(cleaned, cleaned)

def validate_metric_range(value: float, metric_name: str, 
                         ranges: dict) -> bool:
    """Validate if a metric value is within expected range."""
    if metric_name not in ranges:
        return True  # No range defined, assume valid
    
    min_val, max_val = ranges[metric_name]
    # Allow some flexibility (10% outside range)
    min_threshold = min_val * 0.9
    max_threshold = max_val * 1.1
    
    return min_threshold <= value <= max_threshold

def get_period_priority(period: str) -> int:
    """Get priority for period sorting (higher = more recent)."""
    # Extract year and quarter if present
    year_match = re.search(r'20(\d{2})', period)
    quarter_match = re.search(r'Q(\d)', period)
    
    if not year_match:
        return 0
    
    year = int(year_match.group(1))
    base_score = year * 100
    
    if quarter_match:
        quarter = int(quarter_match.group(1))
        return base_score + quarter
    
    # Full year gets priority 5 (after Q4)
    return base_score + 5

def deduplicate_facts(facts: list) -> list:
    """Remove duplicate facts, keeping the most recent."""
    fact_map = {}
    
    for fact in facts:
        key = (fact.get('company'), fact.get('key'))
        period_priority = get_period_priority(fact.get('period_label', ''))
        
        if key not in fact_map or period_priority > fact_map[key][0]:
            fact_map[key] = (period_priority, fact)
    
    return [fact for _, fact in fact_map.values()]

class DDARError(Exception):
    """Base exception for DDAR application."""
    pass

class DataExtractionError(DDARError):
    """Error during data extraction."""
    pass

class CalculationError(DDARError):
    """Error during metric calculation."""
    pass

class PrologError(DDARError):
    """Error in Prolog processing."""
    pass
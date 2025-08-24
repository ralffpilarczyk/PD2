"""
Canonicalizer - Deterministic number normalization for DDAR
Ensures all numbers are represented consistently with 4 decimal places
"""

from decimal import Decimal, ROUND_HALF_EVEN
import hashlib
import re

def canonicalize_number(value_str):
    """
    Convert various number formats to canonical 4 decimal places
    
    Examples:
        "2,454,216" → 2454216.0000
        "(1,516,152)" → -1516152.0000
        "48.2%" → 0.4820
        "1.75B" → 1750000000.0000
    """
    if not value_str or value_str == 'N/A' or value_str == 'NM':
        return None
    
    # Convert to string if not already
    value_str = str(value_str).strip()
    
    # Handle parentheses as negative
    is_negative = False
    if value_str.startswith('(') and value_str.endswith(')'):
        is_negative = True
        value_str = value_str[1:-1]
    
    # Remove currency symbols
    value_str = re.sub(r'[$€£¥₹RM]', '', value_str)
    
    # Remove commas (thousands separator)
    value_str = value_str.replace(',', '')
    
    # Handle percentages
    is_percentage = False
    if '%' in value_str:
        is_percentage = True
        value_str = value_str.replace('%', '')
    
    # Handle multipliers
    multiplier = Decimal('1')
    value_lower = value_str.lower()
    
    if 'trillion' in value_lower or value_str.endswith('T'):
        multiplier = Decimal('1e12')
        value_str = re.sub(r'[Tt]rillion|T', '', value_str)
    elif 'billion' in value_lower or value_str.endswith('B'):
        multiplier = Decimal('1e9')
        value_str = re.sub(r'[Bb]illion|B', '', value_str)
    elif 'million' in value_lower or value_str.endswith('M'):
        multiplier = Decimal('1e6')
        value_str = re.sub(r'[Mm]illion|M', '', value_str)
    elif 'thousand' in value_lower or value_str.endswith('K'):
        multiplier = Decimal('1e3')
        value_str = re.sub(r'[Tt]housand|K', '', value_str)
    
    # Extract numeric value
    value_str = value_str.strip()
    try:
        value = Decimal(value_str)
        
        # Apply percentage conversion
        if is_percentage:
            value = value / Decimal('100')
        
        # Apply multiplier
        value = value * multiplier
        
        # Apply negative sign
        if is_negative:
            value = -value
        
        # Quantize to 4 decimal places with banker's rounding
        return value.quantize(Decimal('0.0001'), rounding=ROUND_HALF_EVEN)
    except:
        return None

def generate_fact_id(company, key, period, value):
    """
    Generate deterministic fact ID using SHA256 hash
    
    Args:
        company: Company name
        key: Metric key
        period: Period string
        value: Canonicalized value
    
    Returns:
        16-character hex string
    """
    # Create deterministic string representation
    fact_string = f"{company}|{key}|{period}|{value}"
    
    # Generate SHA256 hash
    hash_obj = hashlib.sha256(fact_string.encode('utf-8'))
    
    # Return first 16 characters of hex digest
    return hash_obj.hexdigest()[:16]

def format_for_prolog(value):
    """
    Format a Decimal value for Prolog (as float with 4 decimals)
    """
    if value is None:
        return 'null'
    return f"{float(value):.4f}"

# Test the canonicalizer
if __name__ == "__main__":
    test_cases = [
        ("2,454,216", 2454216.0000),
        ("(1,516,152)", -1516152.0000),
        ("48.2%", 0.4820),
        ("1.75B", 1750000000.0000),
        ("$450M", 450000000.0000),
        ("RM 1,234.56", 1234.5600),
        ("N/A", None),
    ]
    
    print("Testing canonicalizer:")
    for input_val, expected in test_cases:
        result = canonicalize_number(input_val)
        status = "✓" if result == expected else "✗"
        print(f"{status} {input_val:20} → {result:20} (expected: {expected})")
    
    # Test fact ID generation
    fact_id = generate_fact_id("Axiata", "ebitda", "Q1 2025", "2454.2160")
    print(f"\nSample fact ID: {fact_id}")
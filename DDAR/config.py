"""
Configuration settings for DDAR application
"""

# File processing settings
MAX_FILE_SIZE_MB = 100
SUPPORTED_FORMATS = ['.md', '.txt']
SOURCE_DIR = '../SourceFiles'
OUTPUT_DIR = 'output'

# Calculation settings
DEFAULT_WACC = 0.10
DEFAULT_TAX_RATE = 0.25
MIN_CONFIDENCE_THRESHOLD = 0.70

# Prolog settings
PROLOG_TIMEOUT = 30
MAX_ITERATIONS = 10
CONVERGENCE_THRESHOLD = 0.01

# Metric validation ranges (in millions for monetary values)
METRIC_RANGES = {
    'revenue': (100, 100000),
    'total_assets': (1000, 500000),
    'total_equity': (500, 200000),
    'operating_cash_flow': (-10000, 50000),
    'net_income': (-5000, 20000),
    'ebitda': (-5000, 50000),
    'ebit': (-5000, 30000),
    'capex': (100, 20000),
    'total_debt': (100, 100000),
    'cash': (10, 20000),
    'working_capital': (-10000, 50000),
    'current_assets': (100, 100000),
    'current_liabilities': (100, 100000),
    'inventory': (0, 50000),
    'receivables': (0, 50000),
    'payables': (0, 50000),
    'depreciation': (10, 10000),
    'interest_expense': (10, 5000),
    'tax_expense': (-1000, 10000),
}

# Industry benchmarks for analysis
INDUSTRY_BENCHMARKS = {
    'telecom': {
        'roe': (0.10, 0.15),
        'debt_to_equity': (0.8, 1.2),
        'net_margin': (0.08, 0.12),
        'interest_coverage': (2.5, 5.0),
        'current_ratio': (0.8, 1.2)
    },
    'default': {
        'roe': (0.08, 0.15),
        'debt_to_equity': (0.5, 1.5),
        'net_margin': (0.05, 0.15),
        'interest_coverage': (2.0, 5.0),
        'current_ratio': (1.0, 2.0)
    }
}

# Report generation settings
REPORT_TEMPLATE_STYLE = 'enhanced'  # 'basic' or 'enhanced'
INCLUDE_SENSITIVITY_ANALYSIS = True
INCLUDE_CHAIN_OF_THOUGHT = True
MAX_CONCLUSIONS_PER_THEOREM = 10

# Logging settings
LOG_LEVEL = 'INFO'  # DEBUG, INFO, WARNING, ERROR
LOG_FILE = 'ddar.log'
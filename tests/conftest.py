import pytest
from unittest.mock import patch, MagicMock


# ---------------------------------------------------------------------------
# Table fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def clean_table_3col():
    return (
        "| Name | Revenue | Growth |\n"
        "| :--- | :--- | :--- |\n"
        "| Acme | 100M | 5% |\n"
        "| Beta | 200M | 10% |"
    )


@pytest.fixture
def corrupted_separator_table():
    header = "| Name | Revenue | Growth |"
    separator = "| " + "-" * 80 + " |"
    row = "| Acme | 100M | 5% |"
    return f"{header}\n{separator}\n{row}"


@pytest.fixture
def long_cell_table():
    long_text = "x" * 1100
    return (
        f"| Header1 | Header2 |\n"
        f"| :--- | :--- |\n"
        f"| {long_text} | short |"
    )


@pytest.fixture
def wide_table_12col():
    headers = "| " + " | ".join(f"Col{i}" for i in range(12)) + " |"
    sep = "| " + " | ".join(":---" for _ in range(12)) + " |"
    row = "| " + " | ".join(f"v{i}" for i in range(12)) + " |"
    return f"{headers}\n{sep}\n{row}"


@pytest.fixture
def tall_table_25rows():
    headers = "| Name | Value |"
    sep = "| :--- | :--- |"
    rows = "\n".join(f"| item{i} | {i} |" for i in range(25))
    return f"{headers}\n{sep}\n{rows}"


@pytest.fixture
def table_with_pipes_in_cells():
    return (
        "| Expression | Result |\n"
        "| :--- | :--- |\n"
        "| a \\| b | true |"
    )


# ---------------------------------------------------------------------------
# Markdown / HTML wrapper fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def markdown_wrapped_content():
    return "```markdown\nSome **bold** content here.\n```"


@pytest.fixture
def html_wrapped_content():
    return "```html\n<div>Content</div>\n```"


@pytest.fixture
def content_with_section_header():
    return "# Section 7: Summary Financials\nActual content here."


# ---------------------------------------------------------------------------
# Footnote fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def two_section_markdown_with_footnotes():
    return (
        'Preamble text.\n\n'
        '<a id="section_1"></a>\n\n'
        '# Section 1.1: Operating Footprint\n\n'
        'Revenue grew[^1] while costs fell[^2].\n\n'
        '[^1]: Annual Report p.12\n'
        '[^2]: Annual Report p.15\n\n'
        '---\n\n'
        '<a id="section_7"></a>\n\n'
        '# Section 1.7: Summary Financials\n\n'
        'EBITDA rose[^1] and capex was flat[^2].\n\n'
        '[^1]: Q3 Filing p.4\n'
        '[^2]: Q3 Filing p.5\n'
    )


# ---------------------------------------------------------------------------
# Section 33 parsing fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def layer1_three_patterns():
    return (
        "1. PATTERN: Revenue vs Volume Divergence\n"
        "METRIC A: Revenue: $500M (FY24) - Direction: Up 15%\n"
        "METRIC B: Volume: 1.2M units (FY24) - Direction: Down 12%\n"
        "PERSISTENCE: 3 periods\n"
        "WHY MATERIAL: Price masking volume decline\n\n"
        "2. PATTERN: Capex vs Depreciation Gap\n"
        "METRIC A: Capex: $450M (FY24)\n"
        "METRIC B: Depreciation: $900M (FY24)\n"
        "PERSISTENCE: 2 periods\n"
        "WHY MATERIAL: Underinvestment risk\n\n"
        "3. PATTERN: Profit vs Cash Flow Disconnect\n"
        "METRIC A: Net Profit: +20% (FY24)\n"
        "METRIC B: Operating Cash Flow: -5% (FY24)\n"
        "PERSISTENCE: 4 periods\n"
        "WHY MATERIAL: Earnings quality question\n"
    )


@pytest.fixture
def layer1_only_two_patterns():
    return (
        "1. PATTERN: Revenue vs Volume Divergence\n"
        "METRIC A: Revenue: $500M (FY24) - Direction: Up 15%\n\n"
        "2. PATTERN: Capex vs Depreciation Gap\n"
        "METRIC A: Capex: $450M (FY24)\n"
    )


@pytest.fixture
def layer2_three_explanations():
    return (
        "EXPLANATION 1\n"
        "NAME: Price Increase Masking\n"
        "MECHANISM: Company raised prices to offset volume declines\n"
        "PREDICTION A: ASP should show increase\n"
        "PREDICTION B: Volume decline in disclosures\n"
        "PREDICTION C: Customer churn rising\n\n"
        "EXPLANATION 2\n"
        "NAME: Mix Shift to Premium\n"
        "MECHANISM: Revenue growing from higher-value product segments\n"
        "PREDICTION A: Premium segment revenue up\n"
        "PREDICTION B: Basic segment revenue flat or down\n"
        "PREDICTION C: Margins improving\n\n"
        "EXPLANATION 3\n"
        "NAME: Accounting Reclassification\n"
        "MECHANISM: Revenue recognition change inflating top line\n"
        "PREDICTION A: Policy change disclosed\n"
        "PREDICTION B: Working capital building\n"
        "PREDICTION C: Cash conversion declining\n"
    )


@pytest.fixture
def layer2_only_one_explanation():
    return (
        "EXPLANATION 1\n"
        "NAME: Price Increase Masking\n"
        "MECHANISM: Company raised prices to offset volume declines\n"
    )


@pytest.fixture
def layer3_with_verdict():
    return (
        "PREDICTION A:\n"
        "EVIDENCE FOUND: ASP increased 12% per annual report p.45\n"
        "DIRECTION: Supports\n\n"
        "OVERALL VERDICT: Strongly supported. Evidence clearly shows pricing offset."
    )


@pytest.fixture
def layer3_without_verdict():
    return (
        "PREDICTION A:\n"
        "EVIDENCE FOUND: Some text here\n"
        "DIRECTION: Ambiguous\n"
    )


# ---------------------------------------------------------------------------
# Mocked instances
# ---------------------------------------------------------------------------

@pytest.fixture
def core_analyzer():
    with patch("google.generativeai.GenerativeModel") as mock_model_cls:
        mock_model_cls.return_value = MagicMock()
        from src.core_analyzer import CoreAnalyzer
        analyzer = CoreAnalyzer.__new__(CoreAnalyzer)
        analyzer.run_timestamp = "2025_09_05_16_23_45"
        analyzer.model_name = "gemini-3-flash-preview"
        analyzer.pdf_parts = []
        analyzer.cached_model_low_temp = None
        analyzer.cached_model_medium_temp = None
        analyzer.cached_model_high_temp = None
        analyzer.model_low_temp = MagicMock()
        analyzer.model_medium_temp = MagicMock()
        analyzer.model_high_temp = MagicMock()
        return analyzer


@pytest.fixture
def profile_generator():
    with patch("google.generativeai.GenerativeModel") as mock_model_cls:
        mock_model_cls.return_value = MagicMock()
        from src.profile_generator import ProfileGenerator
        gen = ProfileGenerator.__new__(ProfileGenerator)
        gen.run_timestamp = "2025_09_05_16_23_45"
        gen.model_name = "gemini-3-flash-preview"
        gen.model = MagicMock()
        return gen

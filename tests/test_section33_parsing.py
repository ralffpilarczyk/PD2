import re
from unittest.mock import patch, MagicMock


class TestSection33ParsePatterns:

    def _parser(self):
        with patch("google.generativeai.GenerativeModel"):
            from src.core_analyzer import CoreAnalyzer
            analyzer = CoreAnalyzer.__new__(CoreAnalyzer)
            return analyzer._section33_parse_patterns

    def test_parses_three_patterns(self, layer1_three_patterns):
        parse = self._parser()
        result = parse(layer1_three_patterns)
        assert len(result) == 3
        for p in result:
            assert "raw" in p
            assert "name" in p

    def test_pattern_names_extracted(self, layer1_three_patterns):
        parse = self._parser()
        result = parse(layer1_three_patterns)
        names = [p["name"] for p in result]
        assert "Revenue vs Volume Divergence" in names[0]
        assert "Capex vs Depreciation Gap" in names[1]
        assert "Profit vs Cash Flow Disconnect" in names[2]

    def test_pads_to_three_when_fewer(self, layer1_only_two_patterns):
        parse = self._parser()
        result = parse(layer1_only_two_patterns)
        assert len(result) == 3
        assert "Unable to identify" in result[2]["raw"]

    def test_truncates_to_three_when_more(self):
        parse = self._parser()
        text = "\n".join(
            f"{i}. PATTERN: Pattern {i}\nMETRIC A: Value\n" for i in range(1, 6)
        )
        result = parse(text)
        assert len(result) == 3

    def test_handles_markdown_bold_numbers(self):
        parse = self._parser()
        text = (
            "**1.** PATTERN: Bold One\nMETRIC A: X\n\n"
            "**2.** PATTERN: Bold Two\nMETRIC A: Y\n\n"
            "**3.** PATTERN: Bold Three\nMETRIC A: Z\n"
        )
        result = parse(text)
        assert len(result) == 3
        assert "Bold One" in result[0]["name"]

    def test_handles_markdown_header_prefixes(self):
        parse = self._parser()
        text = (
            "### 1. PATTERN: Header One\nMETRIC A: X\n\n"
            "### 2. PATTERN: Header Two\nMETRIC A: Y\n\n"
            "### 3. PATTERN: Header Three\nMETRIC A: Z\n"
        )
        result = parse(text)
        assert len(result) == 3
        assert "Header One" in result[0]["name"]

    def test_empty_input(self):
        parse = self._parser()
        result = parse("")
        assert len(result) == 3
        for p in result:
            assert "Unable to identify" in p["raw"]


class TestSection33ParseExplanations:

    def _parser(self):
        with patch("google.generativeai.GenerativeModel"):
            from src.core_analyzer import CoreAnalyzer
            analyzer = CoreAnalyzer.__new__(CoreAnalyzer)
            return analyzer._section33_parse_explanations

    def test_parses_three_explanations(self, layer2_three_explanations):
        parse = self._parser()
        result = parse(layer2_three_explanations)
        assert len(result) == 3

    def test_explanation_names_extracted(self, layer2_three_explanations):
        parse = self._parser()
        result = parse(layer2_three_explanations)
        assert result[0]["name"] == "Price Increase Masking"
        assert result[1]["name"] == "Mix Shift to Premium"
        assert result[2]["name"] == "Accounting Reclassification"

    def test_pads_to_three_when_fewer(self, layer2_only_one_explanation):
        parse = self._parser()
        result = parse(layer2_only_one_explanation)
        assert len(result) == 3

    def test_truncates_to_three_when_more(self):
        parse = self._parser()
        text = "\n".join(
            f"EXPLANATION {i}\nNAME: Exp {i}\nMECHANISM: Something\n"
            for i in range(1, 6)
        )
        result = parse(text)
        assert len(result) == 3

    def test_handles_bold_name_format(self):
        parse = self._parser()
        text = (
            "EXPLANATION 1\n"
            "**NAME**: **Bold Name**\n"
            "MECHANISM: Something\n\n"
            "EXPLANATION 2\n"
            "NAME: Normal Name\n\n"
            "EXPLANATION 3\n"
            "NAME: Another Name\n"
        )
        result = parse(text)
        assert "**" not in result[0]["name"]
        assert "Bold Name" in result[0]["name"]

    def test_empty_input(self):
        parse = self._parser()
        result = parse("")
        assert len(result) == 3


class TestSection33ExtractVerdict:

    def _extractor(self):
        with patch("google.generativeai.GenerativeModel"):
            from src.core_analyzer import CoreAnalyzer
            analyzer = CoreAnalyzer.__new__(CoreAnalyzer)
            return analyzer._section33_extract_verdict

    def test_extracts_verdict_text(self, layer3_with_verdict):
        extract = self._extractor()
        result = extract(layer3_with_verdict)
        assert "Strongly supported" in result

    def test_missing_verdict_returns_fallback(self, layer3_without_verdict):
        extract = self._extractor()
        result = extract(layer3_without_verdict)
        assert "Unable to determine" in result

    def test_handles_bold_verdict_format(self):
        extract = self._extractor()
        text = "**OVERALL VERDICT**: Supported. Clear evidence from filings."
        result = extract(text)
        assert "Supported" in result

    def test_multiline_verdict(self):
        extract = self._extractor()
        text = (
            "OVERALL VERDICT: Supported. Evidence shows pricing offset "
            "across three quarters with documented ASP increases.\n\n"
            "Additional notes follow."
        )
        result = extract(text)
        assert "Supported" in result
        assert "ASP increases" in result
        assert "Additional notes" not in result


class TestSection33FinalAssembly:

    def test_output_has_three_pattern_subsections(self, core_analyzer, layer1_three_patterns):
        patterns = core_analyzer._section33_parse_patterns(layer1_three_patterns)
        output = ""
        for i, p in enumerate(patterns):
            name = p.get("name", f"Pattern {i+1}")
            name = re.sub(r"^\*+|\*+$", "", name).strip()
            output += f"### Pattern {i+1}: {name}\n\nSummary text.\n\n"
        assert "### Pattern 1:" in output
        assert "### Pattern 2:" in output
        assert "### Pattern 3:" in output

    def test_pattern_names_cleaned_of_asterisks(self, core_analyzer, layer1_three_patterns):
        patterns = core_analyzer._section33_parse_patterns(layer1_three_patterns)
        for p in patterns:
            name = p.get("name", "")
            name = re.sub(r"^\*+|\*+$", "", name).strip()
            assert "**" not in name

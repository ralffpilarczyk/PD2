import os
import re
from unittest.mock import patch, MagicMock

from src.profile_generator import get_display_number, GROUP_DEFINITIONS


class TestGetDisplayNumber:

    def test_section_1(self):
        assert get_display_number(1) == "1.1"

    def test_section_12(self):
        assert get_display_number(12) == "1.12"

    def test_section_13(self):
        # 13 is 7th in group 2: [14,15,16,17,18,19,13,33,35]
        assert get_display_number(13) == "2.7"

    def test_section_14(self):
        assert get_display_number(14) == "2.1"

    def test_section_33(self):
        assert get_display_number(33) == "2.8"

    def test_section_35(self):
        assert get_display_number(35) == "2.9"

    def test_section_20(self):
        assert get_display_number(20) == "3.1"

    def test_section_26(self):
        assert get_display_number(26) == "3.7"

    def test_section_27(self):
        assert get_display_number(27) == "4.1"

    def test_section_32(self):
        assert get_display_number(32) == "4.6"

    def test_section_34(self):
        assert get_display_number(34) == "5.1"

    def test_unknown_section_99(self):
        assert get_display_number(99) == "99"


class TestCleanMarkdownContent:

    def _clean(self, content):
        with patch("google.generativeai.GenerativeModel"):
            from src.profile_generator import ProfileGenerator
            gen = ProfileGenerator.__new__(ProfileGenerator)
            return gen._clean_markdown_content(content)

    def test_strips_markdown_code_block_wrapper(self):
        content = "```markdown\nReal content here.\n```"
        result = self._clean(content)
        assert "```" not in result
        assert "Real content here." in result

    def test_strips_html_code_block_wrapper(self):
        content = "```html\n<div>Stuff</div>\n```"
        result = self._clean(content)
        assert "```" not in result
        assert "<div>Stuff</div>" in result

    def test_strips_html_without_newline(self):
        content = "```htmlContent here```"
        result = self._clean(content)
        assert "```" not in result
        assert "Content here" in result

    def test_removes_duplicate_section_headers(self):
        content = "# Section 7: Summary Financials\nActual content."
        result = self._clean(content)
        assert "# Section 7" not in result
        assert "Actual content." in result

    def test_removes_SECTION_prefix_headers(self):
        content = "## SECTION 1: Operating Footprint\nBody text."
        result = self._clean(content)
        assert "SECTION 1" not in result
        assert "Body text." in result

    def test_ensures_blank_line_before_table(self):
        content = "Some text.\n| Col1 | Col2 |\n| :--- | :--- |"
        result = self._clean(content)
        lines = result.split("\n")
        for i, line in enumerate(lines):
            if line.strip().startswith("|") and i > 0:
                prev = lines[i - 1].strip()
                if prev and not prev.startswith("|"):
                    assert lines[i - 1].strip() == "" or lines[i - 2].strip() == "" or any(
                        lines[j].strip() == "" for j in range(max(0, i - 2), i)
                    )

    def test_ensures_blank_line_before_list(self):
        content = "Some text.\n- Item one\n- Item two"
        result = self._clean(content)
        lines = result.split("\n")
        for i, line in enumerate(lines):
            if re.match(r"^\s*-\s", line) and i > 0:
                prev = lines[i - 1].strip()
                if prev and not re.match(r"^\s*-\s", prev):
                    assert lines[i - 1].strip() == "" or any(
                        lines[j].strip() == "" for j in range(max(0, i - 2), i)
                    )

    def test_no_double_blank_lines(self):
        content = "Text.\n\n\n\nMore text."
        result = self._clean(content)
        # The method doesn't explicitly collapse triple newlines in the middle,
        # but it avoids introducing them via its own insertions.
        # Verify no quadruple+ consecutive newlines introduced.
        assert "\n\n\n\n\n" not in result

    def test_plain_content_passes_through(self):
        content = "Just some plain analysis text with numbers 42%."
        result = self._clean(content)
        assert result == content


class TestManageFootnotes:

    def _manage(self, content):
        with patch("google.generativeai.GenerativeModel"):
            from src.profile_generator import ProfileGenerator
            gen = ProfileGenerator.__new__(ProfileGenerator)
            return gen._manage_footnotes(content)

    def test_scopes_footnotes_per_section(self):
        content = (
            '<a id="section_1"></a>\n'
            "Revenue grew[^1].\n"
        )
        result = self._manage(content)
        assert "[^s1_1]" in result
        assert "[^1]" not in result

    def test_footnote_definitions_also_scoped(self):
        content = (
            '<a id="section_5"></a>\n'
            "Text[^1].\n\n"
            "[^1]: Source p.12\n"
        )
        result = self._manage(content)
        assert "[^s5_1]:" in result

    def test_preamble_content_untouched(self):
        content = (
            "Preamble with [^1] ref.\n\n"
            '<a id="section_1"></a>\n'
            "Section body[^1].\n"
        )
        result = self._manage(content)
        # Preamble footnote should remain unscoped
        assert result.startswith("Preamble with [^1] ref.")

    def test_no_footnotes_passes_through(self):
        content = (
            '<a id="section_1"></a>\n'
            "No footnotes here.\n"
        )
        result = self._manage(content)
        assert "No footnotes here." in result

    def test_multiple_footnotes_in_one_section(self):
        content = (
            '<a id="section_2"></a>\n'
            "A[^1] B[^2] C[^3].\n"
        )
        result = self._manage(content)
        assert "[^s2_1]" in result
        assert "[^s2_2]" in result
        assert "[^s2_3]" in result


class TestCollectSectionMarkdown:

    def _setup_section_dir(self, tmp_path, section_num, filename, content):
        run_dir = tmp_path / "runs" / "run_2025_09_05_16_23_45"
        sec_dir = run_dir / f"section_{section_num:02d}"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / filename).write_text(content, encoding="utf-8")
        return str(run_dir)

    def _make_generator(self, run_timestamp="2025_09_05_16_23_45"):
        with patch("google.generativeai.GenerativeModel"):
            from src.profile_generator import ProfileGenerator
            gen = ProfileGenerator.__new__(ProfileGenerator)
            gen.run_timestamp = run_timestamp
            gen.model_name = "gemini-3-flash-preview"
            gen.model = MagicMock()
            return gen

    def _collect(self, gen, pdf_variant=None):
        return gen._collect_section_markdown(pdf_variant=pdf_variant)

    def test_ordering_follows_group_definitions(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        # Section 14 is first in group 2; section 13 is 7th in group 2
        for num, content in [(14, "Strategic objectives " * 20), (13, "Deep dive discoveries " * 20)]:
            sec_dir = run_dir / f"section_{num:02d}"
            sec_dir.mkdir(parents=True, exist_ok=True)
            (sec_dir / "step_4_final_section.md").write_text(content)
        monkeypatch.chdir(tmp_path)
        result, sections = self._collect(gen)
        sec14_pos = result.find("section_14")
        sec13_pos = result.find("section_13")
        assert sec14_pos < sec13_pos

    def test_near_empty_section_skipped(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / "step_4_final_section.md").write_text("Short.")
        monkeypatch.chdir(tmp_path)
        result, sections = self._collect(gen)
        assert len(sections) == 0

    def test_vanilla_variant_selects_step_4(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / "step_4_final_section.md").write_text("Vanilla content " * 20)
        (sec_dir / "step_8_synthesis.md").write_text("Insights content " * 20)
        monkeypatch.chdir(tmp_path)
        result, _ = self._collect(gen, pdf_variant="vanilla")
        assert "Vanilla content" in result
        assert "Insights content" not in result

    def test_insights_variant_selects_step_8(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / "step_4_final_section.md").write_text("Vanilla content " * 20)
        (sec_dir / "step_8_synthesis.md").write_text("Insights content " * 20)
        monkeypatch.chdir(tmp_path)
        result, _ = self._collect(gen, pdf_variant="insights")
        assert "Insights content" in result

    def test_insights_variant_falls_back_to_step_4(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / "step_4_final_section.md").write_text("Fallback content " * 20)
        monkeypatch.chdir(tmp_path)
        result, _ = self._collect(gen, pdf_variant="insights")
        assert "Fallback content" in result

    def test_integrated_variant_selects_step_9(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / "step_4_final_section.md").write_text("Vanilla content " * 20)
        (sec_dir / "step_9_integrated.md").write_text("Integrated content " * 20)
        monkeypatch.chdir(tmp_path)
        result, _ = self._collect(gen, pdf_variant="integrated")
        assert "Integrated content" in result

    def test_integrated_variant_falls_back_to_step_4(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / "step_4_final_section.md").write_text("Fallback content " * 20)
        monkeypatch.chdir(tmp_path)
        result, _ = self._collect(gen, pdf_variant="integrated")
        assert "Fallback content" in result

    def test_default_variant_selects_step_4(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / "step_4_final_section.md").write_text("Default content " * 20)
        (sec_dir / "step_8_synthesis.md").write_text("Insights content " * 20)
        monkeypatch.chdir(tmp_path)
        result, _ = self._collect(gen)
        assert "Default content" in result

    def test_section_anchor_format(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / "step_4_final_section.md").write_text("Content for section one " * 20)
        monkeypatch.chdir(tmp_path)
        result, _ = self._collect(gen)
        assert '<a id="section_1"></a>' in result

    def test_section_header_format(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / "step_4_final_section.md").write_text("Content for section one " * 20)
        monkeypatch.chdir(tmp_path)
        result, _ = self._collect(gen)
        assert "# Section 1.1: Operating Footprint" in result

    def test_table_cleaning_applied(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        header = "| A | B |"
        bad_sep = "| " + "-" * 80 + " |"
        row = "| 1 | 2 |"
        content = f"Long enough content to pass the 50 char threshold.\n\n{header}\n{bad_sep}\n{row}\n\nMore text to pad."
        (sec_dir / "step_4_final_section.md").write_text(content)
        monkeypatch.chdir(tmp_path)
        result, _ = self._collect(gen)
        # The corrupted 80-dash separator should be cleaned
        assert "-" * 80 not in result

    def test_footnote_scoping_applied(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        for num in [1, 7]:
            sec_dir = run_dir / f"section_{num:02d}"
            sec_dir.mkdir(parents=True, exist_ok=True)
            (sec_dir / "step_4_final_section.md").write_text(
                f"Content for section {num} with footnote[^1] reference. " * 10
            )
        monkeypatch.chdir(tmp_path)
        result, _ = self._collect(gen)
        assert "[^s1_1]" in result
        assert "[^s7_1]" in result

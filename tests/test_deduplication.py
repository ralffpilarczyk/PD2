import os
from unittest.mock import patch, MagicMock

with patch("google.generativeai.GenerativeModel"):
    from src.core_analyzer import CoreAnalyzer
    from src.profile_generator import ProfileGenerator


class TestParseDedupOutput:

    def _make_analyzer(self):
        with patch("google.generativeai.GenerativeModel"):
            analyzer = CoreAnalyzer.__new__(CoreAnalyzer)
            return analyzer

    def test_extracts_sections_correctly(self):
        analyzer = self._make_analyzer()
        raw = (
            "=== SECTION 1: Operating Footprint ===\n"
            "Cleaned content for section 1.\n"
            "=== END SECTION 1 ===\n\n"
            "=== SECTION 5: Key Competitors ===\n"
            "Cleaned content for section 5.\n"
            "=== END SECTION 5 ===\n"
        )
        result = analyzer._parse_dedup_output(raw, [1, 5])
        assert result[1] == "Cleaned content for section 1."
        assert result[5] == "Cleaned content for section 5."

    def test_returns_empty_for_malformed_output(self):
        analyzer = self._make_analyzer()
        raw = "This is just random text with no markers."
        result = analyzer._parse_dedup_output(raw, [1, 5])
        assert result == {}

    def test_ignores_unexpected_section_numbers(self):
        analyzer = self._make_analyzer()
        raw = (
            "=== SECTION 99: Unknown ===\n"
            "Some content.\n"
            "=== END SECTION 99 ===\n"
        )
        result = analyzer._parse_dedup_output(raw, [1, 5])
        assert 99 not in result

    def test_handles_multiline_content(self):
        analyzer = self._make_analyzer()
        raw = (
            "=== SECTION 3: Key Customers ===\n"
            "Line one.\n\n"
            "Line two with data.\n\n"
            "| Col | Val |\n"
            "| :--- | :--- |\n"
            "| A | 1 |\n"
            "=== END SECTION 3 ===\n"
        )
        result = analyzer._parse_dedup_output(raw, [3])
        assert "Line one." in result[3]
        assert "| Col | Val |" in result[3]

    def test_handles_extra_whitespace_in_markers(self):
        analyzer = self._make_analyzer()
        raw = (
            "===  SECTION 2 : Products ===\n"
            "Content here.\n"
            "===  END SECTION 2  ===\n"
        )
        result = analyzer._parse_dedup_output(raw, [2])
        assert 2 in result


class TestDedupSafetyChecks:

    def _make_analyzer(self):
        with patch("google.generativeai.GenerativeModel"):
            analyzer = CoreAnalyzer.__new__(CoreAnalyzer)
            analyzer.model_low_temp = MagicMock()
            analyzer.DEDUP_READ_ONLY_SECTIONS = {33, 35}
            analyzer.DEDUP_EXCLUDED_SECTIONS = {34}
            return analyzer

    def test_rejects_output_too_short(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        from src.file_manager import FileManager
        fm = FileManager("2025_01_01_00_00_00")
        fm.setup_directories([{"number": 1}, {"number": 2}])

        for num in [1, 2]:
            fm.save_step_output(num, "step_4_final_section.md",
                                f"This is substantial content for section {num}. " * 20)

        analyzer = self._make_analyzer()

        # LLM returns very short content for section 1, reasonable for section 2
        sec2_cleaned = "Cleaned content for section 2. " * 10  # shorter than original
        short_response = (
            "=== SECTION 1: Operating Footprint ===\n"
            "Too short.\n"
            "=== END SECTION 1 ===\n\n"
            "=== SECTION 2: Products and Services ===\n"
            f"{sec2_cleaned}\n"
            "=== END SECTION 2 ===\n"
        )
        analyzer.model_low_temp.generate_content = MagicMock(
            return_value=MagicMock(text=short_response)
        )

        result = analyzer.deduplicate_sections(fm, [1, 2], insights_enabled=False)
        # Section 1 should NOT have step_10 (too short), section 2 should
        assert result is True
        sec1_path = os.path.join(fm.run_dir, "section_1", "step_10_deduplicated.md")
        sec2_path = os.path.join(fm.run_dir, "section_2", "step_10_deduplicated.md")
        assert not os.path.isfile(sec1_path)
        assert os.path.isfile(sec2_path)

    def test_rejects_output_exceeding_120_percent(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        from src.file_manager import FileManager
        fm = FileManager("2025_01_01_00_00_00")
        fm.setup_directories([{"number": 1}, {"number": 2}])

        original = "Original content here. " * 10  # ~230 chars
        for num in [1, 2]:
            fm.save_step_output(num, "step_4_final_section.md", original)

        analyzer = self._make_analyzer()

        # LLM returns bloated content for section 1 (>120% of original)
        bloated = "Bloated expanded content with lots of new material. " * 20  # much longer
        normal = "Cleaned content that is reasonable length. " * 4  # shorter than original
        response = (
            "=== SECTION 1: Operating Footprint ===\n"
            f"{bloated}\n"
            "=== END SECTION 1 ===\n\n"
            "=== SECTION 2: Products and Services ===\n"
            f"{normal}\n"
            "=== END SECTION 2 ===\n"
        )
        analyzer.model_low_temp.generate_content = MagicMock(
            return_value=MagicMock(text=response)
        )

        result = analyzer.deduplicate_sections(fm, [1, 2], insights_enabled=False)
        sec1_path = os.path.join(fm.run_dir, "section_1", "step_10_deduplicated.md")
        assert not os.path.isfile(sec1_path)


class TestDedupSkipLogic:

    def _make_analyzer(self):
        with patch("google.generativeai.GenerativeModel"):
            analyzer = CoreAnalyzer.__new__(CoreAnalyzer)
            analyzer.model_low_temp = MagicMock()
            analyzer.DEDUP_READ_ONLY_SECTIONS = {33, 35}
            analyzer.DEDUP_EXCLUDED_SECTIONS = {34}
            return analyzer

    def test_skipped_with_fewer_than_2_rw_sections(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        from src.file_manager import FileManager
        fm = FileManager("2025_01_01_00_00_00")
        fm.setup_directories([{"number": 1}])
        fm.save_step_output(1, "step_4_final_section.md",
                            "Content for single section. " * 20)

        analyzer = self._make_analyzer()
        result = analyzer.deduplicate_sections(fm, [1], insights_enabled=False)
        assert result is False
        # LLM should not have been called
        analyzer.model_low_temp.generate_content.assert_not_called()

    def test_section_34_excluded(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        from src.file_manager import FileManager
        fm = FileManager("2025_01_01_00_00_00")
        fm.setup_directories([{"number": 1}, {"number": 34}])
        fm.save_step_output(1, "step_4_final_section.md",
                            "Content for section 1. " * 20)
        fm.save_step_output(34, "step_4_final_section.md",
                            "Data book tables. " * 20)

        analyzer = self._make_analyzer()
        # Only 1 rw section (34 excluded), so dedup should skip
        result = analyzer.deduplicate_sections(fm, [1, 34], insights_enabled=False)
        assert result is False


class TestDedupReadOnlySections:

    def _make_analyzer(self):
        with patch("google.generativeai.GenerativeModel"):
            analyzer = CoreAnalyzer.__new__(CoreAnalyzer)
            analyzer.model_low_temp = MagicMock()
            analyzer.DEDUP_READ_ONLY_SECTIONS = {33, 35}
            analyzer.DEDUP_EXCLUDED_SECTIONS = {34}
            return analyzer

    def test_read_only_in_prompt_not_in_output(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        from src.file_manager import FileManager
        fm = FileManager("2025_01_01_00_00_00")
        fm.setup_directories([{"number": 1}, {"number": 2}, {"number": 33}])
        fm.save_step_output(1, "step_4_final_section.md",
                            "Content for section 1. " * 20)
        fm.save_step_output(2, "step_4_final_section.md",
                            "Content for section 2. " * 20)
        fm.save_step_output(33, "step_4_final_section.md",
                            "Pattern analysis content. " * 20)

        analyzer = self._make_analyzer()

        response = (
            "=== SECTION 1: Operating Footprint ===\n"
            "Cleaned section 1 content here and more. " * 5 + "\n"
            "=== END SECTION 1 ===\n\n"
            "=== SECTION 2: Products and Services ===\n"
            "Cleaned section 2 content here and more. " * 5 + "\n"
            "=== END SECTION 2 ===\n"
        )
        analyzer.model_low_temp.generate_content = MagicMock(
            return_value=MagicMock(text=response)
        )

        result = analyzer.deduplicate_sections(fm, [1, 2, 33], insights_enabled=False)
        assert result is True

        # Verify section 33 was included in the prompt (read-only)
        call_args = analyzer.model_low_temp.generate_content.call_args[0][0][0]
        assert "READ-ONLY SECTIONS" in call_args
        assert "Pattern analysis content" in call_args

        # Verify no step_10 written for section 33
        sec33_path = os.path.join(fm.run_dir, "section_33", "step_10_deduplicated.md")
        assert not os.path.isfile(sec33_path)


class TestDedupFileReading:

    def _make_analyzer(self):
        with patch("google.generativeai.GenerativeModel"):
            analyzer = CoreAnalyzer.__new__(CoreAnalyzer)
            return analyzer

    def test_reads_step9_when_insights_enabled(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        from src.file_manager import FileManager
        fm = FileManager("2025_01_01_00_00_00")
        fm.setup_directories([{"number": 1}])
        fm.save_step_output(1, "step_4_final_section.md", "Step 4 content.")
        fm.save_step_output(1, "step_9_integrated.md", "Step 9 content.")

        analyzer = self._make_analyzer()
        content = analyzer._read_section_final_output(fm.run_dir, 1, insights_enabled=True)
        assert content == "Step 9 content."

    def test_falls_back_to_step4_when_no_step9(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        from src.file_manager import FileManager
        fm = FileManager("2025_01_01_00_00_00")
        fm.setup_directories([{"number": 1}])
        fm.save_step_output(1, "step_4_final_section.md", "Step 4 content.")

        analyzer = self._make_analyzer()
        content = analyzer._read_section_final_output(fm.run_dir, 1, insights_enabled=True)
        assert content == "Step 4 content."

    def test_reads_step4_when_insights_disabled(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        from src.file_manager import FileManager
        fm = FileManager("2025_01_01_00_00_00")
        fm.setup_directories([{"number": 1}])
        fm.save_step_output(1, "step_4_final_section.md", "Step 4 content.")
        fm.save_step_output(1, "step_9_integrated.md", "Step 9 content.")

        analyzer = self._make_analyzer()
        content = analyzer._read_section_final_output(fm.run_dir, 1, insights_enabled=False)
        assert content == "Step 4 content."


class TestCollectSectionMarkdownStep10:

    def _make_generator(self, run_timestamp="2025_09_05_16_23_45"):
        with patch("google.generativeai.GenerativeModel"):
            gen = ProfileGenerator.__new__(ProfileGenerator)
            gen.run_timestamp = run_timestamp
            gen.model_name = "gemini-3-flash-preview"
            gen.model = MagicMock()
            return gen

    def test_vanilla_prefers_step10_over_step4(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / "step_4_final_section.md").write_text("Step 4 vanilla content " * 20)
        (sec_dir / "step_10_deduplicated.md").write_text("Deduped vanilla content " * 20)
        monkeypatch.chdir(tmp_path)
        result, _ = gen._collect_section_markdown(pdf_variant="vanilla")
        assert "Deduped vanilla content" in result
        assert "Step 4 vanilla content" not in result

    def test_integrated_prefers_step10_over_step9(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / "step_4_final_section.md").write_text("Step 4 content " * 20)
        (sec_dir / "step_9_integrated.md").write_text("Integrated content " * 20)
        (sec_dir / "step_10_deduplicated.md").write_text("Deduped integrated content " * 20)
        monkeypatch.chdir(tmp_path)
        result, _ = gen._collect_section_markdown(pdf_variant="integrated")
        assert "Deduped integrated content" in result
        assert "Integrated content" not in result

    def test_insights_variant_ignores_step10(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / "step_4_final_section.md").write_text("Step 4 content " * 20)
        (sec_dir / "step_8_synthesis.md").write_text("Insights content " * 20)
        (sec_dir / "step_10_deduplicated.md").write_text("Deduped content " * 20)
        monkeypatch.chdir(tmp_path)
        result, _ = gen._collect_section_markdown(pdf_variant="insights")
        assert "Insights content" in result
        assert "Deduped content" not in result

    def test_default_prefers_step10_over_step4(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / "step_4_final_section.md").write_text("Step 4 default content " * 20)
        (sec_dir / "step_10_deduplicated.md").write_text("Deduped default content " * 20)
        monkeypatch.chdir(tmp_path)
        result, _ = gen._collect_section_markdown()
        assert "Deduped default content" in result
        assert "Step 4 default content" not in result

    def test_default_falls_back_to_step4_without_step10(self, tmp_path, monkeypatch):
        gen = self._make_generator()
        run_dir = tmp_path / "runs" / f"run_{gen.run_timestamp}"
        sec_dir = run_dir / "section_01"
        sec_dir.mkdir(parents=True, exist_ok=True)
        (sec_dir / "step_4_final_section.md").write_text("Step 4 content " * 20)
        monkeypatch.chdir(tmp_path)
        result, _ = gen._collect_section_markdown()
        assert "Step 4 content" in result

import os
from src.file_manager import FileManager


class TestFileManagerSetup:

    def test_creates_run_directory(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        fm = FileManager("2025_09_05_16_23_45")
        sections = [{"number": 1}, {"number": 2}]
        fm.setup_directories(sections)
        assert os.path.isdir(os.path.join(tmp_path, "runs", "run_2025_09_05_16_23_45"))

    def test_creates_section_directories(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        fm = FileManager("2025_09_05_16_23_45")
        sections = [{"number": 1}, {"number": 7}, {"number": 33}]
        fm.setup_directories(sections)
        for num in [1, 7, 33]:
            path = os.path.join(tmp_path, "runs", "run_2025_09_05_16_23_45", f"section_{num}")
            assert os.path.isdir(path), f"section_{num} directory not created"

    def test_custom_prefix(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        fm = FileManager("2025_09_05_16_23_45", run_dir_prefix="opp")
        sections = [{"number": 1}]
        fm.setup_directories(sections)
        assert os.path.isdir(os.path.join(tmp_path, "runs", "opp_2025_09_05_16_23_45"))


class TestFileManagerSaveOutput:

    def test_saves_step_file(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        fm = FileManager("2025_09_05_16_23_45")
        sections = [{"number": 1}]
        fm.setup_directories(sections)
        fm.save_step_output(1, "step_1_initial_draft.md", "Draft content here.")
        path = os.path.join(
            tmp_path, "runs", "run_2025_09_05_16_23_45",
            "section_1", "step_1_initial_draft.md"
        )
        assert os.path.isfile(path)
        with open(path) as f:
            assert f.read() == "Draft content here."

    def test_standard_step_filenames(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        fm = FileManager("2025_09_05_16_23_45")
        sections = [{"number": 5}]
        fm.setup_directories(sections)
        standard_steps = [
            "step_1_initial_draft.md",
            "step_2_completeness.md",
            "step_3_enhanced_draft.md",
            "step_4_final_section.md",
            "step_5_ground_truth.md",
            "step_6_hypotheses.md",
            "step_7_test_results.md",
            "step_8_synthesis.md",
            "step_9_integrated.md",
            "step_10_deduplicated.md",
        ]
        for step in standard_steps:
            fm.save_step_output(5, step, f"Content for {step}")
        sec_dir = os.path.join(
            tmp_path, "runs", "run_2025_09_05_16_23_45", "section_5"
        )
        for step in standard_steps:
            assert os.path.isfile(os.path.join(sec_dir, step)), f"{step} not created"

    def test_section33_layer_filenames(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        fm = FileManager("2025_09_05_16_23_45")
        sections = [{"number": 33}]
        fm.setup_directories(sections)
        layer_files = [
            "layer1_patterns.txt",
            "layer2_pattern1_hypotheses.txt",
            "layer3_pattern1_hyp1_verdict.txt",
            "layer4_pattern1_synthesis.md",
            "step_4_final_section.md",
        ]
        for lf in layer_files:
            fm.save_step_output(33, lf, f"Content for {lf}")
        sec_dir = os.path.join(
            tmp_path, "runs", "run_2025_09_05_16_23_45", "section_33"
        )
        for lf in layer_files:
            assert os.path.isfile(os.path.join(sec_dir, lf)), f"{lf} not created"

    def test_section35_prompt_filenames(self, tmp_path, monkeypatch):
        monkeypatch.chdir(tmp_path)
        fm = FileManager("2025_09_05_16_23_45")
        sections = [{"number": 35}]
        fm.setup_directories(sections)
        prompt_files = [
            "step_1_initial_draft.md",
            "step_4_final_section.md",
        ]
        for pf in prompt_files:
            fm.save_step_output(35, pf, f"Content for {pf}")
        sec_dir = os.path.join(
            tmp_path, "runs", "run_2025_09_05_16_23_45", "section_35"
        )
        for pf in prompt_files:
            assert os.path.isfile(os.path.join(sec_dir, pf)), f"{pf} not created"

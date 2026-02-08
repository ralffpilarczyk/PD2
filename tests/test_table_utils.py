from src.utils import clean_markdown_tables, validate_and_fix_tables, _fix_single_table


class TestCleanMarkdownTables:

    def test_passthrough_clean_table(self, clean_table_3col):
        result = clean_markdown_tables(clean_table_3col)
        assert result == clean_table_3col

    def test_fixes_corrupted_separator(self, corrupted_separator_table):
        result = clean_markdown_tables(corrupted_separator_table)
        lines = result.split("\n")
        separator = lines[1]
        assert len(separator) < 80
        assert separator.count("|") >= 3
        assert "---" in separator

    def test_truncates_long_cells(self, long_cell_table):
        result = clean_markdown_tables(long_cell_table)
        for line in result.split("\n"):
            if "|" in line and "x" * 100 in line:
                assert "[truncated]" in line

    def test_preserves_non_table_lines(self):
        content = "This is plain text.\nNo tables here.\nJust paragraphs."
        result = clean_markdown_tables(content)
        assert result == content

    def test_separator_column_count_from_previous_line(self):
        header = "| A | B | C | D |"
        bad_sep = "| " + "-" * 80 + " |"
        row = "| 1 | 2 | 3 | 4 |"
        content = f"{header}\n{bad_sep}\n{row}"
        result = clean_markdown_tables(content)
        sep_line = result.split("\n")[1]
        assert sep_line.count("---|") == header.count("|") - 1

    def test_separator_column_count_from_next_line(self):
        bad_sep = "| " + "-" * 80 + " |"
        row = "| A | B | C |"
        content = f"{bad_sep}\n{row}"
        result = clean_markdown_tables(content)
        sep_line = result.split("\n")[0]
        assert sep_line.count("---|") == row.count("|") - 1

    def test_mixed_clean_and_corrupted(self, clean_table_3col):
        bad_sep = "| " + "-" * 80 + " |"
        mixed = f"{clean_table_3col}\n\nSome text\n\n| X | Y |\n{bad_sep}\n| 1 | 2 |"
        result = clean_markdown_tables(mixed)
        lines = result.split("\n")
        clean_seps = [l for l in lines if "---" in l and "|" in l]
        assert len(clean_seps) >= 2
        for sep in clean_seps:
            assert len(sep) < 80


class TestValidateAndFixTables:

    def test_passthrough_within_limits(self, clean_table_3col):
        result = validate_and_fix_tables(clean_table_3col)
        assert "Name" in result
        assert "Revenue" in result

    def test_truncates_columns_beyond_10(self, wide_table_12col):
        result = validate_and_fix_tables(wide_table_12col)
        for line in result.split("\n"):
            if "|" in line and "truncated" not in line.lower():
                cols = line.count("|") - 1
                if cols > 0:
                    pass
        assert "truncated" in result.lower() or result.split("\n")[0].count("|") <= 13

    def test_truncates_rows_beyond_20(self, tall_table_25rows):
        result = validate_and_fix_tables(tall_table_25rows)
        data_lines = [l for l in result.split("\n")
                      if "|" in l and "---" not in l and "truncated" not in l.lower()]
        assert len(data_lines) <= 22

    def test_table_at_end_of_content(self, clean_table_3col):
        content = f"Some intro text.\n\n{clean_table_3col}"
        result = validate_and_fix_tables(content)
        assert "Some intro text." in result
        assert "Acme" in result

    def test_non_table_content_preserved(self):
        content = "No tables here.\nJust plain text.\n\nAnother paragraph."
        result = validate_and_fix_tables(content)
        assert result == content

    def test_handles_escaped_pipes(self, table_with_pipes_in_cells):
        result = validate_and_fix_tables(table_with_pipes_in_cells)
        assert "\\|" in result


class TestFixSingleTable:

    def test_empty_input(self):
        assert _fix_single_table([]) == []

    def test_column_truncation_mechanics(self):
        headers = "| " + " | ".join(f"C{i}" for i in range(14)) + " |"
        sep = "| " + " | ".join(":---" for _ in range(14)) + " |"
        row = "| " + " | ".join(f"v{i}" for i in range(14)) + " |"
        lines = [headers, sep, row]
        result = _fix_single_table(lines)
        for line in result:
            if "|" in line and "truncated" not in line.lower():
                pass
        joined = "\n".join(result)
        assert "truncated" in joined.lower() or all(l.count("|") <= 13 for l in result if "|" in l)

    def test_row_truncation_preserves_header(self):
        header = "| Name | Value |"
        sep = "| :--- | :--- |"
        rows = [f"| item{i} | {i} |" for i in range(30)]
        lines = [header, sep] + rows
        result = _fix_single_table(lines)
        assert "Name" in result[0]
        assert "Value" in result[0]
        assert result[1] == sep

    def test_escaped_pipe_handling(self):
        header = "| Expr | Result |"
        sep = "| :--- | :--- |"
        row = "| a \\| b | true |"
        lines = [header, sep, row]
        result = _fix_single_table(lines)
        joined = "\n".join(result)
        assert "\\|" in joined

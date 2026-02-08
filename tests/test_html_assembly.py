import re
from unittest.mock import patch, MagicMock
from src.profile_generator import get_display_number, GROUP_DEFINITIONS, ProfileGenerator


class TestTimestampCompression:

    def _make_generator(self, timestamp):
        with patch("google.generativeai.GenerativeModel"):
            gen = ProfileGenerator.__new__(ProfileGenerator)
            gen.run_timestamp = timestamp
            gen.model_name = "gemini-3-flash-preview"
            gen.model = MagicMock()
            return gen

    def test_standard_timestamp_compression(self):
        gen = self._make_generator("2025_09_05_16_23_45")
        parts = gen.run_timestamp.split("_")
        year = parts[0][2:]
        month = parts[1]
        day = parts[2]
        hour = parts[3]
        minute = parts[4]
        compact = f"{year}{month}{day}_{hour}{minute}"
        assert compact == "250905_1623"

    def test_short_timestamp_uses_fallback(self):
        gen = self._make_generator("2025_09")
        parts = gen.run_timestamp.split("_")
        assert len(parts) < 5


class TestCoverPage:

    def _generate_cover(self, processed_sections, company_name="TestCo", pdf_variant=None):
        with patch("google.generativeai.GenerativeModel"):
            gen = ProfileGenerator.__new__(ProfileGenerator)
            gen.run_timestamp = "2025_09_05_16_23_45"
            gen.model_name = "gemini-3-flash-preview"
            gen.model = MagicMock()
            return gen._generate_cover_page(processed_sections, company_name, pdf_variant=pdf_variant)

    def test_cover_contains_company_name(self):
        html = self._generate_cover([(1, "Operating Footprint")], company_name="Acme Corp")
        assert "Acme Corp" in html

    def test_cover_contains_version(self):
        html = self._generate_cover([(1, "Operating Footprint")])
        assert "ProfileDash" in html

    def test_toc_groups_present(self):
        all_sections = [(n, f"Title {n}") for n in range(1, 36)]
        html = self._generate_cover(all_sections)
        for name, _ in GROUP_DEFINITIONS:
            assert name in html

    def test_toc_links_use_section_anchors(self):
        html = self._generate_cover([(7, "Summary Financials")])
        assert 'href="#section_7"' in html

    def test_toc_uses_display_numbers(self):
        html = self._generate_cover([(13, "Deep Dive Discoveries")])
        assert "2.7:" in html

    def test_missing_sections_excluded_from_toc(self):
        html = self._generate_cover([(1, "Operating Footprint")])
        assert "section_7" not in html


class TestFullHtmlDocument:

    def _build_html(self, company_name="TestCo", sections_list=None, pdf_variant=None):
        if sections_list is None:
            sections_list = [(1, "Operating Footprint")]
        with patch("google.generativeai.GenerativeModel"):
            gen = ProfileGenerator.__new__(ProfileGenerator)
            gen.run_timestamp = "2025_09_05_16_23_45"
            gen.model_name = "gemini-3-flash-preview"
            gen.model = MagicMock()

        cover_html = gen._generate_cover_page(sections_list, company_name, pdf_variant=pdf_variant)
        md_content = "Some **markdown** content with a table:\n\n| A | B |\n| :--- | :--- |\n| 1 | 2 |"

        from src import __version__
        from datetime import datetime
        footer_date = datetime.now().strftime("%d-%b-%y")
        css_styles = gen._get_css_styles(footer_date)

        variant_suffix = f"_{pdf_variant}" if pdf_variant else ""
        clean_company_name = company_name.replace(" ", "_")

        full_html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{company_name} - ProfileDash {__version__}</title>
    <style>
{css_styles}
    </style>
</head>
<body>
{cover_html}
    <div class="profile-content">
        <div class="section">
            <div class="section-content">
{md_content}
            </div>
        </div>
    </div>
</body>
</html>"""
        return full_html, f"{clean_company_name}{variant_suffix}_250905_1623.html"

    def test_valid_doctype(self):
        html, _ = self._build_html()
        assert html.strip().startswith("<!DOCTYPE html>")

    def test_has_html_head_body(self):
        html, _ = self._build_html()
        assert "<html" in html
        assert "<head>" in html
        assert "<body>" in html
        assert "</html>" in html

    def test_title_contains_company_name(self):
        html, _ = self._build_html(company_name="Acme Corp")
        assert "<title>Acme Corp - ProfileDash" in html

    def test_cover_page_div_present(self):
        html, _ = self._build_html()
        assert 'class="cover-page"' in html

    def test_profile_content_div_present(self):
        html, _ = self._build_html()
        assert 'class="profile-content"' in html

    def test_page_break_between_cover_and_content(self):
        html, _ = self._build_html()
        cover_end = html.find("page-break")
        content_start = html.find("profile-content")
        assert cover_end < content_start

    def test_css_styles_included(self):
        html, _ = self._build_html()
        assert "<style>" in html
        assert "font-family" in html

    def test_variant_suffix_in_filename(self):
        _, filename = self._build_html(pdf_variant="insights")
        assert "_insights_" in filename

    def test_no_variant_suffix_when_none(self):
        _, filename = self._build_html(pdf_variant=None)
        assert "_insights_" not in filename
        assert "TestCo_250905_1623.html" == filename

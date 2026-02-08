from src.profile_sections import sections
from src.profile_generator import GROUP_DEFINITIONS


class TestSectionDefinitions:

    def test_exactly_35_sections_exist(self):
        assert len(sections) == 35

    def test_section_numbers_are_1_through_35(self):
        numbers = {s["number"] for s in sections}
        assert numbers == set(range(1, 36))

    def test_section_numbers_unique(self):
        numbers = [s["number"] for s in sections]
        assert len(numbers) == len(set(numbers))

    def test_all_sections_have_required_keys(self):
        for s in sections:
            assert "number" in s, f"Section missing 'number': {s}"
            assert "title" in s, f"Section {s.get('number')} missing 'title'"
            assert "specs" in s, f"Section {s.get('number')} missing 'specs'"

    def test_sections_1_through_32_have_ground_truth_pointer(self):
        for s in sections:
            if s["number"] <= 32:
                assert "ground_truth_pointer" in s, (
                    f"Section {s['number']} missing 'ground_truth_pointer'"
                )
                assert len(s["ground_truth_pointer"]) > 0

    def test_sections_33_34_35_existence(self):
        nums = {s["number"] for s in sections}
        assert 33 in nums
        assert 34 in nums
        assert 35 in nums

    def test_all_section_titles_non_empty(self):
        for s in sections:
            assert s["title"].strip(), f"Section {s['number']} has empty title"

    def test_all_section_specs_non_empty(self):
        for s in sections:
            assert s["specs"].strip(), f"Section {s['number']} has empty specs"


class TestGroupDefinitions:

    def test_all_sections_appear_in_groups(self):
        grouped = set()
        for _, section_list in GROUP_DEFINITIONS:
            grouped.update(section_list)
        assert grouped == set(range(1, 36))

    def test_no_section_appears_in_multiple_groups(self):
        seen = []
        for _, section_list in GROUP_DEFINITIONS:
            for n in section_list:
                assert n not in seen, f"Section {n} appears in multiple groups"
                seen.append(n)

    def test_group_count_is_5(self):
        assert len(GROUP_DEFINITIONS) == 5

    def test_group_names_are_correct(self):
        names = [name for name, _ in GROUP_DEFINITIONS]
        assert names == [
            "Company Profile",
            "Strategy and Company Analysis",
            "Sellside Positioning",
            "Buyside Due Diligence",
            "Data Book",
        ]

    def test_group_ordering(self):
        expected = [
            [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12],
            [14, 15, 16, 17, 18, 19, 13, 33, 35],
            [20, 21, 22, 23, 24, 25, 26],
            [27, 28, 29, 30, 31, 32],
            [34],
        ]
        for i, (_, section_list) in enumerate(GROUP_DEFINITIONS):
            assert section_list == expected[i], (
                f"Group {i} ordering mismatch: expected {expected[i]}, got {section_list}"
            )

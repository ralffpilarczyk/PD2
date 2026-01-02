"""
Research section definitions for Deep Research.
Uses sections 1-12 from profile_sections.py (Company Profile group).
"""

from src.profile_sections import sections as pd2_sections

# Research uses sections 1-12 (Company Profile)
RESEARCH_SECTIONS = pd2_sections[:12]


def get_research_sections() -> list:
    """Return list of section definitions for Deep Research."""
    return RESEARCH_SECTIONS


def get_section_titles() -> list:
    """Return list of section titles for display."""
    return [s['title'] for s in RESEARCH_SECTIONS]


def get_section_count() -> int:
    """Return number of research sections."""
    return len(RESEARCH_SECTIONS)

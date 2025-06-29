"""
Intelligent Analyst - Modular Components

This package contains the core components for the intelligent document analysis system.
"""

from .core_analyzer import CoreAnalyzer
from .insight_memory import InsightMemory
from .quality_tracker import QualityTracker
from .file_manager import FileManager
from .profile_generator import ProfileGenerator
from .profile_sections import sections

__all__ = [
    'CoreAnalyzer',
    'InsightMemory', 
    'QualityTracker',
    'FileManager',
    'ProfileGenerator',
    'sections'
]

# Version info
__version__ = "2.0.0"

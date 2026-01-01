"""
Intelligent Analyst - Modular Components

This package contains the core components for the intelligent document analysis system.
"""

# Version info
__version__ = "2.2"

from .core_analyzer import CoreAnalyzer
from .file_manager import FileManager
from .profile_generator import ProfileGenerator
from .profile_sections import sections
from . import utils

__all__ = [
    'CoreAnalyzer',
    'FileManager',
    'ProfileGenerator',
    'sections',
    'utils'
]

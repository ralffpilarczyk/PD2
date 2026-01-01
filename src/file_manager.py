import os
import json
from typing import List, Dict
from pathlib import Path
from .utils import thread_safe_print, clean_markdown_tables

class FileManager:
    """Handles all file I/O operations and directory management"""

    def __init__(self, run_timestamp: str, run_dir_prefix: str = "run"):
        """Initialize file manager with run timestamp and optional directory prefix

        Args:
            run_timestamp: Timestamp string for the run
            run_dir_prefix: Prefix for run directory (default: "run", for OPP: "opp" or "opp_custom")
        """
        self.run_timestamp = run_timestamp
        self.run_dir = f"runs/{run_dir_prefix}_{run_timestamp}"
    
    def setup_directories(self, sections: List[Dict]):
        """Create folder structure for tracking"""
        base_dirs = [self.run_dir]

        for section in sections:
            base_dirs.append(f"{self.run_dir}/section_{section['number']}")

        for dir_path in base_dirs:
            os.makedirs(dir_path, exist_ok=True)
    
    def load_markdown_files(self, file_paths: List[str]) -> str:
        """Load and concatenate markdown files"""
        contents = []
        for path in file_paths:
            try:
                with open(path, 'r', encoding='utf-8') as f:
                    raw_content = f.read()
                
                # Clean corrupted markdown tables
                thread_safe_print(f"Checking {os.path.basename(path)} for table corruption...")
                cleaned_content = clean_markdown_tables(raw_content)
                
                contents.append(f"--- Document: {os.path.basename(path)} ---\n{cleaned_content}\n")
            except Exception as e:
                thread_safe_print(f"Warning: Failed to load {path}: {e}")
                continue
        return "\n\n".join(contents)
    
    def save_step_output(self, section_num: int, step: str, content: str):
        """Save output from each step for transparency"""
        filename = f"{self.run_dir}/section_{section_num}/{step}"
        try:
            with open(filename, 'w', encoding='utf-8') as f:
                f.write(content)
        except Exception as e:
            thread_safe_print(f"Error: Failed to save {filename}: {e}")
            raise

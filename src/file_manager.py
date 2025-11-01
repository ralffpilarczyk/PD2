import os
import json
from typing import List, Dict
from datetime import datetime
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
        base_dirs = [
            "memory",
            "memory/memory_library", 
            self.run_dir,
            f"{self.run_dir}/memory_review",
            "quality_metrics"
        ]
        
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
    
    def save_memory_state(self, memory_data: Dict, filename: str):
        """Save memory state to specified file"""
        filepath = f"{self.run_dir}/memory_review/{filename}"
        try:
            with open(filepath, 'w', encoding='utf-8') as f:
                json.dump(memory_data, f, indent=2)
        except Exception as e:
            thread_safe_print(f"Error: Failed to save memory state to {filepath}: {e}")
            raise

    def archive_memory(self, memory_data: Dict, archive_name: str = None, memory_prefix: str = None) -> str:
        """Archive current memory and return archive path

        Args:
            memory_data: Memory data to archive
            archive_name: Optional custom archive name
            memory_prefix: Optional prefix (e.g., 'pd2', 'opp') to include in archive filename
        """
        if archive_name is None:
            timestamp = datetime.now().strftime('%Y_%m_%d_%H_%M_%S')
            if memory_prefix:
                archive_name = f"{memory_prefix}_memory_{timestamp}"
            else:
                archive_name = f"memory_{timestamp}"

        archive_path = f"memory/memory_library/{archive_name}.json"
        os.makedirs("memory/memory_library", exist_ok=True)
        with open(archive_path, 'w', encoding='utf-8') as f:
            json.dump(memory_data, f, indent=2)

        return archive_path
    
    def save_quality_metrics(self, quality_scores: Dict, run_number: int):
        """Save quality metrics for tracking improvement"""
        metrics_file = "quality_metrics/insight_depth_scores.json"
        
        try:
            # Load existing metrics
            if os.path.exists(metrics_file):
                with open(metrics_file, 'r', encoding='utf-8') as f:
                    all_metrics = json.load(f)
            else:
                all_metrics = {}
            
            # Add current run metrics
            run_key = f"run_{run_number}"
            all_metrics[run_key] = quality_scores
            
            # Calculate average
            if quality_scores:
                avg_depth = sum(s["depth_ratio"] for s in quality_scores.values()) / len(quality_scores)
                all_metrics[run_key]["average_depth_ratio"] = avg_depth
            
            # Save updated metrics
            with open(metrics_file, 'w', encoding='utf-8') as f:
                json.dump(all_metrics, f, indent=2)
        except json.JSONDecodeError as e:
            thread_safe_print(f"Error: Corrupted quality metrics file: {e}")
            # Start fresh if corrupted
            all_metrics = {f"run_{run_number}": quality_scores}
            with open(metrics_file, 'w', encoding='utf-8') as f:
                json.dump(all_metrics, f, indent=2)
        except Exception as e:
            thread_safe_print(f"Error: Failed to save quality metrics: {e}")
            raise
    
 
import os
import json
from typing import List, Dict
from datetime import datetime
from pathlib import Path

class FileManager:
    """Handles all file I/O operations and directory management"""
    
    def __init__(self, run_timestamp: str):
        """Initialize file manager with run timestamp"""
        self.run_timestamp = run_timestamp
        self.run_dir = f"runs/run_{run_timestamp}"
    
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
            with open(path, 'r', encoding='utf-8') as f:
                contents.append(f"--- Document: {os.path.basename(path)} ---\n{f.read()}\n")
        return "\n\n".join(contents)
    
    def save_step_output(self, section_num: int, step: str, content: str):
        """Save output from each step for transparency"""
        filename = f"{self.run_dir}/section_{section_num}/{step}"
        with open(filename, 'w', encoding='utf-8') as f:
            f.write(content)
    
    def save_memory_state(self, memory_data: Dict, filename: str):
        """Save memory state to specified file"""
        filepath = f"{self.run_dir}/memory_review/{filename}"
        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(memory_data, f, indent=2)
    
    def ensure_memory_file_exists(self, memory_data: Dict):
        """Ensure the main learning memory file exists"""
        memory_path = "memory/learning_memory.json"
        if not os.path.exists(memory_path):
            with open(memory_path, 'w', encoding='utf-8') as f:
                json.dump(memory_data, f, indent=2)
    
    def save_memory_to_main_file(self, memory_data: Dict):
        """Save updated memory to main file"""
        with open("memory/learning_memory.json", 'w', encoding='utf-8') as f:
            json.dump(memory_data, f, indent=2)
    
    def archive_memory(self, memory_data: Dict, archive_name: str = None) -> str:
        """Archive current memory and return archive path"""
        if archive_name is None:
            timestamp = datetime.now().strftime('%Y_%m_%d_%H_%M_%S')
            archive_name = f"memory_{timestamp}"
        
        archive_path = f"memory/memory_library/{archive_name}.json"
        os.makedirs("memory/memory_library", exist_ok=True)
        with open(archive_path, 'w', encoding='utf-8') as f:
            json.dump(memory_data, f, indent=2)
        
        return archive_path
    
    def save_quality_metrics(self, quality_scores: Dict, run_number: int):
        """Save quality metrics for tracking improvement"""
        metrics_file = "quality_metrics/insight_depth_scores.json"
        
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
    
    def save_run_summary(self, summary_text: str):
        """Save run summary"""
        with open(f"{self.run_dir}/run_summary.txt", 'w', encoding='utf-8') as f:
            f.write(summary_text) 
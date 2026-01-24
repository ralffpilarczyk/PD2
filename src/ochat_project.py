import os
import json
from datetime import datetime
from pathlib import Path
from typing import List, Dict, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from .ochat_sources import SourceManager


class ProjectManager:
    """Manages OChat projects - creation, storage, and note management"""

    PROJECTS_DIR = "ChatProjects"
    PROJECT_METADATA_FILE = "project.json"
    NOTES_DIR = "notes"
    SOURCES_DIR = "sources"  # For Phase 2

    def __init__(self):
        """Initialize project manager and ensure projects directory exists"""
        self.projects_path = Path(self.PROJECTS_DIR)
        self.projects_path.mkdir(exist_ok=True)
        self.current_project: Optional[str] = None
        self.current_project_path: Optional[Path] = None

    def create_project(self, name: str) -> bool:
        """Create a new project

        Args:
            name: Unique project name

        Returns:
            True if created successfully, False if already exists
        """
        # Sanitize project name for filesystem
        safe_name = self._sanitize_name(name)
        project_path = self.projects_path / safe_name

        if project_path.exists():
            return False

        # Create project structure
        project_path.mkdir()
        (project_path / self.NOTES_DIR).mkdir()
        (project_path / self.SOURCES_DIR).mkdir()  # Ready for Phase 2

        # Create metadata file
        metadata = {
            "name": name,
            "created": datetime.now().isoformat(),
            "last_accessed": datetime.now().isoformat(),
            "note_count": 0
        }

        with open(project_path / self.PROJECT_METADATA_FILE, 'w', encoding='utf-8') as f:
            json.dump(metadata, f, indent=2)

        self.current_project = name
        self.current_project_path = project_path

        return True

    def open_project(self, name: str) -> bool:
        """Open an existing project

        Args:
            name: Project name

        Returns:
            True if opened successfully, False if not found
        """
        safe_name = self._sanitize_name(name)
        project_path = self.projects_path / safe_name

        if not project_path.exists():
            return False

        # Update last accessed time
        metadata_path = project_path / self.PROJECT_METADATA_FILE
        if metadata_path.exists():
            with open(metadata_path, 'r', encoding='utf-8') as f:
                metadata = json.load(f)
            metadata["last_accessed"] = datetime.now().isoformat()
            with open(metadata_path, 'w', encoding='utf-8') as f:
                json.dump(metadata, f, indent=2)

        self.current_project = name
        self.current_project_path = project_path

        return True

    def list_projects(self) -> List[Dict]:
        """List all available projects

        Returns:
            List of project info dictionaries
        """
        projects = []

        for item in self.projects_path.iterdir():
            if item.is_dir():
                metadata_path = item / self.PROJECT_METADATA_FILE
                if metadata_path.exists():
                    with open(metadata_path, 'r', encoding='utf-8') as f:
                        metadata = json.load(f)
                    projects.append({
                        "name": metadata.get("name", item.name),
                        "created": metadata.get("created", "Unknown"),
                        "last_accessed": metadata.get("last_accessed", "Unknown"),
                        "note_count": metadata.get("note_count", 0),
                        "path": str(item)
                    })

        # Sort by last accessed (most recent first)
        projects.sort(key=lambda x: x.get("last_accessed", ""), reverse=True)
        return projects

    def delete_project(self, name: str) -> bool:
        """Delete a project and all its contents

        Args:
            name: Project name

        Returns:
            True if deleted successfully
        """
        import shutil

        safe_name = self._sanitize_name(name)
        project_path = self.projects_path / safe_name

        if not project_path.exists():
            return False

        shutil.rmtree(project_path)

        # Clear current project if it was the deleted one
        if self.current_project == name:
            self.current_project = None
            self.current_project_path = None

        return True

    def save_note(self, question: str, response: str, title: Optional[str] = None) -> str:
        """Save a Q&A exchange as a note

        Args:
            question: The user's question
            response: The assistant's response
            title: Optional custom title for the note

        Returns:
            The note filename
        """
        if not self.current_project_path:
            raise ValueError("No project is currently open")

        notes_path = self.current_project_path / self.NOTES_DIR

        # Generate note filename with timestamp
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        note_filename = f"note_{timestamp}.md"
        note_path = notes_path / note_filename

        # Generate title from question if not provided
        if not title:
            # Use first 50 chars of question as title
            title = question[:50].strip()
            if len(question) > 50:
                title += "..."

        # Format note content
        note_content = f"""# {title}

**Saved:** {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}

---

## Question

{question}

---

## Response

{response}
"""

        with open(note_path, 'w', encoding='utf-8') as f:
            f.write(note_content)

        # Update note count in metadata
        self._update_note_count()

        return note_filename

    def list_notes(self) -> List[Dict]:
        """List all notes in the current project

        Returns:
            List of note info dictionaries
        """
        if not self.current_project_path:
            return []

        notes_path = self.current_project_path / self.NOTES_DIR
        notes = []

        for note_file in sorted(notes_path.glob("*.md"), reverse=True):
            # Extract title from first line
            with open(note_file, 'r', encoding='utf-8') as f:
                first_line = f.readline().strip()
                title = first_line.lstrip('#').strip() if first_line.startswith('#') else note_file.stem

            notes.append({
                "filename": note_file.name,
                "title": title,
                "path": str(note_file)
            })

        return notes

    def read_note(self, filename: str) -> Optional[str]:
        """Read a specific note

        Args:
            filename: Note filename

        Returns:
            Note content or None if not found
        """
        if not self.current_project_path:
            return None

        note_path = self.current_project_path / self.NOTES_DIR / filename

        if not note_path.exists():
            # Try matching by partial name
            notes_path = self.current_project_path / self.NOTES_DIR
            matches = list(notes_path.glob(f"*{filename}*"))
            if matches:
                note_path = matches[0]
            else:
                return None

        with open(note_path, 'r', encoding='utf-8') as f:
            return f.read()

    def delete_note(self, filename: str) -> bool:
        """Delete a specific note

        Args:
            filename: Note filename

        Returns:
            True if deleted successfully
        """
        if not self.current_project_path:
            return False

        note_path = self.current_project_path / self.NOTES_DIR / filename

        if not note_path.exists():
            return False

        note_path.unlink()
        self._update_note_count()
        return True

    def get_project_info(self) -> Optional[Dict]:
        """Get info about the current project

        Returns:
            Project metadata or None if no project is open
        """
        if not self.current_project_path:
            return None

        metadata_path = self.current_project_path / self.PROJECT_METADATA_FILE
        if metadata_path.exists():
            with open(metadata_path, 'r', encoding='utf-8') as f:
                return json.load(f)
        return None

    def get_source_manager(self) -> Optional["SourceManager"]:
        """Get a SourceManager for the current project

        Returns:
            SourceManager instance or None if no project is open
        """
        if not self.current_project_path:
            return None

        from .ochat_sources import SourceManager
        return SourceManager(self.current_project_path)

    def _update_note_count(self):
        """Update the note count in project metadata"""
        if not self.current_project_path:
            return

        notes_path = self.current_project_path / self.NOTES_DIR
        note_count = len(list(notes_path.glob("*.md")))

        metadata_path = self.current_project_path / self.PROJECT_METADATA_FILE
        if metadata_path.exists():
            with open(metadata_path, 'r', encoding='utf-8') as f:
                metadata = json.load(f)
            metadata["note_count"] = note_count
            with open(metadata_path, 'w', encoding='utf-8') as f:
                json.dump(metadata, f, indent=2)

    @staticmethod
    def _sanitize_name(name: str) -> str:
        """Sanitize a name for use as a directory name"""
        # Replace spaces and special characters
        safe = name.replace(' ', '_')
        # Remove characters that are problematic in filenames
        for char in ['/', '\\', ':', '*', '?', '"', '<', '>', '|']:
            safe = safe.replace(char, '')
        return safe

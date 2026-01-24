import os
import shutil
import hashlib
from pathlib import Path
from typing import List, Dict, Optional, Tuple, TYPE_CHECKING
from datetime import datetime

if TYPE_CHECKING:
    from .ochat_rag import RAGManager


class SourceManager:
    """Manages source files for OChat projects - upload, conversion, and loading"""

    SUPPORTED_EXTENSIONS = ('.pdf', '.md')
    CACHE_DIR = "SourceCache"  # Shared cache for PDF conversions

    def __init__(self, project_path: Path):
        """Initialize source manager for a project

        Args:
            project_path: Path to the project directory
        """
        self.project_path = project_path
        self.sources_path = project_path / "sources"
        self.sources_path.mkdir(exist_ok=True)

        # Shared cache directory for PDF conversions
        self.cache_path = Path(self.CACHE_DIR)
        self.cache_path.mkdir(exist_ok=True)

        # RAG manager (lazy initialized)
        self._rag_manager: Optional["RAGManager"] = None

    @property
    def rag(self) -> "RAGManager":
        """Get or create RAG manager"""
        if self._rag_manager is None:
            from .ochat_rag import RAGManager
            self._rag_manager = RAGManager(self.project_path)
        return self._rag_manager

    def add_file(self, file_path: str, index: bool = True) -> Tuple[bool, str]:
        """Add a source file to the project

        Args:
            file_path: Path to the source file
            index: Whether to index for RAG (default True)

        Returns:
            Tuple of (success, message)
        """
        source = Path(file_path)

        if not source.exists():
            return False, f"File not found: {file_path}"

        if source.suffix.lower() not in self.SUPPORTED_EXTENSIONS:
            return False, f"Unsupported file type: {source.suffix}. Use .pdf or .md"

        # Copy file to project sources
        dest = self.sources_path / source.name

        # Handle duplicate names
        if dest.exists():
            base = source.stem
            ext = source.suffix
            counter = 1
            while dest.exists():
                dest = self.sources_path / f"{base}_{counter}{ext}"
                counter += 1

        shutil.copy2(source, dest)

        # If PDF, convert to markdown
        md_file = dest
        if source.suffix.lower() == '.pdf':
            success, msg = self._convert_pdf(dest)
            if not success:
                return False, f"File copied but conversion failed: {msg}"
            md_file = dest.with_suffix('.md')
            result_msg = f"Added and converted: {dest.name}"
        else:
            result_msg = f"Added: {dest.name}"

        # Index for RAG
        if index and md_file.exists():
            try:
                with open(md_file, 'r', encoding='utf-8') as f:
                    content = f.read()
                chunk_count = self.rag.index_document(content, md_file.name)
                result_msg += f" ({chunk_count} chunks indexed)"
            except Exception as e:
                result_msg += f" (indexing failed: {e})"

        return True, result_msg

    def _get_file_hash(self, file_path: Path) -> str:
        """Calculate SHA-256 hash of a file for caching"""
        sha256_hash = hashlib.sha256()
        with open(file_path, "rb") as f:
            for byte_block in iter(lambda: f.read(4096), b""):
                sha256_hash.update(byte_block)
        return sha256_hash.hexdigest()[:16]

    def _convert_pdf(self, pdf_path: Path) -> Tuple[bool, str]:
        """Convert PDF to markdown using marker-pdf

        Args:
            pdf_path: Path to the PDF file

        Returns:
            Tuple of (success, message)
        """
        try:
            # Check cache first
            file_hash = self._get_file_hash(pdf_path)
            cache_file = self.cache_path / f"{file_hash}.md"

            if cache_file.exists():
                # Use cached conversion
                md_dest = pdf_path.with_suffix('.md')
                shutil.copy2(cache_file, md_dest)
                return True, "Loaded from cache"

            # Import marker here to avoid startup delay
            from marker.converters.pdf import PdfConverter
            from marker.models import create_model_dict

            # Create converter with models
            models = create_model_dict()
            converter = PdfConverter(artifact_dict=models)

            # Convert PDF
            result = converter(str(pdf_path))

            # Extract markdown from result
            if hasattr(result, 'markdown'):
                markdown_content = result.markdown
            elif isinstance(result, tuple) and len(result) > 0:
                markdown_content = result[0]
            else:
                markdown_content = str(result)

            # Save to project sources
            md_dest = pdf_path.with_suffix('.md')
            with open(md_dest, 'w', encoding='utf-8') as f:
                f.write(markdown_content)

            # Cache the conversion
            with open(cache_file, 'w', encoding='utf-8') as f:
                f.write(markdown_content)

            return True, "Converted successfully"

        except ImportError:
            return False, "marker-pdf not installed. Run: pip install marker-pdf"
        except Exception as e:
            return False, str(e)

    def remove_file(self, filename: str) -> Tuple[bool, str]:
        """Remove a source file from the project

        Args:
            filename: Name of the file to remove

        Returns:
            Tuple of (success, message)
        """
        removed = []
        md_removed = None

        # Try to find and remove files
        for ext in ['', '.pdf', '.md']:
            target = self.sources_path / f"{filename}{ext}"
            if target.exists():
                if target.suffix.lower() == '.md':
                    md_removed = target.name
                target.unlink()
                removed.append(target.name)

        # Also try exact match
        exact = self.sources_path / filename
        if exact.exists() and exact.name not in removed:
            if exact.suffix.lower() == '.md':
                md_removed = exact.name
            exact.unlink()
            removed.append(exact.name)

        # Remove from RAG index if md file was removed
        if md_removed:
            try:
                self.rag._remove_source(md_removed)
            except Exception:
                pass

        if removed:
            return True, f"Removed: {', '.join(removed)}"
        return False, f"File not found: {filename}"

    def list_sources(self) -> List[Dict]:
        """List all source files in the project (MD files only - what's actually used)

        Returns:
            List of source file info dictionaries
        """
        sources = []

        for file in sorted(self.sources_path.iterdir()):
            # Only show .md files (these are what get indexed and used)
            if file.is_file() and file.suffix.lower() == '.md':
                # Get file size
                size = file.stat().st_size
                if size < 1024:
                    size_str = f"{size} B"
                elif size < 1024 * 1024:
                    size_str = f"{size / 1024:.1f} KB"
                else:
                    size_str = f"{size / (1024 * 1024):.1f} MB"

                sources.append({
                    "filename": file.name,
                    "type": file.suffix.lower(),
                    "size": size_str,
                    "path": str(file)
                })

        return sources

    def load_all_sources(self) -> Tuple[str, int]:
        """Load all source documents as combined text (legacy, for non-RAG mode)

        Returns:
            Tuple of (combined_text, token_estimate)
        """
        combined = []
        total_chars = 0

        # Get markdown files (including converted PDFs)
        for file in sorted(self.sources_path.iterdir()):
            if file.is_file() and file.suffix.lower() == '.md':
                try:
                    with open(file, 'r', encoding='utf-8') as f:
                        content = f.read()

                    # Add source header
                    source_text = f"\n\n--- SOURCE: {file.name} ---\n\n{content}"
                    combined.append(source_text)
                    total_chars += len(content)
                except Exception as e:
                    combined.append(f"\n\n--- SOURCE: {file.name} (Error: {e}) ---\n\n")

        combined_text = "\n".join(combined)

        # Rough token estimate (1 token ≈ 4 chars for English)
        token_estimate = total_chars // 4

        return combined_text, token_estimate

    def get_source_count(self) -> int:
        """Get the number of source files (MD files only)"""
        return len([f for f in self.sources_path.iterdir()
                    if f.is_file() and f.suffix.lower() == '.md'])

    def get_context_size(self) -> Tuple[int, int]:
        """Get total context size of all sources

        Returns:
            Tuple of (char_count, token_estimate)
        """
        total_chars = 0

        for file in self.sources_path.iterdir():
            if file.is_file() and file.suffix.lower() == '.md':
                total_chars += file.stat().st_size

        return total_chars, total_chars // 4

    def reindex_all(self) -> Tuple[int, int]:
        """Reindex all source documents for RAG

        Returns:
            Tuple of (files_indexed, total_chunks)
        """
        # Clear existing index
        self.rag.clear_index()

        files_indexed = 0
        total_chunks = 0

        for file in sorted(self.sources_path.iterdir()):
            if file.is_file() and file.suffix.lower() == '.md':
                try:
                    with open(file, 'r', encoding='utf-8') as f:
                        content = f.read()
                    chunk_count = self.rag.index_document(content, file.name)
                    files_indexed += 1
                    total_chunks += chunk_count
                except Exception as e:
                    print(f"  Error indexing {file.name}: {e}")

        return files_indexed, total_chunks

    def get_rag_stats(self) -> Dict:
        """Get RAG index statistics"""
        return self.rag.get_index_stats()


def select_source_files() -> List[str]:
    """Open file dialog to select source files

    Returns:
        List of selected file paths
    """
    try:
        import tkinter as tk
        from tkinter import filedialog

        root = tk.Tk()
        root.withdraw()
        root.attributes('-topmost', True)

        files = filedialog.askopenfilenames(
            title="Select source files (PDF or Markdown)",
            filetypes=[
                ("Supported files", "*.pdf *.md"),
                ("PDF files", "*.pdf"),
                ("Markdown files", "*.md"),
                ("All files", "*.*")
            ]
        )

        root.destroy()
        return list(files)

    except Exception as e:
        print(f"File dialog error: {e}")
        return []

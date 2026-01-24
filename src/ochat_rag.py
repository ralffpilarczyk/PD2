import requests
import json
import re
from pathlib import Path
from typing import List, Dict, Tuple, Optional
import chromadb
from chromadb.config import Settings


class RAGManager:
    """Handles chunking, embedding, storage, and retrieval for RAG"""

    # Chunking parameters
    CHUNK_SIZE = 500  # tokens (approx 4 chars per token)
    CHUNK_OVERLAP = 100  # 20% overlap
    CHARS_PER_TOKEN = 4

    # Retrieval parameters
    MIN_CHUNKS = 5
    MAX_CHUNKS = 15
    SCORE_THRESHOLD = 0.3  # ChromaDB returns distances, lower = better

    # Embedding model
    EMBEDDING_MODEL = "nomic-embed-text"
    OLLAMA_BASE_URL = "http://localhost:11434"

    def __init__(self, project_path: Path):
        """Initialize RAG manager for a project

        Args:
            project_path: Path to the project directory
        """
        self.project_path = project_path
        self.chroma_path = project_path / "chroma_db"

        # Initialize ChromaDB with persistent storage
        self.client = chromadb.PersistentClient(
            path=str(self.chroma_path),
            settings=Settings(anonymized_telemetry=False)
        )

        # Get or create the collection for this project
        self.collection = self.client.get_or_create_collection(
            name="sources",
            metadata={"hnsw:space": "cosine"}  # Use cosine similarity
        )

    def chunk_text(self, text: str, source_file: str) -> List[Dict]:
        """Split text into overlapping chunks

        Args:
            text: Full text to chunk
            source_file: Name of source file for metadata

        Returns:
            List of chunk dictionaries with text and metadata
        """
        chunks = []

        # Convert token counts to character counts
        chunk_chars = self.CHUNK_SIZE * self.CHARS_PER_TOKEN
        overlap_chars = self.CHUNK_OVERLAP * self.CHARS_PER_TOKEN

        # Clean the text
        text = text.strip()
        if not text:
            return chunks

        # Split into chunks with overlap
        start = 0
        chunk_index = 0

        while start < len(text):
            end = start + chunk_chars

            # Try to end at a sentence or paragraph boundary
            if end < len(text):
                # Look for sentence end within last 20% of chunk
                search_start = end - (chunk_chars // 5)
                search_region = text[search_start:end + 100]

                # Find best break point
                best_break = None
                for pattern in ['\n\n', '.\n', '. ', '\n']:
                    pos = search_region.rfind(pattern)
                    if pos != -1:
                        best_break = search_start + pos + len(pattern)
                        break

                if best_break and best_break > start:
                    end = best_break

            chunk_text = text[start:end].strip()

            if chunk_text:
                chunks.append({
                    "text": chunk_text,
                    "source_file": source_file,
                    "chunk_index": chunk_index,
                    "start_char": start,
                    "end_char": end
                })
                chunk_index += 1

            # Move start position (with overlap)
            start = end - overlap_chars if end < len(text) else len(text)

        return chunks

    def embed_text(self, text: str) -> List[float]:
        """Get embedding for a text using Ollama

        Args:
            text: Text to embed

        Returns:
            Embedding vector
        """
        response = requests.post(
            f"{self.OLLAMA_BASE_URL}/api/embeddings",
            json={
                "model": self.EMBEDDING_MODEL,
                "prompt": text
            },
            timeout=60
        )
        response.raise_for_status()
        return response.json()["embedding"]

    def embed_texts(self, texts: List[str]) -> List[List[float]]:
        """Get embeddings for multiple texts

        Args:
            texts: List of texts to embed

        Returns:
            List of embedding vectors
        """
        embeddings = []
        for text in texts:
            embedding = self.embed_text(text)
            embeddings.append(embedding)
        return embeddings

    def index_document(self, text: str, source_file: str) -> int:
        """Index a document by chunking and storing embeddings

        Args:
            text: Document text
            source_file: Source filename

        Returns:
            Number of chunks indexed
        """
        # Remove existing chunks for this source file
        self._remove_source(source_file)

        # Chunk the document
        chunks = self.chunk_text(text, source_file)

        if not chunks:
            return 0

        # Prepare data for ChromaDB
        ids = [f"{source_file}_{c['chunk_index']}" for c in chunks]
        documents = [c["text"] for c in chunks]
        metadatas = [
            {
                "source_file": c["source_file"],
                "chunk_index": c["chunk_index"],
                "start_char": c["start_char"],
                "end_char": c["end_char"]
            }
            for c in chunks
        ]

        # Get embeddings
        embeddings = self.embed_texts(documents)

        # Add to ChromaDB
        self.collection.add(
            ids=ids,
            documents=documents,
            embeddings=embeddings,
            metadatas=metadatas
        )

        return len(chunks)

    def _remove_source(self, source_file: str):
        """Remove all chunks for a source file"""
        try:
            # Get all IDs for this source
            results = self.collection.get(
                where={"source_file": source_file}
            )
            if results["ids"]:
                self.collection.delete(ids=results["ids"])
        except Exception:
            pass  # Collection might be empty

    def retrieve(self, query: str) -> List[Dict]:
        """Retrieve relevant chunks for a query

        Args:
            query: User's question

        Returns:
            List of relevant chunks with scores
        """
        # Check if collection has documents
        if self.collection.count() == 0:
            return []

        # Embed the query
        query_embedding = self.embed_text(query)

        # Query ChromaDB
        results = self.collection.query(
            query_embeddings=[query_embedding],
            n_results=self.MAX_CHUNKS,
            include=["documents", "metadatas", "distances"]
        )

        # Process results
        chunks = []
        if results and results["documents"] and results["documents"][0]:
            for i, doc in enumerate(results["documents"][0]):
                distance = results["distances"][0][i] if results["distances"] else 1.0
                metadata = results["metadatas"][0][i] if results["metadatas"] else {}

                # Convert distance to similarity (cosine distance -> similarity)
                similarity = 1 - distance

                chunks.append({
                    "text": doc,
                    "source_file": metadata.get("source_file", "unknown"),
                    "chunk_index": metadata.get("chunk_index", 0),
                    "similarity": similarity,
                    "distance": distance
                })

        # Filter by score threshold and minimum chunks
        good_chunks = [c for c in chunks if c["similarity"] >= (1 - self.SCORE_THRESHOLD)]

        # Ensure minimum chunks
        if len(good_chunks) < self.MIN_CHUNKS:
            good_chunks = chunks[:self.MIN_CHUNKS]

        return good_chunks

    def build_context(self, query: str) -> Tuple[str, int, List[Dict]]:
        """Build context string from retrieved chunks

        Args:
            query: User's question

        Returns:
            Tuple of (context_string, chunk_count, chunk_details)
        """
        chunks = self.retrieve(query)

        if not chunks:
            return "", 0, []

        # Group chunks by source file for better organization
        by_source = {}
        for chunk in chunks:
            source = chunk["source_file"]
            if source not in by_source:
                by_source[source] = []
            by_source[source].append(chunk)

        # Build context string
        context_parts = []
        for source, source_chunks in by_source.items():
            # Sort chunks by index for coherent reading
            source_chunks.sort(key=lambda x: x["chunk_index"])

            context_parts.append(f"\n--- SOURCE: {source} ---\n")
            for chunk in source_chunks:
                context_parts.append(chunk["text"])
                context_parts.append("\n")

        context = "\n".join(context_parts)

        # Estimate tokens
        token_estimate = len(context) // self.CHARS_PER_TOKEN

        return context, len(chunks), chunks

    def get_index_stats(self) -> Dict:
        """Get statistics about the index

        Returns:
            Dictionary with index statistics
        """
        count = self.collection.count()

        # Get unique source files
        if count > 0:
            try:
                all_docs = self.collection.get(include=["metadatas"])
                sources = set()
                for meta in all_docs["metadatas"]:
                    if meta and "source_file" in meta:
                        sources.add(meta["source_file"])
                source_count = len(sources)
            except Exception:
                source_count = 0
        else:
            source_count = 0

        return {
            "total_chunks": count,
            "source_files": source_count,
            "chunk_size": self.CHUNK_SIZE,
            "overlap": self.CHUNK_OVERLAP
        }

    def clear_index(self):
        """Clear all indexed documents"""
        try:
            self.client.delete_collection("sources")
            self.collection = self.client.get_or_create_collection(
                name="sources",
                metadata={"hnsw:space": "cosine"}
            )
        except Exception:
            pass

    @staticmethod
    def check_embedding_model() -> bool:
        """Check if embedding model is available in Ollama"""
        try:
            response = requests.get("http://localhost:11434/api/tags", timeout=5)
            response.raise_for_status()
            models = [m["name"] for m in response.json().get("models", [])]
            return RAGManager.EMBEDDING_MODEL in models or f"{RAGManager.EMBEDDING_MODEL}:latest" in models
        except Exception:
            return False

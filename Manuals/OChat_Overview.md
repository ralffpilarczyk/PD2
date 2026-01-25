# OChat Technical Overview

Version 1.1

---

## Executive Summary

OChat is an interactive chatbot with RAG (Retrieval Augmented Generation) capabilities for document-based Q&A. It uses local Ollama models for privacy and runs independently from PD2, OPP, and DR.

Key characteristics:
- **Local execution**: Uses Ollama for LLM inference (no cloud API)
- **Document support**: PDF and Markdown files via RAG
- **Session-based**: Each conversation saved to timestamped folder
- **Rich terminal UI**: Markdown rendering with color formatting

---

## Architecture Overview

### System Components

```
ochat.py                 Entry point and CLI interface
    |
    v
src/ochat_engine.py      Chat engine wrapper for Ollama
    |                    - OllamaEngine class
    |                    - Conversation history management
    |
    v
src/ochat_sources.py     Source file management
    |                    - SourceManager class
    |                    - PDF conversion via marker-pdf
    |
    v
src/ochat_rag.py         RAG pipeline
    |                    - RAGManager class
    |                    - Chunking, embedding, retrieval
    |
    v
Ollama API               Local LLM and embeddings
    - Chat: gpt-oss:120b (configurable)
    - Embeddings: nomic-embed-text
```

### External Dependencies

| Component | Purpose |
|-----------|---------|
| Ollama | Local LLM inference |
| ChromaDB | Vector storage for RAG |
| marker-pdf | PDF to Markdown conversion |
| Rich | Terminal markdown rendering |

---

## Core Components

### ochat.py (Entry Point)

Main CLI that:
1. Creates session folder with timestamp
2. Displays welcome screen with commands
3. Checks Ollama availability
4. Runs chat loop
5. Saves conversation on exit

### OllamaEngine Class

Chat engine in `src/ochat_engine.py`:

| Method | Purpose |
|--------|---------|
| `chat()` | Send message and get response |
| `set_rag_manager()` | Connect RAG for context retrieval |
| `clear_history()` | Reset conversation |
| `get_last_exchange()` | Get last Q&A pair |
| `get_conversation_length()` | Count exchanges |

**Configuration:**
```python
DEFAULT_MODEL = "gpt-oss:120b"
DEFAULT_TEMPERATURE = 0.3
MAX_CONTEXT_TOKENS = 131072
```

### SourceManager Class

Source file management in `src/ochat_sources.py`:

| Method | Purpose |
|--------|---------|
| `add_file()` | Add PDF or Markdown, auto-index |
| `remove_file()` | Remove file from sources |
| `list_sources()` | List all source files |
| `reindex_all()` | Rebuild RAG index |
| `get_rag_stats()` | Return index statistics |

### RAGManager Class

RAG pipeline in `src/ochat_rag.py`:

| Method | Purpose |
|--------|---------|
| `chunk_text()` | Split text into overlapping chunks |
| `embed_text()` | Get embedding via Ollama |
| `index_document()` | Add document to ChromaDB |
| `retrieve()` | Find relevant chunks for query |
| `build_context()` | Build context string with quality metrics |

**RAG Parameters:**
```python
CHUNK_SIZE = 1024      # tokens
CHUNK_OVERLAP = 150    # tokens (~15%)
MIN_CHUNKS = 8
MAX_CHUNKS = 25
MAX_DISTANCE = 0.5     # similarity threshold
EMBEDDING_MODEL = "nomic-embed-text"
```

---

## Data Flow

### Chat Flow

```
User Input
    |
    v
Command? --yes--> Handle Command (/upload, /sources, etc.)
    |no
    v
RAG Retrieval (if sources indexed)
    |
    v
Build System Prompt + Context
    |
    v
Ollama API (gpt-oss:120b)
    |
    v
Rich Markdown Rendering
    |
    v
Display Response
```

### RAG Flow

```
User Query
    |
    v
Embed Query (nomic-embed-text with "search_query:" prefix)
    |
    v
ChromaDB Similarity Search (max 25 chunks)
    |
    v
Filter by Distance Threshold (0.5)
    |
    v
Build Context with Quality Metrics
    |
    v
Inject into System Prompt
```

---

## Session Management

### Folder Structure

```
ReportsOChat/
    ochat_YYYYMMDD_HHMMSS/
        conversation.md    # Chat transcript
        sources/           # Uploaded files (PDF + converted MD)
        chroma_db/         # ChromaDB vector index
```

### Session Lifecycle

1. **Start**: Create timestamped folder
2. **Chat**: Questions and responses in memory
3. **Upload**: Add files to `sources/`, index to `chroma_db/`
4. **Clear**: Save `conversation.md`, create new folder
5. **Quit**: Save `conversation.md`, exit

### Conversation Format

```markdown
# OChat Conversation
Date: 2026-01-25 14:30:22

---

## User
What are the company's main revenue streams?

## OChat
Based on the documents, the main revenue streams are...

---

## User
[next question]

## OChat
[next response]
```

---

## Commands

| Command | Description |
|---------|-------------|
| `/upload` | Open file selector for PDF/Markdown |
| `/sources` | List source files and RAG stats |
| `/reindex` | Rebuild RAG index from sources |
| `/remove <file>` | Remove a source file |
| `/clear` | Save conversation, start new session |
| `/help` | Show command list |
| `/quit` | Save conversation, exit |

---

## System Prompt

The default system prompt focuses on completeness and accuracy:

```
You are an expert M&A analyst. Your responses must be:

- COMPLETE: Extract and present ALL relevant information from the source
  documents. Never omit details.
- CORRECT: Only state facts that are explicitly supported by the source
  documents. Do not infer or assume.
- CONCISE: No filler, no repetition, no unnecessary qualifiers. Direct prose.

When asked about lists (assets, plants, subsidiaries, etc.), provide the
COMPLETE list from the documents. Missing items is a critical failure.
```

When RAG context is available, retrieval status is included:
- HIGH CONFIDENCE: avg_similarity >= 0.5
- MODERATE CONFIDENCE: avg_similarity 0.3-0.5
- LOW CONFIDENCE: fallback to minimum chunks

---

## Configuration

### Environment

OChat uses Ollama locally. Ensure Ollama is running:
```bash
ollama serve
```

Required models:
```bash
ollama pull gpt-oss:120b       # Chat model (or your preferred model)
ollama pull nomic-embed-text   # Embedding model
```

### Modifying Defaults

In `src/ochat_engine.py`:
```python
DEFAULT_MODEL = "gpt-oss:120b"    # Change chat model
DEFAULT_TEMPERATURE = 0.3         # Adjust creativity
```

In `src/ochat_rag.py`:
```python
CHUNK_SIZE = 1024                 # Tokens per chunk
MAX_CHUNKS = 25                   # Max chunks per query
MAX_DISTANCE = 0.5                # Similarity threshold
```

---

## Error Handling

### Connection Errors
```
[ERROR] Ollama is not running. Start it with: ollama serve
```

### Missing Models
```
[WARNING] Model gpt-oss:120b not found. Available: llama2, mistral
[WARNING] Embedding model nomic-embed-text not found
         Run: ollama pull nomic-embed-text
```

### File Errors
```
[ERROR] File not found: /path/to/file.pdf
[ERROR] Unsupported file type: .docx. Use .pdf or .md
```

---

## Performance

| Metric | Typical Value |
|--------|---------------|
| Response time | 5-30 seconds (depends on model/context) |
| PDF conversion | 10-60 seconds (depends on size) |
| RAG indexing | 1-5 seconds per file |
| Memory usage | 2-8 GB (depends on model) |

---

## File Reference

| File | Purpose |
|------|---------|
| `ochat.py` | Entry point, CLI interface |
| `src/ochat_engine.py` | OllamaEngine class, chat logic |
| `src/ochat_sources.py` | SourceManager class, file handling |
| `src/ochat_rag.py` | RAGManager class, vector search |
| `ReportsOChat/` | Session output folders |
| `SourceCache/` | Shared PDF conversion cache |

---

## Dependencies

### Python Packages

```
rich                    # Terminal markdown rendering
chromadb                # Vector database
marker-pdf              # PDF conversion
requests                # Ollama API calls
```

### System Requirements

- Ollama installed and running
- 8GB RAM minimum (16GB recommended for large models)
- macOS, Linux, or Windows with WSL

---

## Limitations

1. **Local only**: Requires Ollama running locally
2. **Model dependent**: Quality depends on chosen LLM
3. **PDF quality**: Complex PDFs may not convert well
4. **Context window**: Limited by model's context size (128K for gpt-oss)
5. **No streaming**: Responses appear after full generation

---

## Version History

| Version | Changes |
|---------|---------|
| 1.1 | Fix embedding crash on long markdown table separators |
| 1.0 | Initial release with RAG, Rich UI, session management |

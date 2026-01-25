#!/usr/bin/env python3
"""
OChat - Ollama-based Chat Application
Simple chatbot with RAG capabilities. Each session is saved automatically.

Usage:
    python ochat.py
"""

import sys
import os
import readline  # Enable cursor movement and history in input
from pathlib import Path
from datetime import datetime

from rich.console import Console
from rich.markdown import Markdown

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from src.ochat_engine import OllamaEngine
from src.ochat_sources import SourceManager, select_source_files
from src.ochat_rag import RAGManager

console = Console()

__version__ = "1.1"

REPORTS_DIR = Path("ReportsOChat")


def clear_screen():
    """Clear the terminal screen"""
    os.system('cls' if os.name == 'nt' else 'clear')


def create_session_folder() -> Path:
    """Create a new session folder with timestamp"""
    REPORTS_DIR.mkdir(exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    session_path = REPORTS_DIR / f"ochat_{timestamp}"
    session_path.mkdir(exist_ok=True)
    return session_path


def print_welcome(session_path: Path):
    """Print welcome screen with title and commands"""
    clear_screen()
    print()
    print("=" * 60)
    print()
    print("   OCHAT " + __version__)
    print("   Ollama Chat with RAG")
    print()
    print("=" * 60)
    print()
    print(f"Session: {session_path.name}")
    print()
    print("Commands:")
    print()
    print("  Sources:")
    print("    /upload        - Add source files (PDF, Markdown)")
    print("    /sources       - List source files and index stats")
    print("    /reindex       - Rebuild RAG index")
    print("    /remove <file> - Remove a source file")
    print()
    print("  Session:")
    print("    /clear         - Save and start new conversation")
    print("    /help          - Show commands")
    print("    /quit           - Save and exit")
    print()
    print("=" * 60)
    print()


def print_help():
    """Print available commands"""
    print()
    print("Commands:")
    print()
    print("  Sources:")
    print("    /upload        - Add source files (PDF, Markdown)")
    print("    /sources       - List source files and index stats")
    print("    /reindex       - Rebuild RAG index")
    print("    /remove <file> - Remove a source file")
    print()
    print("  Session:")
    print("    /clear         - Save and start new conversation")
    print("    /help          - Show commands")
    print("    /quit           - Save and exit")
    print()


def check_system() -> bool:
    """Check system requirements"""
    if not OllamaEngine.check_ollama_running():
        print("[ERROR] Ollama is not running. Start it with: ollama serve")
        return False

    models = OllamaEngine.list_models()
    target_model = OllamaEngine.DEFAULT_MODEL

    if target_model not in models:
        if not models:
            print("[ERROR] No models available in Ollama")
            return False
        print(f"[WARNING] Model {target_model} not found. Available: {', '.join(models)}")

    print(f"[OK] Chat model: {target_model}")

    if RAGManager.check_embedding_model():
        print(f"[OK] Embedding model: {RAGManager.EMBEDDING_MODEL}")
    else:
        print(f"[WARNING] Embedding model {RAGManager.EMBEDDING_MODEL} not found")
        print(f"         Run: ollama pull {RAGManager.EMBEDDING_MODEL}")

    print()
    return True


def save_conversation(session_path: Path, engine: OllamaEngine):
    """Save conversation to markdown file"""
    if engine.get_conversation_length() == 0:
        return

    content = f"# OChat Conversation\nDate: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n"

    history = engine.conversation_history
    for i in range(0, len(history), 2):
        if i + 1 < len(history):
            user_msg = history[i]["content"]
            assistant_msg = history[i + 1]["content"]
            content += f"---\n\n## User\n{user_msg}\n\n## OChat\n{assistant_msg}\n\n"

    conv_file = session_path / "conversation.md"
    with open(conv_file, "w", encoding="utf-8") as f:
        f.write(content)

    print(f"[Saved: {conv_file}]")


def handle_command(command: str, engine: OllamaEngine, sm: SourceManager, session_path: Path) -> str:
    """Handle a slash command

    Returns:
        'exit', 'clear', or None
    """
    parts = command.split(maxsplit=1)
    cmd = parts[0].lower()
    arg = parts[1] if len(parts) > 1 else None

    if cmd == '/exit' or cmd == '/quit' or cmd == '/quit':
        save_conversation(session_path, engine)
        print("\nGoodbye!")
        return 'exit'

    elif cmd == '/help':
        print_help()

    elif cmd == '/upload' or cmd == '/add':
        print("\nOpening file selector...")
        files = select_source_files()

        if not files:
            print("No files selected.\n")
            return None

        print(f"Adding {len(files)} file(s)...")
        for file_path in files:
            success, msg = sm.add_file(file_path)
            status = "[OK]" if success else "[ERROR]"
            print(f"  {status} {msg}")
        print()

    elif cmd == '/sources':
        sources = sm.list_sources()
        stats = sm.get_rag_stats()

        if sources:
            print(f"\nSource files ({len(sources)}):")
            for src in sources:
                print(f"  {src['filename']} ({src['size']})")

            print(f"\nRAG Index:")
            print(f"  Chunks: {stats['total_chunks']}")
            print(f"  Chunk size: {stats['chunk_size']} tokens")
            print(f"  Overlap: {stats['overlap']} tokens")
            print()
        else:
            print("\nNo source files. Use /upload to add files.\n")

    elif cmd == '/reindex':
        print("\nRebuilding RAG index...")
        files, chunks = sm.reindex_all()
        print(f"Indexed {files} files into {chunks} chunks.\n")

    elif cmd == '/remove':
        if not arg:
            print("\nUsage: /remove <filename>\n")
        else:
            success, msg = sm.remove_file(arg)
            status = "[OK]" if success else "[ERROR]"
            print(f"\n{status} {msg}\n")

    elif cmd == '/clear':
        save_conversation(session_path, engine)
        return 'clear'

    else:
        print(f"\nUnknown command: {cmd}")
        print("Type /help for commands.\n")

    return None


def format_tokens(n: int) -> str:
    """Format token count for display (e.g., 5K, 128K)"""
    if n >= 1000:
        return f"{n // 1000}K"
    return str(n)


def chat_loop(engine: OllamaEngine, sm: SourceManager, session_path: Path) -> str:
    """Main chat loop

    Returns:
        'exit' or 'clear'
    """
    print("Type your question, or /help for commands.")
    print()

    while True:
        try:
            ctx = engine.get_context_status()
            used = format_tokens(ctx['total_tokens'])
            total = format_tokens(ctx['max_tokens'])
            console.print(f"[dim][{used}/{total}][/dim] [green]You: [/green]", end="")
            user_input = input().strip()
        except EOFError:
            return 'exit'
        except KeyboardInterrupt:
            print("\n")
            continue

        if not user_input:
            continue

        if user_input.startswith('/'):
            result = handle_command(user_input, engine, sm, session_path)
            if result:
                return result
            continue

        console.print()

        try:
            # Show thinking indicator
            with console.status("Thinking..."):
                response = engine.chat(user_input, stream=False)

            # Display response with formatted markdown
            console.print("OChat:")
            console.print(Markdown(response))

            stats = engine.get_last_retrieval_stats()
            if stats['chunks'] > 0:
                console.print(f"[dim][Retrieved {stats['chunks']} chunks, ~{stats['tokens']} tokens][/dim]")

        except ConnectionError as e:
            console.print(f"[red][ERROR] {e}[/red]")
            continue
        except Exception as e:
            console.print(f"[red][ERROR] Failed to get response: {e}[/red]")
            continue

        console.print()
        console.rule(style="dim")


def main():
    """Main entry point"""
    # Create initial session
    session_path = create_session_folder()

    # Show welcome
    print_welcome(session_path)

    # System check
    if not check_system():
        print("\nFix the issues above and try again.")
        sys.exit(1)

    # Main loop
    while True:
        # Initialize engine and source manager for this session
        engine = OllamaEngine()
        sm = SourceManager(session_path)
        engine.set_rag_manager(sm.rag)

        # Chat
        result = chat_loop(engine, sm, session_path)

        if result == 'exit':
            break
        elif result == 'clear':
            # Create new session
            session_path = create_session_folder()
            clear_screen()
            print()
            print("=" * 60)
            print(f"  New session: {session_path.name}")
            print("=" * 60)
            print()
            continue


if __name__ == "__main__":
    main()

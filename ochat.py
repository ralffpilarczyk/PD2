#!/usr/bin/env python3
"""
OChat - Ollama-based Chat Application
A NotebookLM-style chatbot with project organization, note-saving, and RAG capabilities.

Features:
- Project-based organization
- PDF and Markdown source support
- RAG (Retrieval Augmented Generation) for focused context
- Save Q&A exchanges as notes

Usage:
    python ochat.py
"""

import sys
import os

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from src.ochat_engine import OllamaEngine
from src.ochat_project import ProjectManager
from src.ochat_sources import SourceManager, select_source_files
from src.ochat_rag import RAGManager

__version__ = "3.0"


def clear_screen():
    """Clear the terminal screen"""
    os.system('cls' if os.name == 'nt' else 'clear')


def print_welcome():
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
    print("Commands:")
    print()
    print("  Sources:")
    print("    /upload        - Add source files (PDF, Markdown)")
    print("    /sources       - List source files and index stats")
    print("    /reindex       - Rebuild RAG index")
    print("    /remove <file> - Remove a source file")
    print()
    print("  Notes:")
    print("    /save [title]  - Save the last Q&A as a note")
    print("    /notes         - List saved notes")
    print("    /read <note>   - Read a note")
    print("    /delete <note> - Delete a note")
    print()
    print("  Session:")
    print("    /info          - Show project and RAG stats")
    print("    /clear         - Clear conversation history")
    print("    /switch        - Switch project")
    print("    /help          - Show commands")
    print("    /exit          - Exit")
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
    print("  Notes:")
    print("    /save [title]  - Save the last Q&A as a note")
    print("    /notes         - List saved notes")
    print("    /read <note>   - Read a note")
    print("    /delete <note> - Delete a note")
    print()
    print("  Session:")
    print("    /info          - Show project and RAG stats")
    print("    /clear         - Clear conversation history")
    print("    /switch        - Switch project")
    print("    /help          - Show commands")
    print("    /exit          - Exit")
    print()


def check_system() -> bool:
    """Check system requirements (compact output)"""
    # Check Ollama
    if not OllamaEngine.check_ollama_running():
        print("[ERROR] Ollama is not running. Start it with: ollama serve")
        return False

    # Check chat model availability
    models = OllamaEngine.list_models()
    target_model = OllamaEngine.DEFAULT_MODEL

    if target_model not in models:
        if not models:
            print(f"[ERROR] No models available in Ollama")
            return False
        print(f"[WARNING] Model {target_model} not found. Available: {', '.join(models)}")

    print(f"[OK] Chat model: {target_model}")

    # Check embedding model
    if RAGManager.check_embedding_model():
        print(f"[OK] Embedding model: {RAGManager.EMBEDDING_MODEL}")
    else:
        print(f"[WARNING] Embedding model {RAGManager.EMBEDDING_MODEL} not found")
        print(f"         Run: ollama pull {RAGManager.EMBEDDING_MODEL}")

    print()
    return True


def select_or_create_project(pm: ProjectManager, first_run: bool = True) -> bool:
    """Project selection or creation flow"""
    if not first_run:
        clear_screen()
        print()
        print("=" * 60)
        print("  Project Selection")
        print("=" * 60)
        print()

    projects = pm.list_projects()

    if projects:
        print("Existing projects:")
        print()
        for i, proj in enumerate(projects, 1):
            note_str = f"{proj['note_count']} notes" if proj['note_count'] != 1 else "1 note"
            print(f"  {i}. {proj['name']} ({note_str})")
        print()
        print("  N - Create new project")
        print("  Q - Quit")
        print()

        while True:
            choice = input("Select: ").strip()

            if choice.upper() == 'Q':
                return False

            if choice.upper() == 'N':
                return create_new_project(pm)

            try:
                idx = int(choice) - 1
                if 0 <= idx < len(projects):
                    project_name = projects[idx]['name']
                    if pm.open_project(project_name):
                        return True
                    else:
                        print("Failed to open project")
                else:
                    print("Invalid selection")
            except ValueError:
                print("Enter a number, N, or Q")

    else:
        print("No existing projects. Let's create one.")
        print()
        return create_new_project(pm)


def create_new_project(pm: ProjectManager) -> bool:
    """Create a new project"""
    while True:
        name = input("Project name (or 'cancel'): ").strip()

        if name.lower() == 'cancel':
            return False

        if not name:
            print("Name cannot be empty")
            continue

        if pm.create_project(name):
            return True
        else:
            print(f"'{name}' already exists. Choose a different name.")


def setup_rag(engine: OllamaEngine, pm: ProjectManager) -> SourceManager:
    """Setup RAG manager and connect to engine"""
    sm = pm.get_source_manager()
    if sm:
        # Connect RAG manager to engine
        engine.set_rag_manager(sm.rag)
    return sm


def enter_chat(engine: OllamaEngine, pm: ProjectManager, sm: SourceManager):
    """Enter chat mode with clear screen and project header"""
    clear_screen()
    print()
    print("=" * 60)
    print(f"  Project: {pm.current_project}")

    # Show RAG stats
    if sm:
        stats = sm.get_rag_stats()
        source_count = sm.get_source_count()
        if stats['total_chunks'] > 0:
            print(f"  Sources: {source_count} files, {stats['total_chunks']} chunks indexed")
        elif source_count > 0:
            print(f"  Sources: {source_count} files (not indexed - run /reindex)")
        else:
            print("  Sources: None (use /upload to add)")

    print("=" * 60)
    print()
    print("Type your question, or /help for commands.")
    print()


def chat_loop(engine: OllamaEngine, pm: ProjectManager):
    """Main chat loop"""
    # Setup RAG
    sm = setup_rag(engine, pm)

    enter_chat(engine, pm, sm)

    while True:
        try:
            user_input = input("You: ").strip()
        except EOFError:
            break
        except KeyboardInterrupt:
            print("\n")
            continue

        if not user_input:
            continue

        # Handle commands
        if user_input.startswith('/'):
            command_result = handle_command(user_input, engine, pm, sm)
            if command_result == 'exit':
                break
            elif command_result == 'switch':
                return 'switch'
            continue

        # Send to AI
        print()
        print("Assistant: ", end="", flush=True)

        try:
            response = engine.chat(user_input, stream=True)
            print()

            # Show retrieval stats
            stats = engine.get_last_retrieval_stats()
            if stats['chunks'] > 0:
                print(f"  [Retrieved {stats['chunks']} chunks, ~{stats['tokens']} tokens]")

        except ConnectionError as e:
            print(f"\n[ERROR] {e}")
            continue
        except Exception as e:
            print(f"\n[ERROR] Failed to get response: {e}")
            continue

        print()


def handle_command(command: str, engine: OllamaEngine, pm: ProjectManager, sm: SourceManager) -> str:
    """Handle a slash command

    Returns:
        'exit', 'switch', or None
    """
    parts = command.split(maxsplit=1)
    cmd = parts[0].lower()
    arg = parts[1] if len(parts) > 1 else None

    # Exit commands
    if cmd == '/exit' or cmd == '/quit':
        print("\nGoodbye!")
        return 'exit'

    elif cmd == '/help':
        print_help()

    # Source commands
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

    # Note commands
    elif cmd == '/save':
        exchange = engine.get_last_exchange()
        if exchange:
            title = arg if arg else None
            filename = pm.save_note(exchange['question'], exchange['response'], title)
            print(f"\nSaved: {filename}\n")
        else:
            print("\nNo conversation to save yet.\n")

    elif cmd == '/notes':
        notes = pm.list_notes()
        if notes:
            print(f"\nSaved notes ({len(notes)}):")
            for i, note in enumerate(notes, 1):
                print(f"  {i}. {note['title'][:50]}")
                print(f"     {note['filename']}")
            print()
        else:
            print("\nNo saved notes yet.\n")

    elif cmd == '/read':
        if not arg:
            print("\nUsage: /read <filename>\n")
        else:
            content = pm.read_note(arg)
            if content:
                print()
                print("-" * 40)
                print(content)
                print("-" * 40)
                print()
            else:
                print(f"\nNote not found: {arg}\n")

    elif cmd == '/delete':
        if not arg:
            print("\nUsage: /delete <filename>\n")
        else:
            if pm.delete_note(arg):
                print(f"\nDeleted: {arg}\n")
            else:
                print(f"\nNote not found: {arg}\n")

    # Session commands
    elif cmd == '/info':
        info = pm.get_project_info()
        if info:
            print(f"\nProject: {info['name']}")
            print(f"Created: {info['created'][:10]}")
            print(f"Notes: {info['note_count']}")

            # Source info
            source_count = sm.get_source_count() if sm else 0
            print(f"Sources: {source_count} files")

            # RAG stats
            stats = sm.get_rag_stats()
            print(f"RAG chunks: {stats['total_chunks']}")

            # Last retrieval
            retrieval = engine.get_last_retrieval_stats()
            if retrieval['chunks'] > 0:
                print(f"Last retrieval: {retrieval['chunks']} chunks, ~{retrieval['tokens']} tokens")

            print(f"Exchanges: {engine.get_conversation_length()}\n")

    elif cmd == '/clear':
        engine.clear_history()
        print("\nConversation history cleared.\n")

    elif cmd == '/switch':
        return 'switch'

    else:
        print(f"\nUnknown command: {cmd}")
        print("Type /help for commands.\n")

    return None


def main():
    """Main entry point"""
    # Show welcome screen
    print_welcome()

    # System check
    if not check_system():
        print("\nFix the issues above and try again.")
        sys.exit(1)

    # Initialize project manager
    pm = ProjectManager()
    engine = None
    first_run = True

    # Main application loop
    while True:
        # Project selection
        if not select_or_create_project(pm, first_run=first_run):
            print("\nGoodbye!")
            break

        first_run = False

        # Initialize chat engine (fresh for each project session)
        engine = OllamaEngine()

        # Enter chat loop
        result = chat_loop(engine, pm)

        if result == 'switch':
            continue
        else:
            break


if __name__ == "__main__":
    main()

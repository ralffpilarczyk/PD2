import requests
import json
from typing import List, Dict, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from .ochat_rag import RAGManager


class OllamaEngine:
    """Wrapper for Ollama API to handle chat interactions"""

    DEFAULT_MODEL = "gpt-oss:120b"
    DEFAULT_TEMPERATURE = 0.6
    OLLAMA_BASE_URL = "http://localhost:11434"
    MAX_CONTEXT_TOKENS = 131072  # Model's context window
    CONTEXT_WARNING_THRESHOLD = 100000  # Warn when approaching limit

    DEFAULT_SYSTEM_PROMPT = """You are a seasoned bulge bracket M&A banker with a track record of leading complex transactions globally. You have deep expertise in:

- Mergers, acquisitions, and divestitures across all sectors
- Valuation methodologies (DCF, comparable companies, precedent transactions)
- Deal structuring, negotiation tactics, and transaction execution
- Due diligence processes and risk assessment
- Capital markets, financing structures, and leveraged transactions
- Cross-border transactions and regulatory considerations

You provide insightful, direct, and data-driven advice. You think like an investor and focus on value creation, deal dynamics, and strategic rationale. You are concise but thorough, and you always ground your analysis in facts and sound reasoning."""

    def __init__(self, model: str = None, temperature: float = None, system_prompt: str = None):
        """Initialize the Ollama engine

        Args:
            model: Ollama model name (default: gpt-oss:120b)
            temperature: Generation temperature 0.0-1.0 (default: 0.6)
            system_prompt: Custom system prompt (default: M&A banker persona)
        """
        self.model = model or self.DEFAULT_MODEL
        self.temperature = temperature if temperature is not None else self.DEFAULT_TEMPERATURE
        self.system_prompt = system_prompt or self.DEFAULT_SYSTEM_PROMPT
        self.conversation_history: List[Dict[str, str]] = []

        # RAG manager reference (set externally)
        self.rag_manager: Optional["RAGManager"] = None

        # Track last retrieval stats for display
        self.last_retrieval_chunks: int = 0
        self.last_retrieval_tokens: int = 0

    def set_rag_manager(self, rag_manager: "RAGManager"):
        """Set the RAG manager for context retrieval

        Args:
            rag_manager: RAGManager instance for the current project
        """
        self.rag_manager = rag_manager

    def _build_messages(self, user_message: str) -> List[Dict[str, str]]:
        """Build the full message list including system prompt, retrieved context, and history"""
        source_context = ""
        self.last_retrieval_chunks = 0
        self.last_retrieval_tokens = 0

        # Use RAG to retrieve relevant context
        if self.rag_manager and self.rag_manager.collection.count() > 0:
            context, chunk_count, chunks = self.rag_manager.build_context(user_message)
            if context:
                source_context = context
                self.last_retrieval_chunks = chunk_count
                self.last_retrieval_tokens = len(context) // 4

        # Build system prompt with source context if available
        if source_context:
            full_system = f"""{self.system_prompt}

--- RELEVANT SOURCE EXCERPTS ---

The following excerpts from source documents are relevant to the user's question. Use this information to inform your response.

{source_context}

--- END SOURCE EXCERPTS ---

When answering:
1. Prioritize information from the source excerpts when relevant
2. Cite which source the information comes from
3. If the excerpts don't fully address the question, supplement with your general knowledge and note when doing so"""
        else:
            full_system = self.system_prompt

        messages = [{"role": "system", "content": full_system}]
        messages.extend(self.conversation_history)
        messages.append({"role": "user", "content": user_message})
        return messages

    def chat(self, user_message: str, stream: bool = False) -> str:
        """Send a message and get a response

        Args:
            user_message: The user's input message
            stream: Whether to stream the response (for real-time display)

        Returns:
            The assistant's response text
        """
        messages = self._build_messages(user_message)

        payload = {
            "model": self.model,
            "messages": messages,
            "stream": stream,
            "options": {
                "temperature": self.temperature
            }
        }

        try:
            if stream:
                return self._stream_response(payload, user_message)
            else:
                return self._blocking_response(payload, user_message)
        except requests.exceptions.ConnectionError:
            raise ConnectionError(
                "Cannot connect to Ollama. Is it running?\n"
                "Start it with: ollama serve"
            )

    def _blocking_response(self, payload: dict, user_message: str) -> str:
        """Get a complete response (blocking)"""
        response = requests.post(
            f"{self.OLLAMA_BASE_URL}/api/chat",
            json=payload,
            timeout=300  # 5 minute timeout for large models
        )
        response.raise_for_status()

        result = response.json()
        assistant_message = result.get("message", {}).get("content", "")

        # Update conversation history
        self.conversation_history.append({"role": "user", "content": user_message})
        self.conversation_history.append({"role": "assistant", "content": assistant_message})

        return assistant_message

    def _stream_response(self, payload: dict, user_message: str) -> str:
        """Stream the response for real-time display"""
        response = requests.post(
            f"{self.OLLAMA_BASE_URL}/api/chat",
            json=payload,
            stream=True,
            timeout=300
        )
        response.raise_for_status()

        full_response = ""
        for line in response.iter_lines():
            if line:
                chunk = json.loads(line)
                content = chunk.get("message", {}).get("content", "")
                if content:
                    print(content, end="", flush=True)
                    full_response += content

                # Check if done
                if chunk.get("done", False):
                    break

        print()  # New line after streaming completes

        # Update conversation history
        self.conversation_history.append({"role": "user", "content": user_message})
        self.conversation_history.append({"role": "assistant", "content": full_response})

        return full_response

    def clear_history(self):
        """Clear the conversation history"""
        self.conversation_history = []

    def get_last_response(self) -> Optional[str]:
        """Get the last assistant response from history"""
        for msg in reversed(self.conversation_history):
            if msg["role"] == "assistant":
                return msg["content"]
        return None

    def get_last_exchange(self) -> Optional[Dict[str, str]]:
        """Get the last Q&A exchange (question and response)"""
        last_user = None
        last_assistant = None

        for msg in reversed(self.conversation_history):
            if msg["role"] == "assistant" and last_assistant is None:
                last_assistant = msg["content"]
            elif msg["role"] == "user" and last_user is None:
                last_user = msg["content"]

            if last_user and last_assistant:
                break

        if last_user and last_assistant:
            return {"question": last_user, "response": last_assistant}
        return None

    def get_conversation_length(self) -> int:
        """Get the number of exchanges in the conversation"""
        return len([m for m in self.conversation_history if m["role"] == "user"])

    def get_last_retrieval_stats(self) -> Dict:
        """Get stats from the last RAG retrieval"""
        return {
            "chunks": self.last_retrieval_chunks,
            "tokens": self.last_retrieval_tokens
        }

    def get_context_status(self) -> Dict:
        """Get the current context usage status

        Returns:
            Dict with token estimates and warning status
        """
        # Estimate conversation tokens
        conv_chars = sum(len(m["content"]) for m in self.conversation_history)
        conv_tokens = conv_chars // 4

        # System prompt tokens
        system_tokens = len(self.system_prompt) // 4

        # RAG context tokens (from last retrieval)
        rag_tokens = self.last_retrieval_tokens

        # Total estimate
        total_tokens = system_tokens + rag_tokens + conv_tokens

        return {
            "system_tokens": system_tokens,
            "rag_tokens": rag_tokens,
            "conversation_tokens": conv_tokens,
            "total_tokens": total_tokens,
            "max_tokens": self.MAX_CONTEXT_TOKENS,
            "usage_percent": (total_tokens / self.MAX_CONTEXT_TOKENS) * 100,
            "warning": total_tokens > self.CONTEXT_WARNING_THRESHOLD
        }

    @staticmethod
    def check_ollama_running() -> bool:
        """Check if Ollama server is running"""
        try:
            response = requests.get("http://localhost:11434/api/tags", timeout=5)
            return response.status_code == 200
        except requests.exceptions.ConnectionError:
            return False

    @staticmethod
    def list_models() -> List[str]:
        """List available Ollama models"""
        try:
            response = requests.get("http://localhost:11434/api/tags", timeout=5)
            response.raise_for_status()
            data = response.json()
            return [model["name"] for model in data.get("models", [])]
        except Exception:
            return []

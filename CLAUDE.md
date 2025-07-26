# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview
ProfileDash 2.0 is an intelligent document analysis system for financial analysis. It processes PDF financial documents (annual reports, financial statements, presentations) and generates comprehensive company profiles with 32 different analytical sections.

## Architecture
The project uses a clean modular architecture with all core components in the `src/` package:
- `PD2.py` - Main application entry point with UI and orchestration
- `src/core_analyzer.py` - 4-step analysis pipeline (draft → completeness → insights → polish)
- `src/insight_memory.py` - Learning system with section-based memory
- `src/profile_generator.py` - HTML report generation
- `src/utils.py` - Shared utilities including rate limiting

## Key Commands
```bash
# Run the application
python PD2.py

# Install dependencies
pip install -r requirements.txt
```

## Development Guidelines
1. **API Configuration**: Ensure `GEMINI_API_KEY` is set in `.env` file
2. **Rate Limiting**: All Gemini API calls must use `retry_with_backoff` from `src/utils.py`
3. **Memory System**: Learning insights stored in `memory/learning_memory.json` - section-based, max 30 words, quality score 9-10
4. **Section 32**: Special handling - no word limits, pure data extraction
5. **Parallel Processing**: Use ThreadPoolExecutor with 1-3 workers (2 recommended)

## Code Style & Communication
1. **No Emojis**: Do not use emojis in code, comments, commit messages, or documentation
2. **Professional Tone**: Maintain clear, concise technical communication
3. **Comments**: Only add comments when explicitly requested

## Analysis Pipeline
Each section goes through 4 refinement steps with different word limits and temperatures:
1. Initial Draft (1000 words, temp varies by section)
2. Completeness Critique (1000 words, temp 0.2)
3. Insight Critique (700 words, temp 0.9)
4. Polish Critique (500 words, temp 0.6)

## Important Patterns
- Section definitions in `profile_sections.py` organized into 5 groups
- Thread-safe operations for parallel processing
- Two-stage quality filtering for learning insights
- Exponential backoff for API rate limits (up to 3 retries)
- Professional HTML output with markdown-to-HTML conversion

## Testing & Quality
No formal test suite exists. When making changes:
- Test with sample PDFs in `SourceData/`
- Monitor `runs/run_*/run_summary.txt` for errors
- Check `quality_metrics/` for tracking data
- Verify HTML output renders correctly

## Known Limitations
- Depends on Marker library for PDF conversion
- Basic table rendering in HTML output
- No formal linting or type checking setup
- LLM tends to inflate quality scores (requires harsh filtering)
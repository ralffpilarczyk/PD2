# CLAUDE.md

This file provides guidance to AI assistants when working with code in this repository.

## Project Overview
ProfileDash 2.0 is an intelligent document analysis system that processes PDF financial documents (annual reports, financial statements, investor presentations) and generates comprehensive company profiles with 32 different analytical sections using Google's Gemini API.

## Architecture
The project uses a clean modular architecture with all core components in the `src/` package:
- `PD2.py` - Main application entry point with UI and orchestration
- `src/core_analyzer.py` - Multi-step analysis pipeline with progressive refinement
- `src/insight_memory.py` - Learning system that captures analytical patterns
- `src/profile_generator.py` - HTML report generation with markdown processing
- `src/utils.py` - Shared utilities including rate limiting and markdown fixes
- `src/file_manager.py` - Handles file I/O and markdown preprocessing
- `src/profile_sections.py` - Defines all 32 analysis sections

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
5. **Parallel Processing**: Section analysis uses ThreadPoolExecutor (1-5 workers configurable)
6. **PDF Conversion**: Uses single worker to avoid PyTorch tensor memory issues in Marker library

## Code Style & Communication
1. **No Emojis**: Do not use emojis in code, comments, commit messages, or documentation
2. **Professional Tone**: Maintain clear, concise technical communication
3. **Comments**: Only add comments when explicitly requested

## Core Design Principles

### 1. Config-first, principle-based design
- All behavior (semantics, thresholds, strategies, limits) must be defined in configuration, not code
- No hardcoded values, pairs, or workflow assumptions; the configuration is the single source of truth
- The code should be generic and parameterized, able to adapt to unseen domains without changes

### 2. Domain-agnostic semantics and universal resolution
- Represent properties with semantic metadata (e.g., category, polarity, dimensions) to detect contradictions generically
- Resolve conflicts using universal tie-breakers: observed over derived, negative over positive, shorter paths over longer, higher confidence over lower
- Compute and propagate effective confidence (source × rule × depth), and always preserve causal chains

### 3. Transparent, deterministic, and safe outputs
- Every decision must be explainable ("why"), including blockers, policies applied, and confidence used
- Outputs must be de-duplicated, stably ordered, and include unique counts and provenance for auditability
- Reasoning must be bounded and efficient (depth caps, memoization, precompiled patterns), and safe (sanitized dynamic strings, graceful dependency checks)

## Deep Analysis Methodology
When analyzing company data, always apply multi-layer analytical thinking:

1. **Surface vs. Reality**: Identify contradictions between management claims and actual data
2. **Calculate Hidden Metrics**: Derive ratios and relationships not explicitly provided
3. **Pattern Recognition**: Find correlations across time periods and different data sets
4. **Relevance Filter**: Every insight must pass "Does this matter to company prospects?"
5. **Logic Test**: Every insight must pass "Does this make business sense?"
6. **Data Density**: Maximum insights per word - eliminate all fluff and corporate language
7. **Contradiction Highlighting**: Always flag where management narrative diverges from data reality

**Quality Standard**: Would this insight change an investor's view of the company's prospects? If no, remove it.

## Analysis Pipeline
Each section goes through multiple refinement steps with progressive condensation:
1. Initial Draft - Comprehensive analysis with all relevant data
2. Completeness Check - Identifies missing critical information
3. Enhanced Draft - Incorporates missing elements
4. Deep Analysis & Polish - Condenses to ~500 words of essential insights
5. (Optional) Discovery Pipeline - Additional pattern finding for deeper insights
6. Learning Extraction - Captures reusable analytical methodologies

## Important Patterns
- Section definitions in `profile_sections.py` organized into 5 groups
- Thread-safe operations for parallel processing (using thread_safe_print from utils.py)
- Two-stage quality filtering for learning insights
- Exponential backoff for API rate limits (up to 3 retries)
- Professional HTML output with markdown-to-HTML conversion
- All print statements should use thread_safe_print to avoid garbled output in parallel mode

## Testing & Quality
No formal test suite exists. When making changes:
- Test with sample PDFs
- Monitor `runs/run_*/run_summary.txt` for errors
- Check `quality_metrics/` for tracking data
- Verify HTML output renders correctly

## Recent Improvements
- **Markdown Table Fixes**: Automatic correction of corrupted tables from PDF conversion
- **HTML Rendering**: Fixed table rendering and Section 32 code block issues
- **Workflow Optimization**: All user selections happen upfront before processing
- **Safety Features**: Output size limits prevent excessive LLM generation
- **Thread Safety**: All operations use thread-safe printing for parallel processing

## Output Structure
```
runs/
└── run_YYYY_MM_DD_HH_MM_SS/
    ├── section_1/
    │   ├── step_1_initial_draft.md
    │   ├── step_2_completeness_check.txt
    │   ├── step_3_enhanced_draft.md
    │   ├── step_4_final_section.md
    │   └── step_6_learning.json
    ├── [Company]_profile.md      # Combined markdown
    ├── [Company]_profile.html    # Final HTML output
    └── run_summary.txt          # Processing summary
```

## Known Limitations
- Depends on Marker library for PDF conversion (can have table corruption issues)
- PDF conversion limited to single worker due to PyTorch tensor memory constraints
- No formal test suite or type checking
- LLM quality scoring requires aggressive filtering (9-10 threshold)
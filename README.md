# ProfileDash 2.0

ProfileDash 2.0 (PD2) is an intelligent document analysis system that processes PDF financial documents (annual reports, financial statements, investor presentations) and generates comprehensive company profiles with 32 analytical sections using Google's Gemini API.

## Table of Contents

1. [System Requirements](#system-requirements)
2. [Installation](#installation)
3. [Configuration](#configuration)
4. [Using ProfileDash 2.0](#using-profiledash-20)
5. [Understanding the Output](#understanding-the-output)
6. [File Organization](#file-organization)
7. [Technical Details](#technical-details)
8. [Troubleshooting](#troubleshooting)
9. [License](#license)

## Features

- **Multi-step Analysis Pipeline**: Progressive refinement with completeness checks and deep analysis
- **Learning System**: Captures analytical patterns for continuous improvement
- **Parallel Processing**: Configurable thread workers (1-5) for faster analysis
- **32 Analytical Sections**: Comprehensive coverage from financials to ESG metrics
- **Professional HTML Reports**: Clean, navigable output with markdown support
- **Smart Caching**: Converted documents cached for faster reprocessing

## System Requirements

- **Python**: 3.8 or higher
- **RAM**: 8 GB minimum (16 GB recommended)
- **Storage**: 5 GB free space
- **API**: Google Gemini API key (free tier available at https://makersuite.google.com/app/apikey)

## Installation

1. **Clone the repository**:
   ```bash
   git clone https://github.com/yourusername/PD2.git
   cd PD2
   ```

2. **Install dependencies**:
   ```bash
   pip install -r requirements.txt
   ```

## Configuration

1. **Create `.env` file**:
   ```bash
   cp .env.example .env
   ```

2. **Add your Gemini API key**:
   ```
   GEMINI_API_KEY=your-api-key-here
   ```

## Usage

1. **Place PDFs in SourceData folder**:
   ```bash
   mkdir -p SourceData
   # Add your PDF files to SourceData/
   ```

2. **Run the application**:
   ```bash
   python PD2.py
   ```

3. **Select documents**: File picker will open to choose PDFs

4. **Choose analysis sections**:
   - Group 1: Company Overview (sections 1-6)
   - Group 2: Financial Analysis (sections 7-13)
   - Group 3: Strategy & Market Position (sections 14-19)
   - Group 4: Investment Analysis (sections 20-26)
   - Group 5: Risk & Governance (sections 27-32)

5. **Configure parallel workers** (1-5, default 2)

6. **Monitor progress**: Real-time updates for each section's multi-step analysis

## Architecture

### Core Components

- **`PD2.py`**: Main application entry point with UI and orchestration
- **`src/core_analyzer.py`**: Multi-step analysis pipeline with progressive refinement
- **`src/insight_memory.py`**: Learning system that captures analytical patterns
- **`src/profile_generator.py`**: HTML report generation with markdown processing
- **`src/profile_sections.py`**: Defines all 32 analysis sections
- **`src/utils.py`**: Shared utilities including rate limiting and thread safety
- **`src/file_manager.py`**: File I/O and markdown preprocessing

### Analysis Pipeline

Each section undergoes multiple refinement steps:

1. **Initial Draft**: Comprehensive analysis with all relevant data
2. **Completeness Check**: Identifies missing critical information
3. **Enhanced Draft**: Incorporates missing elements
4. **Deep Analysis & Polish**: Condenses to ~500 words of essential insights
5. **Learning Extraction**: Captures reusable analytical methodologies

### Key Features

- **Rate Limiting**: Automatic retry with exponential backoff (up to 3 retries)
- **Thread Safety**: All operations use thread-safe printing for parallel processing
- **Memory System**: Learning insights stored with section-based organization (max 30 words, quality score 9-10)
- **Markdown Fixes**: Automatic correction of corrupted tables from PDF conversion
- **Section 32 Special Handling**: No word limits, pure data extraction

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

memory/
└── learning_memory.json         # Captured analytical patterns

quality_metrics/                 # Performance tracking data
```

## The 32 Analytical Sections

### Group 1: Company Overview (1-6)
1. Operating Footprint
2. Corporate Structure & Ownership
3. Management & Board
4. Corporate History & Milestones
5. Business Model & Value Chain
6. Products & Services Portfolio

### Group 2: Financial Analysis (7-13)
7. Financial Performance Overview
8. Revenue Analysis
9. Margin & Profitability Analysis
10. Cash Flow Analysis
11. Balance Sheet & Capital Structure
12. Capital Allocation & Returns
13. Financial Deep Discoveries

### Group 3: Strategy & Market Position (14-19)
14. Business Strategy & Priorities
15. Competitive Strengths
16. Weaknesses & Challenges
17. Market Opportunities
18. Threats & Risks
19. Market Position & Competition

### Group 4: Investment Analysis (20-26)
20. Investment Thesis
21. Key Catalysts
22. Bull Case Scenario
23. Bear Case Scenario
24. Valuation Analysis
25. Key Investment Risks
26. Investment Deep Discoveries

### Group 5: Risk & Governance (27-32)
27. Quality of Earnings
28. Red Flags & Controversies
29. Corporate Governance
30. ESG & Sustainability
31. Regulatory & Compliance
32. Data Tables & Key Metrics

## Deep Analysis Methodology

ProfileDash 2.0 applies multi-layer analytical thinking:

1. **Surface vs. Reality**: Identifies contradictions between management claims and actual data
2. **Hidden Metrics**: Derives ratios and relationships not explicitly provided
3. **Pattern Recognition**: Finds correlations across time periods and data sets
4. **Relevance Filter**: Every insight must matter to company prospects
5. **Logic Test**: Every insight must make business sense
6. **Data Density**: Maximum insights per word - no fluff or corporate language
7. **Contradiction Highlighting**: Flags where management narrative diverges from data reality

**Quality Standard**: Would this insight change an investor's view of the company's prospects?

## Known Limitations

- Depends on Marker library for PDF conversion (may have table corruption issues)
- PDF conversion limited to single worker due to PyTorch tensor memory constraints
- No formal test suite or type checking
- LLM quality scoring requires aggressive filtering (9-10 threshold)

## Troubleshooting

### Common Issues

- **Rate limit exceeded**: Reduce parallel workers or wait before retrying
- **PDF conversion failed**: Ensure PDF contains searchable text, not scanned images
- **Empty section output**: Occasional API errors - re-run affected sections
- **Out of memory**: Process fewer sections at once or reduce parallel workers

### Getting Help

1. Check `runs/run_*/run_summary.txt` for error details
2. Monitor `quality_metrics/` for tracking data
3. Review console output for specific error messages

## License

This project is licensed under the MIT License. See the LICENSE file in the project directory for full details.
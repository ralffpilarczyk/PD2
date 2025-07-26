# ProfileDash 2.0

An intelligent document analysis system for financial analysis that processes PDF financial documents (annual reports, financial statements, presentations) and generates comprehensive company profiles with 32 different analytical sections.

## Features

- **Intelligent Analysis**: 4-step refinement pipeline for each section
- **Learning System**: Section-based memory that improves over time
- **Parallel Processing**: Concurrent analysis of multiple sections
- **Comprehensive Output**: 32 specialized sections covering all aspects of company analysis
- **Smart Prompting**: Temperature-tuned prompts for different cognitive phases

## Quick Start

### Prerequisites

- Python 3.8+
- Google Gemini API key

### Installation

```bash
# Clone the repository
git clone https://github.com/YOUR_USERNAME/profiledash.git
cd profiledash

# Install dependencies
pip install -r requirements.txt

# Set up environment variables
cp .env.example .env
# Edit .env and add your GEMINI_API_KEY
```

### Usage

```bash
# Run the application
python PD2.py
```

Place your PDF files in the `SourceData/` directory and run the application. The analysis will be saved in the `runs/` directory.

## Architecture

- `PD2.py` - Main application entry point
- `src/core_analyzer.py` - Core analysis pipeline
- `src/insight_memory.py` - Learning and memory system
- `src/profile_generator.py` - HTML report generation
- `src/profile_sections.py` - Section definitions

## Documentation

See [CLAUDE.md](CLAUDE.md) for detailed development guidelines and [PLAN.md](PLAN.md) for the algorithm revision roadmap.

## License

[Your chosen license]

## Contributing

[Contributing guidelines if you want them]
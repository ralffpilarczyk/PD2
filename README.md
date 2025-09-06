# ProfileDash 2.0

Intelligent document analysis system that processes PDF financial documents and generates comprehensive company profiles with 33 analytical sections using Google's Gemini API.

## Quick Start

1. **Install**:
   ```bash
   git clone https://github.com/yourusername/PD2.git
   cd PD2
   pip install -r requirements.txt
   ```

2. **Configure**:
   ```bash
   cp .env.example .env
   # Add your Gemini API key to .env file
   ```

3. **Run**:
   ```bash
   python PD2.py
   ```

## Requirements

- Python 3.8+
- Gemini API key (free at https://makersuite.google.com/app/apikey)
- 8GB RAM minimum

## Usage

1. Place PDFs in `SourceData/` folder
2. Run `python PD2.py`
3. Select PDFs when prompted
4. Choose section groups to analyze:
   - Company Profile (sections 1-13)
   - Strategy and SWOT (sections 14-19)
   - Sellside Positioning (sections 20-26)
   - Buyside Due Diligence (sections 27-32)
   - Data Book (section 33)
5. Set parallel workers (1-5, default 2)
6. Wait for processing (~30-60 minutes for full profile)

## Output

Reports saved to `runs/run_YYYY_MM_DD_HH_MM_SS/`:
- `[Company]_profile.html` - Final report
- `[Company]_profile.md` - Markdown version
- `section_*/` - Individual section analysis steps
- `run_summary.txt` - Processing log

## Architecture

- Multi-step analysis pipeline with progressive refinement
- Learning system captures analytical patterns
- Parallel processing with configurable workers
- Automatic retry with exponential backoff for API limits
- Thread-safe operations for concurrent processing

## License

MIT License - see LICENSE file for details.
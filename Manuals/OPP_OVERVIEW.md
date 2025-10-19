# OPP Code Overview - Complete Technical Documentation

## Executive Summary

OnePageProfile (OPP) is a focused document analysis tool that transforms PDF documents into concise one-page company profiles for M&A evaluation using Google's Gemini LLM API. The system employs a 4-step refinement pipeline optimized for speed and clarity, generating both markdown and PowerPoint outputs with professional formatting.

**Key Metrics**:
- 4 analytical sections (Company Overview, Competitive Positioning, Financial KPIs, Strategic Considerations)
- 4-step progressive refinement pipeline
- 500-word limit per section
- Dual output: Markdown + PowerPoint
- Processing time: ~5-10 minutes per profile

## Architecture Overview

```
┌─────────────────────────────────────────────────────┐
│                    User Input                        │
│                  (PDF Documents)                     │
└────────────────────┬───────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────┐
│                   OPP.py                            │
│         (Main Orchestrator & CLI Interface)          │
│   • Gemini API configuration                         │
│   • PDF selection and encoding                       │
│   • 4-step refinement pipeline                       │
│   • PowerPoint generation                            │
└────────────────────┬───────────────────────────────┘
                     │
        ┌────────────┴────────────┐
        ▼                         ▼
┌──────────────┐      ┌──────────────────┐
│profile_      │      │pptx_generator    │
│prompts.py    │      │                  │
│              │      │                  │
│•Section      │      │•Markdown parsing │
│ requirements │      │•Native bullets   │
│•Prompt       │      │•A4 landscape     │
│ templates    │      │•Formatting       │
└──────────────┘      └──────────────────┘
```

## Core Principles & Design Philosophy

### 1. Speed Over Depth
- Optimized for quick M&A screening, not comprehensive analysis
- 4 steps vs PD2's 6 steps (no discovery, no learning)
- Target: Under 10 minutes per profile

### 2. Clarity Over Completeness
- 500 words max per section (enforced at polish step)
- Focus on essential insights only
- "Does this matter to an investor?" filter applied throughout

### 3. Direct PDF Processing
- PDFs sent directly to Gemini (no markdown conversion)
- Inline base64 encoding for simplicity
- Reduces complexity compared to PD2's conversion pipeline

### 4. Professional Presentation
- Native PowerPoint bullets with proper hanging indents
- A4 landscape format (11.69" × 8.27")
- Consistent color scheme (dark blue #2d5a87, dark grey #4a5568)
- Auto-generated footnotes with version and date

## File Structure & Module Breakdown

```
PD2/
├── OPP.py                       # Main entry point (515 lines)
├── src/
│   ├── profile_prompts.py      # Prompt templates (280 lines)
│   └── pptx_generator.py       # PowerPoint generation (300 lines)
├── Manuals/
│   ├── Profile_Page.md         # User-facing specification
│   └── Refinement.md           # Pipeline documentation
├── requirements.txt            # Dependencies (includes python-pptx)
└── ProfileFiles/               # PowerPoint output directory
```

## Detailed Module Documentation

### 1. OPP.py - Main Orchestrator

**Purpose**: Entry point and orchestration layer for one-page profile generation.

#### OnePageProfile Class

```python
class OnePageProfile:
    def __init__(self, pdf_files: List[str], model_name: str = 'gemini-2.0-flash-exp'):
        self.pdf_files = pdf_files
        self.model_name = model_name
        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.run_dir = Path(f"runs/opp_{self.timestamp}")

        # Three models with different temperatures
        self.model_low_temp = genai.GenerativeModel(model_name, temperature=0.2)
        self.model_medium_temp = genai.GenerativeModel(model_name, temperature=0.6)
```

**Key Design Decisions**:
- Separate models for different tasks (temperature optimization)
- Run directory: `runs/opp_TIMESTAMP/` (separate from PD2)
- Version tracking: `__opp_version__` (distinct from PD2's `__version__`)

#### The 4-Step Refinement Pipeline

**Step 1: Initial Generation**
```python
def generate_initial_profile(self, company_name: str) -> str:
    # Temperature: 0.6 (balanced)
    # Direct PDF input (inline base64)
    # Includes: SECTION_REQUIREMENTS from profile_prompts.py
    # Target: Comprehensive coverage of all 4 sections
    # Output: step1_initial.md
```

**Key Features**:
- PDFs encoded inline (not via File API)
- Section requirements embedded in prompt
- Critical rules enforced:
  - Max 20 words per sentence
  - At least one number per sentence (ideally)
  - Bold 1-2 keywords per bullet (**word** syntax)
  - Skip silently if no data available

**Step 2: Completeness Check**
```python
def check_completeness(self, initial_profile: str) -> str:
    # Temperature: 0.2 (precision)
    # Validates against SECTION_REQUIREMENTS
    # Dual assessment:
    #   - Data completeness (missing from source docs)
    #   - Investor perspective completeness (missing insights)
    # Critical constraint: Only suggest if data exists
    # Output: step2_completeness_check.txt (ADD list)
```

**Key Features**:
- References original SECTION_REQUIREMENTS
- Max 5 suggestions per section
- Includes exact source locations (page numbers)
- Materiality filter: "Would this affect investment decision?"

**Step 3: Enhancement**
```python
def enhance_profile(self, initial_profile: str, add_list: str) -> str:
    # Temperature: 0.6
    # Incorporates ADD list items
    # Critical rule: Skip silently if no supporting data
    # No placeholder text like "metrics not disclosed"
    # Maintains narrative flow
    # Output: step3_enhanced.md
```

**Key Features**:
- Defense against unhelpful additions
- Smooth integration of new content
- Preserves all existing content
- Bullet format with **bold** keywords

**Step 4: Polish (with condensation)**
```python
def polish_section(self, section_name: str, section_content: str, word_limit: int) -> str:
    # Temperature: 0.6
    # Section-by-section polish (not whole document)
    # Word limit: 500 per section
    # Filter: Preserve only content that addresses SECTION_REQUIREMENTS
    # Defense in depth: Prompt + regex cleanup
    # Output: step4_final.md
```

**Key Features**:
- References SECTION_REQUIREMENTS for each section
- Removes duplicate titles and LLM preambles
- Regex cleanup patterns:
  - "Here is..." preambles
  - "This is..." preambles
  - Duplicate **Section Name** headers
- Critical output rules to prevent LLM noise

#### Helper Methods

**Company Name Extraction**:
```python
def extract_company_name(self) -> str:
    # Temperature: 0.6 (medium)
    # Searches: Document titles, headers, letterheads
    # Validation: 2-50 characters
    # Fallback: "Company Profile"
```

**File Management**:
```python
def save_step(self, step_name: str, content: str):
    # Saves to: runs/opp_TIMESTAMP/step_name

def assemble_final_markdown(self, sections: Dict[str, str]) -> str:
    # Combines: Title + Subtitle + 4 sections
    # Section order: Company Overview, Competitive Positioning,
    #                Financial KPIs, Strategic Considerations
```

**Run Logging**:
```python
def save_run_log(self, company_name: str, status: str = "Success", pptx_path: str = None):
    # Logs:
    #   - Timestamp and model
    #   - Company name
    #   - Source files
    #   - Pipeline steps (step1-4, optional PPTX)
    #   - Status
```

#### Main Execution Flow

```python
def generate_profile(self) -> Path:
    # 1. Extract company name from PDFs
    # 2. Generate initial profile (Step 1)
    # 3. Check completeness (Step 2)
    # 4. Enhance profile (Step 3)
    # 5. Polish each section separately (Step 4)
    # 6. Generate PowerPoint from final markdown
    # 7. Save run log
    # 8. Return path to step4_final.md
```

#### CLI Main

```python
if __name__ == "__main__":
    # 1. Select PDF files (file dialog)
    # 2. Initialize OnePageProfile
    # 3. Generate profile with progress display
    # 4. Output paths to markdown and PowerPoint
```

### 2. src/profile_prompts.py - Prompt Templates

**Purpose**: Single source of truth for section requirements and prompt generation.

#### SECTION_REQUIREMENTS Dictionary

```python
SECTION_REQUIREMENTS = {
    "Company Overview": [
        "one sentence where the company is based (city and country) and what its primary business is (key subsector, not high level like 'technology company')",
        "one sentence on the company's primary operating footprint, i.e. where its people are based, with numbers",
        "one sentence on the company's asset base, i.e. where its key assets are based, and if they are owned or leased",
        "one sentence on the mix of products and services and their value proposition, indicating what's most important",
        "one sentence on the mix of geography, indicating what's most important",
        "one sentence on the mix of customers, indicating what's most important, and highlight key customers, if any, and customer relationships, e.g. long term relationships, exclusivity, etc.",
        "one sentence on the company's key suppliers and concentration risk, but only if there is a critical exposure"
    ],
    "Competitive Positioning": [...],
    "Financial KPIs": [...],
    "Strategic Considerations": [...]
}
```

**Design Pattern**: DRY principle - define once, reference everywhere.

#### Prompt Generation Functions

**1. get_profile_generation_prompt(company_name: str) -> str**
- Embeds SECTION_REQUIREMENTS
- Enforces critical rules (20 words/sentence, numbers, bold keywords)
- Formats output structure with title and subtitle

**2. get_completeness_check_prompt(initial_profile: str) -> str**
- Includes SECTION_REQUIREMENTS for validation
- Dual assessment framework (data + investor perspective)
- Critical constraint: Only suggest if source data exists
- Materiality filter with priority levels (CRITICAL, IMPORTANT, USEFUL)

**3. get_enhancement_prompt(initial_profile: str, add_list: str) -> str**
- Instructions to skip silently if no data
- Prohibits "metrics not disclosed" statements
- Formatting rules for consistent markdown
- Emphasis on smooth integration

**4. get_polish_prompt(section_name: str, section_content: str, word_limit: int) -> str**
- Section-specific (not whole document)
- References SECTION_REQUIREMENTS for that section
- Two modes:
  - Title/subtitle (word_limit=0): No condensing
  - Content sections (word_limit=500): Aggressive condensing
- Relevance filter: Preserve only content addressing requirements
- Critical output rules to prevent preambles

### 3. src/pptx_generator.py - PowerPoint Generation

**Purpose**: Converts markdown profile to professionally formatted PowerPoint presentation.

#### Key Functions

**1. parse_markdown_profile(md_path: str) -> Dict**
```python
# Extracts:
#   - Title: # Title
#   - Subtitle: First line after title
#   - Sections: ## Section Name with bullets
# Returns: {'title': str, 'subtitle': str, 'sections': {...}}
```

**Regex Patterns**:
- Title: `r'^#\s+(.+?)$'`
- Subtitle: `r'^#\s+.+?\n(.+?)(?=\n##)'`
- Sections: `r'##\s+(.+?)\n(.*?)(?=\n##|\Z)'`
- Bullets: Lines starting with `*`

**2. create_profile_pptx(md_path: str, company_name: str, timestamp: str) -> str**
```python
# Pipeline:
#   1. Parse markdown → structured data
#   2. Create presentation (A4 landscape)
#   3. Add title area
#   4. Add 2×2 grid of section boxes
#   5. Add footnote
#   6. Save to ProfileFiles/
# Returns: Path to .pptx file
```

**Slide Layout**:
- **Size**: 11.69" × 8.27" (A4 landscape)
- **Title**: 0.5" top, Arial Bold 32pt, dark blue
- **Subtitle**: 1.053" top, Arial Bold 24pt, dark blue
- **Boxes**: 2×2 grid starting at 1.806" top
- **Footnote**: 8.05" top, Arial 7pt, dark blue

**3. _add_title_area(slide, title: str, subtitle: str)**
- Title: 32pt bold, top margin
- Subtitle: 24pt bold, 5mm below title (reduced spacing)

**4. _add_2x2_boxes(slide, sections: Dict[str, List[str]])**

**Layout Grid**:
```
┌─────────────────────┬─────────────────────┐
│  Company Overview   │ Competitive         │
│  (0.5", 1.806")    │ Positioning         │
│  5.195" × 2.635"   │ (6.195", 1.806")   │
├─────────────────────┼─────────────────────┤
│  Financial KPIs     │ Strategic           │
│  (0.5", 4.741")    │ Considerations      │
│  5.195" × 2.635"   │ (6.195", 4.741")   │
└─────────────────────┴─────────────────────┘
```

**5. _add_section_box(slide, section_name, bullets, left, top, width, height)**
- Section title: 12pt bold, dark blue, 6pt space after
- Bullets: Native PowerPoint bullet formatting
- Text: Arial 10pt with **bold** keyword parsing

**6. _format_bullet_text(paragraph, text: str)**

**Markdown Parsing**:
```python
# Split by **bold** markers: r'(\*\*.*?\*\*)'
# For each part:
#   - **keyword** → Dark blue, bold, 10pt
#   - Regular text → Dark grey, 10pt
```

**7. _enable_bullet(paragraph)**

**Native PowerPoint Bullets**:
```python
# Sets via XML:
#   - Bullet character: •
#   - marL: 114300 EMUs (0.125")  # Where text starts
#   - indent: -114300 EMUs        # Bullet offset (hanging)
#   - buFont: Arial
# Result: Proper hanging indent for wrapped lines
```

**Key Insight**: Reduced indent (0.125" vs default 0.25") for tighter spacing.

**8. _add_footnote(slide)**
```python
# Text: "Note: Generated with OnePageProfile v{OPP_VERSION} as of {DD-MMM-YY}."
# Position: Bottom (8.05" from top)
# Font: Arial 7pt, dark blue
# Version: Reads from OPP.__opp_version__
```

#### Color Scheme

```python
DARK_BLUE = RGBColor(45, 90, 135)   # #2d5a87 - titles, bold keywords
DARK_GREY = RGBColor(74, 85, 104)   # #4a5568 - regular text
```

Matches PD2's color scheme for brand consistency.

#### Version Management

```python
def _get_opp_version():
    # Reads __opp_version__ from OPP.py
    # Avoids circular import by reading file directly
    # Fallback: "1.0"
```

## Data Flow & Processing Pipeline

### Input Processing Flow
```
1. PDF Selection
   └─> File dialog (multi-select)

2. PDF Encoding
   ├─> Read each PDF as binary
   ├─> Base64 encode
   └─> Create Gemini API parts list
       [{mime_type: "application/pdf", data: base64_data}, ...]
```

**Key Difference from PD2**: No PDF→Markdown conversion, no caching.

### Profile Generation Flow
```
Step 1: Initial Generation
     │  • Input: PDFs + SECTION_REQUIREMENTS
     │  • Temp: 0.6
     │  • Output: step1_initial.md (~comprehensive)
     ↓
Step 2: Completeness Check
     │  • Input: step1_initial.md + PDFs
     │  • Temp: 0.2 (precision)
     │  • Validation: Against SECTION_REQUIREMENTS
     │  • Output: step2_completeness_check.txt (ADD list)
     ↓
Step 3: Enhancement
     │  • Input: step1 + ADD list + PDFs
     │  • Temp: 0.6
     │  • Action: Incorporate ADD items silently
     │  • Output: step3_enhanced.md
     ↓
Step 4: Polish (per section)
     │  • Input: Each section separately
     │  • Temp: 0.6
     │  • Word limit: 500 per section
     │  • Filter: Relevance to SECTION_REQUIREMENTS
     │  • Cleanup: Regex remove preambles/duplicates
     │  • Output: step4_final.md
     ↓
PowerPoint Generation
     │  • Input: step4_final.md
     │  • Parse: Extract title, subtitle, sections, bullets
     │  • Format: A4 landscape, 2×2 grid, native bullets
     │  • Output: ProfileFiles/[Company]_[timestamp].pptx
```

### Output Generation Flow
```
1. Parse Markdown
   ├─> Extract title (# line)
   ├─> Extract subtitle (line after #)
   └─> Extract 4 sections with bullets (## + * lines)

2. Create PowerPoint
   ├─> Set A4 landscape dimensions
   ├─> Add title and subtitle
   ├─> Add 2×2 grid of sections
   │   ├─> Top-left: Company Overview
   │   ├─> Top-right: Competitive Positioning
   │   ├─> Bottom-left: Financial KPIs
   │   └─> Bottom-right: Strategic Considerations
   ├─> Add footnote with version and date
   └─> Save to ProfileFiles/

3. Markdown Output
   └─> Save to runs/opp_TIMESTAMP/step4_final.md
```

## Error Handling & Recovery Strategies

### Retry Strategy
- Same as PD2: Exponential backoff (1s, 2s, 4s) with jitter
- Honors Retry-After headers from API
- Max 3 retries before giving up

### Fallback Mechanisms
1. **Company name extraction failure** → Use "Company Profile"
2. **Empty section output** → Keep previous step's content
3. **PowerPoint generation failure** → Log error, continue (markdown still available)
4. **Invalid markdown structure** → Best-effort parsing, skip malformed sections

### Validation Points
- Company name: 2-50 characters
- Section presence: All 4 sections required
- Bullet format: Must start with `*`
- Word count: Check at polish step (informational only, not blocking)

## Performance Characteristics

### Processing Time
- **PDF encoding**: ~1-2 seconds (depends on file size)
- **Step 1 (Initial)**: ~1-2 minutes (comprehensive generation)
- **Step 2 (Completeness)**: ~30-60 seconds (validation)
- **Step 3 (Enhancement)**: ~1-2 minutes (incorporation)
- **Step 4 (Polish)**: ~1-2 minutes (4 sections × 20-40s each)
- **PowerPoint generation**: ~1-2 seconds
- **Total**: ~5-10 minutes per profile

### Resource Usage
- **Memory**: ~500MB-1GB (PDFs in memory as base64)
- **API calls**: 5-6 per profile (1 per step + company name)
- **Token usage**: ~50-100k tokens per profile
- **Disk**: ~500KB per run (markdown + logs)

### Optimization Opportunities
1. **Parallel section polish**: Could reduce Step 4 time by 4x
2. **PDF streaming**: Reduce memory footprint
3. **Template caching**: Reuse prompt templates
4. **Batch processing**: Multiple companies in one session

## Configuration & Environment

### Environment Variables
```bash
GEMINI_API_KEY=your_key_here        # Required
```

### Key Dependencies
```
python-pptx             # PowerPoint generation (new for OPP)
google-generativeai     # Gemini API client
python-dotenv           # Environment management
pathlib                 # File operations
tkinter                 # File selection dialog
```

### System Requirements
- Python 3.8+
- 2GB RAM minimum (4GB recommended)
- Gemini API key (free tier sufficient)
- Internet connection

## Output Artifacts

### Final Deliverables
```
ProfileFiles/
└── [Company]_YYMMDD_HHMMSS.pptx
    • A4 landscape (11.69" × 8.27")
    • Single slide with 2×2 grid
    • Native bullet formatting
    • Dark blue titles and bold keywords
    • Footnote with version and date
```

### Work Products
```
runs/opp_YYMMDD_HHMMSS/
├── step1_initial.md              # Initial comprehensive draft
├── step2_completeness_check.txt  # ADD list from validation
├── step3_enhanced.md             # Enhanced with ADD items
├── step4_final.md                # Polished, condensed, final
└── run_log.txt                   # Processing log
```

## Key Algorithms & Techniques

### 1. Section-by-Section Polish Algorithm
```
Input: Enhanced markdown with 4 sections
Output: Polished markdown (500 words per section)

For each section:
  1. Extract section content
  2. Count words
  3. If over limit:
     a. Send to LLM with SECTION_REQUIREMENTS context
     b. Apply relevance filter
     c. Condense to 500 words
     d. Remove duplicates/preambles via regex
  4. Combine polished sections
  5. Reassemble with title and subtitle
```

**Key Insight**: Section-by-section approach allows targeted condensation while preserving structure.

### 2. Markdown **Bold** Parsing Algorithm
```
Input: "This is **keyword** text with **another** word"
Output: PowerPoint runs with mixed formatting

1. Split by regex: r'(\*\*.*?\*\*)'
   → ["This is ", "**keyword**", " text with ", "**another**", " word"]
2. For each part:
   a. If starts/ends with **:
      → Bold run, dark blue, 10pt
   b. Else:
      → Regular run, dark grey, 10pt
3. Append all runs to paragraph
```

**Key Insight**: Preserves markdown bold syntax in PowerPoint with visual distinction.

### 3. Native Bullet Formatting Algorithm
```
Input: Paragraph object in PowerPoint
Output: Properly formatted bullet with hanging indent

1. Access paragraph XML element (pPr)
2. Set marL attribute: 114300 EMUs (0.125")
   → Where text starts (left margin)
3. Set indent attribute: -114300 EMUs (-0.125")
   → Where bullet is positioned (hanging)
4. Create buChar element: • character
5. Create buFont element: Arial
6. Result: PowerPoint handles wrapped line alignment automatically
```

**Key Insight**: Using PowerPoint's native bullet system instead of manual "• " text ensures proper wrapping behavior.

### 4. Regex Cleanup Algorithm
```
Input: LLM output with potential preambles
Output: Clean section content

Patterns to remove (case-insensitive):
1. r'^Here is .*?section:?\s*\n+'
2. r'^This is .*?section:?\s*\n+'
3. r'^\*\*' + section_name + r'\*\*\s*\n+'  # Duplicate bold title
4. r'^\*\*SECTION:\s*' + section_name + r'\*\*\s*\n+'

Applied after LLM generation as safety net.
```

**Design Pattern**: Defense in depth - prompt instructions + regex cleanup.

## Comparison: OPP vs PD2

### Similarities
- Gemini API client
- Progressive refinement approach
- Markdown intermediate format
- Thread-safe console output
- Color scheme (dark blue, dark grey)
- Version management pattern

### Differences

| Aspect | OPP | PD2 |
|--------|-----|-----|
| **Purpose** | Quick M&A screening | Comprehensive analysis |
| **Sections** | 4 fixed sections | 33 configurable sections |
| **Pipeline** | 4 steps | 6 steps (+ discovery) |
| **Word limit** | 500 per section | ~500 per section (Step 4) |
| **Output** | Markdown + PowerPoint | Markdown + PDF |
| **PDF handling** | Inline base64 | Convert to markdown |
| **Learning** | None | Insight memory across runs |
| **Parallel** | Sequential | ThreadPoolExecutor |
| **Processing time** | 5-10 minutes | 30-60 minutes |
| **Complexity** | Simple, focused | Sophisticated, extensible |

## Known Issues & Limitations

### Design Constraints
1. **Fixed sections**: Cannot customize the 4 sections
2. **No learning**: Each run starts fresh (unlike PD2)
3. **No discovery**: No quantitative pattern finding
4. **Sequential processing**: One step at a time (no parallelism)
5. **Single slide**: PowerPoint limited to one slide

### Technical Limitations
1. **PDF memory**: All PDFs loaded into memory as base64
   - Large PDF sets (>100MB total) may cause issues
   - Mitigation: Process in batches

2. **PowerPoint complexity**: python-pptx library limitations
   - No paragraph_format in older versions → Use XML directly
   - Native bullet formatting required XML manipulation

3. **LLM output variability**:
   - May add preambles despite instructions
   - May duplicate section titles
   - Mitigation: Regex cleanup patterns

4. **No formal testing**: Manual validation only

## Future Enhancement Opportunities

### Potential Improvements
1. **Parallel section processing**: Polish 4 sections in parallel
2. **Multi-slide support**: Option for detailed PowerPoint (1 section per slide)
3. **Custom sections**: User-defined section templates
4. **Batch mode**: Process multiple companies sequentially
5. **Learning integration**: Optional connection to PD2's insight memory
6. **PDF streaming**: Reduce memory footprint for large files
7. **Word output**: Generate .docx in addition to .pptx
8. **Section selection**: Allow user to pick subset of 4 sections
9. **Template customization**: Configurable color schemes and layouts
10. **Comparison mode**: Side-by-side profiles for multiple companies

### Architectural Enhancements
1. **Plugin system**: Extensible output formats
2. **Web interface**: Browser-based UI instead of CLI
3. **API mode**: RESTful service for integration
4. **Cloud deployment**: Serverless function implementation
5. **Caching layer**: Store intermediate results for iteration

## Conclusion

OnePageProfile represents a focused, pragmatic approach to M&A screening:
- **Fast**: 5-10 minute turnaround
- **Clear**: 500 words per section, dense with insights
- **Professional**: PowerPoint output with native formatting
- **Simple**: 4-step pipeline, no complex dependencies

The system trades PD2's depth and sophistication for speed and clarity. It's optimized for the specific use case of rapid company evaluation in M&A workflows.

**Key Design Decisions**:
- Direct PDF processing (no conversion overhead)
- Section-by-section polish (targeted condensation)
- Native PowerPoint bullets (proper formatting)
- SECTION_REQUIREMENTS as single source of truth
- Defense in depth (prompt + regex) for output quality

**Recommended Usage**:
1. Initial screening of acquisition targets
2. Quick updates on portfolio companies
3. Preparation for investor meetings
4. Pitch deck appendix material
5. CIM comparison summaries

**Recommended Next Steps for New Developers**:
1. Read `Manuals/Profile_Page.md` for feature specification
2. Review `src/profile_prompts.py` to understand section requirements
3. Trace execution flow starting from `OPP.py` main block
4. Generate a test profile with sample PDFs
5. Examine `runs/opp_*/` directory outputs
6. Open generated PowerPoint to understand formatting
7. Compare with PD2 in `Manuals/CODE_OVERVIEW.md`

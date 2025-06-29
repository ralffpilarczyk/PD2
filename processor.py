import os
import json
import google.generativeai as genai
from typing import List, Dict, Tuple, Optional
from dotenv import load_dotenv
from datetime import datetime
from pathlib import Path
import re

# Load environment variables
load_dotenv()

# Configure Gemini
genai.configure(api_key=os.environ.get("GEMINI_API_KEY"))

# Import test sections
from test_sections import sections

class IntelligentAnalyst:
    # Configuration constants
    MAX_WORDS = 1000  # Maximum word count for sections (Section 32 is exempt)
    SECTION_32_EXEMPT = 32  # Section number that is exempt from word limits
    
    # Memory management constants - focused on insight generation only
    INSIGHTS_PER_SECTION = 5         # Insights per section type (financial, operational, etc.)
    MAX_MEMORY_DISPLAY = 6           # Maximum insights shown to model at once
    
    def __init__(self, markdown_files: List[str]):
        """Initialize the universal self-improving analyst"""
        self.full_context = self._load_markdown_files(markdown_files)
        self.model = genai.GenerativeModel('gemini-2.5-flash')
        
        # Create run directory with timestamp
        self.run_timestamp = datetime.now().strftime('%Y_%m_%d_%H_%M_%S')
        self.run_dir = f"runs/run_{self.run_timestamp}"
        self._setup_directories()
        
        # Load learning memory
        self.learning_memory = self._load_learning_memory()
        self._save_pre_run_memory()
        
        # Ensure learning memory file exists
        self._ensure_memory_file_exists()
        
        # Quality tracking
        self.quality_scores = {}
        
    def _setup_directories(self):
        """Create folder structure for tracking"""
        base_dirs = [
            "memory",
            "memory/memory_library", 
            self.run_dir,
            f"{self.run_dir}/memory_review",
            "quality_metrics"
        ]
        
        for section in sections:
            base_dirs.append(f"{self.run_dir}/section_{section['number']}")
            
        for dir_path in base_dirs:
            os.makedirs(dir_path, exist_ok=True)
    
    def _load_markdown_files(self, file_paths: List[str]) -> str:
        """Load and concatenate markdown files"""
        contents = []
        for path in file_paths:
            with open(path, 'r', encoding='utf-8') as f:
                contents.append(f"--- Document: {os.path.basename(path)} ---\n{f.read()}\n")
        return "\n\n".join(contents)
    
    def _load_learning_memory(self) -> Dict:
        """Load current learning memory or create empty structure"""
        memory_path = "memory/learning_memory.json"
        
        if os.path.exists(memory_path):
            with open(memory_path, 'r', encoding='utf-8') as f:
                loaded_memory = json.load(f)
                
            # Check if we need to migrate from old structure
            if "version" not in loaded_memory.get("meta", {}) or loaded_memory["meta"]["version"] != "2.0":
                print("Migrating memory structure to insights-only format...")
                return self._migrate_memory_structure(loaded_memory)
            
            return loaded_memory
        else:
            # Initialize empty memory structure - insights only
            return {
                "insights": {
                    "financial": [],
                    "operational": [],
                    "competitive": [],
                    "due_diligence": [],
                    "appendix": []
                },
                "meta": {
                    "total_runs": 0,
                    "last_updated": self.run_timestamp,
                    "version": "2.0"  # Updated for insights-only structure
                }
            }
    
    def _migrate_memory_structure(self, old_memory: Dict) -> Dict:
        """Migrate old memory structure to insights-only format"""
        new_memory = {
            "insights": {
                "financial": [],
                "operational": [],
                "competitive": [],
                "due_diligence": [],
                "appendix": []
            },
            "meta": {
                "total_runs": old_memory.get("meta", {}).get("total_runs", 0),
                "last_updated": self.run_timestamp,
                "version": "2.0"
            }
        }
        
        # Archive the old memory structure
        timestamp = datetime.now().strftime('%Y_%m_%d_%H_%M_%S')
        archive_path = f"memory/memory_library/pre_migration_{timestamp}.json"
        os.makedirs("memory/memory_library", exist_ok=True)
        with open(archive_path, 'w', encoding='utf-8') as f:
            json.dump(old_memory, f, indent=2)
        
        print(f"Old memory structure archived to: {archive_path}")
        print("Starting fresh with insights-only structure")
        
        return new_memory
    
    def _save_pre_run_memory(self):
        """Save snapshot of memory before run"""
        memory_file = f"{self.run_dir}/memory_review/pre_run_memory.json"
        with open(memory_file, 'w', encoding='utf-8') as f:
            json.dump(self.learning_memory, f, indent=2)
    
    def _ensure_memory_file_exists(self):
        """Ensure the main learning memory file exists"""
        memory_path = "memory/learning_memory.json"
        if not os.path.exists(memory_path):
            with open(memory_path, 'w', encoding='utf-8') as f:
                json.dump(self.learning_memory, f, indent=2)
    
    def _save_step_output(self, section_num: int, step: str, content: str):
        """Save output from each step for transparency"""
        filename = f"{self.run_dir}/section_{section_num}/{step}"
        with open(filename, 'w', encoding='utf-8') as f:
            f.write(content)
    
    def _get_relevant_memory(self, section: Dict) -> str:
        """Extract relevant insights for current section, prioritized by relevance"""
        section_type = self._classify_section_type(section['number'])
        
        # Collect insights with relevance scoring
        prioritized_insights = []
        
        # Primary: Section-specific insights (highest relevance)
        section_insights = self.learning_memory.get("insights", {}).get(section_type, [])
        for insight in section_insights:
            prioritized_insights.append((insight, 2, "primary"))
        
        # Secondary: Cross-section insights that might be relevant
        for other_section, insights in self.learning_memory.get("insights", {}).items():
            if other_section != section_type:
                for insight in insights:
                    # Add with lower priority for cross-pollination
                    prioritized_insights.append((insight, 1, "secondary"))
        
        if not prioritized_insights:
            return "No previous insights available from this analysis type."
        
        # Sort by relevance score, then by quality
        prioritized_insights.sort(key=lambda x: (
            x[1],  # Relevance level (2 for primary, 1 for secondary)
            x[0].get('relevance_score', 0) if isinstance(x[0], dict) else 0,  # Relevance score
            x[0].get('quality_score', 0) if isinstance(x[0], dict) else 0     # Quality score
        ), reverse=True)
        
        # Limit to MAX_MEMORY_DISPLAY most relevant insights
        top_insights = prioritized_insights[:self.MAX_MEMORY_DISPLAY]
        
        # Format for display focusing on relationships and material impact
        formatted_insights = []
        for i, (insight, relevance, category) in enumerate(top_insights, 1):
            if isinstance(insight, dict):
                insight_text = insight.get('insight', str(insight))
                # Keep concise but informative
                if len(insight_text) > 150:
                    insight_text = insight_text[:147] + "..."
                formatted_insights.append(f"{i}. {insight_text}")
            else:
                formatted_insights.append(f"{i}. {str(insight)[:150]}")
        
        memory_text = "RELATIONSHIP INSIGHTS (patterns that reveal material connections):\n" + "\n".join(formatted_insights)
        
        # Emphasize the purpose: identifying material relationships
        memory_text += f"\n\nFocus: Look for similar relationships and assess their material impact on the current analysis."
        
        return memory_text
    
    def _classify_section_type(self, section_num: int) -> str:
        """Classify section by type for memory organization"""
        section_types = {
            1: "operational",    # Operating Footprint
            7: "financial",      # Summary Financials
            13: "operational",   # Strategic Objectives
            21: "competitive",   # Competitive Positioning
            29: "due_diligence", # Due Diligence
            32: "appendix"       # Appendix
        }
        return section_types.get(section_num, "operational")
    
    def analyze_section(self, section_num: int) -> str:
        """Main analysis pipeline for a section"""
        section = next(s for s in sections if s['number'] == section_num)
        print(f"\n{'='*50}")
        print(f"ANALYZING SECTION {section_num}: {section['title']}")
        print(f"{'='*50}")
        
        try:
            # Step 1: Initial Draft
            print("Step 1: Creating initial draft...")
            initial_draft = self._create_initial_draft(section)
            self._save_step_output(section_num, "step_1_initial_draft.md", initial_draft)
            current_best = initial_draft
            
            # Step 2: Completeness Critique
            print("Step 2: Completeness critique...")
            try:
                completeness_critique = self._completeness_critique(section, current_best)
                self._save_step_output(section_num, "step_2_completeness_critique.txt", completeness_critique)
                
                # Apply completeness critique immediately
                print("Step 2b: Applying completeness critique...")
                current_best = self._apply_critique(section, current_best, completeness_critique, "completeness")
                self._save_step_output(section_num, "step_2_after_completeness.md", current_best)
            except Exception as e:
                print(f"  Warning: Completeness critique failed: {e}")
            
            # Step 3: Insight Critique (Critical step)
            print("Step 3: Insight critique...")
            try:
                insight_critique = self._insight_critique(section, current_best)
                self._save_step_output(section_num, "step_3_insight_critique.txt", insight_critique)
                
                # Apply insight critique immediately  
                print("Step 3b: Applying insight critique...")
                current_best = self._apply_critique(section, current_best, insight_critique, "insight")
                self._save_step_output(section_num, "step_3_after_insights.md", current_best)
            except Exception as e:
                print(f"  Warning: Insight critique failed: {e}")
            
            # Step 4: Polish Critique
            print("Step 4: Polish critique...")
            try:
                polish_critique = self._polish_critique(section, current_best)
                self._save_step_output(section_num, "step_4_polish_critique.txt", polish_critique)
                
                # Apply polish critique immediately  
                print("Step 4b: Applying polish critique (final output)...")
                current_best = self._apply_critique(section, current_best, polish_critique, "polish")
                self._save_step_output(section_num, "step_4_final_output.md", current_best)
            except Exception as e:
                print(f"  Warning: Polish critique failed: {e}")
            
            # Step 5: Learning Extraction
            print("Step 5: Learning extraction...")
            try:
                learning_insights = self._extract_learning(section, current_best)
                self._save_step_output(section_num, "step_5_learning_extraction.txt", learning_insights)
            except Exception as e:
                print(f"  Warning: Learning extraction failed: {e}")
                learning_insights = "Learning extraction failed"
            
            # Calculate quality metrics for this section
            self._calculate_quality_metrics(section_num, current_best)
            
            print(f"Section {section_num} completed successfully")
            return current_best
            
        except Exception as e:
            print(f"Critical error in section {section_num}: {e}")
            # Return minimal fallback
            return f"# Section {section_num}: {section['title']}\n\nAnalysis failed due to system error."
    
    def _create_initial_draft(self, section: Dict) -> str:
        """Step 1: Create initial comprehensive draft"""
        relevant_memory = self._get_relevant_memory(section)
        
        # Check if this is Section 32 (Appendix) - different approach needed
        if section['number'] == self.SECTION_32_EXEMPT:
            prompt = f"""You are a data organizer. Create a comprehensive data appendix for this section.

SECTION {section['number']}: {section['title']}

REQUIREMENTS:
{section['specs']}

DOCUMENTS:
{self.full_context}

Create a clean, well-organized appendix that:
1. Extracts ALL tables and structured data from the documents
2. Organizes tables logically (operational → financial → governance → miscellaneous)
3. Formats tables properly in Markdown
4. Includes clear source references for each table
5. NO ANALYSIS - just organize the raw data tables

CRITICAL: Do NOT provide any analysis, insights, or commentary. This is purely a data reference tool.
Focus on comprehensive data extraction and logical organization.
Output in clean Markdown format with clear section headers and properly formatted tables.
"""
        else:
            prompt = f"""You are an expert business analyst. Create a comprehensive analysis for this section.

SECTION {section['number']}: {section['title']}

REQUIREMENTS:
{section['specs']}

{relevant_memory}

CRITICAL STANCE - Management documents are inherently biased toward positive presentation:
- Challenge optimistic claims with data
- Look for what's NOT being said  
- Question timing of positive announcements
- Identify potential red flags or risks being downplayed
- Identify areas of disconnect between what the numbers say and what the text says

DOCUMENTS:
{self.full_context}

Create a thorough analysis that addresses all requirements. Focus on:
1. Extracting all relevant data points with precise citations
2. Identifying patterns and relationships in the data
3. Providing business insights and implications
4. Maintaining analytical rigor throughout

MANDATORY TABLE REQUIREMENT:
Include at least ONE well-formatted table that presents the most relevant numbers for this section. The table should:
- Highlight key metrics with time periods (3-year trends, quarterly data, etc.)
- Include proper headers and source references
- Be formatted clearly in Markdown table syntax
- Focus on the most important numerical data that supports your analysis

CONSTRAINT: Maximum {self.MAX_WORDS} words (except Section {self.SECTION_32_EXEMPT} which organizes data without word limit).
Output in clean Markdown format with proper headers, tables, and bullet points.
"""
        
        response = self.model.generate_content(prompt)
        return response.text
    
    def _completeness_critique(self, section: Dict, draft: str) -> str:
        """Step 2: Critique completeness against requirements"""
        
        # Count words in current draft
        word_count = len(draft.split())
        
        # Check if this is Section 32 (Appendix) - different requirements
        if section['number'] == self.SECTION_32_EXEMPT:
            prompt = f"""Critique this appendix for DATA COMPLETENESS against the specific requirements.

APPENDIX REQUIREMENTS:
{section['specs']}

DRAFT APPENDIX:
{draft}

CURRENT WORD COUNT: {word_count} words
NOTE: This is Section {self.SECTION_32_EXEMPT} (Appendix) - NO WORD LIMIT applies.

APPENDIX-SPECIFIC EVALUATION:
1. DATA COMPLETENESS: Are all tables and data from the source documents included?
2. ORGANIZATION: Are tables logically grouped (financials, operational, governance, etc.)?
3. FORMAT: Are tables properly formatted in Markdown?
4. SOURCES: Are table sources clearly noted?
5. UNWANTED CONTENT: Are there analytical narratives that should be removed?

For each gap identified, be specific about:
- What tables or data are missing
- How to improve the organization structure
- Any analytical content that should be removed

Focus on making this a comprehensive data reference appendix."""
        else:
            prompt = f"""Critique this analysis for COMPLETENESS against the specific requirements.

ORIGINAL REQUIREMENTS:
{section['specs']}

DRAFT ANALYSIS:
{draft}

CURRENT WORD COUNT: {word_count} words
TARGET: Maximum {self.MAX_WORDS} words (except Section {self.SECTION_32_EXEMPT})

Evaluate systematically:
1. REQUIRED DATA POINTS: Are all specified metrics, time periods, and data elements included?
2. COVERAGE GAPS: What specific requirements are missing or inadequately addressed?
3. SOURCE CITATIONS: Are all claims properly sourced with document references?
4. TIME PERIODS: Are historical data requirements (3 years, 5 quarters, etc.) met?
5. FORMAT REQUIREMENTS: Are tables, bullet points, etc. used as specified?
6. TABLE REQUIREMENT: Is there at least one well-formatted table with the most relevant numbers for this section?
7. LENGTH EFFICIENCY: If approaching/over {self.MAX_WORDS} words, suggest focusing on most critical requirements

For each gap identified, be specific about:
- What exactly is missing
- Where it should be found in the source documents  
- How critical this gap is to meeting requirements
- If lengthy, what lower-priority content could be condensed

TABLE FOCUS: Ensure there is at least ONE table with the most relevant numbers for this section. If multiple tables exist, prioritize the most important one. The table can be small but must capture the key metrics that matter most for this specific section.

Provide actionable feedback for revision that maintains completeness within word limits."""
        
        response = self.model.generate_content(prompt)
        return response.text
    
    def _insight_critique(self, section: Dict, draft: str) -> str:
        """Step 3: Deep insight critique using What-Why-So What framework"""
        
        # Count words in current draft
        word_count = len(draft.split())
        
        # Check if this is Section 32 (Appendix) - no analytical insights needed
        if section['number'] == self.SECTION_32_EXEMPT:
            prompt = f"""Critique this appendix for DATA ORGANIZATION and PRESENTATION.

APPENDIX TO CRITIQUE:
{draft}

CURRENT WORD COUNT: {word_count} words
NOTE: This is Section {self.SECTION_32_EXEMPT} (Appendix) - NO ANALYTICAL INSIGHTS needed.

APPENDIX-SPECIFIC FOCUS:
This should be a clean data reference tool, not an analytical document.

Evaluate:
1. PURE DATA FOCUS: Remove any analytical narratives, insights, or "What-Why-So What" content
2. TABLE ORGANIZATION: Are tables logically sequenced (operational → financial → governance)?
3. DATA INTEGRITY: Are all important tables from source documents included?
4. CLEAN FORMAT: Are tables properly formatted in Markdown with clear headers?
5. REFERENCE UTILITY: Is this easy to use as a data lookup tool?

Focus on:
- Removing all analytical content and keeping only raw data tables
- Improving table formatting and organization
- Making this a comprehensive data reference appendix
- Ensuring tables are complete and well-sourced

The goal is a clean, organized collection of data tables - not analysis."""
        else:
            prompt = f"""Conduct a deep analytical critique of this business analysis using investigative thinking.

ANALYSIS TO CRITIQUE:
{draft}

CURRENT WORD COUNT: {word_count} words
TARGET: Maximum {self.MAX_WORDS} words (except Section {self.SECTION_32_EXEMPT})

CRITICAL PERSPECTIVE - Remember management documents are biased:
- What positive claims lack supporting evidence?
- What negative information might be minimized or omitted?
- Where do the numbers contradict the narrative?
- What context is missing that could change the interpretation?

Apply the What-Why-So What framework systematically:

1. WHAT (Pattern Detection):
   - Scan for mathematical/logical anomalies (revenue up, margins down?)
   - Identify temporal inconsistencies (guidance vs results?)
   - Spot cross-metric relationships that need explanation
   - Find strategic disconnects (says vs does?)
   - Look for numbers that contradict management narrative

2. WHY (Investigation):
   - For each unusual pattern: WHY did this happen? What's the business logic?
   - Are there hidden relationships not explored?
   - What explanations are missing that an experienced analyst would ask?
   - What cross-document inconsistencies need reconciliation?
   - What isn't management telling us?

3. SO WHAT (Implication):
   - For each unexplained pattern: What business risk or opportunity does this create?
   - What are the investment/strategic implications?
   - What would a skeptical investor want to know?
   - What material insights are missing?
   - What red flags should buyers investigate further?

Focus on logical gaps that would make an experienced business analyst say "wait, that doesn't make sense" or "you need to explain why..."

TABLE EVALUATION: Does the analysis include at least one clear table with the MOST RELEVANT numbers for this section? Evaluate:
- What specific metrics should be tabulated for maximum clarity and relevance
- How time periods should be organized (columns for years/quarters)
- What comparisons would be most valuable in table format
- If multiple tables exist, which one is most essential for this section's purpose

Identify specific analytical improvements needed."""
        
        response = self.model.generate_content(prompt)
        return response.text
    
    def _polish_critique(self, section: Dict, draft: str) -> str:
        """Step 4: Critique for clarity and conciseness"""
        
        # Count words in current draft
        word_count = len(draft.split())
        
        # Check if this section is exempt from word limits
        if section['number'] == self.SECTION_32_EXEMPT:
            prompt = f"""Critique this appendix for CLARITY and ORGANIZATION.

APPENDIX ANALYSIS:
{draft}

CURRENT WORD COUNT: {word_count} words
NOTE: This is Section {self.SECTION_32_EXEMPT} (Appendix) - NO WORD LIMIT applies.

APPENDIX-SPECIFIC REQUIREMENTS:
1. PRESERVE ALL TABLES: Tables and data should be maintained, not removed
2. ORGANIZE LOGICALLY: Ensure proper structure and flow between sections
3. CLARITY: Improve readability without losing data
4. REMOVE EXECUTIVE SUMMARY: Appendices should not have executive summaries

Evaluate:
1. ORGANIZATION: Are tables and data logically grouped?
2. COMPLETENESS: Are all important data tables included?
3. CLARITY: Is the structure easy to navigate?
4. FORMAT: Are tables properly formatted and readable?

Focus on:
- Improving organization and flow
- Ensuring all tables are properly formatted
- Removing any executive summary or narrative sections
- Making the appendix a clean data reference tool"""
        else:
            prompt = f"""Critique this analysis for CLARITY and EFFICIENCY.

ANALYSIS:
{draft}

CURRENT WORD COUNT: {word_count} words
TARGET: Maximum {self.MAX_WORDS} words

CRITICAL REQUIREMENTS:
1. If over {self.MAX_WORDS} words: DEMAND specific cuts to reach target
2. PRIORITIZE: Keep only the most important insights and data
3. CUT: Redundant explanations, excessive examples, verbose phrasing
4. PRESERVE: Key data points, sources, and critical insights

Evaluate:
1. LENGTH: Is this under {self.MAX_WORDS} words? If not, what specific sections must be cut?
2. PRIORITY: Are the most important insights prominently featured?
3. EFFICIENCY: What redundant content can be removed without losing core insights?
4. CLARITY: Is the logic flow clear and well-structured?
5. TABLE QUALITY: Is there at least one well-formatted table? Are numbers clear and properly sourced?

Focus on:
- CUTTING content ruthlessly to reach {self.MAX_WORDS} words if over limit
- Removing verbose explanations while keeping insights
- Prioritizing highest-impact findings
- Maintaining analytical rigor in shortened form
- Ensuring at least one clear table with the MOST RELEVANT numbers for this section

If over {self.MAX_WORDS} words, provide specific guidance on what to cut and how to condense."""
        
        response = self.model.generate_content(prompt)
        return response.text
    
    def _apply_critique(self, section: Dict, current_draft: str, critique: str, critique_type: str) -> str:
        """Apply a specific critique to improve the current draft"""
        
        # Check if this is the exempt section (no word limit)
        if section['number'] == self.SECTION_32_EXEMPT:
            word_constraint = ""
        else:
            word_constraint = f"\nCONSTRAINT: Maximum {self.MAX_WORDS} words - prioritize most important insights."
            word_constraint += "\nTABLE REQUIREMENT: Ensure at least one well-formatted table with key numbers is included."
        
        prompt = f"""Revise this analysis by applying the {critique_type} critique feedback.

CURRENT ANALYSIS:
{current_draft}

{critique_type.upper()} CRITIQUE FEEDBACK:
{critique}

CRITICAL STANCE - Maintain skeptical analysis:
- Challenge management claims with data
- Highlight disconnects between numbers and narrative
- Identify what's missing or downplayed

Instructions:
1. Address all specific issues raised in the critique
2. Preserve all valuable content from the current analysis
3. Add missing elements identified in the critique
4. Improve the analysis based on the feedback
5. Do NOT remove any data, sources, or analytical insights - only enhance and add{word_constraint}

Output the improved analysis in clean Markdown format."""
        
        response = self.model.generate_content(prompt)
        return response.text
    

    def _extract_learning(self, section: Dict, final_output: str) -> str:
        """Step 6: Extract relationship insights for memory"""
        section_type = self._classify_section_type(section['number'])
        
        prompt = f"""Extract RELATIONSHIP INSIGHTS from this analysis that reveal material connections and patterns.

SECTION TYPE: {section_type}
FINAL OUTPUT: {len(final_output.split())} words

Focus exclusively on INSIGHT GENERATION about relationships:

RELATIONSHIP INSIGHTS TO EXTRACT:
1. DATA RELATIONSHIPS that proved material:
   - What numerical relationships revealed important patterns? (e.g., margin compression despite revenue growth)
   - How did different metrics connect to tell a bigger story?
   - What time-based relationships showed meaningful trends?

2. CAUSAL CONNECTIONS discovered:
   - What cause-and-effect relationships emerged from the data?
   - How did operational changes impact financial outcomes?
   - What strategic decisions showed measurable consequences?

3. PATTERN RECOGNITION successes:
   - What patterns in this {section_type} analysis would apply to similar companies?
   - Which analytical approaches revealed the most material insights?
   - What relationship types consistently generate valuable intelligence?

For each insight, focus on:
- The RELATIONSHIP itself (what connects to what)
- WHY this relationship is material to business analysis
- HOW to recognize similar patterns in future analyses

Output format - INSIGHTS ONLY:
RELATIONSHIP_INSIGHTS:
- insight: "[specific relationship pattern and its material significance]"
  relevance_score: X (1-10, how broadly applicable is this relationship pattern?)
  quality_score: Y (1-10, how material is this insight to business decisions?)
  
Focus on transferable relationship patterns, not company-specific details.
Each insight should help identify similar material relationships in future analyses."""
        
        response = self.model.generate_content(prompt)
        return response.text
    
    def _calculate_quality_metrics(self, section_num: int, output: str):
        """Calculate insight depth and efficiency metrics"""
        # Count insights (rough heuristic based on analytical language)
        insight_patterns = [
            r'indicates?',
            r'suggests?',
            r'reveals?',
            r'due to',
            r'because',
            r'driven by',
            r'results? from',
            r'implies?',
            r'therefore',
            r'consequently'
        ]
        
        insight_count = 0
        for pattern in insight_patterns:
            insight_count += len(re.findall(pattern, output, re.IGNORECASE))
        
        output_length = len(output.split())
        depth_ratio = insight_count / output_length if output_length > 0 else 0
        
        self.quality_scores[f"section_{section_num}"] = {
            "insights_count": insight_count,
            "output_length": output_length,
            "depth_ratio": depth_ratio
        }
    
    def process_all_sections(self, section_numbers: List[int] = None):
        """Process multiple sections and handle memory updates"""
        if section_numbers is None:
            section_numbers = [s['number'] for s in sections]
        
        results = {}
        
        # Process each section
        for section_num in section_numbers:
            try:
                result = self.analyze_section(section_num)
                results[section_num] = result
            except Exception as e:
                print(f"Failed to process section {section_num}: {e}")
                results[section_num] = f"Processing failed: {e}"
        
        # Post-run memory review and update
        print(f"\n{'='*50}")
        print("CONDUCTING POST-RUN MEMORY REVIEW")
        print(f"{'='*50}")
        
        try:
            self._conduct_memory_review()
        except Exception as e:
            print(f"Warning: Memory review failed: {e}")
        
        # Save quality metrics
        self._save_quality_metrics()
        
        # Generate run summary
        self._generate_run_summary(results)
        
        return results
    
    def _conduct_memory_review(self):
        """Review and update learning memory"""
        print("Extracting new observations...")
        
        # Collect all learning extractions
        learning_files = []
        for section in sections:
            learning_file = f"{self.run_dir}/section_{section['number']}/step_5_learning_extraction.txt"
            if os.path.exists(learning_file):
                with open(learning_file, 'r', encoding='utf-8') as f:
                    learning_files.append(f.read())
        
        if not learning_files:
            print("No learning extractions found")
            return
        
        combined_learning = "\n\n".join(learning_files)
        
        # Generate new insight candidates
        prompt = f"""Analyze these relationship insights to identify new patterns for the insight memory.

LEARNING EXTRACTIONS:
{combined_learning}

CURRENT INSIGHT MEMORY:
{json.dumps(self.learning_memory, indent=2)}

Generate new insight candidates that focus on RELATIONSHIPS and MATERIAL IMPACT:

Criteria for new insights:
1. RELATIONSHIP-FOCUSED: Reveals connections between data points, metrics, or business elements
2. TRANSFERABLE: Pattern recognition that applies across similar analyses
3. MATERIAL: Has significant impact on business understanding or decision-making
4. NOVEL: Not already captured in current insight memory
5. VALIDATED: Based on actual relationship discoveries from this run

For each candidate, provide:
- The insight (relationship pattern and its significance)
- Section type (financial, operational, competitive, due_diligence, appendix)
- Relevance score (1-10) - how broadly applicable across similar analyses
- Quality score (1-10) - how material to business decisions
- Diversity check - how different is this from existing insights?

Output format:
NEW_INSIGHTS:
- insight: "[relationship pattern and material significance]"
  section_type: "[section_type]"
  relevance_score: X
  quality_score: Y
  diversity_note: "[how this differs from existing insights]"
"""
        
        response = self.model.generate_content(prompt)
        new_insights_text = response.text
        
        self._save_step_output("memory_review", "new_insights.txt", new_insights_text)
        
        # Analyze consolidation opportunities
        print("Analyzing consolidation opportunities...")
        
        consolidation_prompt = f"""Analyze the current insight memory for relevance-based optimization and diversity.

CURRENT INSIGHT MEMORY:
{json.dumps(self.learning_memory, indent=2)}

NEW INSIGHTS:
{new_insights_text}

For each section type (financial, operational, competitive, due_diligence, appendix):
1. Identify insights that are too similar (lack diversity)
2. Assess relevance of existing insights to current analysis patterns  
3. Recommend which new insights should be added (max {self.INSIGHTS_PER_SECTION} per section)
4. Ensure diversity - avoid clustering around similar relationship types
5. Prioritize by relevance + quality, but maintain diverse insight types

Focus on:
- Relevance to future analyses (not age-based cleanup)
- Diversity of relationship types and patterns
- Material impact on business understanding
- Transferable pattern recognition

Provide specific recommendations for insight memory updates."""
        
        consolidation_response = self.model.generate_content(consolidation_prompt)
        consolidation_analysis = consolidation_response.text
        
        self._save_step_output("memory_review", "consolidation_analysis.txt", consolidation_analysis)
        
        # Apply memory updates (simplified for now - could be more sophisticated)
        try:
            self._apply_memory_updates(new_insights_text, consolidation_analysis)
            print("Memory review completed")
        except Exception as e:
            print(f"Warning: Memory update failed: {e}")
            # Ensure memory file exists even if update failed
            self._ensure_memory_file_exists()
    
    def _apply_memory_updates(self, new_insights: str, consolidation_analysis: str):
        """Apply insight memory updates based on relevance and diversity"""
        # Archive current memory
        timestamp = datetime.now().strftime('%Y_%m_%d_%H_%M_%S')
        archive_path = f"memory/memory_library/memory_{timestamp}.json"
        with open(archive_path, 'w', encoding='utf-8') as f:
            json.dump(self.learning_memory, f, indent=2)
        
        # Parse and add new insights
        try:
            self._parse_and_add_insights(new_insights)
            print("Successfully added new relationship insights to memory")
            
            # Clean up memory after each addition (relevance & diversity based)
            print("Optimizing insight memory for relevance and diversity...")
            self._cleanup_insights()
            
            # Show memory stats after cleanup
            memory_stats = self._get_memory_stats()
            print(f"Insight optimization completed - {memory_stats['total_insights']}/{memory_stats['max_possible']} insights ({memory_stats['utilization_percent']}% utilized)")
        except Exception as e:
            print(f"Warning: Failed to parse insights: {e}")
        
        # Update metadata
        self.learning_memory["meta"]["total_runs"] += 1
        self.learning_memory["meta"]["last_updated"] = timestamp
        
        # Save updated memory
        with open("memory/learning_memory.json", 'w', encoding='utf-8') as f:
            json.dump(self.learning_memory, f, indent=2)
        
        # Save post-run memory state
        with open(f"{self.run_dir}/memory_review/post_run_memory.json", 'w', encoding='utf-8') as f:
            json.dump(self.learning_memory, f, indent=2)
    
    def _parse_and_add_insights(self, new_insights_text: str):
        """Parse new insights and add them to memory structure"""
        # Extract insights using regex patterns
        insight_pattern = r'- insight: "([^"]+)"'
        section_pattern = r'section_type: "([^"]+)"'
        relevance_pattern = r'relevance_score: (\d+)'
        quality_pattern = r'quality_score: (\d+)'
        
        insights = re.findall(insight_pattern, new_insights_text)
        section_types = re.findall(section_pattern, new_insights_text)
        relevance_scores = re.findall(relevance_pattern, new_insights_text)
        quality_scores = re.findall(quality_pattern, new_insights_text)
        
        # Add insights to appropriate memory sections
        for i, insight in enumerate(insights):
            if i < len(section_types) and i < len(relevance_scores) and i < len(quality_scores):
                section_type = section_types[i]
                relevance = int(relevance_scores[i])
                quality = int(quality_scores[i])
                
                # Only add high-quality insights (8+ out of 10 for either relevance or quality)
                if relevance >= 8 or quality >= 8:
                    insight_entry = {
                        "insight": insight,
                        "relevance_score": relevance,
                        "quality_score": quality,
                        "added_date": datetime.now().strftime('%Y-%m-%d'),
                        "run_number": self.learning_memory["meta"]["total_runs"] + 1
                    }
                    
                    if section_type in self.learning_memory["insights"]:
                        self.learning_memory["insights"][section_type].append(insight_entry)
                        # Keep only top insights per cap for this section
                        if len(self.learning_memory["insights"][section_type]) > self.INSIGHTS_PER_SECTION:
                            # Sort by combined relevance + quality score
                            self.learning_memory["insights"][section_type].sort(
                                key=lambda x: x["relevance_score"] + x["quality_score"], 
                                reverse=True
                            )
                            self.learning_memory["insights"][section_type] = self.learning_memory["insights"][section_type][:self.INSIGHTS_PER_SECTION]
                    
                    print(f"Added {section_type} insight: {insight[:50]}...")
    
    def _cleanup_insights(self):
        """Clean up insight memory by optimizing for relevance and diversity"""
        
        # Clean up each section's insights
        if "insights" in self.learning_memory:
            for section_type in self.learning_memory["insights"]:
                self.learning_memory["insights"][section_type] = self._optimize_insight_list(
                    self.learning_memory["insights"][section_type], 
                    max_items=self.INSIGHTS_PER_SECTION
                )
    
    def _optimize_insight_list(self, insights: List[Dict], max_items: int) -> List[Dict]:
        """Optimize a list of insights for relevance and diversity"""
        if not insights:
            return insights
        
        # Remove duplicates based on similarity
        unique_insights = []
        for insight in insights:
            is_duplicate = False
            insight_text = insight.get("insight", "").lower()
            
            for existing in unique_insights:
                existing_text = existing.get("insight", "").lower()
                
                # Check for high similarity (simple word overlap check)
                insight_words = set(insight_text.split())
                existing_words = set(existing_text.split())
                
                if len(insight_words) > 0 and len(existing_words) > 0:
                    overlap = len(insight_words.intersection(existing_words))
                    similarity = overlap / min(len(insight_words), len(existing_words))
                    
                    # If 60%+ word overlap, consider it duplicate
                    if similarity >= 0.6:
                        # Keep the one with higher combined relevance + quality score
                        existing_score = existing.get("relevance_score", 0) + existing.get("quality_score", 0)
                        insight_score = insight.get("relevance_score", 0) + insight.get("quality_score", 0)
                        
                        if insight_score > existing_score:
                            unique_insights.remove(existing)
                            unique_insights.append(insight)
                        is_duplicate = True
                        break
            
            if not is_duplicate:
                unique_insights.append(insight)
        
        # NO AGE-BASED CLEANUP - focus on relevance only
        # Filter out insights that are too vague or too specific
        filtered_insights = []
        for insight in unique_insights:
            insight_text = insight.get("insight", "")
            
            # Skip if too vague (very short)
            if len(insight_text.split()) < 8:
                continue
                
            # Skip if too specific (contains very specific numbers, dates, or company names)
            if re.search(r'\b(FY\d{4}|\d{4}Q\d|\$\d+\.?\d*[MBK]?|\b[A-Z]{2,}\b.*Ltd|Inc\.|\b\d{1,2}/\d{1,2}/\d{2,4})', insight_text):
                continue
            
            # Keep if it passes filters
            filtered_insights.append(insight)
        
        # Apply diversity optimization - ensure we have diverse relationship types
        if len(filtered_insights) > max_items:
            # Prioritize diverse insights over just highest scoring ones
            diverse_insights = self._select_diverse_insights(filtered_insights, max_items)
            return diverse_insights
        
        # Sort by combined relevance + quality score if under limit
        filtered_insights.sort(key=lambda x: x.get("relevance_score", 0) + x.get("quality_score", 0), reverse=True)
        return filtered_insights[:max_items]
    
    def _select_diverse_insights(self, insights: List[Dict], max_items: int) -> List[Dict]:
        """Select insights to maximize diversity of relationship types"""
        # Identify different types of relationships mentioned
        relationship_keywords = {
            'financial': ['margin', 'revenue', 'profit', 'cost', 'cash', 'debt', 'earnings'],
            'operational': ['efficiency', 'capacity', 'production', 'volume', 'growth', 'expansion'],
            'temporal': ['trend', 'quarter', 'year', 'time', 'period', 'seasonal'],
            'causal': ['due to', 'because', 'results from', 'driven by', 'leads to', 'impact'],
            'competitive': ['market', 'competitor', 'advantage', 'position', 'share'],
            'strategic': ['investment', 'strategy', 'initiative', 'transformation', 'restructuring']
        }
        
        # Score each insight for diversity
        categorized_insights = {}
        for insight in insights:
            insight_text = insight.get("insight", "").lower()
            insight_categories = []
            
            for category, keywords in relationship_keywords.items():
                if any(keyword in insight_text for keyword in keywords):
                    insight_categories.append(category)
            
            # If no specific category, mark as 'general'
            if not insight_categories:
                insight_categories = ['general']
            
            for category in insight_categories:
                if category not in categorized_insights:
                    categorized_insights[category] = []
                categorized_insights[category].append(insight)
        
        # Select diverse insights - try to get at least one from each category
        selected_insights = []
        remaining_slots = max_items
        
        # First pass: Take top insight from each category
        for category, category_insights in categorized_insights.items():
            if remaining_slots > 0:
                # Sort by combined score and take the best from this category
                category_insights.sort(key=lambda x: x.get("relevance_score", 0) + x.get("quality_score", 0), reverse=True)
                selected_insights.append(category_insights[0])
                remaining_slots -= 1
        
        # Second pass: Fill remaining slots with next best insights
        remaining_insights = []
        for category, category_insights in categorized_insights.items():
            remaining_insights.extend(category_insights[1:])  # Skip first one already selected
        
        remaining_insights.sort(key=lambda x: x.get("relevance_score", 0) + x.get("quality_score", 0), reverse=True)
        selected_insights.extend(remaining_insights[:remaining_slots])
        
        return selected_insights
    
    def _get_memory_stats(self) -> Dict:
        """Get current insight memory statistics"""
        stats = {
            "insights_by_section": {}
        }
        
        total_insights = 0
        if "insights" in self.learning_memory:
            for section_type, insights in self.learning_memory["insights"].items():
                count = len(insights)
                stats["insights_by_section"][section_type] = count
                total_insights += count
        
        stats["total_insights"] = total_insights
        
        # Calculate theoretical maximum (5 insights per section × 5 section types)
        max_possible = self.INSIGHTS_PER_SECTION * 5  # 5 section types
        stats["max_possible"] = max_possible
        stats["utilization_percent"] = round((stats["total_insights"] / max_possible) * 100, 1)
        
        return stats
    
    def _save_quality_metrics(self):
        """Save quality metrics for tracking improvement"""
        metrics_file = "quality_metrics/insight_depth_scores.json"
        
        # Load existing metrics
        if os.path.exists(metrics_file):
            with open(metrics_file, 'r', encoding='utf-8') as f:
                all_metrics = json.load(f)
        else:
            all_metrics = {}
        
        # Add current run metrics
        run_key = f"run_{self.learning_memory['meta']['total_runs'] + 1}"
        all_metrics[run_key] = self.quality_scores
        
        # Calculate average
        if self.quality_scores:
            avg_depth = sum(s["depth_ratio"] for s in self.quality_scores.values()) / len(self.quality_scores)
            all_metrics[run_key]["average_depth_ratio"] = avg_depth
        
        # Save updated metrics
        with open(metrics_file, 'w', encoding='utf-8') as f:
            json.dump(all_metrics, f, indent=2)
    
    def _generate_run_summary(self, results: Dict):
        """Generate summary of the run"""
        memory_stats = self._get_memory_stats()
        
        summary = f"""RUN SUMMARY - {self.run_timestamp}
{'='*60}

SECTIONS PROCESSED: {list(results.keys())}

QUALITY METRICS:
{json.dumps(self.quality_scores, indent=2)}

INSIGHT MEMORY STATE (Relationship-focused):
- Financial insights: {memory_stats['insights_by_section'].get('financial', 0)}/{self.INSIGHTS_PER_SECTION}
- Operational insights: {memory_stats['insights_by_section'].get('operational', 0)}/{self.INSIGHTS_PER_SECTION}
- Competitive insights: {memory_stats['insights_by_section'].get('competitive', 0)}/{self.INSIGHTS_PER_SECTION}
- Due diligence insights: {memory_stats['insights_by_section'].get('due_diligence', 0)}/{self.INSIGHTS_PER_SECTION}
- Appendix insights: {memory_stats['insights_by_section'].get('appendix', 0)}/{self.INSIGHTS_PER_SECTION}
- Total insights: {memory_stats['total_insights']}/{memory_stats['max_possible']} ({memory_stats['utilization_percent']}% utilized)
- Quality threshold: 8+/10 for relevance OR quality
- Display limit: Top {self.MAX_MEMORY_DISPLAY} insights shown to model

INSIGHT OPTIMIZATION:
- Relevance-based cleanup (no aging penalties)
- Diversity optimization to prevent clustering
- Relationship pattern recognition focus
- Material impact prioritization

OUTPUTS SAVED:
- Individual section analyses: {self.run_dir}/section_X/
- Step-by-step process: Each step saved for review
- Memory review: {self.run_dir}/memory_review/
- Quality metrics: quality_metrics/insight_depth_scores.json

STATUS: Run completed successfully
"""
        
        with open(f"{self.run_dir}/run_summary.txt", 'w', encoding='utf-8') as f:
            f.write(summary)
        
        print(summary)


# Usage interface
if __name__ == "__main__":
    from tkinter import filedialog
    import tkinter as tk
    
    root = tk.Tk()
    root.withdraw()
    
    print("INTELLIGENT ANALYST - Universal Self-Improving Agent")
    print("="*60)
    
    print("\nSelect markdown files for analysis...")
    markdown_files = filedialog.askopenfilenames(
        title="Select Markdown Files",
        filetypes=[("Markdown files", "*.md"), ("All files", "*.*")]
    )
    
    if not markdown_files:
        print("No files selected. Exiting.")
        exit()
    
    # Initialize the intelligent analyst
    analyst = IntelligentAnalyst(list(markdown_files))
    
    available_sections = [s['number'] for s in sections]
    print(f"\nAvailable sections: {available_sections}")
    
    choice = input("Enter section number, multiple numbers (1,7,13), or 'all' for all sections: ").strip()
    
    if choice.lower() == 'all':
        print("\nProcessing all sections with full learning pipeline...")
        results = analyst.process_all_sections()
    elif ',' in choice:
        # Multiple specific sections
        try:
            section_nums = [int(x.strip()) for x in choice.split(',')]
            invalid_sections = [s for s in section_nums if s not in available_sections]
            if invalid_sections:
                print(f"Invalid section numbers: {invalid_sections}")
                print(f"Available sections: {available_sections}")
                exit()
            print(f"\nProcessing sections {section_nums} with full learning pipeline...")
            results = analyst.process_all_sections(section_nums)
        except ValueError:
            print("Invalid input format. Use comma-separated numbers like: 1,7,13")
            exit()
    else:
        # Single section
        try:
            section_num = int(choice)
            if section_num not in available_sections:
                print(f"Invalid section number. Available sections: {available_sections}")
                exit()
            print(f"\nProcessing section {section_num} with full learning pipeline...")
            results = analyst.process_all_sections([section_num])
        except ValueError:
            print("Invalid input. Enter a section number, comma-separated numbers, or 'all'")
            exit()
    
    print(f"\nAnalysis complete! Check the results in: runs/run_{analyst.run_timestamp}/") 
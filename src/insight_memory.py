import os
import json
import re
from typing import Dict, List, Tuple
from datetime import datetime
import google.generativeai as genai
from .utils import retry_with_backoff, thread_safe_print

class InsightMemory:
    """Clean, flexible insight memory system for section-based analytical instructions"""
    
    def __init__(self, run_timestamp: str, model_name: str = 'gemini-2.5-flash', memory_prefix: str = 'learning'):
        """Initialize with dynamic section support

        Args:
            run_timestamp: Timestamp for this run
            model_name: LLM model name
            memory_prefix: Prefix for memory file (e.g., 'pd2', 'opp'). Defaults to 'learning' for backward compatibility.
        """
        self.run_timestamp = run_timestamp
        self.memory_prefix = memory_prefix
        self.memory_file = f"memory/{memory_prefix}_learning_memory.json"
        self.model_name = model_name
        self.learning_memory = self._load_memory()
        
        # Configuration
        self.MAX_INSIGHTS_PER_SECTION = 6
        self.QUALITY_THRESHOLD = 9  # Only keep insights with quality 9+/10
        self.MAX_DISPLAY_INSIGHTS = 5  # Show top 5 to model
    
    def _load_memory(self) -> Dict:
        """Load memory or create empty structure"""
        if os.path.exists(self.memory_file):
            with open(self.memory_file, 'r', encoding='utf-8') as f:
                memory = json.load(f)
                
            # Ensure proper structure (migrate if needed)
            if not self._is_valid_structure(memory):
                thread_safe_print("Migrating to new insight memory structure...")
                return self._create_fresh_memory()
            
            return memory
        else:
            return self._create_fresh_memory()
    
    def _is_valid_structure(self, memory: Dict) -> bool:
        """Check if memory has the expected v3.0+ structure"""
        return (
            isinstance(memory, dict) and
            "insights" in memory and
            "meta" in memory and
            memory.get("meta", {}).get("version") == "3.0"
        )
    
    def _create_fresh_memory(self) -> Dict:
        """Create clean memory structure"""
        return {
            "insights": {},  # Dynamic section storage: section_X: [instructions...]
            "meta": {
                "version": "3.0",
                "total_runs": 0,
                "last_updated": self.run_timestamp,
                "active_sections": []  # Track which sections have been used
            }
        }
    
    def get_relevant_memory(self, section_num: int) -> str:
        """Get instruction-format insights for specific section"""
        section_key = f"section_{section_num}"
        
        # Get insights for this section only
        section_insights = self.learning_memory["insights"].get(section_key, [])
        
        if not section_insights:
            return ""
        
        # Sort by quality score (highest first)
        section_insights.sort(key=lambda x: x.get('quality_score', 0), reverse=True)
        
        # Take top insights for display
        display_insights = section_insights[:self.MAX_DISPLAY_INSIGHTS]
        
        # Format as instructions
        formatted_instructions = []
        for i, insight in enumerate(display_insights, 1):
            instruction = insight.get('instruction', '')
            quality = insight.get('quality_score', 0)
            formatted_instructions.append(f"{i}. {instruction} (Quality: {quality}/10)")
        
        if formatted_instructions:
            memory_text = f"LEARNED ANALYTICAL INSTRUCTIONS (Section {section_num}):\n"
            memory_text += "\n".join(formatted_instructions)
            memory_text += "\n\nApply these learned patterns during your analysis - they are additive to the section requirements."
            return memory_text
        
        return ""
    
    def process_new_insights(self, insights_text: str):
        """Extract and store new instruction-format insights"""
        insights = self._parse_insights(insights_text)
        
        for insight in insights:
            section_num = insight.get('section_number')
            quality = insight.get('quality_score')
            instruction = insight.get('instruction')
            
            # Be more permissive during initial collection - harsh filtering happens in cleanup
            if quality >= 6 and section_num and instruction:  # Back to 6 - maintain quality standards
                self._add_insight_to_section(section_num, insight)
    
    def _parse_insights(self, text: str) -> List[Dict]:
        """Parse insights from LLM output"""
        insights = []
        
        # Look for instruction blocks with section numbers and quality scores
        # Pattern matches: instruction: "...", section_number: X, quality_score: Y, word_count: Z
        instruction_pattern = r'instruction:\s*"([^"]+)"'
        section_pattern = r'section_number:\s*(\d+)'
        quality_pattern = r'quality_score:\s*(\d+)'
        word_count_pattern = r'word_count:\s*(\d+)'
        
        # Find all matches
        instructions = re.findall(instruction_pattern, text, re.IGNORECASE | re.DOTALL)
        sections = re.findall(section_pattern, text)
        qualities = re.findall(quality_pattern, text)
        word_counts = re.findall(word_count_pattern, text)
        
        # Combine matches (assuming they appear in order)
        for i, instruction in enumerate(instructions):
            if i < len(sections) and i < len(qualities):
                try:
                    # Validate word count if provided
                    actual_word_count = len(instruction.strip().split())
                    reported_word_count = int(word_counts[i]) if i < len(word_counts) else actual_word_count
                    
                    # Skip if instruction exceeds 30 words
                    if actual_word_count > 30:
                        # Silent skip - no output
                        continue
                    
                    insight = {
                        'instruction': instruction.strip(),
                        'section_number': int(sections[i]),
                        'quality_score': int(qualities[i]),
                        'word_count': actual_word_count,
                        'added_date': datetime.now().strftime('%Y-%m-%d'),
                        'run_number': self.learning_memory["meta"]["total_runs"] + 1
                    }
                    insights.append(insight)
                except ValueError:
                    continue  # Skip malformed entries
        
        return insights
    
    def _add_insight_to_section(self, section_num: int, insight: Dict):
        """Add insight to specific section with deduplication"""
        section_key = f"section_{section_num}"
        
        # Initialize section if doesn't exist
        if section_key not in self.learning_memory["insights"]:
            self.learning_memory["insights"][section_key] = []
        
        # Check for duplicates (basic similarity check)
        existing_instructions = [
            existing.get('instruction', '').lower() 
            for existing in self.learning_memory["insights"][section_key]
        ]
        
        new_instruction = insight['instruction'].lower()
        is_duplicate = any(
            self._calculate_similarity(new_instruction, existing) > 0.7 
            for existing in existing_instructions
        )
        
        if not is_duplicate:
            self.learning_memory["insights"][section_key].append(insight)
            # Silent addition - no output

            # Keep only top insights per section
            if len(self.learning_memory["insights"][section_key]) > self.MAX_INSIGHTS_PER_SECTION:
                self.learning_memory["insights"][section_key].sort(
                    key=lambda x: x['quality_score'], reverse=True
                )
                self.learning_memory["insights"][section_key] = (
                    self.learning_memory["insights"][section_key][:self.MAX_INSIGHTS_PER_SECTION]
                )
            
            # Track active sections
            if section_num not in self.learning_memory["meta"]["active_sections"]:
                self.learning_memory["meta"]["active_sections"].append(section_num)
    
    def _calculate_similarity(self, text1: str, text2: str) -> float:
        """Simple word-overlap similarity calculation"""
        words1 = set(text1.split())
        words2 = set(text2.split())
        
        if not words1 or not words2:
            return 0.0
        
        intersection = words1.intersection(words2)
        union = words1.union(words2)
        
        return len(intersection) / len(union) if union else 0.0
    
    def cleanup_and_diversify(self):
        """Use LLM to independently re-evaluate and select only the best insights"""
        model = genai.GenerativeModel(self.model_name)
        
        for section_key, insights in self.learning_memory["insights"].items():
            if len(insights) <= 2:  # Skip sections with very few insights
                continue
            
            # Format insights for harsh re-evaluation (ignore original scores)
            insights_text = "\n".join([
                f"- {insight['instruction']}"
                for insight in insights
            ])
            
            section_num = section_key.split('_')[1]
            
            prompt = f"""HARSH RE-EVALUATION: Ignore all previous quality scores and independently assess these analytical instructions.

INSTRUCTIONS FOR SECTION {section_num}:
{insights_text}

QUALITY STANDARDS - Be brutally selective:
- 10/10: Prevents major analytical errors, reveals systematic deception patterns
- 9/10: Non-obvious techniques that consistently uncover material insights
- 8/10: Solid practices that meaningfully improve analytical thoroughness
- 6-7/10: Standard due diligence (useful but not transformative)
- 1-5/10: Obvious or routine practices

EVALUATION CRITERIA:
1. UNIVERSAL APPLICABILITY: Applies to 80%+ of companies, not just niche situations (parent-subsidiary, multi-entity, captives)?
2. INSIGHT DEPTH: Reveals hidden relationships vs. surface-level checks?
3. CONCISENESS: Instruction is clear and actionable?
4. GAME-CHANGING: Would this genuinely improve analytical quality?

HARSH REALITY CHECK:
- Most instructions should score 6-8/10 (solid but not exceptional)
- Only 1-2 instructions per section deserve 9-10/10
- Expect to reject 60-80% of instructions as routine

TWO-PHASE TASK:

PHASE 1 - SCORE EACH INSTRUCTION:
Score each instruction individually from 1-10. Be brutally honest.

PHASE 2 - DIVERSITY-AWARE SELECTION:
1. Identify the highest score (MAX)
2. Eligible pool = all instructions scoring >= (MAX - 2)
3. From the eligible pool, select UP TO 3 instructions that MAXIMIZE DIVERSITY:
   - Different analytical approaches (quantitative vs qualitative, forward vs backward-looking)
   - Different business aspects (customers vs margins vs strategy vs operations)
   - Different mental models (compare/contrast vs decompose vs correlate vs validate)
   - Complementary techniques, NOT variations of the same idea

DIVERSITY RULES:
- If eligible pool contains similar techniques, choose the most diverse ones
- If all eligible techniques are diverse, choose highest scores
- Stay within the eligible pool (>= MAX-2) even if lower-scored insights are more diverse
- Cap selection at 3 insights maximum
- FALLBACK: If all insights are too similar to achieve diversity, select the top 1-2 by score anyway (never leave section empty)

RESPONSE FORMAT:
SCORES:
1. [Full text of instruction 1] - Score: X/10
2. [Full text of instruction 2] - Score: Y/10
...

ELIGIBLE_POOL:
(List all instructions that scored >= MAX-2)
- [Instruction A] - Score: X/10
- [Instruction B] - Score: Y/10
...

SELECTED (up to 3, prioritizing diversity from eligible pool):
- [Full text of selected instruction 1]
- [Full text of selected instruction 2]
- [Full text of selected instruction 3]

DIVERSITY_RATIONALE:
Briefly explain why these selections provide a diverse, complementary analytical toolkit.

Be ruthless on quality, strategic on diversity."""

            try:
                response_text = retry_with_backoff(
                    lambda: model.generate_content(prompt).text
                )
                scores, selected_instructions = self._parse_scored_and_selected_instructions(response_text)

                # Calculate eligibility metrics for logging
                max_score = max(scores.values()) if scores else 0
                eligible_count = sum(1 for s in scores.values() if s >= max_score - 2) if max_score > 0 else 0

                if selected_instructions:
                    # Keep only the selected instructions
                    filtered_insights = []
                    for insight in insights:
                        instruction_text = insight['instruction']
                        # Check if this instruction was selected (fuzzy matching)
                        if any(
                            self._calculate_similarity(instruction_text.lower(), selected.lower()) > 0.8
                            for selected in selected_instructions
                        ):
                            # Update the insight with harsh re-evaluation flag
                            insight['harsh_reeval'] = True
                            insight['original_score'] = insight.get('quality_score', 0)
                            # Store the new score if we have it, otherwise default to 10
                            new_score = scores.get(instruction_text.lower(), 10)
                            insight['quality_score'] = new_score
                            filtered_insights.append(insight)

                    # Update to keep only selected insights
                    original_count = len(self.learning_memory["insights"][section_key])
                    self.learning_memory["insights"][section_key] = filtered_insights
                    kept_count = len(filtered_insights)

                    # Enhanced logging with eligibility info
                    thread_safe_print(f"Harsh filter - {section_key}: {original_count} total → {eligible_count} eligible (≥{max_score-2}) → {kept_count} selected (diversity)")
                else:
                    # No instructions passed diversity filter - apply fallback
                    if eligible_count > 0:
                        # Fallback: All insights too similar, pick top 1-2 by score
                        eligible_insights = [
                            (scores.get(i['instruction'].lower(), 0), i)
                            for i in insights
                            if scores.get(i['instruction'].lower(), 0) >= max_score - 2
                        ]
                        eligible_insights.sort(key=lambda x: x[0], reverse=True)
                        top_insights = eligible_insights[:2]

                        filtered_insights = []
                        for score, insight in top_insights:
                            insight['harsh_reeval'] = True
                            insight['original_score'] = insight.get('quality_score', 0)
                            insight['quality_score'] = score
                            filtered_insights.append(insight)

                        self.learning_memory["insights"][section_key] = filtered_insights
                        thread_safe_print(f"Harsh filter - {section_key}: Diversity fallback, keeping top {len(filtered_insights)} by score")
                    else:
                        # No eligible insights at all
                        thread_safe_print(f"Harsh filter - {section_key}: All instructions rejected (max score: {max_score})")
                        self.learning_memory["insights"][section_key] = []

            except Exception as e:
                thread_safe_print(f"Warning: Could not apply harsh filter to {section_key}: {e}")
                # Keep original insights as fallback
                continue
    
    def _parse_scored_and_selected_instructions(self, response_text: str) -> Tuple[Dict[str, int], List[str]]:
        """Parse both scores and selected instructions from LLM response

        Returns:
            Tuple of (scores_dict, selected_list)
            - scores_dict: Maps instruction text to score (1-10)
            - selected_list: List of selected instruction texts
        """
        lines = response_text.split('\n')
        scores = {}
        selected = []

        current_section = None

        for line in lines:
            line_stripped = line.strip()

            # Identify sections
            if 'SCORES:' in line_stripped.upper():
                current_section = 'scores'
                continue
            elif 'ELIGIBLE_POOL:' in line_stripped.upper() or 'ELIGIBLE POOL:' in line_stripped.upper():
                current_section = 'eligible'
                continue
            elif 'SELECTED' in line_stripped.upper() and ':' in line_stripped:
                current_section = 'selected'
                continue
            elif 'DIVERSITY_RATIONALE:' in line_stripped.upper() or 'RATIONALE:' in line_stripped.upper():
                current_section = 'rationale'
                continue

            # Parse SCORES section
            if current_section == 'scores' and line_stripped:
                # Format: "1. [instruction text] - Score: X/10"
                # Also handle: "- [instruction text] - Score: X/10"
                score_match = re.search(r'(?:\d+\.\s*|\-\s*)(.+?)\s*-\s*Score:\s*(\d+)', line_stripped, re.IGNORECASE)
                if score_match:
                    instruction_text = score_match.group(1).strip()
                    # Remove brackets if present
                    if instruction_text.startswith('[') and instruction_text.endswith(']'):
                        instruction_text = instruction_text[1:-1].strip()
                    score = int(score_match.group(2))
                    scores[instruction_text.lower()] = score

            # Parse SELECTED section
            elif current_section == 'selected' and line_stripped.startswith('-'):
                instruction = line_stripped[1:].strip()  # Remove '- ' prefix
                # Remove brackets if present
                if instruction.startswith('[') and instruction.endswith(']'):
                    instruction = instruction[1:-1].strip()
                if instruction:
                    selected.append(instruction)

        return scores, selected
    
    def update_metadata(self):
        """Update memory metadata"""
        self.learning_memory["meta"]["total_runs"] += 1
        self.learning_memory["meta"]["last_updated"] = self.run_timestamp
    
    def save_memory(self):
        """Save memory to file"""
        os.makedirs(os.path.dirname(self.memory_file), exist_ok=True)
        with open(self.memory_file, 'w', encoding='utf-8') as f:
            json.dump(self.learning_memory, f, indent=2)
    
    def get_memory_stats(self) -> Dict:
        """Get statistics about current memory state"""
        insights_by_section = {}
        total_insights = 0
        
        for section_key, insights in self.learning_memory["insights"].items():
            section_num = int(section_key.split('_')[1])
            insights_by_section[section_num] = len(insights)
            total_insights += len(insights)
        
        active_sections = self.learning_memory["meta"]["active_sections"]
        max_possible = len(active_sections) * self.MAX_INSIGHTS_PER_SECTION
        
        return {
            "total_insights": total_insights,
            "insights_by_section": insights_by_section,
            "active_sections": active_sections,
            "max_possible": max_possible,
            "utilization_percent": round((total_insights / max_possible * 100) if max_possible > 0 else 0, 1),
            "quality_threshold": self.QUALITY_THRESHOLD,
            "max_per_section": self.MAX_INSIGHTS_PER_SECTION
        }
    
    def get_memory_data(self) -> Dict:
        """Get complete memory data"""
        return self.learning_memory 
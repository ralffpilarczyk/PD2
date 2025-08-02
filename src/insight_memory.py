import os
import json
import re
from typing import Dict, List
from datetime import datetime
import google.generativeai as genai
from .utils import retry_with_backoff, thread_safe_print

class InsightMemory:
    """Clean, flexible insight memory system for section-based analytical instructions"""
    
    def __init__(self, run_timestamp: str):
        """Initialize with dynamic section support"""
        self.run_timestamp = run_timestamp
        self.memory_file = "memory/learning_memory.json"
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
                        thread_safe_print(f"Skipping instruction (too long): {instruction[:50]}... ({actual_word_count} words)")
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
            thread_safe_print(f"Added instruction for Section {section_num}: {insight['instruction'][:60]}...")
            
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
        model = genai.GenerativeModel('gemini-2.5-flash')
        
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

SELECTION CRITERIA:
1. UNIVERSAL APPLICABILITY: Works across different companies/industries?
2. INSIGHT DEPTH: Reveals hidden relationships vs. surface-level checks?
3. CONCISENESS: Instruction is clear and actionable?
4. GAME-CHANGING: Would this genuinely improve analytical quality?

HARSH REALITY CHECK:
- Most instructions should score 6-8/10 (solid but not exceptional)
- Only 1-2 instructions per section deserve 9-10/10
- Expect to reject 60-80% of instructions as routine

TASK: Select only the TOP 2-3 instructions that score 9-10/10 for Section {section_num}.

RESPONSE FORMAT:
SELECTED_INSTRUCTIONS:
- [instruction 1 - only if it's genuinely game-changing]
- [instruction 2 - only if it's genuinely game-changing]
- [instruction 3 - only if it's genuinely game-changing]

RATIONALE:
Briefly explain why each selected instruction deserves 9-10/10.

Be ruthless - most instructions should NOT make the cut. Only keep true analytical breakthroughs."""

            try:
                response_text = retry_with_backoff(
                    lambda: model.generate_content(prompt).text
                )
                selected_instructions = self._parse_selected_instructions(response_text)
                
                if selected_instructions:
                    # Keep only the harshly selected instructions
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
                            insight['quality_score'] = 10  # Harsh filter passed
                            filtered_insights.append(insight)
                    
                    # Update to keep only harshly selected insights
                    original_count = len(self.learning_memory["insights"][section_key])
                    self.learning_memory["insights"][section_key] = filtered_insights
                    kept_count = len(filtered_insights)
                    
                    thread_safe_print(f"Harsh filter - {section_key}: {original_count} → {kept_count} instructions (kept {kept_count/original_count*100:.1f}%)")
                else:
                    # No instructions passed harsh filter - keep empty
                    thread_safe_print(f"Harsh filter - {section_key}: All instructions rejected (0/10 quality)")
                    self.learning_memory["insights"][section_key] = []
                
            except Exception as e:
                thread_safe_print(f"Warning: Could not apply harsh filter to {section_key}: {e}")
                # Keep original insights as fallback
                continue
    
    def _parse_selected_instructions(self, response_text: str) -> List[str]:
        """Parse selected instructions from LLM response"""
        lines = response_text.split('\n')
        instructions = []
        
        in_selected_section = False
        for line in lines:
            line = line.strip()
            # Handle both old and new formats
            if 'SELECTED_INSTRUCTIONS:' in line.upper() or 'SELECTED:' in line.upper():
                in_selected_section = True
                continue
            
            # Stop at RATIONALE section
            if 'RATIONALE:' in line.upper():
                break
            
            if in_selected_section and line.startswith('-'):
                instruction = line[1:].strip()  # Remove the '- ' prefix
                # Clean up any additional formatting
                if instruction.startswith('[') and instruction.endswith(']'):
                    instruction = instruction[1:-1]  # Remove brackets if present
                if instruction:
                    instructions.append(instruction)
        
        return instructions
    
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
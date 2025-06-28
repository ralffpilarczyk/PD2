# --- START OF FILE src/refinement.py ---
"""
Refinement module for ProfileDash v1.3
Handles fact-checking and insight refinement using markdown content and LlamaIndex chunks.
"""
import traceback
from typing import List

from .api_client import cached_generate_content, create_fact_model, create_insight_model
from .html_generator import repair_html, clean_llm_output
from .prompts import persona, output_format

def _create_context_from_chunks(chunks: List) -> str:
    """Helper to create a string context from a list of LlamaIndex nodes."""
    return "\n\n---\n\n".join([f"Source: {node.metadata.get('file_path', 'Unknown')}\n\n{node.get_content()}" for node in chunks])

def get_fact_critique(initial_instruction: str, answer_html: str, all_markdown_content: str) -> str:
    """
    Generate a fact critique using the full markdown content as context.
    """
    instruction = (
        f"You are a meticulous fact-checker. Below is a draft answer for a section of a company profile. "
        f"Your task is to critique this draft based *only* on the comprehensive source text provided. "
        f"Focus exclusively on factual correctness, completeness against the initial instructions, and verifiability. "
        f"Identify specific statements in the draft that are unsupported, contradicted, or incomplete based on the source text. "
        f"List obsolete information which needs ot be deleted and missing information which needs to be added to fulfill the initial instruction.\n\n"
        f"Initial Instruction for the section:\n{initial_instruction}\n\n"
        f"Draft Answer to Critique:\n```html\n{answer_html}\n```\n\n"
        f"Comprehensive Source Text (Ground Truth):\n```markdown\n{all_markdown_content}\n```\n\n"
        "CRITIQUE:"
    )
    fact_model = create_fact_model()
    try:
        print("Fact Refinement: Generating critique...")
        # Note: The 'persona' is omitted here to keep the critique model focused and neutral.
        response = cached_generate_content(fact_model, instruction)
        critique_text = getattr(response, 'text', '').strip()
        if not critique_text:
            return "Critique: No factual inaccuracies found or critique generation failed."
        return critique_text
    except Exception as e:
        print(f"Fact Refinement ERROR generating fact critique: {e}")
        traceback.print_exc()
        return f"Error: Could not generate fact critique due to exception: {e}"

def fact_improvement_response(
    initial_instruction: str,
    original_html: str,
    fact_critique_text: str,
    top_10_chunks: List,
    section_num: int,
    section_title: str
) -> str:
    """
    Generate an improved answer based on fact critique and a new set of context chunks.
    """
    context_from_chunks = _create_context_from_chunks(top_10_chunks)
    
    instruction = (
        f"You are an expert financial analyst. Your task is to revise a draft answer for a company profile section. "
        f"Use the provided context from source documents and the given critique to create a more factually accurate and complete version.\n\n"
        f"Initial Instruction for the section:\n{initial_instruction}\n\n"
        f"Critique to address:\n{fact_critique_text}\n\n"
        f"Original Draft Answer:\n```html\n{original_html}\n```\n\n"
        f"Context from relevant source document chunks:\n```\n{context_from_chunks}\n```\n\n"
        f"INSTRUCTIONS:\n"
        f"1. Revise the 'Original Draft Answer' to address all points in the 'Critique'.\n"
        f"2. Use *only* the information from the 'Context' to make corrections and additions.\n"
        f"3. Ensure the final output is a single, complete HTML section that adheres to all original formatting rules.\n"
        f"4. Add missing information and remove obsolete information as set out in the critique.\n\n"
        f"REVISED HTML FOR SECTION {section_num}:"
    )

    prompt = f"{persona}\n{instruction}\n{output_format}"
    fact_model = create_fact_model()

    try:
        print(f"Fact Refinement: Generating fact-improved response for section {section_num}...")
        response = cached_generate_content(fact_model, prompt, section_num)
        raw_html = getattr(response, 'text', '')
        if not raw_html:
             print(f"Fact Refinement Warning: API returned empty text for fact improvement (Section {section_num}). Returning original.")
             return original_html

        cleaned_html = clean_llm_output(raw_html, section_num, section_title)
        repaired_html = repair_html(cleaned_html, section_num, section_title)
        return repaired_html
    except Exception as e:
        print(f"Fact Refinement ERROR during improvement API call for section {section_num}: {e}")
        traceback.print_exc()
        return original_html # Return original on failure

def get_insight_critique(initial_instruction: str, answer_html: str, all_markdown_content: str) -> str:
    """
    Generate an insight critique using the full markdown content as context.
    """
    instruction = (
         f"You are a master corporate strategist. Below is a factually-corrected draft answer. "
         f"Your task is to critique its analytical depth and strategic insight based *only* on the comprehensive source text provided.\n\n"
         f"Focus exclusively on:\n"
         f"- **Depth of Analysis:** Does the draft go beyond summarizing facts? Does it explain the 'why' and 'so what'?\n"
         f"- **Non-obvious Connections:** Does it connect disparate facts from across the documents to form novel insights?\n"
         f"- **Strategic Implications:** Does it identify the strategic risks and opportunities implied by the data?\n\n"
         f"Initial Instruction for the section:\n{initial_instruction}\n\n"
         f"Draft Answer to Critique:\n```html\n{answer_html}\n```\n\n"
         f"Comprehensive Source Text (Ground Truth):\n```markdown\n{all_markdown_content}\n```\n\n"
         "STRATEGIC INSIGHT CRITIQUE:"
    )
    insight_model = create_insight_model()
    try:
        print("Insight Refinement: Generating critique...")
        response = cached_generate_content(insight_model, f"{persona}\n{instruction}")
        critique_text = getattr(response, 'text', '').strip()
        if not critique_text:
             return "Critique: No specific insight improvements suggested or critique generation failed."
        return critique_text
    except Exception as e:
        print(f"Insight Refinement ERROR generating insight critique: {e}")
        traceback.print_exc()
        return f"Error: Could not generate insight critique due to exception: {e}"

def insight_improvement_response(
    initial_instruction: str,
    original_html: str,
    insight_critique_text: str,
    top_10_chunks: List,
    section_num: int,
    section_title: str
) -> str:
    """
    Generate an improved answer based on insight critique and a new set of context chunks.
    """
    context_from_chunks = _create_context_from_chunks(top_10_chunks)
    
    instruction = (
        f"You are a top-tier investment banker and corporate strategist. Your task is to elevate a factually correct draft into a highly insightful analysis.\n\n"
        f"Initial Instruction for the section:\n{initial_instruction}\n\n"
        f"Insight Critique to address:\n{insight_critique_text}\n\n"
        f"Original Draft Answer:\n```html\n{original_html}\n```\n\n"
        f"Context from relevant source document chunks:\n```\n{context_from_chunks}\n```\n\n"
        f"INSTRUCTIONS:\n"
        f"1. Rewrite the 'Original Draft Answer' to incorporate the deeper analysis and strategic connections suggested by the 'Insight Critique'.\n"
        f"2. Use the 'Context' to find the evidence for your new insights.\n"
        f"3. Go beyond simple data reporting. Explain the implications of the facts. Keep it as concise as you can and cut out filler words and filler phrases. Delete any sentences that are not relevant to the initial instruction.\n"
        f"4. Ensure the final output is a single, complete HTML section that adheres to all original formatting rules.\n\n"
        f"FINAL INSIGHTFUL HTML FOR SECTION {section_num}:"
    )
    
    prompt = f"{persona}\n{instruction}\n{output_format}"
    insight_model = create_insight_model()

    try:
        print(f"Insight Refinement: Generating insight-improved response for section {section_num}...")
        response = cached_generate_content(insight_model, prompt, section_num)
        raw_html = getattr(response, 'text', '')
        if not raw_html:
             print(f"Insight Refinement Warning: API returned empty text for insight improvement (Section {section_num}). Returning original.")
             return original_html

        cleaned_html = clean_llm_output(raw_html, section_num, section_title)
        repaired_html = repair_html(cleaned_html, section_num, section_title)
        return repaired_html
    except Exception as e:
        print(f"Insight Refinement ERROR during improvement API call for section {section_num}: {e}")
        traceback.print_exc()
        return original_html
# --- END OF FILE src/refinement.py ---
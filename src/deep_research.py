"""
Deep Research module for OPP 1.5
Uses Google's Gemini Deep Research Agent for web-based company research.
"""

import time
import threading
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed
from google import genai


class ResearchDisplay:
    """Real-time display for research progress."""

    def __init__(self, num_workers: int):
        self.num_workers = num_workers
        self.status = {}
        self.lock = threading.Lock()

    def update(self, section_num: int, title: str, status: str):
        with self.lock:
            self.status[section_num] = f"Topic {section_num}/12: {title} -> {status}"
            self._redraw()

    def complete(self, section_num: int, completed: int, total: int):
        with self.lock:
            if section_num in self.status:
                del self.status[section_num]
            print(f"\n  Topic {section_num}/12 complete ({completed}/{total})")

    def _redraw(self):
        active = list(self.status.values())[:self.num_workers]
        if active:
            print("\r  " + " | ".join(active), end="", flush=True)


class DeepResearcher:
    """
    Performs web research using Google's Deep Research Agent.
    Researches 12 topics based on profile_sections.py specs.
    """

    AGENT = 'deep-research-pro-preview-12-2025'
    POLL_INTERVAL = 10  # seconds
    MAX_RESEARCH_TIME = 3600  # 60 minutes

    def __init__(self, company_name: str, workers: int = 2, run_dir: Path = None):
        self.company_name = company_name
        self.workers = workers
        self.run_dir = run_dir
        self.client = genai.Client()

    def research_section(self, section: dict, display: 'ResearchDisplay' = None) -> dict:
        """Run single research query for a section."""
        section_num = section['number']
        title = section['title']

        if display:
            display.update(section_num, title, "Starting")

        prompt = self._build_research_prompt(section)

        try:
            if display:
                display.update(section_num, title, "Researching")

            interaction = self.client.interactions.create(
                input=prompt,
                agent=self.AGENT,
                background=True,
                store=True
            )

            result = self._poll_for_result(interaction, section_num, title, display)

            if self.run_dir:
                self._save_section_output(section, result)

            return {
                'number': section_num,
                'title': title,
                'content': result['content'],
                'status': result['status']
            }

        except Exception as e:
            return {
                'number': section_num,
                'title': title,
                'content': f"Research failed: {str(e)}",
                'status': 'failed'
            }

    def run_all_sections(self, sections: list, display: 'ResearchDisplay' = None) -> dict:
        """Run research on all sections with parallel workers."""
        results = {}

        with ThreadPoolExecutor(max_workers=self.workers) as executor:
            futures = {
                executor.submit(self.research_section, section, display): section
                for section in sections
            }

            for future in as_completed(futures):
                section = futures[future]
                try:
                    result = future.result()
                    results[result['number']] = result
                    if display:
                        display.complete(result['number'], len(results), len(sections))
                except Exception as e:
                    results[section['number']] = {
                        'number': section['number'],
                        'title': section['title'],
                        'content': f"Research failed: {str(e)}",
                        'status': 'failed'
                    }
                    if display:
                        display.complete(section['number'], len(results), len(sections))

        return results

    def _build_research_prompt(self, section: dict) -> str:
        """Build research prompt from section specs with minimal adaptation."""
        specs = section['specs']
        specs = specs.replace('Extract and analyze', 'Research and analyze')
        specs = specs.replace('document references, page numbers, and sections', 'source URLs')
        specs = specs.replace('from subsequent interim financials', 'from recent sources')
        specs = specs.replace('from the preceding annual report', 'from older sources')
        specs = specs.replace('Always include precise footnotes with exact sources,', 'Always include source URLs.')

        return f"""Research {self.company_name} with focus on: {section['title']}

{specs}

Output format:
- Use markdown with headers and bullet points
- Include source citations (URLs) for all claims
- Focus on publicly available information from the last 2 years
- Note any conflicting information found across sources
- If specific data is not available, explicitly state this
"""

    def _poll_for_result(self, interaction, section_num: int, title: str,
                         display: 'ResearchDisplay' = None) -> dict:
        """Poll interaction until completed or failed."""
        start_time = time.time()
        poll_count = 0

        while True:
            interaction = self.client.interactions.get(interaction.id)
            poll_count += 1

            if interaction.status == "completed":
                return {
                    'content': interaction.outputs[-1].text,
                    'status': 'completed'
                }
            elif interaction.status == "failed":
                error_msg = getattr(interaction, 'error', 'Unknown error')
                return {
                    'content': f"Research failed: {error_msg}",
                    'status': 'failed'
                }

            elapsed = time.time() - start_time
            if elapsed > self.MAX_RESEARCH_TIME:
                return {
                    'content': "Research timed out after 60 minutes",
                    'status': 'timeout'
                }

            if display and poll_count % 6 == 0:  # Update every ~60 seconds
                mins = int(elapsed // 60)
                display.update(section_num, title, f"{mins}m")

            time.sleep(self.POLL_INTERVAL)

    def _save_section_output(self, section: dict, result: dict):
        """Save research output to run directory."""
        research_dir = self.run_dir / 'deep_research'
        research_dir.mkdir(parents=True, exist_ok=True)

        safe_title = section['title'].lower().replace(' ', '_').replace('&', 'and')
        safe_title = ''.join(c for c in safe_title if c.isalnum() or c == '_')
        filename = f"section_{section['number']:02d}_{safe_title}.md"
        filepath = research_dir / filename

        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(f"# {section['title']}\n\n")
            f.write(f"**Company**: {self.company_name}\n")
            f.write(f"**Status**: {result['status']}\n\n")
            f.write("---\n\n")
            f.write(result['content'])

    def combine_research(self, results: dict) -> str:
        """Combine all research results into single markdown."""
        combined = f"# Deep Research Report: {self.company_name}\n\n"

        for num in sorted(results.keys()):
            result = results[num]
            combined += f"## {num}. {result['title']}\n\n"
            if result['status'] == 'completed':
                combined += result['content']
            else:
                combined += f"*Research {result['status']}: {result['content']}*"
            combined += "\n\n---\n\n"

        if self.run_dir:
            research_dir = self.run_dir / 'deep_research'
            research_dir.mkdir(parents=True, exist_ok=True)
            combined_path = research_dir / 'combined_research.md'
            with open(combined_path, 'w', encoding='utf-8') as f:
                f.write(combined)

        return combined

    def get_research_summary(self, results: dict) -> dict:
        """Get summary statistics of research results."""
        completed = sum(1 for r in results.values() if r['status'] == 'completed')
        failed = sum(1 for r in results.values() if r['status'] == 'failed')
        timeout = sum(1 for r in results.values() if r['status'] == 'timeout')

        return {
            'total': len(results),
            'completed': completed,
            'failed': failed,
            'timeout': timeout,
            'success_rate': completed / len(results) if results else 0
        }

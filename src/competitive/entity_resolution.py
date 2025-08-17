"""
Entity resolution utilities for competitive analysis.
Resolves subsidiaries/brands/JVs to parent entities within a market cell, while preserving market separation.
"""
from typing import List, Dict, Any

# Minimal heuristic-based resolver; can be extended with curated mappings

KNOWN_PARENT_HINTS = [
    ("O2", "Telefónica"),
    ("Waymo", "Alphabet"),
    ("YouTube", "Alphabet"),
    ("Instagram", "Meta"),
    ("WhatsApp", "Meta"),
    ("LinkedIn", "Microsoft"),
]


def resolve_entities(competitors: List[Dict[str, Any]], market_cell: Dict[str, Any]) -> List[Dict[str, Any]]:
    resolved: List[Dict[str, Any]] = []
    for comp in competitors:
        name = comp.get("name", "")
        parent = comp.get("parent_company") or name
        # Heuristic mapping
        for hint, parent_name in KNOWN_PARENT_HINTS:
            if hint.lower() in name.lower():
                parent = parent_name
                break
        comp["parent_company"] = parent
        resolved.append(comp)
    return resolved

"""
Pivot Exporter for Competitive Analysis.
Produces per-market pivot tables: metrics (rows) × competitors (columns)
with normalized values, period, confidence, and source URLs.
"""
from __future__ import annotations

from typing import Dict, List, Any, Tuple
from pathlib import Path
import csv
import json
import sqlite3

from .database import CompetitiveDatabase
from ..utils import thread_safe_print


class PivotExporter:
    """Exports pivot-friendly outputs from the competitive SQLite database."""

    def __init__(self, db: CompetitiveDatabase, output_dir: str):
        self.db = db
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)

    def _fetch_market_cells(self, company_id: int) -> List[sqlite3.Row]:
        return self.db.get_market_cells_for_company(company_id)

    def _fetch_competitors(self, market_cell_id: int) -> List[sqlite3.Row]:
        return self.db.get_competitors_for_market_cell(market_cell_id)

    def _fetch_metric_rows(self, market_cell_id: int) -> List[sqlite3.Row]:
        with self.db.get_connection() as conn:
            rows = conn.execute(
                """
                SELECT DISTINCT m.id AS metric_id, m.name AS metric_name
                FROM observations o
                JOIN metrics m ON m.id = o.metric_id
                WHERE o.market_cell_id = ?
                ORDER BY m.name
                """,
                (market_cell_id,),
            ).fetchall()
        return rows

    def _fetch_observations(self, market_cell_id: int) -> List[sqlite3.Row]:
        with self.db.get_connection() as conn:
            rows = conn.execute(
                """
                SELECT o.id AS observation_id,
                       o.market_cell_id,
                       o.competitor_id,
                       o.metric_id,
                       o.value,
                       o.units,
                       o.period,
                       o.scope,
                       o.currency,
                       o.normalized_value,
                       o.comparability_class,
                       o.confidence_score,
                       o.normalization_notes,
                       o.created_at
                FROM observations o
                WHERE o.market_cell_id = ?
                """,
                (market_cell_id,),
            ).fetchall()
        return rows

    def _fetch_sources_for_observation(self, observation_id: int) -> List[Dict[str, Any]]:
        with self.db.get_connection() as conn:
            rows = conn.execute(
                """
                SELECT s.url, s.title
                FROM observation_sources os
                JOIN sources s ON s.id = os.source_id
                WHERE os.observation_id = ?
                """,
                (observation_id,),
            ).fetchall()
        return [{"url": r["url"], "title": r["title"]} for r in rows]

    @staticmethod
    def _comparability_rank(label: str) -> int:
        label_l = (label or "").lower()
        if label_l == "exact":
            return 0
        if label_l == "adjusted":
            return 1
        return 2  # proxy or unknown

    def _select_best_observation(self, candidates: List[sqlite3.Row]) -> sqlite3.Row | None:
        if not candidates:
            return None
        # Prefer exact > adjusted > proxy, then higher confidence, then latest created_at
        sorted_cands = sorted(
            candidates,
            key=lambda r: (
                self._comparability_rank(r["comparability_class"]),
                -(r["confidence_score"] or 0.0),
                str(r["created_at"]) or "",
            ),
        )
        return sorted_cands[0]

    def export_market_pivot(self, company_id: int, market_cell_row: sqlite3.Row) -> Dict[str, Any]:
        market_cell_id = market_cell_row["id"]
        market_name = f"{market_cell_row['product_service']} × {market_cell_row['geography']} × {market_cell_row['customer_segment']}"

        # Competitors and metrics present
        competitors = self._fetch_competitors(market_cell_id)
        metrics = self._fetch_metric_rows(market_cell_id)
        observations = self._fetch_observations(market_cell_id)

        # Index observations by (metric_id, competitor_id)
        from collections import defaultdict
        obs_map: Dict[Tuple[int, int], List[sqlite3.Row]] = defaultdict(list)
        for row in observations:
            obs_map[(row["metric_id"], row["competitor_id"])].append(row)

        # Build matrix
        competitor_order = [r["name"] for r in competitors]
        competitor_id_to_name = {r["id"]: r["name"] for r in competitors}
        metric_order = [r["metric_name"] for r in metrics]

        matrix: Dict[str, Dict[str, Dict[str, Any]]] = {}

        for m in metrics:
            m_id = m["metric_id"]
            m_name = m["metric_name"]
            matrix[m_name] = {}
            for comp in competitors:
                comp_id = comp["id"]
                comp_name = comp["name"]
                best = self._select_best_observation(obs_map.get((m_id, comp_id), []))
                if best is None:
                    matrix[m_name][comp_name] = None
                    continue
                sources = self._fetch_sources_for_observation(best["observation_id"])
                cell = {
                    "normalized_value": best["normalized_value"],
                    "normalized_currency": best["currency"],
                    "raw_units": best["units"],
                    "period": best["period"],
                    "confidence": best["confidence_score"],
                    "comparability": best["comparability_class"],
                    "sources": sources[:3],
                }
                matrix[m_name][comp_name] = cell

        # Write CSV for this market
        csv_path = self.output_dir / f"market_pivot_cell_{market_cell_id}.csv"
        try:
            with open(csv_path, "w", newline="", encoding="utf-8") as f:
                writer = csv.writer(f)
                # Header row with company names
                header = [""] + competitor_order
                writer.writerow(header)
                
                # Period row - extract periods for each competitor
                period_row = ["Period"]
                for comp_name in competitor_order:
                    # Get the most common period across all metrics for this competitor
                    periods = []
                    for metric_name in metric_order:
                        cell = matrix.get(metric_name, {}).get(comp_name)
                        if cell and cell.get("period"):
                            periods.append(cell.get("period"))
                    # Use most common period or first one found
                    if periods:
                        from collections import Counter
                        most_common = Counter(periods).most_common(1)[0][0]
                        period_row.append(most_common)
                    else:
                        period_row.append("")
                writer.writerow(period_row)
                
                # Data rows for each metric
                for metric_name in metric_order:
                    row: List[Any] = [metric_name]
                    for comp_name in competitor_order:
                        cell = matrix.get(metric_name, {}).get(comp_name)
                        if not cell:
                            row.append("")
                        else:
                            val = cell.get("normalized_value")
                            # Just the value, no period or confidence in cell
                            if val is None:
                                row.append("")
                            else:
                                # Format based on metric type
                                if "percent" in metric_name.lower() or "margin" in metric_name.lower():
                                    row.append(f"{val:.1f}%")
                                elif cell.get("normalized_currency"):
                                    row.append(f"{cell.get('normalized_currency')} {val:,.0f}")
                                else:
                                    row.append(f"{val:,.0f}")
                    writer.writerow(row)
        except Exception as e:
            thread_safe_print(f"Failed to write CSV pivot for market {market_cell_id}: {e}")

        # JSON structure for this market
        market_json = {
            "market_cell_id": market_cell_id,
            "market_name": market_name,
            "competitors": competitor_order,
            "metrics": metric_order,
            "matrix": matrix,
            "csv_path": str(csv_path),
        }
        return market_json

    def export_all_market_pivots(self, company_id: int) -> str:
        """Export pivots for all market cells of a company; returns JSON path."""
        market_cells = self._fetch_market_cells(company_id)
        all_markets: List[Dict[str, Any]] = []
        for mc in market_cells:
            all_markets.append(self.export_market_pivot(company_id, mc))

        json_path = self.output_dir / "market_pivots.json"
        try:
            with open(json_path, "w", encoding="utf-8") as f:
                json.dump({"markets": all_markets}, f, indent=2)
            thread_safe_print(f"Exported market pivots to: {json_path}")
        except Exception as e:
            thread_safe_print(f"Failed to write market pivots JSON: {e}")
        return str(json_path)

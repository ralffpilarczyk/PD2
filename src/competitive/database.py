"""
Database operations for competitive analysis using SQLite.
Handles schema creation, data persistence, and caching.
"""

import sqlite3
import json
import hashlib
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
from pathlib import Path
from ..utils import thread_safe_print


class CompetitiveDatabase:
    """SQLite database for competitive analysis data and caching"""
    
    def __init__(self, db_path: str):
        """Initialize database connection and create schema if needed"""
        self.db_path = db_path
        self.ensure_directory_exists()
        self.init_schema()
    
    def ensure_directory_exists(self):
        """Create directory if it doesn't exist"""
        Path(self.db_path).parent.mkdir(parents=True, exist_ok=True)
    
    def get_connection(self):
        """Get database connection with row factory"""
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row
        return conn
    
    def init_schema(self):
        """Create database schema if tables don't exist"""
        with self.get_connection() as conn:
            # Companies table - basic company information
            conn.execute("""
                CREATE TABLE IF NOT EXISTS companies (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    name TEXT NOT NULL,
                    ticker TEXT,
                    industry TEXT,
                    business_model TEXT,
                    headquarters TEXT,
                    context_json TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    UNIQUE(name, ticker)
                )
            """)
            
            # Business segments - actual reported segments from financial documents
            conn.execute("""
                CREATE TABLE IF NOT EXISTS business_segments (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    company_id INTEGER,
                    segment_name TEXT NOT NULL,
                    description TEXT,
                    revenue_contribution TEXT,
                    profit_contribution TEXT,
                    growth_rate TEXT,
                    key_metrics TEXT,
                    geographic_focus TEXT,
                    products_services TEXT,
                    significance_score REAL,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (company_id) REFERENCES companies (id)
                )
            """)
            
            # Keep market_cells table for backward compatibility (will be deprecated)
            conn.execute("""
                CREATE TABLE IF NOT EXISTS market_cells (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    company_id INTEGER,
                    product_service TEXT NOT NULL,
                    geography TEXT NOT NULL,
                    customer_segment TEXT NOT NULL,
                    materiality_score REAL,
                    strategic_flag BOOLEAN DEFAULT 0,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (company_id) REFERENCES companies (id)
                )
            """)
            
            # Competitors discovered per market cell
            conn.execute("""
                CREATE TABLE IF NOT EXISTS competitors (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    market_cell_id INTEGER,
                    name TEXT NOT NULL,
                    parent_company TEXT,
                    evidence_score REAL,
                    presence_evidence TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (market_cell_id) REFERENCES market_cells (id)
                )
            """)
            
            # Metric definitions and capability mappings
            conn.execute("""
                CREATE TABLE IF NOT EXISTS metrics (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    name TEXT NOT NULL,
                    definition TEXT,
                    capability_family TEXT,
                    unit_hint TEXT,
                    directionality TEXT, -- 'higher_better', 'lower_better', 'neutral'
                    value_impact_category TEXT, -- 'margin', 'roce', 'growth', 'scalability'
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    UNIQUE(name, capability_family)
                )
            """)
            
            # Actual data observations with full provenance
            conn.execute("""
                CREATE TABLE IF NOT EXISTS observations (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    market_cell_id INTEGER,
                    competitor_id INTEGER,
                    metric_id INTEGER,
                    value REAL,
                    units TEXT,
                    period TEXT,
                    scope TEXT,
                    currency TEXT,
                    normalized_value REAL,
                    comparability_class TEXT, -- 'exact', 'adjusted', 'proxy'
                    confidence_score REAL,
                    normalization_notes TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (market_cell_id) REFERENCES market_cells (id),
                    FOREIGN KEY (competitor_id) REFERENCES competitors (id),
                    FOREIGN KEY (metric_id) REFERENCES metrics (id)
                )
            """)
            
            # Web sources with trust scores and caching
            conn.execute("""
                CREATE TABLE IF NOT EXISTS sources (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    url TEXT NOT NULL,
                    title TEXT,
                    source_type TEXT, -- 'earnings_report', 'regulatory_filing', 'news', etc.
                    date_published TEXT,
                    trust_score REAL,
                    cached_content TEXT,
                    content_hash TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    UNIQUE(url, content_hash)
                )
            """)
            
            # Link observations to their sources
            conn.execute("""
                CREATE TABLE IF NOT EXISTS observation_sources (
                    observation_id INTEGER,
                    source_id INTEGER,
                    citation_text TEXT,
                    PRIMARY KEY (observation_id, source_id),
                    FOREIGN KEY (observation_id) REFERENCES observations (id),
                    FOREIGN KEY (source_id) REFERENCES sources (id)
                )
            """)
            
            # Search query cache (30-day retention)
            conn.execute("""
                CREATE TABLE IF NOT EXISTS search_cache (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    query_hash TEXT NOT NULL UNIQUE,
                    query_text TEXT NOT NULL,
                    company_name TEXT,
                    market_cell TEXT,
                    metric_name TEXT,
                    response_json TEXT,
                    grounding_metadata_json TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    expires_at TIMESTAMP
                )
            """)
            
            # Strategy bundles and recommendations
            conn.execute("""
                CREATE TABLE IF NOT EXISTS strategy_bundles (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    market_cell_id INTEGER,
                    name TEXT NOT NULL,
                    mechanism TEXT,
                    required_enablers TEXT,
                    expected_kpi_shifts TEXT,
                    risks TEXT,
                    timeline_months INTEGER,
                    resource_requirements TEXT,
                    conflict_analysis TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (market_cell_id) REFERENCES market_cells (id)
                )
            """)
            
            # Create indexes for performance
            conn.execute("CREATE INDEX IF NOT EXISTS idx_companies_name ON companies(name)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_market_cells_company ON market_cells(company_id)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_competitors_market ON competitors(market_cell_id)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_observations_market ON observations(market_cell_id)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_observations_competitor ON observations(competitor_id)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_search_cache_hash ON search_cache(query_hash)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_search_cache_expires ON search_cache(expires_at)")
            
            conn.commit()

            # Non-breaking migrations (best-effort, ignore if columns already exist)
            try:
                conn.execute("ALTER TABLE competitors ADD COLUMN is_self INTEGER DEFAULT 0")
            except Exception:
                pass

            try:
                conn.execute(
                    """
                    CREATE TABLE IF NOT EXISTS comparisons (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        market_cell_id INTEGER,
                        metric_id INTEGER,
                        subject_value REAL,
                        anchor_competitor TEXT,
                        gap REAL,
                        confidence REAL,
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        FOREIGN KEY (market_cell_id) REFERENCES market_cells (id),
                        FOREIGN KEY (metric_id) REFERENCES metrics (id)
                    )
                    """
                )
            except Exception:
                pass
    
    def insert_company(self, name: str, context: Dict[str, Any]) -> int:
        """Insert company and return company_id"""
        with self.get_connection() as conn:
            cursor = conn.execute("""
                INSERT OR REPLACE INTO companies 
                (name, ticker, industry, business_model, headquarters, context_json)
                VALUES (?, ?, ?, ?, ?, ?)
            """, (
                name,
                context.get('ticker'),
                context.get('industry'),
                context.get('business_model'),
                context.get('headquarters'),
                json.dumps(context)
            ))
            return cursor.lastrowid
    
    def insert_business_segment(self, company_id: int, segment: Dict[str, Any]) -> int:
        """Insert business segment and return segment_id"""
        with self.get_connection() as conn:
            cursor = conn.execute("""
                INSERT INTO business_segments 
                (company_id, segment_name, description, revenue_contribution, 
                 profit_contribution, growth_rate, key_metrics, geographic_focus,
                 products_services, significance_score)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                company_id,
                segment.get('segment_name'),
                segment.get('description'),
                segment.get('revenue_contribution'),
                segment.get('profit_contribution'),
                segment.get('growth_rate'),
                segment.get('key_metrics'),
                segment.get('geographic_focus'),
                segment.get('products_services'),
                segment.get('significance_score', 0.5)
            ))
            return cursor.lastrowid
    
    def get_business_segments_for_company(self, company_id: int) -> List[sqlite3.Row]:
        """Get all business segments for a company sorted by significance"""
        with self.get_connection() as conn:
            cursor = conn.execute("""
                SELECT * FROM business_segments 
                WHERE company_id = ? 
                ORDER BY significance_score DESC
            """, (company_id,))
            return cursor.fetchall()
    
    def insert_market_cell(self, company_id: int, product_service: str, 
                          geography: str, customer_segment: str, 
                          materiality_score: float = 0.0) -> int:
        """Insert market cell and return market_cell_id (DEPRECATED - use insert_business_segment)"""
        with self.get_connection() as conn:
            cursor = conn.execute("""
                INSERT INTO market_cells 
                (company_id, product_service, geography, customer_segment, materiality_score)
                VALUES (?, ?, ?, ?, ?)
            """, (company_id, product_service, geography, customer_segment, materiality_score))
            return cursor.lastrowid
    
    def insert_competitor(self, market_cell_id: int, name: str, 
                         parent_company: str = None, evidence_score: float = 0.0,
                          presence_evidence: str = None, is_self: int = 0) -> int:
        """Insert competitor and return competitor_id"""
        with self.get_connection() as conn:
            cursor = conn.execute(
                """
                INSERT INTO competitors 
                (market_cell_id, name, parent_company, evidence_score, presence_evidence, is_self)
                VALUES (?, ?, ?, ?, ?, ?)
                """,
                (market_cell_id, name, parent_company, evidence_score, presence_evidence, is_self)
            )
            return cursor.lastrowid
    
    def get_company_by_name(self, name: str) -> Optional[sqlite3.Row]:
        """Get company by name"""
        with self.get_connection() as conn:
            cursor = conn.execute("SELECT * FROM companies WHERE name = ?", (name,))
            return cursor.fetchone()
    
    def get_market_cells_for_company(self, company_id: int) -> List[sqlite3.Row]:
        """Get all market cells for a company"""
        with self.get_connection() as conn:
            cursor = conn.execute("""
                SELECT * FROM market_cells 
                WHERE company_id = ? 
                ORDER BY materiality_score DESC
            """, (company_id,))
            return cursor.fetchall()
    
    def get_competitors_for_market_cell(self, market_cell_id: int) -> List[sqlite3.Row]:
        """Get competitors for a market cell"""
        with self.get_connection() as conn:
            cursor = conn.execute("""
                SELECT * FROM competitors 
                WHERE market_cell_id = ? 
                ORDER BY evidence_score DESC
            """, (market_cell_id,))
            return cursor.fetchall()

    # --- Provenance helpers ---
    def upsert_source(self, url: str, title: str = None, source_type: str = None,
                      date_published: str = None, trust_score: float = None,
                      cached_content: str = None) -> int:
        """Insert or update a source record and return its ID."""
        content_hash = hashlib.md5((cached_content or url or '').encode()).hexdigest()
        with self.get_connection() as conn:
            cursor = conn.execute(
                """
                INSERT OR REPLACE INTO sources (url, title, source_type, date_published, trust_score, cached_content, content_hash)
                VALUES (?, ?, ?, ?, ?, ?, ?)
                """,
                (url, title, source_type, date_published, trust_score, cached_content, content_hash)
            )
            return cursor.lastrowid

    def link_observation_to_source(self, observation_id: int, source_id: int, citation_text: str = None):
        """Create link between observation and source (idempotent)."""
        with self.get_connection() as conn:
            conn.execute(
                """
                INSERT OR IGNORE INTO observation_sources (observation_id, source_id, citation_text)
                VALUES (?, ?, ?)
                """,
                (observation_id, source_id, citation_text)
            )

    def get_run_directory(self) -> Path:
        """Return the run directory (parent of the database file)."""
        return Path(self.db_path).parent

    def save_source_snapshot(self, url: str, content: str) -> Path:
        """Save a local snapshot of source content under competitive/sources and return path."""
        run_dir = self.get_run_directory()
        sources_dir = run_dir / "sources"
        sources_dir.mkdir(parents=True, exist_ok=True)
        content_hash = hashlib.md5((content or url or '').encode()).hexdigest()
        file_path = sources_dir / f"{content_hash}.txt"
        try:
            file_path.write_text(content or '', encoding='utf-8')
        except Exception:
            pass
        return file_path

    # --- Convenience helpers ---
    def get_or_create_self_competitor(self, market_cell_id: int, company_name: str) -> int:
        """Ensure a self competitor record exists for given market cell; return its ID."""
        with self.get_connection() as conn:
            row = conn.execute(
                "SELECT id FROM competitors WHERE market_cell_id = ? AND name = ?",
                (market_cell_id, company_name)
            ).fetchone()
            if row:
                return row['id']
            cursor = conn.execute(
                """
                INSERT INTO competitors (market_cell_id, name, parent_company, evidence_score, presence_evidence, is_self)
                VALUES (?, ?, ?, ?, ?, 1)
                """,
                (market_cell_id, company_name, company_name, 1.0, "Subject company")
            )
            return cursor.lastrowid
    
    def cache_search_query(self, query_text: str, company_name: str, 
                          market_cell: str, metric_name: str,
                          response: str, grounding_metadata: Dict) -> str:
        """Cache search query results for 30 days"""
        query_hash = hashlib.md5(f"{company_name}_{market_cell}_{metric_name}_{query_text}".encode()).hexdigest()
        expires_at = datetime.now() + timedelta(days=30)
        
        with self.get_connection() as conn:
            conn.execute("""
                INSERT OR REPLACE INTO search_cache 
                (query_hash, query_text, company_name, market_cell, metric_name, 
                 response_json, grounding_metadata_json, expires_at)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                query_hash, query_text, company_name, market_cell, metric_name,
                response, json.dumps(grounding_metadata), expires_at
            ))
        
        return query_hash
    
    def get_cached_search(self, query_text: str, company_name: str, 
                         market_cell: str, metric_name: str) -> Optional[sqlite3.Row]:
        """Get cached search result if still valid"""
        query_hash = hashlib.md5(f"{company_name}_{market_cell}_{metric_name}_{query_text}".encode()).hexdigest()
        
        with self.get_connection() as conn:
            cursor = conn.execute("""
                SELECT * FROM search_cache 
                WHERE query_hash = ? AND expires_at > CURRENT_TIMESTAMP
            """, (query_hash,))
            return cursor.fetchone()
    
    def cleanup_expired_cache(self):
        """Remove expired cache entries"""
        with self.get_connection() as conn:
            result = conn.execute("DELETE FROM search_cache WHERE expires_at <= CURRENT_TIMESTAMP")
            if result.rowcount > 0:
                thread_safe_print(f"Cleaned up {result.rowcount} expired cache entries")
    
    def get_database_stats(self) -> Dict[str, int]:
        """Get database statistics for monitoring"""
        stats = {}
        with self.get_connection() as conn:
            # Count records in each table
            tables = ['companies', 'market_cells', 'competitors', 'metrics', 
                     'observations', 'sources', 'search_cache', 'strategy_bundles']
            
            for table in tables:
                cursor = conn.execute(f"SELECT COUNT(*) FROM {table}")
                stats[table] = cursor.fetchone()[0]
        
        return stats
#!/usr/bin/env python3
"""
Quick test to verify DDAR modules are working
"""

try:
    from fact_extractor import FactExtractorV5 as FactExtractor
    from calculation_engine import CalculationEngine
    from data_availability import DataAvailabilityTracker
    from report_generator import ReportGenerator
    from ddar_adapter import DDARAdapter
    
    print("[OK] All imports successful")
    
    # Test instantiation
    extractor = FactExtractor()
    print("[OK] FactExtractor instantiated")
    
    calculator = CalculationEngine()
    print("[OK] CalculationEngine instantiated")
    
    availability_tracker = DataAvailabilityTracker()
    print("[OK] DataAvailabilityTracker instantiated")
    
    report_generator = ReportGenerator()
    print("[OK] ReportGenerator instantiated")
    
    adapter = DDARAdapter()
    print("[OK] DDARAdapter instantiated")
    
    print("\n[SUCCESS] DDAR system is ready to run!")
    
except ImportError as e:
    print(f"[ERROR] Import error: {e}")
except Exception as e:
    print(f"[ERROR] {e}")
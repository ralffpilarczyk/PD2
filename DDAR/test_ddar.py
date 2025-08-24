#!/usr/bin/env python3
"""
Quick test to verify DDAR modules are working
"""

try:
    from fact_extractor import FactExtractor
    from calculation_engine import CalculationEngine
    from data_availability import DataAvailabilityTracker
    from report_generator import ReportGenerator
    from ddar_adapter import DDARAdapter
    
    print("✓ All imports successful")
    
    # Test instantiation
    extractor = FactExtractor()
    print("✓ FactExtractor instantiated")
    
    calculator = CalculationEngine()
    print("✓ CalculationEngine instantiated")
    
    availability_tracker = DataAvailabilityTracker()
    print("✓ DataAvailabilityTracker instantiated")
    
    report_generator = ReportGenerator()
    print("✓ ReportGenerator instantiated")
    
    adapter = DDARAdapter()
    print("✓ DDARAdapter instantiated")
    
    print("\n✅ DDAR system is ready to run!")
    
except ImportError as e:
    print(f"❌ Import error: {e}")
except Exception as e:
    print(f"❌ Error: {e}")
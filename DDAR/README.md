# DDAR MVP - Deductive Database and Reasoning System

A minimal viable implementation of the DDAR system for M&A analysis using deterministic theorem-based reasoning.

## Overview

DDAR applies financial theorems to facts extracted from company filings to derive strategic recommendations with complete audit trails. This MVP demonstrates the core concept using:

- Python for fact extraction and orchestration
- SWI-Prolog for theorem evaluation
- Principle-based theorems without hardcoded thresholds

## Key Principles

1. **Deterministic**: Same inputs always produce identical outputs
2. **Traceable**: Every conclusion links to source facts via IDs
3. **Threshold-free**: Uses comparisons and trends, not magic numbers
4. **Stateless**: Fresh Prolog process for each analysis

## Files

- `canonicalizer.py` - Number normalization (4 decimal places)
- `fact_extractor.py` - Extract financial metrics from markdown
- `prolog_interface.py` - Python-Prolog bridge
- `helpers.pl` - Prolog utility predicates
- `theorems.pl` - Principle-based financial theorems
- `engine.pl` - Main Prolog engine
- `test_simple.py` - Test with hardcoded facts
- `run_mvp.py` - Full pipeline with real data

## Installation

```bash
# Install SWI-Prolog
brew install swi-prolog

# Install Python interface
pip install pyswip
```

## Usage

```bash
# Test with simple data
python test_simple.py

# Run full analysis on Axiata
python run_mvp.py
```

## Theorems Implemented

1. **Value Creation**: ROIC/ROE vs WACC
2. **Margin Trends**: Detect deterioration
3. **Capital Efficiency**: Capital intensity trends
4. **Valuation Drivers**: ROE → P/B, Margins → EV/Sales

All theorems use directional comparisons and trends, no arbitrary thresholds.
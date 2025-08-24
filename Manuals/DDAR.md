# DDAR - Deductive Database & Audit Reasoner
## Complete System Documentation

---

## 🎯 Executive Summary

DDAR (Deductive Database & Audit Reasoner) is a sophisticated financial analysis system that uses Prolog-based theorem proving to derive financial insights from company data. Unlike traditional classification systems that label metrics as "good" or "bad", DDAR uses **reverse engineering** to calculate what changes are needed to improve financial metrics.

### Key Philosophy
- **No arbitrary thresholds** (e.g., "15% margin = good")
- **Actionable insights** via sensitivity analysis
- **Mathematical relationships** between financial metrics
- **Cascade effect analysis** to understand interdependencies

---

## 🏗️ System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         USER INTERFACE                       │
│                        (ddar_main.py)                       │
└─────────────────────────┬───────────────────────────────────┘
                          │
┌─────────────────────────▼───────────────────────────────────┐
│                      PYTHON LAYER                           │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │fact_extractor│  │canonicalizer │  │ddar_adapter  │     │
│  └──────────────┘  └──────────────┘  └──────────────┘     │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │calc_engine   │  │data_avail    │  │report_gen    │     │
│  └──────────────┘  └──────────────┘  └──────────────┘     │
└─────────────────────────┬───────────────────────────────────┘
                          │
                    ┌─────▼─────┐
                    │ddar_bridge│
                    └─────┬─────┘
                          │
┌─────────────────────────▼───────────────────────────────────┐
│                      PROLOG LAYER                           │
│  ┌──────────────────────────────────────────────────┐      │
│  │              theorem_engine.pl                    │      │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐       │      │
│  │  │Pass 1:   │→ │Pass 2:   │→ │Pass 3:   │       │      │
│  │  │Calculate │  │Sensitivity│  │Cascade   │       │      │
│  │  └──────────┘  └──────────┘  └──────────┘       │      │
│  └──────────────────────────────────────────────────┘      │
│  ┌──────────────────────────────────────────────────┐      │
│  │               theorems.pl                         │      │
│  │  • 89 Financial Theorems                          │      │
│  │  • Mathematical Functions (ln, exp, sqrt, ncdf)   │      │
│  │  • Generic Sensitivity Analysis                   │      │
│  └──────────────────────────────────────────────────┘      │
│  ┌──────────────┐  ┌──────────────────────────────┐        │
│  │precalculations│  │iterative_analysis.pl        │        │
│  └──────────────┘  └──────────────────────────────┘        │
└──────────────────────────────────────────────────────────────┘
```

---

## 📁 File-by-File Breakdown

### Python Layer Files

#### 1. **ddar_main.py** - Main Entry Point
```python
# Primary orchestrator for DDAR analysis
# Functions:
- main(): CLI interface for running DDAR
- process_company(): Coordinates the analysis pipeline
- display_results(): Formats and shows results
```

#### 2. **fact_extractor.py** - Data Extraction
```python
# Extracts financial facts from various sources
# Key Features:
- Parses financial statements
- Extracts key metrics (revenue, costs, assets, etc.)
- Normalizes data formats
- Handles multiple data sources
```

#### 3. **canonicalizer.py** - Data Standardization
```python
# Standardizes financial terminology
# Examples:
- Maps "Sales" → "revenue"
- Maps "COGS" → "cost_of_goods_sold"
- Maps "EBITDA" → "ebitda"
- Ensures consistent naming across sources
```

#### 4. **ddar_adapter.py** - Python-Prolog Bridge
```python
# Interfaces between Python and Prolog
# Key Methods:
- load_facts(): Sends facts to Prolog
- run_analysis(): Triggers Prolog reasoning
- get_results(): Retrieves derived facts
- Uses pyswip for Prolog integration
```

#### 5. **calculation_engine.py** - Computation Support
```python
# Handles complex calculations Python-side
# Features:
- Pre-calculations before Prolog
- Post-processing of results
- Numerical stability checks
- Unit conversions
```

#### 6. **data_availability.py** - Data Completeness
```python
# Tracks what data is available
# Functions:
- check_completeness(): Identifies missing data
- suggest_alternatives(): Proposes proxy metrics
- confidence_scoring(): Rates data quality
```

#### 7. **report_generator.py** - Output Generation
```python
# Creates human-readable reports
# Output Formats:
- HTML reports with visualizations
- JSON for programmatic access
- Executive summaries
- Detailed sensitivity tables
```

### Prolog Layer Files

#### 8. **ddar_bridge.pl** - Prolog Interface
```prolog
% Bridge between Python and Prolog theorems
% Predicates:
- python_to_prolog/2: Converts Python data
- execute_analysis/2: Runs analysis pipeline
- prolog_to_python/2: Formats results for Python
```

#### 9. **theorem_engine.pl** - Orchestration Engine
```prolog
% Multi-pass analysis orchestrator
% Three-Pass System:
Pass 1: Pre-calculations & Basic Theorems
Pass 2: Sensitivity Analysis (all theorems)
Pass 3: Cascade Analysis (iterative)

% Key Predicates:
- analyze_company/2: Main entry point
- execute_pass_1/1: Calculate all possible theorems
- execute_pass_2/1: Run sensitivity on outputs
- execute_pass_3/1: Check cascade effects
- propagate_derived_facts/0: Convert derived to facts
```

#### 10. **theorems.pl** - Core Theorem Definitions
```prolog
% 89 Financial theorems organized by category
% Format: theorem(Name, Output, [Inputs], Formula)

% Categories:
1. Profitability (ROE, ROA, ROIC)
2. Efficiency (Asset turnover, Working capital)
3. Leverage (Debt ratios, Interest coverage)
4. Liquidity (Current ratio, Quick ratio)
5. Valuation (Multiples, DCF components)
6. Options & Credit (Black-Scholes, Merton)
7. Capital Structure (WACC, Beta, CAPM)
8. Working Capital (DSO, DIO, DPO impacts)

% Generic Sensitivity Engine:
- sensitivity_analysis/4: For ANY theorem
- reverse_engineer/4: Calculate required inputs
```

#### 11. **precalculations.pl** - Pre-Processing
```prolog
% Calculates derived metrics before main analysis
% Examples:
- gross_profit from revenue - cogs
- working_capital from current_assets - current_liabilities
- free_cash_flow components
- Prepares data for theorem application
```

#### 12. **iterative_analysis.pl** - Cascade Analysis
```prolog
% Iterative optimization with constraint checking
% Process:
1. Apply sensitivity improvements
2. Check constraint violations
3. Adjust recommendations
4. Iterate until convergence

% Key Features:
- Multi-objective optimization
- Constraint satisfaction
- Trade-off analysis
- Convergence detection
```

---

## 🔄 Data Flow Diagram

```
INPUT DATA
    │
    ▼
┌──────────────┐
│fact_extractor│ ─── Raw financial data
└──────┬───────┘
       │
       ▼
┌──────────────┐
│canonicalizer │ ─── Standardized terms
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ddar_adapter  │ ─── Load facts to Prolog
└──────┬───────┘
       │
       ▼
┌──────────────┐
│precalculations│ ─── Compute basic metrics
└──────┬───────┘
       │
       ▼
┌──────────────┐
│theorem_engine│ ─── Apply all theorems
└──────┬───────┘
       │
       ├────► Pass 1: Calculate all possible outputs
       │
       ├────► Pass 2: Sensitivity analysis
       │
       └────► Pass 3: Cascade effects
              │
              ▼
┌──────────────┐
│report_gen    │ ─── Format results
└──────┬───────┘
       │
       ▼
   OUTPUT REPORTS
```

---

## 🧮 Theorem Categories & Examples

### 1. Basic Profitability Theorems
```prolog
% DuPont Analysis
theorem(dupont, roe, [npm, at, fl], multiply([npm, at, fl])).
% ROE = Net Profit Margin × Asset Turnover × Financial Leverage

% Example Sensitivity Output:
"To improve ROE by 30%:
 - Increase NPM from 7% to 9.1% (all else equal)
 - OR increase Asset Turnover from 0.5 to 0.65
 - OR increase Financial Leverage from 2.0 to 2.6"
```

### 2. Advanced Option Theorems (Pack 1)
```prolog
% Black-Scholes-Merton for Options
theorem(bsm_d1, d1, [spot, strike, r, q, sigma, 'T'],
    formula((ln(spot/strike) + (r - q + 0.5*sigma*sigma)*'T') / (sigma*sqrt('T')))).

% Merton Credit Model
theorem(merton_pd_T, pd_T, [merton_d2], formula(ncdf(-merton_d2))).

% Example Output:
"To reduce default probability by 50%:
 - Increase asset value from $1B to $1.3B
 - OR reduce asset volatility from 30% to 22%"
```

### 3. Working Capital Impact (Pack 4)
```prolog
% Cash impact from DSO reduction
theorem(cash_delta_from_dso_change, cash_delta_dso,
    [delta_dso, revenue, days_in_period],
    formula(-delta_dso * revenue / days_in_period)).

% Example Output:
"Reducing DSO by 5 days will free up $13.7M in cash"
```

---

## 🔀 Sensitivity Analysis Algorithm

```
For each theorem with calculated output:
1. Identify all input variables
2. For each input variable:
   a. Hold other inputs constant
   b. Calculate required change for target improvement
   c. Store as sensitivity scenario
3. Rank scenarios by feasibility
4. Check cascade effects on other metrics
```

### Visual Example:
```
        CURRENT STATE           TARGET (+30%)
        ┌─────────┐            ┌─────────┐
        │ROE = 10%│   =====>   │ROE = 13%│
        └─────────┘            └─────────┘
              │                      ▲
              │                      │
        ┌─────▼─────┐          ┌─────┴─────┐
        │ INPUTS:   │          │ OPTIONS:  │
        │ NPM: 5%   │          │ NPM: 6.5% │
        │ AT: 1.0   │    OR    │ AT: 1.3   │
        │ FL: 2.0   │          │ FL: 2.6   │
        └───────────┘          └───────────┘
```

---

## 🔄 Iterative Cascade Analysis

```
ITERATION 1: Apply improvement
    │
    ▼
Check constraints:
- Debt/Equity < 3.0? ✓
- Interest Coverage > 2.0? ✓
- Current Ratio > 1.0? ✗
    │
    ▼
ITERATION 2: Adjust for constraint
    │
    ▼
Re-check all constraints... 
    │
    ▼
CONVERGED: All constraints satisfied
```

---

## 💡 Key Innovations

### 1. **Reverse Engineering vs Classification**
```
Traditional: "Operating margin of 15% is GOOD ✓"
DDAR: "To achieve 15% operating margin, reduce costs by 8% OR increase prices by 3%"
```

### 2. **Multi-Path Solutions**
```
Goal: Improve WACC by 50bps
Path 1: Shift D/E from 0.8 to 0.6
Path 2: Reduce cost of equity via lower beta
Path 3: Refinance debt at lower rate
```

### 3. **Constraint-Aware Recommendations**
```
Initial: "Increase leverage to improve ROE"
Constraint Check: "Would violate debt covenant"
Adjusted: "Improve asset turnover instead"
```

---

## 🚀 Quick Start Example

### Input Facts:
```prolog
fact(apple, revenue, 365000).
fact(apple, cogs, 220000).
fact(apple, operating_income, 100000).
fact(apple, total_assets, 340000).
fact(apple, total_equity, 90000).
```

### DDAR Processing:
```
Pass 1: Calculates gross_margin, operating_margin, ROA, ROE
Pass 2: Generates sensitivities for each metric
Pass 3: Checks interaction effects
```

### Output:
```json
{
  "derived_metrics": {
    "gross_margin": 0.397,
    "roe": 0.444,
    "asset_turnover": 1.074
  },
  "sensitivities": {
    "roe_improvement_30%": [
      {"change": "increase_net_margin", "from": 0.15, "to": 0.195},
      {"change": "increase_asset_turnover", "from": 1.07, "to": 1.39},
      {"change": "increase_leverage", "from": 3.78, "to": 4.91}
    ]
  },
  "cascade_warnings": [
    "Increasing leverage beyond 4.5 may trigger debt covenants"
  ]
}
```

---

## 🛠️ Extending DDAR

### Adding New Theorems:
```prolog
% Step 1: Define theorem in theorems.pl
theorem(new_metric, output_name, [input1, input2], 
        formula(input1 / input2)).

% Step 2: Add dependencies if needed
depends_on(new_metric, [input1, input2]).

% Step 3: Sensitivity analysis works automatically!
```

### Adding New Mathematical Functions:
```prolog
% In theorems.pl, add to eval_expr:
eval_expr(new_function(X), Env, Val) :- !,
    eval_expr(X, Env, VX),
    % Your calculation here
    Val is custom_calculation(VX).
```

---

## 🔍 Debugging Tips

### 1. Test Individual Theorems:
```bash
swipl -g "consult('theorems/theorem_engine.pl'), \
         assertz(fact(test, revenue, 1000)), \
         assertz(fact(test, cogs, 600)), \
         apply_all_theorems(test), \
         fact(test, gross_margin, GM), \
         format('GM: ~2f~n', [GM]), halt."
```

### 2. Trace Execution:
```prolog
?- trace, analyze_company(apple, Results).
```

### 3. Check Available Facts:
```prolog
?- fact(Company, Metric, Value).
```

---

## 📊 Performance Characteristics

- **Theorem Count**: 89 financial theorems
- **Calculation Speed**: ~100ms for 50 facts
- **Sensitivity Generation**: ~500ms for all theorems
- **Cascade Analysis**: 1-5 iterations typical
- **Memory Usage**: ~50MB for typical analysis

---

## 🎯 Use Cases

1. **Investment Analysis**: Identify improvement levers for portfolio companies
2. **Credit Assessment**: Calculate distance to default and improvement paths
3. **Strategic Planning**: Quantify impact of operational improvements
4. **M&A Evaluation**: Assess synergy potential via metric improvements
5. **Performance Management**: Set achievable, quantified targets

---

## 🔧 Maintenance Notes

### Common Issues:
1. **Missing Facts**: Theorems won't fire without all inputs
2. **Circular Dependencies**: Avoided via topological sorting
3. **Numerical Precision**: Use proper decimal representation
4. **Convergence**: Cascade analysis limited to 10 iterations

### Best Practices:
1. Always run `propagate_derived_facts` after calculations
2. Use quoted atoms for special variables (e.g., 'T' for time)
3. Test new theorems with sample data first
4. Document theorem assumptions and constraints

---

## 📚 References

- **DuPont Analysis**: Financial ratio decomposition
- **Black-Scholes-Merton**: Option pricing theory
- **Merton Model**: Structural credit risk
- **WACC**: Weighted average cost of capital
- **Prolog**: Logic programming language
- **pyswip**: Python-SWI Prolog bridge

---

## 🤝 Integration Points

### Input Sources:
- Financial statements (10-K, 10-Q)
- Market data (prices, rates)
- Company filings
- API feeds

### Output Consumers:
- Executive dashboards
- Risk management systems
- Portfolio optimization tools
- Automated reporting

---

## 📈 Future Enhancements

1. **Machine Learning Integration**: Learn typical improvement paths
2. **Industry Benchmarking**: Compare sensitivities to peers
3. **Scenario Planning**: Multi-variable optimization
4. **Real-time Analysis**: Stream processing of market data
5. **Natural Language Interface**: Query theorems in plain English

---

## 📝 Summary

DDAR transforms financial analysis from static classification to dynamic optimization. By combining:
- **89 mathematical theorems**
- **Reverse engineering via sensitivity analysis**
- **Cascade effect checking**
- **Constraint satisfaction**

It provides actionable, quantified paths to improve any financial metric rather than just labeling current performance.

**Key Insight**: "Don't tell me it's bad, tell me how to fix it!"

---

*Last Updated: August 24, 2024*
*Version: 2.0 (with Advanced Financial Theorems)*
%%% Enhanced Reasoning Chains for Theorem Analysis
%%% Provides detailed reasoning steps and chain-of-thought logic

:- dynamic reasoning_step/4.  % reasoning_step(Company, Theorem, StepNum, Description)
:- dynamic reasoning_chain/3. % reasoning_chain(Company, Theorem, Chain)
:- dynamic insight/4.         % insight(Company, Theorem, Type, Description)

% ========================================
% REASONING CHAIN BUILDERS
% ========================================

% Build reasoning chain for ROIC analysis
build_roic_chain(Company, ROIC, Chain) :-
    % Get components
    (get_fact_value(Company, npm, NPM) -> true ; NPM = 0),
    (get_fact_value(Company, at, AT) -> true ; AT = 0),
    (get_fact_value(Company, wacc, WACC) -> true ; WACC = 0.10),
    
    % Calculate value spread
    ValueSpread is ROIC - WACC,
    
    % Build chain
    Chain = [
        step(1, calculation, format_atom('ROIC = NPM × Asset Turnover = ~2f% × ~2fx = ~2f%', [NPM*100, AT, ROIC*100])),
        step(2, benchmark, format_atom('Industry average ROIC: ~2f%, Company ROIC: ~2f%', [12.0, ROIC*100])),
        step(3, spread, format_atom('Value Spread (ROIC - WACC): ~2f% - ~2f% = ~2f%', [ROIC*100, WACC*100, ValueSpread*100])),
        step(4, diagnosis, diagnose_roic(ROIC, NPM, AT)),
        step(5, implication, roic_implication(ValueSpread))
    ].

% Diagnose ROIC issues
diagnose_roic(ROIC, NPM, AT) :-
    ROIC < 0.10,
    NPM < 0.10,
    AT < 1.0,
    atom_string(Diagnosis, "Both profitability and asset efficiency are suboptimal").
    
diagnose_roic(ROIC, NPM, AT) :-
    ROIC < 0.10,
    NPM < 0.10,
    AT >= 1.0,
    atom_string(Diagnosis, "Primary issue: Low profit margins despite reasonable asset turnover").
    
diagnose_roic(ROIC, NPM, AT) :-
    ROIC < 0.10,
    NPM >= 0.10,
    AT < 1.0,
    atom_string(Diagnosis, "Primary issue: Poor asset utilization despite healthy margins").
    
diagnose_roic(ROIC, _, _) :-
    ROIC >= 0.10,
    atom_string(Diagnosis, "Capital efficiency meets minimum threshold").

% ROIC implications
roic_implication(Spread) :-
    Spread < 0,
    AbsSpread is abs(Spread),
    format(atom(Implication), 'Destroying ~2f% of capital value annually', [AbsSpread*100]).
    
roic_implication(Spread) :-
    Spread >= 0,
    Spread < 0.03,
    atom_string(Implication, "Marginal value creation - barely covering cost of capital").
    
roic_implication(Spread) :-
    Spread >= 0.03,
    format(atom(Implication), 'Creating ~2f% economic value on deployed capital', [Spread*100]).

% Build reasoning chain for FCF analysis
build_fcf_chain(Company, FCF, Chain) :-
    % Get components
    (get_fact_value(Company, operating_cash_flow, OCF) -> true ; OCF = 0),
    (get_fact_value(Company, capex, CapEx) -> true ; CapEx = 0),
    (get_fact_value(Company, revenue, Revenue) -> true ; Revenue = 1),
    (get_fact_value(Company, ebitda, EBITDA) -> true ; EBITDA = 1),
    
    % Calculate ratios
    (OCF > 0 -> CapExIntensity is CapEx / OCF ; CapExIntensity = 999),
    FCFMargin is FCF / Revenue,
    (EBITDA > 0 -> FCFConversion is FCF / EBITDA ; FCFConversion = 0),
    
    % Build chain
    Chain = [
        step(1, calculation, format_atom('FCF = OCF - CapEx = $~0fM - $~0fM = $~0fM', 
                                         [OCF/1000000, CapEx/1000000, FCF/1000000])),
        step(2, intensity, format_atom('CapEx intensity: ~1fx of operating cash flow', [CapExIntensity])),
        step(3, margin, format_atom('FCF margin: ~1f% of revenue', [FCFMargin*100])),
        step(4, conversion, format_atom('FCF/EBITDA conversion: ~1f%', [FCFConversion*100])),
        step(5, diagnosis, diagnose_fcf(FCF, CapExIntensity, FCFMargin))
    ].

% Diagnose FCF issues
diagnose_fcf(FCF, CapExIntensity, FCFMargin) :-
    FCF < 0,
    CapExIntensity > 1.5,
    format(atom(Diagnosis), 'Negative FCF driven by excessive CapEx (~1fx OCF) - unsustainable without external funding', [CapExIntensity]).
    
diagnose_fcf(FCF, _, FCFMargin) :-
    FCF < 0,
    FCFMargin < -0.10,
    format(atom(Diagnosis), 'Severe cash burn at ~1f% of revenue - immediate action required', [FCFMargin*100]).
    
diagnose_fcf(FCF, _, FCFMargin) :-
    FCF > 0,
    FCFMargin > 0.10,
    format(atom(Diagnosis), 'Healthy FCF generation at ~1f% margin supports growth and returns', [FCFMargin*100]).
    
diagnose_fcf(FCF, _, FCFMargin) :-
    FCF > 0,
    format(atom(Diagnosis), 'Positive but modest FCF at ~1f% of revenue', [FCFMargin*100]).

% Build reasoning chain for Cash Conversion Cycle
build_ccc_chain(Company, CCC, Chain) :-
    % Get components
    (get_fact_value(Company, dso, DSO) -> true ; DSO = 0),
    (get_fact_value(Company, dio, DIO) -> true ; DIO = 0),
    (get_fact_value(Company, dpo, DPO) -> true ; DPO = 0),
    (get_fact_value(Company, revenue, Revenue) -> true ; Revenue = 0),
    
    % Calculate working capital impact
    WorkingCapitalTied is CCC * Revenue / 365,
    
    % Build chain
    Chain = [
        step(1, calculation, format_atom('CCC = DSO + DIO - DPO = ~1f + ~1f - ~1f = ~1f days', 
                                         [DSO, DIO, DPO, CCC])),
        step(2, components, analyze_ccc_components(DSO, DIO, DPO)),
        step(3, impact, format_atom('Working capital tied up: $~1fM', [WorkingCapitalTied/1000000])),
        step(4, benchmark, format_atom('Industry average CCC: ~1f days, Company: ~1f days', [45.0, CCC])),
        step(5, opportunity, identify_ccc_opportunity(DSO, DIO, DPO, CCC))
    ].

% Analyze CCC components
analyze_ccc_components(DSO, DIO, DPO) :-
    format(atom(Analysis), 'Collection: ~0f days | Inventory: ~0f days | Payment: ~0f days', [DSO, DIO, DPO]).

% Identify CCC improvement opportunities
identify_ccc_opportunity(DSO, _, _, _) :-
    DSO > 45,
    format(atom(Opportunity), 'Accelerate collections: DSO of ~0f days exceeds best practice by ~0f days', 
           [DSO, DSO-35]).
    
identify_ccc_opportunity(_, DIO, _, _) :-
    DIO > 30,
    format(atom(Opportunity), 'Optimize inventory: DIO of ~0f days suggests excess stock', [DIO]).
    
identify_ccc_opportunity(_, _, DPO, _) :-
    DPO < 45,
    format(atom(Opportunity), 'Extend payment terms: DPO of ~0f days could be stretched', [DPO]).
    
identify_ccc_opportunity(_, _, _, CCC) :-
    CCC < 30,
    atom_string(Opportunity, "Excellent working capital management - maintain current practices").

% ========================================
% CHAIN GENERATION AND OUTPUT
% ========================================

% Generate and store reasoning chain for a theorem
generate_reasoning_chain(Company, TheoremName, OutputMetric, Value) :-
    % Build appropriate chain based on theorem type
    ( TheoremName = roic_decomp -> build_roic_chain(Company, Value, Chain)
    ; TheoremName = roic_nopat -> build_roic_chain(Company, Value, Chain)
    ; TheoremName = fcf_simple -> build_fcf_chain(Company, Value, Chain)
    ; TheoremName = ccc -> build_ccc_chain(Company, Value, Chain)
    ; Chain = [step(1, result, format_atom('~w calculated as ~4f', [OutputMetric, Value]))]
    ),
    
    % Store the chain
    assertz(reasoning_chain(Company, TheoremName, Chain)),
    
    % Extract and store insights
    extract_insights_from_chain(Company, TheoremName, Chain).

% Extract insights from reasoning chain
extract_insights_from_chain(_, _, []).
extract_insights_from_chain(Company, Theorem, [step(_, Type, Desc)|Rest]) :-
    (Type = diagnosis ; Type = implication ; Type = opportunity),
    assertz(insight(Company, Theorem, Type, Desc)),
    extract_insights_from_chain(Company, Theorem, Rest).
extract_insights_from_chain(Company, Theorem, [_|Rest]) :-
    extract_insights_from_chain(Company, Theorem, Rest).

% Format reasoning chain for output
format_reasoning_chain(Chain, FormattedChain) :-
    format_chain_steps(Chain, 1, FormattedChain).

format_chain_steps([], _, []).
format_chain_steps([step(Num, Type, Desc)|Rest], Num, [FormattedStep|FormattedRest]) :-
    format(atom(FormattedStep), '[~w] ~w: ~w', [Num, Type, Desc]),
    NextNum is Num + 1,
    format_chain_steps(Rest, NextNum, FormattedRest).

% ========================================
% INTEGRATION WITH MAIN THEOREM ENGINE
% ========================================

% Enhanced theorem application with reasoning
apply_theorem_with_reasoning(TheoremName, Company) :-
    calculate_theorem(TheoremName, Company, Result),
    theorem(TheoremName, OutputMetric, _, _),
    generate_reasoning_chain(Company, TheoremName, OutputMetric, Result).

% Apply all theorems with reasoning
apply_all_theorems_with_reasoning(Company) :-
    forall(
        (theorem(Name, _, _, _), can_apply_theorem(Name, Company)),
        apply_theorem_with_reasoning(Name, Company)
    ).

% Output reasoning for a company
output_company_reasoning(Company) :-
    format('~n=== Reasoning Chains for ~w ===~n', [Company]),
    forall(
        reasoning_chain(Company, Theorem, Chain),
        (
            format('~n~w:~n', [Theorem]),
            format_reasoning_chain(Chain, FormattedChain),
            forall(member(Step, FormattedChain), format('  ~w~n', [Step]))
        )
    ),
    format('~n=== Key Insights ===~n', []),
    forall(
        insight(Company, Theorem, Type, Desc),
        format('  [~w] ~w: ~w~n', [Theorem, Type, Desc])
    ).

% Helper to format atoms
format_atom(Template, Args, Atom) :-
    format(atom(Atom), Template, Args).
format_atom(Template, Args) :-
    format(atom(Atom), Template, Args),
    Atom.
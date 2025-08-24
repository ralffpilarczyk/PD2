%%% Unified Theorem Engine with Multi-Pass Sequential Reasoning
%%% Central orchestrator for all theorem execution

:- dynamic fact/3.
:- dynamic derived/3.
:- dynamic theorem_applied/4.
:- dynamic fact_period/3.
:- dynamic fact_confidence/3.

% Load all theorem modules
:- consult('theorems.pl').           % Unified theorem definitions & sensitivity analysis
:- consult('precalculations.pl').    % Pre-calculation adapters
:- consult('iterative_analysis.pl').  % Iterative cascade analysis
:- consult('reasoning_chains.pl').   % Enhanced reasoning and chain-of-thought

% ========================================
% UTILITY PREDICATES
% ========================================

% Get fact if present
get_fact(C, K, V) :- fact(C, K, V), !.

% Check if all facts exist
has_all_facts(_, []) :- !.
has_all_facts(C, [K|Ks]) :- fact(C, K, _), has_all_facts(C, Ks).

% Add derived fact (only if not exists)
add_derived(C, K, V) :- 
    \+ fact(C, K, _),  % Don't override existing facts
    assertz(derived(C, K, V)).

% Record theorem application for dependency tracking
record_theorem_applied(Theorem, Company, DerivedKey, DerivedValue) :-
    assertz(theorem_applied(Theorem, Company, DerivedKey, DerivedValue)).

% ========================================
% FACT PROPAGATION
% ========================================

% Convert all derived facts to regular facts
propagate_derived_facts :-
    forall(
        derived(C, K, V),
        (
            \+ fact(C, K, _) ->  % Only if fact doesn't exist
            assertz(fact(C, K, V))
            ; true
        )
    ),
    % Clear derived facts after propagation
    retractall(derived(_, _, _)).

% ========================================
% THEOREM EXECUTION PASSES
% ========================================

% Pass 1: Pre-calculations and Unified Theorem Calculations
execute_pass_1(Company) :-
    format('~n=== Pass 1: Pre-calculations & Theorem Calculations ===~n', []),
    
    % Run all pre-calculations
    run_all_precalculations(Company),
    
    % Apply all applicable theorems from unified theorems.pl
    ( apply_all_theorems(Company) -> 
        format('  Applied unified theorems~n')
    ; format('  No theorems applicable in pass 1~n')
    ),
    
    % Propagate all derived facts
    propagate_derived_facts.

% Pass 2: Sensitivity Analysis
execute_pass_2(Company) :-
    format('~n=== Pass 2: Sensitivity Analysis ===~n', []),
    
    % Run sensitivity analysis for all theorems with outputs
    TargetImprovement = 0.25,  % 25% improvement target
    
    findall(
        Name,
        (
            theorem(Name, Output, _, _),
            get_fact_value(Company, Output, _),
            sensitivity_analysis(Name, Company, TargetImprovement, Analysis),
            length(Analysis, NumPaths),
            format('  ~w: ~w paths to improvement~n', [Name, NumPaths])
        ),
        AnalyzedTheorems
    ),
    
    length(AnalyzedTheorems, NumTheorems),
    format('  Analyzed ~w theorems~n', [NumTheorems]),
    
    % Propagate derived facts
    propagate_derived_facts.

% Pass 3: Cascade Analysis (if needed)
execute_pass_3(Company) :-
    format('~n=== Pass 3: Cascade Analysis ===~n', []),
    
    % This pass is reserved for cascade/iterative analysis if needed
    format('  (Reserved for iterative cascade analysis)~n'),
    
    % Final propagation
    propagate_derived_facts.

% ========================================
% MAIN ANALYSIS ENTRY POINT
% ========================================

% Analyze a single company with multi-pass execution
analyze_company(Company, Results) :-
    format('~nAnalyzing: ~w~n', [Company]),
    format('~`=t~40|~n', []),
    
    % Execute all passes
    execute_pass_1(Company),
    execute_pass_2(Company),
    execute_pass_3(Company),
    
    % Collect all results
    collect_results(Company, Results),
    
    % Show summary
    show_summary(Company, Results).

% Collect results from all applied theorems
collect_results(Company, Results) :-
    findall(
        result(Theorem, Key, Value),
        theorem_applied(Theorem, Company, Key, Value),
        Results
    ).

% Show analysis summary
show_summary(Company, _Results) :-
    format('~n=== Summary for ~w ===~n', [Company]),
    
    % Count successful theorems
    findall(T, theorem_applied(T, Company, _, _), AppliedTheorems),
    list_to_set(AppliedTheorems, UniqueTheorems),
    length(UniqueTheorems, NumTheorems),
    format('Theorems successfully applied: ~w~n', [NumTheorems]),
    
    % Show key derived metrics
    format('~nKey metrics derived:~n', []),
    ( fact(Company, roe, ROE) -> 
        format('  ROE: ~2f%~n', [ROE * 100]) ; true ),
    ( fact(Company, roic, ROIC) -> 
        format('  ROIC: ~2f%~n', [ROIC * 100]) ; true ),
    ( fact(Company, value_spread, Spread) -> 
        format('  Value Spread (ROIC-WACC): ~2f%~n', [Spread * 100]) ; true ),
    ( fact(Company, eva, EVA) -> 
        format('  EVA: ~0f~n', [EVA]) ; true ),
    ( fact(Company, fcf, FCF) -> 
        format('  FCF: ~0f~n', [FCF]) ; true ).

% ========================================
% DEPENDENCY TRACKING
% ========================================

% Find which theorems enabled others
theorem_dependency(EnablerTheorem, EnabledTheorem, Company, DerivedFact) :-
    theorem_applied(EnablerTheorem, Company, DerivedFact, _),
    theorem_applied(EnabledTheorem, Company, _UsedFact, _),
    % Check if EnabledTheorem uses DerivedFact
    theorem_uses_fact(EnabledTheorem, DerivedFact).

% Define which theorems use which facts
theorem_uses_fact(t001_value_creation, roic).
theorem_uses_fact(t001_value_creation, wacc).
theorem_uses_fact(t001_value_roe, roe).
theorem_uses_fact(t041_eva, roic).
theorem_uses_fact(t041_eva, wacc).
theorem_uses_fact(t002_dupont, npm).
theorem_uses_fact(t002_dupont, at).
theorem_uses_fact(t002_dupont, fl).

% Generate reasoning chain
reasoning_chain(Company, Chain) :-
    findall(
        dependency(Enabler, Enabled, Fact),
        theorem_dependency(Enabler, Enabled, Company, Fact),
        Chain
    ).

% ========================================
% BATCH ANALYSIS
% ========================================

% Analyze multiple companies
analyze_companies([], []).
analyze_companies([Company|Rest], [Results|RestResults]) :-
    analyze_company(Company, Results),
    analyze_companies(Rest, RestResults).

% ========================================
% MAIN ENTRY POINT FOR PROLOG
% ========================================

% Run standard analysis on all companies with facts
run_analysis :-
    findall(C, fact(C, _, _), AllCompanies),
    list_to_set(AllCompanies, Companies),
    format('~nDDAR Theorem Engine - Multi-Pass Analysis~n', []),
    format('~`=t~50|~n', []),
    length(Companies, NumCompanies),
    format('Found ~w companies to analyze~n', [NumCompanies]),
    analyze_companies(Companies, _),
    format('~n~n'),
    format('~`=t~50|~n', []),
    format('Analysis Complete~n', []).

% Run iterative analysis on all companies
run_iterative_analysis :-
    findall(C, fact(C, _, _), AllCompanies),
    list_to_set(AllCompanies, Companies),
    format('~nDDAR Theorem Engine - Iterative Optimization~n', []),
    format('~`=t~50|~n', []),
    length(Companies, NumCompanies),
    format('Found ~w companies to analyze~n', [NumCompanies]),
    forall(member(Company, Companies),
        (analyze_company_iterative(Company, Analysis),
         report_iterative_analysis(Analysis))),
    format('~n~n'),
    format('~`=t~50|~n', []),
    format('Iterative Analysis Complete~n', []).
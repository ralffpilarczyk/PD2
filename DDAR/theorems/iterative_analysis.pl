%%% Iterative Cascade Analysis
%%% Applies suggested improvements, checks constraints, and iterates to find optimal solutions

:- dynamic hypothetical_fact/3.
:- dynamic iteration_history/3.
:- dynamic constraint_violation/3.

% ========================================
% MAIN ITERATIVE ANALYSIS
% ========================================

% Entry point for iterative analysis
analyze_company_iterative(Company, FinalAnalysis) :-
    retractall(iteration_history(_, _, _)),
    retractall(constraint_violation(_, _, _)),
    MaxIterations = 5,
    
    format('~n=== Starting Iterative Analysis for ~w ===~n', [Company]),
    iterative_loop(Company, 1, MaxIterations, [], FinalAnalysis).

% Main iteration loop
iterative_loop(Company, Iteration, MaxIterations, History, FinalAnalysis) :-
    format('~n--- Iteration ~w ---~n', [Iteration]),
    
    % Run sensitivity analysis
    run_all_sensitivities(Company, Sensitivities),
    
    % Check if we should stop
    ( should_stop(Iteration, MaxIterations, Sensitivities, History) ->
        format('Convergence achieved at iteration ~w~n', [Iteration]),
        FinalAnalysis = analysis(
            iterations(Iteration),
            history(History),
            final_state(Company),
            recommendations(Sensitivities)
        )
    ;
        % Select best improvements
        select_improvements(Sensitivities, History, SelectedImprovements),
        
        % Check if we have improvements to apply
        ( SelectedImprovements = [] ->
            format('No more improvements available~n', []),
            FinalAnalysis = analysis(
                iterations(Iteration),
                history(History),
                final_state(Company),
                recommendations(Sensitivities)
            )
        ;
            % Apply improvements hypothetically
            apply_hypothetical_changes(Company, SelectedImprovements, NewCompany),
            
            % Check cascade effects
            check_cascade_effects(Company, NewCompany, SelectedImprovements, CascadeEffects),
            
            % Adjust for constraint violations
            adjust_for_constraints(SelectedImprovements, CascadeEffects, AdjustedImprovements),
            
            % Convert hypothetical facts to real facts for next iteration
            commit_hypothetical_facts(Company),
            
            % Record this iteration
            assertz(iteration_history(Iteration, AdjustedImprovements, CascadeEffects)),
            NewHistory = [iteration(Iteration, AdjustedImprovements, CascadeEffects)|History],
            
            % Continue to next iteration
            NextIteration is Iteration + 1,
            iterative_loop(Company, NextIteration, MaxIterations, NewHistory, FinalAnalysis)
        )
    ).

% ========================================
% SENSITIVITY ANALYSIS
% ========================================

run_all_sensitivities(Company, Sensitivities) :-
    % Run sensitivity analysis for all applicable theorems
    % Target: 25% improvement for all metrics
    TargetImprovement = 0.25,
    
    % Find all theorems where we have the output metric
    findall(
        theorem_sensitivity(TheoremName, Analysis),
        (
            theorem(TheoremName, OutputMetric, _, _),
            get_fact_value(Company, OutputMetric, _),
            sensitivity_analysis(TheoremName, Company, TargetImprovement, Analysis)
        ),
        Sensitivities
    ),
    
    length(Sensitivities, NumSens),
    format('Found ~w sensitivity analyses~n', [NumSens]),
    
    % Debug: show what we got
    forall(member(theorem_sensitivity(Name, _), Sensitivities), 
           format('  Sensitivity for ~w~n', [Name])).

% ========================================
% HYPOTHETICAL CHANGES
% ========================================

% Apply hypothetical changes to create new company state
apply_hypothetical_changes(Company, [], Company) :- !.
apply_hypothetical_changes(Company, [Change|Rest], FinalCompany) :-
    apply_single_change(Company, Change, TempCompany),
    apply_hypothetical_changes(TempCompany, Rest, FinalCompany).

% Apply a single change
apply_single_change(Company, margin_improvement(Pct), NewCompany) :-
    % Don't clear all hypothetical facts - accumulate them
    fact(Company, npm, OldNPM),
    NewNPM is OldNPM * (1 + Pct/100),
    assertz(hypothetical_fact(Company, npm, NewNPM)),
    
    % Recalculate dependent metrics
    recalculate_margins(Company),
    recalculate_ratios(Company),
    
    NewCompany = Company.

apply_single_change(Company, efficiency_improvement(Pct), NewCompany) :-
    fact(Company, at, OldAT),
    NewAT is OldAT * (1 + Pct/100),
    assertz(hypothetical_fact(Company, at, NewAT)),
    
    recalculate_ratios(Company),
    NewCompany = Company.

apply_single_change(Company, revenue_growth(Pct), NewCompany) :-
    get_fact_value(Company, revenue, OldRevenue),
    NewRevenue is OldRevenue * (1 + Pct/100),
    assertz(hypothetical_fact(Company, revenue, NewRevenue)),
    
    % Recalculate dependent metrics
    recalculate_margins(Company),
    recalculate_ratios(Company),
    
    NewCompany = Company.

apply_single_change(Company, cost_reduction(Pct), NewCompany) :-
    get_fact_value(Company, revenue, Revenue),
    get_fact_value(Company, ebit, OpIncome),  % Use ebit
    OldCosts is Revenue - OpIncome,
    NewCosts is OldCosts * (1 - Pct/100),
    NewOpIncome is Revenue - NewCosts,
    
    assertz(hypothetical_fact(Company, ebit, NewOpIncome)),
    assertz(hypothetical_fact(Company, operating_income, NewOpIncome)),
    
    % Recalculate margins
    NewMargin is NewOpIncome / Revenue,
    assertz(hypothetical_fact(Company, operating_margin, NewMargin)),
    
    NewCompany = Company.

apply_single_change(Company, leverage_increase(Pct), NewCompany) :-
    get_fact_value(Company, fl, OldFL),
    NewFL is OldFL * (1 + Pct/100),
    assertz(hypothetical_fact(Company, fl, NewFL)),
    
    % Calculate impact on interest expense
    get_fact_value(Company, interest_expense, OldInterest),
    NewInterest is OldInterest * (1 + Pct/100),  % Simplified assumption
    assertz(hypothetical_fact(Company, interest_expense, NewInterest)),
    
    NewCompany = Company.

% Get fact value (hypothetical or actual)
get_fact_value(Company, Key, Value) :-
    ( hypothetical_fact(Company, Key, HypValue) ->
        Value = HypValue
    ; fact(Company, Key, Value)
    ).

% ========================================
% CASCADE EFFECTS CHECKING
% ========================================

check_cascade_effects(OldCompany, NewCompany, Changes, Effects) :-
    % Check liquidity impact
    check_liquidity_cascade(OldCompany, NewCompany, LiquidityEffect),
    
    % Check solvency impact
    check_solvency_cascade(OldCompany, NewCompany, SolvencyEffect),
    
    % Check profitability impact
    check_profitability_cascade(OldCompany, NewCompany, ProfitabilityEffect),
    
    Effects = [
        liquidity(LiquidityEffect),
        solvency(SolvencyEffect),
        profitability(ProfitabilityEffect)
    ],
    
    % Report effects
    format('Cascade effects:~n', []),
    forall(member(E, Effects), format('  ~w~n', [E])).

check_liquidity_cascade(OldCompany, NewCompany, Effect) :-
    fact(OldCompany, current_ratio, OldCR),  % Use original fact
    (hypothetical_fact(NewCompany, current_ratio, NewCR) -> true ; NewCR = OldCR),
    
    Change is NewCR - OldCR,
    ( NewCR < 1.0 ->
        Effect = violation(current_ratio_below_1, NewCR),
        assertz(constraint_violation(liquidity, current_ratio, NewCR))
    ; NewCR < 1.5, OldCR >= 1.5 ->
        Effect = degradation(current_ratio_weakened, NewCR)
    ; abs(Change) < 0.01 ->
        Effect = unchanged(current_ratio, NewCR)
    ; Effect = ok(current_ratio, NewCR)
    ).

check_solvency_cascade(OldCompany, NewCompany, Effect) :-
    fact(OldCompany, interest_coverage, OldIC),  % Use original fact
    % Calculate new interest coverage based on leverage changes
    (hypothetical_fact(NewCompany, interest_expense, NewIntExp) ->
        fact(OldCompany, ebit, EBIT),
        NewIC is EBIT / NewIntExp
    ; NewIC = OldIC),
    
    ( NewIC < 1.5 ->
        Effect = violation(interest_coverage_critical, NewIC),
        assertz(constraint_violation(solvency, interest_coverage, NewIC))
    ; NewIC < 2.0, OldIC >= 2.0 ->
        Effect = degradation(interest_coverage_weakened, NewIC)
    ; Effect = ok(interest_coverage, NewIC)
    ).

check_profitability_cascade(OldCompany, NewCompany, Effect) :-
    fact(OldCompany, roe, OldROE),  % Use original fact
    % Calculate new ROE from changed components
    get_fact_value(NewCompany, npm, NPM),
    get_fact_value(NewCompany, at, AT),
    get_fact_value(NewCompany, fl, FL),
    NewROE is NPM * AT * FL,
    
    Improvement is (NewROE / OldROE - 1) * 100,
    ( Improvement > 10 ->
        Effect = improvement(roe_increased, Improvement)
    ; Improvement < -5 ->
        Effect = degradation(roe_decreased, Improvement)
    ; Effect = stable(roe_unchanged, Improvement)
    ).

% ========================================
% IMPROVEMENT SELECTION
% ========================================

select_improvements(Sensitivities, History, Selected) :-
    % Extract all possible improvements
    extract_all_improvements(Sensitivities, AllImprovements),
    
    format('Extracted ~w improvements: ~w~n', [length(AllImprovements), AllImprovements]),
    
    % Filter out previously tried improvements
    filter_new_improvements(AllImprovements, History, NewImprovements),
    
    format('After filtering: ~w new improvements~n', [length(NewImprovements)]),
    
    % Rank improvements by feasibility and impact
    rank_improvements(NewImprovements, RankedImprovements),
    
    % Select top improvements
    select_top_improvements(RankedImprovements, 2, Selected),
    
    format('Selected improvements: ~w~n', [Selected]).

extract_all_improvements([], []) :- !.
extract_all_improvements([roe_sensitivity(analysis(_, _, scenarios(Scens), _, _))|Rest], Improvements) :-
    !,
    findall(Imp, 
        (member(S, Scens), extract_improvement_from_scenario(S, Imp)),
        ScenarioImps),
    extract_all_improvements(Rest, RestImps),
    append(ScenarioImps, RestImps, Improvements).
extract_all_improvements([margin_sensitivity(analysis(_, _, scenarios(Scens)))|Rest], Improvements) :-
    !,
    findall(Imp,
        (member(S, Scens), extract_margin_improvement(S, Imp)),
        ScenarioImps),
    extract_all_improvements(Rest, RestImps),
    append(ScenarioImps, RestImps, Improvements).
extract_all_improvements([_|Rest], Improvements) :-
    extract_all_improvements(Rest, Improvements).

extract_improvement_from_scenario(margin_focus(_, _, change_pct(Pct), _), margin_improvement(Pct)).
extract_improvement_from_scenario(efficiency_focus(_, _, change_pct(Pct), _), efficiency_improvement(Pct)).
extract_improvement_from_scenario(leverage_focus(_, _, change_pct(Pct), _), leverage_increase(Pct)).

extract_margin_improvement(revenue_growth(growth_pct(Pct), _), revenue_growth(Pct)).
extract_margin_improvement(cost_reduction(reduction_pct(Pct), _), cost_reduction(Pct)).
extract_margin_improvement(mixed_approach(revenue_growth(RG), cost_efficiency(CE), _), mixed_improvement(RG, CE)).

% ========================================
% CONSTRAINT ADJUSTMENT
% ========================================

adjust_for_constraints(Improvements, Effects, Adjusted) :-
    findall(V, member(violation(V, _), Effects), Violations),
    
    ( Violations = [] ->
        Adjusted = Improvements  % No violations, keep as is
    ;
        % Reduce aggressiveness of improvements
        format('Adjusting for violations: ~w~n', [Violations]),
        scale_down_improvements(Improvements, 0.5, Adjusted)
    ).

scale_down_improvements([], _, []).
scale_down_improvements([revenue_growth(Pct)|Rest], Factor, [revenue_growth(NewPct)|AdjRest]) :-
    NewPct is Pct * Factor,
    scale_down_improvements(Rest, Factor, AdjRest).
scale_down_improvements([cost_reduction(Pct)|Rest], Factor, [cost_reduction(NewPct)|AdjRest]) :-
    NewPct is Pct * Factor,
    scale_down_improvements(Rest, Factor, AdjRest).
scale_down_improvements([H|Rest], Factor, [H|AdjRest]) :-
    scale_down_improvements(Rest, Factor, AdjRest).

% ========================================
% CONVERGENCE CRITERIA
% ========================================

should_stop(Iteration, MaxIterations, Sensitivities, History) :-
    ( Iteration >= MaxIterations ->
        format('Maximum iterations reached~n', []),
        true
    ; (Iteration > 2, improvements_marginal(Sensitivities)) ->
        format('Improvements are now marginal~n', []),
        true
    ; (Iteration > 3, oscillation_detected(History)) ->
        format('Oscillation detected in solutions~n', []),
        true
    ; false
    ).

all_constraints_satisfied :-
    \+ constraint_violation(_, _, _).

improvements_marginal(Sensitivities) :-
    % Check if all improvements are less than 1%
    extract_all_improvements(Sensitivities, Improvements),
    forall(member(Imp, Improvements), 
        (extract_percentage(Imp, Pct), abs(Pct) < 1.0)).

extract_percentage(margin_improvement(Pct), Pct).
extract_percentage(efficiency_improvement(Pct), Pct).
extract_percentage(leverage_increase(Pct), Pct).
extract_percentage(revenue_growth(Pct), Pct).
extract_percentage(cost_reduction(Pct), Pct).

oscillation_detected(History) :-
    length(History, L),
    L >= 3,
    % Check if we're going back and forth
    History = [H1, H2, H3|_],
    similar_iterations(H1, H3).

similar_iterations(iteration(_, Imps1, _), iteration(_, Imps2, _)) :-
    % Simplified: check if improvements are opposite
    member(revenue_growth(P1), Imps1),
    member(revenue_growth(P2), Imps2),
    sign(P1) =\= sign(P2).

% ========================================
% UTILITY PREDICATES
% ========================================

% Convert hypothetical facts to real facts
commit_hypothetical_facts(Company) :-
    forall(
        hypothetical_fact(Company, Key, Value),
        (
            retractall(fact(Company, Key, _)),
            assertz(fact(Company, Key, Value))
        )
    ),
    retractall(hypothetical_fact(Company, _, _)).

recalculate_margins(Company) :-
    get_fact_value(Company, revenue, Revenue),
    (get_fact_value(Company, ebit, OpIncome) ; get_fact_value(Company, operating_income, OpIncome)),
    get_fact_value(Company, net_income, NetIncome),
    
    OpMargin is OpIncome / Revenue,
    NetMargin is NetIncome / Revenue,
    
    assertz(hypothetical_fact(Company, operating_margin, OpMargin)),
    assertz(hypothetical_fact(Company, npm, NetMargin)).

recalculate_ratios(Company) :-
    get_fact_value(Company, net_income, NI),
    get_fact_value(Company, total_equity, TE),
    get_fact_value(Company, total_assets, TA),
    
    ROE is NI / TE,
    ROA is NI / TA,
    
    assertz(hypothetical_fact(Company, roe, ROE)),
    assertz(hypothetical_fact(Company, roa, ROA)).

filter_new_improvements(All, History, New) :-
    % Remove improvements that were tried in recent iterations
    findall(Imp, 
        (member(Imp, All), \+ recently_tried(Imp, History)),
        New).

recently_tried(Imp, History) :-
    member(iteration(_, Imps, _), History),
    member(Imp, Imps).

rank_improvements(Improvements, Ranked) :-
    % Simple ranking: prefer smaller changes first
    findall(Score-Imp,
        (member(Imp, Improvements), score_improvement(Imp, Score)),
        ScoredImps),
    keysort(ScoredImps, SortedImps),
    findall(Imp, member(_-Imp, SortedImps), Ranked).

score_improvement(Imp, Score) :-
    extract_percentage(Imp, Pct),
    Score is abs(Pct).  % Prefer smaller changes

select_top_improvements([], _, []).
select_top_improvements(_, 0, []) :- !.
select_top_improvements([H|T], N, [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    select_top_improvements(T, N1, Rest).

% ========================================
% REPORTING
% ========================================

report_iterative_analysis(FinalAnalysis) :-
    FinalAnalysis = analysis(iterations(N), history(History), final_state(Company), recommendations(Recs)),
    
    format('~n=== Iterative Optimization Complete ===~n', []),
    format('Total iterations: ~w~n', [N]),
    format('~nOptimization path:~n', []),
    
    reverse(History, ChronHistory),
    forall(member(iteration(I, Imps, Effects), ChronHistory),
        (format('  Iteration ~w: ~w~n', [I, Imps]),
         format('    Effects: ~w~n', [Effects]))),
    
    format('~nFinal recommendations:~n', []),
    forall(member(R, Recs), format('  ~w~n', [R])),
    
    format('~nConstraints status:~n', []),
    ( all_constraints_satisfied ->
        format('  All constraints satisfied~n')
    ; findall(V, constraint_violation(Type, Metric, Value), Violations),
      format('  Violations remaining: ~w~n', [Violations])
    ).
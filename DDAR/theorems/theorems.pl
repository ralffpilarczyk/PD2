%%% Universal Financial Theorems
%%% Each theorem is a mathematical relationship that can be:
%%% 1. Calculated forward (given inputs → output)
%%% 2. Reverse engineered (given target output → required inputs)
%%%
%%% No arbitrary thresholds or classifications - only pure calculations

:- dynamic fact/3.
:- dynamic derived/3.
:- dynamic theorem_applied/4.

% ========================================
% THEOREM DEFINITIONS
% ========================================
% Format: theorem(Name, OutputMetric, InputMetrics, Formula)
% Formula types: 
%   multiply([a,b,c]) → a × b × c
%   divide(a, b) → a / b
%   add([a,b]) → a + b
%   subtract(a, b) → a - b
%   weighted([w1,w2], [a,b]) → w1×a + w2×b

% Profitability Theorems
theorem(dupont, roe, [npm, at, fl], multiply([npm, at, fl])).
theorem(dupont_extended, roe_extended, [tax_burden, interest_burden, ebit_margin, asset_turnover, leverage], 
        multiply([tax_burden, interest_burden, ebit_margin, asset_turnover, leverage])).

% Return Theorems  
theorem(roic_decomp, roic, [operating_margin, capital_turnover], multiply([operating_margin, capital_turnover])).
theorem(roic_nopat, roic, [nopat, invested_capital], divide(nopat, invested_capital)).
theorem(roa_basic, roa, [net_income, total_assets], divide(net_income, total_assets)).

% Cash Flow Theorems
theorem(fcf, fcf, [nopat, depreciation, capex, delta_wc], 
        formula(add([nopat, depreciation]) - add([capex, delta_wc]))).
theorem(fcf_simple, fcf_simple, [operating_cash_flow, capex], 
        subtract(operating_cash_flow, capex)).
theorem(fcf_conversion, fcf_conv, [fcf, ebitda], divide(fcf, ebitda)).
theorem(cfroi, cfroi, [operating_cash_flow, invested_capital], 
        divide(operating_cash_flow, invested_capital)).

% Value Creation Theorems
theorem(value_spread, value_spread, [roic, wacc], subtract(roic, wacc)).
theorem(eva, eva, [roic, wacc, invested_capital], 
        multiply([subtract(roic, wacc), invested_capital])).

% Growth Theorems
theorem(sgr, sustainable_growth_rate, [roe, retention_ratio], multiply([roe, retention_ratio])).
theorem(reinvestment_rate, reinvestment_rate, [capex, depreciation, delta_wc, nopat], 
        divide(formula(capex - depreciation + delta_wc), nopat)).

% Working Capital Theorems
theorem(ccc, cash_conversion_cycle, [dio, dso, dpo], 
        formula(dio + dso - dpo)).
theorem(operating_cycle, operating_cycle, [dio, dso], add([dio, dso])).
theorem(wc_turnover, working_capital_turnover, [revenue, working_capital], 
        divide(revenue, working_capital)).

% Asset Efficiency Theorems
theorem(asset_turnover, at, [revenue, total_assets], divide(revenue, total_assets)).
theorem(fixed_asset_turnover, fa_turnover, [revenue, fixed_assets], divide(revenue, fixed_assets)).
theorem(capital_intensity, capital_intensity, [total_assets, revenue], divide(total_assets, revenue)).
theorem(capital_turnover, capital_turnover, [revenue, invested_capital], divide(revenue, invested_capital)).

% Margin Theorems
theorem(gross_margin, gross_margin, [gross_profit, revenue], divide(gross_profit, revenue)).
theorem(operating_margin, operating_margin, [operating_income, revenue], divide(operating_income, revenue)).
theorem(ebitda_margin, ebitda_margin, [ebitda, revenue], divide(ebitda, revenue)).
theorem(net_margin, npm, [net_income, revenue], divide(net_income, revenue)).

% Leverage Theorems
theorem(financial_leverage, fl, [total_assets, total_equity], divide(total_assets, total_equity)).
theorem(debt_to_equity, debt_to_equity, [total_debt, total_equity], divide(total_debt, total_equity)).
theorem(interest_coverage, interest_coverage, [ebit, interest_expense], divide(ebit, interest_expense)).
theorem(debt_service_coverage, dscr, [ebitda, capex, debt_service], 
        divide(subtract(ebitda, capex), debt_service)).

% Liquidity Theorems (as calculations, not classifications)
theorem(current_ratio, current_ratio, [current_assets, current_liabilities], 
        divide(current_assets, current_liabilities)).
theorem(quick_ratio, quick_ratio, [current_assets, inventory, current_liabilities], 
        divide(subtract(current_assets, inventory), current_liabilities)).
theorem(cash_ratio, cash_ratio, [cash, cash_equivalents, current_liabilities], 
        divide(add([cash, cash_equivalents]), current_liabilities)).

% Altman Z-Score (Bankruptcy Prediction)
theorem(altman_z, z_score, [wc_ta, re_ta, ebit_ta, mve_tl, sales_ta],
        weighted([1.2, 1.4, 3.3, 0.6, 1.0], [wc_ta, re_ta, ebit_ta, mve_tl, sales_ta])).

% ========================================
% PACK 1: OPTIONS & DISTRESS (BLACK-SCHOLES-MERTON)
% ========================================

% Put-Call Parity with dividend yield
theorem(put_call_parity_gap, parity_gap,
        [call, put, spot, strike, r, q, 'T'],
        formula( call - put - (spot*exp(-q*'T') - strike*exp(-r*'T')) )).

% Black-Scholes-Merton d1 and d2
theorem(bsm_d1, d1, [spot, strike, r, q, sigma, 'T'],
        formula( (ln(spot/strike) + (r - q + 0.5*sigma*sigma)*'T') / (sigma*sqrt('T')) )).

theorem(bsm_d2, d2, [d1, sigma, 'T'],
        formula( d1 - sigma*sqrt('T') )).

% BSM Call and Put values
theorem(bsm_call, call, [spot, strike, r, q, 'T', d1, d2],
        formula( spot*exp(-q*'T')*ncdf(d1) - strike*exp(-r*'T')*ncdf(d2) )).

theorem(bsm_put, put, [spot, strike, r, q, 'T', d1, d2],
        formula( strike*exp(-r*'T')*ncdf(-d2) - spot*exp(-q*'T')*ncdf(-d1) )).

% Merton corporate model (assets V, asset vol sigma_asset)
theorem(merton_d1, merton_d1, [asset_value, debt_face, r, sigma_asset, 'T'],
        formula( (ln(asset_value/debt_face) + (r + 0.5*sigma_asset*sigma_asset)*'T') / (sigma_asset*sqrt('T')) )).

theorem(merton_d2, merton_d2, [merton_d1, sigma_asset, 'T'],
        formula( merton_d1 - sigma_asset*sqrt('T') )).

% Merton equity as call option on assets
theorem(merton_equity_value, equity_value_merton,
        [asset_value, debt_face, r, 'T', merton_d1, merton_d2],
        formula( asset_value*ncdf(merton_d1) - debt_face*exp(-r*'T')*ncdf(merton_d2) )).

% Merton debt value
theorem(merton_debt_value, debt_value_merton,
        [asset_value, equity_value_merton],
        subtract(asset_value, equity_value_merton)).

% Probability of default and distance to default
theorem(merton_pd_T, pd_T, [merton_d2], 
        formula( ncdf(-merton_d2) )).

theorem(merton_distance_to_default, distance_to_default, [merton_d2], 
        formula( merton_d2 )).

% Equity-asset volatility relationship
theorem(equity_vol_from_asset_vol, sigma_equity,
        [asset_value, equity_value_merton, merton_d1, sigma_asset],
        formula( (asset_value / equity_value_merton) * ncdf(merton_d1) * sigma_asset )).

% ========================================
% PACK 2: CAPITAL STRUCTURE & COST OF CAPITAL
% ========================================

% WACC with tax shield on debt
theorem(wacc_with_tax, wacc, [equity_value, debt_value, cost_of_equity, cost_of_debt_pre_tax, tax_rate],
        formula( (equity_value/(equity_value+debt_value))*cost_of_equity
               + (debt_value/(equity_value+debt_value))*cost_of_debt_pre_tax*(1 - tax_rate) )).

% CAPM cost of equity
theorem(capm_cost_of_equity, cost_of_equity, [risk_free_rate, beta_equity, market_premium],
        formula( risk_free_rate + beta_equity*market_premium )).

% Beta levering (Hamada formula)
theorem(beta_lever, beta_equity_from_asset,
        [beta_asset, debt_value, equity_value, tax_rate],
        formula( beta_asset * (1 + (1 - tax_rate) * debt_value / equity_value) )).

% Beta unlevering (asset beta from equity and debt betas)
theorem(beta_unlever, beta_asset_from_equity_debt,
        [beta_equity, beta_debt, debt_value, equity_value],
        formula( (equity_value/(equity_value+debt_value))*beta_equity
               + (debt_value/(equity_value+debt_value))*beta_debt )).

% Post-tax cost of debt
theorem(cost_of_debt_post_tax, cost_of_debt_post_tax, [cost_of_debt_pre_tax, tax_rate],
        formula( cost_of_debt_pre_tax * (1 - tax_rate) )).

% Enterprise value bridge
theorem(enterprise_value_bridge, enterprise_value,
        [equity_value, net_debt, minority_interest, associates],
        formula( (equity_value + net_debt + minority_interest) - associates )).

% ========================================
% PACK 4: WORKING CAPITAL DELTAS
% ========================================

% Working capital levels from cycle metrics
theorem(accounts_receivable_level, ar_level,
        [revenue, dso, days_in_period],
        formula( revenue * dso / days_in_period )).

theorem(inventory_level, inventory_level,
        [cogs, dio, days_in_period],
        formula( cogs * dio / days_in_period )).

theorem(accounts_payable_level, ap_level,
        [cogs, dpo, days_in_period],
        formula( cogs * dpo / days_in_period )).

theorem(working_capital_total, working_capital_total,
        [ar_level, inventory_level, ap_level],
        formula( (ar_level + inventory_level) - ap_level )).

% Cash delta from day changes (comparative)
theorem(cash_delta_from_dso_change, cash_delta_dso,
        [delta_dso, revenue, days_in_period],
        formula( - delta_dso * revenue / days_in_period )).

theorem(cash_delta_from_dio_change, cash_delta_dio,
        [delta_dio, cogs, days_in_period],
        formula( - delta_dio * cogs / days_in_period )).

theorem(cash_delta_from_dpo_change, cash_delta_dpo,
        [delta_dpo, cogs, days_in_period],
        formula( delta_dpo * cogs / days_in_period )).

% Aggregate cash delta
theorem(cash_delta_from_ccc_components, cash_delta_wc,
        [cash_delta_dso, cash_delta_dio, cash_delta_dpo],
        add([cash_delta_dso, cash_delta_dio, cash_delta_dpo])).

% ========================================
% PACK 3: VALUATION & MULTIPLES (STABLE-STATE)
% ========================================

% Perpetuity with growth (requires growth_rate < discount_rate)
theorem(perpetuity_with_growth, value_perpetuity_g,
        [cash_flow_next, discount_rate, growth_rate],
        formula( cash_flow_next / (discount_rate - growth_rate) )).

% Steady-state FCFF from EBIT
theorem(fcff_steady_state_from_ebit, fcff_ss,
        [ebit, tax_rate, growth_rate, roic],
        formula( ebit*(1 - tax_rate) * (1 - growth_rate/roic) )).

% EV/EBIT multiple in steady state
theorem(ev_over_ebit_multiple_ss, ev_ebit_multiple_ss,
        [wacc, growth_rate, tax_rate, roic],
        formula( ((1 - tax_rate) * (1 - growth_rate/roic)) / (wacc - growth_rate) )).

% Residual income components
theorem(equity_charge, equity_charge, [cost_of_equity, beginning_book_equity],
        multiply([cost_of_equity, beginning_book_equity])).

theorem(residual_income, residual_income, [net_income, equity_charge],
        subtract(net_income, equity_charge)).

% P/B in stable state (Gordon-style residual income)
theorem(price_to_book_ss, price_to_book_ss,
        [roe, cost_of_equity, growth_rate],
        formula( (roe - growth_rate) / (cost_of_equity - growth_rate) )).

% Terminal value calculations
theorem(terminal_value_gordon, terminal_value,
        [fcff_next, wacc, growth_rate],
        formula( fcff_next / (wacc - growth_rate) )).

theorem(terminal_value_share, terminal_value_share,
        [terminal_value, pv_explicit],
        formula( terminal_value / (terminal_value + pv_explicit) )).

% ========================================
% PACK 7: CREDIT & LOSS
% ========================================

% Expected loss identity
theorem(expected_loss_identity, expected_loss, [pd, lgd, ead],
        multiply([pd, lgd, ead])).

% Debt value from option decomposition
theorem(debt_value_from_option_decomp, debt_value_from_option,
        [debt_face, r, 'T', put_on_assets],
        formula( debt_face*exp(-r*'T') - put_on_assets )).

% Zero-coupon credit spread from debt value
theorem(credit_spread_from_debt_value, credit_spread,
        [debt_value, debt_face, r, 'T'],
        formula( -(1/T) * ln( debt_value / (debt_face*exp(-r*'T')) ) )).

% Constant-hazard rate from PD
theorem(hazard_rate_from_pd, hazard_rate, [pd_T, 'T'],
        formula( -(1/T) * ln(1 - pd_T) )).

% Fair yield from spread and risk-free
theorem(fair_yield_from_spread, fair_yield, [risk_free_rate, credit_spread],
        add([risk_free_rate, credit_spread])).

% Bond value with recovery
theorem(zcb_value_with_recovery, debt_value_with_recovery,
        [debt_face, r, 'T', pd_T, recovery_rate],
        formula( debt_face*exp(-r*'T') * ( (1 - pd_T) + pd_T*recovery_rate ) )).

% ========================================
% THEOREM DEPENDENCIES
% ========================================
% What feeds into what (for calculation ordering)

depends_on(sgr, [roe]).
depends_on(roe, [npm, at, fl]).
depends_on(eva, [roic, wacc, invested_capital]).
depends_on(value_spread, [roic, wacc]).
depends_on(roic, [operating_margin, capital_turnover]).
depends_on(fcf, [nopat, depreciation, capex, delta_wc]).

% Pack 1 dependencies (Options & Distress)
depends_on(bsm_d2, [d1]).
depends_on(bsm_call, [d1, d2]).
depends_on(bsm_put, [d1, d2]).
depends_on(merton_d2, [merton_d1]).
depends_on(merton_equity_value, [merton_d1, merton_d2]).
depends_on(merton_debt_value, [equity_value_merton]).
depends_on(merton_pd_T, [merton_d2]).
depends_on(merton_distance_to_default, [merton_d2]).
depends_on(equity_vol_from_asset_vol, [merton_d1]).

% Pack 2 dependencies (Capital Structure)
depends_on(wacc_with_tax, [cost_of_equity, cost_of_debt_post_tax]).
depends_on(cost_of_equity, [beta_equity]).
depends_on(cost_of_debt_post_tax, [cost_of_debt_pre_tax]).
depends_on(beta_equity_from_asset, [beta_asset]).

% Pack 3 dependencies (Valuation)
depends_on(residual_income, [equity_charge]).
depends_on(terminal_value_share, [terminal_value]).
depends_on(ev_over_ebit_multiple_ss, [wacc, roic]).

% Pack 4 dependencies (Working Capital)
depends_on(working_capital_total, [ar_level, inventory_level, ap_level]).
depends_on(cash_delta_wc, [cash_delta_dso, cash_delta_dio, cash_delta_dpo]).

% Pack 7 dependencies (Credit & Loss)
depends_on(fair_yield_from_spread, [credit_spread]).
depends_on(credit_spread_from_debt_value, [debt_value]).
depends_on(ccc, [dio, dso, dpo]).
depends_on(reinvestment_rate, [capex, depreciation, delta_wc, nopat]).
depends_on(dscr, [ebitda, capex, debt_service]).

% ========================================
% FORWARD CALCULATION
% ========================================

% Generic forward calculation for any theorem
calculate_theorem(TheoremName, Company, Result) :-
    theorem(TheoremName, OutputMetric, InputMetrics, Formula),
    get_input_values(Company, InputMetrics, Values),
    evaluate_formula(Formula, InputMetrics, Values, Result),
    store_derived(Company, OutputMetric, Result),
    % Track theorem application (avoid duplicates)
    ( theorem_applied(TheoremName, Company, OutputMetric, _) ->
        true  % Already recorded, skip
    ;
        assertz(theorem_applied(TheoremName, Company, OutputMetric, Result))
    ).

% Get values for input metrics
get_input_values(_, [], []).
get_input_values(Company, [Metric|Rest], [Value|RestValues]) :-
    get_fact_value(Company, Metric, Value),
    get_input_values(Company, Rest, RestValues).

% Get fact value (actual or derived)
get_fact_value(Company, Metric, Value) :-
    (fact(Company, Metric, Value) ; derived(Company, Metric, Value)), !.

% Store derived value (idempotent - updates if changed)
store_derived(Company, Metric, Value) :-
    % Don't override actual facts
    ( fact(Company, Metric, _) -> 
        true
    ; 
        % Check if derived value exists and needs updating
        ( derived(Company, Metric, OldValue) ->
            ( abs(OldValue - Value) > 0.0000001 ->  % Check if meaningfully different
                retract(derived(Company, Metric, OldValue)),
                assertz(derived(Company, Metric, Value))
            ; true  % Value unchanged, do nothing
            )
        ; 
            % No existing value, add it
            assertz(derived(Company, Metric, Value))
        )
    ).

% ========================================
% GENERIC SENSITIVITY ANALYSIS
% ========================================

% Run sensitivity analysis for any theorem - shows what each input needs to change
sensitivity_analysis(TheoremName, Company, TargetImprovement, Sensitivities) :-
    theorem(TheoremName, OutputMetric, InputMetrics, Formula),
    get_fact_value(Company, OutputMetric, CurrentOutput),
    TargetOutput is CurrentOutput * (1 + TargetImprovement),
    
    % Generate sensitivity for each input (all-else-equal)
    findall(
        sensitivity(Input, current(CurrentInput), required(RequiredInput), change_pct(ChangePct)),
        (
            member(Input, InputMetrics),
            get_fact_value(Company, Input, CurrentInput),
            solve_for_variable(Formula, Input, InputMetrics, Company, TargetOutput, RequiredInput),
            ChangePct is (RequiredInput / CurrentInput - 1) * 100
        ),
        InputSensitivities
    ),
    
    % Add balanced scenario based on formula type
    calculate_balanced_change(Formula, InputMetrics, Company, CurrentOutput, TargetOutput, BalancedChange),
    append(InputSensitivities, [BalancedChange], Sensitivities).

% Solve formula for a specific variable (environment-based algebraic solver)
solve_for_variable(Formula, TargetVar, AllVars, Company, TargetOutput, Solution) :-
    % Build environment from company facts
    get_input_values(Company, AllVars, AllValues),
    build_env(AllVars, AllValues, Env),
    
    % Solve based on formula type
    (
        % Multiplication: Y = A × B × C → A = Y / (B × C)
        Formula = multiply(Vars) ->
            member(TargetVar, Vars),
            select(TargetVar, Vars, RestVars),
            maplist(eval_expr_env(Env), RestVars, RestValues),
            product_list(RestValues, Product),
            Product =\= 0,
            Solution is TargetOutput / Product
        ;
        % Division: Y = A / B
        Formula = divide(Numerator, Denominator) ->
            (
                TargetVar = Numerator ->
                    eval_expr(Denominator, Env, DenomValue),
                    Solution is TargetOutput * DenomValue
                ;
                TargetVar = Denominator ->
                    eval_expr(Numerator, Env, NumValue),
                    TargetOutput =\= 0,
                    Solution is NumValue / TargetOutput
            )
        ;
        % Addition: Y = A + B
        Formula = add(Vars) ->
            member(TargetVar, Vars),
            select(TargetVar, Vars, RestVars),
            maplist(eval_expr_env(Env), RestVars, RestValues),
            sum_list(RestValues, Sum),
            Solution is TargetOutput - Sum
        ;
        % Subtraction: Y = A - B
        Formula = subtract(A, B) ->
            (
                TargetVar = A ->
                    eval_expr(B, Env, BValue),
                    Solution is TargetOutput + BValue
                ;
                TargetVar = B ->
                    eval_expr(A, Env, AValue),
                    Solution is AValue - TargetOutput
            )
        ;
        % Weighted sum: Y = w1×A + w2×B + ...
        Formula = weighted(Weights, Vars) ->
            nth0(Index, Vars, TargetVar),
            nth0(Index, Weights, Weight),
            Weight =\= 0,
            select(TargetVar, Vars, RestVars),
            select(Weight, Weights, RestWeights),
            maplist(eval_expr_env(Env), RestVars, RestValues),
            weighted_sum(RestWeights, RestValues, WeightedSum),
            Solution is (TargetOutput - WeightedSum) / Weight
        ;
        % Complex formulas - try to solve symbolically
        Formula = formula(Expr) ->
            solve_complex_formula(Expr, TargetVar, Env, TargetOutput, Solution)
    ).

% Solve complex formula expressions
solve_complex_formula(Expr, TargetVar, Env, TargetOutput, Solution) :-
    % For now, handle simple cases
    % TODO: Implement full symbolic solver
    ( Expr = (A + B - C) ->
        % If it's a simple linear combination, try to isolate TargetVar
        ( A = TargetVar -> 
            eval_expr(B, Env, VB),
            eval_expr(C, Env, VC),
            Solution is TargetOutput - VB + VC
        ; B = TargetVar ->
            eval_expr(A, Env, VA),
            eval_expr(C, Env, VC),
            Solution is TargetOutput - VA + VC
        ; C = TargetVar ->
            eval_expr(A, Env, VA),
            eval_expr(B, Env, VB),
            Solution is VA + VB - TargetOutput
        )
    ; % Default fallback
        Solution is TargetOutput
    ).

% Calculate balanced change (all inputs change proportionally)
calculate_balanced_change(Formula, InputMetrics, Company, CurrentOutput, TargetOutput, BalancedChange) :-
    ImprovementRatio is TargetOutput / CurrentOutput,
    (
        Formula = multiply(_) ->
            % For multiplicative formulas, each factor changes by nth root
            length(InputMetrics, N),
            FactorChange is ImprovementRatio ** (1/N),
            ChangePct is (FactorChange - 1) * 100,
            BalancedChange = balanced(change_pct(ChangePct), all_inputs)
        ;
        Formula = add(_) ->
            % For additive formulas, distribute change proportionally
            ChangePct is (ImprovementRatio - 1) * 100,
            BalancedChange = balanced(change_pct(ChangePct), proportional)
        ;
        % Default
        ChangePct is (ImprovementRatio - 1) * 100,
        BalancedChange = balanced(change_pct(ChangePct), estimated)
    ).

% Helper to get values of other metrics
get_other_values([], _, _, []).
get_other_values([Metric|Rest], TargetMetric, Company, Values) :-
    (Metric = TargetMetric -> 
        get_other_values(Rest, TargetMetric, Company, Values)
    ;
        get_fact_value(Company, Metric, Value),
        get_other_values(Rest, TargetMetric, Company, RestValues),
        Values = [Value|RestValues]
    ).

% Calculate product of list
product_list([], 1).
product_list([H|T], Product) :-
    product_list(T, RestProduct),
    Product is H * RestProduct.

% ========================================
% BATCH CALCULATION WITH CHAINING
% ========================================

% Apply all theorems with fixed-point iteration (chaining)
apply_all_theorems(Company) :-
    MaxIterations = 10,  % Prevent infinite loops
    apply_all_theorems_fixpoint(Company, 0, MaxIterations).

apply_all_theorems_fixpoint(Company, Iteration, MaxIterations) :-
    ( Iteration >= MaxIterations ->
        true  % Stop after max iterations
    ;
        apply_all_theorems_once(Company, NewlyDerived),
        ( NewlyDerived > 0 ->
            % Some theorems fired, try again for chaining
            NextIteration is Iteration + 1,
            apply_all_theorems_fixpoint(Company, NextIteration, MaxIterations)
        ;
            % No new theorems fired, we've reached fixed point
            true
        )
    ).

% Apply theorems once and count how many fired
apply_all_theorems_once(Company, Count) :-
    findall(
        TheoremName,
        (
            theorem(TheoremName, OutputMetric, InputMetrics, _),
            \+ get_fact_value(Company, OutputMetric, _),  % Not already calculated
            forall(member(Input, InputMetrics), get_fact_value(Company, Input, _))  % All inputs available
        ),
        ApplicableTheorems
    ),
    length(ApplicableTheorems, Count),
    maplist(calculate_theorem_wrapper(Company), ApplicableTheorems).

calculate_theorem_wrapper(Company, TheoremName) :-
    calculate_theorem(TheoremName, Company, _).

% ========================================
% SENSITIVITY ANALYSIS FOR ALL THEOREMS
% ========================================

% Run sensitivity analysis for all applicable theorems
run_all_sensitivities(Company, TargetImprovement, AllPaths) :-
    findall(
        sensitivity(TheoremName, Paths),
        (
            theorem(TheoremName, OutputMetric, _, _),
            get_fact_value(Company, OutputMetric, _),  % Can only reverse engineer if we have the output
            reverse_engineer_theorem(TheoremName, Company, TargetImprovement, Paths)
        ),
        AllPaths
    ).

% ========================================
% FORMULA EVALUATION (WITH NESTED EXPRESSION SUPPORT)
% ========================================

% Build environment from variable names and values
build_env([], [], []).
build_env([K|Ks], [V|Vs], [K-V|Env]) :- 
    build_env(Ks, Vs, Env).

% Lookup value in environment
lookup_env([K-V|_], K, V) :- !.
lookup_env([_|T], K, V) :- 
    lookup_env(T, K, V).

% Main evaluation function - now handles nested expressions
evaluate_formula(Formula, InputMetrics, Values, Result) :-
    build_env(InputMetrics, Values, Env),
    eval_expr(Formula, Env, Result).

% Evaluate any expression with environment
eval_expr(Expr, _, Val) :-
    number(Expr), !, Val = Expr.

eval_expr(Expr, Env, Val) :-
    atom(Expr), !, 
    lookup_env(Env, Expr, Val).

% DSL operations
eval_expr(multiply(Vars), Env, Val) :- !,
    maplist(eval_expr_env(Env), Vars, Vals),
    product_list(Vals, Val).

eval_expr(add(Vars), Env, Val) :- !,
    maplist(eval_expr_env(Env), Vars, Vals),
    sum_list(Vals, Val).

eval_expr(subtract(A, B), Env, Val) :- !,
    eval_expr(A, Env, VA),
    eval_expr(B, Env, VB),
    Val is VA - VB.

eval_expr(divide(A, B), Env, Val) :- !,
    eval_expr(A, Env, VA),
    eval_expr(B, Env, VB),
    VB =\= 0,  % Allow negative denominators
    Val is VA / VB.

eval_expr(weighted(Weights, Vars), Env, Val) :- !,
    maplist(eval_expr_env(Env), Vars, Vals),
    weighted_sum(Weights, Vals, Val).

% Handle formula(...) wrapper - just evaluate the inner expression
eval_expr(formula(Inner), Env, Val) :- !,
    eval_expr(Inner, Env, Val).

% Handle raw arithmetic operators: +, -, *, /
eval_expr(Expr, Env, Val) :-
    compound(Expr),
    Expr =.. [Op, A, B],
    member(Op, [+, -, *, /]), !,
    eval_expr(A, Env, VA),
    eval_expr(B, Env, VB),
    ( Op = (+) -> Val is VA + VB
    ; Op = (-) -> Val is VA - VB
    ; Op = (*) -> Val is VA * VB
    ; Op = (/) -> (VB =\= 0 -> Val is VA / VB ; fail)
    ).

% Mathematical functions for advanced theorems
eval_expr(ln(X), Env, Val) :- !,
    eval_expr(X, Env, VX),
    VX > 0,  % Guard: ln only defined for positive values
    Val is log(VX).

eval_expr(exp(X), Env, Val) :- !,
    eval_expr(X, Env, VX),
    Val is exp(VX).

eval_expr(sqrt(X), Env, Val) :- !,
    eval_expr(X, Env, VX),
    VX >= 0,  % Guard: sqrt only defined for non-negative values
    Val is sqrt(VX).

eval_expr(ncdf(X), Env, Val) :- !,
    eval_expr(X, Env, VX),
    approx_normal_cdf(VX, Val).

% Approximation of standard normal CDF using error function
% More accurate than simple approximations
approx_normal_cdf(X, P) :-
    % Using the relationship: Φ(x) = 0.5 * (1 + erf(x/√2))
    % For SWI-Prolog, we'll use a polynomial approximation
    ( X < -6 -> P = 0.0
    ; X > 6 -> P = 1.0  
    ; % Abramowitz and Stegun approximation
      T is 1.0 / (1.0 + 0.2316419 * abs(X)),
      T2 is T * T,
      T3 is T2 * T,
      T4 is T3 * T,
      T5 is T4 * T,
      D is 0.3989423 * exp(-0.5 * X * X),
      CDF_abs is 1.0 - D * T * (0.31938153 - T * (0.356563782 - T * (1.781477937 - T * (1.821255978 - T * 1.330274429)))),
      ( X >= 0 -> P = CDF_abs ; P is 1.0 - CDF_abs )
    ).

% Helper for maplist
eval_expr_env(Env, Expr, Val) :- 
    eval_expr(Expr, Env, Val).

% Legacy interface (for backward compatibility)
evaluate_formula(Formula, Values, Result) :-
    % Simple formulas without variable names
    ( Formula = multiply(Vars) ->
        length(Vars, N),
        length(Values, N),
        product_list(Values, Result)
    ; Formula = divide(_, _) ->
        Values = [Num, Denom],
        Denom =\= 0,
        Result is Num / Denom
    ; Formula = add(Vars) ->
        length(Vars, N),
        length(Values, N),
        sum_list(Values, Result)
    ; Formula = subtract(_, _) ->
        Values = [A, B],
        Result is A - B
    ; Formula = weighted(Weights, Vars) ->
        length(Vars, N),
        length(Values, N),
        weighted_sum(Weights, Values, Result)
    ; fail  % Force use of new evaluator for complex formulas
    ).

% Helper: Calculate weighted sum
weighted_sum([], [], 0).
weighted_sum([W|Ws], [V|Vs], Result) :-
    weighted_sum(Ws, Vs, RestSum),
    Result is W * V + RestSum.

% Helper: Get values for specific variables from list
get_values_for_vars([], _, _, []).
get_values_for_vars([Var|Vars], AllVars, AllValues, [Value|Values]) :-
    nth0(Index, AllVars, Var),
    nth0(Index, AllValues, Value),
    get_values_for_vars(Vars, AllVars, AllValues, Values).

% Helper: Get single variable value
get_var_value(Var, AllVars, AllValues, Value) :-
    nth0(Index, AllVars, Var),
    nth0(Index, AllValues, Value).

% Helper: Get other variable values (excluding target)
get_other_variable_values(AllVars, TargetVar, Company, OtherVars, OtherValues) :-
    select(TargetVar, AllVars, OtherVars),
    get_input_values(Company, OtherVars, OtherValues).

% Helper: Select with index for weighted formulas
select_with_index(Index, Vars, Weights, RestVars, RestWeights) :-
    nth0(Index, Vars, _, RestVars),
    nth0(Index, Weights, _, RestWeights).
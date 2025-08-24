%%% Bridge rules - simplified for essential theorems

% Bridge is not needed for simple facts
bridge_all_facts :- true.

%%% Calculation rules for derived metrics

calculate_dupont_inputs(Company, Period) :-
    best_fact(Company, revenue, Period, Rev, _, _, _),
    best_fact(Company, net_income, Period, NI, _, _, _),
    best_fact(Company, total_assets, Period, TA, _, _, _),
    best_fact(Company, equity_value, Period, EQ, _, _, _),
    NPM is NI / Rev,
    AT is Rev / TA,
    FL is TA / EQ,
    assertz(fact(Company, npm, NPM)),
    assertz(fact(Company, at, AT)),
    assertz(fact(Company, fl, FL)).

calculate_roic(Company, Period) :-
    best_fact(Company, ebit, Period, EBIT, _, _, _),
    best_fact(Company, tax_expense, Period, Tax, _, _, _),
    best_fact(Company, invested_capital, Period, IC, _, _, _),
    EBIT > 0, IC > 0,
    TaxRate is Tax / EBIT,
    NOPAT is EBIT * (1 - TaxRate),
    ROIC is NOPAT / IC,
    assertz(fact(Company, nopat, NOPAT)),
    assertz(fact(Company, roic, ROIC)).

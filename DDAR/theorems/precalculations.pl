%%% Pre-calculation Rules
%%% Automatically derive missing metrics before theorem execution

% ========================================
% MARGIN CALCULATIONS
% ========================================

% NPM (Net Profit Margin) = Net Income / Revenue
precalc_npm(Company) :-
    \+ fact(Company, npm, _),
    fact(Company, net_income, NI),
    fact(Company, revenue, Rev),
    Rev > 0,
    NPM is NI / Rev,
    assertz(fact(Company, npm, NPM)),
    format('Pre-calculated NPM for ~w: ~4f~n', [Company, NPM]).

% Gross Margin = Gross Profit / Revenue
precalc_gross_margin(Company) :-
    \+ fact(Company, gross_margin, _),
    fact(Company, gross_profit, GP),
    fact(Company, revenue, Rev),
    Rev > 0,
    GM is GP / Rev,
    assertz(fact(Company, gross_margin, GM)),
    format('Pre-calculated Gross Margin for ~w: ~4f~n', [Company, GM]).

% Operating Margin = Operating Income / Revenue
precalc_operating_margin(Company) :-
    \+ fact(Company, operating_margin, _),
    fact(Company, operating_income, OpInc),
    fact(Company, revenue, Rev),
    Rev > 0,
    OpMargin is OpInc / Rev,
    assertz(fact(Company, operating_margin, OpMargin)),
    format('Pre-calculated Operating Margin for ~w: ~4f~n', [Company, OpMargin]).

% EBITDA Margin = EBITDA / Revenue
precalc_ebitda_margin(Company) :-
    \+ fact(Company, ebitda_margin, _),
    fact(Company, ebitda, EBITDA),
    fact(Company, revenue, Rev),
    Rev > 0,
    Margin is EBITDA / Rev,
    assertz(fact(Company, ebitda_margin, Margin)),
    format('Pre-calculated EBITDA Margin for ~w: ~4f~n', [Company, Margin]).

% ========================================
% DUPONT COMPONENTS
% ========================================

% AT (Asset Turnover) = Revenue / Total Assets
precalc_at(Company) :-
    \+ fact(Company, at, _),
    fact(Company, revenue, Rev),
    fact(Company, total_assets, TA),
    TA > 0,
    AT is Rev / TA,
    assertz(fact(Company, at, AT)),
    format('Pre-calculated AT for ~w: ~4f~n', [Company, AT]).

% FL (Financial Leverage) = Total Assets / Total Equity
precalc_fl(Company) :-
    \+ fact(Company, fl, _),
    fact(Company, total_assets, TA),
    fact(Company, total_equity, EQ),
    EQ > 0,
    FL is TA / EQ,
    assertz(fact(Company, fl, FL)),
    format('Pre-calculated FL for ~w: ~4f~n', [Company, FL]).

% Alternative: Asset Turnover from fact name variations
precalc_asset_turnover(Company) :-
    \+ fact(Company, asset_turnover, _),
    fact(Company, at, AT),
    assertz(fact(Company, asset_turnover, AT)).

% ========================================
% RETURN METRICS
% ========================================

% ROA (Return on Assets) = Net Income / Total Assets
precalc_roa(Company) :-
    \+ fact(Company, roa, _),
    fact(Company, net_income, NI),
    fact(Company, total_assets, TA),
    TA > 0,
    ROA is NI / TA,
    assertz(fact(Company, roa, ROA)),
    format('Pre-calculated ROA for ~w: ~4f~n', [Company, ROA]).

% ROE (Return on Equity) = Net Income / Total Equity
precalc_roe(Company) :-
    \+ fact(Company, roe, _),
    fact(Company, net_income, NI),
    fact(Company, total_equity, EQ),
    EQ > 0,
    ROE is NI / EQ,
    assertz(fact(Company, roe, ROE)),
    format('Pre-calculated ROE for ~w: ~4f~n', [Company, ROE]).

% ========================================
% ROIC COMPONENTS
% ========================================

% NOPAT = Operating Income × (1 - Tax Rate)
precalc_nopat(Company) :-
    \+ fact(Company, nopat, _),
    fact(Company, operating_income, OpInc),
    fact(Company, tax_provision, Tax),
    fact(Company, operating_income, OpInc2),
    OpInc2 > 0,
    TaxRate is Tax / OpInc2,
    TaxRate >= 0, TaxRate < 1,  % Sanity check
    NOPAT is OpInc * (1 - TaxRate),
    assertz(fact(Company, nopat, NOPAT)),
    format('Pre-calculated NOPAT for ~w: ~4f~n', [Company, NOPAT]).

% Alternative NOPAT from EBIT
precalc_nopat_from_ebit(Company) :-
    \+ fact(Company, nopat, _),
    fact(Company, ebit, EBIT),
    fact(Company, tax_rate, TaxRate),
    NOPAT is EBIT * (1 - TaxRate),
    assertz(fact(Company, nopat, NOPAT)),
    format('Pre-calculated NOPAT from EBIT for ~w: ~4f~n', [Company, NOPAT]).

% Invested Capital (simplified) = Total Assets - Non-interest bearing Current Liabilities
precalc_invested_capital(Company) :-
    \+ fact(Company, invested_capital, _),
    fact(Company, total_assets, TA),
    fact(Company, current_liabilities, CL),
    % Rough approximation: 70% of CL is non-interest bearing
    IC is TA - (CL * 0.7),
    assertz(fact(Company, invested_capital, IC)),
    format('Pre-calculated Invested Capital for ~w: ~4f~n', [Company, IC]).

% Capital Turnover = Revenue / Invested Capital
precalc_capital_turnover(Company) :-
    \+ fact(Company, capital_turnover, _),
    fact(Company, revenue, Rev),
    fact(Company, invested_capital, IC),
    IC > 0,
    CT is Rev / IC,
    assertz(fact(Company, capital_turnover, CT)),
    format('Pre-calculated Capital Turnover for ~w: ~4f~n', [Company, CT]).

% ========================================
% LIQUIDITY RATIOS
% ========================================

% Current Ratio = Current Assets / Current Liabilities
precalc_current_ratio(Company) :-
    \+ fact(Company, current_ratio, _),
    fact(Company, current_assets, CA),
    fact(Company, current_liabilities, CL),
    CL > 0,
    CR is CA / CL,
    assertz(fact(Company, current_ratio, CR)),
    format('Pre-calculated Current Ratio for ~w: ~4f~n', [Company, CR]).

% Quick Ratio = (Current Assets - Inventory) / Current Liabilities
precalc_quick_ratio(Company) :-
    \+ fact(Company, quick_ratio, _),
    fact(Company, current_assets, CA),
    fact(Company, inventory, Inv),
    fact(Company, current_liabilities, CL),
    CL > 0,
    QR is (CA - Inv) / CL,
    assertz(fact(Company, quick_ratio, QR)),
    format('Pre-calculated Quick Ratio for ~w: ~4f~n', [Company, QR]).

% Cash Ratio = Cash / Current Liabilities
precalc_cash_ratio(Company) :-
    \+ fact(Company, cash_ratio, _),
    fact(Company, cash, Cash),
    fact(Company, current_liabilities, CL),
    CL > 0,
    CashR is Cash / CL,
    assertz(fact(Company, cash_ratio, CashR)),
    format('Pre-calculated Cash Ratio for ~w: ~4f~n', [Company, CashR]).

% ========================================
% LEVERAGE RATIOS
% ========================================

% Debt to Equity = Total Debt / Total Equity
precalc_debt_to_equity(Company) :-
    \+ fact(Company, debt_to_equity, _),
    fact(Company, total_debt, TD),
    fact(Company, total_equity, EQ),
    EQ > 0,
    DE is TD / EQ,
    assertz(fact(Company, debt_to_equity, DE)),
    format('Pre-calculated Debt to Equity for ~w: ~4f~n', [Company, DE]).

% Debt to Assets = Total Debt / Total Assets
precalc_debt_to_assets(Company) :-
    \+ fact(Company, debt_to_assets, _),
    fact(Company, total_debt, TD),
    fact(Company, total_assets, TA),
    TA > 0,
    DA is TD / TA,
    assertz(fact(Company, debt_to_assets, DA)),
    format('Pre-calculated Debt to Assets for ~w: ~4f~n', [Company, DA]).

% Interest Coverage = Operating Income / Interest Expense
precalc_interest_coverage(Company) :-
    \+ fact(Company, interest_coverage, _),
    fact(Company, operating_income, OpInc),
    fact(Company, interest_expense, IntExp),
    IntExp > 0,
    IC is OpInc / IntExp,
    assertz(fact(Company, interest_coverage, IC)),
    format('Pre-calculated Interest Coverage for ~w: ~4f~n', [Company, IC]).

% ========================================
% MASTER PRE-CALCULATION RUNNER
% ========================================

run_all_precalculations(Company) :-
    % Margins
    (precalc_npm(Company) ; true),
    (precalc_gross_margin(Company) ; true),
    (precalc_operating_margin(Company) ; true),
    (precalc_ebitda_margin(Company) ; true),
    
    % DuPont components
    (precalc_at(Company) ; true),
    (precalc_fl(Company) ; true),
    (precalc_asset_turnover(Company) ; true),
    
    % Returns
    (precalc_roa(Company) ; true),
    (precalc_roe(Company) ; true),
    
    % ROIC components
    (precalc_nopat(Company) ; true),
    (precalc_nopat_from_ebit(Company) ; true),
    (precalc_invested_capital(Company) ; true),
    (precalc_capital_turnover(Company) ; true),
    
    % Liquidity
    (precalc_current_ratio(Company) ; true),
    (precalc_quick_ratio(Company) ; true),
    (precalc_cash_ratio(Company) ; true),
    
    % Leverage
    (precalc_debt_to_equity(Company) ; true),
    (precalc_debt_to_assets(Company) ; true),
    (precalc_interest_coverage(Company) ; true).
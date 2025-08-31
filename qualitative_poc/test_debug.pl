:- consult('generic_reasoner.pl').

test_contradiction :-
    % Load dimension metadata
    assertz(property_dimension(strong_cash_flow, financial, positive)),
    assertz(property_dimension(weak_cash_flow, financial, negative)),
    
    % Add the facts
    assertz(has_property_full(test_co, strong_cash_flow, 0.69, 1)),
    assertz(has_property_full(test_co, weak_cash_flow, 0.58, 1)),
    assertz(has_property(test_co, strong_cash_flow)),
    assertz(has_property(test_co, weak_cash_flow)),
    
    % Test contradiction detection
    format('Testing contradiction between strong_cash_flow and weak_cash_flow~n'),
    (is_dimension_contradiction(test_co, strong_cash_flow, weak_cash_flow) ->
        format('  ✓ Contradiction detected!~n')
    ;
        format('  ✗ No contradiction detected~n')
    ),
    
    % Check if find_contradiction detects it
    format('~nChecking find_contradiction_with_resolution:~n'),
    (find_contradiction_with_resolution(test_co, Result, Resolution, Facts) ->
        format('  Found contradiction: ~w~n', [Result]),
        format('  Resolution: ~w~n', [Resolution]),
        format('  Facts: ~w~n', [Facts])
    ;
        format('  No contradictions found~n')
    ).

:- test_contradiction, halt.
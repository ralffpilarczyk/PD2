:- consult('generic_reasoner.pl').

test :-
    reset_analysis,
    
    % Load dimension metadata
    assertz(property_dimension(strong_cash_flow, financial, positive)),
    assertz(property_dimension(weak_cash_flow, financial, negative)),
    assertz(property_dimension(weak_position, position, negative)),
    assertz(property_dimension(economies_of_scale, scale, positive)),
    
    % Add observed facts
    assertz(has_property_full(test_co, weak_position, 0.85, 2)),
    assertz(has_property_full(test_co, economies_of_scale, 0.80, 1)),
    assertz(has_property(test_co, weak_position)),
    assertz(has_property(test_co, economies_of_scale)),
    
    % Check if strong_cash_flow and weak_cash_flow would both be derived
    format('Testing derivation of conflicting cash flows...~n'),
    
    % Simulate the derivation
    assertz(has_property(test_co, strong_cash_flow)),
    assertz(has_property(test_co, weak_cash_flow)),
    
    % Now check contradiction detection
    format('~nChecking if contradiction is detected:~n'),
    (setof([P1, P2, Resolution], 
           find_contradiction_with_resolution(test_co, P1, P2, Resolution),
           Contradictions) ->
        forall(member([Prop1, Prop2, Res], Contradictions),
               format('  [!] ~w vs ~w (~w)~n', [Prop1, Prop2, Res]))
    ;
        format('  (none)~n')
    ).

:- test, halt.
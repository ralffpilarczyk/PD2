% Minimal reasoning engine - gate-only derivation with event logging
% Single fact representation, no post-hoc conflict scanning

% Ensure time-limited calls are available for chain printing, if present
:- if(exists_source(library(timeout))).
:- use_module(library(timeout)).
:- endif.

:- dynamic fact/4.           % entity, property, source, confidence
:- dynamic rule/3.           % antecedent, consequent, confidence  
:- dynamic accept/4.         % entity, antecedent, consequent, confidence (event log)
:- dynamic reject/4.         % entity, antecedent, consequent, reason (event log)
:- dynamic edge/3.           % entity, antecedent, consequent (for chain building)
:- dynamic config_setting/1. % min_confidence, max_depth

% Load a causal rule
load_rule(Antecedent, Consequent, Confidence) :-
    assertz(rule(Antecedent, Consequent, Confidence)).

% Set configuration
set_config(Setting) :-
    assertz(config_setting(Setting)).

% Get min confidence threshold
get_min_confidence(MinConf) :-
    config_setting(min_confidence(MinConf)), !.
get_min_confidence(0.5).  % Default

% Get max depth
get_max_depth(MaxDepth) :-
    config_setting(max_depth(MaxDepth)), !.
get_max_depth(5).  % Default

% Check if property has explicit negation
is_negation(Prop1, Prop2) :-
    atom(Prop1),
    atom_concat('no_', Base, Prop1),
    Prop2 = Base.
is_negation(Prop1, Prop2) :-
    atom(Prop1),
    \+ atom_concat('no_', _, Prop1),  % Prop1 is positive
    atom_concat('no_', Prop1, Prop2).  % Generate negative form
is_negation(Prop1, Prop2) :-
    atom(Prop2),
    atom_concat('no_', Base, Prop2),
    Prop1 = Base.

% Gate check: can we assert this derived fact?
can_assert_fact(Entity, Property, Source, Confidence) :-
    % Check 1: No duplicate
    \+ fact(Entity, Property, _, _),
    % Check 2: Confidence threshold
    get_min_confidence(MinConf),
    Confidence >= MinConf,
    % Check 3: No negation conflict
    \+ has_negation_conflict(Entity, Property, Source).

% Get rejection reason for a fact
get_rejection_reason(Entity, Property, Source, Confidence, Reason) :-
    (fact(Entity, Property, _, _) ->
        Reason = 'duplicate_property'
    ; get_min_confidence(MinConf), Confidence < MinConf ->
        Reason = 'below_min_confidence'
    ; has_negation_conflict(Entity, Property, Source) ->
        get_negation_conflict_reason(Entity, Property, Source, Reason)
    ;
        Reason = 'unknown'
    ).

% Check if there's a negation conflict
has_negation_conflict(Entity, Property, Source) :-
    is_negation(Property, NegProp),
    fact(Entity, NegProp, NegSource, _),
    should_block(NegProp, NegSource, Property, Source), !.

% Determine if negation should block
should_block(NegProp, _, _, _) :-
    atom_concat('no_', _, NegProp), !.  % Negative always blocks positive
should_block(_, observed, _, derived).  % Observed blocks derived

% Get specific negation conflict reason
get_negation_conflict_reason(Entity, Property, Source, Reason) :-
    is_negation(Property, NegProp),
    fact(Entity, NegProp, NegSource, _),
    (atom_concat('no_', _, NegProp) ->
        Reason = 'negation_exists'
    ; NegSource = observed, Source = derived ->
        Reason = 'observed_overrides'
    ;
        Reason = 'negation_conflict'
    ).
get_negation_conflict_reason(_, _, _, 'no_conflict').

% Multi-step derivation with bounded fixpoint
derive_all(Entity) :-
    get_max_depth(MaxDepth),
    derive_fixpoint(Entity, 0, MaxDepth).

% Fixpoint loop - derive until no new facts or max depth
derive_fixpoint(Entity, Depth, MaxDepth) :-
    Depth < MaxDepth,
    % Try to derive new facts at this depth
    derive_at_depth(Entity, Depth, NewFactsAdded),
    (NewFactsAdded = true ->
        % New facts were added, continue
        NextDepth is Depth + 1,
        derive_fixpoint(Entity, NextDepth, MaxDepth)
    ;
        % No new facts, fixpoint reached
        true
    ).
derive_fixpoint(_, Depth, MaxDepth) :-
    Depth >= MaxDepth.  % Max depth reached

% Derive facts at specific depth
derive_at_depth(Entity, _, NewFactsAdded) :-
    % Collect all valid derivations at this depth
    findall(Added, 
            (rule(Antecedent, Consequent, RuleConf),
             fact(Entity, Antecedent, _, _),
             \+ fact(Entity, Consequent, _, _),  % Not already derived
             % Try to assert with gate check
             (can_assert_fact(Entity, Consequent, derived, RuleConf) ->
                 % Gate passed - assert and log acceptance
                 assertz(fact(Entity, Consequent, derived, RuleConf)),
                 assertz(accept(Entity, Antecedent, Consequent, RuleConf)),
                 assertz(edge(Entity, Antecedent, Consequent)),
                 Added = true
             ;
                 % Gate failed - log rejection with reason
                 get_rejection_reason(Entity, Consequent, derived, RuleConf, RejectReason),
                 assertz(reject(Entity, Antecedent, Consequent, RejectReason)),
                 Added = false
             )),
            Results),
    % Check if any new facts were added
    (member(true, Results) -> NewFactsAdded = true ; NewFactsAdded = false).

% Build chains from accepted edges
find_chain(Entity, Chain) :-
    fact(Entity, Start, observed, _),
    \+ edge(Entity, _, Start),  % Start is not a consequent
    build_chain(Entity, Start, [Start], RevChain),
    length(RevChain, L), L > 2,  % At least 3 elements
    reverse(RevChain, Chain).  % Reverse to get causal order

build_chain(Entity, Current, Visited, Chain) :-
    edge(Entity, Current, Next),
    \+ member(Next, Visited),
    build_chain(Entity, Next, [Next|Visited], ExtendedChain),
    Chain = ExtendedChain.
build_chain(_, _, Chain, Chain).

% Format chain for display
format_chain([], '').
format_chain([X], X) :- !.
format_chain([H|T], Result) :-
    format_chain(T, RestResult),
    (RestResult = '' ->
        Result = H
    ;
        format(atom(Result), '~w → ~w', [H, RestResult])
    ).

% Main analysis entry point
analyze(Entity) :-
    format('~n=== ANALYZING ~w ===~n', [Entity]),
    
    % Run multi-step derivation
    write('DEBUG: Starting derivation...'), nl,
    derive_all(Entity),
    write('DEBUG: Derivation complete'), nl,
    
    % 1. Observed facts
    write('DEBUG: Printing observed...'), nl,
    format('~nObserved facts:~n'),
    findall([P, C], fact(Entity, P, observed, C), ObservedList),
    sort(ObservedList, SortedObserved),
    forall(member([Prop, Conf], SortedObserved),
           format('  - ~w (conf: ~2f)~n', [Prop, Conf])),
    length(SortedObserved, ObservedCount),
    
    % 2. Derived facts (kept)
    write('DEBUG: Printing derived...'), nl,
    format('~nDerived facts (kept):~n'),
    findall([P, C, A], (accept(Entity, A, P, C), fact(Entity, P, derived, C)), DerivedList),
    sort(DerivedList, SortedDerived),
    forall(member([Prop, Conf, Ant], SortedDerived),
           format('  - ~w (conf: ~2f, from ~w)~n', [Prop, Conf, Ant])),
    length(SortedDerived, DerivedCount),
    
    % 3. Rejected derivations
    write('DEBUG: Printing rejected...'), nl,
    format('~nRejected derivations:~n'),
    findall([C, R], reject(Entity, _, C, R), RejectList),
    sort(RejectList, TempRejected),
    % Deduplicate by consequent (show each rejected property once with first reason)
    deduplicate_rejects(TempRejected, SortedRejected),
    forall(member([Cons, Reason], SortedRejected),
           format('  - ~w (reason: ~w)~n', [Cons, Reason])),
    length(SortedRejected, RejectedCount),
    
    % 4. Reasoning chains (skip for documents with many facts/edges)
    write('DEBUG: Starting chains section...'), nl,
    format('~nReasoning chains:~n'),
    % Count observed facts and edges
    findall(1, fact(Entity, _, observed, _), ObsList),
    length(ObsList, ObsCount),
    findall(1, edge(Entity, _, _), EdgeList),
    length(EdgeList, EdgeCount),
    
    % Skip chains if too many facts OR edges
    (ObsCount > 5 ->
        format('  (chains omitted - too many facts: ~w)~n', [ObsCount])
    ; EdgeCount > 15 ->
        format('  (chains omitted - graph too large: ~w edges)~n', [EdgeCount])
    ;
        % Try to find chains with very short time limit
        ( current_predicate(call_with_time_limit/2) ->
            catch(
                call_with_time_limit(1,
                    (% Simply show first few edges as chains
                     findall(Start-End,
                             (edge(Entity, Start, End),
                              fact(Entity, Start, observed, _)),
                             DirectEdges),
                     sort(DirectEdges, SortedEdges),
                     % Show up to 5 direct edges
                     take_first_n(SortedEdges, 5, TopEdges),
                     forall(member(S-E, TopEdges),
                            format('  - ~w → ~w~n', [S, E]))
                    )
                ),
                time_limit_exceeded,
                format('  (chains timed out)~n')
            )
          ;
            format('  (chains omitted - no time limit support)~n')
        )
    ),
    
    % 5. Summary
    format('~n--- Summary ---~n'),
    format('Observed: ~w~n', [ObservedCount]),
    format('Derived: ~w~n', [DerivedCount]),
    format('Rejected: ~w~n', [RejectedCount]),
    
    % Calculate max depth actually used
    findall(D, (edge(Entity, A, B), edge(Entity, B, C), 
                edge(Entity, C, D)), Depth3),
    (Depth3 \= [] -> MaxDepthUsed = 3
    ; (edge(Entity, A, B), edge(Entity, B, _)) -> MaxDepthUsed = 2
    ; edge(Entity, _, _) -> MaxDepthUsed = 1
    ; MaxDepthUsed = 0),
    format('Max depth used: ~w~n', [MaxDepthUsed]),
    
    % Compute metrics from event log only
    % Attempts = all derivation attempts (accept + reject)
    findall(1, accept(Entity, _, _, _), AcceptList),
    findall(1, reject(Entity, _, _, _), RejectList),
    length(AcceptList, Applied),
    length(RejectList, Rejected),
    Attempts is Applied + Rejected,
    
    format('~nRule metrics: ~w attempts, ~w applied, ~w rejected~n', 
           [Attempts, Applied, Rejected]),
    
    % Integrity validation
    (Applied > Attempts ->
        format('WARNING: Integrity violation - applied > attempts!~n')
    ; Rejected < 0 ->
        format('WARNING: Integrity violation - negative rejected count!~n')
    ; true),
    !.  % Cut to ensure success

% Take first N elements from list
take_first_n(_, 0, []) :- !.
take_first_n([], _, []).
take_first_n([H|T], N, [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take_first_n(T, N1, Rest).

% Deduplicate rejects - keep first reason for each consequent
deduplicate_rejects([], []).
deduplicate_rejects([[C, R]|Rest], [[C, R]|Deduped]) :-
    remove_consequent(C, Rest, Filtered),
    deduplicate_rejects(Filtered, Deduped).

remove_consequent(_, [], []).
remove_consequent(C, [[C, _]|Rest], Filtered) :-
    remove_consequent(C, Rest, Filtered).
remove_consequent(C, [[C2, R2]|Rest], [[C2, R2]|Filtered]) :-
    C \= C2,
    remove_consequent(C, Rest, Filtered).


% Reset for fresh analysis
reset_analysis :-
    retractall(fact(_, _, _, _)),
    retractall(rule(_, _, _)),
    retractall(accept(_, _, _, _)),
    retractall(reject(_, _, _, _)),
    retractall(edge(_, _, _)),
    retractall(config_setting(_)).
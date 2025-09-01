% Generic reasoning engine - configuration driven
% No hardcoded rules or properties

:- dynamic has_property/2.
:- dynamic has_property_full/4.  % entity, property, confidence, priority
:- dynamic causal_rule/3.
:- dynamic causal_rule_full/5.  % antecedent, consequent, confidence, blockers, rationale
:- dynamic expected_pattern/4.
:- dynamic property_negates/2.
:- dynamic property_dimension/3.  % property, dimension, polarity
:- dynamic blocking_rule/4.  % blocker, blocks_list, strength, rationale
:- dynamic blocked_fact/2.
:- dynamic cascade_blocked/3.
:- dynamic config_setting/1.
:- dynamic block_source/4.  % entity, blocked, blocker, principle
:- dynamic contradiction_clusters/3.  % entity, count, clusters

% Load rules from configuration (called by Python)
load_causal_rule(Antecedent, Consequent, Confidence) :-
    assertz(causal_rule(Antecedent, Consequent, Confidence)).

load_causal_rule_full(Antecedent, Consequent, Confidence, Blockers, Rationale) :-
    assertz(causal_rule_full(Antecedent, Consequent, Confidence, Blockers, Rationale)),
    % Also create simple form for backward compatibility
    assertz(causal_rule(Antecedent, Consequent, Confidence)).

load_expected_pattern(Prop1, Prop2, Confidence, Implication) :-
    assertz(expected_pattern(Prop1, Prop2, Confidence, Implication)).

load_negation_pair(Prop1, Prop2) :-
    assertz(property_negates(Prop1, Prop2)),
    assertz(property_negates(Prop2, Prop1)).

load_blocking_rule(Blocker, BlocksList, Strength, Rationale) :-
    assertz(blocking_rule(Blocker, BlocksList, Strength, Rationale)).

set_config(Setting) :-
    assertz(config_setting(Setting)).

% Generic contradiction detection - X vs no_X pattern
is_negation(Prop1, Prop2) :-
    property_negates(Prop1, Prop2).
is_negation(Prop1, Prop2) :-
    atom_concat('no_', Base, Prop1),
    Prop2 = Base.
is_negation(Prop1, Prop2) :-
    atom_concat('no_', Base, Prop2),
    Prop1 = Base.

% Check if properties are in same dimension with opposite polarity
is_dimension_contradiction(_, Prop1, Prop2) :-
    property_dimension(Prop1, Dim1, Pol1),
    property_dimension(Prop2, Dim2, Pol2),
    Dim1 = Dim2,
    opposite_polarity(Pol1, Pol2).

opposite_polarity(positive, negative).
opposite_polarity(negative, positive).

% Get property dimension and polarity (from config or patterns)
property_dimension(market_leader, position, positive).
property_dimension(weak_position, position, negative).
property_dimension(pricing_power, pricing, positive).
property_dimension(no_pricing_power, pricing, negative).
property_dimension(strong_brand, brand, positive).
property_dimension(weak_brand, brand, negative).
property_dimension(economies_of_scale, scale, positive).
property_dimension(no_economies_of_scale, scale, negative).

% Check if a property is overridden by its negation
is_overridden(Entity, Property) :-
    has_property(Entity, Property),
    has_property(Entity, NegProperty),
    is_negation(Property, NegProperty),
    atom_concat('no_', _, NegProperty).  % Negative wins

% Check if property is blocked by blocking rules
is_blocked_by_rule(Entity, Property) :-
    has_property(Entity, Blocker),
    blocking_rule(Blocker, BlocksList, _, _),
    member(Property, BlocksList).

% Check if causal rule should apply (checking blockers)
rule_applies(Entity, Antecedent, Consequent) :-
    has_property(Entity, Antecedent),
    causal_rule_full(Antecedent, Consequent, Confidence, Blockers, _),
    get_min_confidence(MinConf),
    Confidence >= MinConf,
    \+ is_overridden(Entity, Antecedent),
    % Check none of the blockers are present
    \+ (member(Blocker, Blockers), has_property(Entity, Blocker)).

% Get minimum confidence setting
get_min_confidence(MinConf) :-
    config_setting(min_confidence(MinConf)), !.
get_min_confidence(0.5).  % Default

% Check if cascade blocking is enabled
cascade_enabled :-
    config_setting(cascade_blocking(true)), !.
cascade_enabled.  % Default true

% Generic causal derivation with conflict and cascade checking
derive_consequence(Entity, Property, Reason) :-
    % Collect all possible derivations
    findall(derived(P, R), derive_single(Entity, P, R), AllPossible),
    AllPossible \= [],
    % Resolve conflicts and cascade
    resolve_with_cascade(Entity, AllPossible, Resolved),
    % Return resolved facts
    member(derived(Property, Reason), Resolved).

% Collect ALL possible derivations at any depth
derive_single(Entity, Property, Reason) :-
    derive_all_depths(Entity, [], AllDerived),
    (member(derived(Property, Reason, _), AllDerived) ; member(derived(Property, Reason), AllDerived)).

% Calculate confidence with depth decay
calculate_derived_confidence(BaseConfidence, Depth, FinalConfidence) :-
    (config_setting(confidence_decay(Decay)) -> true ; Decay = 0.9),
    Power is Decay ^ Depth,
    FinalConfidence is BaseConfidence * Power.

% Iteratively derive facts at all depths with confidence
derive_all_depths(Entity, Seen, AllDerived) :-
    derive_all_depths_with_conf(Entity, Seen, 0, AllDerived).

derive_all_depths_with_conf(Entity, Seen, Depth, AllDerived) :-
    % Get current facts (observed + already derived)
    findall(F, has_property(Entity, F), ObservedFacts),
    findall(F, member(derived(F, _, _), Seen), DerivedFacts),
    append(ObservedFacts, DerivedFacts, CurrentFacts),
    
    % Find new derivations from current facts with confidence
    findall(derived(P, because(Source), Conf),
            (member(Source, CurrentFacts),
             \+ is_overridden(Entity, Source),
             causal_rule(Source, P, RuleConf),
             \+ member(P, CurrentFacts),
             \+ member(derived(P, _, _), Seen),
             calculate_derived_confidence(RuleConf, Depth, Conf),
             % Check minimum confidence threshold after decay
             (config_setting(min_confidence(MinConf)) -> Conf >= MinConf ; Conf >= 0.5),
             % Check blockers if they exist
             (\+ causal_rule_full(Source, P, _, Blockers, _) ; 
              (causal_rule_full(Source, P, _, Blockers, _),
               \+ (member(Blocker, Blockers), member(Blocker, CurrentFacts))))),
            NewDerived),
    
    % If no new derivations, we're done
    (NewDerived = [] ->
        AllDerived = Seen
    ;
        % Otherwise, add new and continue with increased depth
        append(Seen, NewDerived, NewSeen),
        NextDepth is Depth + 1,
        derive_all_depths_with_conf(Entity, NewSeen, NextDepth, AllDerived)
    ).

% Resolve conflicts with cascade removal
resolve_with_cascade(Entity, Possible, Resolved) :-
    % Identify direct negation conflicts within derived facts
    findall([Pos, Neg, 'negation'], 
            ((member(derived(Pos, _, _), Possible) ; member(derived(Pos, _), Possible)),
             (member(derived(Neg, _, _), Possible) ; member(derived(Neg, _), Possible)),
             is_negation(Pos, Neg),
             atom_concat('no_', _, Neg)),  % Neg is the negative one
            NegationConflicts),
    
    % Check dimension-based contradictions within derived facts
    findall([DerivedPos, DerivedNeg, 'dimension'],
            ((member(derived(DerivedPos, _, _), Possible) ; member(derived(DerivedPos, _), Possible)),
             (member(derived(DerivedNeg, _, _), Possible) ; member(derived(DerivedNeg, _), Possible)),
             DerivedPos \= DerivedNeg,
             is_dimension_contradiction(_, DerivedPos, DerivedNeg),
             property_dimension(DerivedPos, _, positive),
             property_dimension(DerivedNeg, _, negative)),
            DerivedDimensionConflicts),
    
    % Check for dimension-based contradictions between observed and derived
    findall([DerivedPos, ObservedNeg, 'observed-derived'],
            ((member(derived(DerivedPos, _, _), Possible) ; member(derived(DerivedPos, _), Possible)),
             has_property(Entity, ObservedNeg),
             is_dimension_contradiction(_, DerivedPos, ObservedNeg),
             property_dimension(DerivedPos, _, positive),
             property_dimension(ObservedNeg, _, negative)),
            ObservedDerivedConflicts),
    
    % Combine all conflicts
    append(NegationConflicts, DerivedDimensionConflicts, Temp),
    append(Temp, ObservedDerivedConflicts, DirectConflicts),
    
    % Record contradiction clusters for reporting
    length(DirectConflicts, NumClusters),
    (NumClusters > 0 -> assertz(contradiction_clusters(Entity, NumClusters, DirectConflicts)) ; true),
    
    % Track blockers properly: map blocked -> blocker with principle
    findall(_, 
            (member([Blocked, Blocker, Type], DirectConflicts),
             (Type = 'observed-derived' -> Principle = 'observed beats derived'
             ; Type = 'negation' -> Principle = 'negative beats positive'
             ; Type = 'dimension' -> Principle = 'negative beats positive'),
             (\+ block_source(Entity, Blocked, Blocker, Principle) ->
                 assertz(block_source(Entity, Blocked, Blocker, Principle))
             ; true)),
            _),
    
    % Mark blocked facts (positives that lost)
    findall(Blocked, member([Blocked, _, _], DirectConflicts), BlockedFacts),
    
    % Find all facts that depend on blocked facts (cascade)
    find_cascade_blocks(Possible, BlockedFacts, AllBlocked),
    
    % Record what was blocked without duplicates
    sort(BlockedFacts, UniqueBlocked),
    forall(member(B, UniqueBlocked), 
           (\+ blocked_fact(Entity, B) -> assertz(blocked_fact(Entity, B)) ; true)),
    forall((member(C, AllBlocked), \+ member(C, UniqueBlocked)), 
           (\+ cascade_blocked(Entity, C, _) -> assertz(cascade_blocked(Entity, C, UniqueBlocked)) ; true)),
    
    % Keep only facts not in AllBlocked (handle both forms)
    findall(Fact,
            ((member(derived(P, R, C), Possible), Fact = derived(P, R, C);
              member(derived(P, R), Possible), Fact = derived(P, R)),
             \+ member(P, AllBlocked)),
            Resolved).

% Find all facts that depend on blocked facts
find_cascade_blocks(Possible, BlockedFacts, AllBlocked) :-
    find_cascade_blocks_iter(Possible, BlockedFacts, BlockedFacts, AllBlocked).

find_cascade_blocks_iter(Possible, Current, Seen, AllBlocked) :-
    % Find facts that depend on current blocked facts (handle both forms)
    findall(P, 
            ((member(derived(P, because(Source), _), Possible);
              member(derived(P, because(Source)), Possible)),
             member(Source, Current),
             \+ member(P, Seen)),
            NewBlocked),
    (NewBlocked = [] ->
        AllBlocked = Seen
    ;
        append(Seen, NewBlocked, NewSeen),
        find_cascade_blocks_iter(Possible, NewBlocked, NewSeen, AllBlocked)
    ).

% Find blocked derivations
find_blocked_derivations(Entity, BaseProperty, _, Reason) :-
    has_property(Entity, BaseProperty),
    is_overridden(Entity, BaseProperty),
    has_property(Entity, NegProperty),
    is_negation(BaseProperty, NegProperty),
    causal_rule(BaseProperty, Property, _),
    format(atom(Reason), 'Would derive ~w from ~w, but ~w is overridden by ~w',
           [Property, BaseProperty, BaseProperty, NegProperty]).

% Generic unusual pattern detection
find_unusual_pattern(Entity, Prop1, Prop2, Explanation) :-
    has_property(Entity, Prop1),
    expected_pattern(Prop1, Prop2, Confidence, Implication),
    Confidence > 0.6,
    \+ has_property(Entity, Prop2),
    format(atom(Explanation), '~w without ~w - ~w', [Prop1, Prop2, Implication]).

% Find reasoning chains (only valid ones, not through blocked facts)
derive_chain(Entity, Target, Chain) :-
    has_property(Entity, Base),
    \+ is_overridden(Entity, Base),
    \+ blocked_fact(Entity, Base),
    find_valid_path(Entity, Base, Target, [Base], Path),
    \+ has_property(Entity, Target),
    \+ blocked_fact(Entity, Target),
    \+ cascade_blocked(Entity, Target, _),
    Path = [Base|Rest],
    length(Rest, L), L > 0,
    clean_path(Entity, Path, Chain).

% Find blocked chains for educational display
find_blocked_chain(Entity, Chain) :-
    blocked_fact(Entity, BlockedFact),
    has_property(Entity, Base),
    find_any_path(Base, BlockedFact, [Base], Path),
    Path = [Base|Rest],
    length(Rest, L), L > 0,
    Chain = Path.

% Find any path (including through blocked facts)
find_any_path(Current, Target, _, [Current, Target]) :-
    causal_rule(Current, Target, _).
find_any_path(Current, Target, Visited, [Current|Path]) :-
    causal_rule(Current, Next, _),
    \+ member(Next, Visited),
    find_any_path(Next, Target, [Next|Visited], Path).

% Build valid path (avoiding blocked facts)
find_valid_path(Entity, Current, Target, Visited, [Current, Target]) :-
    causal_rule(Current, Target, _),
    \+ is_overridden(Entity, Target),
    \+ blocked_fact(Entity, Target),
    \+ cascade_blocked(Entity, Target, _),
    \+ member(Target, Visited).
find_valid_path(Entity, Current, Target, Visited, [Current|Path]) :-
    causal_rule(Current, Next, _),
    \+ member(Next, Visited),
    \+ is_overridden(Entity, Next),
    \+ blocked_fact(Entity, Next),
    \+ cascade_blocked(Entity, Next, _),
    find_valid_path(Entity, Next, Target, [Next|Visited], Path).

% Clean path of observed intermediates
clean_path(_, [], []).
clean_path(_, [H], [H]).
clean_path(Entity, [_Start, Next|Rest], Clean) :-
    has_property(Entity, Next),
    !,
    clean_path(Entity, [Next|Rest], Clean).
clean_path(Entity, [Start, Next|Rest], [Start|CleanRest]) :-
    clean_path(Entity, [Next|Rest], CleanRest).

% Format chain nicely
format_chain([], '').
format_chain([X], X) :- !.
format_chain([H|T], Result) :-
    format_chain(T, RestResult),
    (RestResult = '' ->
        Result = H
    ;
        format(atom(Result), '~w → ~w', [H, RestResult])
    ).

% Remove duplicate chains
remove_duplicates([], []).
remove_duplicates(Chains, Unique) :-
    sort(Chains, Unique).

% Find contradictions between all facts (observed and derived)
find_contradiction_with_resolution(Entity, P1, P2, Resolution) :-
    has_property(Entity, P1),
    has_property(Entity, P2),
    P1 @< P2,  % Avoid duplicates
    (is_negation(P1, P2) ; is_dimension_contradiction(_, P1, P2)),
    resolve_contradiction(Entity, P1, P2, Resolution).

find_contradiction_with_resolution(Entity, P1, P2, Resolution) :-
    has_property(Entity, P1),
    derive_consequence(Entity, P2, _),
    P1 @< P2,
    (is_negation(P1, P2) ; is_dimension_contradiction(_, P1, P2)),
    resolve_contradiction(Entity, P1, P2, Resolution).

% Resolve contradiction and return resolution principle
resolve_contradiction(Entity, P1, P2, Resolution) :-
    % Check if one is observed and other is derived
    (has_property(Entity, P1), \+ has_property(Entity, P2),
     derive_consequence(Entity, P2, _) ->
        Resolution = 'observed beats derived, P2 blocked'
    ; has_property(Entity, P2), \+ has_property(Entity, P1),
      derive_consequence(Entity, P1, _) ->
        Resolution = 'observed beats derived, P1 blocked'
    % Both observed or both derived - check polarity
    ; (atom_concat('no_', _, P1) ; property_dimension(P1, _, negative)) ->
        Resolution = 'negative beats positive, P2 blocked'
    ; (atom_concat('no_', _, P2) ; property_dimension(P2, _, negative)) ->
        Resolution = 'negative beats positive, P1 blocked'
    ; Resolution = 'unresolved'
    ).

% Determine which principle caused blocking
determine_blocking_principle(Entity, Blocked, Blocker, Principle) :-
    % Check if blocker is observed and blocked is derived
    (has_property_full(Entity, Blocker, _, _),
     \+ has_property_full(Entity, Blocked, _, _) ->
        Principle = 'observed beats derived'
    ;
    % Check if blocker is negative and blocked is positive
    (atom_concat('no_', _, Blocker) ->
        Principle = 'negative beats positive'
    ;
    % Check confidence difference
    (has_property_full(Entity, Blocker, BConf, _),
     has_property_full(Entity, Blocked, PConf, _),
     BConf > PConf ->
        Principle = 'higher confidence wins'
    ;
        Principle = 'priority-based resolution'
    ))).

% Enhanced analysis with configuration support
analyze_with_config(Entity) :-
    analyze(Entity).

% Main analysis - fully generic with cascade reporting
analyze(Entity) :-
    format('~nAnalyzing ~w:~n', [Entity]),
    
    % Direct observations with confidence
    format('~nDirect observations:~n'),
    forall(has_property(Entity, P),
           (has_property_full(Entity, P, Conf, Priority) ->
               format('  - ~w (conf: ~2f, priority: ~w)~n', [P, Conf, Priority])
           ;
               format('  - ~w~n', [P])
           )),
    
    % Derived insights with effective confidence
    format('~nDerived insights (conflicts resolved):~n'),
    % Get all derivations once for confidence lookup
    derive_all_depths(Entity, [], AllDerivedFacts),
    forall(derive_consequence(Entity, P, because(R)),
           (% Find effective confidence
            (member(derived(P, because(R), EffConf), AllDerivedFacts) ->
                (config_setting(show_rationale(true)) ->
                    (causal_rule_full(R, P, _, _, Rationale) ->
                        format('  - ~w (conf: ~2f, from ~w) [~w]~n', [P, EffConf, R, Rationale])
                    ;
                        format('  - ~w (conf: ~2f, from ~w)~n', [P, EffConf, R])
                    )
                ;
                    format('  - ~w (conf: ~2f, from ~w)~n', [P, EffConf, R])
                )
            ;
                % Fallback without confidence
                (config_setting(show_rationale(true)) ->
                    (causal_rule_full(R, P, _, _, Rationale) ->
                        format('  - ~w (from ~w) [~w]~n', [P, R, Rationale])
                    ;
                        format('  - ~w (from ~w)~n', [P, R])
                    )
                ;
                    format('  - ~w (from ~w)~n', [P, R])
                )
            )
           )),
    
    % Valid reasoning chains (not through blocked facts)
    format('~nValid reasoning chains:~n'),
    findall(Chain, derive_chain(Entity, _, Chain), AllChains),
    remove_duplicates(AllChains, UniqueChains),
    forall((member(Chain, UniqueChains),
            format_chain(Chain, ChainStr)),
           format('  - ~w~n', [ChainStr])),
    
    % Contradiction clusters encountered during resolution
    format('~nContradiction clusters encountered:~n'),
    (contradiction_clusters(Entity, NumClusters, Clusters) ->
        (NumClusters > 0 ->
            format('  ~w conflicts resolved during derivation:~n', [NumClusters]),
            forall(member([Blocked, Blocker, Type], Clusters),
                   (Type = 'negation' -> 
                       format('    - ~w vs ~w (negation conflict)~n', [Blocked, Blocker])
                   ; Type = 'dimension' ->
                       format('    - ~w vs ~w (dimension conflict)~n', [Blocked, Blocker])
                   ; Type = 'observed-derived' ->
                       format('    - ~w vs ~w (observed beats derived)~n', [Blocked, Blocker])
                   ))
        ;
            format('  (none)~n'))
    ;
        format('  (none)~n')
    ),
    
    % Contradictions detected (post-resolution state)
    format('~nContradictions Detected (post-resolution):~n'),
    (setof([P1, P2, Resolution], 
           find_contradiction_with_resolution(Entity, P1, P2, Resolution),
           UniqueContradictions) ->
        forall(member([Prop1, Prop2, Res], UniqueContradictions),
               format('  [!] ~w vs ~w (~w)~n', [Prop1, Prop2, Res]))
    ;
        format('  (none)~n')
    ),
    
    % Unusual patterns (context-aware)
    format('~nUnusual Patterns:~n'),
    forall(find_unusual_pattern(Entity, P1, P2, BaseExplanation),
           (% Check why P2 is absent and provide precise cause
            (block_source(Entity, P2, Blocker, Principle) ->
                format('  [?] ~w - absent due to override by ~w (~w)~n', 
                       [BaseExplanation, Blocker, Principle])
            ; cascade_blocked(Entity, P2, Sources) ->
                sort(Sources, UniqueSources),
                format('  [?] ~w - absent due to cascade from blocked ~w~n', 
                       [BaseExplanation, UniqueSources])
            ;
                format('  [?] ~w - pattern expectation not met~n', [BaseExplanation])
            ))),
    
    % Blocked derivations with resolution principle
    format('~nBlocked Derivations (with resolution principle):~n'),
    (setof([P, Blocker, Principle], 
           block_source(Entity, P, Blocker, Principle), 
           BlockedList) ->
        forall(member([Fact, BlockerFact, Princ], BlockedList),
               format('  - ~w blocked by ~w (~w)~n', [Fact, BlockerFact, Princ]))
    ;
        format('  (none)~n')),
    
    % Cascade blocked (downstream from conflicts)
    format('~nCascade Blocked (downstream from conflicts):~n'),
    (setof(Fact, S^cascade_blocked(Entity, Fact, S), CascadeFacts) ->
        forall(member(F, CascadeFacts),
               (cascade_blocked(Entity, F, Sources),
                sort(Sources, UniqueSources),
                format('  - ~w blocked (depends on blocked ~w)~n', [F, UniqueSources])))
    ;
        true),
    
    % Show unique blocked chains (deduplicated)
    format('~nBlocked Chains (what would have been derived):~n'),
    findall(Chain, find_blocked_chain(Entity, Chain), AllBlockedChains),
    sort(AllBlockedChains, UniqueBlockedChains),  % Remove duplicates
    length(UniqueBlockedChains, TotalBlocked),
    (TotalBlocked > 10 ->
        % Show only first 10 if many
        append(First10, _, UniqueBlockedChains),
        length(First10, 10),
        forall((member(Chain, First10),
                format_chain(Chain, ChainStr)),
               format('  × ~w~n', [ChainStr])),
        Remaining is TotalBlocked - 10,
        format('  ... and ~w more blocked chains~n', [Remaining])
    ;
        % Show all if 10 or fewer
        forall((member(Chain, UniqueBlockedChains),
                format_chain(Chain, ChainStr)),
               format('  × ~w~n', [ChainStr]))
    ),
    
    % Summary statistics with unique counts
    format('~n--- Summary Statistics ---~n'),
    aggregate_all(count, has_property(Entity, _), ObservedCount),
    aggregate_all(count, derive_consequence(Entity, _, _), DerivedCount),
    
    % Count unique blocked facts
    (setof(F, blocked_fact(Entity, F), UniqueBlockedFacts) -> 
        length(UniqueBlockedFacts, UniqueBlockedCount) ; UniqueBlockedCount = 0),
    
    % Count unique cascade blocked facts
    (setof(F, S^cascade_blocked(Entity, F, S), UniqueCascadeFacts) -> 
        length(UniqueCascadeFacts, UniqueCascadeCount) ; UniqueCascadeCount = 0),
    
    format('Observed facts: ~w~n', [ObservedCount]),
    format('Derived facts: ~w~n', [DerivedCount]),
    format('Directly blocked: ~w unique facts~n', [UniqueBlockedCount]),
    format('Cascade blocked: ~w unique facts~n', [UniqueCascadeCount]),
    
    % Show top blockers (only if there are any)
    (UniqueBlockedCount > 0 ->
        (format('~nTop blocking reasons:~n'),
         % Get unique blockers from block_source
         setof(Blocker, P^Pr^block_source(Entity, P, Blocker, Pr), RealBlockers),
         findall(Blocker-TotalCount,
                 (member(Blocker, RealBlockers),
                  % Count facts directly blocked by this blocker
                  aggregate_all(count, block_source(Entity, _, Blocker, _), DirectBlockCount),
                  % Count cascade blocks triggered by facts this blocker blocked
                  findall(CascadeFact,
                          (block_source(Entity, DirectBlocked, Blocker, _),
                           cascade_blocked(Entity, CascadeFact, Sources),
                           member(DirectBlocked, Sources)),
                          CascadeFactsList),
                  sort(CascadeFactsList, UniqueCascadeFacts),
                  length(UniqueCascadeFacts, CascadeCount),
                  TotalCount is DirectBlockCount + CascadeCount),
                 BlockerCounts),
         sort(2, @>=, BlockerCounts, SortedBlockers),
         % Show top 3 blockers with their actual blocking effect
         (SortedBlockers = [B1-C1|_] ->
             format('  1. ~w (blocks ~w facts total)~n', [B1, C1])
         ; true),
         (SortedBlockers = [_,B2-C2|_] ->
             format('  2. ~w (blocks ~w facts total)~n', [B2, C2])
         ; true),
         (SortedBlockers = [_,_,B3-C3|_] ->
             format('  3. ~w (blocks ~w facts total)~n', [B3, C3])
         ; true))
    ; true),
    
    % Policy footer
    format('~n--- Active Configuration ---~n'),
    (config_setting(min_confidence(MinConf)) -> 
        format('Min confidence threshold: ~2f~n', [MinConf]) ; 
        format('Min confidence threshold: 0.5 (default)~n')),
    (config_setting(cascade_blocking(Cascade)) -> 
        format('Cascade blocking: ~w~n', [Cascade]) ; 
        format('Cascade blocking: true (default)~n')),
    (config_setting(show_rationale(ShowRat)) -> 
        format('Show rationale: ~w~n', [ShowRat]) ; 
        format('Show rationale: false (default)~n')),
    format('Resolution strategy: negative facts override positive~n'),
    
    % Count rules and diagnostics
    aggregate_all(count, causal_rule(_, _, _), TotalRules),
    
    % Count rules that COULD apply based on available antecedents
    findall(A-C, (causal_rule(A, C, _), 
                  (has_property(Entity, A) ; derive_consequence(Entity, A, _))),
            PossibleRules),
    sort(PossibleRules, UniquePossibleRules),
    length(UniquePossibleRules, ApplicableRules),
    
    % Count rules that actually produced surviving facts
    findall(A-C, (derive_consequence(Entity, C, because(A))),
            ActiveRules),
    sort(ActiveRules, UniqueActiveRules),
    length(UniqueActiveRules, AppliedRules),
    
    % Count blocked rules
    (setof(A-C, P^Pr^(block_source(Entity, C, _, _), 
                      causal_rule(A, C, _)),
           BlockedRules) ->
        length(BlockedRules, BlockedCount)
    ;
        BlockedCount = 0
    ),
    
    format('Causal rules: ~w total, ~w applicable, ~w applied, ~w blocked~n', 
           [TotalRules, ApplicableRules, AppliedRules, BlockedCount]),
    
    % Count contradictions
    findall(1, find_contradiction_with_resolution(Entity, _, _, _), Contradictions),
    length(Contradictions, ContradictionCount),
    format('Contradictions detected and resolved: ~w~n', [ContradictionCount]),
    
    % Check for unresolved contradictions (validation)
    (find_contradiction_with_resolution(Entity, _, _, 'unresolved') ->
        format('WARNING: Unresolved contradictions exist!~n')
    ; true).

% Clean up for fresh analysis
reset_analysis :-
    retractall(has_property(_, _)),
    retractall(has_property_full(_, _, _, _)),
    retractall(causal_rule(_, _, _)),
    retractall(causal_rule_full(_, _, _, _, _)),
    retractall(expected_pattern(_, _, _, _)),
    retractall(property_negates(_, _)),
    retractall(blocked_fact(_, _)),
    retractall(cascade_blocked(_, _, _)),
    retractall(block_source(_, _, _, _)),
    retractall(contradiction_clusters(_, _, _)).
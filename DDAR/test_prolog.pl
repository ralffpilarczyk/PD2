%%% Run unified theorem engine on DDAR facts

:- consult('theorems/theorem_engine.pl').
:- consult('ddar_facts_norm.pl').
:- consult('ddar_bridge.pl').

run_unified_analysis :-
    bridge_all_facts,
    writeln('Unified Theorem Analysis'),
    format('~`=t~50|~n', []),
    writeln(''),
    analyze_company(axiata, Results),
    writeln('RESULTS_START'),
    writeln(Results),
    writeln('RESULTS_END'),
    halt.

:- initialization(run_unified_analysis).

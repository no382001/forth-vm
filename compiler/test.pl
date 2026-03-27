:- module(test, [run_tests/1, run_tests/0]).

:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(charsio)).

%% run_tests(+Quads)
%% Quads = list of test(Name, Got, Expected, Cmp)
%% Cmp = eq | match
%% Prints results, halts with 1 on failure.

run_tests(Quads) :-
    run_all(Quads, 0, 0, Pass, Fail),
    format("~d passed, ~d failed~n", [Pass, Fail]),
    ( Fail > 0 -> halt(1) ; true ).

run_tests :-
    format("no tests~n", []).

run_all([], P, F, P, F).
run_all([test(Name, Got, Expected, Cmp)|Rest], P0, F0, P, F) :-
    ( compare_result(Cmp, Got, Expected) ->
        P1 is P0 + 1,
        run_all(Rest, P1, F0, P, F)
    ;
        format("FAIL ~w~n  expected: ~w~n       got: ~w~n", [Name, Expected, Got]),
        F1 is F0 + 1,
        run_all(Rest, P0, F1, P, F)
    ).

compare_result(eq, X, X).
compare_result(eq_ok, ok(_), ok(_)).
compare_result(check_error, error(_), is_error).

%% quadtests.pl — Quad test checker for Scryer Prolog
%%
%% Reads a module file, extracts ?- query / expected answer pairs,
%% runs them and reports results.
%%
%% Usage:
%%   scryer-prolog -f -g "use_module(quadtests), check_module_quads(mymodule, _), halt"

:- module(quadtests, [check_module_quads/2]).

:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(charsio)).

%% ============================================================
%% entry
%% ============================================================

check_module_quads(Module, Result) :-
    module_file(Module, File),
    read_file_terms(File, Terms),
    extract_quads(Terms, Quads),
    length(Quads, N),
    format("% Checking ~d quads for ~w~n", [N, Module]),
    run_quads(Module, Quads, 0, 0, Pass, Fail),
    format("% ~d passed, ~d failed~n", [Pass, Fail]),
    ( Fail > 0 -> Result = fail ; Result = ok ).

module_file(Module, File) :-
    atom_chars(Module, MChars),
    append(MChars, ".pl", FChars),
    atom_chars(File, FChars).

%% ============================================================
%% read all terms from a file
%% ============================================================

read_file_terms(File, Terms) :-
    open(File, read, Stream),
    read_all_terms(Stream, Terms),
    close(Stream).

read_all_terms(Stream, Terms) :-
    catch(
        read_term(Stream, Term, []),
        _,
        Term = end_of_file
    ),
    ( Term == end_of_file ->
        Terms = []
    ;
        Terms = [Term | Rest],
        read_all_terms(Stream, Rest)
    ).

%% ============================================================
%% extract quad pairs: ?- Query followed by expected answer
%% ============================================================

extract_quads([], []).
extract_quads([(?- Query), Expected | Rest], [quad(Query, Expected) | Quads]) :-
    !,
    extract_quads(Rest, Quads).
extract_quads([_ | Rest], Quads) :-
    extract_quads(Rest, Quads).

%% ============================================================
%% run quads
%% ============================================================

run_quads(_, [], P, F, P, F).
run_quads(Module, [quad(Query, Expected) | Rest], P0, F0, P, F) :-
    format("% CHECKING.. ~w~n", [Query]),
    ( check_quad(Module, Query, Expected) ->
        P1 is P0 + 1,
        run_quads(Module, Rest, P1, F0, P, F)
    ;
        format("% FAIL: ~w~n%   expected: ~w~n", [Query, Expected]),
        F1 is F0 + 1,
        run_quads(Module, Rest, P0, F1, P, F)
    ).

%% ============================================================
%% check a single quad
%% ============================================================

%% Expected = true: query must succeed
check_quad(Module, Query, true) :-
    Module:Query, !.

%% Expected = false: query must fail
check_quad(Module, Query, false) :-
    \+ Module:Query.

%% Expected = bindings (e.g., X = 10): run query, then check bindings match
check_quad(Module, Query, Expected) :-
    Expected \= true,
    Expected \= false,
    Module:Query,
    call(Expected), !.

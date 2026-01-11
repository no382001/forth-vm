:- module(macro, [resolve/2]).

:- use_module(assemble).

resolve(Source, Result) :-
    assemble:tokenize(Source, ok(Tokens)),
    phrase(parse_terms(Terms), Tokens),
    expand_macros(Terms, [], Expanded),
    collect_labels(Expanded, 0, Labels),
    maplist(resolve_ref(Labels), Expanded, Nested), !,
    append(Nested, Resolved),
    Result = ok(Resolved).

% ============================================================
% parsing
% ============================================================

parse_terms([T|Ts]) --> parse_term(T), !, parse_terms(Ts).
parse_terms([]) --> [].

parse_term(macro_call(Name, Args)) -->
    [Tok],
    { atom_string(Tok, S), sub_string(S, _, _, _, "("),
      split_string(S, "(", ")", [N, A]),
      atom_string(Name, N),
      split_string(A, ",", " ", ArgStrs),
      maplist(parse_arg, ArgStrs, Args) }, !.

parse_term(Tok) --> [Tok].

% sonvert "5" -> 5, "foo" -> foo
parse_arg(Str, Num) :- number_string(Num, Str), !.
parse_arg(Str, Atom) :- atom_string(Atom, Str).

% ============================================================
% expand macros
% ============================================================

expand_macros([], _, []).

expand_macros([macro_call(def, [N, V])|T], Defs, Out) :-
    expand_macros(T, [N-V|Defs], Out).

expand_macros([macro_call(subst, [N])|T], Defs, [V|Out]) :-
    member(N-V, Defs),
    expand_macros(T, Defs, Out).

% expand_macros([macro_call(Name, Args)|T], Defs, Out) :-
%     macro(Name, Args, Expansion), !,
%     append(Expansion, T, NewTerms),
%     expand_macros(NewTerms, Defs, Out).

expand_macros([X|T], Defs, [X|Out]) :-
    expand_macros(T, Defs, Out).

% ============================================================
% collect labels
% ============================================================

collect_labels([], _, []).
collect_labels([macro_call(label, [N])|T], PC, [N-PC|Labels]) :-
    collect_labels(T, PC, Labels).
collect_labels([macro_call(branch, _)|T], PC, Labels) :-
    PC1 is PC + 1,
    collect_labels(T, PC1, Labels).
collect_labels([macro_call(zbranch, _)|T], PC, Labels) :-
    PC1 is PC + 1,
    collect_labels(T, PC1, Labels).
collect_labels([X|T], PC, Labels) :-
    X \= macro_call(_, _),
    PC1 is PC + 1,
    collect_labels(T, PC1, Labels).

% ============================================================
% resolve references
% ============================================================

resolve_ref(_, macro_call(label, _), []) :- !.
resolve_ref(Labels, macro_call(branch, [L]), [branch(Addr)]) :- member(L-Addr, Labels), !.
resolve_ref(Labels, macro_call(zbranch, [L]), [zbranch(Addr)]) :- member(L-Addr, Labels), !.
resolve_ref(_, X, [X]).
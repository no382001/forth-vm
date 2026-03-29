:- module(deadcode, [find_dead_code/2]).

:- use_module(library(lists)).

%% ============================================================
%% entry point
%% ============================================================

find_dead_code(Defs, DeadNames) :-
    collect_defined(Defs, Defined),
    collect_referenced(Defs, Referenced),
    findall(Name,
        (member(Name, Defined),
         Name \= main,
         \+ member(Name, Referenced)),
        DeadNames).

%% ============================================================
%% collect defined function names
%% ============================================================

collect_defined([], []).
collect_defined([def(Name, _, _, _, _)|Rest], [Name|Names]) :-
    collect_defined(Rest, Names).
collect_defined([const(_, _, _)|Rest], Names) :-
    collect_defined(Rest, Names).
collect_defined([extern(_, _, _)|Rest], Names) :-
    collect_defined(Rest, Names).

%% ============================================================
%% collect all referenced names (calls + addr)
%% ============================================================

collect_referenced([], []).
collect_referenced([def(_, _, _, _, Body)|Rest], Refs) :-
    body_refs(Body, BodyRefs),
    collect_referenced(Rest, RestRefs),
    append(BodyRefs, RestRefs, Refs).
collect_referenced([const(_, _, _)|Rest], Refs) :-
    collect_referenced(Rest, Refs).
collect_referenced([extern(_, _, _)|Rest], Refs) :-
    collect_referenced(Rest, Refs).

body_refs([], []).
body_refs([E|Es], Refs) :-
    expr_refs(E, ERefs),
    body_refs(Es, RestRefs),
    append(ERefs, RestRefs, Refs).

expr_refs(num(_), []).
expr_refs(str(_), []).
expr_refs(var(_), []).
expr_refs(addr(Name), [Name]).
expr_refs(execute(E), Refs) :- expr_refs(E, Refs).
expr_refs(deref(E), Refs) :- expr_refs(E, Refs).
expr_refs(deref8(E), Refs) :- expr_refs(E, Refs).

expr_refs(store(A, V), Refs) :-
    expr_refs(A, RA), expr_refs(V, RV),
    append(RA, RV, Refs).
expr_refs(store8(A, V), Refs) :-
    expr_refs(A, RA), expr_refs(V, RV),
    append(RA, RV, Refs).

expr_refs(binop(_, A, B), Refs) :-
    expr_refs(A, RA), expr_refs(B, RB),
    append(RA, RB, Refs).

expr_refs(if(C, T, E), Refs) :-
    expr_refs(C, RC), expr_refs(T, RT), expr_refs(E, RE),
    append(RC, RT, R1), append(R1, RE, Refs).

expr_refs(let(Bindings, Body), Refs) :-
    bindings_refs(Bindings, BR),
    body_refs(Body, BodyR),
    append(BR, BodyR, Refs).

expr_refs(do(Exprs), Refs) :- body_refs(Exprs, Refs).

expr_refs(while(Cond, Body), Refs) :-
    expr_refs(Cond, CR),
    body_refs(Body, BR),
    append(CR, BR, Refs).

expr_refs(call(Name, Args), [Name|ArgRefs]) :-
    args_refs(Args, ArgRefs).

bindings_refs([], []).
bindings_refs([bind(_, Expr)|Rest], Refs) :-
    expr_refs(Expr, ER),
    bindings_refs(Rest, RR),
    append(ER, RR, Refs).

args_refs([], []).
args_refs([A|As], Refs) :-
    expr_refs(A, AR),
    args_refs(As, RR),
    append(AR, RR, Refs).

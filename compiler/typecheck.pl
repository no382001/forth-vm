:- module(typecheck, [check_program/2]).

:- use_module(library(lists)).

%% ============================================================
%% entry point
%% ============================================================

check_program(Defs, Result) :-
    builtin_env(Builtins),
    build_func_env(Defs, Builtins, FuncEnv),
    check_defs(Defs, FuncEnv, Errors),
    ( Errors = [] ->
        Result = ok(Defs)
    ;
        Result = error(Errors)
    ).

%% built-in functions (traps)
builtin_env([
    func(emit, [int], void),
    func(key, [], int),
    func(bye, [], void),
    func('assert-fail', [], void)
]).

%% ============================================================
%% build function environment from definitions
%% ============================================================

build_func_env([], Env, Env).
build_func_env([def(Name, Params, RetType, _Body) | Rest], Acc, Env) :-
    param_types(Params, PTypes),
    build_func_env(Rest, [func(Name, PTypes, RetType) | Acc], Env).
build_func_env([extern(Name, PTypes, RetType) | Rest], Acc, Env) :-
    build_func_env(Rest, [func(Name, PTypes, RetType) | Acc], Env).
build_func_env([const(Name, Type, _) | Rest], Acc, Env) :-
    build_func_env(Rest, [const(Name, Type) | Acc], Env).

param_types([], []).
param_types([param(_, T) | Rest], [T | Ts]) :-
    param_types(Rest, Ts).

%% ============================================================
%% check all definitions
%% ============================================================

check_defs([], _, []).
check_defs([Def | Rest], FuncEnv, Errors) :-
    check_def(Def, FuncEnv, DefErrors),
    check_defs(Rest, FuncEnv, RestErrors),
    append(DefErrors, RestErrors, Errors).

check_def(extern(_, _, _), _, []).
check_def(const(Name, Type, Expr), FuncEnv, Errors) :-
    ( infer([], FuncEnv, Expr, ExprType),
      types_compatible(Type, ExprType) ->
        Errors = []
    ;
        Errors = [type_mismatch(const, Name, Type)]
    ).
check_def(def(Name, Params, RetType, Body), FuncEnv, Errors) :-
    params_to_env(Params, LocalEnv),
    check_body(Body, LocalEnv, FuncEnv, RetType, Name, Errors).

params_to_env([], []).
params_to_env([param(N, T) | Rest], [var(N, T) | Env]) :-
    params_to_env(Rest, Env).

%% ============================================================
%% check function body (list of exprs, last one is return value)
%% ============================================================

check_body([], _, _, void, _, []).
check_body([Expr], Env, FEnv, RetType, FName, Errors) :-
    ( infer(Env, FEnv, Expr, ExprType),
      types_compatible(RetType, ExprType) ->
        Errors = []
    ;
        Errors = [return_type_mismatch(FName, RetType)]
    ).
check_body([Expr | Rest], Env, FEnv, RetType, FName, Errors) :-
    Rest = [_|_],
    check_expr_for_side_effects(Env, FEnv, Expr, ExprErrors, Env1),
    check_body(Rest, Env1, FEnv, RetType, FName, RestErrors),
    append(ExprErrors, RestErrors, Errors).

%% expressions used for side effects (middle of body)
check_expr_for_side_effects(Env, FEnv, Expr, Errors, NewEnv) :-
    ( Expr = let(Bindings, _Body) ->
        check_let_bindings(Bindings, Env, FEnv, [], BindErrors, ExtEnv),
        Expr = let(_, LetBody),
        check_let_body(LetBody, ExtEnv, FEnv, LetBodyErrors),
        append(BindErrors, LetBodyErrors, Errors),
        NewEnv = Env  % let doesn't leak scope
    ; infer(Env, FEnv, Expr, _) ->
        Errors = [],
        NewEnv = Env
    ;
        Errors = [type_error(Expr)],
        NewEnv = Env
    ).

%% ============================================================
%% type inference
%% ============================================================

%% literals
infer(_, _, num(_), int).
infer(_, _, str(_), ptr(byte)).

%% variable lookup
infer(Env, _, var(Name), Type) :-
    member(var(Name, Type), Env), !.
infer(_, FEnv, var(Name), Type) :-
    member(const(Name, Type), FEnv).

%% binary arithmetic -> int
infer(Env, FEnv, binop(Op, A, B), int) :-
    member(Op, [+, -, *, 'and', 'or', 'xor']),
    infer(Env, FEnv, A, AT),
    infer(Env, FEnv, B, BT),
    numeric_type(AT),
    numeric_type(BT).

%% comparison -> bool
infer(Env, FEnv, binop(Op, A, B), bool) :-
    member(Op, [=, <, >, '!=', '<=', '>=']),
    infer(Env, FEnv, A, AT),
    infer(Env, FEnv, B, BT),
    numeric_type(AT),
    numeric_type(BT).

%% if -> type of branches (must agree)
infer(Env, FEnv, if(Cond, Then, Else), T) :-
    infer(Env, FEnv, Cond, bool),
    infer(Env, FEnv, Then, T),
    infer(Env, FEnv, Else, T2),
    types_compatible(T, T2).

%% let
infer(Env, FEnv, let(Bindings, Body), T) :-
    eval_bindings(Bindings, Env, FEnv, ExtEnv),
    infer_body(Body, ExtEnv, FEnv, T).

%% do — returns type of last expr
infer(Env, FEnv, do(Exprs), T) :-
    infer_body(Exprs, Env, FEnv, T).

%% while -> void
infer(Env, FEnv, while(Cond, Body), void) :-
    infer(Env, FEnv, Cond, bool),
    infer_body(Body, Env, FEnv, _).

%% deref: ptr(T) -> T
infer(Env, FEnv, deref(E), T) :-
    infer(Env, FEnv, E, ptr(T)).

%% store: ptr(T) × T -> void
infer(Env, FEnv, store(Addr, Val), void) :-
    infer(Env, FEnv, Addr, ptr(T)),
    infer(Env, FEnv, Val, VT),
    types_compatible(T, VT).

%% store8: ptr(byte) × byte -> void
infer(Env, FEnv, store8(Addr, Val), void) :-
    infer(Env, FEnv, Addr, ptr(byte)),
    infer(Env, FEnv, Val, VT),
    numeric_type(VT).

%% function call
infer(Env, FEnv, call(Name, Args), RetType) :-
    member(func(Name, ParamTypes, RetType), FEnv),
    length(ParamTypes, Arity),
    length(Args, Arity),
    check_args(Args, ParamTypes, Env, FEnv).

%% ============================================================
%% helpers
%% ============================================================

infer_body([E], Env, FEnv, T) :-
    infer(Env, FEnv, E, T).
infer_body([E | Rest], Env, FEnv, T) :-
    Rest = [_|_],
    infer(Env, FEnv, E, _),
    infer_body(Rest, Env, FEnv, T).

eval_bindings([], Env, _, Env).
eval_bindings([bind(Name, Expr) | Rest], Env, FEnv, FinalEnv) :-
    infer(Env, FEnv, Expr, T),
    eval_bindings(Rest, [var(Name, T) | Env], FEnv, FinalEnv).

check_let_bindings([], Env, _, _, [], Env).
check_let_bindings([bind(Name, Expr) | Rest], Env, FEnv, Acc, Errors, FinalEnv) :-
    ( infer(Env, FEnv, Expr, T) ->
        check_let_bindings(Rest, [var(Name, T) | Env], FEnv, Acc, Errors, FinalEnv)
    ;
        check_let_bindings(Rest, Env, FEnv, Acc, RestErrors, FinalEnv),
        Errors = [type_error(bind(Name, Expr)) | RestErrors]
    ).

check_let_body([], _, _, []).
check_let_body([E], Env, FEnv, Errors) :-
    ( infer(Env, FEnv, E, _) -> Errors = [] ; Errors = [type_error(E)] ).
check_let_body([E | Rest], Env, FEnv, Errors) :-
    Rest = [_|_],
    ( infer(Env, FEnv, E, _) -> E1Errors = [] ; E1Errors = [type_error(E)] ),
    check_let_body(Rest, Env, FEnv, RestErrors),
    append(E1Errors, RestErrors, Errors).

check_args([], [], _, _).
check_args([A | As], [T | Ts], Env, FEnv) :-
    infer(Env, FEnv, A, AT),
    types_compatible(T, AT),
    check_args(As, Ts, Env, FEnv).

%% int and byte are interchangeable for arithmetic
%% bool is separate
types_compatible(T, T) :- !.
types_compatible(int, byte) :- !.
types_compatible(byte, int) :- !.

numeric_type(int).
numeric_type(byte).

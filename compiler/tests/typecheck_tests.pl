:- module(typecheck_tests, [run/0]).

:- use_module('../parser').
:- use_module('../ast').
:- use_module('../typecheck').
:- use_module('../test').

run :-
    Tests = [
        %% basic function typing
        test(identity_int, R1, ok(_), eq_ok),
        test(add_two_ints, R2, ok(_), eq_ok),
        test(comparison_returns_bool, R3, ok(_), eq_ok),
        test(if_needs_bool_cond, R4, ok(_), eq_ok),

        %% type errors
        test(wrong_return_type, R5, is_error, check_error),
        test(if_branch_mismatch, R6, is_error, check_error),
        test(undefined_var, R7, is_error, check_error),
        test(wrong_arg_count, R8, is_error, check_error),

        %% let bindings
        test(let_binding, R9, ok(_), eq_ok),

        %% pointers
        test(deref_ptr, R10, ok(_), eq_ok),
        test(store_typed, R11, ok(_), eq_ok),

        %% extern
        test(extern_call, R12, ok(_), eq_ok),

        %% const
        test(const_def, R13, ok(_), eq_ok),

        %% while
        test(while_void, R14, ok(_), eq_ok),

        %% multi-expr body
        test(multi_body, R15, ok(_), eq_ok)
    ],
    pipeline("(def id ((x : int)) : int x)", R1),
    pipeline("(def add ((a : int) (b : int)) : int (+ a b))", R2),
    pipeline("(def lt ((a : int) (b : int)) : bool (< a b))", R3),
    pipeline("(def abs ((n : int)) : int (if (< n 0) (- 0 n) n))", R4),
    pipeline("(def bad ((n : int)) : bool n)", R5),
    pipeline("(def bad ((n : int)) : int (if (< n 0) 1 (< n 5)))", R6),
    pipeline("(def bad () : int x)", R7),
    pipeline("(def f ((x : int)) : int x) (def g () : int (f 1 2))", R8),
    pipeline("(def f ((x : int)) : int (let ((y (+ x 1))) y))", R9),
    pipeline("(def f ((p : (ptr int))) : int (deref p))", R10),
    pipeline("(def f ((p : (ptr int)) (v : int)) : void (store p v))", R11),
    pipeline("(extern emit (int) : void) (def f () : void (emit 65))", R12),
    pipeline("(const BUFSIZE int 256) (def f () : int BUFSIZE)", R13),
    pipeline("(def f ((n : int)) : void (while (< n 10) (+ n 1)))", R14),
    pipeline("(extern emit (int) : void) (def f ((n : int)) : int (emit n) (+ n 1))", R15),
    run_tests(Tests).

%% parse -> ast transform -> typecheck
pipeline(Src, Result) :-
    parse(Src, ParseResult),
    ( ParseResult = ok(Forms) ->
        transform_program(Forms, AstResult),
        ( AstResult = ok(Defs) ->
            check_program(Defs, Result)
        ;
            Result = AstResult
        )
    ;
        Result = ParseResult
    ).

%% custom comparators added to test framework
:- use_module('../test').

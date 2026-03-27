:- module(codegen_tests, [run/0]).

:- use_module('../parser').
:- use_module('../ast').
:- use_module('../typecheck').
:- use_module('../codegen').
:- use_module('../compiler').
:- use_module('../test').

run :-
    Tests = [
        test(emit_A, R1, ok(_), eq_ok),
        test(add_and_emit, R2, ok(_), eq_ok),
        test(if_expr, R3, ok(_), eq_ok),
        test(let_binding, R4, ok(_), eq_ok),
        test(function_call, R5, ok(_), eq_ok),
        test(ir_has_labels, R6, true, eq)
    ],

    %% (emit 65) -> should compile to: lit 65, lit 0, trap
    compile_source("(def main () : void (emit 65) (bye))", ir, R1),

    %% (+ 60 5) then emit -> should produce lit 60, lit 5, +, lit 0, trap
    compile_source("(def main () : void (emit (+ 60 5)) (bye))", ir, R2),

    %% if expression
    compile_source("(def main () : void (emit (if (< 1 2) 65 66)) (bye))", ir, R3),

    %% let
    compile_source("(def main () : void (let ((x 65)) (emit x)) (bye))", ir, R4),

    %% function call
    compile_source(
        "(def add1 ((n : int)) : int (+ n 1)) (def main () : void (emit (add1 64)) (bye))",
        ir, R5),

    %% check that IR contains expected labels
    ( R5 = ok(Tokens), member(label(main), Tokens), member(label(add1), Tokens) ->
        R6 = true ; R6 = false ),

    run_tests(Tests).

:- module(parser_tests, [run/0]).

:- use_module('../parser').
:- use_module('../test').

run :-
    Tests = [
        %% atoms
        test(sym, G1, ok([sym(hello)]), eq),
        test(operator, G2, ok([sym(+)]), eq),
        test(multi_sym, G3, ok([sym(foo), sym(bar)]), eq),

        %% numbers
        test(pos_num, G4, ok([num(42)]), eq),
        test(neg_num, G5, ok([num(-7)]), eq),
        test(zero, G6, ok([num(0)]), eq),

        %% lists
        test(empty_list, G7, ok([list([])]), eq),
        test(simple_list, G8, ok([list([sym(+), num(1), num(2)])]), eq),
        test(nested_list, G9, ok([list([sym(if), list([sym(<), sym(x), num(0)]), num(1), num(2)])]), eq),

        %% strings
        test(string_lit, G10, ok([str("hello")]), eq),

        %% whitespace / comments
        test(ws_handling, G11, ok([num(1), num(2)]), eq),
        test(comment, G12, ok([num(42)]), eq),

        %% function def
        test(def_form, G13,
            ok([list([sym(def), sym(square), list([sym(x)]),
                 sym(:), sym(int),
                 list([sym(*), sym(x), sym(x)])])]),
            eq),

        %% empty
        test(empty, G14, ok([]), eq)
    ],
    parse("hello", G1),
    parse("+", G2),
    parse("foo bar", G3),
    parse("42", G4),
    parse("-7", G5),
    parse("0", G6),
    parse("()", G7),
    parse("(+ 1 2)", G8),
    parse("(if (< x 0) 1 2)", G9),
    parse("\"hello\"", G10),
    parse("  1   2  ", G11),
    parse("; this is a comment\n42", G12),
    parse("(def square (x) : int (* x x))", G13),
    parse("", G14),
    run_tests(Tests).

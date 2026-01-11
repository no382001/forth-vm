:- module(macro_tests, []).

:- use_module(library(plunit)).
:- use_module(assembler/macro).

:- begin_tests(macro).

test(def_simple) :-
    macro:resolve("def(BUFFER,1000) lit subst(BUFFER) @", Result),
    assertion(Result == ok([lit, 1000, '@'])).

test(def_multiple) :-
    macro:resolve("def(X,42) def(Y,100) subst(X) subst(Y) +", Result),
    assertion(Result == ok([42, 100, '+'])).

test(def_zero) :-
    macro:resolve("def(ZERO,0) subst(ZERO)", Result),
    assertion(Result == ok([0])).

test(def_large_number) :-
    macro:resolve("def(ADDR,65535) subst(ADDR)", Result),
    assertion(Result == ok([65535])).

test(label_simple) :-
    macro:resolve("label(start) lit 5 dup", Result),
    assertion(Result == ok([lit, 5, dup])).

test(label_multiple) :-
    macro:resolve("label(start) lit 1 label(middle) lit 2 label(end) lit 3", Result),
    assertion(Result == ok([lit, 1, lit, 2, lit, 3])).

test(label_at_position_zero) :-
    macro:resolve("label(loop) dup", Result),
    assertion(Result == ok([dup])).

test(branch_forward) :-
    macro:resolve("branch(end) lit 1 label(end) lit 2", Result),
    assertion(Result == ok([branch(3), lit, 1, lit, 2])).

test(branch_backward) :-
    macro:resolve("label(loop) dup branch(loop) lit 1", Result),
    assertion(Result == ok([dup, branch(0), lit, 1])).

test(branch_to_same_position) :-
    macro:resolve("label(here) branch(here)", Result),
    assertion(Result == ok([branch(0)])).

test(branch_multiple) :-
    macro:resolve("label(a) lit 1 branch(b) lit 2 label(b) lit 3", Result),
    assertion(Result == ok([lit, 1, branch(5), lit, 2, lit, 3])).

test(zbranch_forward) :-
    macro:resolve("zbranch(end) lit 1 label(end) lit 2", Result),
    assertion(Result == ok([zbranch(3), lit, 1, lit, 2])).

test(zbranch_backward) :-
    macro:resolve("label(loop) dup zbranch(loop) lit 1", Result),
    assertion(Result == ok([dup, zbranch(0), lit, 1])).

test(zbranch_multiple) :-
    macro:resolve("label(a) zbranch(b) lit 1 label(b) zbranch(c) label(c) lit 2", Result),
    assertion(Result == ok([zbranch(3), lit, 1, zbranch(4), lit, 2])).

test(subst_single) :-
    macro:resolve("def(X,10) subst(X)", Result),
    assertion(Result == ok([10])).

test(subst_multiple) :-
    macro:resolve("def(A,1) def(B,2) subst(A) subst(B) +", Result),
    assertion(Result == ok([1, 2, '+'])).

test(subst_in_expression) :-
    macro:resolve("def(VAL,5) lit subst(VAL) dup +", Result),
    assertion(Result == ok([lit, 5, dup, '+'])).

test(def_and_label) :-
    macro:resolve("def(ADDR,1000) label(start) lit subst(ADDR) @", Result),
    assertion(Result == ok([lit, 1000, '@'])).

test(label_and_branch) :-
    macro:resolve("label(loop) dup branch(loop)", Result),
    assertion(Result == ok([dup, branch(0)])).

test(def_label_branch_subst) :-
    macro:resolve("def(X,42) label(loop) subst(X) dup zbranch(end) branch(loop) label(end) drop", Result),
    assertion(Result == ok([42, dup, zbranch(4), branch(0), drop])).


test(bootloader_pattern) :-
    macro:resolve("def(BUFFER,1000) def(END,1001) label(loop) lit subst(BUFFER) @ lit subst(END) ! branch(loop)", Result),
    assertion(Result == ok([lit, 1000, '@', lit, 1001, '!', branch(0)])).

test(conditional_loop) :-
    macro:resolve("def(LIMIT,10) label(start) subst(LIMIT) dup zbranch(done) 1 - branch(start) label(done) drop", Result),
    assertion(Result == ok([10, dup, zbranch(6), 1, '-', branch(0), drop])).

test(forward_reference) :-
    % [branch(?), lit, 1, lit, 2] - label(forward) at index 3
    macro:resolve("branch(forward) lit 1 label(forward) lit 2", Result),
    assertion(Result == ok([branch(3), lit, 1, lit, 2])).

test(empty_input) :-
    macro:resolve("", Result),
    assertion(Result == ok([])).

test(only_label) :-
    macro:resolve("label(x)", Result),
    assertion(Result == ok([])).

test(only_def) :-
    macro:resolve("def(X,5)", Result),
    assertion(Result == ok([])).

test(label_then_label) :-
    macro:resolve("label(a) label(b) lit 1", Result),
    assertion(Result == ok([lit, 1])).

test(multiple_branches_same_label) :-
    macro:resolve("branch(end) lit 1 branch(end) lit 2 label(end) lit 3", Result),
    assertion(Result == ok([branch(6), lit, 1, branch(6), lit, 2, lit, 3])).

:- end_tests(macro).
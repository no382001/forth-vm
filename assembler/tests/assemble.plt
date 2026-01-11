:- module(assemble_tests, []).

:- use_module(library(plunit)).
:- use_module(assembler/assemble).

:- begin_tests(assemble).

test(simple_words) :-
    tokenize("lit dup drop", Result),
    assertion(Result == ok([lit, dup, drop])).

test(words_with_numbers) :-
    tokenize("lit 100 dup", Result),
    assertion(Result == ok([lit, 100, dup])).

test(arithmetic_operators) :-
    tokenize("+ - * /", Result),
    assertion(Result == ok([+, -, *, /])).

test(stack_operations) :-
    tokenize(">r r> r@", Result),
    assertion(Result == ok(['>r', 'r>', 'r@'])).

test(memory_operations) :-
    tokenize("@ ! c@ c!", Result),
    assertion(Result == ok([@, !, 'c@', 'c!'])).

test(handles_newlines) :-
    tokenize("lit\n100\ndup", Result),
    assertion(Result == ok([lit, 100, dup])).

test(handles_tabs) :-
    tokenize("lit\t100\tdup", Result),
    assertion(Result == ok([lit, 100, dup])).

test(handles_mixed_whitespace) :-
    tokenize("lit  \n  100\t\ndup", Result),
    assertion(Result == ok([lit, 100, dup])).

test(handles_leading_whitespace) :-
    tokenize("   lit dup", Result),
    assertion(Result == ok([lit, dup])).

test(handles_trailing_whitespace) :-
    tokenize("lit dup   \n", Result),
    assertion(Result == ok([lit, dup])).

test(ignores_comments) :-
    tokenize("lit \\ this is a comment\n100", Result),
    assertion(Result == ok([lit, 100])).

test(handles_comment_at_end_of_line) :-
    tokenize("dup drop \\ cleanup", Result),
    assertion(Result == ok([dup, drop])).

test(handles_multiple_comments) :-
    tokenize("lit \\ push\n100 \\ value\ndup \\ duplicate", Result),
    assertion(Result == ok([lit, 100, dup])).

test(forth_program) :-
    tokenize("lit 100\ndup\n+\n", Result),
    assertion(Result == ok([lit, 100, dup, +])).

test(branching_program) :-
    tokenize("dup\n0branch\n10\ncall\nret", Result),
    assertion(Result == ok([dup, '0branch', 10, call, ret])).

test(memory_operations_program) :-
    tokenize("lit 1000\n@\nlit 42\n!", Result),
    assertion(Result == ok([lit, 1000, @, lit, 42, !])).

test(empty_input) :-
    tokenize("", Result),
    assertion(Result == ok([])).

test(whitespace_only_input) :-
    tokenize("   \n  \t  ", Result),
    assertion(Result == ok([])).

test(negative_numbers) :-
    tokenize("lit -42 +", Result),
    assertion(Result == ok([lit, -42, +])).

test(underscores_in_identifiers) :-
    tokenize("my_var test_fn", Result),
    assertion(Result == ok([my_var, test_fn])).

test(zero_byte) :-
    tokenize("nop lit 0 drop", Result),
    assertion(Result == ok([nop, lit, 0, drop])).

test(trap_instruction) :-
    tokenize("trap halt", Result),
    assertion(Result == ok([trap, halt])).

test(all_forth_ops) :-
    tokenize("lit @ ! c@ c! drop dup swap over >r r> r@ + - and or xor = < branch 0branch call ret execute trap", Result),
    assertion(Result == ok([lit, @, !, 'c@', 'c!', drop, dup, swap, over, '>r', 'r>', 'r@', +, -, and, or, xor, =, <, branch, '0branch', call, ret, execute, trap])).

:- end_tests(assemble).
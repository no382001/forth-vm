:- module(assemble, [tokenize/2]).
:- use_module(gen/gen).

tokenize(Source, Result) :-
    string_codes(Source, Codes),
    phrase(tokens(Result), Codes), !.

tokens(ok(Ts)) --> ws, tokens_list(Ts).

tokens_list([T|Ts]) --> word(T), !, ws, tokens_list(Ts).
tokens_list([]) --> [].

tokens_list(error(Msg, Ctx)) --> 
    [C], 
    { \+ char_type(C, space), atom_codes(A, [C]), format(atom(Msg), 'unexpected character: ~w', [A]) },
    context(Ctx).

ws --> [C], { char_type(C, space) }, !, ws.
ws --> skip_comment, !, ws.
ws --> [].

skip_comment --> [92], skip_to_newline.  % 92 is '\'

skip_to_newline --> [10], !.  % 10 is '\n'
skip_to_newline --> [_], skip_to_newline.
skip_to_newline --> [].       % EOF

word(W) --> 
    word_chars(Chars), 
    { Chars \= [], 
      (catch(atom_codes(A, Chars), _, fail),
       atom_number(A, Num) ->
        W = Num
      ;
        atom_codes(W, Chars)
      )
    }.

word_chars([34|Cs]) --> [34], !, quoted_then_rest(Cs).  % 34 is "
word_chars([C|Cs]) --> 
    [C], 
    { \+ char_type(C, space), 
      \+ char_type(C, white),
      C \= 92 },
    word_chars(Cs).
word_chars([]) --> [].

quoted_then_rest(Cs) -->
    quoted_content(QCs),
    [34],
    word_chars(Rest),
    { append(QCs, [34|Rest], Cs) }.

quoted_content([C|Cs]) --> [C], { C \= 34 }, !, quoted_content(Cs).
quoted_content([]) --> [].

context(Ctx) -->
    rest(Codes),
    { take(40, Codes, Prefix),
      string_codes(Ctx, Prefix) }.

rest(Codes, Codes, Codes).

take(N, List, Prefix) :-
    length(Prefix, N),
    append(Prefix, _, List), !.
take(_, List, List).
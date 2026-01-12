:- module(macro, [resolve/2]).

:- use_module(assemble).
:- use_module(gen/gen).

resolve(Source, Result) :-
    assemble:tokenize(Source, ok(Tokens)),
    phrase(parse_terms(Terms), Tokens),
    expand_macros(Terms, [], Expanded),
    
    % separate strings from code
    partition(is_string_def, Expanded, StringDefs, Code),
    
    % collect labels from code
    collect_labels(Code, 0, Labels),
    
    % figure out where strings go (after code) - IN BYTES!
    code_size(Code, TokenCount),
    gen:cell_size(CellSize),
    CodeBytes is TokenCount * CellSize,
    collect_strings(StringDefs, CodeBytes, StringMap, StringBytes),
    
    % resolve all references
    append(Labels, StringMap, AllRefs),
    maplist(resolve_ref(AllRefs), Code, NestedCode), !,
    append(NestedCode, ResolvedCode),
    
    % append string data as raw bytes
    append(ResolvedCode, StringBytes, Resolved),
    Result = ok(Resolved).

is_string_def(string_def(_, _)).

load_terms(File, Terms) :-
    read_file_to_string(File, Src, []),
    assemble:tokenize(Src, ok(Toks)),
    phrase(parse_terms(Terms), Toks).

% ============================================================
% parsing
% ============================================================

parse_terms([T|Ts]) --> parse_term(T), !, parse_terms(Ts).
parse_terms([]) --> [].

% string(name,"content")
parse_term(string_def(Name, Bytes)) -->
    [Tok],
    { atom_string(Tok, S),
      sub_string(S, 0, 7, _, "string("),
      parse_string_macro(S, Name, Bytes)
    }, !.

parse_term(include(File)) -->
    [Tok],
    { atom(Tok),
      atom_string(Tok, S),
      sub_string(S, 0, 8, _, "include("),
      sub_string(S, 8, _, 1, Inner),
      sub_string(Inner, 1, _, 1, File)
    }.

parse_term(macro_call(Name, Args)) -->
    [Tok],
    { atom_string(Tok, S), sub_string(S, _, _, _, "("),
      split_string(S, "(", ")", [N, A]),
      atom_string(Name, N),
      split_string(A, ",", " ", ArgStrs),
      maplist(parse_arg, ArgStrs, Args) }, !.

parse_term(Tok) --> [Tok].

parse_arg(Str, Num) :- number_string(Num, Str), !.
parse_arg(Str, Atom) :- atom_string(Atom, Str).

% TODO: I hate looking at a solution like this!
% parse string(name,"content") -> Name, Bytes (null-terminated)
parse_string_macro(S, Name, Bytes) :-
    sub_string(S, 7, _, 1, Inside),  % strip "string(" and ")"
    sub_string(Inside, Before, 1, _, ","),
    sub_string(Inside, 0, Before, _, NameStr),
    atom_string(Name, NameStr),
    Start is Before + 2,  % skip comma and opening quote
    sub_string(Inside, Start, _, 1, Content),  % strip closing quote
    string_codes(Content, ContentCodes),
    append(ContentCodes, [0], Bytes).  % null terminate

% ============================================================
% expand macros
% ============================================================

expand_macros([], _, []).

expand_macros([include(File)|T], Defs, Out) :-
    load_terms(File, Terms),
    expand_macros(Terms, Defs, IncOut),
    expand_macros(T, Defs, RestOut),
    append(IncOut, RestOut, Out).

expand_macros([macro_call(def, [N, V])|T], Defs, Out) :-
    expand_macros(T, [N-V|Defs], Out).

expand_macros([macro_call(subst, [N])|T], Defs, [V|Out]) :-
    member(N-V, Defs),
    expand_macros(T, Defs, Out).

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
collect_labels([macro_call(call, _)|T], PC, Labels) :-
    PC1 is PC + 1,
    collect_labels(T, PC1, Labels).
collect_labels([macro_call(addrofstr, _)|T], PC, Labels) :-
    PC1 is PC + 2,  % becomes: lit <addr>
    collect_labels(T, PC1, Labels).
collect_labels([X|T], PC, Labels) :-
    X \= macro_call(_, _),
    PC1 is PC + 1,
    collect_labels(T, PC1, Labels).

% ============================================================
% code size
% ============================================================

code_size([], 0).
code_size([macro_call(label, _)|T], N) :- code_size(T, N).
code_size([macro_call(branch, _)|T], N) :- code_size(T, N1), N is N1 + 2.
code_size([macro_call(zbranch, _)|T], N) :- code_size(T, N1), N is N1 + 2.
code_size([macro_call(call, _)|T], N) :- code_size(T, N1), N is N1 + 2.
code_size([macro_call(addrofstr, _)|T], N) :- code_size(T, N1), N is N1 + 2.
code_size([_|T], N) :- code_size(T, N1), N is N1 + 1.

% ============================================================
% collect strings
% ============================================================

collect_strings([], _, [], []).
collect_strings([string_def(Name, Bytes)|Rest], Offset, [Name-Offset|Map], AllBytes) :-
    length(Bytes, Len),
    NextOffset is Offset + Len,
    collect_strings(Rest, NextOffset, Map, RestBytes),
    maplist([B, byte(B)]>>true, Bytes, WrappedBytes),
    append(WrappedBytes, RestBytes, AllBytes).

% ============================================================
% resolve references
% ============================================================

resolve_ref(_, macro_call(label, _), []) :- !.
resolve_ref(Refs, macro_call(branch, [L]), [branch(Addr)]) :- member(L-Addr, Refs), !.
resolve_ref(Refs, macro_call(zbranch, [L]), [zbranch(Addr)]) :- member(L-Addr, Refs), !.
resolve_ref(Refs, macro_call(call, [L]), [call(Addr)]) :- member(L-Addr, Refs), !.
resolve_ref(Refs, macro_call(addrofstr, [N]), [lit, Addr]) :- member(N-Addr, Refs), !.
resolve_ref(_, X, [X]).
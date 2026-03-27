:- module(emit, [emit_binary/2]).

:- use_module(library(lists)).
:- use_module('../gen/gen').

%% Cell size in bytes (matches VM)
cell_size(S) :- gen:cell_size(S).

%% ============================================================
%% entry
%% ============================================================

emit_binary(Tokens, Bytes) :-
    collect_labels(Tokens, 0, Labels),
    encode_tokens(Tokens, Labels, Bytes).

%% ============================================================
%% pass 1: collect label -> byte offset mapping
%% ============================================================

collect_labels([], _, []).
collect_labels([label(Name) | Rest], Pos, [Name-Pos | Labels]) :-
    collect_labels(Rest, Pos, Labels).
collect_labels([Token | Rest], Pos, Labels) :-
    Token \= label(_),
    token_size(Token, Size),
    Pos1 is Pos + Size,
    collect_labels(Rest, Pos1, Labels).

%% ============================================================
%% token sizes (in bytes)
%% ============================================================

token_size(op(_), S)      :- cell_size(S).
token_size(lit(_), S)     :- cell_size(C), S is C * 2.     % opcode + value
token_size(call(_), S)    :- cell_size(C), S is C * 2.     % opcode + addr
token_size(branch(_), S)  :- cell_size(C), S is C * 2.     % opcode + addr
token_size(zbranch(_), S) :- cell_size(C), S is C * 2.     % opcode + addr
token_size(byte(_), 1).
token_size(label(_), 0).

%% ============================================================
%% pass 2: encode tokens to bytes
%% ============================================================

encode_tokens([], _, []).
encode_tokens([Token | Rest], Labels, Bytes) :-
    encode_token(Token, Labels, TBytes),
    encode_tokens(Rest, Labels, RestBytes),
    append(TBytes, RestBytes, Bytes).

%% label — emits nothing
encode_token(label(_), _, []).

%% literal: lit opcode + value
encode_token(lit(N), _, Bytes) :-
    opcode(lit, Op),
    cell_size(CS),
    encode_cell(CS, Op, OpBytes),
    encode_cell(CS, N, ValBytes),
    append(OpBytes, ValBytes, Bytes).

%% opcode
encode_token(op(Name), _, Bytes) :-
    opcode(Name, Op),
    cell_size(CS),
    encode_cell(CS, Op, Bytes).

%% call label
encode_token(call(Label), Labels, Bytes) :-
    opcode(call, Op),
    member(Label-Addr, Labels),
    cell_size(CS),
    encode_cell(CS, Op, OpBytes),
    encode_cell(CS, Addr, AddrBytes),
    append(OpBytes, AddrBytes, Bytes).

%% branch label
encode_token(branch(Label), Labels, Bytes) :-
    opcode(branch, Op),
    member(Label-Addr, Labels),
    cell_size(CS),
    encode_cell(CS, Op, OpBytes),
    encode_cell(CS, Addr, AddrBytes),
    append(OpBytes, AddrBytes, Bytes).

%% zbranch label
encode_token(zbranch(Label), Labels, Bytes) :-
    opcode('0branch', Op),
    member(Label-Addr, Labels),
    cell_size(CS),
    encode_cell(CS, Op, OpBytes),
    encode_cell(CS, Addr, AddrBytes),
    append(OpBytes, AddrBytes, Bytes).

%% raw byte
encode_token(byte(B), _, [B]).

%% ============================================================
%% cell encoding (little-endian)
%% ============================================================

encode_cell(0, _, []) :- !.
encode_cell(N, Value, [B | Bs]) :-
    N > 0,
    B is Value /\ 0xFF,
    Value1 is Value >> 8,
    N1 is N - 1,
    encode_cell(N1, Value1, Bs).

%% ============================================================
%% opcode lookup (from gen/gen.pl)
%% ============================================================

opcode(Name, Op) :- gen:op(Name, Op, _, _, _, _).

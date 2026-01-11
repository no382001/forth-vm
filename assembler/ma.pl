#!/usr/bin/env swipl

:- initialization(main, main).

:- use_module(macro).
:- use_module(gen/gen).

main(Argv) :-
    ( parse_args(Argv, Options) ->
        run(Options)
    ;
        format(user_error, "failed to parse arguments~n", []),
        halt(1)
    ).

% ------------------------------
% argument parsing
% ------------------------------

parse_args([], Options) :-
    defaults(Options).
parse_args([H|_], _) :-
    member(H, ['-h', '--help']),
    help,
    halt(0).
parse_args(['-i', File|Rest], [input(file(File))|Options]) :-
    parse_args(Rest, Options).
parse_args(['-o', File|Rest], [output(file(File))|Options]) :-
    parse_args(Rest, Options).
parse_args(['--tokens'|Rest], [target(tokens)|Options]) :-
    parse_args(Rest, Options).
parse_args(['--resolved'|Rest], [target(resolved)|Options]) :-
    parse_args(Rest, Options).
parse_args(['-b'|Rest], [target(binary)|Options]) :-
    parse_args(Rest, Options).
parse_args(['--binary'|Rest], [target(binary)|Options]) :-
    parse_args(Rest, Options).
parse_args([Arg|_], _) :-
    format(user_error, "unknown argument: ~w~n", [Arg]),
    help,
    halt(1).

defaults([input(stdin), output(stdout), target(resolved)]).

help :-
    format("usage: ma.pl [-i FILE] [-o FILE] [--tokens|--resolved|-b]~n"),
    format("~n"),
    format("options:~n"),
    format("  -i FILE      input file (default: stdin)~n"),
    format("  -o FILE      output file (default: stdout)~n"),
    format("  --tokens     output after tokenization~n"),
    format("  --resolved   output after macro resolution (default)~n"),
    format("  -b, --binary output assembled binary~n"),
    format("  -h, --help   show this help~n").

% ------------------------------
% option access
% ------------------------------

option(Options, Key, Value) :-
    Term =.. [Key, Value],
    member(Term, Options), !.
option(_, input, stdin).
option(_, output, stdout).
option(_, target, resolved).

% ------------------------------
% run
% ------------------------------

run(Options) :-
    option(Options, input, Input),
    option(Options, output, Output),
    option(Options, target, Target),
    read_input(Input, Source),
    process(Target, Source, Result),
    handle_result(Result, Target, Output).

read_input(stdin, Source) :-
    read_string(user_input, _, Source).
read_input(file(File), Source) :-
    read_file_to_string(File, Source, []).

process(tokens, Source, Result) :-
    assemble:tokenize(Source, Result).
process(resolved, Source, Result) :-
    macro:resolve(Source, Result).
process(binary, Source, Result) :-
    macro:resolve(Source, Resolved),
    ( Resolved = ok(Code) ->
        assemble_binary(Code, Result)
    ;
        Result = Resolved
    ).

assemble_binary(Code, ok(Bytes)) :-
    gen:cell_size(CellSize),
    build_offset_map(Code, CellSize, 0, OffsetMap),
    maplist(encode_token(CellSize, OffsetMap), Code, BytesList),
    append(BytesList, Bytes).

% Build a map from token index to byte offset
build_offset_map([], _, _, []).
build_offset_map([Token|Rest], CellSize, BytePos, [BytePos|RestMap]) :-
    token_size(Token, CellSize, Size),
    NextPos is BytePos + Size,
    build_offset_map(Rest, CellSize, NextPos, RestMap).

token_size(branch(_), CellSize, Size) :- Size is CellSize * 2.
token_size(zbranch(_), CellSize, Size) :- Size is CellSize * 2.
token_size(call(_), CellSize, Size) :- Size is CellSize * 2.
token_size(Token, CellSize, CellSize) :-
    Token \= branch(_),
    Token \= zbranch(_),
    Token \= call(_).

encode_token(CellSize, OffsetMap, Token, Bytes) :-
    ( Token = branch(Idx) ->
        gen:op(branch, Op, _, _, _, _),
        encode_cell(CellSize, Op, OpBytes),
        nth0(Idx, OffsetMap, ByteAddr),
        encode_cell(CellSize, ByteAddr, AddrBytes),
        append(OpBytes, AddrBytes, Bytes)
    ; Token = zbranch(Idx) ->
        gen:op('0branch', Op, _, _, _, _),
        encode_cell(CellSize, Op, OpBytes),
        nth0(Idx, OffsetMap, ByteAddr),
        encode_cell(CellSize, ByteAddr, AddrBytes),
        append(OpBytes, AddrBytes, Bytes)
    ; Token = call(Idx) ->
        gen:op(call, Op, _, _, _, _),
        encode_cell(CellSize, Op, OpBytes),
        nth0(Idx, OffsetMap, ByteAddr),
        encode_cell(CellSize, ByteAddr, AddrBytes),
        append(OpBytes, AddrBytes, Bytes)
    ; number(Token) ->
        encode_cell(CellSize, Token, Bytes)
    ; gen:op(Token, Op, _, _, _, _) ->
        encode_cell(CellSize, Op, Bytes)
    ;
        format(user_error, "unknown token: ~w~n", [Token]),
        halt(1)
    ).

encode_cell(CellSize, Value, Bytes) :-
    encode_cell_(CellSize, Value, Bytes).

encode_cell_(0, _, []) :- !.
encode_cell_(N, Value, [B|Bs]) :-
    B is Value /\ 0xFF,
    Value1 is Value >> 8,
    N1 is N - 1,
    encode_cell_(N1, Value1, Bs).

handle_result(ok(Data), Target, Output) :-
    write_output(Target, Data, Output).
handle_result(error(Stage, Error), _, _) :-
    format(user_error, "error in ~w: ~w~n", [Stage, Error]),
    halt(1).
handle_result(error(Error), _, _) :-
    format(user_error, "error: ~w~n", [Error]),
    halt(1).

write_output(binary, Bytes, stdout) :-
    set_stream(user_output, type(binary)),
    maplist(put_byte, Bytes).
write_output(binary, Bytes, file(File)) :-
    open(File, write, Stream, [type(binary)]),
    maplist(put_byte(Stream), Bytes),
    close(Stream).
write_output(Target, Data, stdout) :-
    Target \= binary,
    format("~w~n", [Data]).
write_output(Target, Data, file(File)) :-
    Target \= binary,
    open(File, write, Stream),
    format(Stream, "~w~n", [Data]),
    close(Stream).
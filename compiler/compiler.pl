:- module(compiler, [compile_source/2, compile_source/3]).

:- use_module(parser).
:- use_module(ast).
:- use_module(typecheck).
:- use_module(codegen).
:- use_module(emit).

:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(charsio)).

%% compile_source(+Source, -Result)
%% Result = ok(Bytes) | error(Stage, Detail)
compile_source(Source, Result) :-
    compile_source(Source, binary, Result).

%% compile_source(+Source, +Target, -Result)
%% Target = ast | typed | ir | binary
compile_source(Source, Target, Result) :-
    %% Stage 1: parse
    parser:parse(Source, ParseResult),
    ( ParseResult = ok(Forms) -> true
    ; Result = error(parse, ParseResult), !
    ),
    ( Target = parsed -> Result = ok(Forms), ! ; true ),

    %% Stage 2: AST transform
    ast:transform_program(Forms, AstResult),
    ( AstResult = ok(Defs) -> true
    ; Result = error(ast, AstResult), !
    ),
    ( Target = ast -> Result = ok(Defs), ! ; true ),

    %% Stage 3: typecheck
    typecheck:check_program(Defs, TcResult),
    ( TcResult = ok(TypedDefs) -> true
    ; Result = error(typecheck, TcResult), !
    ),
    ( Target = typed -> Result = ok(TypedDefs), ! ; true ),

    %% Stage 4: codegen
    codegen:compile_program(TypedDefs, CgResult),
    ( CgResult = ok(Tokens) -> true
    ; Result = error(codegen, CgResult), !
    ),
    ( Target = ir -> Result = ok(Tokens), ! ; true ),

    %% Stage 5: emit binary
    %% First, add entry point: branch to main then halt
    ( member(label(main), Tokens) ->
        Entry = [branch(main)],
        append(Entry, Tokens, AllTokens)
    ;
        AllTokens = Tokens
    ),
    emit:emit_binary(AllTokens, Bytes),
    Result = ok(Bytes).

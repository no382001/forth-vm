:- module(compiler, [compile_source/2, compile_source/3, compile_file/2]).

:- use_module(parser).
:- use_module(ast).
:- use_module(typecheck).
:- use_module(codegen).
:- use_module(emit).

:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(charsio)).
:- use_module(library(iso_ext)).

%% compile_source(+Source, -Result)
%% Result = ok(Bytes) | error(Stage, Detail)
compile_source(Source, Result) :-
    compile_source(Source, binary, Result).

%% compile_source(+Source, +Target, -Result)
%% Target = parsed | ast | typed | ir | binary
compile_source(Source, Target, Result) :-
    %% Stage 1: parse
    parser:parse(Source, ParseResult),
    ( ParseResult \= ok(_) ->
        Result = error(parse, ParseResult)
    ; ParseResult = ok(Forms),
      ( Target = parsed ->
          Result = ok(Forms)
      ;
          %% Stage 2: AST transform
          ast:transform_program(Forms, AstResult),
          ( AstResult \= ok(_) ->
              Result = error(ast, AstResult)
          ; AstResult = ok(Defs),
            ( Target = ast ->
                Result = ok(Defs)
            ;
                %% Stage 3: typecheck
                typecheck:check_program(Defs, TcResult),
                ( TcResult \= ok(_) ->
                    Result = error(typecheck, TcResult)
                ; TcResult = ok(TypedDefs),
                  ( Target = typed ->
                      Result = ok(TypedDefs)
                  ;
                      %% Stage 4: codegen
                      codegen:compile_program(TypedDefs, CgResult),
                      ( CgResult \= ok(_) ->
                          Result = error(codegen, CgResult)
                      ; CgResult = ok(Tokens),
                        ( Target = ir ->
                            Result = ok(Tokens)
                        ;
                            %% Stage 5: emit binary
                            ( member(label(main), Tokens) ->
                                append([branch(main)], Tokens, AllTokens)
                            ;
                                AllTokens = Tokens
                            ),
                            emit:emit_binary(AllTokens, Bytes),
                            Result = ok(Bytes)
                        )
                      )
                  )
                )
            )
          )
      )
    ).

%% ============================================================
%% file compilation
%% ============================================================

compile_file(InFile, OutFile) :-
    read_source(InFile, Chars),
    compile_source(Chars, binary, Result),
    ( Result = ok(Bytes) ->
        write_binary(OutFile, Bytes)
    ;
        format("compile error: ~w~n", [Result]),
        halt(1)
    ).

read_source(File, Chars) :-
    open(File, read, S),
    get_chars(S, Chars),
    close(S).

get_chars(S, Chars) :-
    get_char(S, C),
    ( C = end_of_file ->
        Chars = []
    ;
        Chars = [C|Rest],
        get_chars(S, Rest)
    ).

write_binary(File, Bytes) :-
    open(File, write, S, [type(binary)]),
    maplist(put_byte(S), Bytes),
    close(S).

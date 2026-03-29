:- module(compiler, [compile_source/2, compile_source/3, compile_file/2]).
:- use_module(library(between)).

:- use_module(parser).
:- use_module(ast).
:- use_module(typecheck).
:- use_module(codegen).
:- use_module(emit).
:- use_module(effects).

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
      %% Stage 1.5: meta-expansion ($section, $alloc, $vm-sp, etc.)
      expand_metas(Forms, Expanded),
      ( Target = parsed ->
          Result = ok(Expanded)
      ;
          compile_from_forms(Expanded, Target, Result)
      )
    ).

%% ============================================================
%% file compilation
%% ============================================================

compile_file(InFile, OutFile) :-
    file_directory(InFile, BaseDir),
    read_source(InFile, Chars),
    compile_source_with_includes(Chars, BaseDir, binary, Result),
    ( Result = ok(Bytes) ->
        write_binary(OutFile, Bytes)
    ;
        format("compile error: ~w~n", [Result]),
        halt(1)
    ).

%% compile with include expansion
compile_source_with_includes(Source, BaseDir, Target, Result) :-
    parser:parse(Source, ParseResult),
    ( ParseResult \= ok(_) ->
        Result = error(parse, ParseResult)
    ; ParseResult = ok(Forms),
      expand_includes(Forms, BaseDir, IncExpanded),
      expand_metas(IncExpanded, Expanded),
      ( Target = parsed ->
          Result = ok(Expanded)
      ;
          compile_from_forms(Expanded, Target, Result)
      )
    ).

%% compile pipeline from already-parsed (and include-expanded) forms
compile_from_forms(Forms, Target, Result) :-
    ast:transform_program(Forms, AstResult),
    ( AstResult \= ok(_) ->
        Result = error(ast, AstResult)
    ; AstResult = ok(Defs),
      ( Target = ast ->
          Result = ok(Defs)
      ;
          typecheck:check_program(Defs, TcResult),
          ( TcResult \= ok(_) ->
              Result = error(typecheck, TcResult)
          ; TcResult = ok(TypedDefs),
            ( Target = typed ->
                Result = ok(TypedDefs)
            ;
                %% Stage 3.5: effect inference
                effects:infer_effects(TypedDefs, EffectEnv),
                ( Target = effects ->
                    Result = ok(EffectEnv)
                ;
                codegen:compile_program(TypedDefs, CgResult),
                ( CgResult \= ok(_) ->
                    Result = error(codegen, CgResult)
                ; CgResult = ok(Tokens),
                  ( Target = ir ->
                      Result = ok(Tokens)
                  ;
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
    ).

%% ============================================================
%% include expansion
%% ============================================================

expand_includes([], _, []).
expand_includes([list([sym(include), str(File)])|Rest], BaseDir, Expanded) :-
    !,
    atom_chars(BaseDir, BaseDirChars),
    append(BaseDirChars, File, FullPathChars),
    atom_chars(FullPath, FullPathChars),
    read_source(FullPath, IncChars),
    parser:parse(IncChars, IncResult),
    ( IncResult = ok(IncForms) ->
        file_directory(FullPath, IncDir),
        expand_includes(IncForms, IncDir, ExpandedInc),
        expand_includes(Rest, BaseDir, ExpandedRest),
        append(ExpandedInc, ExpandedRest, Expanded)
    ;
        format("include error: ~w: ~w~n", [File, IncResult]),
        halt(1)
    ).
expand_includes([F|Rest], BaseDir, [F|ExpandedRest]) :-
    expand_includes(Rest, BaseDir, ExpandedRest).

%% ============================================================
%% meta-expansion: $vm-sp, $vm-rp, $vm-ip, $cell, $mem,
%%                 ($section addr), ($alloc name size)
%% ============================================================

%% VM layout constants (must match vm.h)
meta_const('$mem',  65535).  %% MEMORY_SIZE
meta_const('$cell', 2).     %% CELL_SIZE
meta_const('$stack-size', 256).
meta_const('$ds-start', DS) :- DS is 65535 - (256 * 2 * 2).
meta_const('$rs-start', RS) :- DS is 65535 - (256 * 2 * 2), RS is DS + (256 * 2).
meta_const('$vm-sp', A)  :- DS is 65535 - (256 * 2 * 2), A is DS - 6.
meta_const('$vm-rp', A)  :- DS is 65535 - (256 * 2 * 2), A is DS - 4.
meta_const('$vm-ip', A)  :- DS is 65535 - (256 * 2 * 2), A is DS - 2.

%% expand_metas(+Forms, -Expanded)
%% Process top-level forms, expanding $-prefixed meta directives
expand_metas(Forms, Expanded) :-
    expand_metas_(Forms, 0, Expanded).

%% expand_metas_(+Forms, +AllocPtr, -Expanded)
expand_metas_([], _, []).

%% ($section addr) — set allocation pointer
expand_metas_([list([sym('$section'), num(Addr)])|Rest], _, Expanded) :-
    !,
    expand_metas_(Rest, Addr, Expanded).

%% ($alloc name size) — allocate and emit const
expand_metas_([list([sym('$alloc'), sym(Name), num(Size)])|Rest], Ptr, [Const|Expanded]) :-
    !,
    Const = list([sym(const), sym(Name), sym(int), num(Ptr)]),
    NextPtr is Ptr + Size,
    expand_metas_(Rest, NextPtr, Expanded).

%% any other form — recursively expand meta-expressions inside it
expand_metas_([Form|Rest], Ptr, [Expanded|ExpandedRest]) :-
    expand_meta_expr(Form, Expanded),
    expand_metas_(Rest, Ptr, ExpandedRest).

%% expand_meta_expr: replace ($vm-sp) etc. with num(N) inside any form
expand_meta_expr(list([sym(Name)]), num(Val)) :-
    meta_const(Name, Val), !.
expand_meta_expr(list(Elems), list(ExpandedElems)) :-
    !, maplist(expand_meta_expr, Elems, ExpandedElems).
expand_meta_expr(X, X).

file_directory(Path, Dir) :-
    atom_chars(Path, Chars),
    reverse(Chars, Rev),
    ( append(_, ['/'|DirRev], Rev) ->
        reverse(['/'|DirRev], DirChars),
        atom_chars(Dir, DirChars)
    ;
        Dir = './'
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

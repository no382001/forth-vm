:- module(compiler, [compile_source/2, compile_source/3, compile_file/2]).
:- use_module(library(between)).

:- use_module(parser).
:- use_module(ast).
:- use_module(typecheck).
:- use_module(codegen).
:- use_module(emit).
:- use_module(effects).
:- use_module(deadcode).
:- use_module(constfold).

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
      compute_def_lines(Source, DefLines),
      ( Target = parsed ->
          Result = ok(Expanded)
      ;
          compile_from_forms(Expanded, Target, DefLines, Result)
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
      compute_def_lines(Source, MainDefLines),
      expand_includes(Forms, BaseDir, IncExpanded, IncDefLines),
      append(MainDefLines, IncDefLines, DefLines),
      expand_metas(IncExpanded, Expanded),
      ( Target = parsed ->
          Result = ok(Expanded)
      ;
          compile_from_forms(Expanded, Target, DefLines, Result)
      )
    ).

%% compile pipeline from already-parsed (and include-expanded) forms
compile_from_forms(Forms, Target, DefLines, Result) :-
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
                effects:check_annotations(TypedDefs, EffectEnv, DefLines, EffErrors),
                ( EffErrors \= [] ->
                    format_effect_errors(EffErrors),
                    Result = error(effects, EffErrors)
                ;
                %% Stage 3.6: dead code warnings (non-fatal, to stderr)
                deadcode:find_dead_code(TypedDefs, DeadNames),
                warn_dead_code(DeadNames),
                %% Stage 3.7: constant folding for det functions
                constfold:fold_constants(TypedDefs, EffectEnv, FoldedDefs),
                codegen:compile_program(FoldedDefs, CgResult),
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
      )
    ).

%% ============================================================
%% include expansion
%% ============================================================

expand_includes([], _, [], []).
expand_includes([list([sym(include), str(File)])|Rest], BaseDir, Expanded, DefLines) :-
    !,
    atom_chars(BaseDir, BaseDirChars),
    append(BaseDirChars, File, FullPathChars),
    atom_chars(FullPath, FullPathChars),
    read_source(FullPath, IncChars),
    parser:parse(IncChars, IncResult),
    ( IncResult = ok(IncForms) ->
        atom_chars(FileAtom, File),
        compute_def_lines_file(IncChars, FileAtom, FileDefLines),
        file_directory(FullPath, IncDir),
        expand_includes(IncForms, IncDir, ExpandedInc, IncDefLines),
        expand_includes(Rest, BaseDir, ExpandedRest, RestDefLines),
        append(ExpandedInc, ExpandedRest, Expanded),
        append(FileDefLines, IncDefLines, DL1),
        append(DL1, RestDefLines, DefLines)
    ;
        format("include error: ~w: ~w~n", [File, IncResult]),
        halt(1)
    ).
expand_includes([F|Rest], BaseDir, [F|ExpandedRest], DefLines) :-
    expand_includes(Rest, BaseDir, ExpandedRest, DefLines).

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

%% ANSI color helpers
esc_code(Codes) :- char_code(Esc, 27), Codes = [Esc, '['].

ansi_bold(Text, Colored) :-
    esc_code(E), append(E, "1m", Pre),
    esc_code(E2), append(E2, "0m", Post),
    append(Pre, Text, P1), append(P1, Post, Colored).
ansi_red(Text, Colored) :-
    esc_code(E), append(E, "1;31m", Pre),
    esc_code(E2), append(E2, "0m", Post),
    append(Pre, Text, P1), append(P1, Post, Colored).
ansi_yellow(Text, Colored) :-
    esc_code(E), append(E, "1;35m", Pre),
    esc_code(E2), append(E2, "0m", Post),
    append(Pre, Text, P1), append(P1, Post, Colored).

%% dead code warnings to stderr
warn_dead_code([]).
warn_dead_code([Name|Rest]) :-
    atom_chars(Name, NameChars),
    ansi_yellow("warning:", WarnTag),
    ansi_bold(NameChars, BoldName),
    append(WarnTag, " unused function '", P1),
    append(P1, BoldName, P2),
    append(P2, "'\n", Msg),
    write_stderr(Msg),
    warn_dead_code(Rest).

%% format effect errors to stdout
format_effect_errors([]).
format_effect_errors([effect_mismatch(Name, Decl, Inferred, Loc)|Rest]) :-
    atom_chars(Name, NameCs),
    atom_chars(Decl, DeclCs),
    atom_chars(Inferred, InfCs),
    format_loc(Loc, LocCs),
    ansi_bold(LocCs, BoldLoc),
    ansi_red("error:", ErrTag),
    ansi_bold(NameCs, BoldName),
    append(BoldLoc, ErrTag, P1),
    append(P1, " '", P2),
    append(P2, BoldName, P3),
    append(P3, "' declared [", P4),
    append(P4, DeclCs, P5),
    append(P5, "] but inferred ", P6),
    append(P6, InfCs, P7),
    append(P7, "\n", Msg),
    maplist(put_char, Msg),
    format_effect_errors(Rest).

format_loc(loc(L, C), Cs) :-
    number_chars(L, LCs),
    number_chars(C, CCs),
    append(LCs, ":", P1),
    append(P1, CCs, P2),
    append(P2, ": ", Cs).
format_loc(loc(File, L, C), Cs) :-
    atom_chars(File, FCs),
    number_chars(L, LCs),
    number_chars(C, CCs),
    append(FCs, ":", P1),
    append(P1, LCs, P2),
    append(P2, ":", P3),
    append(P3, CCs, P4),
    append(P4, ": ", Cs).
format_loc(unknown, "").

write_stderr(Msg) :-
    current_output(StdOut),
    open('/dev/stderr', write, Err),
    set_output(Err),
    maplist(put_char, Msg),
    close(Err),
    set_output(StdOut).

read_source(File, Chars) :-
    open(File, read, S),
    get_chars(S, Chars),
    close(S).

%% compute_def_lines(+SourceChars, -Map)
%% Map = [Name-loc(Line,Col), ...] for each (def Name ...) found in source.
compute_def_lines(Source, Map) :-
    phrase(def_lines_(1, 1, Map), Source).

%% compute_def_lines_file(+SourceChars, +FileName, -Map)
%% Map = [Name-loc(File,Line,Col), ...] with filename.
compute_def_lines_file(Source, File, Map) :-
    phrase(def_lines_file_(File, 1, 1, Map), Source).

def_lines_file_(F, L, _, Map) --> ['\n'], !, { L1 is L+1 },
    def_lines_file_(F, L1, 1, Map).
def_lines_file_(F, L, Col, Map) --> [';'], !, skip_comment,
    def_lines_file_(F, L, Col, Map).
def_lines_file_(F, L, Col, [Name-loc(F,L,Col)|Map]) -->
    "(def ", !, scan_def_name(NameCs),
    { atom_chars(Name, NameCs),
      length(['(','d','e','f',' '|NameCs], Skip),
      Col1 is Col + Skip },
    def_lines_file_(F, L, Col1, Map).
def_lines_file_(F, L, Col, Map) --> [_], !, { Col1 is Col+1 },
    def_lines_file_(F, L, Col1, Map).
def_lines_file_(_, _, _, []) --> [].

def_lines_(L, _, Map) --> ['\n'], !, { L1 is L+1 },
    def_lines_(L1, 1, Map).
def_lines_(L, Col, Map) --> [';'], !, skip_comment,
    def_lines_(L, Col, Map).
def_lines_(L, Col, [Name-loc(L,Col)|Map]) -->
    "(def ", !, scan_def_name(NameCs),
    { atom_chars(Name, NameCs),
      length(['(','d','e','f',' '|NameCs], Skip),
      Col1 is Col + Skip },
    def_lines_(L, Col1, Map).
def_lines_(L, Col, Map) --> [_], !, { Col1 is Col+1 },
    def_lines_(L, Col1, Map).
def_lines_(_, _, []) --> [].

scan_def_name([C|Rest]) --> [C],
    { C \= ' ', C \= '\n', C \= '\t', C \= '(', C \= ')' }, !,
    scan_def_name(Rest).
scan_def_name([]) --> [].

skip_comment --> [C], { C \= '\n' }, !, skip_comment.
skip_comment --> [].

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

:- module(codegen, [compile_program/2]).

:- use_module(library(lists)).

%% ============================================================
%% entry
%% ============================================================

compile_program(Defs, ok(Code)) :-
    collect_consts(Defs, Consts),
    compile_defs(Defs, Consts, 0, 256, Code).

%% ============================================================
%% collect top-level constants for inlining
%% ============================================================

collect_consts([], []).
collect_consts([const(Name, _, num(V)) | Rest], [const(Name, V) | Cs]) :-
    collect_consts(Rest, Cs).
collect_consts([_ | Rest], Cs) :-
    collect_consts(Rest, Cs).

%% ============================================================
%% compile definitions
%% ============================================================

compile_defs([], _, _, _, []).
compile_defs([Def | Rest], Consts, LN0, Slot0, Code) :-
    compile_def(Def, Consts, LN0, Slot0, DefCode, LN1, Slot1),
    compile_defs(Rest, Consts, LN1, Slot1, RestCode),
    append(DefCode, RestCode, Code).

compile_def(extern(_, _, _), _, LN, Slot, [], LN, Slot).
compile_def(const(_, _, _), _, LN, Slot, [], LN, Slot).

compile_def(def(Name, Params, _RetType, Body), Consts, LN0, Slot0, Code, LN, SlotAfter) :-
    %% Allocate slots for params
    alloc_params(Params, Slot0, Env, SlotParams),
    %% Generate prologue: pop args from stack into memory slots
    %% Args arrive on stack: rightmost on top (Forth convention)
    %% So we pop in param order (last param first from stack)
    reverse(Env, RevEnv),
    prologue(RevEnv, PrologueCode),
    %% Compile body
    compile_body(Body, Env, Consts, LN0, BodyCode, LN),
    %% Assemble: label + prologue + body + ret
    append(PrologueCode, BodyCode, InnerCode),
    append([label(Name) | InnerCode], [op(ret)], Code),
    %% next function can reuse slots (no recursion)
    SlotAfter = SlotParams.

alloc_params([], Slot, [], Slot).
alloc_params([param(Name, _Type) | Rest], Slot, [var(Name, Slot) | Env], SlotOut) :-
    Slot1 is Slot + 2,
    alloc_params(Rest, Slot1, Env, SlotOut).

prologue([], []).
prologue([var(_, Addr) | Rest], Code) :-
    prologue(Rest, RestCode),
    append([lit(Addr), op('!')], RestCode, Code).

%% ============================================================
%% body: list of exprs; middle ones drop result, last keeps it
%% ============================================================

compile_body([], _, _, LN, [], LN).
compile_body([E], Env, Consts, LN0, Code, LN) :-
    compile_expr(E, Env, Consts, LN0, Code, LN).
compile_body([E | Rest], Env, Consts, LN0, Code, LN) :-
    Rest = [_|_],
    compile_expr(E, Env, Consts, LN0, ECode, LN1),
    ( void_expr(E) -> DropCode = [] ; DropCode = [op(drop)] ),
    compile_body(Rest, Env, Consts, LN1, RestCode, LN),
    append(ECode, DropCode, ECodeD),
    append(ECodeD, RestCode, Code).

void_expr(store(_, _)).
void_expr(store8(_, _)).
void_expr(while(_, _)).
void_expr(call(Name, _)) :- builtin_trap(Name, void, _).

%% ============================================================
%% expressions
%% ============================================================

%% literal
compile_expr(num(N), _, _, LN, [lit(N)], LN).

%% variable: load from memory slot
compile_expr(var(Name), Env, Consts, LN, Code, LN) :-
    ( member(var(Name, Addr), Env) ->
        Code = [lit(Addr), op('@')]
    ; member(const(Name, V), Consts) ->
        Code = [lit(V)]
    ).

%% binop
compile_expr(binop(Op, A, B), Env, Consts, LN0, Code, LN) :-
    compile_expr(A, Env, Consts, LN0, ACode, LN1),
    compile_expr(B, Env, Consts, LN1, BCode, LN),
    op_to_vm(Op, VmOps),
    append(ACode, BCode, ABCode),
    append(ABCode, VmOps, Code).

%% if
compile_expr(if(Cond, Then, Else), Env, Consts, LN0, Code, LN) :-
    genlabel(LN0, "_else_", ElseL, LN1),
    genlabel(LN1, "_endif_", EndL, LN2),
    compile_expr(Cond, Env, Consts, LN2, CC, LN3),
    compile_expr(Then, Env, Consts, LN3, TC, LN4),
    compile_expr(Else, Env, Consts, LN4, EC, LN),
    append(CC, [zbranch(ElseL)], C1),
    append(C1, TC, C2),
    append(C2, [branch(EndL), label(ElseL)], C3),
    append(C3, EC, C4),
    append(C4, [label(EndL)], Code).

%% let
compile_expr(let(Bindings, Body), Env, Consts, LN0, Code, LN) :-
    compile_let(Bindings, Env, Consts, LN0, BindCode, ExtEnv, LN1),
    compile_body(Body, ExtEnv, Consts, LN1, BodyCode, LN),
    append(BindCode, BodyCode, Code).

%% do
compile_expr(do(Exprs), Env, Consts, LN0, Code, LN) :-
    compile_body(Exprs, Env, Consts, LN0, Code, LN).

%% while
compile_expr(while(Cond, Body), Env, Consts, LN0, Code, LN) :-
    genlabel(LN0, "_wstart_", StartL, LN1),
    genlabel(LN1, "_wend_", EndL, LN2),
    compile_expr(Cond, Env, Consts, LN2, CC, LN3),
    compile_body(Body, Env, Consts, LN3, BC, LN),
    append([label(StartL) | CC], [zbranch(EndL)], C1),
    append(C1, BC, C2),
    append(C2, [branch(StartL), label(EndL)], Code).

%% deref
compile_expr(deref(E), Env, Consts, LN0, Code, LN) :-
    compile_expr(E, Env, Consts, LN0, EC, LN),
    append(EC, [op('@')], Code).

%% store
compile_expr(store(Addr, Val), Env, Consts, LN0, Code, LN) :-
    compile_expr(Val, Env, Consts, LN0, VC, LN1),
    compile_expr(Addr, Env, Consts, LN1, AC, LN),
    %% stack now: [... val addr], ! expects (val addr --)
    append(VC, AC, C1),
    append(C1, [op('!')], Code).

%% store8
compile_expr(store8(Addr, Val), Env, Consts, LN0, Code, LN) :-
    compile_expr(Val, Env, Consts, LN0, VC, LN1),
    compile_expr(Addr, Env, Consts, LN1, AC, LN),
    append(VC, AC, C1),
    append(C1, [op('c!')], Code).

%% function call
compile_expr(call(Name, Args), Env, Consts, LN0, Code, LN) :-
    compile_args(Args, Env, Consts, LN0, ArgsCode, LN),
    ( builtin_trap(Name, _, TrapCode) ->
        append(ArgsCode, TrapCode, Code)
    ;
        append(ArgsCode, [call(Name)], Code)
    ).

%% ============================================================
%% helpers
%% ============================================================

compile_args([], _, _, LN, [], LN).
compile_args([A | As], Env, Consts, LN0, Code, LN) :-
    compile_expr(A, Env, Consts, LN0, AC, LN1),
    compile_args(As, Env, Consts, LN1, RestCode, LN),
    append(AC, RestCode, Code).

compile_let([], Env, _, LN, [], Env, LN).
compile_let([bind(Name, Expr) | Rest], Env, Consts, LN0, Code, ExtEnv, LN) :-
    compile_expr(Expr, Env, Consts, LN0, ExprCode, LN1),
    %% Allocate a new slot: find max addr in Env + 2
    max_addr(Env, MaxAddr),
    Addr is MaxAddr + 2,
    StoreCode = [lit(Addr), op('!')],
    NewEnv = [var(Name, Addr) | Env],
    compile_let(Rest, NewEnv, Consts, LN1, RestCode, ExtEnv, LN),
    append(ExprCode, StoreCode, BindCode),
    append(BindCode, RestCode, Code).

max_addr([], 254).  % base - 2, so first alloc = 256
max_addr([var(_, A) | Rest], Max) :-
    max_addr(Rest, RestMax),
    ( A > RestMax -> Max = A ; Max = RestMax ).

genlabel(N, Prefix, Label, N1) :-
    N1 is N + 1,
    number_chars(N, NChars),
    atom_chars(PrefixAtom, Prefix),
    atom_chars(PrefixAtom, PrefixChars),
    append(PrefixChars, NChars, LabelChars),
    atom_chars(Label, LabelChars).

%% Map source ops to VM instruction sequences
op_to_vm(+, [op('+')]).
op_to_vm(-, [op('-')]).
op_to_vm('and', [op('and')]).
op_to_vm('or', [op('or')]).
op_to_vm('xor', [op('xor')]).
op_to_vm(=, [op('=')]).
op_to_vm(<, [op('<')]).
%% > : swap then <
op_to_vm(>, [op(swap), op('<')]).
%% != : = then logical invert (0= -> swap truth)
%% invert bool: lit 0 =  (if TOS was 0 -> -1, if TOS was -1 -> 0)
op_to_vm('!=', [op('='), lit(0), op('=')]).
%% <= : > then invert
op_to_vm('<=', [op(swap), op('<'), lit(0), op('=')]).
%% >= : < then invert
op_to_vm('>=', [op('<'), lit(0), op('=')]).

%% Built-in trap functions: name, return_type, code
builtin_trap(emit, void, [lit(0), op(trap)]).
builtin_trap(key, int, [lit(1), op(trap)]).
builtin_trap(bye, void, [lit(2), op(trap)]).
builtin_trap('assert-fail', void, [lit(3), op(trap)]).

%% ============================================================
%% tests
%% ============================================================

:- use_module(parser).
:- use_module(ast).
:- use_module(typecheck).

codegen_pipeline(Src, Result) :-
    parse(Src, ok(Forms)),
    transform_program(Forms, ok(Defs)),
    check_program(Defs, ok(_)),
    compile_program(Defs, Result).

?- codegen_pipeline("(def main () : void (emit 65) (bye))", ok(_)).
   true.

?- codegen_pipeline("(def main () : void (emit (+ 60 5)) (bye))", ok(_)).
   true.

?- codegen_pipeline("(def main () : void (emit (if (< 1 2) 65 66)) (bye))", ok(_)).
   true.

?- codegen_pipeline("(def main () : void (let ((x 65)) (emit x)) (bye))", ok(_)).
   true.

?- codegen_pipeline("(def add1 ((n : int)) : int (+ n 1)) (def main () : void (emit (add1 64)) (bye))", ok(Tokens)),
   member(label(main), Tokens), member(label(add1), Tokens).
   true.

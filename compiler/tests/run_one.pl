:- module(test_run, [go/0]).
:- use_module('../parser').
:- use_module('../ast').
:- use_module('../typecheck').
:- use_module('../codegen').

:- use_module('../emit').

go :-
    parse("(def main () : void (emit 65) (bye))", ok(F)),
    transform_program(F, ok(D)),
    check_program(D, ok(_)),
    compile_program(D, ok(Tokens)),
    AllTokens = [branch(main) | Tokens],
    emit_binary(AllTokens, Bytes),
    open('/tmp/test_vm.bin', write, S, [type(binary)]),
    write_bytes(S, Bytes),
    close(S),
    halt.

write_bytes(_, []).
write_bytes(S, [B|Bs]) :- put_byte(S, B), write_bytes(S, Bs).

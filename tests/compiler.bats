#!/usr/bin/env bats

setup() {
  source "$BATS_TEST_DIRNAME/helpers.sh"
}

# ============================================================
# typecheck: should accept
# ============================================================

@test "typecheck: identity function" {
  result="$(compile '(def id ((x : int)) : int x)' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: arithmetic" {
  result="$(compile '(def f ((a : int) (b : int)) : int (+ a b))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: comparison returns bool" {
  result="$(compile '(def f ((a : int) (b : int)) : bool (< a b))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: if expression" {
  result="$(compile '(def abs ((n : int)) : int (if (< n 0) (- 0 n) n))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: let binding" {
  result="$(compile '(def f ((x : int)) : int (let ((y (+ x 1))) y))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: while loop" {
  result="$(compile '(def f ((n : int)) : void (while (< n 10) (+ n 1)))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: pointer deref" {
  result="$(compile '(def f ((p : (ptr int))) : int (deref p))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: pointer store" {
  result="$(compile '(def f ((p : (ptr int)) (v : int)) : void (store p v))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: const" {
  result="$(compile '(const SIZE int 256) (def f () : int SIZE)' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: builtin emit" {
  result="$(compile '(def f () : void (emit 65))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: builtin key" {
  result="$(compile '(def f () : int (key))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: string literal is ptr(byte)" {
  result="$(compile '(def f () : int (deref8 "hi"))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: int and ptr compatible" {
  result="$(compile '(def f ((a : int)) : int (deref8 a))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: deref8 on int address" {
  result="$(compile '(const BUF int 1024) (def f () : byte (deref8 BUF))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: store8 with int address" {
  result="$(compile '(const BUF int 1024) (def f () : void (store8 BUF 0))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: function call" {
  result="$(compile '(def inc ((x : int)) : int (+ x 1)) (def f () : int (inc 5))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: do block" {
  result="$(compile '(def f () : int (do (emit 65) 42))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: ptr arithmetic" {
  result="$(compile '(def f ((p : (ptr byte))) : (ptr byte) (+ p 1))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: ptr comparison" {
  result="$(compile '(def f ((a : (ptr byte)) (b : (ptr byte))) : bool (= a b))' typed)"
  [[ "$result" == ok* ]]
}

@test "typecheck: string arg to ptr(byte) param" {
  result="$(compile '(def f ((s : (ptr byte))) : byte (deref8 s)) (def main () : void (f "hi") (bye))' typed)"
  [[ "$result" == ok* ]]
}

# ============================================================
# typecheck: should reject
# ============================================================

@test "typecheck rejects: wrong return type" {
  result="$(compile '(def bad ((n : int)) : bool n)' typed)"
  [[ "$result" == error* ]]
}

@test "typecheck rejects: if branch type mismatch" {
  result="$(compile '(def bad ((n : int)) : int (if (< n 0) 1 (< n 5)))' typed)"
  [[ "$result" == error* ]]
}

@test "typecheck rejects: undefined variable" {
  result="$(compile '(def bad () : int x)' typed)"
  [[ "$result" == error* ]]
}

@test "typecheck rejects: wrong arity" {
  result="$(compile '(def f ((x : int)) : int x) (def g () : int (f 1 2))' typed)"
  [[ "$result" == error* ]]
}

# ============================================================
# end-to-end: compile and run
# ============================================================

@test "e2e: emit literal" {
  run run_program '(def main () : void (emit 65) (bye))'
  [ "$output" = "A" ]
}

@test "e2e: arithmetic" {
  run run_program '(def main () : void (emit (+ 60 5)) (bye))'
  [ "$output" = "A" ]
}

@test "e2e: if expression" {
  run run_program '(def main () : void (emit (if (< 1 2) 65 66)) (bye))'
  [ "$output" = "A" ]
}

@test "e2e: let binding" {
  run run_program '(def main () : void (let ((x 65)) (emit x)) (bye))'
  [ "$output" = "A" ]
}

@test "e2e: function call" {
  run run_program '(def add1 ((n : int)) : int (+ n 1)) (def main () : void (emit (add1 64)) (bye))'
  [ "$output" = "A" ]
}

@test "e2e: while loop" {
  run run_program '(const I int 1024) (def main () : void (store I 65) (while (< (deref I) 68) (emit (deref I)) (store I (+ (deref I) 1))) (bye))'
  [ "$output" = "ABC" ]
}

@test "e2e: string literal first byte" {
  run run_program '(def main () : void (emit (deref8 "Hello")) (bye))'
  [ "$output" = "H" ]
}

@test "e2e: echo program" {
  run run_program \
    '(const BUF int 1028) (const I int 1026) (const C int 1024) (def main () : void (store I 0) (while (do (store C (key)) (!= (deref C) 10)) (store8 (+ BUF (deref I)) (deref C)) (store I (+ (deref I) 1))) (store8 (+ BUF (deref I)) 0) (store I 0) (while (!= (deref8 (+ BUF (deref I))) 0) (emit (deref8 (+ BUF (deref I)))) (store I (+ (deref I) 1))) (bye))' \
    $'hello\n'
  [ "$output" = "hello" ]
}

@test "e2e: user void function no spurious drop" {
  run run_program '(def greet () : void (emit 72) (emit 105)) (def main () : void (greet) (emit 10) (bye))'
  [ "$output" = "Hi" ]
}

@test "e2e: multiple void calls in sequence" {
  run run_program '(def a () : void (emit 65)) (def b () : void (emit 66)) (def main () : void (a) (b) (emit 10) (bye))'
  [ "$output" = "AB" ]
}

@test "e2e: ptr(byte) param with string literal" {
  run run_program '(def first ((s : (ptr byte))) : byte (deref8 s)) (def main () : void (emit (first "Zap")) (bye))'
  [ "$output" = "Z" ]
}

@test "e2e: dict program" {
  run run_program_file "$BATS_TEST_DIRNAME/../programs/dict.sets" $'hello\nwords\nfoo\nbye\n'
  [ "$output" = $'Hello!\nwords hello bye \nfoo ?' ]
}

@test "e2e: forth repl arithmetic" {
  run run_program_file "$BATS_TEST_DIRNAME/../programs/forth.sets" $'1 2 + .\nbye\n'
  [[ "$output" == *"3 "* ]]
}

@test "e2e: forth repl stack ops" {
  run run_program_file "$BATS_TEST_DIRNAME/../programs/forth.sets" $'5 dup * .\nbye\n'
  [[ "$output" == *"25 "* ]]
}

@test "e2e: forth repl number parsing" {
  run run_program_file "$BATS_TEST_DIRNAME/../programs/forth.sets" $'42 .\nbye\n'
  [[ "$output" == *"42 "* ]]
}

@test "e2e: forth repl unknown word" {
  run run_program_file "$BATS_TEST_DIRNAME/../programs/forth.sets" $'foo\nbye\n'
  [[ "$output" == *"foo ?"* ]]
}

@test "e2e: forth colon def simple" {
  run run_program_file "$BATS_TEST_DIRNAME/../programs/forth.sets" $': test 42 ;\ntest .\nbye\n'
  [[ "$output" == *"42 "* ]]
}

@test "e2e: forth colon def using builtins" {
  run run_program_file "$BATS_TEST_DIRNAME/../programs/forth.sets" $': square dup * ;\n5 square .\nbye\n'
  [[ "$output" == *"25 "* ]]
}

@test "e2e: forth colon def calling colon def" {
  run run_program_file "$BATS_TEST_DIRNAME/../programs/forth.sets" $': square dup * ;\n: quad square square ;\n3 quad .\nbye\n'
  [[ "$output" == *"81 "* ]]
}

@test "e2e: forth colon def with literal and op" {
  run run_program_file "$BATS_TEST_DIRNAME/../programs/forth.sets" $': double 2 * ;\n7 double .\nbye\n'
  [[ "$output" == *"14 "* ]]
}

@test "e2e: forth throw and recover" {
  run run_program_file "$BATS_TEST_DIRNAME/../programs/forth.sets" $'99 throw\n3 4 + .\nbye\n'
  [[ "$output" == *"error: 99"* ]]
  [[ "$output" == *"7 "* ]]
}

@test "e2e: forth throw from colon def" {
  run run_program_file "$BATS_TEST_DIRNAME/../programs/forth.sets" $': boom 42 throw ;\nboom\n10 .\nbye\n'
  [[ "$output" == *"error: 42"* ]]
  [[ "$output" == *"10 "* ]]
}

@test "e2e: meta alloc and vm-sp" {
  prog='($section 1000)
($alloc X 2)
(def main () : void
  (store X ($vm-sp))
  (if (!= (deref X) 0) (emit 89) (emit 78))
  (bye))'
  run run_program "$prog"
  [ "$output" = "Y" ]
}

# ============================================================
# effects: inference
# ============================================================

@test "effects: pure arithmetic is det" {
  result="$(compile '(def f ((x : int)) : int (+ x 1))' effects)"
  [[ "$result" == *"eff(f,det)"* ]]
}

@test "effects: emit is nondet" {
  result="$(compile '(def f () : void (emit 65))' effects)"
  [[ "$result" == *"eff(f,nondet)"* ]]
}

@test "effects: key is nondet" {
  result="$(compile '(def f () : int (key))' effects)"
  [[ "$result" == *"eff(f,nondet)"* ]]
}

@test "effects: bye is nondet" {
  result="$(compile '(def f () : void (bye))' effects)"
  [[ "$result" == *"eff(f,nondet)"* ]]
}

@test "effects: deref is semidet" {
  result="$(compile '(def f ((p : (ptr int))) : int (deref p))' effects)"
  [[ "$result" == *"eff(f,semidet)"* ]]
}

@test "effects: deref8 is semidet" {
  result="$(compile '(def f ((p : (ptr byte))) : byte (deref8 p))' effects)"
  [[ "$result" == *"eff(f,semidet)"* ]]
}

@test "effects: store is semidet" {
  result="$(compile '(def f ((p : (ptr int)) (v : int)) : void (store p v))' effects)"
  [[ "$result" == *"eff(f,semidet)"* ]]
}

@test "effects: store8 is semidet" {
  result="$(compile '(def f ((p : (ptr byte)) (v : byte)) : void (store8 p v))' effects)"
  [[ "$result" == *"eff(f,semidet)"* ]]
}

@test "effects: if/let/while stay det when pure" {
  result="$(compile '(def f ((n : int)) : int (if (< n 0) (- 0 n) n))' effects)"
  [[ "$result" == *"eff(f,det)"* ]]
}

@test "effects: call propagates callee effect" {
  result="$(compile '(def pure ((x : int)) : int (+ x 1)) (def impure () : void (emit (pure 5)))' effects)"
  [[ "$result" == *"eff(pure,det)"* ]]
  [[ "$result" == *"eff(impure,nondet)"* ]]
}

@test "effects: semidet caller of det callee stays semidet" {
  result="$(compile '(def inc ((x : int)) : int (+ x 1)) (def f ((p : (ptr int))) : void (store p (inc 5)))' effects)"
  [[ "$result" == *"eff(inc,det)"* ]]
  [[ "$result" == *"eff(f,semidet)"* ]]
}

@test "effects: transitive nondet through call chain" {
  result="$(compile '(def a () : void (emit 65)) (def b () : void (a)) (def c () : void (b))' effects)"
  [[ "$result" == *"eff(a,nondet)"* ]]
  [[ "$result" == *"eff(b,nondet)"* ]]
  [[ "$result" == *"eff(c,nondet)"* ]]
}

@test "effects: addr is det" {
  result="$(compile '(def f () : int (addr f))' effects)"
  [[ "$result" == *"eff(f,det)"* ]]
}

@test "effects: execute is nondet" {
  result="$(compile '(def f ((p : int)) : void (execute p))' effects)"
  [[ "$result" == *"eff(f,nondet)"* ]]
}

@test "effects: do block joins children" {
  result="$(compile '(def f ((p : (ptr int))) : int (do (store p 1) (deref p)))' effects)"
  [[ "$result" == *"eff(f,semidet)"* ]]
}

@test "effects: while with semidet body is semidet" {
  result="$(compile '(const I int 1024) (def f () : void (while (< (deref I) 10) (store I (+ (deref I) 1))))' effects)"
  [[ "$result" == *"eff(f,semidet)"* ]]
}

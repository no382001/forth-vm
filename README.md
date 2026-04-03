# forth-vm

> **draft** — work in progress, details may change

16-bit stack VM with a statically-typed s-expression compiler called **sets** *((**s**)-(**e**)xpression (**t**)yped (**s**)ystems language)*. VM in C++20, compiler in Scryer Prolog.

## contents

- [build](#build)
- [usage](#usage)
- [test](#test)
- [language](#language)
  - [syntax](#syntax-ebnf)
  - [types](#types)
  - [effect annotations](#effect-annotations)
  - [built-in primitives](#built-in-primitives)
  - [directives](#directives)
  - [expressions](#expressions)
  - [example](#example)
- [compiler pipeline](#compiler-pipeline)
- [VM](#vm)
  - [memory model](#memory-model)
  - [instruction set](#instruction-set)

## build

```sh
make
```

Requires: C++20 compiler, [Scryer Prolog](https://github.com/mthom/scryer-prolog)

## usage

```sh
./run programs/echo.sets
```

## test

```sh
make test
```

Requires: [bats](https://github.com/bats-core/bats-core)

> the inline `?-` tests in the compiler `.pl` files won't run unless you use the [quads branch of Scryer](https://github.com/no382001/scryer-prolog/tree/quads)

## language

### syntax (EBNF)

```ebnf
program    = form* ;
form       = list | bracket | asm | string | number | symbol ;
list       = '(' form* ')' ;
bracket    = '[' form* ']' ;
asm        = '{' symbol* '}' ;
string     = '"' char* '"' ;
number     = '-'? digit+ ;
symbol     = (any char except whitespace, parens, brackets, braces, '"', ';')+ ;

top-level  = def | extern | const | directive ;
def        = '(' 'def' symbol params ':' type effect? expr+ ')' ;
extern     = '(' 'extern' symbol param-types ':' type ')' ;
const      = '(' 'const' symbol type expr ')' ;
directive  = '(' '$include' string ')'
           | '(' '$section' number ')'
           | '(' '$alloc' symbol number ')' ;

params     = '(' param* ')' ;
param      = symbol | '(' symbol ':' type ')' ;
param-types = '(' type* ')' ;
effect     = '[' 'det' ']' | '[' 'semidet' ']' | '[' 'nondet' ']' ;

type       = 'int' | 'byte' | 'bool' | 'void' | '(' 'ptr' type ')' ;

expr       = number | string | symbol
           | '(' 'if' expr expr expr ')'
           | '(' 'let' '(' binding* ')' expr+ ')'
           | '(' 'do' expr+ ')'
           | '(' 'while' expr expr+ ')'
           | '(' 'deref' expr ')'
           | '(' 'deref8' expr ')'
           | '(' 'store' expr expr ')'
           | '(' 'store8' expr expr ')'
           | '(' 'addr' symbol ')'
           | '(' 'execute' expr ')'
           | '(' binop expr expr ')'
           | '(' symbol expr* ')'
           | '{' symbol* '}' ;

binding    = '(' symbol expr ')' ;
binop      = '+' | '-' | '=' | '<' | '>' | '!=' | '<=' | '>=' | 'and' | 'or' | 'xor' ;
```

Comments start with `;` and run to end of line.

### types

| type | description |
|------|-------------|
| `int` | signed integer (`cell_t` wide) |
| `byte` | 8-bit value (always) |
| `bool` | boolean (0 or 1) |
| `void` | no value |
| `(ptr T)` | pointer to T |

> Sizes depend on the VM configuration. To change them, edit `src/vm.h`:
> - `cell_t` — the native cell type; determines the width of `int` and `(ptr T)` (default: `int16_t`)
> - `MEMORY_SIZE` — total address space in bytes (default: `0xFFFF`)
> - `STACK_SIZE` — max depth of each stack in cells (default: `256`)

### effect annotations

Functions can optionally declare their computational effect:

| annotation | meaning |
|------------|---------|
| `[det]` | pure, no side effects and thus eligible for constant folding |
| `[semidet]` | reads or writes memory |
| `[nondet]` | performs I/O or otherwise escapes |

The compiler infers effects and errors if a declaration is stricter than inferred. Unannotated `det` functions produce a warning.

### built-in primitives

| primitive | signature | description |
|-----------|-----------|-------------|
| `emit` | `(int) : void` | write character to stdout |
| `key` | `() : int` | read character from stdin |
| `bye` | `() : void` | halt the VM |

Inline VM opcodes can be emitted directly with `{...}`, bypassing the type system.

### directives

Directives are compile-time only — they emit no code.

| directive | description |
|-----------|-------------|
| `($include "file")` | splices `file` into the current program at this point, resolved relative to the including file's directory |
| `($section addr)` | sets the static allocation cursor to `addr` |
| `($alloc name size)` | binds `name` to the current cursor as an `int` constant, then advances the cursor by `size` bytes |

### expressions

**`(let ((x expr) ...) body...)`** — binds names to values for the duration of `body`. Each binding is stored at a statically-assigned memory address allocated per-function starting at `0x4000`. No allocation occurs at runtime; the addresses are fixed at compile time.

**`(addr name)`** — pushes the address of a named function as an integer, without calling it. Used to pass functions as values.

**`(execute expr)`** — evaluates `expr` to get a function address, then calls it as an indirect call. Combined with `addr`, this is the only form of indirect dispatch.

**String literals** — a string `"hello"` compiles to a `branch` over its bytes, which are embedded inline in the code segment as null-terminated bytes, followed by a `lit` of the string's start address. Strings are read-only and live in the code region.

### example

```lisp
($section 1022)
($alloc STRI  2)
($alloc CHAR  2)
($alloc IDX   2)
($alloc BUF 256)

($include "core.sets") ; definitions for `true` and `false`

(def streq ((a : int) (b : int)) : bool
  (store STRI 0)
  (while (if (= (deref8 (+ a (deref STRI))) (deref8 (+ b (deref STRI))))
             (!= (deref8 (+ a (deref STRI))) 0)
             false)
    (store STRI (+ (deref STRI) 1)))
  (= (deref8 (+ a (deref STRI))) (deref8 (+ b (deref STRI)))))

(def main () : void
  (while true
    (store IDX 0)
    (while (do (store CHAR (key))
               (!= (deref CHAR) 10))
      (store8 (+ BUF (deref IDX)) (deref CHAR))
      (store IDX (+ (deref IDX) 1)))
    (store8 (+ BUF (deref IDX)) 0)
    (if (streq BUF "bye")
      (bye)
      (do (store IDX 0)
          (while (!= (deref8 (+ BUF (deref IDX))) 0)
            (emit (deref8 (+ BUF (deref IDX))))
            (store IDX (+ (deref IDX) 1)))
          (emit 10)))))
```

Memory layout of the above program at runtime (default config):

```
; $section 1022 sets the cursor; each $alloc advances it

 addr  0        ~N    1022  1024  1026  1028            1283
       ┌────────┬─────┬─────┬─────┬─────┬───────────────┐
       │ code   │     │STRI │CHAR │ IDX │      BUF      │ ...
       └────────┴─────┴─────┴─────┴─────┴───────────────┘
                       <-2-> <-2-> <-2-> <---256 bytes--->

; "bye" string literal is inlined in the code region (branch over + bytes + lit addr)

; let/param slots are statically assigned per-function starting at 0x4000

 addr  16384  16386   16404
       ┌──────┬───────┬────────── ...
       │  a   │   b   │  main slots
       │      streq   │
       └──────┴───────┴────────── ...

; stacks and registers occupy the top of address space
; (with MEMORY_SIZE=65535, STACK_SIZE=256, CELL_SIZE=2)
; DS_START = 64511, RS_START = 65023

 addr  64505   64511          65023          65535
       ┌───────┬──────────────┬──────────────┐
       │ regs  │   rstack     │   dstack     │
       │SP RP  │   512 B      │   512 B      │
       └───────┴──────────────┴──────────────┘
```

## compiler pipeline

The compiler is a multi-pass Prolog program (`compiler/`):

| stage | file | description |
|-------|------|-------------|
| parse | `parser.pl` | characters -> s-expression forms (DCG) |
| ast | `ast.pl` | forms -> typed AST nodes |
| typecheck | `typecheck.pl` | monomorphic type synthesis + checking |
| effects | `effects.pl` | fixed-point effect inference (det/semidet/nondet) |
| dead code | `deadcode.pl` | warn on unreachable definitions |
| const fold | `constfold.pl` | fold `det` calls with constant arguments |
| codegen | `codegen.pl` | AST -> VM token sequence |
| emit | `emit.pl` | tokens -> binary bytecode |

The assembler binding (`gen/gen.pl`) is auto-generated from the C++ dispatch table, keeping the Prolog compiler in sync with the VM instruction set.

## VM

### memory model

Flat byte-addressable memory. Programs are loaded at address 0 and grow upward. The two stacks (data and return) are fixed-size regions at the top of the address space, laid out downward from `MEMORY_SIZE`:

```
0                                                           MEMORY_SIZE
┌─────────────────────────────────┬───────┬──────────┬──────────┐
│ program + heap (grows ->)       │ regs  │  rstack  │  dstack  │
└─────────────────────────────────┴───────┴──────────┴──────────┘
                                  DS_START-n DS_START  RS_START
```

Everything is memory-mapped, there are no loose parts. The instruction pointer (`IP`), data stack pointer (`SP`), and return stack pointer (`RP`) are stored as cells in memory just below the stack region (`DS_START - 1..3`). The stacks themselves are contiguous cell arrays in memory. Nothing lives outside the flat address space.

This means that you can just dump memory in runtime, and when you load it back in you have restored a snapshot of the execution.

`$section` / `$alloc` directives place named variables in the heap region; the compiler does not manage the heap at runtime.

Word size, memory size, and stack depth are set in `src/vm.h` (see [types](#types)):

| constant | default | effect |
|----------|---------|--------|
| `cell_t` | `int16_t` | native word width |
| `MEMORY_SIZE` | `0xFFFF` | total address space in bytes |
| `STACK_SIZE` | `256` | max depth of each stack in cells |

### instruction set

Minimal threaded-style bytecode:

| group | opcodes |
|-------|---------|
| literals | `nop` `lit` |
| memory | `@` `!` `c@` `c!` |
| stack | `dup` `drop` `swap` `over` |
| return stack | `>r` `r>` `r@` |
| alu | `+` `-` `and` `or` `xor` `=` `<` |
| control | `branch` `0branch` `call` `ret` `execute` |
| system | `trap` |

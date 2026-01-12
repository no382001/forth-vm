# forth-vm

A minimal Forth-like stack machine with a macro assembler written in Prolog.

## table of contents

- [dependencies](#dependencies)
- [build](#build)
- [example](#example)
- [architecture](#architecture)
- [instruction set](#instruction-set)
  - [core](#core)
  - [memory](#memory)
  - [stack manipulation](#stack-manipulation)
  - [return stack](#return-stack)
  - [arithmetic & logic](#arithmetic--logic)
  - [control flow](#control-flow)
  - [system](#system)
  - [traps](#traps)
- [assembler](#assembler)
  - [example program](#example-program)
  - [assembler directives](#assembler-directives)
- [tests](#tests)

## dependencies
```bash
apt install g++ make swipl bats clang-format
```

## build
```bash
make
```

## example
```bash
# assemble and run a simple rpn program
./assembler/ma.pl -i programs/repl1.ma -b | ./vm
```

## architecture

The VM is a stack machine with:
- configurable cell size (16-bit by default)
- data and return stack
- 64KB memory space
- 26 opcodes (arithmetic, stack manipulation, memory, control flow)
- traps for I/O (emit, key, bye)

## instruction set

Stack notation: `( before -- after )` where the rightmost value is the top of stack.

### core

| opcode | name  | stack effect    | description                     |
|--------|-------|-----------------|----------------------------------|
| 0      | `nop` | `( -- )`        | no operation                     |
| 1      | `lit` | `( -- n )`      | push next cell as literal value  |

### memory

| opcode | name | stack effect      | description                          |
|--------|------|-------------------|--------------------------------------|
| 2      | `@`  | `( addr -- n )`   | fetch cell (16-bit) from address     |
| 3      | `!`  | `( n addr -- )`   | store cell (16-bit) to address       |
| 4      | `c@` | `( addr -- c )`   | fetch byte from address              |
| 5      | `c!` | `( c addr -- )`   | store byte to address                |

### stack manipulation

| opcode | name   | stack effect        | description                      |
|--------|--------|---------------------|----------------------------------|
| 6      | `drop` | `( a -- )`          | discard top of stack             |
| 7      | `dup`  | `( a -- a a )`      | duplicate top of stack           |
| 8      | `swap` | `( a b -- b a )`    | swap top two items               |
| 9      | `over` | `( a b -- a b a )`  | copy second item to top          |

### return stack

| opcode | name | stack effect           | r-stack effect | description                    |
|--------|------|------------------------|----------------|--------------------------------|
| 10     | `>r` | `( n -- )`             | `( -- n )`     | move top to return stack       |
| 11     | `r>` | `( -- n )`             | `( n -- )`     | move from return stack to data |
| 12     | `r@` | `( -- n )`             | `( n -- n )`   | copy top of return stack       |

### arithmetic & logic

| opcode | name  | stack effect      | description                              |
|--------|-------|-------------------|------------------------------------------|
| 13     | `+`   | `( a b -- a+b )`  | addition                                 |
| 14     | `-`   | `( a b -- a-b )`  | subtraction                              |
| 15     | `and` | `( a b -- a&b )`  | bitwise AND                              |
| 16     | `or`  | `( a b -- a\|b )` | bitwise OR                               |
| 17     | `xor` | `( a b -- a^b )`  | bitwise XOR                              |
| 18     | `=`   | `( a b -- flag )` | equality test (-1 if equal, 0 otherwise) |
| 19     | `<`   | `( a b -- flag )` | less than (-1 if a<b, 0 otherwise)       |

### control flow

| opcode | name      | stack effect  | description                              |
|--------|-----------|---------------|------------------------------------------|
| 20     | `branch`  | `( -- )`      | unconditional jump to address            |
| 21     | `0branch` | `( flag -- )` | jump if top is zero                      |
| 22     | `call`    | `( -- )`      | call subroutine, push return address     |
| 23     | `ret`     | `( -- )`      | return from subroutine                   |
| 24     | `execute` | `( addr -- )` | call address on stack                    |

### system

| opcode | name   | stack effect | description            |
|--------|--------|--------------|------------------------|
| 25     | `trap` | varies       | system call (see below)|

### traps

| trap # | name   | stack effect  | description          |
|--------|--------|---------------|----------------------|
| 0      | emit   | `( c -- )`    | output character     |
| 1      | key    | `( -- c )`    | read character       |
| 2      | bye    | `( -- )`      | exit VM              |
| 3      | assert | `( -- )`      | exit with error (1)  |

## assembler

The macro assembler (`ma.pl`) processes `.ma` files through several stages:

1. **tokenization** - lexing the source
2. **macro expansion** - `def()`, `subst()`, `include()`
3. **label resolution** - `label()`, `branch()`, `zbranch()`, `call()`
4. **binary output** - raw bytecode for the VM

### example program
```forth
def(emit,0)
def(key,1)
def(bye,2)

\ echo input characters
label(loop)
trap key       \ key ( -- c )
trap emit      \ emit ( c -- )
branch(loop)
trap bye       \ bye
```

### assembler directives

| directive             | description                      |
|-----------------------|----------------------------------|
| `label(name)`         | define a jump target             |
| `branch(name)`        | unconditional jump               |
| `zbranch(name)`       | jump if top of stack is zero     |
| `call(name)`          | call subroutine                  |
| `def(name,value)`     | define a constant                |
| `subst(name)`         | substitute a constant            |
| `include("file")`     | include another file             |
| `string(name,"text")` | define a null-terminated string  |
| `addrofstr(name)`     | push string address              |

## tests
```bash
make test
```
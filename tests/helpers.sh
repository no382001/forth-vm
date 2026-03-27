#!/bin/sh

FORTH_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")/.." && pwd)"
PROLOG="scryer-prolog -f"
COMPILER_DIR="$FORTH_ROOT/compiler"

# compile <source> [target]
# Compile a source string, print the result term.
# target: parsed | ast | typed | ir | binary (default: binary)
compile() {
  local src="$1" target="${2:-binary}"
  local srcfile
  srcfile="$(mktemp /tmp/forth-XXXXXX.lisp)"
  printf '%s' "$src" > "$srcfile"
  (
    cd "$COMPILER_DIR"
    $PROLOG -g "
      use_module(compiler),
      compiler:read_source('$srcfile', Chars),
      compile_source(Chars, $target, R),
      write(R), nl, halt.
    " < /dev/null 2>/dev/null
  )
  rm -f "$srcfile"
}

# run_program <source> [stdin_input]
# Compile source to binary, run it on the VM with optional input.
run_program() {
  local src="$1" input="${2:-}"
  local srcfile binfile
  srcfile="$(mktemp /tmp/forth-XXXXXX.lisp)"
  binfile="${srcfile%.lisp}.bin"
  printf '%s' "$src" > "$srcfile"
  (
    cd "$COMPILER_DIR"
    $PROLOG -g "
      use_module(compiler),
      compile_file('$srcfile', '$binfile'), halt.
    " < /dev/null 2>/dev/null
  )
  printf '%s' "$input" | "$FORTH_ROOT/vm" "$binfile"
  rm -f "$srcfile" "$binfile"
}

# run_program_file <lisp_file> [stdin_input]
# Compile a .lisp file to binary, run it on the VM with optional input.
run_program_file() {
  local srcfile="$1" input="${2:-}"
  local binfile
  binfile="$(mktemp /tmp/forth-XXXXXX.bin)"
  (
    cd "$COMPILER_DIR"
    $PROLOG -g "
      use_module(compiler),
      compile_file('$srcfile', '$binfile'), halt.
    " < /dev/null 2>/dev/null
  )
  printf '%s' "$input" | "$FORTH_ROOT/vm" "$binfile"
  rm -f "$binfile"
}

#!/usr/bin/env bats

@test "strap with def/branch/label/subst" {
  run bash -c 'echo "def(something,100)
branch(end)
lit 1
branch(end)
lit 2
subst(something)
label(end)
lit 3
lit 65
trap 0
trap 2" | ./assembler/ma.pl -i /dev/stdin -b | ./vm'
  [ "$status" -eq 0 ]
  [ "$output" = "A" ]
}
#!/usr/bin/env bats

@test "strcmp equal strings returns -1" {
  run bash -c '
cat <<EOF | ./assembler/ma.pl -i /dev/stdin -b | ./vm
string(a,"hello")
string(b,"hello")

branch(main)
include("programs/stdlib.ma")

label(main)
addrofstr(a)
addrofstr(b)
call(strcmp)
zbranch(fail)
lit 2 trap

label(fail)
lit 3 trap
EOF
'
  [ "$status" -eq 0 ]
}

@test "strcmp different strings returns 0" {
  run bash -c '
cat <<EOF | ./assembler/ma.pl -i /dev/stdin -b | ./vm
string(a,"hello")
string(b,"world")

branch(main)
include("programs/stdlib.ma")

label(main)
addrofstr(a)
addrofstr(b)
call(strcmp)
zbranch(pass)
lit 3 trap

label(pass)
lit 2 trap
EOF
'
  [ "$status" -eq 0 ]
}

@test "strcmp first string shorter returns 0" {
  run bash -c '
cat <<EOF | ./assembler/ma.pl -i /dev/stdin -b | ./vm
string(a,"hell")
string(b,"hello")

branch(main)
include("programs/stdlib.ma")

label(main)
addrofstr(a)
addrofstr(b)
call(strcmp)
zbranch(pass)
lit 3 trap

label(pass)
lit 2 trap
EOF
'
  [ "$status" -eq 0 ]
}

@test "strcmp second string shorter returns 0" {
  run bash -c '
cat <<EOF | ./assembler/ma.pl -i /dev/stdin -b | ./vm
string(a,"hello")
string(b,"hell")

branch(main)
include("programs/stdlib.ma")

label(main)
addrofstr(a)
addrofstr(b)
call(strcmp)
zbranch(pass)
lit 3 trap

label(pass)
lit 2 trap
EOF
'
  [ "$status" -eq 0 ]
}

@test "strcmp empty strings returns -1" {
  run bash -c '
cat <<EOF | ./assembler/ma.pl -i /dev/stdin -b | ./vm
string(a,"")
string(b,"")

branch(main)
include("programs/stdlib.ma")

label(main)
addrofstr(a)
addrofstr(b)
call(strcmp)
zbranch(fail)
lit 2 trap

label(fail)
lit 3 trap
EOF
'
  [ "$status" -eq 0 ]
}

@test "strcmp empty vs non-empty returns 0" {
  run bash -c '
cat <<EOF | ./assembler/ma.pl -i /dev/stdin -b | ./vm
string(a,"")
string(b,"hello")

branch(main)
include("programs/stdlib.ma")

label(main)
addrofstr(a)
addrofstr(b)
call(strcmp)
zbranch(pass)
lit 3 trap

label(pass)
lit 2 trap
EOF
'
  [ "$status" -eq 0 ]
}

@test "strcmp single char equal returns -1" {
  run bash -c '
cat <<EOF | ./assembler/ma.pl -i /dev/stdin -b | ./vm
string(a,"x")
string(b,"x")

branch(main)
include("programs/stdlib.ma")

label(main)
addrofstr(a)
addrofstr(b)
call(strcmp)
zbranch(fail)
lit 2 trap

label(fail)
lit 3 trap
EOF
'
  [ "$status" -eq 0 ]
}

@test "strcmp single char different returns 0" {
  run bash -c '
cat <<EOF | ./assembler/ma.pl -i /dev/stdin -b | ./vm
string(a,"x")
string(b,"y")

branch(main)
include("programs/stdlib.ma")

label(main)
addrofstr(a)
addrofstr(b)
call(strcmp)
zbranch(pass)
lit 3 trap

label(pass)
lit 2 trap
EOF
'
  [ "$status" -eq 0 ]
}
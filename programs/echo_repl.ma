\ read line, echo unless "bye"
\ buffer at address 128

label(start)
    lit 128              \ buffer address

label(readloop)
    trap 1               \ ( addr -- addr c )
    dup lit 10 =         \ is it newline?
    zbranch(store)

\ newline: null-terminate and check
drop                 \ ( addr c -- addr )
lit 0 over c!        \ store null terminator
drop
branch(check)

label(store)
    \ store char, increment addr
    swap                 \ ( addr c -- c addr )
    dup >r               \ save addr on return stack
    c!                   \ ( c addr -- )
    r> lit 1 +           \ ( -- addr+1 )
    branch(readloop)

label(check)
    \ compare to "bye" (98 121 101 0)
    lit 128 c@ lit 98 =  \ [0] == 'b'?
    zbranch(notbye)
    lit 129 c@ lit 121 = \ [1] == 'y'?
    zbranch(notbye)
    lit 130 c@ lit 101 = \ [2] == 'e'?
    zbranch(notbye)
    lit 131 c@ lit 0 =   \ [3] == null? (not "byeee")
    zbranch(notbye)

trap 2               \ goodbye!

label(notbye)
    \ echo the buffer
    lit 128

label(echoloop)
    dup c@               \ ( addr -- addr c )
    dup zbranch(echodone) \ if null, we're done
    trap 0               \ emit
    lit 1 +              \ next addr
    branch(echoloop)

label(echodone)
    drop drop            \ clean up stack
    lit 10 trap 0        \ emit newline
    branch(start)
\ read line, echo unless "bye"
\ buffer at address 128

string(bye_str,"bye")

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
    swap                 \ ( addr c -- c addr )
    dup >r               \ save addr on return stack
    c!                   \ ( c addr -- )
    r> lit 1 +           \ ( -- addr+1 )
    branch(readloop)

label(check)
    lit 128 addrofstr(bye_str) call(strcmp)
    zbranch(notbye)
    trap 2               \ goodbye!

label(notbye)
    lit 128

label(echoloop)
    dup c@
    dup zbranch(echodone)
    trap 0
    lit 1 +
    branch(echoloop)

label(echodone)
    drop drop
    lit 10 trap 0
    branch(start)

\ ============================================================
\ strcmp: ( addr1 addr2 -- flag )
\ -1 if equal, 0 if not
\ ============================================================

label(strcmp)
    over c@              \ a1 a2 c1
    over c@              \ a1 a2 c1 c2
    over over            \ a1 a2 c1 c2 c1 c2
    = zbranch(strcmp_no) \ chars differ? not equal

    \ c1 == c2, check if null (end of string)
    drop                 \ a1 a2 c1
    zbranch(strcmp_yes)  \ both null? equal!

    \ advance both pointers
    swap lit 1 + swap    \ a1+1 a2
    lit 1 +              \ a1+1 a2+1
    branch(strcmp)

label(strcmp_yes)
    drop drop            \ clean up addr1 addr2
    lit 0 lit 1 -        \ -1 (true)
    ret

label(strcmp_no)
    drop drop drop drop  \ clean up stack
    lit 0
    ret
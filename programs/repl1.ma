\ ============================================================
\ tiny stack machine repl: can handle numbers,
\ the `+`  and the `.` operator and extists on `bye`
\ ============================================================

def(INBUF,768)
def(USP,832)
def(USTACK,833)

string(s_add,"+")
string(s_dot,".")
string(s_bye,"bye")

branch(main)

include("programs/stdlib.ma")

\ ============================================================
\ user stack
\ ============================================================

\ upush: ( n -- )
label(upush)
    lit subst(USP) c@
    lit subst(USTACK) + c!
    lit subst(USP) c@ lit 1 + lit subst(USP) c!
    ret

\ upop: ( -- n )
label(upop)
    lit subst(USP) c@ lit 1 - dup lit subst(USP) c!
    lit subst(USTACK) + c@
    ret

\ ============================================================
\ main
\ ============================================================

label(main)
    lit 0 lit subst(USP) c!     \ init user stack

label(repl)
    call(readword)
    call(dispatch)
    branch(repl)

\ ============================================================
\ readword: read word into INBUF
\ ============================================================

label(readword)
label(skipws)
    trap 1
    dup lit 32 = zbranch(check_nl)
    drop branch(skipws)
label(check_nl)
    dup lit 10 = zbranch(got_char)
    drop branch(skipws)

label(got_char)
    lit subst(INBUF)

label(read_loop)
    swap over c!         \ store char (expects: char addr)
    lit 1 +              \ addr++
    trap 1               \ read next char (stack: addr char)
    dup lit 32 = zbranch(check_nl2)
    drop lit 0 swap c!
    ret
label(check_nl2)
    dup lit 10 = zbranch(continue_read)
    drop lit 0 swap c!
    ret
label(continue_read)
    swap branch(read_loop)

\ ============================================================
\ dispatch
\ ============================================================

label(dispatch)
    \ bye
    lit subst(INBUF) addrofstr(s_bye) call(strcmp)
    zbranch(try_add)
    trap 2

label(try_add)
    lit subst(INBUF) addrofstr(s_add) call(strcmp)
    zbranch(try_dot)
    call(upop) call(upop) +
    call(upush)
    ret

label(try_dot)
    lit subst(INBUF) addrofstr(s_dot) call(strcmp)
    zbranch(try_num)
    call(upop) call(print_num)
    lit 10 trap 0
    ret

label(try_num)
    lit subst(INBUF) call(parse_num)
    zbranch(unknown)
    call(upush)
    ret

label(unknown)
    drop
    lit 63 trap 0
    lit 10 trap 0
    ret

\ ============================================================
\ parse_num: ( addr -- n flag )
\ ============================================================

label(parse_num)
    lit 0 swap           \ ( 0 addr )

label(pn_loop)
    dup c@               \ ( acc addr c )
    dup zbranch(pn_ok)

    dup lit 48 < zbranch(pn_check_hi)
    drop drop drop lit 0 lit 0 ret

label(pn_check_hi)
    dup lit 58 < zbranch(pn_bad)

    lit 48 -             \ ( acc addr digit )
    >r                   \ R:( digit ), ( acc addr )
    >r                   \ R:( digit addr ), ( acc )
    dup dup + dup + dup +  \ ( acc acc*8 )
    swap dup + +         \ ( acc*10 )
    r> swap              \ ( addr acc*10 )
    r> +                 \ ( addr acc*10+digit ) = ( addr acc' )
    swap lit 1 +         \ ( acc' addr+1 )
    branch(pn_loop)

label(pn_bad)
    drop drop drop lit 0 lit 0 ret

label(pn_ok)
    drop swap drop       \ ( acc )
    lit 0 lit 1 -        \ -1 = success
    ret

\ ============================================================
\ print_num: ( n -- ) prints number
\ ============================================================

label(print_num)
    dup zbranch(print_zero)

    \ push digits in reverse onto return stack
    lit 0 >r             \ sentinel

label(pnum_loop)
    dup zbranch(pnum_emit)
    dup call(mod10)      \ ( n n%10 )
    swap call(div10)     \ ( n%10 n/10 )
    swap                 \ ( n/10 digit )
    lit 48 + >r          \ push ascii digit
    branch(pnum_loop)

label(pnum_emit)
    drop
label(pnum_emit_loop)
    r>
    dup zbranch(pnum_done)
    trap 0
    branch(pnum_emit_loop)

label(pnum_done)
    drop ret

label(print_zero)
    drop lit 48 trap 0 ret

\ ============================================================
\ div10/mod10: ( n -- n/10 ) and ( n -- n%10 )
\ ============================================================

label(div10)
    lit 0 swap           \ ( 0 n )
label(div10_loop)
    dup lit 10 < zbranch(div10_cont)
    drop ret
label(div10_cont)
    lit 10 -
    swap lit 1 + swap
    branch(div10_loop)

label(mod10)
label(mod10_loop)
    dup lit 10 < zbranch(mod10_cont)
    ret
label(mod10_cont)
    lit 10 -
    branch(mod10_loop)
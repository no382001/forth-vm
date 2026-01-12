\ ============================================================
\ tiny stack machine repl: can handle numbers,
\ the `+`  and the `.` operator and extists on `bye`
\ ============================================================

def(INBUF,768)
def(USP,832)
def(USTACK,834)

string(s_add,"+")
string(s_dot,".")
string(s_bye,"bye")

branch(main)

include("programs/stdlib.ma") \ strcmp

\ ============================================================
\ user stack
\ ============================================================

\ upush: ( n -- )
label(upush)
    lit subst(USP) @
    lit subst(USTACK) + !
    lit subst(USP) @ lit 2 + lit subst(USP) !
    ret

\ upop: ( -- n )
label(upop)
    lit subst(USP) @ lit 2 - dup lit subst(USP) !
    lit subst(USTACK) + @
    ret

\ ============================================================
\ main
\ ============================================================

label(main)
    lit 0 lit subst(USP) !     \ init user stack

label(repl)
    call(readword)
    call(dispatch)
    branch(repl)

\ ============================================================
\ readword: ( -- )
\ read whitespace-delimited word into INBUF
\ skips leading spaces/newlines, null-terminates result
\ ============================================================

label(readword)

label(skipws)
    trap 1                   \ ( c ) -- read char
    dup lit 32 =             \ is it space?
    zbranch(check_nl)
    drop branch(skipws)      \ skip space, continue

label(check_nl)
    dup lit 10 =             \ is it newline?
    zbranch(got_char)
    drop branch(skipws)      \ skip newline, continue

label(got_char)
    lit subst(INBUF)         \ ( c addr ) -- start of buffer

label(read_loop)
    swap over c!             \ ( addr ) -- store char at addr
    lit 1 +                  \ ( addr+1 ) -- advance pointer
    trap 1                   \ ( addr+1 c ) -- read next char
    dup lit 32 =             \ is it space?
    zbranch(check_nl2)
    drop lit 0 swap c!       \ null-terminate and return
    ret

label(check_nl2)
    dup lit 10 =             \ is it newline?
    zbranch(continue_read)
    drop lit 0 swap c!       \ null-terminate and return
    ret

label(continue_read)
    swap branch(read_loop)   \ ( addr c ) -- continue loop

\ ============================================================
\ dispatch
\ ============================================================

label(dispatch)
    \ bye
    lit subst(INBUF) addrofstr(s_bye) call(strcmp)
    zbranch(try_add)
    trap 2

label(try_add)
    \ +
    lit subst(INBUF) addrofstr(s_add) call(strcmp)
    zbranch(try_dot)
    call(upop) call(upop) +
    call(upush)
    ret

label(try_dot)
    \ .
    lit subst(INBUF) addrofstr(s_dot) call(strcmp)
    zbranch(try_num)
    call(upop) call(print_num)
    lit 10 trap 0
    ret

label(try_num)
    \ <n>
    lit subst(INBUF) call(parse_num)
    zbranch(unknown)
    call(upush)
    ret

label(unknown)
    \ print `?`
    drop
    lit 63 trap 0
    lit 10 trap 0
    ret

\ ============================================================
\ parse_num: ( addr -- n flag )
\ parse null-terminated decimal string to integer
\ returns ( n -1 ) on success, ( 0 0 ) on failure
\ ============================================================

label(parse_num)
    lit 0 swap           \ ( acc addr ) -- acc starts at 0

label(pn_loop)
    dup c@               \ ( acc addr c ) -- fetch current char
    dup zbranch(pn_ok)   \ null terminator: done, success

    dup lit 48 < zbranch(pn_check_hi)   \ c < '0'? fail
    drop drop drop lit 0 lit 0 ret

label(pn_check_hi)
    dup lit 58 < zbranch(pn_bad)        \ c >= ':'? fail (not 0-9)

    lit 48 -             \ ( acc addr digit ) -- convert ASCII to digit
    >r                   \ R:( digit ), ( acc addr )
    >r                   \ R:( digit addr ), ( acc )
    dup dup + dup + dup +  \ ( acc acc*8 )
    swap dup + +         \ ( acc*10 )
    r> swap              \ ( addr acc*10 )
    r> +                 \ ( addr acc*10+digit ) = ( addr acc' )
    swap lit 1 +         \ ( acc' addr+1 )
    branch(pn_loop)

label(pn_bad)
    drop drop drop lit 0 lit 0 ret   \ invalid char: return failure

label(pn_ok)
    drop drop            \ drop the 0 and addr, keep acc
    lit 0 lit 1 -        \ ( n -1 ) -- push success flag
    ret

\ ============================================================
\ print_num: ( n -- )
\ print unsigned integer to stdout
\ uses return stack to reverse digit order
\ ============================================================

label(print_num)
    dup zbranch(print_zero)  \ special case: n == 0

    lit 0 >r                 \ push sentinel (0) to mark end of digits

label(pnum_loop)
    dup zbranch(pnum_emit)   \ if n == 0, all digits extracted
    dup call(mod10)          \ ( n n%10 )
    swap call(div10)         \ ( n%10 n/10 )
    swap                     \ ( n/10 digit )
    lit 48 + >r              \ convert digit to ASCII, push to R-stack
    branch(pnum_loop)

label(pnum_emit)
    drop                     \ drop the 0

label(pnum_emit_loop)
    r>                       \ pop digit (or sentinel)
    dup zbranch(pnum_done)   \ sentinel (0) means we're done
    trap 0                   \ emit character
    branch(pnum_emit_loop)

label(pnum_done)
    drop ret                 \ drop sentinel, return

label(print_zero)
    drop                     \ drop the 0 input
    lit 48 trap 0            \ emit '0'
    ret

\ ============================================================
\ div10: ( n -- n/10 )
\ integer division by 10 using repeated subtraction
\ ============================================================

label(div10)
    lit 0 swap           \ ( quotient n )
label(div10_loop)
    dup lit 10 <         \ is n < 10?
    zbranch(div10_cont)  \ if n >= 10, continue
    drop ret             \ n < 10: drop remainder, return quotient
label(div10_cont)
    lit 10 -             \ n = n - 10
    swap lit 1 + swap    \ quotient++
    branch(div10_loop)

\ ============================================================
\ mod10: ( n -- n%10 )
\ modulo 10 using repeated subtraction
\ ============================================================

label(mod10)
label(mod10_loop)
    dup lit 10 <         \ is n < 10?
    zbranch(mod10_cont)  \ if n >= 10, continue
    ret                  \ n < 10: n is the remainder
label(mod10_cont)
    lit 10 -             \ n = n - 10
    branch(mod10_loop)
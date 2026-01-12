\ ============================================================
\ strcmp: ( addr1 addr2 -- flag )
\ returns -1 if equal, 0 if not equal
\ ============================================================

label(strcmp)
label(strcmp_loop)
    over c@              \ ( a1 a2 c1 )
    over c@              \ ( a1 a2 c1 c2 )
    
    \ check if chars are equal
    over over =          \ ( a1 a2 c1 c2 equal? )
    zbranch(strcmp_ne)   \ not equal -> return 0
    
    \ chars are equal, check if we hit null (end of both strings)
    drop                 \ ( a1 a2 c1 )
    zbranch(strcmp_eq)   \ c1=0 means both ended -> return -1
    
    \ advance both pointers
    swap lit 1 + swap    \ ( a1+1 a2 )
    lit 1 +              \ ( a1+1 a2+1 )
    branch(strcmp_loop)

label(strcmp_eq)
    drop drop            \ clean up a1 a2
    lit 0 lit 1 -        \ push -1 (true)
    ret

label(strcmp_ne)
    drop drop drop drop  \ clean up a1 a2 c1 c2
    lit 0                \ push 0 (false)
    ret
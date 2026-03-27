\ this is a macro-assembled forth program

label(loop)
lit 1 trap   \ key ( -- c )
lit 0 trap   \ emit ( c -- )
branch(loop)
lit 2 trap
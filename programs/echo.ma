\ this is a macro-assembled forth program

label(loop)
trap 1       \ key ( -- c )
trap 0       \ emit ( c -- )
branch(loop)
trap 2
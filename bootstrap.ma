\ this is a macro-assembled forth program

def(something,100)

branch(end)
lit 1

branch(end)
lit 2

subst(something)
label(end)
lit 3

lit 65 trap 0
trap 2

int 2
global GroupSize
<=
bz revert
gtxn 0 TypeEnum
int appl
==
bz revert
gtxn 0 ApplicationID
arg 0
==
bz revert
b done
revert:
int 0
return
done:
int 1
return
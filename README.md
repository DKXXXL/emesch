# emesch
Scheme Compiler target to C written in Haskell

Maybe One day, all the .hs will be rewritten into .scm

and another Scheme compiler target to C written in Scheme

and I can bootstrap.

Not tested.

JUST FOR FUN 

AND FOR THE LAST QUESTION IN SICP.

For the record of SICP & Write you a Scheme in 48 hours

##Usage:

Use GHC(8.0.1) to compile Emeschc.hs.

With 'Emeschc', use standard input('stdin') and standard output, to input source code and get the output c code.

(So, it only support single file right now.)

The Output c code will need the head file "emeschlib.h" "runtime.h"(which I use "typeof" keyword, not standard c keyword).

Use c compilers to them. BETTER USE -O3!

Even you do not want to use -O3, please use -fno-optimize-sibling-calls! I use many tail call to simulate the Return in order to use call/cc.






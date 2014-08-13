Changes since the 0.0.6 release
-------------------------------

* Clean up internals
* Enable `-fReadline` and `-fHaskeline` by default

Changes since the 0.0.5 release
-------------------------------

Readline/Haskeline support.

Changes since the 0.0.3 release
-------------------------------

Function defintions are added.

Examples:

    > let uncurry(f) = eval . prod(f, I)
    uncurry(f) = eval.prod(f,I)
    f: *a -> exp(*b,*c)
    -----------------------------
    uncurry(f): prod(*a,*b) -> *c

    > let primrec(f,g) = pi2.pr(pair(0,f), pair(s.pi1, g))
    primrec(f,g) = pi2.pr(pair(0,f),pair(s.pi1,g))
    f: 1 -> *a  g: prod(nat,*a) -> *a
    ---------------------------------
    primrec(f,g): nat -> *a

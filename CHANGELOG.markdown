Unreleased
-------------------------------

* Add `show function <name>` subcommand to display information of
  functions including functors, factorizers, and user-defined
  parameterized morphisms
* Add WebAssembly support for browser-based CPL interpreter
* Add tutorials both in English and Japanese
* Drop support for GHC <9.2 (base <4.16)

0.1.0 (2025-10-29)
-------------------------------

* Update Cabal-Version requirement from >=1.10 to 2.2
* Require `mtl` >=2.2.1 for `Control.Monad.Except`
* Fix compilation error on recent `mtl` package

0.0.9 (2018-02-16)
-------------------------------

* Use `Control.Monad.Except` instead of deprecated Control.Monad.Error

0.0.8 (2016-01-14)
-------------------------------

* "→" can be used instead of "->"
* GHC-7.10 support
* Add windows installer

0.0.7 (2014-08-13)
-------------------------------

* Clean up internals
* Enable `-fReadline` and `-fHaskeline` by default

0.0.6 (2009-10-26)
-------------------------------

Readline/Haskeline support.

0.0.4
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

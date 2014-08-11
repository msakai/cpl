An implementation of "A Categorical Programing Language"
========================================================

This package is an implementation of "A Categorical Programing Language"
(CPL for short)[1][2] written in Haskell.

CPL is a functional programming language based on category
theory. Data types are declared in a categorical manner by
adjunctions. Data types that can be handled include the terminal
object, the initial object, the binary product functor, the binary
coproduct functor, the exponential functor, the natural number object,
the functor for finite lists, and the functor for infinite lists.
Each data type is declared with its basic operations or
morphisms. Programs consist of these morphisms, and execution of
programs is the reduction of elements (i.e. special morphisms) to
their canonical form.

Requirements
------------

* GHC-6.10

Install
-------

De-Compress archive and enter its top directory.
Then type:

    $ runhaskell Setup.hs configure
    $ runhaskell Setup.hs build
    $ runhaskell Setup.hs install

If you want to compile with readline or haskeline, add -fReadline or
-fHaskeline respectively to configure command.

Usage
-----

See chapter 5 of [1]

License
-------

This program is licenced under the BSD-style license.
(See the file 'COPYING'.)

Copyright (C) 2004-2003 Masahiro Sakai <masahiro.sakai@gmail.com>

Author
------

Masahiro Sakai <masahiro.sakai@gmail.com>

Bibliography
------------

(1) Tatsuya Hagino, “A Categorical Programming Languge”.
    Ph.D. Thesis, University of Edinburgh, 1987
    available at <http://www.tom.sfc.keio.ac.jp/~hagino/index.html.en>

(2) Tatsuya Hagino, “Categorical Functional Programming Language”.
    Computer Software, Vol 7, No.1.
    Advances in Software Science and Technology 4, 1992
    ISBN 0-12-037104-9

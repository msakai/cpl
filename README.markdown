An implementation of "A Categorical Programming Language"
=========================================================

[![Build Status](https://github.com/msakai/cpl/actions/workflows/build.yaml/badge.svg)](https://github.com/msakai/cpl/actions/workflows/build.yaml)
[![Hackage](https://img.shields.io/hackage/v/CPL.svg)](https://hackage.haskell.org/package/CPL)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/CPL.svg)](https://packdeps.haskellers.com/feed?needle=CPL)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

This package is an implementation of "A Categorical Programming Language"
(CPL for short)[1][2] written in Haskell.

🚀 **Try CPL in your browser:** [WebAssembly Demo](https://msakai.github.io/cpl/) (no installation required!)

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

📦 Install
-------

### Option 1: Use WebAssembly Version (No Installation) 🌐

Try CPL directly in your browser:
[https://msakai.github.io/cpl/](https://msakai.github.io/cpl/)

No installation required! Works on Chrome, Firefox, Safari, and Edge.

### Option 2: Build from Source 🔧

**Supported GHC versions:** >=9.2

#### Standard Build (Native)

De-Compress the archive and enter its top directory.
Then type:

```bash
$ cabal configure
$ cabal build
$ cabal install
```

If you want to compile without haskeline, add `-f-Haskeline` to the configure
command.

Alternatively, you can use Stack:

```bash
$ stack build
$ stack exec cpl
```

To disable haskeline with Stack, use the `--flag` option:

```bash
$ stack build --flag CPL:-Haskeline
```

#### WebAssembly Build

See [web/README.md](web/README.md) for detailed instructions on building and testing the WebAssembly version.

📖 Usage
-----

If you are new to CPL, we recommend starting with the [Tutorial](#tutorial) below.
For a concise reference of the language syntax, see chapter 5 of [1].

### Quick Start ⚡

Once you have CPL running (either in browser or terminal), try these commands:

```
cpl> edit
| right object 1 with !
| end object;
right object 1 defined
cpl> edit
| right object prod(a,b) with pair is
|   pi1: prod -> a
|   pi2: prod -> b
| end object;
right object prod(+,+) defined
cpl> edit
| right object exp(a,b) with curry is
|   eval: prod(exp,a) -> b
| end object;
right object exp(-,+) defined
cpl> edit
| left object nat with pr is
|   0: 1 -> nat
|   s: nat -> nat
| end object;
left object nat defined
cpl> show pair(pi2,eval)
pair(pi2,eval)
    : prod(exp(*a,*b),*a) -> prod(*a,*b)
cpl> let add=eval.prod(pr(curry(pi2), curry(s.eval)), I)
add : prod(nat,nat) -> nat  defined
cpl> simp add.pair(s.s.0, s.0)
s.s.s.0
    : 1 -> nat
cpl> help
  exit                        exit the interpreter
  quit                        ditto
  bye                         ditto
  edit                        enter editing mode
  simp [full] <exp>           evaluate expression
  show <exp>                  print type of expression
  show function <name>        print information of function
  show object <functor>       print information of functor
  load <filename>             load from file
  set trace [on|off]          enable/disable trace
  reset                       remove all definitions
cpl> exit
```

For more examples, see the `samples/` directory.
For step-by-step explanations, see the [Tutorial](#tutorial) below.

### Tutorial

A comprehensive tutorial that covers CPL usage and the category theory concepts behind it.
No prior knowledge of category theory is required.

- [Tutorial (English)](TUTORIAL.md)
- [チュートリアル (日本語)](TUTORIAL_ja.md)

Topics covered: terminal objects, products, coproducts, exponential objects,
natural numbers, lists, infinite lists, and the duality between `left object` and `right object`.

License
-------

This program is licensed under the BSD-style license.
(See the file [COPYING](COPYING).)

Copyright (C) 2004-2026 Masahiro Sakai <masahiro.sakai@gmail.com>

Author
------

Masahiro Sakai <masahiro.sakai@gmail.com>

📖 Bibliography
------------

1. Tatsuya Hagino, “A Categorical Programming Language”.
    Ph.D. Thesis, University of Edinburgh, 1987.
    available at <https://web.sfc.keio.ac.jp/~hagino/index.html.en>

2. Tatsuya Hagino, “Categorical Functional Programming Language”.
    Computer Software, Vol 7, No.1.
    Advances in Software Science and Technology 4, 1992.
    ISBN 0-12-037104-9.

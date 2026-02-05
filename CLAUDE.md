# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

CPL (Categorical Programming Language) is a Haskell implementation of Tatsuya Hagino's categorical programming language. It's a functional language where data types are declared categorically through adjunctions, supporting terminal/initial objects, product/coproduct functors, exponential functors, natural numbers, and finite/infinite list functors.

## Build Commands

```bash
# Build with Stack (recommended)
stack build

# Build with Cabal
cabal configure
cabal build

# Run the interpreter
stack exec cpl

# Run tests
stack test
```

### Build Flags

- `-fReadline`: Enable readline support (default: True)
- `-fHaskeline`: Enable haskeline support (default: True)
- `-fLinuxStatic`: Build statically linked binaries on Linux
- `-fWeb`: Build for browser environment with WebAssembly + JavaScript FFI (default: False)

## Architecture

### Module Structure (src/)

**Core Type System:**
- `CDT.hs` - Categorical Data Types: central type definitions for categorical objects (`CDT`, `ObjectType`, `Nat`). Uses hs-boot file for mutual recursion with Type module.
- `Type.hs` - Generic type representation (`GenType f`, `Type = GenType CDT`)
- `Variance.hs` - Variance lattice (Covariance/Contravariance/Fixed/Free) for type safety
- `FE.hs` - Functorial Expressions with variance tracking

**Expression System:**
- `Exp.hs` - Expression AST (Identity, Comp, Nat, Fact, Funct, Var)
- `AExp.hs` - Annotated expressions with type schemes for type-safe evaluation
- `Simp.hs` - Expression simplifier with `CompiledExp` intermediate representation and trace-aware reduction

**Type Inference:**
- `Typing.hs` - Hindley-Milner style type inference (`inferType`, `unify`)
- `Subst.hs` - Substitution and unification (based on "Typing Haskell in Haskell")
- `Statement.hs` - Equations and categorical constraints

**Parsing:**
- `CDTParser.hs` - Parser for CDT declarations with adjunction syntax
- `ExpParser.hs` - Parser for categorical expressions (composition via `.` or `∘`)
- `ParserUtils.hs` - Parsec utilities

**Integration:**
- `CPLSystem.hs` - System environment (`System` record) integrating parser, type checker, and simplifier
- `Main.hs` - REPL with command dispatch, file loading, and console abstraction (readline/haskeline/plain)

### Key Design Patterns

- **Monad Stack**: `UI a = ExceptT String (StateT UIState Console) a` for error handling and state
- **Variance Lattice**: Four levels (Covariance, Contravariance, FixedVariance, FreeVariance) critical for categorical type soundness
- **CPP Console Abstraction**: Conditional compilation for readline/haskeline/plain I/O

## REPL Commands

- `simp [full] <exp>` - Evaluate expressions
- `show <exp>` / `show object <functor>` - Display type information
- `let <name>(<params>) = <expr>` - Define functions
- `load <filename>` - Load program file
- `set trace [on|off]` - Toggle execution tracing
- `reset` - Clear definitions

## Sample Files

The `samples/` directory contains example programs:
- `*.cdt` files - Category definitions (automata, ccc, misc, rec)
- `*.cpl` files - CPL programs (examples, ack, benchmark, function)
- `examples.txt` - Interactive session transcript

## WebAssembly Build

### web/ Directory

Source files for the browser-based WebAssembly version:

- `index.html` - Web interface with xterm.js terminal emulator
- `cpl-terminal.js` - JavaScript terminal controller and WASM loader
- `tutorial.css` - Stylesheet for tutorial pages

See `web/README.md` for detailed documentation.

### _site/ Directory

Build output directory for WebAssembly version. Contains generated files:

- `cpl.wasm` - Compiled CPL interpreter
- `cpl.js` - JavaScript glue module
- `samples.js` - Sample files module
- Tutorial HTML files (generated from `TUTORIAL.md` and `TUTORIAL_ja.md`)

Build with: `./scripts/build-wasm.sh`

## GitHub Actions Workflows

- `build.yaml` - Native build and test across multiple GHC versions (9.2-9.12), runs on push/PR
- `wasm-deploy.yaml` - WASM build and [GitHub Pages](https://msakai.github.io/cpl/) deployment
- `claude.yml` - Claude Code action for issue/PR automation

## References

1. Tatsuya Hagino, "A Categorical Programming Language", PhD Thesis, University of Edinburgh, 1987
2. Tatsuya Hagino, "Categorical Functional Programming Language", Computer Software Vol 7 No.1, 1992

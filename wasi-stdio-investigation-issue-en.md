# Investigation: Using WASI stdio instead of JavaScript FFI for Web I/O

## Overview

The Web version of CPL currently uses JavaScript FFI from WebAssembly code to perform I/O with xterm.js. We investigated whether it would be possible to configure stdin/stdout on the wasi-shim side so that the Haskell codebase requires no changes and I/O can be done without JavaScript FFI.

## Current Architecture

### Haskell Side (`src/Main.hs`)

Under the `USE_WEB_BACKEND` flag, four JavaScript FFI functions are used:
- `js_readLine` — passes a prompt and receives user input
- `js_printLine` — outputs a string to the terminal
- `js_initialize` — initializes the terminal
- `js_loadFile` — loads files (sample file lookup / file picker)

In non-Web builds, standard stdin/stdout I/O is used via `putStr`/`getLine` or haskeline.

### JavaScript Side (`web/cpl-terminal.js`)

- `terminal_readLine`, `terminal_printLine`, etc. are defined on `window` and called from FFI
- wasi-shim (`@bjorn3/browser_wasi_shim`) configuration:
  - stdin: an empty `File` (`new OpenFile(new File([]))`)
  - stdout/stderr: `ConsoleStdout.lineBuffered` routing output to the browser console

## Findings

### stdout (Output) — Relatively Straightforward

By simply redirecting the wasi-shim stdout callback to xterm.js, `putStrLn` output can be displayed in xterm.js:

```javascript
ConsoleStdout.lineBuffered(msg => term.write(msg + '\r\n'))
```

No changes to the Haskell side are required.

### stdin (Input) — Fundamental Challenge

- **WASI's `fd_read` is a synchronous API** — Haskell's `getLine` calls WASI's `fd_read`, which must return a value synchronously
- **Browser input is inherently asynchronous** — waiting for user keystrokes is fundamentally an asynchronous operation
- With `browser_wasi_shim`'s `OpenFile(new File([]))`, `fd_read` immediately returns EOF, making interactive input impossible

#### Potential Solution: JSPI (JavaScript Promise Integration)

[JSPI](https://v8.dev/blog/jspi) is a Web standard that allows synchronous WASM code to call asynchronous JavaScript APIs:

- Wrap WASI's `fd_read` with `WebAssembly.Suspending` — WASM suspends while waiting for input
- User completes input — Promise resolves — WASM resumes

**Constraints:**
- Available in Chrome 137+ and Firefox 139+, but **Safari does not yet support it** (withdrew its objection in late 2025, but implementation timeline is unknown)
- It is unclear whether the GHC WASM backend officially supports WASI stdio combined with JSPI
- `browser_wasi_shim` itself does not have a JSPI-compatible `fd_read` implementation; a custom implementation would be required

### File Loading (`load` command)

`cmdLoad` uses the `js_loadFile` FFI for sample file lookup and opening a file picker. This functionality is independent of WASI stdio and cannot be replaced by changes to stdin/stdout alone.

## Summary

| Item | Possible with wasi-shim alone? | Notes |
|------|-------------------------------|-------|
| **stdout** (output) | Yes | Just redirect the `ConsoleStdout` callback to xterm.js |
| **stderr** (error output) | Yes | Same as above |
| **stdin** (interactive input) | No | Requires JSPI, which has browser compatibility and ecosystem concerns |
| **File loading** | No | File picker, etc. are outside the scope of WASI stdio |

## Conclusion

- **A partial refactoring to route only stdout/stderr through wasi-shim is possible**, but this alone cannot completely eliminate JavaScript FFI
- **Making stdin work interactively requires JSPI**, which currently faces challenges in cross-browser support and ecosystem maturity
- The current JavaScript FFI approach is a reasonable design decision

## Future Possibilities

- As JSPI browser support expands (especially Safari), I/O solely through WASI stdio could become practical
- A gradual approach could first route only stdout/stderr through wasi-shim while keeping FFI for stdin and load
- If `browser_wasi_shim` or an alternative library implements a JSPI-compatible `fd_read`, the barrier to adoption would be significantly lowered

## References

- [GHC WebAssembly backend User's Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)
- [A detailed guide to using GHC's WebAssembly backend (Finley McIlwaine)](https://finley.dev/blog/2024-08-24-ghc-wasm.html)
- [V8 Blog: Introducing JSPI](https://v8.dev/blog/jspi)
- [browser_wasi_shim](https://github.com/bjorn3/browser_wasi_shim)
- [Tweag: Frontend live-coding via ghci](https://www.tweag.io/blog/2025-04-17-wasm-ghci-browser/)

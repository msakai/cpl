# Investigation: Using WASI stdio instead of JavaScript FFI for Web I/O

## 概要

Web 版 CPL では現在、WebAssembly コードから JavaScript FFI を使って xterm.js に対して入出力を行っています。wasi-shim 側で stdin/stdout の設定を変更すれば、Haskell 本体側は無変更で JavaScript FFI なしに入出力できるのではないか、という可能性について調査しました。

## 現在のアーキテクチャ

### Haskell 側 (`src/Main.hs`)

`USE_WEB_BACKEND` フラグにより、4つの JavaScript FFI 関数を使用:
- `js_readLine` — プロンプトを渡してユーザー入力を受け取る
- `js_printLine` — 文字列をターミナルに出力
- `js_initialize` — ターミナル初期化
- `js_loadFile` — ファイル読み込み（サンプルファイル参照・ファイルピッカー）

非 Web ビルドでは `putStr`/`getLine` や haskeline を使い、通常の stdin/stdout で I/O しています。

### JavaScript 側 (`web/cpl-terminal.js`)

- `terminal_readLine`, `terminal_printLine` 等を `window` に定義して FFI から呼ばれる
- wasi-shim (`@bjorn3/browser_wasi_shim`) の設定:
  - stdin: 空の `File`（`new OpenFile(new File([]))`）
  - stdout/stderr: `ConsoleStdout.lineBuffered` でブラウザコンソールに出力

## 調査結果

### stdout（出力）側 — 比較的容易

wasi-shim の stdout コールバックを xterm.js にリダイレクトするだけで、`putStrLn` の出力を xterm.js に表示できます:

```javascript
ConsoleStdout.lineBuffered(msg => term.write(msg + '\r\n'))
```

Haskell 側の変更は不要です。

### stdin（入力）側 — 根本的な課題あり

- **WASI の `fd_read` は同期 API** — Haskell の `getLine` は WASI の `fd_read` を呼ぶが、これは同期的に値を返す必要がある
- **ブラウザの入力は非同期** — ユーザーのキー入力待ちは本質的に非同期操作
- `browser_wasi_shim` の `OpenFile(new File([]))` では `fd_read` は即座に EOF を返すだけで、対話的な入力は不可能

#### 解決策候補: JSPI (JavaScript Promise Integration)

[JSPI](https://v8.dev/blog/jspi) は同期的な WASM コードから非同期な JavaScript API を呼べるようにする Web 標準:

- WASI の `fd_read` を `WebAssembly.Suspending` でラップ → WASM が入力待ちで一時停止
- ユーザーが入力完了 → Promise が resolve → WASM が再開

**制約:**
- Chrome 137+、Firefox 139+ で利用可能だが、**Safari は未対応**（2025年末に反対を撤回したが実装時期は不明）
- GHC WASM バックエンドが JSPI と組み合わせた WASI stdio を公式にサポートしているかは不明
- `browser_wasi_shim` 自体に JSPI 対応の `fd_read` 実装はなく、カスタム実装が必要

### ファイル読み込み (`load` コマンド)

`cmdLoad` は `js_loadFile` FFI を使っており、サンプルファイルの参照やファイルピッカーの起動を行っています。この機能は WASI stdio とは独立しており、stdin/stdout の変更だけでは代替できません。

## まとめ

| 項目 | wasi-shim だけで可能か | 備考 |
|------|----------------------|------|
| **stdout** (出力) | ✅ 可能 | `ConsoleStdout` のコールバックを xterm.js に向けるだけ |
| **stderr** (エラー出力) | ✅ 可能 | 同上 |
| **stdin** (対話入力) | ❌ 困難 | JSPI が必要だがブラウザ互換性・エコシステムに課題 |
| **ファイル読み込み** | ❌ 不可 | ファイルピッカー等は WASI stdio の範囲外 |

## 結論

- **stdout/stderr のみ wasi-shim 経由にする部分的なリファクタリングは可能** だが、それだけでは JavaScript FFI を完全に排除できない
- **stdin を対話的に動作させるには JSPI が必要** で、現時点ではクロスブラウザ対応やエコシステムの成熟度に課題がある
- 現在の JavaScript FFI による方式は妥当な設計判断と言える

## 今後の可能性

- JSPI のブラウザサポートが広がれば（特に Safari）、WASI stdio のみでの I/O が現実的になる
- 段階的アプローチとして、まず stdout/stderr のみ wasi-shim 経由にし、stdin と load は FFI を維持する方法も考えられる
- `browser_wasi_shim` または代替ライブラリが JSPI 対応の `fd_read` を実装すれば、導入のハードルが大幅に下がる

## 参考

- [GHC WebAssembly backend User's Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)
- [A detailed guide to using GHC's WebAssembly backend (Finley McIlwaine)](https://finley.dev/blog/2024-08-24-ghc-wasm.html)
- [V8 Blog: Introducing JSPI](https://v8.dev/blog/jspi)
- [browser_wasi_shim](https://github.com/bjorn3/browser_wasi_shim)
- [Tweag: Frontend live-coding via ghci](https://www.tweag.io/blog/2025-04-17-wasm-ghci-browser/)

# チュートリアル

## 初期画面

起動すると以下のような画面になります。

```
Categorical Programming Language (Haskell version)
version 0.1.0

Type help for help

cpl> 
```

## 終対象の定義

CPLには組み込みのデータ型がなく、全てのデータ型は定義する必要があります。

何をするにも、他の関数型言語のユニット型に相応する終対象が必ず必要なため、まずはこれを定義します。

`edit` コマンドを用いて、複数行編集モードに入り、データ型の定義を入力し、セミコロン `;` で複数行編集モードを終了します。

```
cpl> edit
| right object 1 with !
| end object;
right object 1 defined
```

`right object 1 defined` と出力され、終対象 `1` が定義できました。

定義された対象の詳細な情報は `show object` を用いて表示することができます。

```
cpl> show object 1
right object 1
- natural transformations:
- factorizer:
    
    ----------
    !: *a -> 1
- equations:
    (RFEQ): 1=!
    (RCEQ): g=!
- unconditioned: yes
- productive: ()
```

ここでは詳細は省きますが、対象と同時に特別な射 `!` も定義されています。

`show` コマンドを使うことで、この射の型(定義域と余域)を表示することができます。

```
cpl> show !
!
    : *a -> 1
```

ここで `*a` は型変数を表しており、 `1` は単なる射ではなく自然変換（一般的な関数型言語における多相関数に相当）になっています。

## 直積の定義

続いて直積を定義します。

```
cpl> edit
| right object prod(a,b) with pair is
|   pi1: prod -> a
|   pi2: prod -> b
| end object;
right object prod(+,+) defined
```

`right object` で対象を定義する際の `pi1`, `pi2` の定義域では、今定義しようとしている対象については引数を省略して書くことになっていることに気をつけてください。ここでは `prod(a,b)` ではなく `prod` と書かれています。

定義結果では `prod(+,+)` と表示され、`prod` は2引数の関手で、どちらの引数についても共変であることが分かります。 `show object` を用いて詳細を表示します。

```
cpl> show object prod
right object prod(+,+)
- natural transformations:
    pi1: prod(*a,*b) -> *a
    pi2: prod(*a,*b) -> *b
- factorizer:
    f0: *a -> *b  f1: *a -> *c
    ------------------------------
    pair(f0,f1): *a -> prod(*b,*c)
- equations:
    (REQ1): pi1.pair(f0,f1)=f0
    (REQ2): pi2.pair(f0,f1)=f1
    (RFEQ): prod(f0,f1)=pair(f0.pi1,f1.pi2)
    (RCEQ): pi1.g=f0 & pi2.g=f1 => g=pair(f0,f1)
- unconditioned: yes
- productive: (yes,yes)
```

終対象の場合と異なり、直積の場合には射影を表す二つの自然変換が同時に定義されています。 (`show` コマンドでも型を確認してみましょう)

また、 `f1: *a -> *b` と `f2: *a -> *c` から `pair(f1,f2): *a -> prod(*b,*c)` を作る関数 `pair` も同時に定義されています。

`pair` 自体は射ではないので `show` で表示することは出来ず、 `show function` を使うことで型を表示できます。

```
cpl> show function pair
f0: *a -> *b  f1: *a -> *c
------------------------------
pair(f0,f1): *a -> prod(*b,*c)
```

また、 `equations` を見ると、

- (REQ1): `pi1.pair(f0,f1)=f0`
- (REQ2): `pi2.pair(f0,f1)=f1`
- (RFEQ): `prod(f0,f1)=pair(f0.pi1,f1.pi2)`
- (RCEQ): `pi1.g=f0 & pi2.g=f1 => g=pair(f0,f1)`

という4つの等式が成り立つことが分かります。これらは圏論において直積を特徴付ける条件です。

## 指数対象の定義

```
cpl> edit
| right object exp(a,b) with curry is
|   eval: prod(exp,a) -> b
| end object;
right object exp(-,+) defined
```

`exp(-,+)` と表示され、`exp` は2引数の関手で、第一引数について反変、第二引数について共変であることが分かります。

`show object` を使って詳細を表示してみます。

```
cpl> show object exp
right object exp(-,+)
- natural transformations:
    eval: prod(exp(*a,*b),*a) -> *b
- factorizer:
    f0: prod(*a,*b) -> *c
    ---------------------------
    curry(f0): *a -> exp(*b,*c)
- equations:
    (REQ1): eval.prod(curry(f0),I)=f0
    (RFEQ): exp(f0,f1)=curry(f1.eval.prod(I,f0))
    (RCEQ): eval.prod(g,I)=f0 => g=curry(f0)
- unconditioned: yes
- productive: (no,no)
```

関数適用を行う `eval` 、カリー化を行う `curry` 、圏論での指数対象の条件などが定義されていることが分かります。

## 自然数対象の定義

```
cpl> edit
| left object nat with pr is
|   0: 1 -> nat
|   s: nat -> nat
| end object;
left object nat defined
```

これは通常の関数型言語での以下のような定義（この例はHaskellでの例）に対応するものです。

```haskell
data Nat where
  Zero :: () -> Nat
  Succ :: Nat -> Nat
```

`show object nat` とすることで、情報を表示してみましょう。

```
cpl> show object nat
left object nat
- natural transformations:
    0: 1 -> nat
    s: nat -> nat
- factorizer:
    f0: 1 -> *a  f1: *a -> *a
    -------------------------
    pr(f0,f1): nat -> *a
- equations:
    (LEQ1): pr(f0,f1).0=f0
    (LEQ2): pr(f0,f1).s=f1.pr(f0,f1)
    (LFEQ): nat=pr(0,s)
    (LCEQ): g.0=f0 & g.s=f1.g => g=pr(f0,f1)
- unconditioned: no
- productive: ()
```

零と後者関数を表す `0` と `s` 、また数学的帰納法に対応する `pr` およびそれらが満たすべき条件が定義されています。

## 直和の定義

```
cpl> edit
| left object coprod(a,b) with case is
|   in1: a -> coprod
|   in2: b -> coprod
| end object;
left object coprod(+,+) defined
```

```
cpl> show object coprod
left object coprod(+,+)
- natural transformations:
    in1: *a -> coprod(*a,*b)
    in2: *b -> coprod(*a,*b)
- factorizer:
    f0: *b -> *a  f1: *c -> *a
    --------------------------------
    case(f0,f1): coprod(*b,*c) -> *a
- equations:
    (LEQ1): case(f0,f1).in1=f0
    (LEQ2): case(f0,f1).in2=f1
    (LFEQ): coprod(f0,f1)=case(in1.f0,in2.f1)
    (LCEQ): g.in1=f0 & g.in2=f1 => g=case(f0,f1)
- unconditioned: yes
- productive: (no,no)
```

これは Haskell で書けば以下のようなデータ型に対応します（名前は異なりますが、標準で用意されている `Either` 型と同型です）。

 ```haskell
 data Coprod a b where
   In1 :: a -> Coprod a b
   In2 :: b -> Coprod a b
 ```

## 型の表示

`show` コマンドを使って射の型(定義域と余域)を表示することができます。

```
cpl> show pair(pi2,eval)
pair(pi2,eval)
    : prod(exp(*a,*b),*a) -> prod(*a,*b)
```

`*a` や `*b` は型変数を表し、この射は正確には `prod(exp(*a,*b),*a)` という関手から `prod(*a,*b)` という関手への自然変換を表しています。

## 式に名前を付ける

`let` を使うことで式に名前を付け、後から名前で参照することができます。

```
cpl> let add=eval.prod(pr(curry(pi2), curry(s.eval)), I)
add : prod(nat,nat) -> nat  defined
```

これは通常関数型言語の以下のような関数を畳み込みで表現した上でポイントフリーにしたものに対応します。

```haskell
add 0 y = y
add (x + 1) y = add x y + 1
```

また、パラメータを持つような定義も行うことができます。

```
cpl> let uncurry(f) = eval . prod(f, I)
f: *a -> exp(*b,*c)
-----------------------------
uncurry(f): prod(*a,*b) -> *c
```


## 計算

CPLでは `simp` コマンドによって、射の式を簡約することで計算を行います。先ほど定義した加算の関数 `add` を使った射を簡約してみましょう。

```
cpl> simp add.pair(s.s.0, s.0)
s.s.s.0
    : 1 -> nat
```

加算と同様に乗算と階乗も定義して計算してみましょう:

```
cpl> let mult=eval.prod(pr(curry(0.!), curry(add.pair(eval, pi2))), I)
mult : prod(nat,nat) -> nat
cpl> let fact=pi1.pr(pair(s.0,0), pair(mult.pair(s.pi2,pi1), s.pi2))
fact : nat -> nat  defined
cpl> simp fact.s.s.s.s.0
s.s.s.s.s.s.s.s.s.s.s.s.s.s.s.s.s.s.s.s.s.s.s.s.0
    : 1 -> nat
```

## リスト型

次は自然数とほぼ同じですが少しだけ複雑なデータ型であるリスト型を定義してみます。

```
cpl> edit
| left object list(p) with prl is
|   nil: 1 -> list
|   cons: prod(p,list) -> list
| end object;
left object list(+) defined
```

これはHaskellでいえば以下のようなデータ型に対応します。

```haskell
data List a where
  Nil :: () -> List a
  Cons :: (a, List a) -> List a
```

次にリスト型を用いたお馴染みの関数達を表現してみましょう。

連結（`append`）:
```
cpl> let append = eval.prod(prl(curry(pi2), curry(cons.pair(pi1.pi1, eval.pair(pi2.pi1, pi2)))), I)
append : prod(list(*a),list(*a)) -> list(*a)  defined
```

逆転（`reverse`）:
```
cpl> let reverse=prl(nil, append.pair(pi2, cons.pair(pi1, nil.!)))
reverse : list(*a) -> list(*a)  defined
```

`head` / `tail`:
```
cpl> let hd = prl(in2, in1.pi1)
hd : list(*a) -> coprod(*a,1)  defined
cpl> let tl = coprod(pi2,I).prl(in2, in1.prod(I, case(cons,nil)))
tl : list(*a) -> coprod(list(*a),1)  defined
```

後で無限リストを使う際に `head` / `tail` という名前を使いたいので、ここでは `hd` / `tl` という名前を使っています。また、CPLでは全域関数しか存在せず部分関数型言語存在しないため、余域は`1`との直和（他の言語における `Maybe` 型や `option` 型）になっています。

さらに、 `head` / `tail` の結果に再度 `head` / `tail` を適用するのに便利なように、定義域を `1` との直和に持ち上げたバージョンも定義しておきましょう。

```
cpl> let hdp=case(hd,in2)
hdp : coprod(list(*a),1) -> coprod(*a,1)  defined
cpl> let tlp = case(tl, in2)
tlp : coprod(list(*a),1) -> coprod(list(*a),1)  defined
```

連番 `[n-1, n-2, ..., 1, 0]`:
```
cpl> let seq = pi2.pr(pair(0,nil), pair(s.pi1, cons))
seq : nat -> list(nat)  defined
```

これらを利用して計算を行ってみます（一部の計算は `simp` の代わりに `simp full` を使わないと簡約が進まないケースがあるので注意してください) 。

```
cpl> simp seq.s.s.s.0
cons.pair(s.pi1,cons).pair(s.pi1,cons).pair(0,nil)
    : 1 -> list(nat)
cpl> simp full seq.s.s.s.0
cons.pair(s.s.0,cons.pair(s.0,cons.pair(0,nil)))
    : 1 -> list(nat)
cpl> simp hdp.tl.seq.s.s.s.0
in1.s.0
    : 1 -> coprod(nat,*a)
cpl> simp full append.pair(seq.s.s.0, seq.s.s.s.0)
cons.pair(s.0,cons.pair(0,cons.pair(s.s.0,cons.pair(s.0,cons.pair(0,nil)))))
    : 1 -> list(nat)
cpl> simp full reverse.it
cons.pair(0,cons.pair(s.0,cons.pair(s.s.0,cons.pair(0,cons.pair(s.0,nil.!)))))
    : 1 -> list(nat)
```

`simp full reverse.it` では直前の計算結果を `it` で参照しています。

## 無限リスト

自然数やリストのような有限のデータ型だけでなく、無限リストのデータ型を定義することもできます。

```
cpl> edit
| right object inflist(a) with fold is
|   head: inflist -> a
|   tail: inflist -> inflist
| end object;
right object inflist(+) defined
```

```
cpl> show object inflist
right object inflist(+)
- natural transformations:
    head: inflist(*a) -> *a
    tail: inflist(*a) -> inflist(*a)
- factorizer:
    f0: *a -> *b  f1: *a -> *a
    ------------------------------
    fold(f0,f1): *a -> inflist(*b)
- equations:
    (REQ1): head.fold(f0,f1)=f0
    (REQ2): tail.fold(f0,f1)=fold(f0,f1).f1
    (RFEQ): inflist(f0)=fold(f0.head,tail)
    (RCEQ): head.g=f0 & tail.g=g.f1 => g=fold(f0,f1)
- unconditioned: no
- productive: (no)
```

`fold` という名前が定義されていますが、これは現代ではむしろ `unfold` と呼びだくなりますね。

それでは、無限リストを用いた射の定義と計算をしてみましょう。

```
cpl> let incseq=fold(I,s).0
incseq : 1 -> inflist(nat)  defined
cpl> simp head.incseq
0
    : 1 -> nat
cpl> simp head.tail.tail.tail.incseq
s.s.s.0
    : 1 -> nat
cpl> let alt=fold(head.pi1, pair(pi2, tail.pi1))
alt : prod(inflist(*a),inflist(*a)) -> inflist(*a)  defined
cpl> let infseq=fold(I,I).0
infseq : 1 -> inflist(nat)  defined
cpl> simp head.tail.tail.alt.pair(incseq, infseq)
s.0
    : 1 -> nat
```

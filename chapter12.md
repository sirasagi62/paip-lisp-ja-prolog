# 第12章
## ロジックプログラムのコンパイル

[第 11 章](chapter11.md) の最後では、論理変数の新しい、より効率的な表現が導入されました。
この表現を組み込んでProlog インタープリターの新しいバージョンを構築するのが合理的でしょう。
しかし、[第 9 章](chapter9.md) では、コンパイラはインタープリタよりも高速に実行され、構築もそれほど難しくないことを学びました。
そのため、この章では、Prolog から Lisp に変換する Prolog コンパイラを紹介します。

それぞれのProlog 述語は Lisp 関数に変換され、異なる数の引数で呼び出される述語は異なる述語とするという規則を採用します。
シンボル `p` が 1 つまたは 2 つの引数で呼び出される場合、2 つの述語を実装するには 2 つの Lisp 関数が必要になります。
Prolog の伝統に従い、これらは `p/1` および `p/2` と呼ばれます。

次のステップは、生成された Lisp コードがどのようになるかを決定することです。
各節のheadを引数に対して単一化する必要があり、単一化が成功した場合は、bodyの述語を呼び出す必要があります。
難しいのは、選択ポイントを覚えておかなければならないことです。
最初の節の述語の呼び出しが失敗した場合は、2 番目の節に戻って再試行できる必要があります。

これは、すべての述語に追加の引数として *成功継続* を渡すことによって実行できます。
この継続は、未解決のまま残っている目標、つまり「証明」の「その他の目標」の議論を表します。
述語内の各節について、節内のすべての目標が成功した場合、成功継続を呼び出す必要があります。
目標が達成できなかった場合、特別なことは何もせず、次の節に進むだけです。
1 つ複雑な点があります。失敗した後、`unify!` によって行われた束縛をすべて元に戻す必要があります。
例を考えてみましょう。次の節

```lisp
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
```

は次のようにコンパイルできます。

```lisp
(defun likes/2 (?arg1 ?arg2 cont)
 ;; First clause:
 (if (and (unify! ?arg1 'Robin) (unify! ?arg2 'cats))
   (funcall cont))
 (undo-bindings)
 ;; Second clause:
 (if (unify! ?argl 'Sandy)
   (likes/2 ?arg2 'cats cont))
 (undo-bindings)
 ;; Third clause:
 (if (unify! ?argl 'Kim)
   (likes/2 ?arg2 'Lee
     #'(lambda () (likes/2 ?arg2 'Kim cont))))))
```

最初の節では、2 つの引数をチェックし、単一化が成功した場合は、最初の節にbodyがないため、継続を直接呼び出します。
2 番目の節では、`likes/2` が再帰的に呼び出され、`?arg2` が `cats` を好きかどうかを確認します。
これが成功すると、元の目標は成功し、継続 `cont` が呼び出されます。
3 番目の節では、`likes/2` を再度再帰的に呼び出す必要があります。今回は、`?arg2` が `Lee` を好きかどうかを確認するように要求します。
このチェックが成功すると、継続が呼び出されます。
この場合、継続には `likes/2` への別の呼び出しが含まれ、`?arg2` が `Kim` を好きかどうかを確認します。
これが成功すると、最終的に元の継続である `cont` が呼び出されます。

Prolog インタープリタでは、保留中の目標のリストである`other-goals`を節のbodyの目標に追加する必要があったことを思い出してください。
コンパイラでは、`append` を実行する必要はありません。代わりに、継続 cont が他の目標を表し、節のbodyは関数への明示的な呼び出しによって表されます。

前述の `likes/2` のコードでは、不要な `unify!` の呼び出しがいくつか削除されていることに注意してください。
最も明白な実装では、引数ごとに `unify!` を 1 回呼び出します。
したがって、2 番目の節のコードは次のようになります。

```lisp
(if (and (unify! ?argl 'Sandy) (unify! ?arg2 ?x))
 (likes/2 ?x 'cats cont))
```

ここで、変数 `?x` に適切な let 束縛が必要になります。

## 12.1 Prologコンパイラ

このセクションでは、[図12.1](#f0010)にまとめられたコンパイラを紹介します。
最上位レベルには関数 `prolog-compile` があり、これはシンボルを受け取り、そのシンボルに対して定義された節を調べ、節をアリティ別にグループ化します。
各シンボル/アリティは、`compile-predicate` によって個別の Lisp 関数にコンパイルされます。

| 関数 | 説明 |
|----------------------------|-------------------------------------------------------------------|
| | **トップレベル関数** |
| `?-` | クエリを作成しますが、まずすべてをコンパイルします。 |
| | **特殊変数** |
| `*trail*` | これまでに作成されたすべての束縛のリスト。 |
| | **主な関数** |
| `top-level-prove` | 最初にすべてをコンパイルする新バージョン。 |
| `run-prolog` | すべてをコンパイルし、Prolog 関数を呼び出します。 |
| `prolog-compile-symbols` | Prolog シンボルのリストをコンパイルします。 |
| `prolog-compile` | シンボルをコンパイルし、各引数ごとに個別の関数を作成します。 |
| `compile-predicate` | 指定されたシンボル/アリティのすべての節をコンパイルします。 |
| `compile-clause` | headを変換して、結果のbodyをコンパイルします。 |
| `compile-body` | 節のbodyをコンパイルします。 |
| `compile-call` | Prolog 述語への呼び出しをコンパイルします。 |
| `compile-arg` | bodyの目標への引数のコードを生成します。 |
| `compile-unify` | var とterm が単一化されているかどうかをテストするコードを返します。 |
| | **補助関数** |
| `clauses-with-arity` | headが指定されたアリティを持つすべての節を返します。 |
| `relation-arity` | リレーションに対する引数の数。 |
| `args` | 関係の引数。 |
| `make-parameters` | パラメータのリストを作成します。 |
| `make-predicate` | 名前/アリティ形式のシンボルを構築します。 |
| `make-=` | 単一化関係を構築します。 |
| `def-prolog-compiler-macro` | Prolog のコンパイラ マクロを定義します。 |
| `prolog-compiler-macro` | Prolog 述語のコンパイラ マクロを取得します。 |
| `has-variable-p` | 式 `x` のどこかに変数があるかどうか? |
| `proper-listp` | `x` は適切な (ドットなしの) リストかどうか? |
| `maybe-add-undo-bindings` | 元に戻す必要がある束縛をすべて元に戻します。 |
| `bind-unbound-vars` | 必要に応じて `let` を追加します。 |
| `make-anonymous` | 一度だけ使用される変数を `?` に置き換えます。 |
| `anonymous-variables-in` | 匿名変数のリスト。 |
| `compile-if` | IF フォームをコンパイルします。`else` 部分は使用できません。 |
| `compile-unify-variable` | `var` の単一化をコンパイルします。 |
| `bind-variables-in` | `exp` 内のすべての変数をそれら自身に束縛します。 |
| `follow-binding` | 束縛に従って `var` の最終的な束縛を取得します。 |
| `bind-new-variables` | 束縛されていない変数を含めるように束縛を拡張します。 |
| `ignore` | 何もしない - 引数を無視します。 |
| | **以前に定義された関数** |
| `unify!` | 破壊的単一化 (セクション 11.6 を参照) |
| `undo-bindings!` | トレイルを使用してバックトラックし、束縛を元に戻します。 |
| `binding-val` | var/val 束縛の値部分を選択します。 |
| `symbol` | インターンされたシンボルを作成または検索します。 |
| `new-symbol` | 新しいインターンされていないシンボルを作成します。 |
| `find-anywhere` | アイテムがツリー内のどこかに出現するかどうか? |

図12.1: Prologコンパイラの用語集

```lisp
(defun prolog-compile (symbol &optional
                       (clauses (get-clauses symbol)))
  "Compile a symbol; make a separate function for each arity."
  (unless (null clauses)
    (let ((arity (relation-arity (clause-head (first clauses)))))
      ;; Compile the clauses with this arity
      (compile-predicate
        symbol arity (clauses-with-arity clauses #'= arity))
      ;; Compile all the clauses with any other arity
      (prolog-compile
        symbol (clauses-with-arity clauses #'/= arity)))))
```

ここには 3 つのユーティリティ関数が含まれています。

```lisp
(defun clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given arity."
  (find-all arity clauses
            :key #'(lambda (clause)
                     (relation-arity (clause-head clause)))
            :test test))

(defun relation-arity (relation)
  "The number of arguments to a relation.
  Example: (relation-arity '(p a b c)) => 3"
  (length (args relation)))

(defun args (x) "The arguments of a relation" (rest x))
```

次のステップは、固定の引数を持つ特定の述語の節を Lisp 関数にコンパイルすることです。
今のところ、各節を個別にコンパイルし、適切なパラメータ リストを使用して `lambda` でラップすることによって実行されます。

```lisp
(defun compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
  into a single LISP function."
  (let ((predicate (make-predicate symbol arity))
        (parameters (make-parameters arity)))
    (compile
     (eval
      `(defun ,predicate (,@parameters cont)
            .,(mapcar #'(lambda (clause)
                        (compile-clause parameters clause 'cont))
              clauses))))))

(defun make-parameters (arity)
  "Return the list (?arg1 ?arg2 ... ?arg-arity)"
  (loop for i from 1 to arity
        collect (new-symbol '?arg i)))

(defun make-predicate (symbol arity)
  "Return the symbol: symbol/arity"
  (symbol symbol '/ arity))
```

さて、ここからが難しい部分です。実際に節のコードを生成する必要があります。
ここでも、1 つの節に必要なコードの例を示します。
まず、次の簡単なコードをターゲットとして設定します。

```lisp
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(defun likes/2 (?arg1 ?arg2 cont)
 ...
 (if (and (unify! ?argl 'Kim) (unify! ?arg2 ?x)
   (likes/2 ?arg2 'Lee
      #'(lambda () (likes/2 ?x 'Kim))))
...)
```

ただし、改善されたコードへのアップグレードの可能性も検討しています。

```lisp
(defun likes/2 (?arg1 ?arg2 cont)
 ...
 (if (unify! ?arg1 'Kim)
   (likes/2 ?arg2 'Lee
      #'(lambda () (likes/2 ?arg2 'Kim))))
...)
```

1 つのアプローチとしては、`compile-head` と `compile-body` という 2 つの関数を記述し、それらをコードに結合することです (if *head body*)。
このアプローチでは、以前のコードを簡単に生成できます。
しかし、少し先のことを考えてみましょう。最終的に改善されたコードを生成したい場合は、headとbodyの間で何らかの通信が必要になります。
headについては`?arg2` と `?x` の単一化をコンパイルしないことを決定しましたため、bodyについては `?arg2` を `?x` に置き換える必要があることを認識しなければなりません。
つまり、`compile-head` 関数は概念的に、headのコードとbodyで実行する置換の指示という2 つの値を返すということです。
これは複数の値を明示的に操作することで処理できますが、複雑に思えます。

別の方法としては、`compile-head` を削除して `compile-body` のみを記述する方法があります。
これは、実際にその節に対してソースコード変換を実行する場合に可能になります。
この節を次のように扱う代わりに、

```lisp
(<- (likes Kim ?x)
  (likes ?x Lee) (likes ?x Kim))
```

これを同等のものに変形します:

```lisp
(<- (likes ?arg1 ?arg2)
  (= ?arg1 Kim) (= ?arg2 ?x) (likes ?x Lee) (likes ?x Kim))
```

これで、節の先頭の引数が関数 `likes/2` の引数と一致するため、先頭のコードを生成する必要はありません。
これにより、`compile-head` が削除されて単純になり、さらに別の理由でも分解がより適切になります。`compile-head` に最適化を追加する代わりに、`=` を処理する `compile-body` のコードに最適化を追加します。
こうすることで、ソースコード変換によって導入された呼び出しに加えて、ユーザーが `=` に対して行う呼び出しを最適化できます。

概要を把握するために、関数の呼び出しシーケンスは次のようになります。

```lisp
prolog-compile
  compile-predicate
    compile-clause
      compile-body
        compile-call
        compile-arg
        compile-unify
            compile-arg
```

各関数は、1 レベルインデントされたその下の関数を呼び出します。
最初の 2 つの関数はすでに定義されています。
これが `compile-clause` の最初のバージョンです。

```lisp
(defun compile-clause (parms clause cont)
  "Transform away the head, and compile the resulting body."
  (compile-body
    (nconc
      (mapcar #'make-= parms (args (clause-head clause)))
      (clause-body clause))
    cont))

(defun make-= (x y) `(= ,x ,y))
```

作業の大部分は `compile-body` で行われますが、これは少し複雑です。
3つの場合が考えられます。まずbodyがない場合は、継続を呼び出すだけです。次にbodyが `=` の呼び出しで始まる場合は、 `unify!` の呼び出しをコンパイルします。そして、それ以外の場合は適切な継続を渡して関数の呼び出しをコンパイルします。

しかし、この時点で先を見据えて考えていたほうがよいでしょう。今 `=` を特別に扱いたいのであれば、おそらく後で他の目標も特別に扱いたくなるでしょう。したがって、`=` を明示的に確認する代わりに、データ駆動型のディスパッチを実行して、`prolog-compiler-macro` プロパティがアタッチされている述語をすべて探すことにしましょう。
Lisp コンパイラ マクロと同様に、マクロは目標の処理を拒否できます。
`:pass` を返した場合マクロがそれを処理しないことを決定したことを意味するという規則を採用し、したがって通常のゴールとしてコンパイルする必要があります。

```lisp
(defun compile-body (body cont)
  "Compile the body of a clause."
  (if (null body)
      `(funcall ,cont)
      (let* ((goal (first body))
             (macro (prolog-compiler-macro (predicate goal)))
             (macro-val (if macro
                            (funcall macro goal (rest body) cont))))
        (if (and macro (not (eq macro-val :pass)))
            macro-val
            (compile-call
               (make-predicate (predicate goal)
                               (relation-arity goal))
               (mapcar #'(lambda (arg) (compile-arg arg))
                       (args goal))
               (if (null (rest body))
                   cont
                   `#'(lambda ()
                      ,(compile-body (rest body) cont))))))))

(defun compile-call (predicate args cont)
  "Compile a call to a prolog predicate."
  `(,predicate ,@args ,cont))

(defun prolog-compiler-macro (name)
  "Fetch the compiler macro for a Prolog predicate."
  ;; Note NAME is the raw name, not the name/arity
  (get name 'prolog-compiler-macro))

(defmacro def-prolog-compiler-macro (name arglist &body body)
  "Define a compiler macro for Prolog."
  `(setf (get ',name 'prolog-compiler-macro)
         #'(lambda ,arglist .,body)))

(def-prolog-compiler-macro = (goal body cont)
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass
        `(if ,(compile-unify (first args) (second args))
             ,(compile-body body cont)))))

(defun compile-unify (x y)
  "Return code that tests if var and term unify."
  `(unify! ,(compile-arg x) ,(compile-arg y)))
  ```

あと残っているのは、body内の目標への引数をコンパイルする関数である `compile-arg` だけです。
以下の `q` の引数へのコンパイルに示すように、考慮すべきケースは 3 つです。

| | |
|----------------------------|------------------------------|
| `1 (<- (p ?x) (q ?x))` | `(q/1 ?x cont)` |
| `2 (<- (p ?x) (q (fab)))` | `(q/1 '(fab) cont)` |
| `3 (<- (p ?x) (q (f ?xb)))` | `(q/1 (list 'f ?x 'b) cont)` |

1つめの場合、引数は変数であり、そのままコンパイルされます。
2つめの場合では、引数は引用符で囲まれた式にコンパイルされる定数式 (変数のない式) です。
3つめの場合では、引数に変数が含まれているため、式を構築するコードを生成する必要があります。
３つめについては、実際にはリストの先で 2 つに分割されています。最初のケースでは `list` の呼び出しにコンパイルされ、もう１つの場合では `cons` の呼び出しにコンパイルされます。
目標 `(q (f ?xb))` には関数 `f` の呼び出しは含まれないことを覚えておくことが重要です。
むしろ、これは単なる3つの要素からなるリストである `(f ?xb)` という用語を含んでいるのです。

```lisp
(defun compile-arg (arg)
  "Generate code for an argument to a goal in the body."
  (cond ((variable-p arg) arg)
        ((not (has-variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar #'compile-arg arg)))
        (t `(cons ,(compile-arg (first arg))
                  ,(compile-arg (rest arg))))))

(defun has-variable-p (x)
  "Is there a variable anywhere in the expression x?"
  (find-if-anywhere #'variable-p x))

(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))
```

どのように動作するか見てみましょう。
以下の節について検討してみます。

```lisp
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))
```

`prolog-compile` の結果は次のとおりです:

```lisp
(DEFUN LIKES/2 (?ARG1 ?ARG2 CONT)
 (IF (UNIFY! ?ARG1 'ROBIN)
  (IF (UNIFY! ?ARG2 'CATS)
   (FUNCALL CONT)))
 (IF (UNIFY! ?ARG1 'SANDY)
  (IF (UNIFY! ?ARG2 ?X)
   (LIKES/2 ?X 'CATS CONT)))
 (IF (UNIFY! ?ARG1 'KIM)
  (IF (UNIFY! ?ARG2 ?X)
   (LIKES/2 ?X 'LEE (LAMBDA ()
      (LIKES/2 ?X 'KIM CONT))))))
(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT)
 (IF (UNIFY! ?ARG1 ?ITEM)
  (IF (UNIFY! ?ARG2 (CONS ?ITEM ?REST))
   (FUNCALL CONT)))
 (IF (UNIFY! ?ARG1 ?ITEM)
  (IF (UNIFY! ?ARG2 (CONS ?X ?REST))
   (MEMBER/2 ?ITEM ?REST CONT))))
```

## 12.2 コンパイラのエラーを修正する

このバージョンのコンパイラにはいくつか問題があります。

* `unify!` を呼び出すたびに束縛を元に戻すのを忘れています。

* 以前定義した `undo-bindings!` の定義では、引数として `*trail*` 配列のインデックスが必要です。したがって、各関数に入るときに、トレイルの現在の最上部を保存する必要があります。

* `?x` などのローカル変数は導入されずに使用されました。それらは新しい変数に束縛される必要があります。

束縛を元に戻すのは簡単です。`compile-predicate` に 1 行追加して、関数 `maybe-add-undo-bindings` を呼び出すだけです。
この関数は、失敗するたびに `undo-bindings!` の呼び出しを挿入します。
節が 1 つしかない場合は、呼び出しシーケンスの上位にある述語が失敗したときに元に戻すため、元に戻す必要はありません。
複数の節がある場合、関数は関数body全体を let でラップし、トレイルのフィル ポインターの初期値をキャプチャします。これにより、束縛を適切なポイントまで元に戻すことができます。
同様に、コンパイルされた各節を `bind-unbound-vars` の呼び出しでラップすることで、束縛されていない変数の問題を処理できます。

```lisp
(defun compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
  into a single LISP function."
  (let ((predicate (make-predicate symbol arity))
        (parameters (make-parameters arity)))
    (compile
     (eval
      `(defun ,predicate (,@parameters cont)
  .,(maybe-add-undo-bindings                  ;***
     (mapcar #'(lambda (clause)
           (compile-clause parameters clause 'cont))
      clauses)))))))

(defun compile-clause (parms clause cont)
  "Transform away the head, and compile the resulting body."
  (bind-unbound-vars                                   ;***
    parms                                              ;***
    (compile-body
      (nconc
        (mapcar #'make-= parms (args (clause-head clause)))
        (clause-body clause))
      cont)))

(defun maybe-add-undo-bindings (compiled-exps)
  "Undo any bindings that need undoing.
  If there are any, bind the trail before we start."
  (if (length=1 compiled-exps)
      compiled-exps
      `((let ((old-trail (fill-pointer *trail*)))
          ,(first compiled-exps)
          ,@(loop for exp in (rest compiled-exps)
                  collect '(undo-bindings! old-trail)
                  collect exp)))))

(defun bind-unbound-vars (parameters exp)
  "If there are any variables in exp (besides the parameters)
  then bind them to new vars."
  (let ((exp-vars (set-difference (variables-in exp)
                                  parameters)))
    (if exp-vars
        `(let ,(mapcar #'(lambda (var) `(,var (?)))
                       exp-vars)
           ,exp)
        exp)))
```

これらの改善により、`likes` と `member` のコードは次のようになります。

```lisp
(DEFUN LIKES/2 (?ARG1 ?ARG2 CONT)
 (LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))
  (IF (UNIFY! ?ARG1 'ROBIN)
   (IF (UNIFY! ?ARG2 'CATS)
      (FUNCALL CONT)))
  (UNDO-BINDINGS! OLD-TRAIL)
  (LET ((?X (?)))
   (IF (UNIFY! ?ARG1 'SANDY)
    (IF (UNIFY! ?ARG2 ?X)
      (LIKES/2 ?X 'CATS CONT))))
  (UNDO-BINDINGS! OLD-TRAIL)
  (LET ((?X (?)))
   (IF (UNIFY! ?ARG1 'KIM)
    (IF (UNIFY! ?ARG2 ?X)
      (LIKES/2 ?X 'LEE (LAMBDA ()
          (LIKES/2 ?X 'KIM CONT))))))))
(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT)
 (LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))
  (LET ((?ITEM (?))
      (?REST (?)))
   (IF (UNIFY! ?ARG1 ?ITEM)
      (IF (UNIFY! ?ARG2 (CONS ?ITEM ?REST))
            (FUNCALL CONT))))
  (UNDO-BINDINGS! OLD-TRAIL)
  (LET ((?X (?))
      (? ITEM (?))
      (?REST (?)))
  (IF (UNIFY! ?ARG1 ?ITEM)
   (IF (UNIFY! ?ARG2 (CONS ?X ?REST))
            (MEMBER/2 ?ITEM ?REST CONT))))))

```
## 12.3 コンパイラの改善

これはかなり良いですが、まだ改善の余地があります。
小さな改善点の 1 つは、不要な変数を削除することです。
たとえば、`member` の最初の節の `?rest` と 2 番目の節の `?x` は、新しい変数 (`(?)` 呼び出しの結果) に束縛され、一度だけ使用されます。
生成されたコードは、変数に束縛してからその変数を参照するのではなく、単に `(?)` をインラインに配置することで、もう少し簡潔にすることができます。
この変更には 2 つの部分があります。`compile-arg` を更新して匿名変数をインラインでコンパイルすることと、`<-` マクロを変更して、節内に 1 回だけ出現するすべての変数を匿名変数に変換することです。

```lisp
(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(add-clause ',(make-anonymous clause)))

(defun compile-arg (arg)
  "Generate code for an argument to a goal in the body."
  (cond ((variable-p arg) arg)
        ((not (has-variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar #'compile-arg arg)))
        (t `(cons ,(compile-arg (first arg))
                  ,(compile-arg (rest arg))))))

(defun make-anonymous (exp &optional
                       (anon-vars (anonymous-variables-in exp)))
  "Replace variables that are only used once with ?."
  (cond ((consp exp)
         (reuse-cons (make-anonymous (first exp) anon-vars)
                     (make-anonymous (rest exp) anon-vars)
                     exp))
        ((member exp anon-vars) '?)
        (t exp)))
```

匿名変数を見つけるのは簡単ではありません。
次の関数は、1 回だけ使用された変数と 2 回以上使用された変数の 2 つのリストを保持します。
次に、ローカル関数 `walk` を使用してツリーを走査し、各cons cellのコンポーネントを再帰的に考慮し、各変数に遭遇するたびに 2 つのリストを更新します。
このローカル関数の使用法は、[ページ 428](#p428) の [演習 12.23](#p4625) で説明されている代替案と同様に覚えておく必要があります。

```lisp
(defun anonymous-variables-in (tree)
  "Return a list of all variables that occur only once in tree."
  (let ((seen-once nil)
            (seen-more nil))
    (labels ((walk (x)
            (cond
                ((variable-p x)
                    (cond ((member x seen-once)
                              (setf seen-once (delete x seen-once))
                              (push x seen-more))
                        ((member x seen-more) nil)
                        (t (push x seen-once))))
                ((consp x)
                    (walk (first x))
                    (walk (rest x))))))
      (walk tree)
      seen-once)))
```

これで、`member` は次のようにコンパイルされます。

```lisp
(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT)
 (LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))
  (LET ((?ITEM (?)))
   (IF (UNIFY! ?ARG1 ?ITEM)
    (IF (UNIFY! ?ARG2 (CONS ?ITEM (?)))
        (FUNCALL CONT))))
  (UNDO-BINDINGS! OLD-TRAIL)
  (LET ((?ITEM (?))
    (?REST (?)))
   (IF (UNIFY! ?ARG1 ?ITEM)
    (IF (UNIFY! ?ARG2 (CONS (?) ?REST))
      (MEMBER/2 ?ITEM ?REST CONT))))))

```
## 12.4 単一化のコンパイルの改善

ここで、`compile-unify` の改善について説明します。
`unify!` への特定の呼び出しを削除したいことを思い出してください。その結果、例えば`member`の最初の節では

```lisp
(<- (member ?item (?item . ?rest)))
```

がコンパイルされると次のようになります:

```lisp
(LET ((?ITEM (?)))
 (IF (UNIFY! ?ARG1 ?ITEM)
  (IF (UNIFY! ?ARG2 (CONS ?ITEM (?)))
    (FUNCALL CONT))))
```

より効率的にコンパイルできる場合:

```lisp
(IF (UNIFY! ?ARG2 (CONS ?ARG1 (?)))
  (FUNCALL CONT))
```
1 つの目標で単一化を解除すると、後で他の目標にも影響が出るため、単一化された式を追跡する必要があります。ここで`compile-unify`の設計についてグローバル状態変数を変更するという方法と、複数の値を返す方法の2つについて選択の余地があります。今回はグローバル変数は扱いにくいという理由で、2 番目の選択肢を採用します。`compile-unify` は束縛リストを追加の引数として受け取り、実際のコードと更新された束縛 リストの2 つの値を返します。
これらの複数の値を処理するには、他の関連関数を変更する必要があると考えられます。

`compile-unify` が例に示した節で最初に呼び出されると、`?arg1` と `?item` を単一化するように求められます。ここではコードを返さないようにします (より正確には、自明な真のテスト `t`です)。
2 番目の値については、`?item` が `?arg1` に束縛された新しい束縛リストを返す必要があります。
この束縛は、後続のコードで `?item` を `?arg1` に置き換えるために使用されます。

`?item` を `?arg1` に束縛するのではなく、その逆を行うとなぜわかるのでしょうか?
これは`?arg1` がすでに何か、つまり `member` に渡された値に束縛されているからです。この値が何であるかはわかりませんが、無視することはできません。
したがって、初期束縛 リストでは、パラメーターが何かに束縛されていることを示す必要があります。
単純な規則としては、パラメータをそれ自体に束縛することです。
したがって、初期の束縛 リストは次のようになります。

```lisp
((?arg1 .?arg1) (?arg2 . ?arg2))
```

前の章 ([ページ 354](chapter11.md#p354)) で、変数をそれ自身に束縛すると問題が発生する可能性があることを説明したので、注意を払わなければなりません。

パラメータに対する新しい変数の単一化を排除する以外にも、改善できる点はたくさんあります。
たとえば、定数のみを含む単一化はコンパイル時に実行できます。
呼び出し `(= (fa) (fa ))` は常に成功しますが、 `(= 3 4)` は常に失敗します。
さらに、2 つのコンス セルの単一化は、コンパイル時にコンポーネントに分割できます。`(= (f ?x) (fa))` は `(= ?xa)` と `(= ff)` に簡略化され、後者は簡単に成功します。
コンパイル時にいくつかの出現検査を行うこともできます。例えば`(= ?x (f ?x))` は失敗するはずです。

次の表は、これらの改善点と、束縛された `(?arg1)` または束縛されていない `(?x)` 変数を別の式に対して単一化する場合の内訳を示しています。
最初の列は単一化呼び出し、2 番目は生成されたコード、3 番目は呼び出しの結果として追加される束縛です。

| | 単一化 | コード | 束縛 |
|------|---------------------|----------------|----------|
| 1 | `(= 3 3)` | `t` | `-` |
| 2 | `(= 3 4)` | `nil` | `-` |
| 3 | `(= (f ?x) (?p 3))` | `t` | `(?x . 3) (?p . f)` |
| 4 | `(= ?arg1 ?y)` | `t` | `(?y . ?arg1)` |
| 5 | `(= ?arg1 ?arg2)` | `(unify! ?arg1 ?arg2)` | `(?arg1 . ?arg2)` |
| 6 | `(= ?arg1 3)` | `(unify! ?arg1 3)` | `(?arg1 . 3)` |
| 7 | `(= ?arg1 (f ? y))` | `(単一化! ?arg1 . . . )` | `(?y . ?y)` |
| 8 | `(= ?x ?y)` | `t` | `(?y . ?y)` |
| 9 | `(= ?x 3)` | `t` | `(?x . 3)` |
| 10 | `(= ?x (f ? y))` | `(unify! ?x . . . )` | `(?y . ?y)` |
| 11 | `(= ?x (f ? x))` | `nil` | `-` |
| 12 | `(= ?x ?)` | `t` | `-` |

この表から、`compile-unify` の新しいバージョンを作成できます。
最初の部分はかなり簡単です。
これは、この表の最初の 3 つのケースを処理し、他のケースでは最初の引数として変数を使用して `compile-unify-variable` が呼び出されるようにします。

```lisp
(defun compile-unify (x y bindings)
  "Return 2 values: code to test if x and y unify,
  and a new binding list."
  (cond
    ;; Unify constants and conses:                       ; Case
    ((not (or (has-variable-p x) (has-variable-p y)))    ; 1,2
     (values (equal x y) bindings))
    ((and (consp x) (consp y))                           ; 3
     (multiple-value-bind (code1 bindings1)
         (compile-unify (first x) (first y) bindings)
       (multiple-value-bind (code2 bindings2)
           (compile-unify (rest x) (rest y) bindings1)
         (values (compile-if code1 code2) bindings2))))
    ;; Here x or y is a variable.  Pick the right one:
    ((variable-p x) (compile-unify-variable x y bindings))
    (t              (compile-unify-variable y x bindings))))

(defun compile-if (pred then-part)
  "Compile a Lisp IF form. No else-part allowed."
  (case pred
    ((t) then-part)
    ((nil) nil)
    (otherwise `(if ,pred ,then-part))))
```

次の関数 `compile-unify-variable` は、これまで見た中で最も複雑なものの 1 つです。
各引数について、束縛 (ローカル変数 `xb` および `yb`) があるかどうかを確認し、束縛を使用して各引数の値 (`x1` および `y1`) を取得します。
束縛されていない変数または自身に束縛されている変数のいずれの場合も、`x` は `x1` に等しくなります (`y` と `y1` についても同様です)。
どちらかの値のペアが等しくない場合は、新しい値 (`x1` または `y1`) を使用する必要があります。コメント化された deref 節がそれを実行します。
その後については場合分けを一つずつ確認していきます。
前の表から少し順序を変える方が簡単であることがわかりましたが、各節には対応する番号がコメントされています。

```lisp
(defun compile-unify-variable (x y bindings)
  "X is a variable, and Y may be."
  (let* ((xb (follow-binding x bindings))
         (x1 (if xb (cdr xb) x))
         (yb (if (variable-p y) (follow-binding y bindings)))
         (y1 (if yb (cdr yb) y)))
    (cond                                                 ; Case:
      ((or (eq x '?) (eq y '?)) (values t bindings))      ; 12
      ((not (and (equal x x1) (equal y y1)))              ; deref
       (compile-unify x1 y1 bindings))
      ((find-anywhere x1 y1) (values nil bindings))       ; 11
      ((consp y1)                                         ; 7,10
       (values `(unify! ,x1 ,(compile-arg y1 bindings))
               (bind-variables-in y1 bindings)))
      ((not (null xb))
       ;; i.e. x is an ?arg variable
       (if (and (variable-p y1) (null yb))
           (values 't (extend-bindings y1 x1 bindings))   ; 4
           (values `(unify! ,x1 ,(compile-arg y1 bindings))
                   (extend-bindings x1 y1 bindings))))    ; 5,6
      ((not (null yb))
       (compile-unify-variable y1 x1 bindings))
      (t (values 't (extend-bindings x1 y1 bindings)))))) ; 8,9

```

少し時間をかけてこの機能がどのように動作するかを理解しましょう。理解したら、次の補助機能に進みましょう。

```lisp
(defun bind-variables-in (exp bindings)
  "Bind all variables in exp to themselves, and add that to
  bindings (except for variables already bound)."
  (dolist (var (variables-in exp))
    (unless (get-binding var bindings)
      (setf bindings (extend-bindings var var bindings))))
  bindings)

(defun follow-binding (var bindings)
  "Get the ultimate binding of var according to bindings."
  (let ((b (get-binding var bindings)))
    (if (eq (car b) (cdr b))
        b
        (or (follow-binding (cdr b) bindings)
            b))))
```

ここで、新しい `compile-unify` をコンパイラの残りの部分に単一化する必要があります。
問題は、新しいバージョンでは追加の引数を取り、追加の値を返すため、それを呼び出すすべての関数を変更する必要があることです。
呼び出しシーケンスをもう一度見てみましょう。

```lisp
prolog-compile
  compile-predicate
    compile-clause
      compile-body
        compile-call
        compile-arg
          compile-unify
            compile-arg
            ```

まず、下に向かっていくと、`compile-arg` は適切な値を検索して代入できるように、引数として束縛 リストを取る必要があることがわかります。
ただし、束縛 リストは変更されないため、1 つの値が返されます。

```lisp
(defun compile-arg (arg bindings)
  "Generate code for an argument to a goal in the body."
  (cond ((eq arg '?) '(?))
        ((variable-p arg)
         (let ((binding (get-binding arg bindings)))
           (if (and (not (null binding))
                    (not (eq arg (binding-val binding))))
             (compile-arg (binding-val binding) bindings)
             arg)))
        ((not (find-if-anywhere #'variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar #'(lambda (a) (compile-arg a bindings))
                          arg)))
        (t `(cons ,(compile-arg (first arg) bindings)
                  ,(compile-arg (rest arg) bindings)))))

```

一方上方向については`compile-body` は束縛 リストを受け取り、それをさまざまな関数に渡す必要があることがわかります。

```lisp
(defun compile-body (body cont bindings)
  "Compile the body of a clause."
  (cond
    ((null body)
     `(funcall ,cont))
    ((eq (first body) '!)                              ;***
     `(progn ,(compile-body (rest body) cont bindings) ;***
             (return-from ,*predicate* nil)))          ;***
    (t (let* ((goal (first body))
              (macro (prolog-compiler-macro (predicate goal)))
              (macro-val (if macro
                             (funcall macro goal (rest body)
                                      cont bindings))))
        (if (and macro (not (eq macro-val :pass)))
            macro-val
            `(,(make-predicate (predicate goal)
                               (relation-arity goal))
              ,@(mapcar #'(lambda (arg)
                            (compile-arg arg bindings))
                        (args goal))
              ,(if (null (rest body))
                   cont
                   `#'(lambda ()
                        ,(compile-body
                           (rest body) cont
                           (bind-new-variables bindings goal))))))))))

```
関数 `bind-new-variables` は、目標内でまだ束縛されていないすべての変数を取り、それらの変数を自分自身に束縛します。
これは、目標が何であれ、その目標がその引数を拘束する可能性があるためです。

```lisp
(defun bind-new-variables (bindings goal)
  "Extend bindings to include any unbound variables in goal."
  (let ((variables (remove-if #'(lambda (v) (assoc v bindings))
                              (variables-in goal))))
    (nconc (mapcar #'self-cons variables) bindings)))

(defun self-cons (x) (cons x x))
```

束縛 リストを受け入れるために変更する必要がある関数の 1 つは、`=` のコンパイラ マクロです。

```lisp
(def-prolog-compiler-macro = (goal body cont bindings)
  "Compile a goal which is a call to =."
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass ;; decline to handle this goal
        (multiple-value-bind (code1 bindings1)
            (compile-unify (first args) (second args) bindings)
          (compile-if
            code1
            (compile-body body cont bindings1))))))
```

最後のステップは、`compile-clause` を変更して、すべてのパラメータが自身に束縛された束縛リストを `compile-body` に渡すことですべてを開始するようにすることです。

```lisp
(defun compile-clause (parms clause cont)
  "Transform away the head, and compile the resulting body."
  (bind-unbound-vars
    parms
    (compile-body
      (nconc
        (mapcar #'make-= parms (args (clause-head clause)))
        (clause-body clause))
      cont
      (mapcar #'self-cons parms))))                    ;***

```

ついに、私たちの努力が実を結びました。

```lisp
(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT)
 (LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))
  (IF (UNIFY! ?ARG2 (CONS ?ARG1 (?)))
      (FUNCALL CONT))
  (UNDO-BINDINGS! OLD-TRAIL)
  (LET ((?REST (?)))
    (IF (UNIFY! ?ARG2 (CONS (?) ?REST))
        (MEMBER/2 ?ARG1 ?REST CONT)))))
 (DEFUN LIKES/2 (?ARG1 ?ARG2 CONT)
  (LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))
    (IF (UNIFY! ?ARG1 'ROBIN)
        (IF (UNIFY! ?ARG2 'CATS)
          (FUNCALL CONT)))
    (UNDO-BINDINGS! OLD-TRAIL)
    (IF (UNIFY! ?ARG1 'SANDY)
      (LIKES/2 ?ARG2 'CATS CONT))
    (UNDO-BINDINGS! OLD-TRAIL)
    (IF (UNIFY! ?ARG1 'KIM)
      (LIKES/2 ?ARG2 'LEE (LAMBDA ()
            (LIKES/2 ?ARG2 'KIM CONT))))))
```

## 12.5 単一化のさらなる改善

`compile-unify` をさらに改善できるでしょうか?
`unify!` を呼び出すのであれば、これ以上改善することはできないようにみえます。
しかし、実際には `unify!` をコンパイルすることで改善できます。
これは、Prolog コンパイラーで最も一般的に使用されているモデルである Warren Abstract Machine (WAM) の重要なアイデアです。

4 つのケース (5、6、7、および 10) で `unify!` を呼び出しますが、いずれの場合も最初の引数は変数であり、2 番目の引数については何かがわかっています。
しかし、`unify!` が最初に行うことは、最初の引数が変数であるかどうかを冗長にテストすることです。
したがって汎用的な関数`unify!`ではなく、より特化した関数を呼び出すことで、不要なテストを排除できます。
次の呼び出しを考えてみましょう:

```lisp
(unify! ?arg2 (cons ?arg1 (?)))
```

`?arg2` が束縛されていない変数の場合、このコードは適切です。
しかし、`?arg2` が定数アトムである場合は、`cons` と `?` がガベージを生成するのを許可せずに、すぐに失敗する必要があります。
テストをここで参照されている適切な関数定義を用いて次のように変更できます。

```lisp
(and (consp-or-variable-p ?arg2)
  (unify-first! ?arg2 ?arg1)
  (unify-rest! ?arg2 (?)))
```

この変更により、実行時間が短縮され、生成されるガベージの量が制限されるはずです。
もちろん、生成されるコードは長くなるため、プログラムがコードをプロセッサに送るのに時間がかかりすぎると、処理速度が低下する可能性があります。

**演習 12.1 [h]** `consp-or-variable-p、unify-first!、`、`unify-rest!` の定義を書いてください。また、コンパイラを変更して、前に概説したようなコードを生成してください。
[セクション9.6](chapter9.md#s0035)の[ページ300](chapter9.md#p300)から始まる関数`compile-rule`を確認するとよいでしょう。
この関数は `pat-match` の呼び出しを個別のテストにコンパイルしました。今度は同じことを `unify!` に対して実行します。
いくつかのベンチマークを実行して、変更されたコンパイラを元のバージョンと比較してみてください。

**演習 12.2 [h]** どの変数が逆参照されたかを追跡し、適切な単一化関数 (引数を逆参照する関数、または引数がすでに逆参照されていると想定する関数) を呼び出すことで、さらに効率を上げることができます。
このアプローチを実装してください。

**演習 12.3 [m]** `(= (f (g ?x) ?y) (f ?y (?pa))) に対してどのようなコードが生成されますか?` 同じ単一化を表すより効率的なコードはどのようになるでしょうか?
このより効率的な結果を得るためにコンパイラを変更するのはどれくらい簡単でしょうか?

**演習 12.4 [h]** 振り返ってみると、`(?argl . ?argl`) のように変数を自分自身に束縛するのはあまり良い考えではなかったようです。
これにより、束縛の意味が複雑になり、既存のツールを使用できなくなります。
たとえば、ケース 11 では、`occur-check` は非循環束縛 リストを期待するため、`occur-check` ではなく `find-anywhere` を使用する必要がありました。
しかし、find-anywhere は `occur-check` ほど完全な仕事をしません。
コード、非循環束縛 リスト、および不明な値に束縛されている変数のリストの 3 つの値を返す `compile-unify` バージョンを書いてみてください。

**演習 12.5 [h]** 前の演習の代わりとして、束縛リストをまったく使用しないこともできます。
代わりに、同値クラスのリスト、つまり各サブリストに単一化された1 つ以上の要素が含まれるリストのリストを渡すこともできます。
このアプローチでは、初期の同値クラスリストは `((?arg1) (?arg2))` になります。
`?arg1` を `?x` と単一化し、`?arg2` を `?y` と単一化し、`?x` を 4 と単一化すると、リストは ( `(4 ?arg1 ?x) (?arg2 ?y))` になります。
これは、同値クラスの標準メンバー (他のすべてのメンバーの代わりになるメンバー) が最初に来るという規則を前提としています。
このアプローチを実装してみましょう。また、これにはどのような利点と欠点があるでしょうか?

## 12.6 コンパイラのユーザーインターフェース

コンパイラは Prolog を Lisp に変換できますが、適切な Prolog 関係をコンパイルして適切な Lisp 関数を呼び出すように都合よく調整できない限り、それは役に立ちません。
つまり、コンパイラを `<-` および `?` マクロと単一化する必要があります。
驚くべきことに、これらのマクロを変更する必要はまったくありません。
むしろ、これらのマクロが呼び出す関数を変更します。
新しい節が入力されると、その節の述語がリスト `*uncompiled*` に入力されます。
これは`add-clause:`への1行の追加です。

```lisp
(defvar *uncompiled* nil
  "Prolog symbols that have not been compiled.")

(defun add-clause (clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (pushnew pred *uncompiled*)                          ;***
    (setf (get pred 'clauses)
          (nconc (get-clauses pred) (list clause)))
    pred))
```

クエリが実行されると、`?-` マクロが `top-level-prove` の呼び出しに展開されます。
クエリ内のゴールのリストは、`show-prolog-vars` ゴールとともに、関係 `top-level-query` の唯一の節として追加されます。
次に、そのクエリは、コンパイルされていないリストにある他のクエリとともにコンパイルされます。
最後に、新しくコンパイルされたトップレベルのクエリ関数が呼び出されます。

```lisp
(defun top-level-prove (goals)
  "Prove the list of goals by compiling and calling it."
  ;; First redefine top-level-query
  (clear-predicate 'top-level-query)
  (let ((vars (delete '? (variables-in goals))))
    (add-clause `((top-level-query)
                  ,@goals
                  (show-prolog-vars ,(mapcar #'symbol-name vars)
                                    ,vars))))
  ;; Now run it
  (run-prolog 'top-level-query/0 #'ignore)
  (format t "~&No.")
  (values))

(defun run-prolog (procedure cont)
  "Run a 0-ary prolog procedure with a given continuation."
  ;; First compile anything else that needs it
  (prolog-compile-symbols)
  ;; Reset the trail and the new variable counter
  (setf (fill-pointer *trail*) 0)
  (setf *var-counter* 0)
  ;; Finally, call the query
  (catch 'top-level-prove
    (funcall procedure cont)))

(defun prolog-compile-symbols (&optional (symbols *uncompiled*))
  "Compile a list of Prolog symbols.
  By default, the list is all symbols that need it."
  (mapc #'prolog-compile symbols)
  (setf *uncompiled* (set-difference *uncompiled* symbols)))

(defun ignore (&rest args)
  (declare (ignore args))
  nil)
```

トップレベルでは、継続して何かを行う必要がないことに注意してください。
任意ですが、今回は引数を無視するように定義された関数 `ignore` を渡すことを選択しました。
この関数はさまざまな場所で役立ちます。プログラマーによっては、これをインラインで宣言し、ignore 宣言の代わりに `ignore` の呼び出しを使用する人もいます。

```lisp
(defun third-arg (x y z)
  (ignore x y)
  z)
```

コンパイラの呼び出し規約はインタープリタとは異なるため、プリミティブを再定義する必要があります。
プリミティブ `show-prolog-vars` の古い定義には、目標への引数のリスト、束縛リスト、保留中の目標のリストの 3 つのパラメーターがありました。
`show-prolog-vars/2` の新しい定義にも 3 つのパラメータがありますが、これは単なる偶然です。
最初の 2 つのパラメーターは、目標に対する 2 つの個別の引数です。変数名のリストと変数値のリストです。
最後のパラメータは継続関数です。
続行するにはその関数を呼び出しますが、失敗する場合は `top-level-prove` で設定されたキャッチポイントにスローします。

```lisp
(defun show-prolog-vars/2 (var-names vars cont)
  "Display the variables, and prompt the user to see
  if we should continue.  If not, return to the top level."
  (if (null vars)
      (format t "~&Yes")
      (loop for name in var-names
            for var in vars do
            (format t "~&~a = ~a" name (deref-exp var))))
  (if (continue-p)
      (funcall cont)
      (throw 'top-level-prove nil)))

(defun deref-exp (exp)
  "Build something equivalent to EXP with variables dereferenced."
  (if (atom (deref exp))
      exp
      (reuse-cons
        (deref-exp (first exp))
        (deref-exp (rest exp))
        exp)))
        ```

これらの定義が配置されていると、`?-` マクロを使用してクエリを実行するだけで、コンパイラを自動的に呼び出すことができます。

**演習 12.6 [m]** `q` を呼び出す述語 `p` を定義し、次に `q` を定義するとします。
Lisp の一部の実装では、`(?- (p ?x))` のようなクエリを実行すると、正しい答えが得られる前に `"function q/1 undefined"` のような警告メッセージが表示されることがあります。
問題は、各関数が個別にコンパイルされるため、関数 `q/1` が後で定義される場合でも、`p/1` のコンパイル中に検出された警告がすぐに出力されることです。
ANSI Common Lisp には、一連のコンパイルが完了するまで警告の出力を遅らせる方法があります。コンパイルをマクロ `with-compilation-unit` でラップします。
実装でこのマクロが提供されない場合でも、別の名前で同じ機能が提供される場合があります。
実装で `with-compilation-unit` がすでに定義されているか、または定義できるかどうかを確認してみてください。

## 12.7 コンパイラのベンチマーク

コンパイルされた Prolog コードは、シマウマ パズルを 17.4 秒で実行します。これは、解釈されたバージョンに比べて 16 倍高速で、速度は 740 LIPS です。

もう一つの一般的なベンチマークは Lisp の `reverse` 関数で、これは `rev` 関係としてコード化できます。

```lisp
(<- (rev () ()))
(<- (rev (?x . ?a) ?b) (rev ?a ?c) (concat ?c (?x) ?b))

(<- (concat () ?1 ?1)
(<- (concat (?x . ?a) ?b (?x . ?c)) (concat ?a ?b ?c))
```

`rev` は連結を表す関係 `concat` を使用します。
`(concat ?a ?b ?c)` は、`?a` を `?b` に連結すると `?c` になるときに true になります。
この関係に似た名前は、append のようなより手続き的な名前よりも優先されます。
しかし、`rev` は次の Lisp 定義と非常によく似ています。

```lisp
(defun rev (1)
  (if (null 1)
    nil
    (app (rev (rest 1 ))
        (list (first 1)))))

(defun app (x y)
  (if (null x)
    y
      (cons (first x)
        (app (rest x) y))))

```

どちらのバージョンも非効率的です。
追加のconsingを行わず、末尾再帰である `reverse` の反復バージョンを記述することは可能です。

```lisp
(<- (irev ?l ?r) (irev3 ?l () ?r))
(<- (irev3 (?x . ?l) ?so-far ?r) (irev3 ?l (?x . ?so-far) ?r))
(<- (irev3 () ?r ?r))

```

Prolog の `irev` は次の Lisp プログラムと同等です。

```lisp
(defun irev (list) (irev2 list nil))

(defun irev2 (list so-far)
  (if (consp list)
      (irev2 (rest list) (cons (first list) so-far))
      so-far))
```

次の表は、Prolog と Lisp の両方で、長さ 20 と 100 のリストに対してこれらのルーチンをインタプリター上とコンパイラで実行するのにかかる時間 (秒単位) を示しています。
(コンパイルされた Lisp だけが、スタック領域を使い果たすことなく 100 要素のリストで rev を実行できました。) このプログラムの Lisp バージョンはありませんが、シマウマ パズルの時間も含まれています。

| 問題 | Prolog のインタプリター | Prolog のコンパイル | スピードアップ | Lisp のインタプリター | Lisp のコンパイル |
|-----------|----------------|--------------|---------------|--------------|----------|
| `シマウマ` | 278.000 | 17.241 | 16 | - | - |
| `rev 20` | 4.24 | .208 | 20 | .241 | .0023 |
| `回転数 100` | - | - | - | - | .0614 |
| `rev 20` | .22 | .010 | 22 | .028 | .0005 |
| `rev 100` | 9.81 | .054 | 181 | .139 | .0014 |

このベンチマークは結論付けるには不十分ですが、これらの例では、Prolog コンパイラは Prolog インタープリタよりも 16 ～ 181 倍高速で、インタープリタ型 Lisp よりもわずかに高速ですが、コンパイル型 Lisp よりも 17 ～ 90 倍遅いです。
これは、Prolog インタープリタは実用的なプログラミングツールとして使用できないもの、Prolog コンパイラは使用できることを示唆しています。

先に進む前に、Prolog がオプションの引数を自動的に提供することに注目してください。
オプション引数には特別な構文はありませんが、よく使用される規則では、関係に2 つのバージョン (1 つは *n* 個の引数、もう 1 つは *n* - 1 個) があります。
*n* - 1 の場合の単一の節では、欠落している、つまり「オプショナルな」引数を提供します。
次の例では、`irev/2` は、省略可能な引数が () である `irev/3` のバージョンと見なすことができます。

```lisp
(<- (irev ?l ?r) (irev ?l () ?r))
(<- (irev (?x . ?l ) ?so-far ?r) (irev ?l (?x . ?so-far) ?r))
(<- (irev () ?r ?r))
```

これは、次の Lisp バージョンとほぼ同等です。

```lisp
(defun irev (list &optional (so-far nil))
  (if (consp list)
      (irev (rest list) (cons (first list) so-far))
      so-far))
```

## 12.8 プリミティブの追加

Lisp コンパイラが入出力や演算などを実行するためにマシン命令を必要とするのと同様に、Prolog システムも特定の基本的なアクションを実行できる必要があります。
Prolog インタープリタの場合、プリミティブは関数シンボルによって実装されました。
インタープリタが節のリストを取得しようとしたときに、代わりに関数を取得した場合は、その関数を呼び出して、現在の関係、現在の束縛、および満たされていない目標のリストへの引数を渡します。
Prolog コンパイラの場合、継続を最終引数として取るという規則に従い、*symbol/arity* という形式の名前を持つ Lisp 関数を記述するだけで、プリミティブをインストールできます。たとえば、入力と出力を処理する簡単な方法を次に示します。

```lisp
(defun read/1 (exp cont)
 (if (unify! exp (read))
   (funcall cont)))
(defun write/1 (exp cont)
 (write (deref-exp exp) :pretty t)
 (funcall cont))
```

`(write ?x)` の呼び出しは常に成功するので、継続は常に呼び出されます。
同様に、`(read ?x)` を使用して値を読み取り、それを `?x` と単一化することもできます。
`?x` が束縛されていない場合、これは値を割り当てることと同じです。
しかしながら、`(read (?x + ?y))` のような呼び出しを行うことも可能で、これは入力が中央に + がある 3 要素のリストである場合にのみ成功します。
どのストリームを使用するかを示す関係として `read/2` と `write/2` を定義するのは簡単な拡張となります。
これを便利にするには、パス名を1 つの引数として受け取り、ストリームをもう 1 つの引数として返す関係として `open/2` を定義する必要があります。
必要に応じて、他のオプションの引数もサポートされます。

プリミティブ `nl` は改行を出力します。

```lisp
(defun nl/0 (cont) (terpri) (funcall cont))
```

単一化述語`=`に特別なサポートを提供しましたが、`=/2` の単純な定義を使用することで、コンパイラを大幅に簡素化できます。

```lisp
(defun =/2 (?arg1 ?arg2 cont)
 (if (unify! ?arg1 ?arg2)
  (funcall cont)))
```

実際、コンパイラに次の単一の節を与えると、

`(<- (= ?x ?x))`

`=/2` の定義に対してこのコードが生成されます。
他にも考慮すべき等価述語があります。
述語 `==/2` はLisp ではequal に似ています。
単一化は行いませんが、代わりに2 つの構造がその要素に関して等しいかどうかをテストします。
変数はそれ自身とのみ等しいとみなされます。
実装は次のとおりです。

```lisp
(defun =/2 (?arg1 ?arg2 cont)
 "Are the two arguments EQUAL with no unification,
 but with dereferencing? If so, succeed."
 (if (deref-equal ?arg1 ?arg2)
  (funcall cont)))
(defun deref-equal (x y)
 "Are the two arguments EQUAL with no unification,
 but with dereferencing?"
 (or (eql (deref x) (deref y))
  (and (consp x)
   (consp y)
   (deref-equal (first x) (first y))
   (deref-equal (rest x) (rest y)))))
```

最も重要なプリミティブの 1 つは `call` です。
Lisp の `funcall` と同様に、`call` を使用すると目標を構築し、それを証明することができます。

```lisp
(defun call/1 (goal cont)
  "Try to prove goal by calling it."
  (deref goal)
  (apply (make-predicate (first goal)
          (length (args goal)))
      (append (args goal) (list cont))))

```

このバージョンの `call` では、最初の要素が適切に定義された述語であるリストに目標がインスタンス化されていない場合、実行時エラーが発生します。これを確認し、定義された述語がない場合は何も表示せずに失敗するようにすると良いでしょう。
目標が正当な場合の `call` の例を次に示します。

```lisp
> (?- (= ?p member) (call (?p ?x (a b c))))
?P = MEMBER
?X = A;
?P = MEMBER
?X = B;
?P = MEMBER
?X = C;
No.
```

`call` ができたので、多くの新しいことを実装できるようになりました。
論理接続詞 and と or は次のとおりです。

```lisp
(<- (or ?a ?b) (call ?a))
(<- (or ?a ?b) (call ?b))

(<- (and ?a ?b) (call ?a) (call ?b))

```

これらは二項接続子のみであり、Lisp で使用される *n* 項特殊形式ではないことに注意してください。
また、この定義はコンパイルの利点のほとんどを無効にします。
`and` または `or` 内の目標は、コンパイルされるのではなく、`call` によって解釈されます。

加えて、`not`、あるいは少なくとも論理的な`not`とはまったく異なる通常の Prolog の`not`を定義することもできます。
実際、いくつかの方言では `not` は `\+` と書かれますが、これは ⊬、つまり「導出できない」という意味になります。
解釈としては、目標 G が証明できない場合は (`not G` ) が真となります。
論理的には、 (`not G` ) が真であることと未知であることの間には違いがありますが、その違いを無視すると、Prolog はより実用的なプログラミング言語になります。
Prologにおける否定の形式意味論の詳細については、[Lloyd 1987](bibliography.md#bb0745)を参照してください。

以下は `not/1` の実装です。
証跡を操作する必要があり、同じことを行う他の述語がある可能性があるため、`maybe-add-undo-bindings` で行われたことをマクロ `with-undo-bindings:` にパッケージ化します。

```lisp
(defmacro with-undo-bindings (&body body)
  "Undo bindings after each expression in body except the last."
  (if (length=1 body)
      (first body)
      '(let ((old-trail (fill-pointer *trail*)))
         ,(first body)
          ,@(loop for exp in (rest body)
                  collect '(undo-bindings! old-trail)
                  collect exp))))
(defun not/1 (relation cont)
  "Negation by failure: If you can't prove G. then (not G) true."
  ;; Either way, undo the bindings.
  (with-undo-bindings
    (call/1 relation #'(lambda () (return-from not/1 nil)))
    (funcall cont)))

```
以下は `not` が正常に動作する例です:

```lisp
> (?- (member ?x (a b c)) (not (= ?x b)))
?X = A;
?X = C;
No.
```

ここで、2 つの目標の順序を逆にすると何が起こるかを見てみましょう。

```lisp
> (?- (not (= ?x b)) (member ?x (a b c)))
No.
```

最初の例は、`?x` が `b` に束縛されていない限り成功します。
2 番目の例では、`?x` は開始時に束縛されていないため、`(= ?x b )` は成功し、`not` は失敗し、`member` 目標には到達しません。
したがって、`not` の実装には一貫した手続き的解釈がありますが、これは論理否定に通常与えられる宣言的解釈と同等ではありません。
通常、目標の順序に関係なく、`a` と `c` がクエリに対する有効な解法になると予想されます。

Prolog と Lisp の基本的な違いの 1 つは、Prolog が関係的であることです。つまり、個々の関係を簡単に表現できます。
一方、Lisp は、物事のコレクションをリストとして表現するのに優れています。
これまでのところ、Prolog で関係を満たすオブジェクトのコレクションを形成する方法はありません。
オブジェクトを反復処理するのは簡単ですが、それらをまとめることはできません。
プリミティブな `bagof` はコレクションを実行する 1 つの方法です。
一般に、`(bagof ?x (p ?x) ?bag)` は、`?bag` を、`(p ?x)` を満たすすべての `?x's` のリストと単一化します。
そのような `?x's` がない場合、 `bagof` の呼び出しは失敗します。
*バッグ* は、重複が許可された順序なしのコレクションです。
たとえば、バッグ {*a*, *b, a*} はバッグ {*a*, *a*, *b*} と同じですが、{*a*, *b*} とは異なります。
バッグは、重複のない順序なしのコレクションである *セット* とは対照的です。
集合 {*a*, *b*} は集合 {*b*, *a*} と同じです。
以下は `bagof` の実装です:

```lisp
(defun bagof/3 (exp goal result cont)
 "Find all solutions to GOAL, and for each solution,
 collect the value of EXP into the list RESULT."
 ;; Ex: Assume (p 1) (p 2) (p 3). Then:
 ;: (bagof ?x (p ?x) ?1) => ?1 = (1 2 3)
 (let ((answers nil))
 (call/1 goal #'(lambda ()
   (push (deref-copy exp) answers)))
 (if (and (not (null answers))
  (unify! result (nreverse answers)))
 (funcall cont))))
 (defun deref-copy (exp)
 "Copy the expression, replacing variables with new ones.
 The part without variables can be returned as is."
 (sublis (mapcar #'(lambda (var) (cons (deref var) (?))
  (unique-find-anywhere-if #'var-p exp))
 exp))

```

以下では、`bagof` を使用して、Sandy が好きな人のリストを収集します。
結果はセットではなくバッグであることに注意してください。Sandy は複数回表示されます。

```lisp
> (?- (bagof ?who (likes Sandy ?who) ?bag))
?WHO = SANDY
?BAG = (LEE KIM ROBIN SANDY CATS SANDY);
No.
```

次の例では、`A` と `B` をメンバーとして持つ長さ 3 のすべてのリストのバッグを作成します。

```lisp
> (?- (bagof ?l (and (length ?l (1  + (1  + (1  + 0))))
      (and (member a ?l) (member b ?l)))
    ?bag))
?L = (?5 ?8 ?11 ?68 ?66)
?BAG = ((A B ?17) (A ?21 B) (B A ?31) (?38 A B) (B ?48 A) (?52 B A))
No.

```
同じ答えの複数のバージョンを含むバッグに失望した人は、`bagof` と同じ計算を行い、重複を破棄するプリミティブ `setof` を好むかもしれません。

```lisp
(defun setof/3 (exp goal result cont)
 "Find all unique solutions to GOAL, and for each solution,
 collect the value of EXP into the list RESULT."
 ;; Ex: Assume (p 1) (p 2) (p 3). Then:
 ;; (setof ?x (p ?x) ?l ) => ?l = (1 2 3)
 (let ((answers nil))
 (call/1 goal #'(lambda ()
   (push (deref-copy exp) answers)))
 (if (and (not (null answers))
  (unify! result (delete-duplicates
    answers
    :test #'deref-equal)))
 (funcall cont))))
```

Prolog は演算子 `is` を使用した算術演算をサポートしています。
たとえば、`(is ?x (+ ?y 1))` は、`?x` を `?y` の値に 1 を加えた値と単一化します。
この式は、`?y` が束縛されていない場合は失敗し、`?y` が数値でない場合は実行時エラーが発生します。
私たちのバージョンの Prolog では、算術演算だけでなく、あらゆる Lisp 式もサポートできます。

```lisp
(defun is/2 (var exp cont)
 ;; Example: (is ?x (+ 3 (* ?y (+ ?z 4))))
 ;; Or even: (is (?x ?y ?x) (cons (first ?z) ?l))
 (if (and (not (find-if-anywhere #'unbound-var-p exp))
  (unify! var (eval (deref-exp exp))))
 (funcall cont)))
(defun unbound-var-p (exp)
 "Is EXP an unbound var?"
 (and (var-p exp) (not (bound-p exp))))
 ```

おまけとして、Prolog プログラマーが関数 `unbound-var-p` を使えるようにしてもよいでしょう。
この述語の標準名は `var/1` です。

```lisp
(defun var/1 (?arg1 cont)
  "Succeeds if ?arg1 is an uninstantiated variable."
  (if (unbound-var-p ?arg1)
  (funcall cont)))
```

2 番目の引数の一部が束縛されていない場合、 is プリミティブは失敗します。
しかしながら、たとえ`eval` を直接呼び出すことはできなかったとしても、解くことができる変数を含む式があります。
たとえば、次の目標は、`?x` を `2` に束縛することで解決できます。

```lisp
(solve (=  12 (* (+ ?x 1) 4)))
```

Prolog から Lisp にもっと直接アクセスしたいと思うかもしれません。
`is` の問題は、束縛されていない変数のチェックが必要であり、引数を再帰的に評価するために `eval` を呼び出すことです。
場合によっては、is が提供するセーフティネットを経由せずに、Lisp の `apply` だけを実行したいことがあります。
プリミティブな `lisp` がそれを行います。
言うまでもなく、`lisp` は標準 Prolog の一部ではありません。

```lisp
(defun lisp/2 (?result exp cont)
 "Apply (first exp) to (rest exp), and return the result."
 (if (and (consp (deref exp))
  (unify! ?result (apply (first exp) (rest exp))))
 (funcall cont)))

```

**演習12.7 [m]** 学生の演習([ページ 225](chapter7.md#p225))
で使用した関数 `solve` と同じように動作するプリミティブ `solve/1` を定義してください。 引数として単一の方程式を取るか、方程式のリストを取るかを考えてみてください。

**演習 12.8 [h]** `(solve (= 12 (* (+ ?x 1) 4)))` という形式の目標があると仮定します。
実行時に `solve/1` が呼び出されたときに方程式を操作するのではなく、呼び出しを `(solve (= ?x 2))` であるかのように扱い、コンパイル時に作業の一部を実行することを好む場合があります。
`solve` 用の Prolog コンパイラ マクロを記述してください。
コンパイラ マクロを定義した場合でも、述語が `call/1` を通じて呼び出される可能性があるため、基礎となるプリミティブが必要であることに注意してください。
Lisp でも同じことが起こります。コンパイラ マクロを提供する場合でも、`funcall` または `apply` の場合は実際の関数が必要になります。

**演習 12.9 [h]** 述語 `call`、`and`、`or`、`not`、`repeat` のうち、コンパイラ マクロの恩恵を受けられるのはどれですか。
使用できる述語に対してコンパイラ マクロを記述してください。

**演習 12.10 [m]** `call/1` が 2 つの重要な点で非効率的であることに気づいたかもしれません。
まず、`make-predicate` を呼び出します。これは、文字列を追加してシンボルを構築し、その文字列を Lisp シンボル テーブルで検索する必要があります。
`make-predicate` を変更して、述語シンボルが最初に作成されたときにそれを保存し、後続の呼び出しでより高速に検索できるようにします。
2 番目の非効率性は、append の呼び出しです。
継続引数が最後ではなく最初に来るようにコンパイラ全体を変更し、`call` で追加しないで済むように書き換えてください。。

**演習 12.11 [s]** プリミティブ `true/0` は常に成功し、 `fail/0` は常に失敗します。
これらのプリミティブを定義してください。
ヒント: 最初のものは Common Lisp 関数に対応し、2 番目はこの章ですでに定義されている関数です。

**演習 12.12 [s]** `==/2` をプリミティブではなく節のリストとして記述することは可能でしょうか?

**演習 12.13 [m]** 引数式を 1 回だけ走査する `deref-copy` のバージョンを記述してください。

## 12.9 カット

Lisp では、バックトラックポイントが 1 つまたは 2 つ以上ある場合は扱いにくいものの、明示的にバックトラックするプログラムを記述できます。
Prolog では、バックトラックは自動的かつ暗黙的に行われますが、バックトラックを回避する方法はまだわかっていません。
Prolog プログラマがバックトラックを無効にしたい理由は 2 つあります。
まず、バックトラックポイントを追跡するには時間とメモリが必要です。
ある問題には 1 つの解決策しかないことを知っているプログラマーは、プログラムに他の可能な分岐を考慮しないように指示することで計算を高速化できるはずです。
第二に、問題を単純に論理的に指定すると、冗長な解決策や、意図しない解決策が生成されることがあります。
単に検索空間を整理してバックトラッキングをなくすと、必要な答えだけが得られるかもしれませんが、すべての答えと正しい答えだけが得られるようにプログラムを再構築するのはより困難でしょう。
ここに例があります。
3 番目の引数が最初の 2 つの引数の最大値である場合に成立する述語 `max/3` を定義したいとします。ここで、最初の 2 つの引数は常に数値にインスタンス化されます。
簡単な定義は次のとおりです。

```lisp
(<- (max ?x ?y ?x) (>= ?x ?y))
(<- (max ?x ?y ?y) (< ?x ?y))
```

宣言的にはこれは正しいのですが、手続き的には、`>=` が成功した場合に `<` 関係を計算するのは時間の無駄です。その場合、`<` は決して成功しません。
`!` と書かれたカット記号を使用すると、無駄な計算を停止できます。
次のように書くこともできます。

```lisp
(<- (max ?x ?y ?x) (>= ?x ?y) !)
(<- (max ?x ?y ?y))
```

最初の節のカットは、最初の節が成功した場合、他の節は考慮されないことを示しています。
したがって、2 番目の節は単独では解釈できません。
むしろ、これは「最初の節が失敗した場合、2 つの数値の `max` は 2 番目の数値になる」と解釈されます。

一般的に、カットは節の最後だけでなく、節bodyのどこにでも発生する可能性があります。
カットについては適切な宣言的解釈はありませんが、手続き的解釈は 2 つあります。
まず、カットが目標として「実行」されると、それは必ず成功します。
しかし、成功するだけでなく、後戻りしても越えられない柵も設置されます。
カットは、カットの右側の目標 (同じ節内) とカットの下の節 (同じ述語内) の両方からのバックトラッキングを遮断する役割を果たします。
より抽象的な例を見てみましょう。

```lisp
(<- (p) (q) (r) ! (s) (t))
(<- (p) (s))
```

`p` の最初の節を処理する際、`q` と `r` を解決しようとしている間にバックトラックが自由に発生する可能性があります。
`r` が解決されると、カットに遭遇します。
その時点から、`s` と `t` を解決しながらバックトラックが自由に発生する可能性がありますが、Prolog はカットを超えて `r` にバックトラックすることはなく、2 番目の節も考慮されません。
一方、`q` または `r` が失敗した場合 (カットに遭遇する前)、Prolog は 2 番目の節に進みます。

カットの意図が明確になったので、それをどのように実装するかを考えてみましょう。
変数と複数のカットを含む、もう少し複雑な述語を見てみましょう。

```lisp
(<- (p ?x a) ! (q ?x))
(<- (p ?x b) (r ?x) ! (s ?x))
```

カットに戻るとすぐに、それ以上の目標は考慮されないように調整する必要があります。
最初の節では、`q/1` が失敗した場合、2 番目の節を考慮せずに、すぐに `p/2` から戻りたいと考えています。
同様に、`s/1` が初めて失敗したときは、`r/1` の他の解決策を検討するのではなく、`p/2` から戻りたいと考えます。
したがって、次のようなコードが必要になります。

```lisp
(defun p/2 (argl arg2 cont)
 (let ((old-trail (fill-pointer *trail*)))
  (if (unify! arg2 'a)
   (progn (q/1 argl cont)
     (return-from p/2 nil)))
  (undo-bindings! old-trail)
  (if (unify! arg2 'b)
   (r/1 argl #'(lambda ()
       (progn (s/1 argl cont)
        (return-from p/2 nil)))))))
```

このコードは、`compile-body` に 1 つの変更を加えることで取得できます。bodyの最初のゴール (またはbodyの残りの部分) がカット シンボルである場合、残りのbodyのコードを含む `progn` を生成し、その後にコンパイルされる述語の `return-from` を生成しなければなりません。
残念ながら、述語の名前は `compile-body` では使用できません。
`compile-clause` と `compile-body` を変更して述語名を追加の引数として受け取るようにしたり、述語を `compile-predicate` 内の特殊変数として束縛したりすることができます。
私は後者を選びます:

```lisp
(defvar *predicate* nil
  "The Prolog predicate currently being compiled")

(defun compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
  into a single LISP function."
  (let ((*predicate* (make-predicate symbol arity))    ;***
        (parameters (make-parameters arity)))
    (compile
     (eval
      `(defun ,*predicate* (,@parameters cont)
  .,(maybe-add-undo-bindings
     (mapcar #'(lambda (clause)
           (compile-clause parameters clause 'cont))
      clauses)))))))

(defun compile-body (body cont bindings)
  "Compile the body of a clause."
  (cond
    ((null body)
     `(funcall ,cont))
    ((eq (first body) '!)                              ;***
     `(progn ,(compile-body (rest body) cont bindings) ;***
             (return-from ,*predicate* nil)))          ;***
    (t (let* ((goal (first body))
              (macro (prolog-compiler-macro (predicate goal)))
              (macro-val (if macro
                             (funcall macro goal (rest body)
                                      cont bindings))))
        (if (and macro (not (eq macro-val :pass)))
            macro-val
            `(,(make-predicate (predicate goal)
                               (relation-arity goal))
              ,@(mapcar #'(lambda (arg)
                            (compile-arg arg bindings))
                        (args goal))
              ,(if (null (rest body))
                   cont
                   `#'(lambda ()
                        ,(compile-body
                           (rest body) cont
                           (bind-new-variables bindings goal))))))))))
```

**演習 12.14 [m]** 以下の定義に基づいて、`test-cut` の呼び出しが何を実行し、何が書き込まれるかを考えましょう。

```lisp
(<- (test-cut) (p a) (p b) ! (p c) (p d))
(<- (test-cut) (p e))
(<- (p ?x) (write (?x 1)))
(<- (p ?x) (write (?x 2)))
```

カットを使用する別の方法は、*繰り返し/失敗*ループです。
述語 `repeat` は次の 2 つの節で定義されます。

```lisp
(<- (repeat))
(<- (repeat) (repeat))
```

プリミティブとしての別の定義は次のとおりです。

```lisp
(defun repeat/0 (cont)
  (loop (funcall cont)))
  ```

残念ながら、`repeat`は最も乱用される述語の 1 つです。
いくつかの Prolog の本では、次のようなプログラムが紹介されています。

```lisp
(<- (main)
  (write "Hello.")
  (repeat)
  (write "Command: ")
  (read ?command)
  (process ?command)
  (= ?command exit)
  (write "Good bye."))
  ```

目的は、コマンドを 1 つずつ読み取って処理することです。
`exit` 以外の各コマンドに対して、`process` は適切なアクションを実行してから失敗します。
これにより、繰り返し目標までバックトラックされ、新しいコマンドが読み取られて処理されます。
コマンドが `exit` の場合、プロシージャは戻ります。

このプログラムが不十分な理由は 2 つあります。
まず、参照透明性の原則に違反します。
似ているものは、使用される文脈に関係なく、似ているはずです。
しかし、ここでは、bodyの 6 つの目標のうち 4 つがループを構成し、残りの目標がループの外側にあることを知る方法はありません。
第二に、抽象化の原則に違反します。
述語は個別の単位として理解できる必要があります。
しかし、ここで述語プロセスを理解するには、それが呼び出されるコンテキスト、つまり各コマンドの処理後に失敗することを要求するコンテキストを考慮する必要があります。
[Richard O'Keefe 1990](bibliography.md#bb0925)が指摘しているように、この節の正しい書き方は次のとおりです。

```lisp
(<- (main)
  (write "Hello.")
  (repeat)
      (write "Command: ")
      (read ?command)
      (process ?command)
      (or (= ?command exit) (fail))
  !
  (write "Good bye."))
  ```

インデントにより、繰り返しループの制限が明確に示されます。
ループは明示的なテストによって終了され、その後にカットが続くため、呼び出し元のプログラムは、終了した後に誤ってループに戻ることはありません。
個人的には、括弧によってループなどの構造が明示的になり、インデントが自動的に行われる Lisp のような言語を好みます。
しかし、オキーフは、よく構造化された読みやすいプログラムを Prolog で記述できることを示しています。

if-then および if-then-else 構文は、節として簡単に記述できます。
if-then-else は、テストが満たされた場合にカットを使用して `then` 部分にコミットすることに注意してください。

```lisp
(<- (if ?test ?then) (if ?then ?else (fail)))
(<- (if ?test ?then ?else)
  (call ?test)
  !
  (call ?then))
(<- (if ?test ?then ?else)
  (call ?else))

```

カットは非論理学的な`not`を実装するために使用できます。
`not` の定義として、次の 2 つの節がよく使用されます。
私たちのコンパイラは、これらの 2 つの節を、プリミティブ `not/1` に対して以前に与えられたものとまったく同じコードに変換します。

```lisp
(<- (not ?p) (call ?p) ! (fail))
(<- (not ?p))
```

## 12.10 「実際の」プロローグ

この章で開発される Prolog-In-Lisp システムは、Lisp システムに組み込むことを目的としているため、Lisp 構文を使用します。
Lisp 構文を使用する他の Prolog 実装には、micro-Prolog、Symbolics Prolog、LMI Prolog などがあります。

ただし、Prolog システムの大部分は、従来の数学表記に近い構文を使用します。
次の表は、「標準」Prolog の構文と Prolog-In-Lisp の構文を比較したものです。
現在、Prolog の標準化に取り組んでいる国際委員会がありますが、最終報告書はまだ発表されていないため、方言によって構文が若干異なる場合があります。
ただし、ほとんどの実装はここで要約した表記法に従います。
これらは、エディンバラ大学の David H. が DEC-10 用に開発した Prolog から派生したものです。
D.
ウォーレンと彼の同僚たち。
最後のセクションのプリミティブの名前も、Edinburgh Prolog から取られています。

| | Prolog | Prolog-In-Lisp |
|----------|------|--------------|
| アトム | `lower` | `const` |
|変数 | `Upper` | `?var` |
| 匿名 | `-` | `?` |
| 目標 | `p(Var,const)` | `(p ?var const)` |
| ルール | `p(X) :- q(X).` | `(<- (p ?x) (q ?x))` |
| 事実 | `p(a).` | `(<- (pa))` |
| クエリ | `?- p(X).` | `(?- (p ?x))` |
| リスト | `[a,b,c]` | `(a b c)` |
| cons | `[a\| Rest]` | `(a . ?rest)` |
| nil | `[]` | `()` |
| and | `p(X). q(X)` | `(and (p ?x) (q ?x))` |
| or | `P(X): q(X)` | `(or (p ?x) (q ?x))` |
| not | `\+ p(X)` | `(not (p ?x))` |

私たちは Lisp のリスト重視の考え方を採用しました。つまり、項はアトム、変数、および他の項のコンスから構築されます。
実際の Prolog ではconsセルが提供されますが、項は通常リストではなく *構造* から構築されます。
Prolog の項 `p(a,b)` は、リスト `(pab)` ではなく、Lisp ベクトル `#(p/2 ab)` に対応します。
Prolog 実装の少数では *構造共有* を使用します。このアプローチでは、すべての非アトミック項が、変数のプレースホルダーを含むスケルトンと、スケルトンを指してプレースホルダーを埋める変数も含むヘッダーによって表されます。
構造共有を使用すると、コピーの作成は簡単です。スケルトンのサイズに関係なく、ヘッダーをコピーするだけです。
ただし、用語の操作は、スケルトンとヘッダーの両方を追跡する必要があるため複雑になります。
構造共有の詳細については[Boyer and Moore 1972](bibliography.md#bb0110)を参照してください。

もう 1 つの大きな違いは、実際の Prolog では成功継続ではなく失敗継続と同等のものが使用されることです。
クロージャという意味での実際の継続は構築されません。
代わりに、選択が行われると、次の選択のコード アドレスがスタックにプッシュされます。
失敗すると、次の選択肢がスタックからポップされます。
これは、[ページ 772](chapter22.md#p772) で概説されている Scheme の `call/cc` 機能を使用したバックトラッキング アプローチを彷彿とさせます。

**演習 12.15 [m]** 成功継続の代わりに失敗継続のスタックを使用するアプローチを想定して、`p` と `member` のコードがどのようになるかを示してください。
失敗の継続を渡す必要がないことに注意してください。`top-level-prove` が呼び出すスタックにプッシュするだけで済みます。
カットはどのように実施されるのでしょうか?
成功継続を使用してコンパイラを実装するというのは正しい選択をだったのでしょうか、それとも失敗継続の方がよかったのでしょうか?

## 12.11 歴史と参考文献

[第 11 章](chapter11.md) で説明したように、論理プログラミングの考え方は 1970 年代半ばまでにかなりよく理解されていました。
しかし、当時の実装は遅かったため、論理プログラミングは普及しませんでした。
論理プログラミングを Lisp やその他の汎用言語に代わる本格的な選択肢にしたのは、DEC-10 用の Prolog コンパイラでした。
このコンパイラは 1977 年に David H.D.ウォーレンとフェルナンド・ペレイラ、ルイス・ペレイラによって開発されました。
[Warren (1979)](bibliography.md#bb1325) および 3 名全員 (1977) による論文を参照してください。

残念ながら、David H.D.ウォーレンの Prolog コンパイルに関する先駆的な研究は、これまで広くアクセス可能な形で公開されたことはありません。
彼の主な貢献は、コンパイルされた Prolog の命令セットである Warren Abstract Machine (WAM) の説明でした。
既存のコンパイラのほとんどは、この命令セット、またはそれに若干の修正を加えたものを使用します。
これは、バイトコードの解釈またはネイティブマシン命令へのマクロ展開を通じて実行できます。
[Aït-Kaci 1991](bibliography.md#bb0020)はWAMに関する優れたチュートリアルを提供しているが、オリジナル([Warren 1983](bibliography.md#bb1330))よりもはるかに簡潔です。
この章で紹介するコンパイラは WAM を使用していません。
代わりに、これは Mark [Stickel (1988)](bibliography.md#bb1200) の定理証明器をモデルにしています。
同様のコンパイラは、Jacques [Cohen 1985](bibliography.md#bb0225)によって簡単に説明されています。

## 12.12 演習

**演習 12.16 [m]** 暗黙の `calls` を許可するように Prolog コンパイラを変更してください。
つまり、目標が述語で始まるcons セルでない場合は、それを `call` であるかのようにコンパイルします。
節:

```lisp
(<- (p ?x ?y) (?x c) ?y)
```

次のようにコンパイルする必要があります:

```lisp
(<- (p ?x ?y) (call (?x c)) (call ?y))
```

**演習 12.17 [h]** 以下に、いくつかの標準的な Prolog プリミティブを示します。

* `get/1` 1文字を読み取り、引数と単一化します。

* `put/1` 1文字を出力します。

* `nonvar/1, /=, /==` それぞれ `var, = および = =` の反対です。

* `integer/1` 引数が整数の場合は True になります。

* `atom/1` 引数がシンボル（Lisp の `symbol p` など）の場合は True になります。

* `atomic/1` 引数が数値またはシンボル（Lisp の `atom` など）の場合は True になります。

* `<`、`>`、`=<`、`>=` 算術比較。引数が両方とも数値にインスタンス化され、比較が真の場合に成功します。

* `listing/0` 定義されたすべての述語の節を出力します。

* `listing/1` 引数述語の節を出力します。

これらの述語を実装してください。
それぞれのケースで、述語をプリミティブとして実装するか、節のリストとして実装するか、およびコンパイラ マクロを使用するかどうかを決定してください。

解決する必要がある名前の競合がいくつかあります。
`atom`のような用語は、Prolog ではある意味を持ち、Lisp では別の意味を持ちます。
また、Prolog での通常の表記は `/=` と `/==` ではなく `\=` と `\==` です。
Prolog-In-Lisp の場合、Prolog と Lisp のどちらの表記法を使用するかを決定しなければなりません。

**演習 12.18 [s]** Lispでは、`(< 1 n 10 )`や`(= x y z)`のようなn項呼び出しを書くことに慣れています。
n 項呼び出しを一連の 2 項呼び出しに展開するコンパイラ マクロを記述してください。
たとえば、`(< 1 n 10)` は `(and (< 1 n) (< n 10))` に展開されます。

**演習 12.19 [m]** Prolog にはない Lisp の機能の 1 つは、`quote` メカニズムです。
`quote` には用途がありますか? ある場合は実装してください。ない場合は、なぜ必要ないのか説明してください。

**演習 12.20 [h]** Prolog のトレース機構を記述します。
Prolog 述語をトレースおよびトレース解除するための手順 `p-trace` および `p-untrace` を追加します。
トレースされた目標の、表示プロシージャへの呼び出しを生成するコードをコンパイラに追加してください。
Lisp では、プロシージャが呼び出されたときと戻ったときを追跡する必要があります。
Prolog では、呼び出し、正常完了、後続の節へのバックトラック、およびそれ以上の節がない失敗という 4 つのケースを考慮する必要があります。
これら 4 つのケースをそれぞれ `call`、`exit`、`redo`、`fail` と呼びます。
`member` をトレースすると、トレース出力は次のようになると予想されます。

```lisp
> (?- (member ?x (a b c d)) (fail))
  CALL MEMBER: ?1 (A B C D)
  EXIT MEMBER: A (A B C D)
  REDO MEMBER: ?1 (A B C D)
    CALL MEMBER: ?1 (B C D)
    EXIT MEMBER: B (B C D)
    REDO MEMBER: ?1 (B C D)
      CALL MEMBER: ?1 (C D)
      EXIT MEMBER: C (C D)
      REDO MEMBER: ?1 (C D)
        CALL MEMBER: ?1 (D)
        EXIT MEMBER: D (D)
        REDO MEMBER: ?1 (D)
          CALL MEMBER: ?1 NIL
          REDO MEMBER: ?1 NIL
          FAIL MEMBER: ?1 NIL
        FAIL MEMBER: ?1 (D)
      FAIL MEMBER: ?1 (C D)
    FAIL MEMBER: ?1 (B C D)
  FAIL MEMBER: ?1 (A B C D)
No.
```

**演習 12.21 [m]** 一部の Lisp システムでは関数のコンパイルが非常に遅くなります。
`KCL` は例です。`C` に変換してから `C` コンパイラとアセンブラを呼び出してコンパイルします。
`KCL` では、完全にデバッグされたコードのみをコンパイルし、プログラムの開発中に解釈されて実行するのが最適です。

Lisp コンパイラの呼び出しがオプションになるように Prolog コンパイラを変更してください。
いずれの場合も、Prolog 関数は Lisp に変換されますが、変数が設定されている場合にのみマシン語にコンパイルされます。

**演習 12.22 [d]** 一部の Prolog システムでは、変数がインスタンス化されるまで目標を「凍結」する述語 `freeze` が提供されています。
たとえば、目標 `(freeze x (> x 0))` は次のように解釈されます: `x` がインスタンス化されると、目標 `(> x 0)` を評価し、結果に応じて成功または失敗します。
ただし、`x` が束縛されていない場合は成功して計算を続行しますが、目標 `(> x 0)` を記憶し、`x` がインスタンス化されるとすぐにそれを評価します。
`freeze` を実装してください。

**演習 12.23 [m]** ローカル関数を使用しない `anonymous-variables-in` の再帰バージョンを記述してください。

## 12.13 回答

**回答 12.6** Texas Instruments および Lucid 実装で動作するバージョンは次のとおりです。

```lisp
(defmacro with-compilation-unit (options &body body)
  "Do the body, but delay compiler warnings until the end."
  ;; This is defined in Common Lisp the Language, 2nd ed.
  '(,(read-time-case
    #+TI 'compiler:compiler-warnings-context-bind
    #+Lucid 'with-deferred-warnings
        'progn)
    .,body))

(defun prolog-compile-symbols (&optional (symbols *uncompiled*))
  "Compile a list of Prolog symbols.
  By default, the list is all symbols that need it."
  (with-compilation-unit ()
  (mapc #'prolog-compile symbols)
  (setf *uncompiled* (set-difference *uncompiled* symbols))))
```

**回答 12.9** `and` および `or` のマクロはよく使用されるため、非常に重要です。
`and` のマクロは簡単です:

```lisp
(def-prolog-compiler-macro and (goal body cont bindings)
  (compile-body (append (args goal) body) cont bindings))
```

`or` のマクロはより複雑です:

```lisp
(def-prolog-compiler-macro or (goal body cont bindings)
  (let ((disjuncts (args goal)))
    (case (length disjuncts)
      (0 fail)
      (1 (compile-body (cons (first disjuncts) body) cont bindings))
      (t (let ((fn (gensym "F")))
        '(flet ((,fn () ,(compile-body body cont bindings)))
          .,(maybe-add-undo-bindings
            (loop for g in disjuncts collect
              (compile-body (list g) '#',fn
                bindings)))))))))
```

**回答 12.11** `true/0` は `funcall` です。目標が成功すると継続を呼び出し、 `fail/0` は `ignore` です。目標が失敗すると継続を無視します。
これらのプリミティブに対してコンパイラ マクロを定義することもできます。

```lisp
(def-prolog-compiler-macro true (goal body cont bindings)
  (compile-body body cont bindings))

(def-prolog-compiler-macro fail (goal body cont bindings)
  (declare (ignore goal body cont bindings))
  nil)
```

**回答12.13**

```lisp
(defun deref-copy (exp)
  "Build a copy of the expression, which may have variables.
  The part without variables can be returned as is."
  (let ((var-alist nil ))
    (labels
      ((walk (exp)
        (deref exp)
        (cond ((consp exp)
          (reuse-cons (walk (first exp))
              (walk (rest exp))
              exp))
          ((var-p exp)
          (let ((entry (assoc exp var-alist)))
            (if (not (null entry))
            (cdr entry)
            (let ((var-copy (?)))
                (push (cons exp var-copy) var-alist)
                var-copy))))
          (t exp))))
    (walk exp))))
```

**回答 12.14** `test-cut` の最初の節では、`p` への 4 つの呼び出しはすべて `p` の最初の節を介して成功します。
次に、`(pc)` と `(pd)` の呼び出しでバックトラックが発生します。
`1` と `2` の 4 つの組み合わせはすべて成功します。
その後、バックトラックは通常、`(pb)` の呼び出しに戻ります。
しかし、カットによってこれが阻止され、2 番目の節が考慮されることなく、`(test-cut)` 目標全体が失敗します。
実際の出力は次のとおりです。

```lisp
(?- (test-cut))
(A 1)(B 1)(C 1) (D 1)
Yes;
(D 2)
Yes;
(C 2)(D 1)
Yes;
(D 2)
Yes;
No.
```

**回答12.17** たとえば:

```lisp
(defun >/2 (x y cont)
  (if (and (numberp (deref x)) (numberp (deref y)) (> x y))
    (funcall cont)))
(defun numberp/1 (x cont)
  (if (numberp (deref x))
    (funcall cont)))

```

**回答 12.19** Lisp では、`quote` を 2 つの方法で使用します。シンボルとそのシンボルによって表される変数の値を区別するため、およびリテラル リストと関数呼び出しを評価することによって返される値を区別するためです。
Prolog が語彙規則によって行う最初の区別: 変数は Prolog では疑問符で始まり、実際の Prolog では大文字で始まります。
Prolog は関数型ではなくリレーショナル型であるため、2 番目の区別は必要ありません。
式は、節のbodyのメンバーである場合は目標であり、目標への引数である場合はリテラルです。

**回答 12.20** ヒント: `member` に、4 種類のトレース イベントに関する情報を出力するプロシージャ `prolog-trace` の呼び出しを追加する方法は次のとおりです。

```lisp
(defun member/2 (?arg1 ?arg2 cont)
 (let ((old-trail (fill-pointer *tra1l*))
   (exit-cont #'(lambda ()
     (prolog-trace 'exit 'member ?arg1 ?arg2 )
     (funcall cont))))
  (prolog-trace 'call 'member ?arg1 ?arg2)
  (if (unify! ?arg2 (cons ?arg1 (?)))
   (funcall exit-cont))
  (undo-bindings! old-trail)
  (prolog-trace 'redo 'member ?arg1 ?arg2)
  (let ((?rest (?)))
   (if (unify! ?arg2 (cons (?) ?rest))
   (member/2 ?arg1 ?rest exit-cont)))
  (prolog-trace 'fail 'member ?arg1 ?arg2)))
```
`prolog-trace` の定義は次のとおりです。

```lisp
(defvar *prolog-trace-indent* 0)
(defun prolog-trace (kind predicate &rest args)
  (if (member kind '(call redo))
  (incf *prolog-trace-indent* 3))
  (format t "~&~VT~a ~  a:~{ ~  a  ~}"
      *prolog-trace-indent* kind predicate args)
  (if (member kind '(fail exit))
  (decf *prolog-trace-indent* 3)))
```
**答え 12.23**

```lisp
(defun anonymous-variables-in (tree)
  "Return a list of all variables that occur only once in tree."
  (values (anon-vars-in tree nil nil)))

(defun anon-vars-in (tree seen-once seen-more)
  "Walk the data structure TREE, returning a list of variables
  seen once, and a list of variables seen more than once."
  (cond
    ((consp tree)
    (multiple-value-bind (new-seen-once new-seen-more)
      (anon-vars-in (first tree) seen-once seen-more)
      (anon-vars-in (rest tree) new-seen-once new-seen-more)))
    ((not (variable-p tree)) (values seen-once seen-more))
    ((member tree seen-once)
    (values (delete tree seen-once) (cons tree seen-more)))
    ((member tree seen-more)
    (values seen-once seen-more))
    (t (values (cons tree seen-once) seen-more))))

```

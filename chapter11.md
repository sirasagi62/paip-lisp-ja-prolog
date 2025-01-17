# 第11章
## 論理プログラミング

> プログラミングに対する考え方に影響を与えない言語は、知る価値がありません。

> -アラン・パリス

Lisp は AI 研究における主要な言語ですが、決して唯一の選択肢というわけではありません。
もう一つの有力な候補は Prolog で、その名前は「論理に基づくプログラミング (Programming in logic)」に由来しています。<a id="tfn11-1"></a><sup>[1](#fn11-1)</sup>
論理プログラミングの背後にある考え方は、「プログラマーの役割は、問題とその解法の関係を記述することである。」というものです。
これらの関係は、問題を解決するアルゴリズムに対する制約として機能しますが、アルゴリズムの詳細については、プログラマーではなくシステム自体が責任を持ちます。
「プログラミング」と「ロジック」の間の緊張関係については、[第 14 章](chapter14.md) で取り上げますが、ここでは Prolog が論理プログラミングの理想的な目標に近似していると言って差し支えないでしょう。
Prolog は、従来のプログラミング言語と論理仕様言語の間の居心地の良い場所へと収まったのです。
その基盤となる概念を以下に3つ紹介します。

* Prolog では、ただ１つの *統一されたデータベース* の使用が推奨されています。
優れたコンパイラは、このデータベースへの効率的なアクセスを提供し、Lisp プログラマが詳細に処理しなければならないベクトル、ハッシュテーブル、プロパティリストおよびその他のデータ構造の必要性を軽減します。
Prolog はデータベースの考え方に基づいています。Lisp (およびほとんどの言語) が *関数型 (functional)* であるのに対して、Prolog は言うなれば *関係型 (relational)* です。Prolog では、たとえば「サンフランシスコの人口は750,000人である」という事実を関係として表現します。
一方、Lisp では、都市を入力として受け取り、人口を返す関数`population`を記述する傾向があります。
関係はより柔軟であり、サンフランシスコの人口を調べるだけでなく、たとえば「人口が500,000 人を超える都市を探す」といったことにも利用できます。

* Prolog は、「通常の」変数の代わりに *論理変数 (logic variable)* を提供します。
論理変数は、代入ではなく *単一化 (unification)* によって束縛されます。
一度束縛された論理変数は変更できません。
この点で、論理変数は数学の変数に似ています。
論理変数と単一化の存在により、論理プログラマーは、 (代入文で必要となるような) 評価の順序を記述することなしに、(数学でするのと同じように) 問題を制約する方程式を記述することができます。

* Prolog は *自動バックトラッキング* を提供します。
Lisp では、各関数呼び出しは単一の値を返します (特別な手配をしない限り、複数の値や値のリストを返すことはありません)。
一方、Prolog では各クエリがデータベース内の関係を検索し、クエリを満たすものを見つけます。複数の関係が存在する場合、それらは一度に一つずつ処理されます。
「人口が 500,000 人を超え、かつ州都である都市はどこか」のように、クエリに複数の関係が含まれる場合、Prolog はまず`人口`関係をたどり、人口が 500,000 人を超える都市を検索します。
そして見つかった都市ごとに`州都`関係を検索して、その都市が州都であるかどうかを確認します。もし、そうであるなら Prolog は都市を出力し、そうでない場合は*バックトラック*して、`人口`関係で別の都市を探します。
このように、Prolog を使用すると、プログラマーはデータの保存方法や検索方法について考慮する必要がなくなります。
ただし、問題によっては、単純な自動検索が非効率すぎるため、その場合はプログラマーが問題を再定義する必要があります。
しかし、解法のための制約を記述し、解法がどのように達成されるかを詳細に説明しない Prolog プログラムが理想であることに変わりはありません。

この章の目的は 2 つあります。一つは、Lisp ではなく Prolog で特定のプログラムを記述できる可能性を読者に知らせることです。もう一つは、上で述べた 3 つの重要な概念の実装を紹介して、それらを Lisp プログラム内で (独立してまたは組み合わせて) 使用できるようにすることです。
Prolog は、プログラミングのやり方を興味深く、異なる視点から見る方法を表しており、故に知っておく価値があるのです。
以降の章では、Prolog 流のアプローチの有用な応用例をいくつか見ていきます。

## 11.1 アイデア 1: 統一されたデータベース

Prolog の最初の重要なアイデアは、この本の読者にはおなじみのはずです。それは、アサーションの格納されたデータベースを操作することです。
Prolog では、アサーションは *節 (clause)* と呼ばれ、いくつかのオブジェクト間の関係を述べる *事実 (fact)* と、偶発的な事実を述べるために使用される *規則 (rule)* の 2 つのタイプに分けられます。
ここでは、サンフランシスコとカリフォルニア州の州都の人口に関する 2 つの事実を示します。
関係は`人口`と`首都`であり、これらの関係に参加するオブジェクトは`SF`,`750000`,`サクラメント`,および`CA`です。

```lisp
(population SF 750000)
(capital Sacramento CA)
```

Lisp に埋め込むことができる Prolog インタプリタが必要なため、Lisp 構文を使用しています。
実際の Prolog 表記は `population(sf,750000)` になります。
`likes`関係に関するいくつかの事実を以下に示します。

```lisp
(likes Kim Robin)
(likes Sandy Lee)
(likes Sandy Kim)
(likes Robin cats)
```

これらの事実は、KimはRobinが好き、SandyはLeeとKimの両方が好き、Robinは猫が好き、という意味に解釈できます。
これらが Lisp 関数呼び出しではなく Prolog の事実として解釈されることを Lisp に伝える何らかの方法が必要です。そこで事実をマークするためにマクロ `<-` を使用します。これを、データベースにファクトを追加する割り当て矢印として考えてみましょう。

```lisp
(<- (likes Kim Robin))
(<- (likes Sandy Lee))
(<- (likes Sandy Kim))
(<- (likes Robin cats))
```

Prolog と Lisp の主な違いの 1 つは、関係と関数の違いにあります。
Lisp では、関数 `likes` を定義して、(`likes 'Sandy`) がリスト (`Lee Kim`) を返すようにします。
逆の方法で情報にアクセスしたい場合は、別の関数、たとえば `likers-of` を定義して、(`likers-of 'Lee`) が (`Sandy`) を返すようにします。
Prolog では、複数の関数ではなく、単一の `likes` 関係が存在します。
この単一の関係は、異なるクエリを実行することで、複数の関数であるかのように使用できます。
たとえば、クエリ (`likes Sandy ?who`) は `?who` が `Lee or Kim` に束縛されている場合に成功し、クエリ (`likes ?who Lee`) は `?who` が `Sandy` に束縛されている場合に成功します。

Prolog データベースの 2 番目のタイプの節は *規則* です。規則は条件付きの事実を述べます。たとえば、Sandyは猫が好きな人を好きになるという規則は次のように表すことができます。

```lisp
(<- (likes Sandy ?x) (likes ?x cats))
```

これには2つの解釈があります。
論理的主張として見ると、これは「任意の x について、x が猫を好む場合、Sandy は x を好む」と解釈されます。これは*宣言的*解釈です。
Prolog プログラムの一部として見ると、「Sandy が何か x を好きだということを示す場合、その方法の 1 つは、x が猫を好きだということを示すことである」と解釈されます。これは *手続き型* の解釈です。
これは、目標 (Sandy は x が好きだ) から前提 (x は猫が好きだ) へと逆方向に推論するため、*後方連鎖*解釈と呼ばれます。
記号 `<-` は、両方の解釈に適しています。これは論理的含意を示す矢印であり、後方連鎖を示すために後方を指します。

宣言形式に複数の手続き的解釈を与えることが可能です。
([第 1 章](chapter1.md) でこれを行いました。ここでは、文法規則を使用して単語の文字列と解析ツLeeの両方を生成しました。)
上記の規則は、手続き的には「`x` が猫を好きだとわかったら、Sandy は `x` を好きだと結論付ける」と解釈できます。
これは、前提から結論までの推論である*前方連鎖*です。Prolog は後方連鎖のみを実行することが明白です。多くのエキスパート システムでは、前方連鎖のみが使用されますが、一部のシステムでは 2 つが混在して使用されます。

節内の最も左にある表現は「head」と呼ばれ、残りの表現は「body」と呼ばれます。この見方では、事実は単なる本体のない規則です。つまり、事実はどのような場合でも真実です。
一般的に、節の形式は次のようになります。

`(<- head body...)`

節は、bodyのすべての目標が真である場合にのheadが真であると主張します。
たとえば、次の節は、KimはLeeとKimの両方を好きな人を好きだと述べています。

```lisp
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
```

これは次のように読むことができます。

*任意の* x について、`Kimは x が好きだ*`と推論します。

もし`x は Lee が好き`であり、`x は Kim が好き`であると*証明できれば。*

## 11.2 アイデア2: 論理変数の単一化

単一化(unification)は、パターン マッチングの考え方をそのまま拡張したものです。
これまで見てきたパターン マッチング関数は、常にパターン (変数を含む式) を定数式 (変数を含まない式) と照合していました。
単一化では、それぞれに変数を含めることができる 2 つのパターンが相互に照合されます。
パターン マッチングと単一化の違いの例を次に示します。

```lisp
> (pat-match '(?x + ?y) '(2 + 1)) => ((?Y . 1) (?X . 2))
> (unify '(?x + 1) '(2 + ?y)) => ((?Y . 1) (?X . 2))
```

単一化フレームワーク内では、変数 (上記の `?x` や `?y` など) は *論理変数* と呼ばれます。通常の変数と同様に、論理変数には値を割り当てることも、束縛を解除することもできます。
違いは、論理変数は変更できないことです。
一度値が割り当てられると、その値が保持されます。異なる値と単一化しようとすると失敗します。`(?x + ?x)` と `(2 + 2)`のパターン マッチが可能であったのと同様に、同じ値を持つ変数を複数回単一化することも可能です。

単純なパターン マッチングと単一化の違いは、単一化では 2 つの変数を互いに一致させることができる点です。2 つの変数は束縛されていないままですが、同等になります。どちらかの変数がその後値に束縛されると、両方の変数がその値を採用します。
次の例では、`?x` を `?y` に束縛して、変数 `?x` と `?y` を等しくします。

```lisp
> (unify '(f ?x) '(f ?y)) => ((?X . ?Y))
```

単一化は、高度な推論を行うために使用できます。
たとえば、`a + a = 0`と `x + y = 0`という 2 つの方程式があり、これら 2 つの方程式が単一化されることがわかっている場合は、`a`,`x`,`y` はすべて 0 であると結論付けることができます。
私たちが定義する `unify` のバージョンは、`?y` を `0` に、`?x` を `?y` に、`?a` を `?x` に束縛 (bind) することでこの結果を示します。
また、2 つの構造体(structure)を単一化した結果の構造体を示す関数 `unifier` も定義します。

```lisp
> (unify '(?a + ?a = 0) '(?x + ?y = ?y)) =>
((?Y . 0) (?X . ?Y) (?A . ?X))

> (unifier '(?a + ?a = 0) '(?x + ?y = ?y)) => (0 + 0 = 0)
```

単一化の力に夢中にならないようにするには、単一化によって何が得られるのかを正確に把握しておくことが賢明です。
単一化は変数が他の変数または式と等しいことを示す方法を提供します。方程式を自動的に解いたり、等式以外の制約を適用したりする方法は提供されません。
次の例は、単一化が記号+を加算演算子としてではなく、未解釈アトム(uninterpreted atom)として扱うことを明白にしています。

```lisp
> (unifier '(?a + ?a = 2) '(?x + ?y = ?y)) => (2 + 2 = 2)
```

`unify` のコードを作成する前に、パターン マッチング ユーティリティ ([第 6 章](chapter6.md)) のコードを再掲します。

```lisp
(defconstant fail nil "パターンマッチングの失敗を示す")
(defconstant no-bindings '((t . t))
 "変数がない場合のパターンマッチングの成功を示す")
(defun variable-p (x)
 "xが変数かどうか (そのシンボルが'?'から始まっているかどうか)?"
 (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))
(defun get-binding (var bindings)
 "束縛リストから変数と値のペア'(variable . value)'を探す"
 (assoc var bindings))
(defun binding-val (binding)
 "ある束縛から値部分を取り出す"
 (cdr binding))
(defun lookup (var bindings)
 "束縛リストから値部分(for var)を取り出す"
 (binding-val (get-binding var bindings)))
(defun extend-bindings (var val bindings)
 "束縛リストに1つの(var . value)を追加する"
 (cons (cons var val)
       ;; 1回でも「本物」の束縛を追加した場合は
       ;; ダミーであるno-bindingsを取り除く
       (if (and (eq bindings no-bindings))
           nil
           bindings)))
(defun match-variable (var input bindings)
 "VARが入力とマッチしているかどうか?使ってみて（もしくは更新してみて）束縛を返す"
 (let ((binding (get-binding var bindings)))
 (cond ((not binding) (extend-bindings var input bindings))
       ((equal input (binding-val binding)) bindings)
       (t fail))))
```

`unify` 関数は次のようになります。これは、`***` でマークされた行が追加されていることを除いて、`pat-match` (180 ページで定義) と同一です。
関数 `unify-variable` も `match-variable` によく似ています:

```lisp
(defun unify (x y &optional (bindings no-bindings))
 "See if x and y match with given bindings."
 (cond ((eq bindings fail) fail)
       ((variable-p x) (unify-variable x y bindings))
       ((variable-p y) (unify-variable y x bindings)) ;***
       ((eql x y) bindings)
       ((and (consp x) (consp y))
        (unify (rest x) (rest y)
               (unify (first x) (first y) bindings)))
       (t fail)))
(defun unify-variable (var x bindings)
 "Unify var with x, using (and maybe extending) bindings."
 ;; Warning - buggy version
 (if (get-binding var bindings)
  (unify (lookup var bindings) x bindings)
  (extend-bindings var x bindings)))
```

残念ながら、この定義は正確ではありません。
簡単な例を扱います:

```lisp
> (unify '(?x + 1) '(2 + ?y)) => ((?Y . 1) (?X . 2))
> (unify '?x '?y) => ((?X . ?Y))
> (unify '(?x ?x) '(?y ?y)) => ((?Y . ?Y) (?X . ?Y))
```

しかし、対処できない病的なケースがいくつかあります。

```lisp
> (unify '(?x ?x ?x) '(?y ?y ?y))
>>Trap #043622 (PDL-OVERFLOW REGULAR)
The regular push-down list has overflowed.
While in the function GET-BINDING <= UNIFY-VARIABLE <= UNIFY
```

ここでの問題は、`?y` が1界でもそれ自体に束縛されると、`unify-variable` 内の `unify` の呼び出しが無限ループを引き起こすことです。しかし、`?y` とそれ自身とのマッチングは常に成功する必要があるため、`unify` 内の等価性テストを変数テストの前に移動します。
これは、等しい変数が `eql` であると仮定しており、シンボルとして実装された変数に対して有効な仮定です (ただし、他の方法で変数を実装することにした場合は注意してください)。

```lisp
(defun unify (x y &optional (bindings no-bindings))
 "See if x and y match with given bindings."
 (cond ((eq bindings fail) fail)
  ((eql x y) bindings) ;*** moved this line
  ((variable-p x) (unify-variable x y bindings))
  ((variable-p y) (unify-variable y x bindings))
  ((and (consp x) (consp y))
  (unify (rest x) (rest y)
      (unify (first x) (first y) bindings)))
   (t fail)))
```

以下にいくつかのテストケースを示します。

```lisp
> (unify '(?x ?x) '(?y ?y)) => ((?X . ?Y))
> (unify '(?x ?x ?x) '(?y ?y ?y)) => ((?X . ?Y))
> (unify '(?x ?y) '(?y ?x)) => ((?Y . ?X) (?X . ?Y))
> (unify '(?x ?y a) '(?y ?x ?x))
>>Trap #043622 (PDL-OVERFLOW REGULAR)
The regular push-down list has overflowed.
While in the function GET-BINDING <= UNIFY-VARIABLE <= UNIFY
```

私たちは問題を先送りしましたが、解決はしていません。
同じ束縛リストで `(?Y . ?X)` と `(?X . ?Y)` の両方を許可することは、`(?Y . ?Y)` を許可するのと同じくらい悪いことです。
この問題を回避するには、束縛された変数ではなく、束縛 リストで指定された値を処理するポリシーにする必要があります。
関数 `unify-variable` はこのポリシーを実装できません。
var が束縛された変数である場合に var の束縛を取得するチェックがありますが、`x` が束縛された変数である場合に `x` の値を取得するチェックも必要です。

```lisp
(defun unify-variable (var x bindings)
 "Unify var with x, using (and maybe extending) bindings."
 (cond ((get-binding var bindings)
   (unify (lookup var bindings) x bindings))
  ((and (variable-p x) (get-binding x bindings)) ;***
   (unify var (lookup x bindings) bindings)) ;***
  (t (extend-bindings var x bindings))))
```


さらにいくつかのテストケースを示します。

```lisp
> (unify '(?x ?y) '(?y ?x)) => ((?X . ?Y))
> (unify '(?x ?y a) '(?y ?x ?x)) => ((?Y . A) (?X . ?Y))
```

問題は解決したようです。
では、新しい問題に挑戦してみましょう。

```lisp
> (unify '(?x ?y) '(?y ?x)) => ((?X . ?Y))
> (unify '(?x ?y a) '(?y ?x ?x)) => ((?Y . A) (?X . ?Y))
```

ここで `((?XF ?X))` は実際には `((?X . ((F ?X))))` を意味するため、 `?X` は (`F ?X`) に束縛されます。
これは循環的で無限の単一化を表しています。
Prolog のいくつかのバージョン、特に Prolog II ([Giannesini et al.
[1986](bibliography.md#bb0460)) はそのような構造の解釈を提供しますが、無限構造の意味を定義するのは難しいです。

このような無限構造に対処する最も簡単な方法は、それらを禁止することです。
この禁止は、変数をその変数を含む構造体と単一化しようとするたびに失敗するようにunifierを変更することによって実現できます。
これは、単一化の分野では *出現検査(occurs check)* として知られています。実際には、この問題が発生することはめったになく、計算の複雑さが大幅に増加する可能性があるため、ほとんどの Prolog システムでは、出現検査が無視されています。
つまり、これらのシステムは不健全な回答を生み出す可能性があるということです。
次の `unify` の最終バージョンでは、ユーザーが出現検査をオンまたはオフにできるようにする変数が提供されます。

```lisp
(defparameter *occurs-check* t "Should we do the occurs check?")

(defun unify (x y &optional (bindings no-bindings))
 "See if x and y match with given bindings."
 (cond ((eq bindings fail) fail)
       ((eql x y) bindings)
       ((variable-p x) (unify-variable x y bindings))
       ((variable-p y) (unify-variable y x bindings))
       ((and (consp x) (consp y))
        (unify (rest x) (rest y)
               (unify (first x) (first y) bindings)))
       (t fail)))

(defun unify-variable (var x bindings)
 "Unify var with x, using (and maybe extending) bindings."
 (cond ((get-binding var bindings)
     (unify (lookup var bindings) x bindings))
     ((and (variable-p x) (get-binding x bindings))
     (unify var (lookup x bindings) bindings))
     ((and *occurs-check* (occurs-check var x bindings)) fail)
     (t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
 "Does var occur anywhere inside x?"
 (cond ((eq var x) t)
     ((and (variable-p x) (get-binding x bindings))
     (occurs-check var (lookup x bindings) bindings))
     ((consp x) (or (occurs-check var (first x) bindings)
         (occurs-check var (rest x) bindings)))
     (t nil)))
```

ここで、`unify` がどのように使用されるかを考えてみましょう。
特に、必要なのは、束縛 リストを式に置き換える関数です。
当初、関数 `sublis` が利用可能であったため、束縛の実装として連想リストを選択しました。
皮肉なことに、変数は他の変数に束縛され、その変数は式に束縛されるため、`sublis` は機能しなくなります。
`function subst-bindings` は、再帰束縛を置き換える点を除いて、`sublis` のように動作します。

```lisp
(defun subst-bindings (bindings x)
 "Substitute the value of variables in bindings into x,
 taking recursively bound variables into account."
 (cond ((eq bindings fail) fail)
     ((eq bindings no-bindings) x)
     ((and (variable-p x) (get-binding x bindings))
     (subst-bindings bindings (lookup x bindings)))
     ((atom x) x)
     (t (reuse-cons (subst-bindings bindings (car x))
            (subst-bindings bindings (cdr x))
            x))))
```

それでは、いくつかの例で `unify` を試してみましょう。

```
> (unify '(?x ?y a) '(?y ?x ?x)) => ((?Y . A) (?X . ?Y))
> (unify '?x '(f ?x)) => NIL
> (unify '(?x ?y) '((f ?y) (f ?x))) => NIL
> (unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y))) => NIL
> (unify 'a 'a) => ((T . T))
```

最後に、関数 `unifier` は `unify` を呼び出し、結果の束縛 リストを引数の 1 つに代入します。
`x` の選択は任意です。束縛 リストを `y` に代入すると、同じ結果が得られます。

```lisp
(defun unifier (x y)
 "Return something that unifies with both x and y (or fail)."
 (subst-bindings (unify x y) x))
```

`unifier` の例をいくつか示します。

```
> (unifier '(?x ?y a) '(?y ?x ?x)) => (A A A)
> (unifier '((?a * ?x ^ 2) + (?b * ?x) + ?c)
        '(?z + (4 * 5) + 3)) =>
((?A * 5 ^ 2) + (4 * 5) + 3)
```

`*occurs-check*` が false の場合、次の回答が得られます。

```lisp
> (unify '?x '(f ?x)) => ((?X F ?X))
> (unify '(?x ?y) '((f ?y) (f ?x))) => ((?Y F ?X) (?X F ?Y))
> (unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y))) => ((?Z ?X ?Y) (?Y ?X ?Z) (?X ?Y ?Z))
```

### Prolog によるプログラミング

Prolog 節の素晴らしい点は、通常「データ」ではなく「プログラム」として考えられる関係を表現できることです。たとえば、項目とその項目を含むリストの間に保持される`member`関係を定義できます。
より正確には、その項目がそのリストの最初の要素であるか、リストの残りの部分のメンバーである場合、その項目はリストのメンバーになります。
この定義は、ほぼそのまま Prolog に翻訳できます。

```lisp
(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))
```

もちろん、Lisp でも同様の定義を記述できます。
最も目に見える違いは、Prolog ではパターンを節の先頭に置くことができるため、`consp` のような認識子や `first` や `rest` のようなアクセス子が必要ないことです。
それ以外の点では、Lispの定義は同様です:<a id="tfn11-2"></a><sup>[2](#fn11-2)</sup>

```lisp
(defun lisp-member (item list)
  (and (consp list)
  (or (eql item (first list))
    (lisp-member item (rest list)))))
```

パターン機能を利用せずに Prolog コードを記述すると、Lisp バージョンに似たものになります。

```lisp
(<- (member ?item ?list)
  (= ?list (?item . ?rest)))
(<- (member ?item ?list)
  (= ?list (?x . ?rest))
  (member ?item ?rest))
```

orをProlog上で定義すると、明らかに Lisp バージョンの構文上の派生形にあたるバージョンを記述することになります。

```lisp
(<- (member ?item ?list)
  (= ?list (?first . ?rest))
  (or (= ?item ?first)
  (member ?item ?rest)))
```

Prolog バージョンの `member` がどのように動作するかを見てみましょう。
マクロ `?-` を使用してクエリを指定できる Prolog インタプリタがあり、`member` の定義が入力されていると想像してください。
すると次のようになります:

```lisp
> (?- (member 2 (1 2 3)))
Yes;
> (?- (member 2 (1 2 3 2 1)))
Yes;
Yes;
```

2 はリストの残りのメンバーであるため、最初のクエリに対する答えは「はい」です。
2 番目のクエリでは、リストに 2 が 2 回出現するため、答えは 2 回「はい」になります。
これは Lisp プログラマーにとっては少々意外なことですが、Prolog と Lisp の `member` の間にはかなり密接な対応関係があるようです。ただし、Prolog の `member` ではできても Lisp ではできないことがあります。

```lisp
> (?- (member ?x (1 2 3)))
?X = 1;
?X = 2;
?X = 3;
```

ここで `member` は述語としてではなく、リスト内の要素のジェネレータとして使用されます。
Lisp 関数は常に特定の入力 (または複数の入力) から特定の出力にマッピングしますが、Prolog の関係はさまざまな方法で使用できます。
`member` の場合、最初の引数 `?x` は、指定された目標に応じて、入力または出力のいずれかになります。
単一の仕様を複数の異なる方向を指し示す関数として使用できるこの機能は、Prolog の非常に柔軟な機能です。
(残念ながら、`member` のような単純な関係では非常にうまく機能しますが、実際には大規模なプログラムではうまく機能しません。
たとえば、コンパイラを設計して、それを自動的に逆アセンブラとしても動作させることは非常に困難です。)

ここで、[図 11.1](#f0010) にまとめられているように、Prolog インタープリタの実装について説明します。
最初の実装の選択肢は、規則と事実の表現です。
規則と事実を区別せずに、唯一の単一化された節データベース(a single uniform data base of caluses)を構築します。
節の最も単純な表現は、headとbodyを保持するcons cellです。事実上、bodyは空になります。

| 機能 | 説明 |
|-------------------------------------|------------------------------------------------|
| | **トップレベルマクロ** |
| `<-` | データベースに節を追加します。 |
| `?-` | クエリを証明し、回答を出力します。 |
| | **特殊変数** |
| `*db-predicates*` | すべての述語のリスト。 |
| `*occurs-check*` | 循環した単一化をチェックする必要があるか否か（出現検査を行うかどうか）? |
| | **データ型** |
| `clause` | headとbodyで構成されます。 |
| `variable` | `?` で始まるシンボル。 |
| | **主な機能** |
| `add-clause` | データベースに節を追加します。 |
| `prove` | 目標に対する可能解(possible solutions)のリストを返します。 |
| `prove-all` | 目標の結合に対する解のリストを返します。 |
| `top-level-prove` | 目標を証明し、変数を読みやすい形式で出力します。 |
| | **補助機能** |
| `get-clauses` | 述語のすべての節を検索します。 |
| `predicate` | 関係から述語を選択します。 |
| `clear-db` | データベースからすべての節（すべての述語）を削除します。 |
| `clear-predicate` | 単一の述語の節を削除します。 |
| `rename-variables` | `x` 内のすべての変数を新しい変数に置き換えます。 |
| `unique-find-anywhere-if` | 述語を満たすすべての一意の葉(リーフ)を検索します。 |
| `show-prolog-solutions` | 各解の変数を出力します。 |
| `show-prolog-vars` | 各変数とその束縛を出力します。 |
| `variables-in` | 式内のすべての変数のリストを返します。 |
| | **以前に定義された定数** |
| `fail` | 単一化が失敗したことを示します。 |
| `no-bindings` | 変数なしでの単一化の成功 |
| | **以前に定義された関数** |
| `unify` | 2 つの式を単位うする束縛を返します (セクション 11.2)。 |
| `unify-variable` | 式に対して変数を単一化します。 |
| `occurs-check` | 式の中に特定の変数が出現するかどうかを確認します。 |
| `subst-bindings` | 束縛を式に置き換えます。 |
| `get-binding` | 変数の `(var . val)` 束縛を取得します。 |
| `lookup` | 変数の値を取得します。 |
| `extend-bindings` | 束縛 リストに新しい変数/値のペアを追加します。 |
| `variable-p` | 引数が変数かどうか |
| `reuse-cons` | `cons` と似ていますが、可能な場合は古い値を再利用します。 |

図11.1: Prologインタプリタの用語集

```lisp
;; Clauses are represented as (head . body) cons cells
(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))
```

次の質問は、節をどのように索引付け(index)するかです。
節の手続き的解釈を思い出してください。headを証明したいときは、bodyを証明することによって証明できます。
これは、節をそのheadに基づいて索引付けする必要があることを示唆しています。
各節は、節のheadの述語のプロパティ リストに格納されます。
データベースはさまざまなシンボルのプロパティ リストに分散されているため、データベース全体を `*db-predicates*` の値として格納されるシンボルのリストとして表します。

```lisp
;; Clauses are stored on the predicate's plist
(defun get-clauses (pred) (get pred 'clauses))
(defun predicate (relation) (first relation))

(defvar *db-predicates* nil
  "A list of all predicates stored in the database.")
```

ここで、新しい節を追加する方法が必要です。その作業は、ユーザーインターフェイスを提供するマクロ `<-` と、それを実行する関数 `add-clause` に分割されます。
実際には Prolog-In-Lisp という新しい言語を定義しているので、節を追加するためのマクロを定義することは価値があります。
この言語には、節を追加するための `<-` マクロと、クエリを作成するための `?-` マクロの 2 つの構文構造しかありません。

```lisp
(defmacro <- (&rest clause)
 "Add a clause to the data base."
 '(add-clause '.clause))

(defun add-clause (clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
          (nconc (get-clauses pred) (list clause)))
    pred))

```

あと必要なのは、節を削除する方法だけです。そうすれば、データベースは完成します。

```lisp
(defun clear-db ()
  "Remove all clauses (for all predicates) from the data base."
  (mapc #'clear-predicate *db-predicates*))

(defun clear-predicate (predicate)
  "Remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))
```

データを出し入れする方法がなければ、データベースは役に立ちません。関数 prove は、指定された目標がデータベース内に直接存在する事実と一致するか、規則から導き出せるかを証明するために使用されます。目標を証明するには、まずその目標の候補となる節をすべて見つけます。各候補について、目標が節のheadと単一化されているかどうかを確認します。真であるならば、節のbodyにあるすべての目的を証明するようにします。
実際には、bodyに目標はないので、成功はすぐに起こります。
規則の場合、bodyの目標は、前のステップからの束縛が維持されていることを確認しながら、一度に 1 つずつ証明する必要があります。
実装は簡単です:

```lisp
(defun prove (goal bindings)
  "Return a list of possible solutions to goal."
  (mapcan #'(lambda (clause)
              (let ((new-clause (rename-variables clause)))
                (prove-all (clause-body new-clause)
                           (unify goal (clause-head new-clause) bindings))))
          (get-clauses (predicate goal))))

(defun prove-all (goals bindings)
  "Return a list of solutions to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) (list bindings))
        (t (mapcan #'(lambda (goal1-solution)
                       (prove-all (rest goals) goal1-solution))
                   (prove (first goals) bindings)))))
```

難しいのは、ある節の変数 `?x` と別の節の別の変数 `?x` を区別する方法が必要なことです。
そうしないと、証明の過程で 2 つの異なる節で使用される変数は、各節で同じ値を取る必要があり、これは間違いを引き落とします。
関数の引数が関数への異なる再帰呼び出しで異なる値を持つことができるのと同様に、節内の変数は異なる再帰使用で異なる値を取ることができます。
変数を区別する最も簡単な方法は、各節が使用される前にすべての変数の名前を変更することです。
関数 `rename-variables` はこれを実行します:<a id="tfn11-3"></a><sup>[3](#fn11-3)</sup>

```lisp
(defun rename-variables (x)
  "Replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
                  (variables-in x))
          x))
```

`Rename-variables` は、呼び出されるたびに新たなシンボルを生成する関数である `gensym` を利用します。
シンボルはどのパッケージにもインターン化されていないため、プログラマーが同じ名前のシンボルを入力する危険性はありません。
述語 `variables-in` とその補助関数は次のように定義されます。

```lisp
(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'variable-p exp))

(defun unique-find-anywhere-if (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
        predicate
        (first tree)
        (unique-find-anywhere-if predicate (rest tree)
                                 found-so-far))))
```

最後に、証明機能への優れたインターフェースが必要です。
クエリを導入するためのマクロとして `?-` を使用します。
クエリでは目標の結合も許可される可能性があるため、`?-` は `prove-all` を呼び出します。
`<-` と `?-` を組み合わせることで、Prolog-In-Lisp 言語の完全な構文が定義されます。

```lisp
(defmacro ?- (&rest goals) '(prove-all ',goals no-bindings))
```

これで、前の例で示したすべての節を入力できます。

```
(<- (likes Kim Robin))
(<- (likes Sandy Lee))
(<- (likes Sandy Kim))
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(<- (likes ?x ?x))
```

Sandyが誰を好きかを尋ねるには、次のように使います。

```lisp
> (?- (likes Sandy ?who))
(((?WHO . LEE))
  ((?WHO . KIM))
  ((?X2856 . ROBIN) (?WHO .?X2856))
  ((?X2860 . CATS) (?X2857 CATS) (?X2856 . SANDY) (?WHO ?X2856)
  ((?X2865 . CATS) (?X2856 ?X2865)((?WHO . ?X2856))
  (?WHO . SANDY) (?X2867 . SANDY)))
```

驚くかもしれませんが、答えは6つあります。
事実に基づいて、最初の 2 つの答えは Lee と Kim です。
次の 3 つは、「Sandyは猫が好きな人なら誰でも好きだ」という節から生じます。
まず、Robinは猫が好きであるという事実から、Robinが答えです。
Robin が答えであることを確認するには、束縛を解く必要があります。`?who` は `?x2856` に束縛されており、`?x2856` は Robin に束縛されています。

ここで驚きの事実が明らかになります。Sandyがリストに載っているのは、次の理由からです。

1. Sandyは猫が好きな人/物が好き
2. 猫は「誰もが自分自身を好き」だから猫が好き
3. したがってSandyは猫が好き
4. したがってSandyはSandyが好き。

ステップ2により「Cats」が答えとなり、最後に「自分自身を好きになる」という節により「Sandy」が再び答えとなります。
クエリの結果は解法のリストであり、各解法はクエリが真であることを証明するさまざまな方法に対応していることに注意してください。
Sandyが 2 回登場するのは、SandyがSandyを好きだということを示す方法が 2 つあるためです。
解法が表示される順序は、検索の順序によって決まります。
Prolog は、上から下、左から右の順に解法を検索します。
節は上から下へ検索されるため、最初に入力された節が最初に試行されます。
節内では、本文は左から右に検索されます。
(`likes Kim ?x`) 節を使用すると、Prolog はまず Lee が好きな `x` を見つけようとし、次に `x` が Kim を好きかどうかを確認します。

`prove-all` からの出力はあまりきれいではありません。これを修正するには、新しい関数 `top-level-prove` を定義します。この関数は、以前と同じように `prove-all` を呼び出しますが、解法のリストを `show-prolog-solutions` に渡し、より読みやすい形式で出力します。`show-prolog-solutions` は値 `(values).` を返さないことに注意してください。つまり、`(values)` がトップレベルの呼び出しの結果である場合、read-eval-print ループは何も出力しません。

```lisp
(defmacro ?- (&rest goals) `(top-level-prove ',goals))

(defun top-level-prove (goals)
  "Prove the goals, and print variables readably."
  (show-prolog-solutions
    (variables-in goals)
    (prove-all goals no-bindings)))

(defun show-prolog-solutions (vars solutions)
  "Print the variables in each of the solutions."
  (if (null solutions)
      (format t "~&No.")
      (mapc #'(lambda (solution) (show-prolog-vars vars solution))
            solutions))
  (values))

(defun show-prolog-vars (vars bindings)
  "Print each variable with its binding."
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
        (format t "~&~a = ~a" var
                (subst-bindings bindings var))))
  (princ ";"))
```

それでは、いくつかのクエリを試してみましょう。

```lisp
> (?- (likes Sandy ?who))
?WHO = LEE;
?WHO = KIM;
?WHO = ROBIN;
?WHO = SANDY;
?WHO = CATS;
?WHO = SANDY;
> (?- (likes ?who Sandy))
?WHO = SANDY;
?WHO = KIM;
?WHO = SANDY;
> (?- (likes Robin Lee))
No.
```

最初のクエリは再び Sandy が誰を好きなのかを尋ね、2 番目のクエリは Sandy が誰を好きなのかを尋ねます。
3番目は事実の確認を求めます。
答えは「いいえ」です。RobinがLeeを好きだということを示す節や事実は存在しないからです。
もう 1 つの例として、相互に好意関係にある 2 人のペアのリストを示します。
最後の答えにはインスタンス化されていない変数があり、誰もが自分自身を好きだということを示しています。

```lisp
> (?- (likes ?x ?y) (likes ?y ?x))
?Y = KIM
?X = SANDY;
?Y = SANDY
?X = SANDY;
?Y = SANDY
?X = SANDY;
?Y = SANDY
?X = KIM;
?Y = SANDY
?X = SANDY;
?Y = ?X3251
?X = ?X3251;
```

Prolog では、「2 はどのリストのメンバーですか?」や「どの項目がどのリストの要素ですか?」などのオープンエンドのクエリを実行するのが理にかなっています。

```lisp
(?- (member 2 ?list))
(?- (member ?item ?list))
```

これらのクエリは有効な Prolog であり、解法を返しますが、その数は無限になります。
私たちのインタプリタは、すべての解法を 1 つのリストに収集してからそれらを表示するため、解法を見ることはできません。
次のセクションでは、この問題を修正する新しいインタプリタを作成する方法を示します。

**演習 11.1 [m]** 関係の表現は、最初の要素がシンボルであるリストでした。
ただし、引数のない関係の場合、`(<- (p) (q) (r))` ではなく `(<- pqr)` と書くことを好む人もいます。
どちらの形式も受け入れられるように変更を加えます。

**演習 11.2 [m]** `<-` 表記は読みにくいと感じる人もいます。
マクロ `rule` と `fact` を定義して、次のように記述します。

```lisp
(fact (likes Robin cats))
(rule (likes Sandy ?x) if (likes ?x cats))
```

## 11.3 アイデア3: 自動バックトラッキング

前のセクションで実装された Prolog インタープリタは、すべての可能な解法のリストを返すことによって問題を解決します。
回答は 1 つの中断のないバッチ処理で取得されるため、これを *バッチ* アプローチと呼びます。
時にはそれがまさにあなたが望むことですが、時には単一の解法で十分なこともあります。
実際の Prolog では、解法は見つかった順に 1 つずつ提示されます。
各解法が印刷された後、ユーザーはさらに解法を要求するか、停止するかを選択できます。
これは*漸進的な(incremental)*アプローチです。
望ましい解法が多くの選択肢の中で最初の解法の 1 つである場合、漸進的アプローチの方が速くなります。
漸進的アプローチは、解法が無限にある場合でも機能します。
それだけでは不十分な場合は、深さ優先で検索するように漸進的アプローチを実装できます。
つまり、すべての解法を一度にメモリに保持する必要があるバッチ アプローチよりも、必要なストレージ スペースが少なくなります。

このセクションでは、漸進的 Prolog インタプリタを実装します。
1 つのアプローチとしては、最後のセクションのインタプリタを変更して、リストではなくパイプを使用するようにすることです。
パイプを使用すると、不必要な計算が遅延され、無限リストであっても有限の時間と空間で表現できます。
`prove` と `prove-all` の `mapcan` を `mappend-pipe` に変更するだけで、パイプに変更することができます (286 ページ)。
[Winston and Horn (1988)](bibliography.md#bb1410) と [Abelson and Sussman (1985)](bibliography.md#bb0010) の書籍では、このアプローチを採用しています。
今回は別のアプローチを採用します。

最初のステップは、すべての可能な解法のリストではなく、単一の解法を返す `prove` と `prove-all` のバージョンです。これは、`gps` の `achieve` と `achieve-all` ([第 4 章](chapter4.md)) を彷彿とさせます。`gps` とは異なり、再帰的なサブゴールと破壊された兄弟ゴールはチェックされません。ただし、`prove` はすべての解法を体系的に検索する必要があるため、最初の目標を達成した後に達成する他の目標のリストという追加のパラメータが渡されます。これは、継続を `prove` に渡すことと同じです。その結果、`prove` が成功した場合、トップレベルの目標全体が成功したことを意味します。失敗した場合は、プログラムがバックトラッキングして別の選択シーケンスを試行していることを意味します。`prove` は some の使用方法により、`fail` が `nil` であるという事実に依存していることに注意してください。

```lisp
(defun prove-all (goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) bindings)
        (t (prove (first goals) bindings (rest goals)))))
(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (some #'(lambda (clause)
             (let ((new-clause (rename-variables clause)))
               (prove-all
                 (append (clause-body new-clause) other-goals)
             (unify goal (clause-head new-clause) bindings))))
  (get-clauses (predicate goal))))
```

`prove` が成功した場合、解法が見つかったことを意味します。
より多くの解法が必要な場合は、プロセスを失敗させて、後戻りして再試行させる方法が必要です。
これを行う 1 つの方法は、変数を出力し、計算を続行するかどうかをユーザーに尋ねるという目標ですべてのクエリを拡張することです。
ユーザーが「はい」と答えると、目標は「失敗」となり、バックトラッキングが開始されます。
ユーザーが「いいえ」と答えた場合、目標は成功し、それが最終目標であるため、計算は終了します。
これには、データベースと照合するのではなく、何らかの手順を実行してアクションを起こす、まったく新しいタイプの目標が必要です。
Prolog では、このような手順は言語に組み込まれており、ユーザーが新しい手順を定義することはできないため、「プリミティブ」と呼ばれます。
もちろん、ユーザーはプリミティブを呼び出す非プリミティブ プロシージャを定義することもできます。

私たちの実装では、プリミティブは Lisp 関数として表現されます。
述語は、節のリスト (これまでのように) として表すことも、単一のプリミティブとして表すこともできます。
以下は、適切な場合にプリミティブを呼び出す `prove` のバージョンです。

```lisp
(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (let ((clauses (get-clauses (predicate goal))))
      (if (listp clauses)
              (some
                  #'(lambda (clause)
                          (let ((new-clause (rename-variables clause)))
                              (prove-all
                                (append (clause-body new-clause) other-goals)
                                (unify goal (clause-head new-clause) bindings))))
                  clauses)
              ;; The predicate's "clauses" can be an atom:
              ;; a primitive function to call
              (funcall clauses (rest goal) bindings
                                other-goals))))
```

以下は、プリミティブ目標 `show-prolog-vars` を目標リストの最後に追加した `top-level-prove` のバージョンです。
このバージョンでは、表示に関する処理は `show-prolog-vars` のプリミティブによって処理されるため、 `show-prolog-solutions` 自体を呼び出す必要がないことに注意してください。

```
(defun top-level-prove (goals)
  (prove-all '(,@goals (show-prolog-vars ,@(variables-in goals)))
                        no-bindings)
  (format t "~&No.")
  (values))
```

ここで、プリミティブ `show-prolog-vars` を定義します。
すべてのプリミティブは、プリミティブ関係への引数のリスト (ここでは表示する変数のリスト)、これらの引数の束縛 リスト、および保留中の目標のリストという 3 つの引数の関数である必要があります。
プリミティブは、`fail` を返すか、`prove-all` を呼び出して続行する必要があります。

```lisp
(defun show-prolog-vars (vars bindings other-goals)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (if (null vars)
          (format t "~&Yes")
          (dolist (var vars)
              (format t "~&~a = ~a" var
                              (subst-bindings bindings var))))
  (if (continue-p)
          fail
          (prove-all other-goals bindings)))

```

プリミティブは述語シンボルの `clauses` プロパティのエントリとして表されるため、次のように `show-prolog-vars` をプリミティブとして登録する必要があります。

```lisp
(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)
```

最後に、Lisp 述語 `continue-p` は、ユーザーにさらに解法を表示するかどうかを尋ねます。

```lisp
(defun continue-p ()
 "Ask user if we should continue looking for solutions."
 (case (read-char)
  (#\; t)
  (#\. nil)
  (#\newline (continue-p))
  (otherwise
   (format t " Type ; to see more or . to stop")
   (continue-p))))
```

このバージョンは有限問題では以前のバージョンと同様に機能します。
唯一の違いは、セミコロンを入力するのはシステムではなくユーザーであることです。
利点は、このシステムを無限の問題にも使用できるようになったことです。
まず、リスト 2 がどのリストのメンバーであるかを確認します。

```lisp
> (?- (member 2 ?list))
?LIST = (2 . ?REST3302);
?LIST = (?X3303 2 . ?REST3307);
?LIST = (?X3303 ?X3308 2 . ?REST3312);
?LIST = (?X3303 ?X3308 ?X3313 2 . ?REST3317).
No.
```

この答えは、2 が 2 で始まるリスト、2 番目の要素が 2 であるリスト、3 番目の要素が 2 であるリストなどのメンバーであることを意味します。
ユーザーがセミコロンではなくピリオドを入力したとき、無限計算は停止しました。
「no」は、表示する回答がもうないことを意味します。回答がまったくない場合、ユーザーがピリオドを入力した場合、またはすべての回答が印刷された場合に表示されます。

さらに抽象的なクエリを実行することもできます。次のクエリの回答では、項目がリストの最初の要素、2 番目、3 番目、4 番目などの要素である場合に、その項目がリストの要素であることを示します。

```lisp
> (?- (member 2 ?list))
?LIST = (2 . ?REST3302);
?LIST = (?X3303 2 . ?REST3307);
?LIST = (?X3303 ?X3308 2 . ?REST3312);
?LIST = (?X3303 ?X3308 ?X3313 2 . ?REST3317).
No.
```
次に、関係の長さの定義を追加しましょう。

```lisp
(<- (length () 0))
(<- (length (?x . ?y) (1 + ?n)) (length ?y ?n))
```

以下に、長さを使用して 2 番目の引数、最初の引数、またはその両方を検索できることを示すクエリをいくつか示します。

```lisp
> (?- (length (a b c d) ?n))
?N = (1+ (1+ (1+ (1+ 0))));
No.
> (?- (length ?list (1+ (1+ 0))))
?LIST = (?X3869 ?X3872);
No.
> (?- (length ?list ?n))
?LIST = NIL
?N = 0;
?LIST = (?X3918)
?N = (1+ 0);
?LIST = (?X3918 ?X3921)
?N = (1+ (1+ 0)).
No.
```

次の 2 つのクエリは、`a` をメンバーとして持つ長さ 2 の 2 つのリストを表示します。
どちらのクエリも正しい答え、つまり「a」で始まるか終わる 2 つの要素のリストを返します。
ただし、これら 2 つの解法を生成した後の動作はまったく異なります。

```lisp
> (?- (length ?l (1 + (1 + 0))) (member a ?l))
?L = (A ?X4057);
?L = (?Y4061 A);
No.
> (?- (member a ?l) (length ?l (1 + (1 + 0))))
?L = (A ?X4081);
?L = (?Y4085 A);[Abort]
```

最初のクエリでは、length は 1 つの可能な解法、つまり 2 つの束縛されていない要素を含むリストのみを生成します。
`member` はこの解法を受け取り、最初の要素または 2 番目の要素のいずれかを `a` にインスタンス化します。

2 番目のクエリでは、`member` は潜在的な解法を生成し続けます。
最初の 2 つの部分解 (`a` は長さが不明なリストの最初または 2 番目のメンバー) は、`length` によって拡張され、リストの長さが 2 である解が生成されます。
その後、`member` はどんどん長いリストを生成し続けますが、`length` はそれを拒否し続けます。
`member` の定義では、後続の解法が長くなることが暗黙的に示されていますが、それが明示的にわかっていないため、いずれにせよそれらはすべて生成され、その後 `length` によって明示的にテストされて拒否されます。

この例では、純粋な論理プログラミング言語としての Prolog の限界が明らかになります。
ユーザーは、問題の論理だけでなく、制御の流れについても考慮する必要があることがわかります。
Prolog は、検索空間が十分に小さい場合はバックトラックしてすべての解法を見つけるほど賢いですが、検索空間が無限大 (または非常に大きい) の場合、プログラマーは依然として制御の流れを導く責任があります。
自動的な制御フローの点で、さらに多くのことを実現する言語を考案することは可能です。<a id="tfn11-4"></a><sup>[4](#fn11-4)</sup>
Prolog は、命令型言語と純粋論理の中間に位置する便利で効率的な言語です。

### バックトラッキングへのアプローチ

既存のプログラムに「小さな」変更を加えるように求められたとします。
問題は、単一の値を持つと考えられていた関数「f」が、特定の状況下では 2 つ以上の有効な回答を返すことが分かっている点です。
つまり、`f` は非決定的です。
(おそらく `f` は `sqrt` なので、負の数を扱いたいのです)。
プログラマーとしての選択肢は何ですか?
5 つの可能性が考えられます。

* 推測する。1 つの可能性を選択し、他の可能性を破棄します。これには、正しい推測を行う手段、または間違った推測から回復する手段が必要です。

* 知る。場合によっては、何が正しい選択であるかを判断するのに十分な追加情報を提供できることもあります。これは、追加情報を提供するために呼び出し関数を変更することを意味します。

* リストを返す。つまり、呼び出し関数は応答のリストを期待するように変更する必要があります。

* [セクション9.3](chapter9.md#s0020)で定義されている*パイプ*を返す。繰り返しになりますが、呼び出し関数はパイプを期待するように変更する必要があります。

* 推測して保存する。1 つの可能性を選択してそれを返しますが、後で他の可能性を計算できるように十分な情報を記録します。
これには、計算の現在の状態と、残りの可能性に関する情報を保存する必要があります。

最後の選択肢が最も望ましいです。
決して使用されない答えを計算する必要がないため、効率的です。
呼び出し関数 (および呼び出し関数の呼び出し関数)を応答のリストまたはパイプを期待するように変更する必要がないため、目立たない処理です。
残念ながら、これには 1 つの大きな問題があります。最初の選択が機能しない場合に元の状態に戻れるように、計算の現在の状態をパッケージ化して保存する方法が必要です。
Prolog インタプリタでは、現在の状態は目標のリストとして簡潔に表現されます。
他の問題では、全体の状態を要約するのはそれほど簡単ではありません。

[セクション 22.4](chapter22.md#s0025) で、Lisp の Scheme 方言が、まさに私たちが求めている機能、つまり計算の現在の状態を関数にパッケージ化し、それを保存して後で呼び出すことができる機能を提供する `call-with-current-continuation` 関数を提供していることがわかります。
残念ながら、Common Lisp には対応する関数はありません。

### 匿名変数

先に進む前に、*匿名変数*の概念を紹介しておくと便利です。これは、節またはクエリ内の他のすべての変数とは区別される変数ですが、プログラマーは名前を付ける手間を省きたいと考えています。
実際の Prolog では、匿名変数にはアンダースコアが使用されますが、ここでは単一の疑問符を使用します。
次の `member` の定義では、節内で必要のない用語内の位置に匿名変数を使用します。

```lisp
(<- (member ?item (?item . ?)))
(<- (member ?item (? . ?rest)) (member ?item ?rest))
```

ただし、節内で複数の匿名変数を許可しながらも、各匿名変数を他のすべての変数と区別できるようにする必要があります。
そのための 1 つの方法は、各匿名変数を一意の変数に置き換えることです。
関数 `replace-?-vars` は `gensym` を使用してまさにそれを実行します。
これはトップレベルのマクロ `<-` および `?-` にインストールされ、すべての節とクエリが適切に処理されます。

```lisp
(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(add-clause ',(replace-?-vars clause)))
(defmacro ?- (&rest goals)
  "Make a query and print answers."
  `(top-level-prove ',(replace-?-vars goals)))
(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eq exp '?) (gensym "?"))
        ((atom exp) exp)
        (t (reuse-cons (replace-?-vars (first exp))
                       (replace-?-vars (rest exp))
                       exp))))
```

節内で 1 回だけ使用される名前付き変数も匿名変数と見なすことができます。
これについては、[セクション12.3](chapter12.md#s0020)で別の方法で説明されています。

## 11.4 シマウマパズル

Prolog が非常に得意とするものの例として、論理パズルが挙げられます。
このパズルには 15 個の事実、つまり制約があります。

1. 5 つの家が一列に並んでおり、それぞれの家には所有者、ペット、タバコ、飲み物、色があります。

2. イギリス人は赤い家に住んでいます。

3. スペイン人は犬を飼っています。

4. コーヒーは温室で飲まれます。

5. ウクライナ人はお茶を飲みます。

6. 緑の家はアイボリーの家のすぐ右側にあります。

7. ウィンストンを吸う人はカタツムリを飼っています。

8. クールは黄色い家で吸われます。

9. ミルクは真ん中の家で飲まれます。

10. ノルウェー人は左から最初の家に住んでいます。

11. チェスターフィールドを吸う男はキツネを連れた男の隣に住んでいます。

12. 馬がいる家の隣の家ではクールが吸われています。

13. ラッキーストライクの喫煙者はオレンジジュースを飲みます。

14. 日本人はパーラメントを吸います。

15. ノルウェー人は青い家の隣に住んでいます。

答えるべき質問は、誰が水を飲むのか、そしてシマウマを所有しているのは誰なのか、ということです。
このパズルを解くには、まず `nextto` (「次の」) と `iright` (「すぐ右」) の関係を定義します。
これらは、ここで繰り返される「メンバー」と密接に関連しています。

```lisp
(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(<- (nextto ?x ?y ?list) (iright ?x ?y ?list))
(<- (nextto ?x ?y ?list) (iright ?y ?x ?list))

(<- (iright ?left ?right (?left ?right . ?rest)))
(<- (iright ?left ?right (?x . ?rest))
    (iright ?left ?right ?rest))

(<- (= ?x ?x))
```

また、恒等関係「=」も定義しました。
任意の x はそれ自身と等しいという単一の節があります。
これは `eq` または `equal` を実装していると思われるかもしれません。
実際、Prolog は、目標の 2 つの引数がそれぞれ `?x` で単一化されるかどうかを確認するために単一化を使用するため、`=` は単一化であることを意味します。

これで、単一の（長い）節でシマウマパズルを定義する準備が整いました。
変数 `?h` は 5 つの家のリストを表し、各家は `(house *nationality pet cigarette drink color*)` という形式の節で表されます。
変数 `?w` は水を飲む人、 `?z` はシマウマの飼い主です。
パズルの 15 個の制約はそれぞれ `zebra` の本体にリストされていますが、制約 9 と 10 は最初の制約に結合されています。
制約 2「イギリス人は赤い家に住んでいる」について考えてみましょう。これは、「国籍がイギリス人で、色が赤で、家のリストのメンバーである家がある」と解釈されます。言い換えると、`(member (house englishman ? ? ? red) ?h)`です。他の制約も同様に簡単です。

```lisp
(<- (zebra ?h ?w ?z)
 ;; Each house is of the form:
 ;; (house nationality pet cigarette drink house-color)
 (= ?h ((house norwegian ? ? ? ?)                  ;1,10
        ?
        (house ? ? ? milk ?) ? ?))                 ; 9
 (member (house englishman ? ? ? red) ?h)          ; 2
 (member (house spaniard dog ? ? ?) ?h)            ; 3
 (member (house ? ? ? coffee green) ?h)            ; 4
 (member (house ukrainian ? ? tea ?) ?h)           ; 5
 (iright (house ? ? ? ? ivory)                     ; 6
         (house ? ? ? ? green) ?h)
 (member (house ? snails winston ? ?) ?h)          ; 7
 (member (house ? ? kools ? yellow) ?h)            ; 8
 (nextto (house ? ? chesterfield ? ?)              ;11
         (house ? fox ? ? ?) ?h)
 (nextto (house ? ? kools ? ?)                     ;12
         (house ? horse ? ? ?) ?h)
 (member (house ? ? luckystrike orange-juice ?) ?h);13
 (member (house japanese ? parliaments ? ?) ?h)    ;14
 (nextto (house norwegian ? ? ? ?)                 ;15
         (house ? ? ? ? blue) ?h)
 ;; Now for the questions:
 (member (house ?w ? ? water ?) ?h)                ;Q1
 (member (house ?z zebra ? ? ?) ?h))               ;Q2
```

パズルの質問と解答は次のとおりです。

```lisp
> (?- (zebra ?houses ?water-drinker ?zebra-owner))
?HOUSES = ((HOUSE NORWEGIAN FOX KOOLS WATER YELLOW)
                      (HOUSE UKRAINIAN HORSE CHESTERFIELD TEA BLUE)
                      (HOUSE ENGLISHMAN SNAILS WINSTON MILK RED)
                      (HOUSE SPANIARD DOG LUCKYSTRIKE ORANGE-JUICE IVORY)
                      (HOUSE JAPANESE ZEBRA PARLIAMENTS COFFEE GREEN))
?WATER-DRINKER = NORWEGIAN
?ZEBRA-OWNER = JAPANESE.
No.
```

これには 278 秒かかり、プロファイリング (288 ページを参照) により、関数 `prove` が 12,825 回呼び出されたことが明らかになりました。
証明の呼び出しは *論理的推論* と呼ばれており、私たちのシステムは 1 秒あたり 12825/278 = 46 の論理的推論、つまり LIPS を実行しています。
優れた Prolog システムは 10,000 ～ 100,000 LIPS 以上で動作するので、これはかろうじてなんとか機能していると言えます。

問題に対する小さな変更は、検索時間に大きな影響を与える可能性があります。
たとえば、最初の家が 2 番目の家のすぐ右にある場合、または 2 番目の家が最初の家のすぐ右にある場合に、関係 `nextto` が成立します。
これらの節がどの順序で列挙されるかは任意であり、どの順序で列挙されても違いはないと考える人もいるかもしれません。
実際、これら 2 つの節の順序を逆にすると、実行時間はほぼ半分に短縮されます。

## 11.5 バックトラッキングと単一化の相乗効果

Prolog のバックトラッキングによる後方連鎖は、問題に対する可能な解法を生成する強力な手法です。
これにより、可能な解法が 1 つずつ検討され、候補となる解法が拒否されると次の解法が提案される、*生成してテストする*戦略を簡単に実装できるようになります。
しかし、生成とテストは、可能な解法の空間が小さい場合にのみ実行可能です。

シマウマパズルでは、5 つの家それぞれに 5 つの属性があります。
したがって、5!<sup>5</sup>、つまり 240 億を超える候補解法があり、一度に 1 つずつテストするには多すぎます。
このパズルで生成とテストを可能にするのは、単一化の概念（対応する論理変数の概念を含む）です。
完全な候補解法を列挙する代わりに、単一化により、*部分的な*候補を指定できます。
最初に、5 つの家があり、ノルウェー人が一番左に住んでいて、牛乳を飲む人が真ん中に住んでいることがわかっています。
これら 2 つの制約を満たす完全な候補をすべて生成するのではなく、残りの家と属性を匿名の論理変数で単一化することで、残りの情報を曖昧なままにします。
次の制約 (2 番) では、イギリス人は赤い家にいます。
「member」の書き方により、まずイギリス人を一番左の家に配置しようとします。
これは、イギリス人とノルウェー人が単一化できないため却下され、次の可能性が考慮され、イギリス人が第 2 ハウスに配置されます。
しかし、2 番目の家の他の特徴は指定されていないため、イギリス人の家が緑、黄色などであるかどうかを個別に推測する必要はありませんでした。
検索は継続され、必要な部分のみ入力し、単一化が失敗するたびにバックアップが行われます。

この問題では、単一化は遅延マクロ (281 ページ) と同じ目的を果たします。
これにより、ある属性の値の決定を可能な限り遅らせることができますが、同じ属性に 2 つの異なる値を与えようとする解法をすぐに拒否することができます。
こうすることで、計算が行われる前に後戻りすることになった場合でも時間を節約でき、後で値を入力できるようになります。

単一化を拡張して、単一化がより多くの作業を実行し、バックトラックがより少ない作業を実行するようにすることが可能です。
次の計算を考えてみましょう。

```lisp
(?- (length ?l 4)
        (member d ?l) (member a ?l) (member c ?l) (member b ?l)
        (= ?l (a b c d)))
```

最初の 2 行はリスト (`dacb`) の順列を生成し、3 行目は順列が (`abcd`) に等しいかどうかをテストします。
作業のほとんどはバックトラッキングによって行われます。
別の方法としては、単一化を拡張して、定数や変数だけでなくリストも処理できるようにすることです。
`length` や `member` のような述語は、リストの表現について知っておく必要があるプリミティブになります。
すると、上記のプログラムの最初の 2 行は `set ?l` を `#s (list :length 4 :members (dacd))` のようなものに設定します。
3 行目は拡張された単一化手順の呼び出しであり、`?l` を次のように指定します。

```lisp
#s(list :length 4 imembers (d a c d) :order (abc d))
```

単一化手順をより複雑にすることで、バックトラッキングの必要性が完全になくなります。

**演習 11.3 [s]** `member` テストを遅延させる単一化アルゴリズムは、シマウマパズルにとって良いアイデアでしょうか、それとも悪いアイデアでしょうか?

## 11.6 破壊的単一化

[セクション 11.2](#s0015) で見たように、変数の束縛 リストを追跡するのは少し難しいです。
また、束縛 リストが大きくなると、リストを線形に検索する必要があり、束縛 リストを保持するためのスペースを割り当てる必要があるため、非効率になりがちです。
別の実装としては、`unify` を破壊的な操作に変更する方法があります。
このアプローチでは、束縛 リストはありません。
代わりに、各変数は、その束縛用のフィールドを含む構造体として表されます。
変数が別の式と単一化されると、変数の束縛 フィールドは式を指すように変更されます。
このような変数は、疑問符で始まるシンボルとしての変数の実装と区別するために `vars` と呼ばれます。
`vars` は次のコードで定義されます。

```lisp
(defconstant unbound "Unbound")
(defstruct var name (binding unbound))
(defun bound-p (var) (not (eq (var-binding var) unbound)))
```

マクロ `deref` は変数の束縛を取得し、それが束縛されていない変数または非変数式である場合はその引数を返します。
変数は別の変数に束縛され、その変数は最終値に束縛されるため、ループが含まれます。

通常、deref をマクロとして実装するのは悪い習慣だと考えられています。なぜなら、呼び出し側が `(deref x)` ではなく `(setf x (deref x))` と記述するのであれば、インライン関数として実装できるからです。
ただし、次のセクションで説明する Prolog コンパイラの一部のバージョンによって生成されたコードには deref が表示されます。
したがって、生成されたコードをよりきれいに見せるために、私は `deref` マクロを使用することにしました。

```lisp
(defmacro deref (exp)
  "Follow pointers for bound variables."
  '(progn (loop while (and (var-p ,exp) (bound-p ,exp))
                        do (setf ,exp (var-binding ,exp)))
                  ,exp)
```

以下の関数 `unify!` は `unify` の破壊的なバージョンです。
これは、成功した場合は true を返し、失敗した場合は false を返す述語であり、変数束縛を変更する副作用があります。

```lisp
(defmacro deref (exp)
  "Follow pointers for bound variables."
  '(progn (loop while (and (var-p ,exp) (bound-p ,exp))
                        do (setf ,exp (var-binding ,exp)))
                  ,exp))
```

`vars` を読みやすくするために、`:print-function` をインストールすることができます:

```lisp
(defun unify! (x y)
 "Destructively unify two expressions"
 (cond ((eql (deref x) (deref y)) t)
       ((var-p x) (set-binding! x y))
       ((var-p y) (set-binding! y x))
       ((and (consp x) (consp y))
       (and (unify! (first x) (first y))
            (unify! (rest x) (rest y))))
       (t nil)))
(defun set-binding! (var value)
 "Set var's binding to value. Always succeeds (returns t)."
 (setf (var-binding var) value)
 t)
```

これは、慎重に作成された `:print-function` の最初の例です。
注目すべき点が3つあります。
まず、引数として渡されたストリームに明示的に書き込みます。
デフォルトのストリームには書き込みません。
次に、変数 `depth` を `*print-level*` と比較し、深さを超えた場合は変数名だけを出力します。
3 番目に、`write` を使用して束縛を出力します。
これは、write が `*print-escape*`、`*print-pretty*` などの現在の値に注意を払うためです。
`prinl` や `print` などの他の印刷関数はこれらの変数に注意を払いません。

ここで、バックトラッキングの目的で、`set-binding!` に作成された束縛を追跡させ、後で元に戻せるようにします。

```lisp
(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))
(defun set-binding! (var value)
 "Set var's binding to value, after saving the variable
 in the trail. Always returns t."
 (unless (eq var value)
   (vector-push-extend var *trail*)
   (setf (var-binding var) value))
 t)
(defun undo-bindings! (old-trail)
 "Undo all bindings back to a given point in the trail."
 (loop until (= (fill-pointer *trail*) old-trail)
   do (setf (var-binding (vector-pop *trail*)) unbound)))
```

ここで、それぞれが異なる新しい変数を作成する方法が必要です。
これは、各変数に新しい名前を `gensym-ing` することで実行できますが、より迅速な解法は、カウンターを漸進的することです。
コンストラクター関数 `?` は、新しい整数の名前を持つ新しい変数を生成するように定義されています。
これは厳密には必要ではありません。自動的に提供されるコンストラクター `make-var` を使用することもできます。
しかし、新しい匿名変数を提供する操作は、名前付き変数を提供する操作とは大きく異なるため、独自の機能が必要であると考えました。
さらに、`make-var` はキーワード引数を処理する必要があるため、効率が低下する可能性があります。
関数 `?` には引数がなく、`var` 構造体のスロットに指定されたデフォルト値を割り当てるだけです。

```lisp
(defvar *var-counter* 0)
(defstruct (var (:constructor ? ())
                      (:print-function print-var))
  (name (incf *var-counter*))
  (binding unbound))
```

次のステップとして妥当なのは、破壊的単一化を使用してより効率的なインタプリタを作成することです。
ただし、これは演習として残し、代わりにインタプリタを脇に置いて、次の章でコンパイラを開発します。

## 11.7 Prolog 内の Prolog

この章の冒頭で述べたように、Prolog には、プログラム開発にとって Lisp を魅力的なものにしている多くの同じ機能があります。
Lisp で Lisp インタープリタを書くのが簡単であるのと同様に、Prolog で Prolog インタープリタを書くのも簡単です。
次の Prolog メタインタープリタには 3 つの主要な関係があります。
関係節は、解釈される規則と事実を構成する節を格納するために使用されます。
関係「prove」は目標を証明するために使用されます。
これは、ゴールのリストを証明しようとする `prove-all` を呼び出します。`prove-all` は次の 2 つの方法で成功します: (1) リストが空の場合、または (2) 最初のゴールに一致する節があり、その節の本体を証明でき、その後に残りのゴールが続く場合:

```lisp
(<- (prove ?goal) (prove-all (?goal)))
(<- (prove-all nil))
(<- (prove-all (?goal . ?goals))
    (clause (<- ?goal . ?body))
    (concat ?body ?goals ?new-goals)
    (prove-all ?new-goals))
```

ここで、メンバー関係を定義するために、データベースに 2 つの節を追加します。

```lisp
(<- (clause (<- (mem ?x (?x . ?y)))))
(<- (clause (<- (mem ?x (? . ?z)) (mem ?x ?z))))
```

最後に、インタプリタを使用して目標を証明できます。

```lisp
(?- (prove (mem ?x (1 2 3))))
?X = 1;
?X = 2;
?X = 3;
No.
```

## 11.8 PrologとLispの比較

Prolog を AI (および一般的なプログラム開発) に適した言語にしている機能の多くは、Lisp の機能と同じです。
Lisp を従来の言語と異なるものにしている機能のリスト (25 ページを参照) を再考し、Prolog が何を提供しているかを見てみましょう。

* *リスト (およびその他のデータ型) の組み込みサポート* リストまたは構造体 (構造体が推奨されます) を使用して、新しいデータ型を簡単に作成できます。コンポーネントの読み取り、表示、アクセスのサポートが自動的に提供されます。数字、記号、文字もサポートされています。
ただし、論理変数は変更できないため、特定のデータ構造と操作は提供されません。たとえば、Prolog ではベクトルの要素を更新する方法はありません。

* *自動ストレージ管理* プログラマーは、オブジェクトの再利用を心配することなく、新しいオブジェクトを割り当てることができます。ほとんどのデータはヒープ割り当てではなくスタック割り当てできるため、通常、Prolog では Lisp よりも再利用が高速です。

* *動的型付け* 宣言は必要ありません。実際、一部の実装では型宣言が許可されていますが、型宣言を行う標準的な方法はありません。一部の Prolog システムでは、fixnum のみが提供されているため、大規模な宣言クラスは不要になります。

* *第一級関数* Prolog には `lambda` に相当するものはありませんが、組み込み述語 `call` を使用すると、節 (データの一部) を目標として呼び出すことができます。バックトラッキング選択ポイントは第一級オブジェクトではありませんが、Lisp の継続と非常によく似た方法で使用できます。

* *単一化された構文* Lisp と同様に、Prolog はプログラムとデータの両方に対して単一化された構文を持っています。
これにより、Prolog でインタープリタとコンパイラを簡単に記述できるようになります。Lisp の前置演算子リスト表記はより画一的ですが、Prolog では中置演算子と後置演算子が許可されており、一部のアプリケーションではより自然な場合があります。

* *インタラクティブな環境* 式はすぐに評価できます。高品質の Prolog システムは、コンパイラとインタープリタの両方に加え、多数のデバッグ ツールも提供します。

* *拡張性* Prolog 構文は拡張可能です。プログラムとデータは同じ形式を共有するため、Prolog でマクロと同等のものを記述し、埋め込み言語を定義することが可能です。ただし、結果のコードが効率的にコンパイルされることを保証することは困難になる可能性があります。
Prolog コンパイルの詳細は実装に依存します。

物事の全体像を把握するために、Lisp は利用可能な最高レベルの言語の 1 つであると同時に、汎用的なアセンブリ言語でもあることを考慮してください。
データ、機能、制御の抽象化を簡単にキャプチャできる、高水準言語です。
Lisp は、現代のコンピューターで利用可能な操作を直接反映したスタイルで記述できるため、優れたアセンブリ言語です。

Prolog は一般にアセンブリ言語ほど効率的ではありませんが、少なくとも一部の問題に関しては、仕様言語としてはより簡潔になります。
ユーザーは仕様、つまり問題領域で保持できる関係を記述する公理のリストを記述します。
これらの仕様が正しい形式であれば、プログラマーが明示的なアルゴリズムを提供しなくても、Prolog の自動バックトラッキングによって解法を見つけることができます。
他の問題の場合、探索空間が大きすぎたり無限であったり、あるいは Prolog のバックアップ付き単純な深さ優先探索が柔軟性に欠けることになります。
この場合、Prolog は仕様言語ではなくプログラミング言語として使用する必要があります。
プログラマーは Prolog の検索戦略を認識し、それを使用して手元の問題に適切なアルゴリズムを実装する必要があります。

Prolog は Lisp と同様に、いくつかの一般的な誤解によって不当に苦しめられてきました。
初期の実装はインタープリタ型であり、インタープリタの作成に使用されていたため、非効率的な言語であると考えられてきました。
しかし、現代のコンパイルされた Prolog は非常に効率的です ([Warren et al.
1977](bibliography.md#bb1335)およびVan Roy 1990)を参照。
Prolog をプログラミング言語としてではなく、それ自体が解法として見る誘惑があります。
そのような見解をとる人々は、Prolog の深さ優先探索戦略と述語計算の基礎が柔軟性に欠けていると異議を唱えます。
この反論に対して、Prolog プログラマーは、Lisp や他の言語と同様に、この言語が提供する機能を使用して、より強力な検索戦略と表現を構築します。

## 11.9 歴史と参考文献

コーデル・[グリーン(1968)](bibliography.md#bb0490)は、定理証明の数学的結果を使用して推論を行い、それによって質問に答えることができるという見解を初めて表明した人物である。
しかし、当時使用されていた主要な手法である導出原理による定理証明(resolution theorem proving, [Robinson 1965](bibliography.md#bb0995)を参照）では、検索が適切に制約されなかったため、実用的ではありませんでした。
目標指向コンピューティングのアイデアは、ロボットの問題解決のための PLANNER 言語に関する Carl Hewitt の研究 (1971 年) で開発されました。
彼は、ユーザーが演繹を制御する方法について明確なヒントを提供することを提案しました。

ほぼ同じ時期に、Alain Colmerauer は独自に自然言語解析を実行するシステムを開発していました。
彼のアプローチは、論理言語を弱めて、計算的に複雑なステートメント (論理和など) を作成できないようにするというものでした。
Colmerauerと彼のグループは、1972年の夏にAlgol-Wを使用して最初のPrologインタープリタを実装しました（[Roussel 1975](bibliography.md#bb1005)を参照）。
「programmation en logique」の略語として Prolog という名前を思いついたのは、ルーセルの妻ジャクリーヌでした。最初の大規模な Prolog プログラムは、同じ年に完成した自然言語システムでした ([Colmerauer et al.
1973年](bibliography.md#bb0255))。
フランス語よりも英語の方が得意な人のために、[Colmerauer (1985)](bibliography.md#bb0245) が Prolog の概要を説明しています。
ロバート・コワルスキーは、一般的に Prolog の共同発明者と考えられています。
1974 年の論文では彼のアプローチの概要が述べられており、1988 年の論文では初期の論理プログラミング作業に関する歴史的レビューが述べられています。

現在、Prolog に関する教科書は数十冊あります。
私の中では、このうち 6 つが特に目立っています。
Clocksin と Mellish の *Programming in Prolog* (1987) は最初の本であり、今でも最高の本の一つです。
Sterling と Shapiro の *The Art of Prolog* (1986) にはより充実した例が掲載されていますが、参考資料としては完全ではありません。
もう少し数学的な観点からの優れた概要としては、Pereira と Shieber の「Prolog and Natural-Language Analysis」(1987) があります。
この本は Prolog を扱っているだけでも価値があり、言語理解のための論理プログラミングの使用についても優れた入門書となっています (このテーマの詳細についてはパート V を参照してください)。
O'Keefe の *The Craft of Prolog* (1990) には、いくつかの高度なテクニックが紹介されています。
O'Keefe 氏は確かに Prolog コミュニティで最も影響力のある発言者の 1 人です。
彼は、コーディング スタイルの良し悪しについて明確な見解を持っており、自分の意見を共有することに躊躇しません。
読者は、この本が Clocksin と Mellish の本に関する一連のメモから発展したものであり、ところどころ構成が不十分な点があることに気付くでしょう。
しかし、他のどこにも見つからない高度な資料が含まれています。
書籍としてまとめられた別のノート集は、Coelho と Cotta の *Prolog by Example* です。1988 年に出版されたこの本は、1980 年の書籍 *How to Solve it in Prolog* の改訂版です。以前の書籍は、この分野の隠れた名著であり、Prolog プログラマーの世代を教育するのに役立ちました。
どちらのバージョンにも豊富な例が含まれていますが、残念ながらドキュメントが少なく、タイプミスが多くあります。
最後に、Ivan Bratko の *Prolog Programming for Artificial Intelligence* (1990) では、Prolog の観点から AI の入門資料が紹介されています。

Maier と Warren の *Computing with Logic* (1988) は、Prolog の実装に関心のある人にとって最適な参考書です。
これは、Prolog の変数なしバージョン用の単純なインタープリタから始まり、その後、完全な言語に移行し、その途中でインタープリタに改良を加えていきます。
(2 番目の著者である David S.
ストーニーブルックのウォーレンは、デイビッド H とは異なります。
D.
ウォーレンは以前はエディンバラにいて、現在はブリストルにいます。
二人ともPrologの専門家です。

ロイドの*Foundations of Logic Programming*(1987) は、Prolog および関連言語の形式意味論の理論的説明を提供します。
[ラセズ他
(1988)](bibliography.md#bb0705)と[Knight (1989)](bibliography.md#bb0625)は単一化の概要を示しています。

Prolog を拡張して論理プログラミングの理想に近づけようとする試みは数多く行われてきました。
MU-Prolog と NU-Prolog ([Naish 1986](bibliography.md#bb0890)) および Prolog III ([Colmerauer 1990](bibliography.md#bb0250)) という言語は特に興味深いです。
後者には、≠ 関係の体系的な処理と無限ツリーの解釈が含まれます。

## 11.10 演習

**演習 11.4 [m]** 1 つ以上の有効な回答が表示された後に「no」と表示されるのを見ると、少し混乱します。
プログラムを変更して、回答がまったくない場合にのみ「no」を出力し、その他の場合には「no more」を出力するようにします。

**演習 11.5 [h]** 少なくとも 6 冊の本 (Abelson and Sussman 1985、[Charniak and McDermott 1985](bibliography.md#bb0175)、Charniak et al.
1986、[Hennessey 1989](bibliography.md#bb0530)、[Wilensky 1986](bibliography.md#bb1390)、および[Winston and Horn 1988](bibliography.md#bb1410))は、共通のエラーを持つ単一化アルゴリズムを提示しています。
これらすべてにおいて、(`?x ?ya`) と (`?y ?x ?x`) を単一化する際に問題があります。
これらのテキストの一部では、2 つの引数間で変数が共有されていないコンテキストで `unify` が呼び出されると想定しています。
ただし、次の例が示すように、バグの疑いはまだ残っています。

```lisp
> (unify '(f (?x ?y a) (?y ?x ?x)) '(f ?z ?z))
((?Y . A) (?X . ?Y) (?Z ?X ?Y A))
```

この微妙なバグにもかかわらず、私は読者にそれぞれの本を強くお勧めします。
同じアルゴリズムの異なる実装を比較するのは興味深いことです。
相違点よりも類似点の方が多いことが判明しました。
これは 2 つのことを示しています: 
1. これらの関数の記述には一般的に合意されたスタイルがあり、
2. 優れたプログラマーは他の人のコードを見る機会を活用することがあります。

質問は、この章で紹介したアルゴリズムの正しさを非形式的に証明できるかということです。
まず、仕様を明確に述べます。
それを他のアルゴリズムに適用し、どこで間違いが起こるかを示します。
次に、この章の `unify` 関数が正しいことを証明できるかどうかを確認します。
完全な証明ができない場合でも、少なくともアルゴリズムが常に終了することを証明できますか?
この問題の詳細については[Norvig 1991](bibliography.md#bb0915)を参照してください。

**演習 11.6 [h]** 論理変数は Prolog にとって非常に基本的なものなので、効率的に使用したいと考えます。
ほとんどの実装では、構造体は小さなオブジェクトには最適な選択ではありません。
変数には名前と束縛の 2 つのスロットしかないことに注意してください。
束縛は重要ですが、名前は結果の表示にのみ必要であり、ほとんどの変数では任意です。
これは代替実装を示唆しています。
各変数は、変数の束縛のcons cellと、型を示す任意のマーカーになります。
このマーカーは `variable-p` によってチェックされます。
変数名は、各クエリの前にクリアされるハッシュ テーブルに保存できます。
変数に対してこの表現を実装し、それを構造体による表現と比較します。

**演習 11.7 [m]** 匿名変数の次の代替実装を検討してください。マクロ `<-` と `?-` をそのままにして、アサーションとクエリで匿名変数が許可されるようにします。
代わりに、`unify` を変更して、匿名変数に対して何でも一致できるようにします。

```lisp
(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
              ((eql x y) bindings)
              ((or (eq x '?) (eq y '?)) bindings)      ;***
              ((variable-p x) (unify-variable x y bindings))
              ((variable-p y) (unify-variable y x bindings))
              ((and (consp x) (consp y))
                (unify (rest x) (rest y)
                          (unify (first x) (first y) bindings)))
              (t fail)))
```

この選択肢は正しいでしょうか?
もしそうなら、非形式的な証明を示してください。
そうでない場合は反例を挙げてください。

**演習 11.8 [h]** 束縛 リストの代わりに破壊的単一化を使用する Prolog インタープリタのバージョンを記述します。

**演習 11.9 [m]** 父、母、息子、娘、およびそれぞれの祖父母のバージョンを表す Prolog 規則を記述します。
親、子、妻、夫、兄弟、姉妹、叔父、叔母も定義します。
どの関係がプリミティブ (Prolog データベースに格納されている) で、どの関係が規則によって導出されるかを決定する必要があります。

たとえば、G が C の親である P の父親である場合、G は C の祖父であるという祖父の定義を次に示します。

```lisp
(<- (grandfather ?g ?c)
        (father ?g ?p)
        (parent ?p ?c))
```

**演習11.10 [m]** 次の問題は[Wirth 1976](bibliography.md#bb1415)に掲載されています。

*私は、成人した娘（D と呼ぶ）を持つ未亡人（W と呼ぶ）と結婚しました。
頻繁に訪ねてきた父（F）は、私の継娘に恋をして結婚しました。
こうして私の父は私の義理の息子となり、私の継娘は私の母となったのです。
数か月後、妻は息子（S<sub>1</sub>）を出産しました。その子は私の父の義理の兄弟であり、叔父でもありました。
私の父の妻、つまり私の継娘にも息子がいました（S<sub>2</sub>）。*

前の演習で定義した述語を使用してこの状況を表現し、その結論を検証し、この物語の語り手が自分の祖父であることを証明してください。

**演習11.11 [d]** 次の例を思い出してください。

```lisp
> (?- (length (a b` c `d) ?n))
?N = (1 + (1 + (1 + (1 + 0))));
```

単一化の概念を拡張することで、`(1+ (1+ (1+ (1+ 0))))` の代わりに 4 を生成することが可能です。
[AÏt-Kaci et al.
1987](bibliography.md#bb0025) には、これを行う方法についてのアイデアがいくつか記載されているかもしれません。

**演習 11.12 [h]** 関数 `rename-variables` は、`unify` の最初の引数の変数と 2 番目の引数の変数の混乱を避けるために必要でした。
別の方法としては、`unify` を変更して、引数ごとに 1 つずつ、合計 2 つの束縛 リストを取得し、それらを別々に保持するようにします。
この代替案を実装してください。

## 11.11 回答

**回答 11.9** プリミティブとして、単項述語`male`と`female`、および二項述語`child`と`married`を選択します。
前者はまず子供を、後者はまず夫を連れ去ります。
これらのプリミティブが与えられた場合、次の定義を行うことができます。

```lisp
(<- (father ?f ?e)   (male ?f) (parent ?f ?c))
(<- (mother ?m ?c)   (female ?m) (parent ?m c))
(<- (son ?s ?p)      (male ?s) (parent ?p ?s))
(<- (daughter ?s ?p) (male ?s) (parent ?p ?s))

(<- (grandfather ?g ?c)     (father ?g ?p) (parent ?p ?c))
(<- (grandmother ?g ?c)     (mother ?g ?p) (parent ?p ?c))
(<- (grandson ?gs ?gp)      (son ?gs ?p) (parent ?gp ?p))
(<- (granddaughter ?gd ?gp) (daughter ?gd ?p) (parent ?gp ?p))

(<- (parent ?p ?c)   (child ?c ?p))
(<- (wife ?w ?h)     (married ?h ?w))
(<- (husband ?h ?w)  (married ?h ?w))

(<- (sibling ?x ?y)  (parent ?p ?x) (parent ?p ?y))
(<- (brother ?b ?x)  (male ?b) (sibling ?b ?x))
(<- (sister ?s ?x)   (female ?s) (sibling ?s ?x))
(<- (uncle ?u ?n)    (brother ?u ?p) (parent ?p ?n))
(<- (aunt ?a ?n)     (sister ?a ?p) (parent ?p ?n  ))
```

Prolog では *真の* 定義を表現する方法はないことに注意してください。
「C が P の子である場合に限り、P は C の親である」と言いたいのですが、Prolog では双条件式を一方向にしか表現できません。

**回答 11.10** これまでの定義では継親関係を考慮していないため、親の概念を継親まで含めるように拡張する必要があります。
無限ループを避けるために、定義は慎重に記述する必要があります。
戦略としては、定義された用語を厳密な階層に構造化します。つまり、4 つのプリミティブが最下部にあり、次に親がプリミティブの観点から定義され、他の用語が親とプリミティブの観点から定義されます。

義理の息子の定義も示します。

```lisp
(<- (parent ?p ?c) (married ?p ?w) (child ?c ?w))
(<- (parent ?p ?c) (married ?h ?p) (child ?c ?w))
(<- (son-in-law ?s ?p) (parent ?p ?w) (married ?s ?w))
```

これでクエリを実行する準備が整いました。

```lisp
(<- (male I)) (<- (male F)) (<- (male S1)) (<- (male S2))
(<- (female W)) (<- (female D))
(<- (married I W))
(<- (married F D))
(<- (child D W))
(<- (child I F))
(<- (child S1 I))
(<- (child S2 F))
```

----------------------

<a id="fn11-1"></a><sup>[1](#tfn11-1)</sup>
実際には、フランスのグループによって発明されたため、*programmation en logique* と呼ばれます (382 ページを参照)。

<a id="fn11-2"></a><sup>[2](#tfn11-2)</sup>
実際、これは Lisp の `member` というよりは Lisp の `find` に似ています。
この章では、`member` の従来の Prolog 定義を採用しました。

<a id="fn11-3"></a><sup>[3](#tfn11-3)</sup>
別のアプローチについては、演習 11.12 を参照してください。

<a id="fn11-4"></a><sup>[4](#tfn11-4)</sup>
MU-Prolog言語とNU-Prolog言語を参照してください（[Naish 1986](bibliography.md#bb0890)）。

#OCaml@p : A debugging print system for OCaml

OCaml@p is a tool supporting debug in OCaml programming. When you compile a program by this tool, this tool generates definition of print function automatically, and inserts function calls to print expression attached marker [@p] automatically.

note : This tool doesn't consider use in combination with other PPX tools using attributes.

#How to use

##installation
Version : 1.0.1

required OCaml version : OCaml 4.03.0

OPAM released

install

```
opam install ocaml_at_p
```

uninstall

```
opam remove ocaml_at_p
```

##How to compile with OCaml@p

When you compile a.ml with OCaml@p

```
ocamlfind ocamlc(ocamlopt) -package ocaml_at_p -linkpkg a.ml
```

##How to write code

###Marker for print

In OCaml@p, when you write marker [@p] behind expression that you want to print the return value, so you can print. The marker [@p] is attribute syntax in OCaml.

```
let add x y = x + y [@p x] [@p y] [@p]

let a = add 1 10
```

Then，markers (`[@p x],[@p y],[@p]`) are attached the expression (`x + y`)．Two marker `[@p x]` and `[@p y]` are used to print values of expressions `x` and `y` ，and the marker `[@p]` is the value of the expression `x + y`．The following is result of run the program.

```
1 			<- [@p x] の出力
10			<- [@p y] の出力
11			<- [@p]   の出力
```

###Marker types

There are two types marker `[@p]` and `[@ps]`.

* `[@p]` - newline after printing

* `[@ps]` - not newline after printing

###Outer module check [@@@ppopen]

This tool cannot be sure to define print function that print a value of datatype defined in other ml files, so it is difficulty for users to understand error messages. Then, users need to write `[@@@ppopen module_name]` in Toplevel of ml file written markers to be clear that the ml files of module are compiled by OCaml@p.

---

# OCaml@p : OCamlにおけるデバッグ出力機構 

OCamlプログラムのデバッグ出力をサポートするツールです．このツールを用いてコンパイルすると，型定義から型に対応した出力関数定義を自動生成します．また，マーカ[@p]のついた式を出力する関数呼び出しを自動で挿入します．

注 : 本システムでは他のattributeを用いたPPXツールとの併用は考慮していません．

#使用方法

##インストール方法
Version : 1.0.1

必要な OCaml version : OCaml 4.03.0

opam でリリースされています

install

```
opam install ocaml_at_p
```

uninstall

```
opam remove ocaml_at_p
```

##OCaml@pを用いたコンパイル方法

a.mlをOCaml@pでコンパイルするとき

```
ocamlfind ocamlc -package ocaml_at_p -linkpkg a.ml
```

##コードの記述方法

###出力マーカ [@p]

OCaml@pでは返す値を出力したい式にマーカ[@p]を記述することで，出力することができます．マーカはOCamlのattribute構文を用いています．


```
let add x y = x + y [@p x] [@p y] [@p]

let a = add 1 10
```

ここで，`[@p x],[@p y],[@p]`は式`x + y`に付与されています．`[@p x],[@p y]`はそれぞれ`x,y`の値を，`[@p]`は`x + y`の値を出力します．このプログラムを実行した時の出力結果は以下のようになります．

```
1 			<- [@p x] の出力
10			<- [@p y] の出力
11			<- [@p]   の出力
```

###マーカの種類

マーカは2種類存在する．

* `[@p]` - 出力後に改行

* `[@ps]` - 出力後に改行しない

###外部モジュールチェック [@@@ppopen]

本システムでは他のmlファイル内で定義されたデータ型を出力する際に，出力関数が定義されているか確かめる術がないため，Unboundエラーが出てしまい、エラー内容がわかりづらい．そこで，そのモジュールのmlファイルがOCaml@pを用いてコンパイルされていることを、ユーザが出力を行うファイルのトップレベルに`[@@@ppopen モジュール名]`と記述するようにした．

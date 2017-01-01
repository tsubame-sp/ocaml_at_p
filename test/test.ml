[@@@ppopen Mylist]

open Mylist.M

module M2 : sig
    type t = int * string
    val first : t -> int
end = struct
    type t = int * string
    let first (a : t) = fst a
end

module Incl = struct
    include Mylist.M
end

type m = int

type m' = m

type 'a v = A of 'a

type 'a vv = 'a v = A of 'a

type ('a,'b) v2 = A of 'a | B of 'b | C of 'a

and ('a,'b) v3 = Tup of ('a * 'b)
               | Lis of ('a * 'b) list
               | Fun of ('a -> 'b)

type r = {a:int list;b:float*string}

type ('a,'b) r2 = {a2:'a;b2:'b}

type odd = So of even

and even = Se of odd | O

type ex = ..

type ex += Aex of int

let _ = Aex 10 [@p]

type ex += Bex of ex

let _ = Bex (Aex 10) [@p]

type p = [`A of p | `N]

type p2 = [p | `B of p2]

type podd = [`So of peven]

and peven = [`Se of podd | `O]

class c = object
    val mutable list = ( [] : int list )
    method get = list
    method add a = list <- (a :: list)
end

and ['a] c2 = object(self)
    val mutable list = ( [] : 'a list )
    method get = list
    method add a = list <- (a :: list)
end

let () = Format.(pp_print_newline std_formatter ())

let _ = 1 [@p]
let _ = 1 [@p _this + 10]
let _ = 3.14 [@p]
let _ = 'A' [@p]
let _ = true [@p]
let _ = "abcde" [@p]
let _ = ("%d" : _ format6) [@p]

let _ = () [@p]
let _ = Bytes.make 10 'B' [@p]

let _ = stdin [@p]
let _ = stdout [@p]
let _ = stderr [@p]
let _ = ("%d" : _ format4) [@p]
let _ = ("%d" : _ format) [@p]

let () = Format.(pp_print_newline std_formatter ())

let _ = [] [@p]
let _ = [1;2;3] [@p]
let _ = [[];[1];[2;3]] [@p]

let _ = [||] [@p]
let _ = [|1;2;3|] [@p]
let _ = [|[||];[|1|];[|2;3|]|] [@p]

let _ = (1,"abc") [@p]
let _ = (1,3.14,'A',true,"abc","%d",()) [@p]
let _ = (1,3.14,'A',true,"abc","%d",(),()) [@p]
let _ = [(1,'A');(2,'B');(3,'C')] [@p]
let _ = [|(1,'A');(2,'B');(3,'C')|] [@p]

let () = Format.(pp_print_newline std_formatter ())

let _ = None [@p]
let _ = Some 1 [@p]

let _ = ref [] [@p]
let _ = FP_normal [@p]
let _ = Open_rdonly [@p]
exception Exn
let _ = Exn [@p]

let () = Format.(pp_print_newline std_formatter ())

let _ = (fun x -> x+1) [@p]
let _ = (fun ?(x=1) ~second:y -> x * y) [@p]

let f a b = a + b
let _ = f [@p]

let _ = 10l [@p]
let _ = 10L [@p]
let _ = 10n [@p]
let _ = lazy 10 [@p]

let () = Format.(pp_print_newline std_formatter ())

let _ = [MNil;MNil] [@p]
let _ = MNil [@p]
let _ = MCons (10,MNil) [@p]
let _ = length (MCons ('A',MNil)) [@p]
let _ = ((10,"abc") : M2.t) [@p]
let _ = M2.first ((1,"abc") : M2.t) [@p]

let () = Format.(pp_print_newline std_formatter ())

let _ = (10 : m') [@p]
let _ = A 100 [@p]
let _ = (A 100 : _ vv) [@p]
let _ = B ('B',"abc") [@p]
let _ = Tup (1,1.) [@p]
let _ = Lis [('A',"A")] [@p]
let _ = Fun (fun x -> x) [@p]

let () = Format.(pp_print_newline std_formatter ())

let _ = {a=[100];b=3.14,"3.14"} [@p]
let _ = {a2=[100];b2=3.14,"3.14"} [@p]
let _ = {a2=([1],['A']);b2=(['B'],[2])} [@p]
let _ = {Mylist.M.a = 1;Mylist.M.b = 3.14} [@p]
let _ = So (Se (So O)) [@p]

let () = Format.(pp_print_newline std_formatter ())

let _ = `A `N [@p]
let _ = (`A `N : p) [@p]
let _ = (`B `N : p2) [@p]
let _ = (`So (`Se (`So `O)) : podd) [@p]

let () = Format.(pp_print_newline std_formatter ())

let _ = new c [@p]
let cn = new c2
let _ = cn [@p]
let _ = let _ = cn#add 1 in cn [@p]

let _ = {Incl.a = 1;Incl.b = 3.14} [@p]

let () = Format.(pp_print_newline std_formatter ())

let x [@p] = 10
let Some e [@p] = Some "string"

let () = Format.(pp_print_newline std_formatter ())

let _ =
    let x [@p] = 10 in
    let (a,b) [@p] = (1,'A') in
    let [l1;l2;l3] [@p] = [1;2;3] in
    let y::ys [@p] = [1;2;3] in
    let {a=a_elm;b=b_elm} [@p] = {a=[10];b=(2e5,"2e^5")} in
    let Some e [@p] = Some 10 in
    let `Some e2 [@p] = `Some 20 in
    let ((fst,10)|(fst,20)) [@p] = (10,10) in
    Format.(pp_print_string std_formatter "DONE.\n");
    ignore(x,a,b,l1,l2,l3,y,ys,a_elm,b_elm,e,e2,fst)

open Helper
open Typedtree
open Asttypes

let string_of_constant = function
    | Const_int i -> string_of_int i
    | Const_char c -> Char.escaped c
    | Const_string (s,_) -> s
    | Const_float f -> f
    | Const_int32 i -> (Int32.to_string i) ^ "l"
    | Const_int64 i -> (Int64.to_string i) ^ "L"
    | Const_nativeint i -> (Nativeint.to_string i) ^ "n"

let rec string_of_pattern pat =
    match pat.pat_desc with
    | Tpat_any -> 
            "_"
    | Tpat_var (_,{txt=s;_}) ->
            s
    | Tpat_alias (p,_,{txt=s;_}) ->
            (string_of_pattern p) ^ " as " ^ s
    | Tpat_constant const ->
            string_of_constant const
    | Tpat_tuple pl ->
            "(" ^ (s_of_p_list "," pl) ^ ")"
    | Tpat_construct ({txt=Longident.Lident "::";_},_,pl) ->
            mk_list pl
    | Tpat_construct ({txt=ls;_},_,pl) ->
            (l_to_s ls) ^ " (" ^ (s_of_p_list "," pl) ^ ")"
    | Tpat_variant (l,None,_) ->
            "`" ^ l
    | Tpat_variant (l,Some p,_) ->
            "`" ^ l ^ " " ^ (string_of_pattern p)
    | Tpat_record (record_list,Closed) ->
            s_of_record_list record_list true
    | Tpat_record (record_list,Open) ->
            s_of_record_list record_list false
    | Tpat_array pl ->
            "[| " ^ (s_of_p_list ";" pl) ^ " |]"
    | Tpat_or (p1,p2,_) ->
            (string_of_pattern p1) ^ " | " ^ (string_of_pattern p2)
    | Tpat_lazy p ->
            "lazy " ^ string_of_pattern p

and s_of_p_list split pl  =
    let rec sub acc = function
        | [] -> ""
        | x::[] -> acc ^ string_of_pattern x
        | x::xs -> sub (acc ^ string_of_pattern x ^ split ^ " ") xs
    in
    sub "" pl

and mk_list pl = 
    let rec sub acc = function
        | [] -> acc
        | x::[] -> acc ^ (string_of_pattern x)
        | x::xs -> sub (acc ^ (string_of_pattern x) ^ "; ") xs
    in
    "[ " ^ (sub "" pl) ^ " |]"

and s_of_record_list rl b =
    let last = if b then " " else "; _" in
    let rec sub acc = function
        | [] -> if b then "" else "_"
        | ({txt=ls;_},_,p)::[] -> acc ^ (l_to_s ls) ^ "=" ^ (string_of_pattern p) ^ last
        | ({txt=ls;_},_,p)::xs -> sub (acc ^ (l_to_s ls) ^ "=" ^ (string_of_pattern p) ^ "; ") xs
    in
    "{ " ^ (sub "" rl) ^ "}"

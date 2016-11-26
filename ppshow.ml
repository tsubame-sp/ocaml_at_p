open Format

let std_formatter = std_formatter

let _pp_newline = pp_print_newline

let _pp_int = pp_print_int
let _pp_float = pp_print_float
let _pp_char = pp_print_char
let _pp_bool = pp_print_bool 
let _pp_string = pp_print_string
let _pp_format6 prf fmt = _pp_string prf (string_of_format fmt^" : < format6 >")
let _pp_unit prf () = _pp_string prf "()"
let _pp_bytes prf x = _pp_string prf (Bytes.to_string x)

(* Pervasives *)
let _pp_in_channel prf (_:in_channel) = _pp_string prf "< in_channel >"
let _pp_out_channel prf (_:out_channel) = _pp_string prf "< out_channel >"

let _pp_format4 prf (fmt:(_,_,_,_) format4) = _pp_string prf (string_of_format fmt^" : < format4 >")
let _pp_format prf (fmt:(_,_,_) format) = _pp_string prf (string_of_format fmt^" : < format >")

(* list *)
let _pp_list_empty prf _ = _pp_string prf "[]"

let _pp_list_with sep _pp_a prf = function
    | [] -> ()
    | x::xs ->
            _pp_a prf x;
            List.iter (fprintf prf "%s@;%a" sep _pp_a) xs

let _pp_list _pp_a prf xs = fprintf prf "@[<1>[%a]@]" (_pp_list_with ";" _pp_a) xs

(* array *)
let _pp_array_empty prf _ = _pp_string prf "[||]"

let _pp_array _pp_a prf xs = 
    fprintf prf "@[<2>[|";
    let len = Array.length xs in
    if len > 0 then begin
        _pp_a prf xs.(0);
        for i=1 to len-1 do fprintf prf ";@;%a" _pp_a xs.(i) done end;
        fprintf prf "|]@]"

(* string_of *)
let strbuf = Buffer.create 1024

let s_of _pp_a a = 
    let prf = formatter_of_buffer strbuf in
    _pp_a prf a; pp_print_flush prf ();
    let str = Buffer.contents strbuf in 
    Buffer.clear strbuf;
    str

(* pp_poly *)
type pp_poly = { pp_poly : 'b. 'b pp_neg -> 'b }
and 'b pp_neg = { pp_neg : 'a. (formatter -> 'a -> unit) -> 'a -> 'b }
let pp_poly _pp_a x = { pp_poly = fun k -> k.pp_neg _pp_a x }
let apply_pp_poly prf p = p.pp_poly { pp_neg = fun _pp_a -> _pp_a prf }

(* prefix operator for pp_poly *)
let (!%) _pp_a = pp_poly _pp_a

let rec string_forall p str i j =
    j < i || p str.[i] && string_forall p str (i+1) j

(* PP for Heterogeneous list *)
let pp_poly_list prf = function
    | [] -> ()
    | [p] ->
            let s = s_of apply_pp_poly p in
            if s = "" then ()
            else
                let is_atom = match s.[0] with
                | '(' | '[' | '{' | '<' | '"' | '\'' -> true
                | _ -> string_forall ( function
                    | '0'..'9' | 'A'..'Z' | '_' | 'a'..'z' -> true
                    | _ -> false ) s 1 (String.length s - 1) in
                if is_atom then fprintf prf " %a" apply_pp_poly p
                else fprintf prf "(%a)" apply_pp_poly p
    | p::ps ->
            fprintf prf "@[<1>(%a" apply_pp_poly p;
            List.iter (fprintf prf ",@;%a" apply_pp_poly) ps;
            fprintf prf ")@]"

(* tuple *)
let _pp_tuple (make_pps : 'a -> pp_poly list) prf x = pp_poly_list prf (make_pps x)

let _pp__tuple2 _pp_a _pp_b prf x =
    _pp_tuple (fun (a,b) -> [ !%_pp_a a; !%_pp_b b ]) prf x

let _pp__tuple3 _pp_a _pp_b _pp_c prf x =
    _pp_tuple (fun (a,b,c) -> [ !%_pp_a a; !%_pp_b b; !%_pp_c c ]) prf x

let _pp__tuple4 _pp_a _pp_b _pp_c _pp_d prf x =
    _pp_tuple (fun (a,b,c,d) -> [ !%_pp_a a; !%_pp_b b; !%_pp_c c; !%_pp_d d ]) prf x

let _pp__tuple5 _pp_a _pp_b _pp_c _pp_d _pp_e prf x =
    _pp_tuple (fun (a,b,c,d,e) -> [ !%_pp_a a; !%_pp_b b; !%_pp_c c; !%_pp_d d; !%_pp_e e ]) prf x

let _pp__tuple6 _pp_a _pp_b _pp_c _pp_d _pp_e _pp_f prf x =
    _pp_tuple (fun (a,b,c,d,e,f) -> [ !%_pp_a a; !%_pp_b b; !%_pp_c c; !%_pp_d d; !%_pp_e e; !%_pp_f f ]) prf x

let _pp__tuple7 _pp_a _pp_b _pp_c _pp_d _pp_e _pp_f _pp_g prf x =
    _pp_tuple (fun (a,b,c,d,e,f,g) -> [ !%_pp_a a; !%_pp_b b; !%_pp_c c; !%_pp_d d; !%_pp_e e; !%_pp_f f; !%_pp_g g ]) prf x


 (* variant *)
let _pp__variant (make_cps : 'a -> string * pp_poly list) prf x =
    let constr_name, ps = make_cps x in
    fprintf prf "%s%a" constr_name pp_poly_list ps

(*
type pp_poly_variant_cps = PPConstr of string * pp_poly list | PPKnown of pp_poly

let pp_poly_variant (make_cps : 'a -> pp_poly_variant_cps) prf x =
    match make_cps x with
      | PPConstr (c,ps) -> _pp_variant (fun () -> (c,ps)) prf ()
      | PPKnown p       -> apply_pp_poly prf p
*)

(* PP for option *)
let _pp_option _pp_a prf a =
    _pp__variant (function
        | None   -> "None", []
        | Some x -> "Some", [!%_pp_a x]
    ) prf a

(* PP for records and objects *)
let _pp_recobj encL encC encR (make__pp_fields : 'a -> (string * pp_poly) list) prf x =
    let apply__pp_field prf (f,p) = fprintf prf "@[<2>%s %s @,%a@]" f encC apply_pp_poly p in 
    fprintf prf "@[<1>%s" encL;
    begin match make__pp_fields x with
      | [] -> ()
      | fp::fps ->
              apply__pp_field prf fp;
              List.iter (fprintf prf ";@;%a" apply__pp_field) fps end;
    fprintf prf "%s@]" encR

let _pp__record mpf prf x = _pp_recobj "{" "=" "}" mpf prf x
let _pp__object mpf prf x = _pp_recobj "<" ":" ">" mpf prf x

(* Pervasive *)
let _pp_fpclass prf x = _pp__variant (function
    | FP_normal -> "FP_normal", []
    | FP_subnormal -> "FP_subnormal", []
    | FP_zero -> "FP_zero", []
    | FP_infinite -> "FP_infinite", []
    | FP_nan -> "FP_nan", []) prf x

let _pp_open_flag prf x = _pp__variant (function
    | Open_rdonly -> "Open_rdonly", []
    | Open_wronly -> "Open_wronly", []
    | Open_append -> "Open_append", []
    | Open_creat -> "Open_creat", []
    | Open_trunc -> "Open_trunc", []
    | Open_excl -> "Open_excl", []
    | Open_binary -> "Open_binary", []
    | Open_text -> "Open_text", []
    | Open_nonblock -> "Open_nonblock", []) prf x

let _pp_ref _pp_a prf x = _pp__record (fun r -> ["contents", !%_pp_a r.contents]) prf x

let _pp_exn prf exn = _pp_string prf (Printexc.to_string exn)

let _pp__function str prf _ = _pp_string prf ("< fun : "^str^" >")

let _pp_int32 prf x = _pp_string prf (Int32.to_string x ^ "l")
let _pp_int64 prf x = _pp_string prf (Int64.to_string x ^ "L")
let _pp_nativeint prf x = _pp_string prf (Nativeint.to_string x ^ "n")
let _pp_lazy_t _pp_a prf x = _pp_string prf "lazy ";_pp_a prf (Lazy.force x)

let _pp__dump str prf _ = _pp_string prf str
let _pp__unsup prf _ = _pp_string prf "< Unsupported Type >"
let _pp__nouse prf _ = _pp_string prf "< This is dummy pp >"

let _pp__trash prf x = ()

module String = struct
    include String
    let _pp_t prf x = _pp_string prf x
end

module SSet = Set.Make(struct
    type t = string
    let compare = compare
end)

open Asttypes
open Format
open Types
open Typedtree
open Longident
open Ident

(* ppopen check *)
let ppopen = ref (SSet.empty)

(*
 * Types
 *)
(* empty type_expr creator *)
let type_none = {desc=Tnil;level=0;id=0}

(*
 * Path
 *)
(* Path.Pident creator from string *)
let path_ident_create s = Path.Pident (Ident.create s)

(* type path to pp path *)
let rec path_set = function
    | Path.Pident {name = s;_} -> Path.Pident (Ident.create ("_pp_"^s))
    | Path.Pdot (t,s,i) -> Path.Pdot (t,"_pp_"^s,i)
    | Path.Papply (t1,t2) -> Path.Papply (t1,path_set t2)

(*
 * Longident
 *)
(* path to Longident *)
let rec path_to_longident = function
    | Path.Pident {name=s;_} -> Lident s
    | Path.Pdot (t,s,_) -> Ldot (path_to_longident t,s)
    | Path.Papply (t1,t2) -> Lapply (path_to_longident t1,path_to_longident t2)

(* Longident to path *)
let rec longident_to_path = function
    | Lident s -> path_ident_create s
    | Ldot (t,s) -> Path.Pdot (longident_to_path t,s,0)
    | Lapply (t1,t2) -> Path.Papply (longident_to_path t1,longident_to_path t2)

(* Longident first *)
let rec first = function
    | Lident s -> s
    | Ldot (t,s) -> first t
    | Lapply (t1,t2) -> first t1

(* Longident last *)
let last = last

let rec l_to_s = function
    | Lident s -> s
    | Ldot (t,s) -> (l_to_s t) ^ "." ^ s
    | Lapply (t1,t2) -> (l_to_s t1) ^ " " ^ (l_to_s t2)

(*
 * constructor_description
 *)
let cstr_desc =
    { cstr_name = "";
      cstr_res = type_none;
      cstr_existentials = [];
      cstr_args = [];
      cstr_arity = 0;
      cstr_tag = Cstr_constant 0;
      cstr_consts = 0;
      cstr_nonconsts = 0;
      cstr_normal = 0;
      cstr_generalized = false;
      cstr_private = Public;
      cstr_loc = Location.none;
      cstr_attributes = [];
      cstr_inlined = None;
    }

(*
 * Structure_item
 *)
let make_Tstr_val vb_list =
    { str_desc = 
        Tstr_value (Recursive,vb_list);
      str_loc = Location.none;
      str_env = Env.empty}

(*
 * Signature_item
 *)
let make_Tsig_val name ctyp =
    { sig_desc = 
        Tsig_value { val_id = Ident.create name;
                     val_name = {txt=name;loc=Location.none};
                     val_desc = ctyp;
                     val_val = { val_type = type_none;
                                 val_kind = Val_reg;
                                 val_loc = Location.none;
                                 val_attributes = [] };
                     val_prim = [];
                     val_loc = Location.none;
                     val_attributes = [] };
      sig_loc = Location.none;
      sig_env = Env.empty}

(*
 * Pattern
 *)
(* Tpat_any creator *)
let make_Tpat_any =
    { pat_desc = Tpat_any;
      pat_loc = Location.none;
      pat_extra = [];
      pat_type = type_none;
      pat_env = Env.empty;
      pat_attributes = []
    }

(* Tpat_var creator *)
let make_Tpat_var name =
    { pat_desc = Tpat_var ({stamp=0;name=name;flags=0},{txt=name;loc=Location.none});
      pat_loc = Location.none;
      pat_extra = [];
      pat_type = type_none;
      pat_env = Env.empty;
      pat_attributes = []
    }

(* Tpat_construct creator *)
let make_Tpat_construct longident pat_list =
    { pat_desc = Tpat_construct ({txt=longident;loc=Location.none},cstr_desc,pat_list);
      pat_loc = Location.none;
      pat_extra = [];
      pat_type = type_none;
      pat_env = Env.empty;
      pat_attributes = []
    }

(* Tpat_variant creator *)
let make_Tpat_variant label pat_opt rd_ref =
    { pat_desc = Tpat_variant (label,pat_opt,rd_ref);
      pat_loc = Location.none;
      pat_extra = [];
      pat_type = type_none;
      pat_env = Env.empty;
      pat_attributes = []
    }

(*
 * Expression
 *)
(* Texp_construct creator *)
let make_Texp_construct longident expr_list =
    { exp_desc = Texp_construct ({txt=longident;loc=Location.none},cstr_desc,expr_list);
      exp_loc = Location.none;
      exp_extra = [];
      exp_type = type_none;
      exp_env = Env.empty;
      exp_attributes = []
    }

(* Texp_constant creator *)
let make_Texp_constant const =
    { exp_desc = Texp_constant const;
      exp_loc = Location.none;
      exp_extra = [];
      exp_type = type_none;
      exp_env = Env.empty;
      exp_attributes = []
    }

(* Texp_tuple creator *)
let make_Texp_tuple elist =
    { exp_desc = Texp_tuple elist;
      exp_loc = Location.none;
      exp_extra = [];
      exp_type = type_none;
      exp_env = Env.empty;
      exp_attributes = []
    }

(* Texp_ident creator *)
let make_Texp_ident ?(typ=type_none) path =
    { exp_desc = Texp_ident (path,
                             {txt = path_to_longident path;loc = Location.none},
                             {val_type = typ;val_kind=Val_reg;val_loc=Location.none;val_attributes=[]});
      exp_loc = Location.none;
      exp_extra = [];
      exp_type = type_none;
      exp_env = Env.empty;
      exp_attributes = []
    }

(* Texp_field creator *)
let make_Texp_field exp longident =
    let ld =
        { lbl_name = "";
          lbl_res = type_none;
          lbl_arg = type_none;
          lbl_mut = Mutable;
          lbl_pos = 0;
          lbl_all = [||];
          lbl_repres = Record_regular;
          lbl_private = Private;
          lbl_loc = Location.none;
          lbl_attributes = []}
    in
    { exp_desc = 
        Texp_field (exp,{txt=longident;loc=Location.none},ld);
      exp_loc = Location.none;
      exp_extra = [];
      exp_type = type_none;
      exp_env = Env.empty;
      exp_attributes = []
    }

(* Texp_apply creator *)
let make_Texp_apply expr1 arg_list =
    { exp_desc = 
        Texp_apply (expr1,arg_list);
      exp_loc = Location.none;
      exp_extra = [];
      exp_type = type_none;
      exp_env = Env.empty;
      exp_attributes = []
    }

(* Texp_function creator *)
let make_Texp_function case_list =
    { exp_desc = 
        Texp_function (Nolabel,case_list,Total);
      exp_loc = Location.none;
      exp_extra = [];
      exp_type = type_none;
      exp_env = Env.empty;
      exp_attributes = []
    }

(* Texp_sequence creator *)
let make_Texp_sequence e1 e2 =
    { exp_desc = 
        Texp_sequence (e1,e2);
      exp_loc = Location.none;
      exp_extra = [];
      exp_type = type_none;
      exp_env = Env.empty;
      exp_attributes = []
    }

(*
 * core_type
 * *)
let make_Cty_var s =
    { ctyp_desc = Ttyp_var s;
      ctyp_type = type_none;
      ctyp_env = Env.empty;
      ctyp_loc = Location.none;
      ctyp_attributes = [] }

let make_Cty_constr longident ctyl =
    { ctyp_desc = Ttyp_constr (longident_to_path longident,{txt=longident;loc=Location.none},ctyl);
      ctyp_type = type_none;
      ctyp_env = Env.empty;
      ctyp_loc = Location.none;
      ctyp_attributes = [] }

let make_Cty_arrow label x y =
    { ctyp_desc = Ttyp_arrow (label,x,y);
      ctyp_type = type_none;
      ctyp_env = Env.empty;
      ctyp_loc = Location.none;
      ctyp_attributes = [] }

let set_ps name params =
    let ps = List.map fst params in
    let cty_fmt = make_Cty_constr (Ldot (Lident "Format","formatter")) [] in
    let cty_x = make_Cty_constr (Lident name) ps in
    let cty_u = make_Cty_constr (Lident "unit") [] in
    let nl = Asttypes.Nolabel in
    let cty_arr2 a b c = make_Cty_arrow nl a (make_Cty_arrow nl b c) in
    let rec ret x = 
        match x.ctyp_desc with
        | Ttyp_arrow _ ->
                make_Cty_arrow
                    nl
                    cty_fmt 
                    (make_Cty_constr (Lident "string") [])
        | Ttyp_tuple xs ->
                let rec from_list = function
                    | [] -> cty_arr2 cty_fmt x cty_u
                    | x::xs ->
                            make_Cty_arrow nl (ret x) (from_list xs)
                in
                from_list xs
        | Ttyp_constr (_,{txt=li;_},tyl) ->
                let rec from_list = function
                    | [] -> cty_arr2 cty_fmt x cty_u
                    | x::xs ->
                            make_Cty_arrow nl (ret x) (from_list xs)
                in
                from_list tyl
        | Ttyp_class (_,{txt=li;_},tyl) ->
                let rec from_list = function
                    | [] -> cty_arr2 cty_fmt x cty_u
                    | x::xs ->
                            make_Cty_arrow nl (ret x) (from_list xs)
                in
                from_list tyl
        | _ -> cty_arr2 cty_fmt x cty_u
    in
    let rec from_list = function
        | [] -> cty_arr2 cty_fmt cty_x cty_u
        | x::xs ->
                make_Cty_arrow nl (ret x) (from_list xs)
    in
    from_list ps

(* expr of std_formatter *)
let expr_std_formatter = 
    make_Texp_ident (Path.Pdot (Path.Pident (Ident.create "Format"),"std_formatter",0))

(* select pp from type_expr *)
let rec select_pp typ =
    let typelist_to_arglist typelist = 
        let rec from_list acc = function
            | [] -> List.rev acc
            | x::xs ->
                    from_list ((Nolabel,Some (select_pp x)) :: acc) xs
        in
        from_list [] typelist
    in
    let from_row_desc ({ row_fields = rf_list;_ } as rd) =
        let rec make_caselist_from_rflist acc = function
            | [] -> acc
            | (cons,Rpresent None)::xs ->
                    make_caselist_from_rflist
                       ({ c_lhs = make_Tpat_variant cons None (ref rd);
                          c_guard = None;
                          c_rhs = make_Texp_tuple
                                     [make_Texp_constant (Const_string ("`"^cons,None));
                                      make_cps_expr 1 []]} :: acc)
                       xs
            | (cons,Rpresent (Some ty_expr))::xs ->
                    make_caselist_from_rflist
                       ({ c_lhs = make_Tpat_variant cons (Some (List.hd (pat_list 1))) (ref rd);
                          c_guard = None;
                          c_rhs = make_Texp_tuple
                                     [make_Texp_constant (Const_string ("`"^cons,None));
                                      make_cps_expr 1 [{ctyp_desc = Ttyp_any;
                                                        ctyp_type = ty_expr;
                                                        ctyp_env = Env.empty;
                                                        ctyp_loc = Location.none;
                                                        ctyp_attributes = []}]]} :: acc)
                       xs
            | (cons,Reither (b1,tyl,b2,_))::xs ->
                    let rec make_tyl acc = function
                        | [] -> List.rev acc
                        | x::xs -> 
                                make_tyl
                                  ({ctyp_desc = Ttyp_any;
                                    ctyp_type = x;
                                    ctyp_env  = Env.empty;
                                    ctyp_loc = Location.none;
                                    ctyp_attributes = []} :: acc)
                                  xs
                    in
                    make_caselist_from_rflist
                       ({ c_lhs = make_Tpat_variant cons (Some (List.hd (pat_list 1))) (ref rd);
                          c_guard = None;
                          c_rhs = make_Texp_tuple
                                     [make_Texp_constant (Const_string ("`"^cons,None));
                                      make_cps_expr 1 (make_tyl [] tyl)]} :: acc)
                       xs
            | (cons,Rabsent)::xs ->
                    failwith "TODO: Types.Rabsent"
        in
        let case_list = make_caselist_from_rflist [] rf_list in
        make_Texp_apply (make_Texp_ident (path_ident_create "_pp__variant"))
                        [Nolabel,Some (make_Texp_function case_list)]
    in
    let rec from_tfields ty =
        match ty.desc with
        | Tnil -> make_Texp_construct (Lident "[]") []
        | Tfield (name,_,mty,rest) ->
                let ty_s = Format.asprintf "%a" Printtyp.type_expr mty in
                make_Texp_construct
                    (Lident "::")
                    [make_Texp_tuple
                         [make_Texp_constant (Const_string (name,None));
                          make_Texp_apply
                              (make_Texp_apply
                                   (make_Texp_ident (path_ident_create "!%"))
                                   [Nolabel,Some (make_Texp_apply
                                                      (make_Texp_ident (path_ident_create "_pp__dump"))
                                                      [Nolabel,Some (make_Texp_constant (Const_string (ty_s,None)))])])
                              [Nolabel,Some (make_Texp_construct (Lident "()") [])]];
                     from_tfields rest]
        | _ -> failwith "TODO: from_tfields"
    in
    match typ.desc with
    | Tvar None -> 
            make_Texp_ident (path_ident_create "_pp__unsup") ~typ:typ
    | Tvar (Some s) ->
            make_Texp_ident (path_ident_create ("_arg_"^s)) ~typ:typ
    | Tarrow _ ->
            let type_str = Format.asprintf "%a" Printtyp.type_expr typ in
            make_Texp_apply (make_Texp_ident (path_ident_create ("_pp__function")))
                            [Nolabel,Some (make_Texp_constant (Const_string (type_str,None)))]
    | Ttuple typelist ->
            let len = List.length typelist in
            if len < 2 || len > 7
            then make_Texp_apply (make_Texp_ident (path_ident_create ("_pp__dump")))
                                 [Nolabel,Some (make_Texp_constant (Const_string ("< "^string_of_int len^" elements tuple >",None)))]
            else make_Texp_apply (make_Texp_ident (path_ident_create ("_pp__tuple"^string_of_int len)))
                                 (typelist_to_arglist typelist)
    | Tconstr (Path.Pident {name=s;_},[],_) ->
            make_Texp_ident (path_ident_create ("_pp_"^s)) ~typ:typ
    | Tconstr (Path.Pident {name=s;_},typelist,_) ->
            make_Texp_apply (make_Texp_ident (path_ident_create ("_pp_"^s)) ~typ:typ) (typelist_to_arglist typelist)
    | Tconstr (path,[],_) ->
            begin match Path.name path with
            | "Pervasives.in_channel"
            | "Pervasives.out_channel"
            | "Pervasives.fpclass" 
            | "Pervasives.open_flag" -> 
                    make_Texp_ident (path_ident_create ("_pp_"^Path.last path)) ~typ:typ
            | _ ->  let m = first (path_to_longident path) in
                    if SSet.mem m !ppopen
                    then make_Texp_ident (path_set path) ~typ:typ
                    else make_Texp_apply (make_Texp_ident (path_ident_create "_pp__dump") ~typ:typ)
                                         [Nolabel,Some (make_Texp_constant 
                                                            (Const_string ("< Unsupported Type > : You may not compile "^m^".ml by ocaml@p or may not write [@@@ppopen]",None)))]
            end
    | Tconstr (path,typelist,_) ->
            begin match Path.name path with
            | "Pervasives.format6"
            | "Pervasives.format4"
            | "Pervasives.format" -> 
                    make_Texp_ident (path_ident_create ("_pp_"^Path.last path)) ~typ:typ
            | "Pervasives.ref" -> 
                    make_Texp_apply (make_Texp_ident (path_ident_create ("_pp_"^Path.last path)) ~typ:typ) (typelist_to_arglist typelist)
            | _ ->  let m = first (path_to_longident path) in
                    if SSet.mem m !ppopen 
                    then make_Texp_apply (make_Texp_ident (path_set path) ~typ:typ) (typelist_to_arglist typelist)
                    else make_Texp_apply (make_Texp_ident (path_ident_create "_pp__dump") ~typ:typ)
                                         [Nolabel,Some (make_Texp_constant 
                                                            (Const_string ("< Unsupported Type > : You may not compiled "^m^".ml by ocaml@p or may not write [@@@ppopen]",None)))]
                    
            end
    | Tobject (ty,ref) ->
            begin match !ref with
            | Some _ -> failwith "TODO: Tobject (_,Some ...)"
            | None -> 
                    let expression = from_tfields ty in
                    let case_list =
                        [{ c_lhs = make_Tpat_any;
                           c_guard = None;
                           c_rhs = expression }]
                    in
                    make_Texp_apply (make_Texp_ident (path_ident_create "_pp__object"))
                                    [Nolabel,Some (make_Texp_function case_list)]
            end
    | Tfield _ -> failwith "TODO: Tfield (?)"
    | Tnil ->
            make_Texp_ident (path_ident_create "_pp__nouse") ~typ:typ
    | Tlink ty -> select_pp ty
    | Tsubst _ -> failwith "TODO: Tsubst (?)"
    | Tvariant row_desc ->
            from_row_desc row_desc
    | Tunivar _ -> failwith "TODO: Tunivar (?)"
    | Tpoly (ty,_) ->
            select_pp ty
            (* TODO : これでいいのかわからない *)
    | Tpackage _ -> failwith "TODO: Tpackage (?)"

(* select pp from core_type *)
and select_pp_core {ctyp_type = type_expr;_} = select_pp type_expr

(*
 *  * prepare for variant pp
 *   *)
(* create constructor argument *)
and pat_list n =
    let rec loop acc = function
        | 0 -> acc
        | n -> loop (make_Tpat_var ("_p"^string_of_int n)::acc) (n-1)
    in
    loop [] n

(* create cps *)
and make_cps_expr n = function
    | [] -> make_Texp_construct (Lident "[]") []
    | x::xs ->
            make_Texp_construct
               (Lident "::")
               [make_Texp_apply
                   (make_Texp_apply
                       (make_Texp_ident (path_ident_create "!%"))
                       [Nolabel,Some (select_pp_core x)])
                   [Nolabel,Some (make_Texp_ident (path_ident_create ("_p"^string_of_int n)))];
                make_cps_expr (n+1) xs]

(* create value_binding for pp *)
let make_vb ty_name exp =
    {vb_pat = 
        {pat_desc = Tpat_var ({stamp=0;name="_pp_"^ty_name;flags=0},
                              {txt="_pp_"^ty_name;loc=Location.none});
         pat_loc = Location.none;
         pat_extra = [];
         pat_type = type_none;
         pat_env = Env.empty;
         pat_attributes = []};
     vb_expr = exp;
     vb_attributes = [];
     vb_loc = Location.none}

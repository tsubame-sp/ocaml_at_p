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
let ppopen = ref SSet.empty

(* case_list table for type_extension & poly variant *)
let caselist_tbl = ( Hashtbl.create 20 : (string,Typedtree.case list) Hashtbl.t )

(* polymorphic variant's params tbl *)
let params_tbl = ( Hashtbl.create 20 : (string,(core_type * variance) list) Hashtbl.t )

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
    { pat_desc = Tpat_var (Ident.create name,{txt=name;loc=Location.none});
      pat_loc = Location.none;
      pat_extra = [];
      pat_type = type_none;
      pat_env = Env.empty;
      pat_attributes = []
    }

(* Tpat_tuple creator *)
let make_Tpat_tuple pat_list =
    { pat_desc = Tpat_tuple pat_list;
      pat_loc = Location.none;
      pat_extra = [];
      pat_type = type_none;
      pat_env = Env.empty;
      pat_attributes = []
    }

(* Tpat_alias creator *)
let make_Tpat_alias pat name =
    { pat_desc = Tpat_alias (pat,Ident.create name,{txt=name;loc=Location.none});
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
let rd_none =
    { row_fields = [];
      row_more = type_none;
      row_bound = ();
      row_closed = true;
      row_fixed = false;
      row_name = None }

let make_Tpat_variant label ?(rd=rd_none) pat_opt =
    { pat_desc = Tpat_variant (label,pat_opt,ref rd);
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
                             {txt = path_to_longident path;
                              loc = Location.none},
                             {val_type = typ;
                              val_kind=Val_reg;
                              val_loc=Location.none;
                              val_attributes=[]});
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

(* Texp_function single creator *)
let make_Texp_fun name e =
    make_Texp_function
        [{ c_lhs = {pat_desc = 
                       Tpat_var ((Ident.create name),
                                 {txt=name;loc=Location.none});
                    pat_loc = Location.none;
                    pat_extra = [];
                    pat_type = type_none;
                    pat_env = Env.empty;
                    pat_attributes = []};
           c_guard = None;
           c_rhs = e}]

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

(* Texp_let creator *)
let make_Texp_let flag vbl e =
    { exp_desc = 
        Texp_let (flag,vbl,e);
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

(* For alias (as) loop *)
let pre = ref SSet.empty

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
    (*
    let rec check_rf = function
        | [] -> ()
        | (cons,_)::xs ->
                pre := SSet.add cons !pre;
                check_rf xs
    in
            *)
    let from_row_desc ({ row_fields = rf_list;row_more = more;_ } as rd) =
        (*
        let rec check_more ty =
            match ty.desc with
            | Tlink ty2 ->
                    print_endline "check Tlink";
                    let _ = check_more ty2 in
                    false
            | Tvar None -> 
                    print_endline "check Tvar";
                    false
            | Tvar (Some s) -> 
                    print_endline ("check Tvar "^s);
                    true
            | Tarrow _ ->
                    print_endline "check Tarrow";
                    true
            | Ttuple _ ->
                    print_endline "check Ttuple";
                    true
            | Tconstr _ ->
                    print_endline "check Tconstr";
                    true
            | Tobject _ ->
                    print_endline "check Tobject";
                    true
            | Tfield _ ->
                    print_endline "check Tfield";
                    true
            | Tnil ->
                    print_endline "check Tnil";
                    true
            | Tsubst _ ->
                    print_endline "check Tsubst";
                    true
            | Tvariant _ ->
                    print_endline "check Tvariant";
                    true
            | Tunivar None ->
                    print_endline "check Tunivar None";
                    true
            | Tunivar (Some s) ->
                    print_endline ("check Tunivar "^s);
                    true
            | Tpoly _ ->
                    print_endline "check Tpoly";
                    true
            | _ ->  
                    print_endline "check false";
                    true
        in
        *)
        let rec make_caselist_from_rflist acc = function
            | [] -> List.rev acc
            | (cons,Rpresent None)::xs ->
                    make_caselist_from_rflist
                        ({ c_lhs = make_Tpat_variant cons None ~rd:rd;
                           c_guard = None;
                           c_rhs = make_Texp_tuple
                                       [make_Texp_constant (Const_string ("`"^cons,None));
                                       make_cps_expr 1 []] } :: acc)
                        xs
            | (cons,Rpresent (Some ty_expr))::xs ->
                        
                    make_caselist_from_rflist
                        ({ c_lhs = make_Tpat_variant cons (Some (List.hd (pat_list 1))) ~rd:rd;
                           c_guard = None;
                           c_rhs = make_Texp_tuple
                                       [make_Texp_constant (Const_string ("`"^cons,None));
                                        make_cps_expr 1 [{ctyp_desc = Ttyp_any;
                                                          ctyp_type = ty_expr;
                                                          ctyp_env = Env.empty;
                                                          ctyp_loc = Location.none;
                                                          ctyp_attributes = []}]] } :: acc)
                        xs
            | (cons,Reither (_,tyl,_,_))::xs ->
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
                       ({ c_lhs = make_Tpat_variant cons (Some (List.hd (pat_list 1))) ~rd:rd;
                          c_guard = None;
                          c_rhs = make_Texp_tuple
                                     [make_Texp_constant (Const_string ("`"^cons,None));
                                      make_cps_expr 1 (make_tyl [] tyl)]} :: acc)
                       xs
            | (cons,Rabsent)::xs ->
                    failwith "TODO: Types.Rabsent"
        in
        (*
        let b = check_more more in
        if b && SSet.mem (fst (List.hd rf_list)) !pre
        then
             make_Texp_ident (path_ident_create "_pp__rec")
        else
            (let () = check_rf rf_list in
        *)
             let case_list = make_caselist_from_rflist [] rf_list in
        (*
             let vblist = 
                 [{ vb_pat = make_Tpat_var "_pp__rec";
                    vb_expr = 
                        app_prfx
        *)
                           (make_Texp_apply 
                                (make_Texp_ident (path_ident_create "_pp__variant"))
                                [Nolabel,Some (make_Texp_function case_list)])(*;
                    vb_attributes = [];
                    vb_loc = Location.none }]
             in
             (pre := SSet.empty;
              make_Texp_let
                  Recursive
                  vblist
                  (make_Texp_ident (path_ident_create "_pp__rec"))))
        *)
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
                                                            (Const_string ("< "^m^"'s type without OCaml@p >",None)))]
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
                                                            (Const_string ("< "^m^"'s type without OCaml@p >",None)))]
                    
            end
    | Tobject (ty,ref) ->
            begin match !ref with
            | Some _ (* TODO *)
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
    | Tfield _ -> failwith "not use"
    | Tnil ->
            make_Texp_ident (path_ident_create "_pp__nouse") ~typ:typ
    | Tlink ty -> select_pp ty
    | Tsubst _ -> failwith "TODO: Tsubst"
    | Tvariant row_desc ->
            from_row_desc row_desc
    | Tunivar _ -> failwith "TODO: Tunivar"
    | Tpoly (ty,_) ->
            (* TODO *)
            select_pp ty
    | Tpackage _ -> failwith "TODO: Tpackage"

(* select pp from core_type *)
(*
and select_pp_core {ctyp_type = type_expr;_} = select_pp type_expr
*)

and select_pp_core ?(ty_name="") cty =
    let typelist_to_arglist typelist = 
        let rec from_list acc = function
            | [] -> List.rev acc
            | x::xs ->
                    from_list ((Nolabel,Some (select_pp_core x)) :: acc) xs
        in
        from_list [] typelist
    in
    let rec make_caselist_obj acc_f = function
        | [] -> [{ c_lhs = make_Tpat_any;
                   c_guard = None;
                   c_rhs = acc_f (make_Texp_construct (Lident "[]") [] )}]
        | (name,_,cty)::xs ->
                let type_s = Format.asprintf "%a" Printtyp.type_expr cty.ctyp_type in
                make_caselist_obj
                    (fun inter -> 
                       acc_f
                         (make_Texp_construct
                              (Lident "::")
                              [make_Texp_tuple
                                   [make_Texp_constant (Const_string (name,None));
                                    make_Texp_apply
                                        (make_Texp_apply
                                             (make_Texp_ident (path_ident_create "!%"))
                                             [Nolabel,Some (make_Texp_apply
                                                                (make_Texp_ident (path_ident_create "_pp__dump"))
                                                                [Nolabel,Some (make_Texp_constant (Const_string (type_s,None)))])])
                                        [Nolabel,Some (make_Texp_construct (Lident "()") [])]];
                                   inter]))
                    xs
    in
    let rec arg_select_pp_ctyl acc = function
        | [] -> List.rev acc
        | x::xs ->
                arg_select_pp_ctyl
                   ((Nolabel,Some (select_pp_core x)) :: acc)
                   xs
    in
    let rev_app params ctyl ori b = 
        let rec loop a = function
        | [] -> a
        | ({c_rhs = e;_} as x)::xs -> 
                if (List.length params) = 0
                then
                    loop (x::a) xs
                else
                    loop 
                       ({x with c_rhs = make_Texp_apply
                                            (fun_exp ~prfx:false e params)
                                            (arg_select_pp_ctyl [] ctyl); } :: a) 
                       xs
        in
        loop ori b
    in
    let rec make_caselist_var acc = function
        | [] -> List.rev acc
        | (Ttag (const,_,_,ctyl))::xs ->
                let len = List.length ctyl in
                let arg =
                    match len with
                    | 0 -> None
                    | 1 -> Some (List.hd (pat_list len))
                    | _ -> Some (make_Tpat_tuple (pat_list len))
                in
                make_caselist_var
                    ({ c_lhs = make_Tpat_variant const arg;
                       c_guard = None;
                       c_rhs = make_Texp_tuple
                                   [make_Texp_constant (Const_string ("`"^const,None));
                                    make_cps_expr 1 ctyl]} :: acc)
                    xs
        | (Tinherit {ctyp_desc = Ttyp_constr(Path.Pident {name=s;_},_,ctyl);})::xs ->
                let inh_cl = Hashtbl.find caselist_tbl s in
                let params = Hashtbl.find params_tbl s in
                make_caselist_var (rev_app params ctyl acc inh_cl) xs
        | _ -> failwith "Inheritance from other file isn't unsupported."
    in
    (*
    let check_params_alias ty_name s =
        let rec loop = function
            | [] -> false
            | ({ctyp_desc = Ttyp_var name;_},_)::xs ->
                    if name = s
                    then true
                    else loop xs
            | _::xs -> loop xs
        in
        try
            let ls = Hashtbl.find params_tbl ty_name in
            loop ls
        with _ -> false
    in
    *)
    match cty.ctyp_desc with
    | Ttyp_any -> 
            make_Texp_ident (path_ident_create "_pp__unsup") ~typ:cty.ctyp_type
    | Ttyp_var s ->
            make_Texp_ident (path_ident_create ("_arg_"^s)) ~typ:cty.ctyp_type
    | Ttyp_arrow _ ->
            let type_str = Format.asprintf "%a" Printtyp.type_expr cty.ctyp_type in
            make_Texp_apply (make_Texp_ident (path_ident_create ("_pp__function")))
                            [Nolabel,Some (make_Texp_constant (Const_string (type_str,None)))]
    | Ttyp_tuple ctyl ->
            let len = List.length ctyl in
            if len < 2 || len > 7
            then make_Texp_apply (make_Texp_ident (path_ident_create ("_pp__dump")))
                                 [Nolabel,Some (make_Texp_constant (Const_string ("< "^string_of_int len^" elements tuple >",None)))]
            else make_Texp_apply (make_Texp_ident (path_ident_create ("_pp__tuple"^string_of_int len)))
                                 (typelist_to_arglist ctyl)
    | Ttyp_constr (Path.Pident {name=s;_},_,[]) ->
            make_Texp_ident (path_ident_create ("_pp_"^s)) ~typ:cty.ctyp_type
    | Ttyp_constr (Path.Pident {name=s;_},_,typelist) ->
            make_Texp_apply (make_Texp_ident (path_ident_create ("_pp_"^s)) ~typ:cty.ctyp_type) (typelist_to_arglist typelist)
    | Ttyp_constr (path,_,[]) ->
            begin match Path.name path with
            | "Pervasives.in_channel"
            | "Pervasives.out_channel"
            | "Pervasives.fpclass" 
            | "Pervasives.open_flag" -> 
                    make_Texp_ident (path_ident_create ("_pp_"^Path.last path)) ~typ:cty.ctyp_type
            | _ ->  let m = first (path_to_longident path) in
                    if SSet.mem m !ppopen
                    then make_Texp_ident (path_set path) ~typ:cty.ctyp_type
                    else make_Texp_apply (make_Texp_ident (path_ident_create "_pp__dump") ~typ:cty.ctyp_type)
                                         [Nolabel,Some (make_Texp_constant 
                                                            (Const_string ("< "^m^"'s type without OCaml@p >",None)))]
            end
    | Ttyp_constr (path,_,typelist) ->
            begin match Path.name path with
            | "Pervasives.format6"
            | "Pervasives.format4"
            | "Pervasives.format" -> 
                    make_Texp_ident (path_ident_create ("_pp_"^Path.last path)) ~typ:cty.ctyp_type
            | "Pervasives.ref" -> 
                    make_Texp_apply (make_Texp_ident (path_ident_create ("_pp_"^Path.last path)) ~typ:cty.ctyp_type) (typelist_to_arglist typelist)
            | _ ->  let m = first (path_to_longident path) in
                    if SSet.mem m !ppopen 
                    then make_Texp_apply (make_Texp_ident (path_set path) ~typ:cty.ctyp_type) (typelist_to_arglist typelist)
                    else make_Texp_apply (make_Texp_ident (path_ident_create "_pp__dump") ~typ:cty.ctyp_type)
                                         [Nolabel,Some (make_Texp_constant 
                                                            (Const_string ("< "^m^"'s type without OCaml@p >",None)))]
                    
            end
    | Ttyp_object (str_att_cty_list,_) ->
            let case_list = make_caselist_obj (fun x -> x) str_att_cty_list in
            make_Texp_apply (make_Texp_ident (path_ident_create "_pp__object"))
                            [Nolabel,Some (make_Texp_function case_list)]
    | Ttyp_class (path,_,typelist) ->
            let m = first (path_to_longident path) in
            if SSet.mem m !ppopen 
            then make_Texp_apply (make_Texp_ident (path_set path) ~typ:cty.ctyp_type) (typelist_to_arglist typelist)
            else make_Texp_apply (make_Texp_ident (path_ident_create "_pp__dump") ~typ:cty.ctyp_type)
                                 [Nolabel,Some (make_Texp_constant 
                                                    (Const_string ("< "^m^"'s Type without OCaml@p >",None)))]
    | Ttyp_alias (ctyp,s) ->
            (*
            let c = check_params_alias ty_name s in
            if c
            then
                select_pp_core ctyp
            else
            *)
                (make_Texp_let 
                     Recursive 
                     [{ vb_pat = make_Tpat_var ("_arg_"^s);
                        vb_expr = app_prfx (select_pp_core ctyp);
                        vb_attributes = [];
                        vb_loc = Location.none }]
                     (make_Texp_ident (path_ident_create ("_arg_"^s))))
    | Ttyp_variant (row_field_list,_,_) ->
            let case_list = make_caselist_var [] row_field_list in
            Hashtbl.add caselist_tbl ty_name case_list;
            make_Texp_apply (make_Texp_ident (path_ident_create "_pp__variant"))
                            [Nolabel,Some (make_Texp_function case_list)]
    | Ttyp_poly (sl,ctyl) ->
            (* TODO *)
            select_pp cty.ctyp_type
    | Ttyp_package p ->
            (* TODO *)
            select_pp cty.ctyp_type

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

and app_prfx exp =
    make_Texp_fun "_prf" 
           (make_Texp_fun "_arg"
                   (make_Texp_apply
                        exp
                        [Nolabel,Some (make_Texp_ident (path_ident_create "_prf"));
                         Nolabel,Some (make_Texp_ident (path_ident_create "_arg"))]))

(* core_type Ttyp_var to string *)
and get_name ct =
    match ct.ctyp_desc with
    | Ttyp_var s -> s
    | _ -> failwith "Not reached"

(* fun _pp_1 -> ... -> expr : expression -> (core_type * variance) list -> expression *)
and fun_exp ?(prfx = true) exp ls =
    let rec loop acc = function
        | [] -> acc
        | x::xs -> 
                let name = "_arg_" ^ get_name x in
                loop (make_Texp_fun name acc) xs
    in
    if prfx
    then loop (app_prfx exp) (List.rev (List.map fst ls))
    else loop exp (List.rev (List.map fst ls))

(* create value_binding for pp *)
let make_vb ty_name exp =
    {vb_pat = 
        {pat_desc = Tpat_var (Ident.create ("_pp_"^ty_name),
                              {txt="_pp_"^ty_name;loc=Location.none});
         pat_loc = Location.none;
         pat_extra = [];
         pat_type = type_none;
         pat_env = Env.empty;
         pat_attributes = []};
     vb_expr = exp;
     vb_attributes = [];
     vb_loc = Location.none}

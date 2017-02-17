(* ======================================================================
 * Project Name    : OCaml@p
 * File Name       : create.ml
 * Encoding        : utf-8
 
 * Copyright © 2016 Kenji Sakurai. All rights reserved.
 * ======================================================================
 *)

open Asttypes
open Typedtree
open Helper
open Longident

(* 
 * make pp for variant 
 * *)
let make_variant_expr const_decl_list =
    let rec make_caselist_from_cdlist acc = function
        | [] -> List.rev acc
        | {cd_name = {txt=name;_};cd_args = Cstr_tuple core_type_list;_}::xs ->
                make_caselist_from_cdlist 
                    ({ c_lhs = make_Tpat_construct (Lident name) (pat_list (List.length core_type_list));
                       c_guard = None;
                       c_rhs = make_Texp_tuple
                                  [make_Texp_constant (Const_string (name,None));
                                   make_cps_expr_cty 1 core_type_list]} :: acc)
                    xs
        | _::xs -> failwith "TODO"
    in
    let case_list = make_caselist_from_cdlist [] const_decl_list
    in
    make_Texp_apply (make_Texp_ident (path_ident_create "_pp__variant"))
                    [Nolabel,Some (make_Texp_function case_list)]

(* 
 * make pp for record
 * *)
let make_record_expr label_decl_list =
    let make_caselist_from_ldlist () =
        let rec make_pp_fields = function
            | [] -> make_Texp_construct (Lident "[]") []
            | {ld_name = {txt=name;_};ld_type = ctype;_ }::xs ->
                    make_Texp_construct
                        (Lident "::")
                        [make_Texp_tuple
                            [make_Texp_constant (Const_string (name,None));
                             make_Texp_apply
                                (make_Texp_apply
                                    (make_Texp_ident (path_ident_create "!%"))
                                    [Nolabel,Some (select_pp_core ctype)])
                                [Nolabel,Some (make_Texp_field (make_Texp_ident (path_ident_create "_arg")) (Lident name))]];
                         make_pp_fields xs]
        in
        let expression = make_pp_fields label_decl_list
        in
        [{ c_lhs = make_Tpat_var "_arg";
           c_guard = None;
           c_rhs = expression }]
    in
    let case_list = make_caselist_from_ldlist ()
    in
    make_Texp_apply (make_Texp_ident (path_ident_create "_pp__record"))
                    [Nolabel,Some (make_Texp_function case_list)]

(* type_declaration to pp
 *
 * variant record abstract extension_open*)
let make_pp_type =
    function
    (* manifest *)
    | { typ_name = {txt=name;_}; typ_params = params; typ_manifest = Some ty;_ } ->
            Hashtbl.add params_tbl name params;
            make_vb name (fun_exp (select_pp_core (*~ty_name:name*) ty) params)
    (* variant *)
    | { typ_name = {txt=name;_}; typ_params = params; typ_kind = Ttype_variant const_decl_list;_ } ->
            let expression = make_variant_expr const_decl_list in 
            make_vb name (fun_exp expression params)
    (* record *)
    | { typ_name = {txt=name;_}; typ_params = params; typ_kind = Ttype_record label_decl_list;_ } ->
            let expression = make_record_expr label_decl_list in 
            make_vb name (fun_exp expression params)
    (* type_extension open *)
    | { typ_name = {txt=name;_}; typ_params = params; typ_kind = Ttype_open;_ } ->
            let expression = make_Texp_ident (path_ident_create "_pp__trash") in 
            Hashtbl.add caselist_tbl name [];
            make_vb name (fun_exp expression params)
    | { typ_name = {txt=name;_};_ } ->
            make_vb name (select_pp type_none)

(* create each type printer from type_declaration list  *)
let make_pp_type_set type_decl_list ret =
    let rec from_list acc = function
        | [] -> acc
        | x::xs ->
                from_list (make_pp_type x :: acc) xs
    in
    let vb_list = from_list [] type_decl_list in
    (make_Tstr_val vb_list) :: ret

(*
 * create type_extension printer from type_extension 
 * *)
let make_ext_expr ty_name ex_cons_list =
    let rec make_caselist_from_exconslist acc = function
        | [] -> List.rev acc
        | { ext_name = {txt=name;_}; ext_kind = Text_decl (Cstr_tuple core_type_list,_);_ }::xs ->
                make_caselist_from_exconslist 
                    ({ c_lhs = make_Tpat_construct (Lident name) (pat_list (List.length core_type_list));
                       c_guard = None;
                       c_rhs = make_Texp_tuple
                                  [make_Texp_constant (Const_string (name,None));
                                   make_cps_expr_cty 1 core_type_list]} :: acc)
                    xs
        | _ -> failwith "TODO"
    in
    let start = List.rev (Hashtbl.find caselist_tbl ty_name) in
    let case_list = make_caselist_from_exconslist start ex_cons_list in
    Hashtbl.add caselist_tbl ty_name case_list;
    make_Texp_apply (make_Texp_ident (path_ident_create "_pp__variant"))
                    [Nolabel,Some (make_Texp_function case_list)]


let make_pp_type_ext type_ext ret =
    match type_ext with
    | { tyext_txt = {txt=Lident name;_}; tyext_params = params; tyext_constructors = consl;_ } ->
            let expression = fun_exp (make_ext_expr name consl) params in
            let vb_list = [make_vb name expression] in
            (make_Tstr_val vb_list) :: ret
    | { tyext_txt = {txt=name;_};_ } ->
            let vb_list = [make_vb (last name) (make_Texp_ident (path_ident_create "_pp__unsup"))] in
            (make_Tstr_val vb_list) :: ret

let make_pp_sig_type_set type_decl_list ret =
    let rec from_list acc = function
        | [] -> List.rev acc
        | { typ_name = {txt=name;_}; typ_params = params; _ }::xs ->
                from_list (make_Tsig_val ("_pp_"^name) (set_ps name params) :: acc) xs
    in
    let sigl = from_list [] type_decl_list in
    sigl @ ret

let make_pp_sig_type_ext type_ext ret =
    match type_ext with
    | { tyext_txt = {txt=Lident name;_}; tyext_params = params; _ } ->
            (make_Tsig_val ("_pp_"^name) (set_ps name params)) :: ret
    | { tyext_params = params; _ } ->
            (make_Tsig_val "_pp_unsup" (set_ps "_pp_unsup" params)) :: ret

(*
 * create class printer from class_declaration
 *)
let from_classfields class_fields =
    let rec make_pp_fields = function
        | [] -> make_Texp_construct (Lident "[]") []
        | { cf_desc = Tcf_method ({txt=name;_},Public,Tcfk_concrete (_,{exp_type={Types.desc=Types.Tarrow (_,_,typ,_)};_}));_ }::xs ->
                let typ_s = Format.asprintf "%a" Printtyp.type_expr typ in
                make_Texp_construct
                    (Lident "::")
                    [make_Texp_tuple
                         [make_Texp_constant (Const_string (name,None));
                          make_Texp_apply
                              (make_Texp_apply
                                   (make_Texp_ident (path_ident_create "!%"))
                                   [Nolabel,Some (make_Texp_apply
                                                      (make_Texp_ident (path_ident_create "_pp__dump"))
                                                      [Nolabel,Some (make_Texp_constant (Const_string (typ_s,None)))])])
                              [Nolabel,Some (make_Texp_construct (Lident "()") [])]];
                     make_pp_fields xs]
        | { cf_desc = Tcf_constraint _;_ }::xs ->
                print_endline "TODO";
                make_pp_fields xs
        | { cf_desc = Tcf_initializer _;_ }::xs ->
                print_endline "TODO";
                make_pp_fields xs
        | { cf_desc = Tcf_attribute _;_ }::xs ->
                print_endline "TODO";
                make_pp_fields xs
        | _::xs ->
                make_pp_fields xs
    in
    let expression = make_pp_fields class_fields in
    let case_list =
        [{ c_lhs = make_Tpat_any;
           c_guard = None;
           c_rhs = expression }]
    in
    make_Texp_apply (make_Texp_ident (path_ident_create "_pp__object"))
                    [Nolabel,Some (make_Texp_function case_list)]


let rec make_exp_class class_expr =
    match class_expr.cl_desc with
    | Tcl_ident (_,{txt = name;_},_) ->
            make_Texp_ident (path_set (longident_to_path name))
    | Tcl_structure { cstr_fields = clfl;_ } ->
            from_classfields clfl
    | Tcl_fun (_,_,_,c_exp,_) ->
            make_exp_class c_exp
    | Tcl_apply (c_exp,_) ->
            make_exp_class c_exp
    | Tcl_constraint (c_exp,_,_,_,_) ->
            make_exp_class c_exp
    | Tcl_let (_,_,_,c_exp) ->
            make_exp_class c_exp

(* set from class_declaration_list *)
let make_pp_class_set class_decl_list ret =
    let rec from_list acc = function
        | [] -> List.rev acc
        | ({ ci_virt = Concrete; ci_id_name = {txt=cname;_}; ci_expr = c_expr; ci_params = params;_ },_)::xs ->
                let expression = fun_exp (make_exp_class c_expr) params in
                let vb = make_vb cname expression in
                from_list (vb :: acc) xs
        | _::xs ->
                from_list acc xs
    in
    let vb_list = from_list [] class_decl_list in
    if vb_list = []
    then ret
    else (make_Tstr_val vb_list) :: ret

let make_pp_sig_class_set class_desc_list ret =
    let rec from_list acc = function
        | [] -> List.rev acc
        | { ci_virt = Concrete; ci_id_name = {txt=cname;_}; ci_params = params;_ }::xs ->
                let sig_ = make_Tsig_val ("_pp_"^cname) (set_ps cname params) in
                from_list (sig_ :: acc) xs
        | _::xs ->
                from_list acc xs
    in
    let sigl = from_list [] class_desc_list in
    sigl @ ret

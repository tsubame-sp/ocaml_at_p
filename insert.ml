(* ======================================================================
 * Project Name    : OCaml@p
 * File Name       : insert.ml
 * Encoding        : utf-8
 
 * Copyright © 2016 Kenji Sakurai. All rights reserved.
 * ======================================================================
 *)

open Asttypes
open Typedtree
open Helper
open Longident

(* _this expression *)
let expr_this typ = make_Texp_ident ~typ:typ (path_ident_create "_this")

(* Format.eprintf *)
let expr_eprintf = 
    make_Texp_ident (Path.Pdot (path_ident_create "Format","eprintf",0))

(* actual insert function *)
let insert_pp expr print_expr b extra =
    let expr_format =
        (*
         * Fix me: constant:string -> construct:format
         * *)
        if b
        then
            make_Texp_constant (Const_string ("%a@.",None))
        else
            make_Texp_constant (Const_string ("%a@?",None))
    in
    let expr_pp pp = 
        make_Texp_sequence
           (make_Texp_apply
               expr_eprintf
               [Nolabel,Some expr_format;
                Nolabel,Some pp;
                Nolabel,Some print_expr])
           expr
    in
    let pp = select_pp print_expr.exp_type
    in
    expr_pp pp

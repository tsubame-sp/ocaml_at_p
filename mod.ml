open Format
open Typedtree
open Asttypes
open Helper
open Insert
open Create
open Longident

module MapArg : TypedtreeMap.MapArgument = struct
    include TypedtreeMap.DefaultMapArgument

    (* 
     * let式のpatternへのマーカ検出
     * 
     * 1つ以上あると1回だけ出力
     *)
    let rec check_attr a1 a2 = function
        | [] -> (a1,List.rev a2)
        | ({txt="p";_},_)::xs -> check_attr true a2 xs
        | x::xs -> check_attr a1 (x::a2) xs

    let rec check_pat acc = function
        | [] -> List.rev acc
        | ({vb_pat=pat;vb_expr=e;_} as vb)::xs ->
                let (check,cut) = check_attr false [] pat.pat_attributes in
                if check
                then
                    let p = {pat with pat_attributes = cut} in
                    let pat_str = 
                        Format.asprintf 
                          "%a = " 
                          Pprintast.pattern 
                          (Untypeast.(default_mapper.pat default_mapper p)) 
                    in
                    let ms =
                        let open Ast_helper in
                        let open Parsetree in
                        ({txt="ps";loc=Location.none},
                         (PStr [Str.eval (Exp.constant (Pconst_string (pat_str,None)))]))
                    in
                    let mp = ({txt="p";loc=Location.none},Parsetree.PStr []) in
                    let new_vb = 
                        {vb with vb_pat = p;
                                 vb_expr = 
                                     {e with exp_attributes = ms :: mp :: e.exp_attributes}} in
                    check_pat (new_vb :: acc) xs
                else check_pat (vb :: acc) xs

    (* 
     * expression mapper 
     * *)
    let enter_expression expr =
        let this_i = Ident.create "_this" in
        let value_d expr =
            Types.{ val_type = expr.exp_type;
                    val_kind = Val_reg;
                    val_loc = Location.none;
                    val_attributes = []
                  }
        in
        let rec pickup_attr expr extra = function
            | [] -> expr_this expr.exp_type
            (* print argument with newline *)
            | ({txt = "p";_},Parsetree.PStr [{Parsetree.pstr_desc=Parsetree.Pstr_eval (print_ast_expr,_);_}])::xs -> 
                    insert_pp (pickup_attr expr extra xs) (Typecore.type_expression Env.(add_value this_i (value_d expr) expr.exp_env) print_ast_expr) true extra
            (* print expression with newline *)
            | ({txt = "p";_},Parsetree.PStr [])::xs ->
                    insert_pp (pickup_attr expr extra xs) (expr_this expr.exp_type) true extra
            (* print argument *)
            | ({txt = "ps";_},Parsetree.PStr [{Parsetree.pstr_desc=Parsetree.Pstr_eval (print_ast_expr,_);_}])::xs -> 
                    insert_pp (pickup_attr expr extra xs) (Typecore.type_expression Env.(add_value this_i (value_d expr) expr.exp_env) print_ast_expr) false extra
            (* print expression *)
            | ({txt = "ps";_},Parsetree.PStr [])::xs ->
                    insert_pp (pickup_attr expr extra xs) (expr_this expr.exp_type) false extra
            (* other attributes *)
            | _::xs -> pickup_attr expr extra xs
        in
        let mk_exp attr extra expr =
            { exp_desc = 
                Texp_let 
                    (Nonrecursive,
                     [{  vb_pat = {  pat_desc = Tpat_var (Ident.create "_this",{txt = "_this";loc = Location.none});
                                     pat_loc = Location.none;
                                     pat_extra = [];
                                     pat_type = expr.exp_type;
                                     pat_env = Env.empty;
                                     pat_attributes = [];
                                  };
                         vb_expr = { expr with exp_attributes = [];exp_extra = extra };
                         vb_attributes = [];
                         vb_loc = Location.none;
                      }],
                     pickup_attr expr extra attr);
              exp_loc = Location.none;
              exp_extra = [];
              exp_type = expr.exp_type;
              exp_env = expr.exp_env;
              exp_attributes = []
            }
        in
        let check_extra ls =
            let rec loop acc1 acc2 = function
                | [] -> (acc1,List.rev acc2)
                | (t1,t2,x)::xs -> 
                        if List.length x = 0 
                        then loop acc1 ((t1,t2,x)::acc2) xs
                        else loop (x @ acc1) ((t1,t2,[])::acc2) xs
            in
            loop [] [] ls
        in
        let pat_checked_expr =
            match expr.exp_desc with
            | Texp_let (f,vblist,e) ->
                    let new_vblist = check_pat [] vblist in
                    { expr with exp_desc = Texp_let (f,new_vblist,e) }
            | _ ->  expr
        in
        let (extra_attr,extra) = check_extra pat_checked_expr.exp_extra in
        if List.length pat_checked_expr.exp_attributes = 0
        then 
           (if List.length extra_attr = 0
            then pat_checked_expr
            else mk_exp extra_attr extra pat_checked_expr)
        else mk_exp (expr.exp_attributes @ extra_attr) extra pat_checked_expr

    (* 
     * structure_item mapper 
     * *)
    let enter_structure structure =
        let rec select_str_item acc = function
            | [] -> List.rev acc
            (* declaration let *)
            | ({str_desc = Tstr_value (flag,vblist);_} as str_item)::xs ->
                    let new_vblist = check_pat [] vblist in
                    select_str_item ({ str_item with str_desc = Tstr_value (flag,new_vblist) }::acc) xs
            (* variant, record, type_extension open, poly variant*)
            | ({str_desc = Tstr_type (_,type_decl_list);_} as str_item)::xs ->
                    select_str_item (make_pp_type_set type_decl_list (str_item :: acc)) xs
            (* type_extension extend *)
            | ({str_desc = Tstr_typext (type_ext);_} as str_item)::xs ->
                    select_str_item (make_pp_type_ext type_ext (str_item :: acc)) xs
            (* class *)
            | ({str_desc = Tstr_class cdslist;_} as str_item)::xs ->
                    select_str_item (make_pp_class_set cdslist (str_item :: acc)) xs
            (* ppopen *)
            | {str_desc = 
                Tstr_attribute
                   ({txt="ppopen";_},
                    Parsetree.PStr ([{Parsetree.pstr_desc = 
                        Parsetree.Pstr_eval ({Parsetree.pexp_desc=Parsetree.Pexp_construct ({txt = Lident name;_},_);_},_);_}])
                   )}::xs ->
                       ppopen := SSet.add name !ppopen;
                       select_str_item acc xs
            (* module ppopen *)
            | ({str_desc = Tstr_module ({mb_name = {txt=name;_}; mb_expr = me;_} as mb);_} as str_item)::xs ->
                    ppopen := SSet.add name !ppopen;
                    select_str_item ({str_item with str_desc = Tstr_module {mb with mb_expr = enter_module_expr me}} :: acc) xs
            | x::xs ->  select_str_item ((enter_structure_item x)::acc) xs
        in
        { structure with str_items = (select_str_item [] structure.str_items) }

    (* 
     * structure_item mapper 
     * *)
    let enter_signature signature =
        let rec select_sig_item acc = function
            | [] -> List.rev acc
            (* variant, record, type_extension open, poly variant*)
            | ({sig_desc = Tsig_type (_,type_decl_list);_} as sig_item)::xs ->
                    select_sig_item (make_pp_sig_type_set type_decl_list (sig_item :: acc)) xs
            (* type_extension extend *)
            | ({sig_desc = Tsig_typext (type_ext);_} as sig_item)::xs ->
                    select_sig_item (make_pp_sig_type_ext type_ext (sig_item :: acc)) xs
            (* class *)
            | ({sig_desc = Tsig_class cdesclist;_} as sig_item)::xs ->
                    select_sig_item (make_pp_sig_class_set cdesclist (sig_item :: acc)) xs
            | x::xs ->  select_sig_item ((enter_signature_item x)::acc) xs
        in
        { signature with sig_items = (select_sig_item [] signature.sig_items) }

    (* 
     * module Print open 
     * *)
    let leave_structure structure =
        let str = { str_desc =
                       Tstr_open { open_path = path_ident_create "Print";
                                   open_txt = {txt=Lident "Print";loc=Location.none};
                                   open_override = Fresh;
                                   open_loc = Location.none;
                                   open_attributes = []};
                    str_loc = Location.none;
                    str_env = Env.empty}
        in
        { structure with str_items = str :: structure.str_items }

    (*
     * module_expr
     *)
    let rec enter_module_expr module_expr =
        match module_expr.mod_desc with
        | Tmod_functor (id,a,b,me) ->
                let name = Ident.name id in
                (ppopen := SSet.add name !ppopen;
                 (let ret = enter_module_expr me in
                  (*ppopen := SSet.remove name !ppopen;*)
                  {module_expr with mod_desc = Tmod_functor (id,a,b,ret)}))
        | Tmod_apply (m1,m2,mc) ->
                {module_expr with mod_desc = Tmod_apply (enter_module_expr m1,enter_module_expr m2,mc)}
        | _ -> leave_module_expr module_expr
end

module Map = TypedtreeMap.MakeMap(MapArg)

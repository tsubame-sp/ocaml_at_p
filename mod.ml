open Format
open Typedtree
open Asttypes
open Helper
open Insert
open Create

let pe s = pp_print_string std_formatter s;
           pp_print_newline std_formatter ()
let ps = pp_print_string std_formatter
let pn = pp_print_newline std_formatter
let pi = pp_print_int std_formatter

module MapArg : TypedtreeMap.MapArgument = struct
    include TypedtreeMap.DefaultMapArgument

    (* 
     * expression mapper 
     * *)
    let enter_expression expr =
        let rec pickup_attr = function
            | [] -> expr_this expr.exp_type
            (* print argument with newline *)
            | ({txt = "p";_},Parsetree.PStr [{pstr_desc=Pstr_eval (print_ast_expr,_);_}])::xs -> 
                    insert_pp (pickup_attr xs) (Typecore.type_expression expr.exp_env print_ast_expr) true
            (* print expression with newline *)
            | ({txt = "p";_},Parsetree.PStr [])::xs ->
                    insert_pp (pickup_attr xs) (expr_this expr.exp_type) true
            (* print argument *)
            | ({txt = "ps";_},Parsetree.PStr [{pstr_desc=Pstr_eval (print_ast_expr,_);_}])::xs -> 
                    insert_pp (pickup_attr xs) (Typecore.type_expression expr.exp_env print_ast_expr) false
            (* print expression *)
            | ({txt = "ps";_},Parsetree.PStr [])::xs ->
                    insert_pp (pickup_attr xs) (expr_this expr.exp_type) false
            (* other attributes *)
            | _::xs -> pickup_attr xs
        in
        let mk_exp attr extra =
            { exp_desc = 
                Texp_let 
                    (Nonrecursive,
                     [{  vb_pat = {  pat_desc = Tpat_var (Ident.create "_this_",{txt = "_this_";loc = Location.none});
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
                     pickup_attr attr);
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
        let (extra_attr,extra) = check_extra expr.exp_extra in
        let ret = ref expr in
        if List.length expr.exp_attributes = 0
        then if List.length extra_attr = 0
             then expr
             else mk_exp extra_attr extra
        else mk_exp (expr.exp_attributes @ extra_attr) extra

    (* 
     * structure_item mapper 
     * *)
    let enter_structure structure =
        let rec select_str_item acc = function
            | [] -> List.rev acc
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
                Tstr_attribute ({txt="ppopen";_},
                                PStr ([{pstr_desc = 
                                    Pstr_eval ({pexp_desc=Pexp_construct ({txt = Lident name;_},_);_},_);_}]))}::xs ->
                    ppopen := name :: !ppopen;
                    select_str_item acc xs
            (* module ppopen *)
            | ({str_desc = Tstr_module ({mb_name = {txt=name;_}; mb_expr = me;_} as mb);_} as str_item)::xs ->
                    ppopen := name :: !ppopen;
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
     * module Ppshow open 
     * *)
    let leave_structure structure =
        let str = { str_desc =
                       Tstr_open { open_path = path_ident_create "Ppshow";
                                   open_txt = {txt=Lident "Ppshow";loc=Location.none};
                                   open_override = Fresh;
                                   open_loc = Location.none;
                                   open_attributes = []};
                    str_loc = Location.none;
                    str_env = Env.empty}
        in
        { structure with str_items = str :: structure.str_items }
end

module Map = TypedtreeMap.MakeMap(MapArg)

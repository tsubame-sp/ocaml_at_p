open Asttypes
open Typedtree
open Helper
open Longident

(* _this expression *)
let expr_this typ =
    { exp_desc = Texp_ident (Path.Pident (Ident.create "_this"),
                             {txt = Lident "_this";loc = Location.none},
                             {Types.val_type = typ;
                              Types.val_kind=Types.Val_reg;
                              Types.val_loc=Location.none;
                              Types.val_attributes=[]});
      exp_loc = Location.none;
      exp_extra = [];
      exp_type = typ;
      exp_env = Env.empty;
      exp_attributes = []
    }

(* actual insert function *)
let insert_pp expr print_expr b =
    let expr_newline =
        if b
        then
            { exp_desc = 
                Texp_sequence 
                    ({ exp_desc = 
                        Texp_apply (make_Texp_ident (path_ident_create "_pp_newline"),
                                   [Nolabel,Some expr_std_formatter;Nolabel,Some (make_Texp_construct (Lident "()") [])]);
                       exp_loc = Location.none;
                       exp_extra = [];
                       exp_type = type_none(*{print_expr.exp_type with desc = Tvar (Some "unit")}*);
                       exp_env = expr.exp_env;
                       exp_attributes = []
                     },
                     expr);
              exp_loc = Location.none;
              exp_extra = [];
              exp_type = expr.exp_type;
              exp_env = expr.exp_env;
              exp_attributes = []
            }
        else expr
    in
    let expr_pp = 
        { exp_desc = 
            Texp_sequence 
                ({ exp_desc = 
                    Texp_apply (select_pp print_expr.exp_type,
                               [Nolabel,Some expr_std_formatter;Nolabel,Some print_expr]);
                   exp_loc = Location.none;
                   exp_extra = [];
                   exp_type = type_none;
                   exp_env = expr.exp_env;
                   exp_attributes = []
                 },
                 expr_newline);
          exp_loc = Location.none;
          exp_extra = [];
          exp_type = expr.exp_type;
          exp_env = expr.exp_env;
          exp_attributes = []
        }
    in
    expr_pp

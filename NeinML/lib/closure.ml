open Ast

let empty = Base.Set.empty (module Base.String)
let singleton = Base.Set.singleton (module Base.String)
let of_list = Base.Set.of_list (module Base.String)
let empty_map = Base.Map.empty (module Base.String)

type name_set = (string, Base.String.comparator_witness) Base.Set.t
type free_vars_map_type = (string, name_set, Base.String.comparator_witness) Base.Map.t

let get_args (func : Ast.expression) =
  let rec func_helper acc = function
    | Func (arg, inner_func) -> func_helper (Base.List.append acc [ arg ]) inner_func
    | _ as func_body -> acc, func_body
  in
  let args, body = func_helper [] func in
  args, body
;;

let rec get_all_vars (acc : name_set) = function
  | Variable name -> Base.Set.add acc name
  | Apply (func, arg) ->
    let acc_with_func_vars = get_all_vars acc func in
    get_all_vars acc_with_func_vars arg
  | Func (_, _) as func ->
    let args, body = get_args func in
    let body_vars = get_all_vars acc body in
    Base.Set.diff body_vars (of_list args)
  | LetIn (name, definition, body) | RecLetIn (name, definition, body) ->
    let acc1 = get_all_vars acc definition in
    let acc2 = Base.Set.remove acc1 name in
    get_all_vars acc2 body
  | Lambda func -> get_all_vars acc func
  | Value _ -> acc
  | Add (expr1, expr2)
  | Mul (expr1, expr2)
  | Div (expr1, expr2)
  | Mod (expr1, expr2)
  | Sub (expr1, expr2)
  | And (expr1, expr2)
  | Or (expr1, expr2)
  | Equal (expr1, expr2)
  | NotEqual (expr1, expr2)
  | Less (expr1, expr2)
  | LessOrEq (expr1, expr2)
  | More (expr1, expr2)
  | MoreOrEq (expr1, expr2) ->
    let acc1 = get_all_vars acc expr1 in
    get_all_vars acc1 expr2
  | IfThenElse (condition, expr1, expr2) ->
    let acc1 = get_all_vars acc condition in
    let acc2 = get_all_vars acc1 expr1 in
    get_all_vars acc2 expr2
;;

let get_stmt_vars = function
  | Define (_, expr) | RecDefine (_, expr) -> get_all_vars empty expr
;;

let check_func stmt =
  let print_set my_set = Base.Set.iter my_set ~f:(fun x -> Format.printf "%s " x) in
  let result = get_stmt_vars stmt in
  print_set result
;;

let check_func_expr stmt =
  let print_set my_set = Base.Set.iter my_set ~f:(fun x -> Format.printf "%s " x) in
  let result = get_all_vars empty stmt in
  print_set result
;;

let get_op_constructor = function
  | Add (_, _) -> cadd
  | Sub (_, _) -> csub
  | Mul (_, _) -> cmul
  | Div (_, _) -> cdiv
  | Mod (_, _) -> cmod
  | Equal (_, _) -> ceq
  | NotEqual (_, _) -> cnoteq
  | More (_, _) -> cmore
  | MoreOrEq (_, _) -> cmoreeq
  | Less (_, _) -> cless
  | LessOrEq (_, _) -> clesseq
  | And (_, _) -> cand
  | Or (_, _) -> cor
  | _ -> fun _ _ -> Variable "not happening"
;;

let get_letin_constructor = function
  | LetIn (_, _, _) -> cletin
  | RecLetIn (_, _, _) -> crecletin
  | _ -> fun _ _ _ -> Variable "not happening"
;;

let get_define_constructor = function
  | Define (_, _) -> cdef
  | RecDefine (_, _) -> crecdef
;;

let rec closure_conversion
  (free_vars_map : free_vars_map_type)
  (global_scope : name_set)
  (expr : expression)
  : expression
  =
  match expr with
  | LetIn (name, definition, body) | RecLetIn (name, definition, body) ->
    let args, def_body = get_args definition in
    let all_def_vars = get_all_vars empty def_body in
    let free_vars = Base.Set.diff all_def_vars (of_list args) in
    let free_vars = Base.Set.diff free_vars global_scope in
    let free_vars = Base.Set.diff free_vars (singleton name) in
    let new_map = Base.Map.set free_vars_map ~key:name ~data:free_vars in
    let def_conversion = closure_conversion new_map global_scope def_body in
    let def_conversion_with_args =
      Base.List.fold (List.rev args) ~init:def_conversion ~f:(fun acc arg ->
        Func (arg, acc))
    in
    let def_conversion_with_free_vars =
      Base.List.fold
        (Base.List.rev (Base.Set.to_list free_vars))
        ~init:def_conversion_with_args
        ~f:(fun acc arg -> Func (arg, acc))
    in
    let body_conversion = closure_conversion new_map global_scope body in
    let constructor = get_letin_constructor expr in
    constructor name def_conversion_with_free_vars body_conversion
  | Lambda func ->
    let args, body = get_args func in
    let conversed_body = closure_conversion free_vars_map global_scope body in
    let lambda_conversion_expr =
      Base.List.fold (List.rev args) ~init:conversed_body ~f:(fun acc arg ->
        Func (arg, acc))
    in
    let lambda_free_vars = get_all_vars empty lambda_conversion_expr in
    let func_with_new_args =
      Base.List.fold
        (Base.List.rev (Base.Set.to_list lambda_free_vars))
        ~init:lambda_conversion_expr
        ~f:(fun acc arg -> Func (arg, acc))
    in
    let apply_new_args =
      Base.Set.fold lambda_free_vars ~init:(Lambda func_with_new_args) ~f:(fun acc arg ->
        Apply (acc, Variable arg))
    in
    apply_new_args
  | Variable name ->
    (match Base.Map.find free_vars_map name with
     | Some new_args ->
       let make_application =
         Base.Set.fold new_args ~init:(Variable name) ~f:(fun acc arg ->
           Apply (acc, Variable arg))
       in
       make_application
     | None -> Variable name)
  | Add (expr1, expr2)
  | Mul (expr1, expr2)
  | Div (expr1, expr2)
  | Mod (expr1, expr2)
  | Sub (expr1, expr2)
  | And (expr1, expr2)
  | Or (expr1, expr2)
  | Equal (expr1, expr2)
  | NotEqual (expr1, expr2)
  | Less (expr1, expr2)
  | LessOrEq (expr1, expr2)
  | More (expr1, expr2)
  | MoreOrEq (expr1, expr2) ->
    let conversed_left = closure_conversion free_vars_map global_scope expr1 in
    let conversed_right = closure_conversion free_vars_map global_scope expr2 in
    let constructor = get_op_constructor expr in
    constructor conversed_left conversed_right
  | Apply (inner_func, last_arg) ->
    let conversed_func = closure_conversion free_vars_map global_scope inner_func in
    let conversed_arg = closure_conversion free_vars_map global_scope last_arg in
    capply conversed_func conversed_arg
  | Value _ as value -> value
  | IfThenElse (cond, expr1, expr2) ->
    let conversed_cond = closure_conversion free_vars_map global_scope cond in
    let conversed_expr1 = closure_conversion free_vars_map global_scope expr1 in
    let conversed_expr2 = closure_conversion free_vars_map global_scope expr2 in
    IfThenElse (conversed_cond, conversed_expr1, conversed_expr2)
  | Func (_, _) -> Variable "not happening"
;;

let rec converse_stms (global_scope : name_set) (acc : statements_list) = function
  | stmt :: tail ->
    (match stmt with
     | Define (name, body) | RecDefine (name, body) ->
       let args, def_body = get_args body in
       let new_scope = Base.Set.add global_scope name in
       let scope_for_conversion = Base.Set.union new_scope (of_list args) in
       let conversed_body = closure_conversion empty_map scope_for_conversion def_body in
       let conversed_func =
         Base.List.fold (List.rev args) ~init:conversed_body ~f:(fun acc arg ->
           Func (arg, acc))
       in
       let constructor = get_define_constructor stmt in
       let new_stmt = constructor name conversed_func in
       converse_stms new_scope (new_stmt :: acc) tail)
  | _ -> acc
;;

let closure_converse stmts_list = converse_stms empty [] stmts_list

let closure_test expr =
  Format.printf "%a\n%!" Ast.pp_statements_list (converse_stms empty [] expr)
;;

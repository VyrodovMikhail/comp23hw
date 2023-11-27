open Ast
open Typing
open Base

let empty = Base.Set.empty (module Base.String)
let singleton = Base.Set.singleton (module Base.String)
let of_list = Base.Set.of_list (module Base.String)
let empty_map = Base.Map.empty (module Base.String)

type name_set = (string, Base.String.comparator_witness) Base.Set.t
type typ_map = (string, ty, Base.String.comparator_witness) Base.Map.t

type free_vars_map_type =
  (string, typ_map * ty, Base.String.comparator_witness) Base.Map.t

let get_arg_typ = function
  | Arrow (arg_typ, _) -> arg_typ
  | other -> other
;;

let get_args_body (func : ty Ast.expression) =
  let rec func_helper acc = function
    | Func (arg, inner_func, typ) ->
      func_helper ((arg, get_arg_typ typ) :: acc) inner_func
    | _ as func_body -> acc, func_body
  in
  let args, body = func_helper [] func in
  args, body
;;

let remove_keys map keys =
  Base.Map.fold map ~init:map ~f:(fun ~key ~data fold_map ->
    match Base.Set.mem keys key with
    | true -> Base.Map.remove fold_map key
    | false -> fold_map)
;;

let rec get_unbound_vars (acc : typ_map) = function
  | Variable (name, typ) ->
    Base.Map.update acc name ~f:(fun x ->
      match x with
      | None -> typ
      | Some _ -> typ)
  | Apply (func, arg, _) ->
    let acc_with_func_vars = get_unbound_vars acc func in
    get_unbound_vars acc_with_func_vars arg
  | Func (_, _, _) as func ->
    let args, body = get_args_body func in
    let body_vars = get_unbound_vars acc body in
    let args_set = of_list (Base.List.map args ~f:fst) in
    remove_keys body_vars args_set
  | LetIn (name, definition, body, _) | RecLetIn (name, definition, body, _) ->
    let acc1 = get_unbound_vars acc definition in
    let acc2 = Base.Map.remove acc1 name in
    get_unbound_vars acc2 body
  | Value _ -> acc
  | BinOp (expr1, expr2, _, _) ->
    let acc1 = get_unbound_vars acc expr1 in
    get_unbound_vars acc1 expr2
  | IfThenElse (condition, expr1, expr2, _) ->
    let acc1 = get_unbound_vars acc condition in
    let acc2 = get_unbound_vars acc1 expr1 in
    get_unbound_vars acc2 expr2
;;

let get_stmt_vars = function
  | Define (_, expr, _) | RecDefine (_, expr, _) -> get_unbound_vars empty_map expr
;;

let check_func stmt =
  let print_keys my_map = Base.Map.iter_keys my_map ~f:(fun x -> Format.printf "%s " x) in
  let result = get_stmt_vars stmt in
  print_keys result
;;

let get_letin_constructor = function
  | LetIn (_, _, _, _) -> cletin
  | RecLetIn (_, _, _, _) -> crecletin
  | _ -> fun _ _ _ -> cvar "Not happening"
;;

let get_define_constructor = function
  | Define (_, _, _) -> cdef
  | RecDefine (_, _, _) -> crecdef
;;

let delete_arrow = function
  | Arrow (_, inner_type) -> inner_type
  | another_case -> another_case
;;

type ancestor_type =
  | Func
  | NotFunctype

let rec closure_conversion
  (free_vars_map : free_vars_map_type)
  (global_scope : name_set)
  (ancestor : ancestor_type)
  (expr : ty expression)
  : ty expression
  =
  match expr with
  | LetIn (name, definition, body, _) | RecLetIn (name, definition, body, _) ->
    (match get_meta definition with
     | Prim _ as let_typ ->
       let new_map = Base.Map.set free_vars_map ~key:name ~data:(empty_map, let_typ) in
       let body_conversion = closure_conversion new_map global_scope NotFunctype body in
       let body_type = get_meta body_conversion in
       let constructor = get_letin_constructor expr in
       constructor name definition body_conversion body_type
     | let_typ ->
       let args, def_body = get_args_body definition in
       let all_def_vars = get_unbound_vars empty_map def_body in
       let free_vars = remove_keys all_def_vars (of_list (Base.List.map args ~f:fst)) in
       let free_vars = remove_keys free_vars global_scope in
       let free_vars = remove_keys free_vars (singleton name) in
       let new_func_type =
         Base.List.fold
           (Base.List.rev (Base.Map.keys free_vars))
           ~init:let_typ
           ~f:(fun acc arg -> Arrow (Base.Map.find_exn free_vars arg, acc))
       in
       let new_map =
         Base.Map.set free_vars_map ~key:name ~data:(free_vars, new_func_type)
       in
       let def_conversion =
         closure_conversion new_map global_scope NotFunctype def_body
       in
       let def_conversion_with_args =
         Base.List.fold args ~init:def_conversion ~f:(fun acc arg ->
           let arg_name, arg_typ = arg in
           Func (arg_name, acc, Arrow (arg_typ, get_meta acc)))
       in
       let def_conversion_with_free_vars =
         Base.List.fold
           (Base.List.rev (Base.Map.keys free_vars))
           ~init:def_conversion_with_args
           ~f:(fun acc arg ->
             Func (arg, acc, Arrow (Base.Map.find_exn free_vars arg, get_meta acc)))
       in
       let new_map =
         Base.Map.update new_map name ~f:(fun _ ->
           free_vars, get_meta def_conversion_with_free_vars)
       in
       let body_conversion = closure_conversion new_map global_scope NotFunctype body in
       let body_type = get_meta body_conversion in
       let constructor = get_letin_constructor expr in
       constructor name def_conversion_with_free_vars body_conversion body_type)
  | Variable (name, typ) ->
    (match Base.Map.find free_vars_map name with
     | Some (new_args, new_typ) ->
       let make_application =
         Base.Map.fold
           new_args
           ~init:(Variable (name, new_typ))
           ~f:(fun ~key ~data acc ->
             Apply (acc, Variable (key, data), delete_arrow (get_meta acc)))
       in
       make_application
     | None -> Variable (name, typ))
  | BinOp (expr1, expr2, op, _) as binop ->
    let conversed_left =
      closure_conversion free_vars_map global_scope NotFunctype expr1
    in
    let conversed_right =
      closure_conversion free_vars_map global_scope NotFunctype expr2
    in
    cbinop conversed_left conversed_right op (get_meta binop)
  | Apply (inner_func, last_arg, apply_typ) ->
    let conversed_func =
      closure_conversion free_vars_map global_scope NotFunctype inner_func
    in
    let conversed_arg =
      closure_conversion free_vars_map global_scope NotFunctype last_arg
    in
    let new_typ =
      match delete_arrow (get_meta conversed_func) with
      | Ty_var _ -> apply_typ
      | normal_typ -> normal_typ
    in
    capply conversed_func conversed_arg new_typ
  | Value _ as value -> value
  | IfThenElse (cond, expr1, expr2, _) as if_stmt ->
    let conversed_cond = closure_conversion free_vars_map global_scope NotFunctype cond in
    let conversed_expr1 =
      closure_conversion free_vars_map global_scope NotFunctype expr1
    in
    let conversed_expr2 =
      closure_conversion free_vars_map global_scope NotFunctype expr2
    in
    cifthenelse conversed_cond conversed_expr1 conversed_expr2 (get_meta if_stmt)
  | Func (arg_name, inner_func, func_typ) as new_func ->
    (match ancestor with
     | NotFunctype ->
       let conversed_func = closure_conversion free_vars_map global_scope Func new_func in
       let lambda_free_vars = get_unbound_vars empty_map conversed_func in
       let func_with_new_args =
         Base.List.fold
           (Base.List.rev (Base.Map.keys lambda_free_vars))
           ~init:conversed_func
           ~f:(fun acc arg ->
             Func (arg, acc, Arrow (Base.Map.find_exn lambda_free_vars arg, get_meta acc)))
       in
       let apply_new_args =
         Base.Map.fold lambda_free_vars ~init:func_with_new_args ~f:(fun ~key ~data acc ->
           Apply (acc, Variable (key, data), delete_arrow (get_meta acc)))
       in
       apply_new_args
     | Func ->
       let conversed_inner_func =
         closure_conversion free_vars_map global_scope Func inner_func
       in
       let arg_typ = get_arg_typ func_typ in
       Func
         (arg_name, conversed_inner_func, Arrow (arg_typ, get_meta conversed_inner_func)))
;;

let rec converse_stms (global_scope : name_set) (acc : ty statements_list) = function
  | stmt :: tail ->
    (match stmt with
     | Define (name, body, _) | RecDefine (name, body, _) ->
       let args, def_body = get_args_body body in
       let new_scope = Base.Set.add global_scope name in
       (* let scope_for_conversion = Base.Set.union new_scope (of_list args) in *)
       let conversed_body = closure_conversion empty_map new_scope NotFunctype def_body in
       let conversed_func =
         Base.List.fold args ~init:conversed_body ~f:(fun acc arg ->
           let arg_name, arg_typ = arg in
           Func (arg_name, acc, Arrow (arg_typ, get_meta acc)))
       in
       let constructor = get_define_constructor stmt in
       let new_stmt = constructor name conversed_func (get_meta conversed_func) in
       converse_stms new_scope (new_stmt :: acc) tail)
  | _ -> acc
;;

(* let closure_converse stmts_list = [] *)

let closure_converse stmts_list = converse_stms empty [] stmts_list

let closure_test expr =
  Format.printf "%a\n%!" (Ast.pp_statements_list pp_type) (converse_stms empty [] expr)
;;

(* let closure_test expr = Format.printf "%a\n%!" (Ast.pp_statements_list (fun _ _ -> ())) [] *)

(*let%expect_test _ =
  closure_test;
  [%expect {| |}]
;; *)
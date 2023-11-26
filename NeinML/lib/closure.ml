open Ast

let empty = Base.Set.empty (module Base.String)
let singleton = Base.Set.singleton (module Base.String)
let of_list = Base.Set.of_list (module Base.String)
let empty_map = Base.Map.empty (module Base.String)

type name_set = (string, Base.String.comparator_witness) Base.Set.t
type free_vars_map_type = (string, name_set, Base.String.comparator_witness) Base.Map.t
type name_mapping = (string, string, Base.String.comparator_witness) Base.Map.t

module IntState = struct
  include Monad.State (struct
      type t = int * name_set
    end)

  let ( let* ) = ( >>= )

  let incrementCounter =
    let* counter, scope = get in
    let* () = put (counter + 1, scope) in
    return counter
  ;;

  let fresh name =
    let* num = incrementCounter in
    return @@ String.concat "" [ "__neinml_uni"; string_of_int num; name ]
  ;;

  let get_scope =
    let* _, scope = get in
    return scope
  ;;

  let put_scope scope =
    let* counter, _ = get in
    put (counter, scope)
  ;;
end

module ListM = Monad.ListM (IntState)

let unique_names stmts =
  let open IntState in
  let process_name mapping name =
    let* scope = get_scope in
    if Base.Set.mem scope name
    then
      let* new_name = fresh name in
      let mapping = Base.Map.set mapping ~key:name ~data:new_name in
      return (mapping, new_name)
    else
      let* () = Base.Set.add scope name |> put_scope in
      return (mapping, name)
  in
  let rec expr_helper mapping = function
    | Lambda _ -> failwith "please remove it!!!!!!!"
    | BinOp (op_left, op_right, op, meta) ->
      let* op_left = expr_helper mapping op_left in
      let* op_right = expr_helper mapping op_right in
      return @@ BinOp (op_left, op_right, op, meta)
    | IfThenElse (cond, thn, els, meta) ->
      let* cond = expr_helper mapping cond in
      let* thn = expr_helper mapping thn in
      let* els = expr_helper mapping els in
      return @@ IfThenElse (cond, thn, els, meta)
    | Apply (f, x, meta) ->
      let* f = expr_helper mapping f in
      let* x = expr_helper mapping x in
      return @@ Apply (f, x, meta)
    | Value _ as v -> return v
    | Variable (name, meta) ->
      let new_name = Base.Map.find mapping name |> Base.Option.value ~default:name in
      return @@ Variable (new_name, meta)
    | Func (name, body, meta) ->
      let* mapping, new_name = process_name mapping name in
      let* body = expr_helper mapping body in
      return @@ Func (new_name, body, meta)
    | LetIn (name, def, body, meta) ->
      let* new_mapping, new_name = process_name mapping name in
      let* new_def = expr_helper mapping def in
      let* new_body = expr_helper new_mapping body in
      return @@ LetIn (new_name, new_def, new_body, meta)
    | RecLetIn (name, def, body, meta) ->
      let* mapping, new_name = process_name mapping name in
      let* new_def = expr_helper mapping def in
      let* new_body = expr_helper mapping body in
      return @@ RecLetIn (new_name, new_def, new_body, meta)
  in
  let stmt_helper mapping = function
    | Define (name, body, meta) ->
      let* new_body = expr_helper mapping body in
      let* mapping, new_name = process_name mapping name in
      return (Define (new_name, new_body, meta), mapping)
    | RecDefine (name, body, meta) ->
      let* mapping, new_name = process_name mapping name in
      let* new_body = expr_helper mapping body in
      return (RecDefine (new_name, new_body, meta), mapping)
  in
  let rename_stmt (mapping, stmts) stmt =
    let* stmt, mapping = stmt_helper mapping stmt in
    return (mapping, stmt :: stmts)
  in
  ListM.fold_left rename_stmt (empty_map, []) stmts
  |> IntState.eval (0, empty)
  |> snd
  |> List.rev
;;

let get_args_body (func : _ Ast.expression) =
  let rec func_helper acc = function
    | Func (arg, inner_func, _) -> func_helper (arg :: acc) inner_func
    | _ as func_body -> acc, func_body
  in
  let args, body = func_helper [] func in
  args, body
;;

let rec get_unbound_vars (acc : name_set) = function
  | Variable (name, _) -> Base.Set.add acc name
  | Apply (func, arg, _) ->
    let acc_with_func_vars = get_unbound_vars acc func in
    get_unbound_vars acc_with_func_vars arg
  | Func (_, _, _) as func ->
    let args, body = get_args_body func in
    let body_vars = get_unbound_vars acc body in
    Base.Set.diff body_vars (of_list args)
  | LetIn (name, definition, body, _) | RecLetIn (name, definition, body, _) ->
    let acc1 = get_unbound_vars acc definition in
    let acc2 = Base.Set.remove acc1 name in
    get_unbound_vars acc2 body
  | Lambda (func, _) -> get_unbound_vars acc func
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
  | Define (_, expr, _) | RecDefine (_, expr, _) -> get_unbound_vars empty expr
;;

let check_func stmt =
  let print_set my_set = Base.Set.iter my_set ~f:(fun x -> Format.printf "%s " x) in
  let result = get_stmt_vars stmt in
  print_set result
;;

let check_func_expr stmt =
  let print_set my_set = Base.Set.iter my_set ~f:(fun x -> Format.printf "%s " x) in
  let result = get_unbound_vars empty stmt in
  print_set result
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

let rec closure_conversion
  (free_vars_map : free_vars_map_type)
  (global_scope : name_set)
  (expr : unit expression)
  : unit expression
  =
  match expr with
  | LetIn (name, definition, body, _) | RecLetIn (name, definition, body, _) ->
    let args, def_body = get_args_body definition in
    let all_def_vars = get_unbound_vars empty def_body in
    let free_vars = Base.Set.diff all_def_vars (of_list args) in
    let free_vars = Base.Set.diff free_vars global_scope in
    let free_vars = Base.Set.diff free_vars (singleton name) in
    let new_map = Base.Map.set free_vars_map ~key:name ~data:free_vars in
    let def_conversion = closure_conversion new_map global_scope def_body in
    let def_conversion_with_args =
      Base.List.fold args ~init:def_conversion ~f:(fun acc arg -> Func (arg, acc, ()))
    in
    let def_conversion_with_free_vars =
      Base.List.fold
        (Base.List.rev (Base.Set.to_list free_vars))
        ~init:def_conversion_with_args
        ~f:(fun acc arg -> Func (arg, acc, ()))
    in
    let body_conversion = closure_conversion new_map global_scope body in
    let constructor = get_letin_constructor expr in
    constructor name def_conversion_with_free_vars body_conversion ()
  | Lambda (func, _) ->
    let args, body = get_args_body func in
    let conversed_body = closure_conversion free_vars_map global_scope body in
    let lambda_conversion_expr =
      Base.List.fold args ~init:conversed_body ~f:(fun acc arg -> Func (arg, acc, ()))
    in
    let lambda_free_vars = get_unbound_vars empty lambda_conversion_expr in
    let func_with_new_args =
      Base.List.fold
        (Base.List.rev (Base.Set.to_list lambda_free_vars))
        ~init:lambda_conversion_expr
        ~f:(fun acc arg -> Func (arg, acc, ()))
    in
    let apply_new_args =
      Base.Set.fold
        lambda_free_vars
        ~init:(Lambda (func_with_new_args, ()))
        ~f:(fun acc arg -> Apply (acc, Variable (arg, ()), ()))
    in
    apply_new_args
  | Variable (name, ()) ->
    (match Base.Map.find free_vars_map name with
     | Some new_args ->
       let make_application =
         Base.Set.fold
           new_args
           ~init:(Variable (name, ()))
           ~f:(fun acc arg -> Apply (acc, Variable (arg, ()), ()))
       in
       make_application
     | None -> Variable (name, ()))
  | BinOp (expr1, expr2, op, _) ->
    let conversed_left = closure_conversion free_vars_map global_scope expr1 in
    let conversed_right = closure_conversion free_vars_map global_scope expr2 in
    cbinop conversed_left conversed_right op ()
  | Apply (inner_func, last_arg, _) ->
    let conversed_func = closure_conversion free_vars_map global_scope inner_func in
    let conversed_arg = closure_conversion free_vars_map global_scope last_arg in
    capply conversed_func conversed_arg ()
  | Value _ as value -> value
  | IfThenElse (cond, expr1, expr2, _) ->
    let conversed_cond = closure_conversion free_vars_map global_scope cond in
    let conversed_expr1 = closure_conversion free_vars_map global_scope expr1 in
    let conversed_expr2 = closure_conversion free_vars_map global_scope expr2 in
    cifthenelse conversed_cond conversed_expr1 conversed_expr2 ()
  | Func (_, _, _) -> Variable ("not happening", ())
;;

let rec converse_stms (global_scope : name_set) (acc : unit statements_list) = function
  | stmt :: tail ->
    (match stmt with
     | Define (name, body, _) | RecDefine (name, body, _) ->
       let args, def_body = get_args_body body in
       let new_scope = Base.Set.add global_scope name in
       let scope_for_conversion = Base.Set.union new_scope (of_list args) in
       let conversed_body = closure_conversion empty_map scope_for_conversion def_body in
       let conversed_func =
         Base.List.fold args ~init:conversed_body ~f:(fun acc arg -> Func (arg, acc, ()))
       in
       let constructor = get_define_constructor stmt in
       let new_stmt = constructor name conversed_func () in
       converse_stms new_scope (new_stmt :: acc) tail)
  | _ -> acc
;;

let closure_converse stmts_list = converse_stms empty [] stmts_list

let closure_test expr =
  Format.printf
    "%a\n%!"
    (Ast.pp_statements_list (fun _ _ -> ()))
    (converse_stms empty [] expr)
;;

type expression =
  | BinOp of expression * expression * Ast.binop * Typing.ty (** x * y *)
  | IfThenElse of expression * expression * expression * Typing.ty
  (** if condition then expr1 else expr2 *)
  | Apply of Ast.name * expression * expression list * Typing.ty (** func arg1 arg2 ... *)
  | Variable of Ast.name * Typing.ty (** var *)
  | Value of Ast.const * Typing.ty (** value (const) *)
[@@deriving show { with_path = false }]

type 'expression var_decl = Ast.name * 'expression * Typing.ty
[@@deriving show { with_path = false }]

type 'expression statement =
  | Define of
      Ast.name * Ast.name list * 'expression var_decl list * 'expression * Typing.ty
  | RecDefine of
      Ast.name * Ast.name list * 'expression var_decl list * 'expression * Typing.ty
[@@deriving show { with_path = false }]

module IntState = struct
  include Monad.State (struct
      type t = int * expression statement list
    end)

  let ( let* ) = ( >>= )

  let get_counter =
    let* counter, _ = get in
    return counter
  ;;

  let modify_counter f =
    let* counter, lifted = get in
    put (f counter, lifted)
  ;;

  let fresh =
    let* counter = get_counter in
    let* () = modify_counter succ in
    return @@ String.concat "_" [ "__neinml"; "ll"; string_of_int counter ]
  ;;
end

let lift_lambda statements = failwith "undefined"

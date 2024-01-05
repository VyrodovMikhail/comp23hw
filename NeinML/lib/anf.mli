type imm_expr =
  | ImmVar of Ast.name * Typing.ty
  | ImmInt of int
  | ImmBool of bool
[@@deriving show { with_path = false }]

type cexpr =
  | CBinOp of imm_expr * imm_expr * Ast.binop * Typing.ty
  | CApply of imm_expr * imm_expr * imm_expr list * Typing.ty
  | CIfThenElse of
      imm_expr
      * (cexpr Lambda_lifting.var_decl list * cexpr)
      * (cexpr Lambda_lifting.var_decl list * cexpr)
      * Typing.ty
  | CImm of imm_expr
[@@deriving show { with_path = false }]

val make_anf
  :  Lambda_lifting.expression Lambda_lifting.statement list
  -> cexpr Lambda_lifting.statement list

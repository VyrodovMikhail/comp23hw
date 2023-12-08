type expression =
    BinOp of expression * expression * Ast.binop * Typing.ty
  | IfThenElse of expression * expression * expression * Typing.ty
  | Apply of string * expression * expression list * Typing.ty
  | Variable of string * Typing.ty
  | Value of Ast.const * Typing.ty
val pp_expression : Format.formatter -> expression -> unit
val show_expression : expression -> string

type 'expression var_decl = string * 'expression * Typing.ty
val pp_var_decl :
  (Format.formatter -> 'expression -> unit) ->
  Format.formatter -> 'expression var_decl -> unit
val show_var_decl :
  (Format.formatter -> 'expression -> unit) -> 'expression var_decl -> string

type 'expression statement =
    Define of string * string list * 'expression var_decl list *
      'expression * Typing.ty
  | RecDefine of string * string list * 'expression var_decl list *
      'expression * Typing.ty
val pp_statement :
  (Format.formatter -> 'expression -> unit) ->
  Format.formatter -> 'expression statement -> unit
val show_statement :
  (Format.formatter -> 'expression -> unit) ->
  'expression statement -> string

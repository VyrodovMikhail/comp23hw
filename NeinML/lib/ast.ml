(** Copyright 2023-2024, Mikhail Vyrodov and Vyacheslav Buchin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving eq, show { with_path = false }]

type const =
  | VInt of int
  | VBool of bool
[@@deriving show { with_path = false }]

type varname_part = VarPart of name [@@deriving show { with_path = false }]

type 'a expression =
  | Add of 'a expression * 'a expression * 'a
  | Sub of 'a expression * 'a expression * 'a
  (* | UnaryMin of expression
     | UnaryPlus of expression *)
  | Mul of 'a expression * 'a expression * 'a
  | Div of 'a expression * 'a expression * 'a
  | Mod of 'a expression * 'a expression * 'a
  | And of 'a expression * 'a expression * 'a
  | Or of 'a expression * 'a expression * 'a
  | Equal of 'a expression * 'a expression * 'a
  | NotEqual of 'a expression * 'a expression * 'a
  | Less of 'a expression * 'a expression * 'a
  | LessOrEq of 'a expression * 'a expression * 'a
  | More of 'a expression * 'a expression * 'a
  | MoreOrEq of 'a expression * 'a expression * 'a
  | LetIn of name * 'a expression * 'a expression * 'a
  | RecLetIn of name * 'a expression * 'a expression * 'a
  | IfThenElse of 'a expression * 'a expression * 'a expression * 'a
  | Func of name * 'a expression * 'a
  | Apply of 'a expression * 'a expression * 'a
  | Variable of name * 'a
  | Value of const * 'a
[@@deriving show { with_path = false }]

type 'a statement =
  | Define of name * 'a expression * 'a (** let var (arg1 arg2...) = <expression> *)
  | RecDefine of name * 'a expression * 'a
  (** let rec var (arg1 arg2...) = <expression> *)

and 'a statements_list = 'a statement list [@@deriving show { with_path = false }]

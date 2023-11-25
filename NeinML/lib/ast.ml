(** Copyright 2023-2024, Mikhail Vyrodov and Vyacheslav Buchin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving eq, show { with_path = false }]

type const =
  | VInt of int
  | VBool of bool
[@@deriving show { with_path = false }]

type varname_part = VarPart of name [@@deriving show { with_path = false }]

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Equal
  | NotEqual
  | Less
  | LessOrEq
  | More
  | MoreOrEq
[@@deriving show { with_path = false }]

type 'a expression =
  | BinOp of 'a expression * 'a expression * binop * 'a
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

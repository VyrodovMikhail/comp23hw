(** Copyright 2023-2024, Mikhail Vyrodov and Vyacheslav Buchin*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving eq, show { with_path = false }]

type const =
  | VInt of int (** int number *)
  | VBool of bool (** bool value *)
[@@deriving show { with_path = false }]

type varname_part = VarPart of name [@@deriving show { with_path = false }]

type 'a expression =
  | Add of 'a expression * 'a expression * 'a (** a + b *)
  | Sub of 'a expression * 'a expression * 'a (** a - b *)
  | Mul of 'a expression * 'a expression * 'a (** a * b *)
  | Div of 'a expression * 'a expression * 'a (** a / b *)
  | Mod of 'a expression * 'a expression * 'a (** a % b *)
  | And of 'a expression * 'a expression * 'a (** a && b *)
  | Or of 'a expression * 'a expression * 'a (** a || b *)
  | Equal of 'a expression * 'a expression * 'a (** a = b *)
  | NotEqual of 'a expression * 'a expression * 'a (** a <> b *)
  | Less of 'a expression * 'a expression * 'a (** a < b *)
  | LessOrEq of 'a expression * 'a expression * 'a (** a <= b *)
  | More of 'a expression * 'a expression * 'a (** a > b *)
  | MoreOrEq of 'a expression * 'a expression * 'a (** a >= b *)
  | LetIn of name * 'a expression * 'a expression * 'a (** let func = ... in ... *)
  | RecLetIn of name * 'a expression * 'a expression * 'a (** let rec func = ... in ... *)
  | IfThenElse of 'a expression * 'a expression * 'a expression * 'a
      (** if condition then expr1 else expr2 *)
  | Func of name * 'a expression * 'a (** ast 'a expression for defining functions with currying *)
  | Apply of 'a expression * 'a expression * 'a (** func arg1 arg2 ... *)
  | Variable of name * 'a (** var *)
  | Value of const * 'a (** value (const) *)
[@@deriving show { with_path = false }]

type 'a statement =
  | Define of name * 'a expression * 'a (** let var (arg1 arg2...) = <'a expression> *)
  | RecDefine of name * 'a expression * 'a (** let rec var (arg1 arg2...) = <'a expression> *)

and 'a statements_list = 'a statement list [@@deriving show { with_path = false }]

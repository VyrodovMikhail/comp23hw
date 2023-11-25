(** Copyright 2023-2024, Mikhail Vyrodov and Vyacheslav Buchin*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving eq, show { with_path = false }]

type const =
  | VInt of int (** int number *)
  | VBool of bool (** bool value *)
[@@deriving show { with_path = false }]

type varname_part = VarPart of name [@@deriving show { with_path = false }]

type binop = 
  | Add (** a + b *)
  | Sub (** a - b *)
  | Mul (** a * b *)
  | Div (** a / b *)
  | Mod (** a % b *)
  | And (** a && b *)
  | Or (** a || b *)
  | Equal (** a = b *)
  | NotEqual (** a <> b *)
  | Less (** a < b *)
  | LessOrEq (** a <= b *)
  | More (** a > b *)
  | MoreOrEq (** a >= b *)

type 'a expression =
  | BinOp of 'a expression * 'a expression * binop * 'a (** x * y *)
  | LetIn of name * 'a expression * 'a expression * 'a (** let func = ... in ... *)
  | RecLetIn of name * 'a expression * 'a expression * 'a (** let rec func = ... in ... *)
  | IfThenElse of 'a expression * 'a expression * 'a expression * 'a
      (** if condition then expr1 else expr2 *)
  | Func of name * 'a expression * 'a (** ast expression for defining functions with currying *)
  | Apply of 'a expression * 'a expression * 'a (** func arg1 arg2 ... *)
  | Variable of name * 'a (** var *)
  | Value of const * 'a (** value (const) *)
[@@deriving show { with_path = false }]

type 'a statement =
  | Define of name * 'a expression * 'a (** let var (arg1 arg2...) = <'a expression> *)
  | RecDefine of name * 'a expression * 'a (** let rec var (arg1 arg2...) = <'a expression> *)

and 'a statements_list = 'a statement list [@@deriving show { with_path = false }]

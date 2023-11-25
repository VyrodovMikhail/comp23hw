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
  | Lambda of 'a expression * 'a
  | Func of name * 'a expression * 'a
  | Apply of 'a expression * 'a expression * 'a
  | Variable of name * 'a
  | Value of const * 'a
[@@deriving show { with_path = false }]

type 'a statement =
  | Define of name * 'a expression * 'a
  | RecDefine of name * 'a expression * 'a

and 'a statements_list = 'a statement list [@@deriving show { with_path = false }]

let cval x typ = Value (x, typ)
let cvar x typ = Variable (x, typ)
let cbinop x y op typ = BinOp (x, y, op, typ)
let clam x typ = Lambda (x, typ)
let capply x y typ = Apply (x, y, typ)
let cfunc x y typ = Func (x, y, typ)
let cdef x y typ = Define (x, y, typ)
let crecdef x y typ = RecDefine (x, y, typ)
let cletin x y z typ = LetIn (x, y, z, typ)
let crecletin x y z typ = RecLetIn (x, y, z, typ)
let cifthenelse x y z typ = IfThenElse (x, y, z, typ)

(** Copyright 2023-2024, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving eq, show { with_path = false }]

type const =
  | VInt of int
  | VBool of bool
[@@deriving show { with_path = false }]

type varname_part = VarPart of name [@@deriving show { with_path = false }]

type expression =
  | Add of expression * expression
  | Sub of expression * expression
  (* | UnaryMin of expression
  | UnaryPlus of expression *)
  | Mul of expression * expression
  | Div of expression * expression
  | Mod of expression * expression
  | And of expression * expression
  | Or of expression * expression
  | Equal of expression * expression
  | NotEqual of expression * expression
  | Less of expression * expression
  | LessOrEq of expression * expression
  | More of expression * expression
  | MoreOrEq of expression * expression
  | LetIn of name * expression * expression
  | RecLetIn of name * expression * expression
  | IfThenElse of expression * expression * expression
  | Lambda of expression
  | Func of name * expression
  | Apply of expression * expression
  | Variable of name
  | Value of const
[@@deriving show { with_path = false }]

type statement =
  | Define of name * expression (** let var (arg1 arg2...) = <expression> *)
  | RecDefine of name * expression (** let rec var (arg1 arg2...) = <expression> *)

and statements_list = statement list [@@deriving show { with_path = false }]

let cval x = Value x
let cvar x = Variable x
let cadd x y = Add (x, y)
let csub x y = Sub (x, y)
let cmul x y = Mul (x, y)
let cdiv x y = Div (x, y)
let cmod x y = Mod (x, y)
let ceq x y = Equal (x, y)
let cmore x y = More (x, y)
let cmoreeq x y = MoreOrEq (x, y)
let cnoteq x y = NotEqual (x, y)
let cless x y = Less (x, y)
let clesseq x y = LessOrEq (x, y)
let cand x y = And (x, y)
let cor x y = Or (x, y)
let capply x y = Apply (x, y)
let cfunc x y = Func (x, y)
let cdef x y = Define (x, y)
let crecdef x y = RecDefine (x, y)
let cletin x y z = LetIn (x, y, z)
let crecletin x y z = RecLetIn (x, y, z)
let clam x = Lambda x
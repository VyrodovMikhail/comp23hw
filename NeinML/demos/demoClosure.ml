(** Copyright 2023-2024, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Neinml_lib

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Neinml_lib.Parser.parse s with
  | Result.Ok ast ->
    (match Neinml_lib.Inferencer.w_stms_list ast with
     | Result.Ok result ->
       let _, typ_ast = result in
       Format.printf
         "%a\n%!"
         (Ast.pp_statements_list Neinml_lib.Typing.pp_type)
         (Neinml_lib.Closure.closure_converse typ_ast)
     | Result.Error _ -> Format.printf "inferencer error")
  | Error _ -> Format.printf "Parsing error"
;;
(* Expected that inner lets should be lifted except for variable declarations *)
  $ ./demoLambda.exe <<-EOF
  > let val =
  >   let var = 42 in
  >   let f x = x in
  >   f var
  > EOF
  [(Define ("f", ["x"], [], (Variable ("x", (Ty_var 0))),
      (Arrow ((Ty_var 0), (Ty_var 0)))));
    (Define ("val", [],
       [("var", (Value ((VInt 42), (Prim TInt))), (Prim TInt))],
       (Apply ((Variable ("f", (Arrow ((Ty_var 0), (Ty_var 0))))),
          (Variable ("var", (Prim TInt))), [], (Prim TInt))),
       (Prim TInt)))
    ]

(* Result is:
let g uni0x = uni0x
let f x = let uni1x = g x in let val = uni1x in g x + val
*)
  $ ./demoLambda.exe <<-EOF
  > let f x =
  >   let g x = x in
  >   let val =
  >     let x = g x in
  >     x
  >   in
  >   g x + val
  > EOF
  [(Define ("g", ["__neinml_uni0x"], [],
      (Variable ("__neinml_uni0x", (Ty_var 1))),
      (Arrow ((Ty_var 1), (Ty_var 1)))));
    (Define ("f", ["x"],
       [("__neinml_uni1x",
         (Apply ((Variable ("g", (Arrow ((Prim TInt), (Prim TInt))))),
            (Variable ("x", (Prim TInt))), [], (Prim TInt))),
         (Prim TInt));
         ("val", (Variable ("__neinml_uni1x", (Prim TInt))), (Prim TInt))],
       (BinOp (
          (Apply ((Variable ("g", (Arrow ((Ty_var 1), (Ty_var 1))))),
             (Variable ("x", (Prim TInt))), [], (Prim TInt))),
          (Variable ("val", (Prim TInt))), Add, (Prim TInt))),
       (Arrow ((Prim TInt), (Prim TInt)))))
    ]

  $ ./demoLambda.exe <<-EOF
  > let f = (fun x ->
  >   let g x = x in
  >   (if x then g else (fun y -> y + 5)) 4
  > )
  > EOF
  [(Define ("g", ["__neinml_uni0x"], [],
      (Variable ("__neinml_uni0x", (Ty_var 1))),
      (Arrow ((Ty_var 1), (Ty_var 1)))));
    (Define ("__neinml_ll_0", ["y"], [],
       (BinOp ((Variable ("y", (Prim TInt))), (Value ((VInt 5), (Prim TInt))),
          Add, (Prim TInt))),
       (Arrow ((Prim TInt), (Prim TInt)))));
    (Define ("f", ["x"], [],
       (Apply (
          (IfThenElse ((Variable ("x", (Prim TBool))),
             (Variable ("g", (Arrow ((Ty_var 1), (Ty_var 1))))),
             (Variable ("__neinml_ll_0", (Arrow ((Prim TInt), (Prim TInt))))),
             (Arrow ((Prim TInt), (Prim TInt))))),
          (Value ((VInt 4), (Prim TInt))), [], (Prim TInt))),
       (Arrow ((Prim TBool), (Prim TInt)))))
    ]

  $ ./demoLambda.exe <<-EOF
  > let fac n =
  >   let rec fack n k =
  >   if n <= 1 then k 1 1
  >   else fack (n - 1) (fun m z -> k (m * n) z)
  >   in
  >   fack n (fun x y -> x)
  > EOF
  [(Define ("__neinml_ll_0", ["__neinml_uni0n"; "k"; "m"; "z"], [],
      (Apply (
         (Variable ("k",
            (Arrow ((Prim TInt), (Arrow ((Prim TInt), (Prim TInt))))))),
         (BinOp ((Variable ("m", (Prim TInt))),
            (Variable ("__neinml_uni0n", (Prim TInt))), Mul, (Prim TInt))),
         [(Variable ("z", (Prim TInt)))], (Prim TInt))),
      (Arrow ((Prim TInt),
         (Arrow ((Arrow ((Prim TInt), (Arrow ((Prim TInt), (Prim TInt))))),
            (Arrow ((Prim TInt), (Arrow ((Prim TInt), (Prim TInt)))))))
         ))
      ));
    (RecDefine ("fack", ["__neinml_uni0n"; "k"], [],
       (IfThenElse (
          (BinOp ((Variable ("__neinml_uni0n", (Prim TInt))),
             (Value ((VInt 1), (Prim TInt))), LessOrEq, (Prim TBool))),
          (Apply (
             (Variable ("k",
                (Arrow ((Prim TInt), (Arrow ((Prim TInt), (Prim TInt))))))),
             (Value ((VInt 1), (Prim TInt))),
             [(Value ((VInt 1), (Prim TInt)))], (Prim TInt))),
          (Apply (
             (Variable ("fack",
                (Arrow ((Prim TInt),
                   (Arrow (
                      (Arrow ((Prim TInt), (Arrow ((Prim TInt), (Prim TInt))))),
                      (Prim TInt)))
                   ))
                )),
             (BinOp ((Variable ("__neinml_uni0n", (Prim TInt))),
                (Value ((VInt 1), (Prim TInt))), Sub, (Prim TInt))),
             [(Apply (
                 (Variable ("__neinml_ll_0",
                    (Arrow ((Prim TInt),
                       (Arrow (
                          (Arrow ((Prim TInt),
                             (Arrow ((Prim TInt), (Prim TInt))))),
                          (Arrow ((Prim TInt),
                             (Arrow ((Prim TInt), (Prim TInt)))))
                          ))
                       ))
                    )),
                 (Variable ("__neinml_uni0n", (Prim TInt))),
                 [(Variable ("k",
                     (Arrow ((Prim TInt), (Arrow ((Prim TInt), (Prim TInt)))))
                     ))
                   ],
                 (Arrow ((Prim TInt), (Arrow ((Prim TInt), (Prim TInt)))))))
               ],
             (Prim TInt))),
          (Prim TInt))),
       (Arrow ((Prim TInt),
          (Arrow ((Arrow ((Prim TInt), (Arrow ((Prim TInt), (Prim TInt))))),
             (Prim TInt)))
          ))
       ));
    (Define ("__neinml_ll_1", ["x"; "y"], [], (Variable ("x", (Prim TInt))),
       (Arrow ((Prim TInt), (Arrow ((Prim TInt), (Prim TInt)))))));
    (Define ("fac", ["n"], [],
       (Apply (
          (Variable ("fack",
             (Arrow ((Prim TInt),
                (Arrow (
                   (Arrow ((Prim TInt), (Arrow ((Prim TInt), (Prim TInt))))),
                   (Prim TInt)))
                ))
             )),
          (Variable ("n", (Prim TInt))),
          [(Variable ("__neinml_ll_1",
              (Arrow ((Prim TInt), (Arrow ((Prim TInt), (Prim TInt)))))))
            ],
          (Prim TInt))),
       (Arrow ((Prim TInt), (Prim TInt)))))
    ]

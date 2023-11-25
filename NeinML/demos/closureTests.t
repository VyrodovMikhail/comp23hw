(* В результате получается:
let a c d =
let m = c + d in
let xx y = 1 + y in
let k m xx l = l + m + xx l in
k m xx (5 + m)
*)
  
  $ ./demoClosure.exe <<-EOF
  > let a c d = 
  > let m = c + d in
  > let xx y = 1 + y in
  > let k l = l + m + xx l in
  > k (5 + m)
  > EOF
  [(Define ("a",
      (Func ("c",
         (Func ("d",
            (LetIn ("m", (Add ((Variable "c"), (Variable "d"))),
               (LetIn ("xx",
                  (Func ("y", (Add ((Value (VInt 1)), (Variable "y"))))),
                  (LetIn ("k",
                     (Func ("m",
                        (Func ("xx",
                           (Func ("l",
                              (Add ((Variable "l"),
                                 (Add ((Variable "m"),
                                    (Apply ((Variable "xx"), (Variable "l")))))
                                 ))
                              ))
                           ))
                        )),
                     (Apply (
                        (Apply ((Apply ((Variable "k"), (Variable "m"))),
                           (Variable "xx"))),
                        (Add ((Value (VInt 5)), (Variable "m")))))
                     ))
                  ))
               ))
            ))
         ))
      ))
    ]


(* В результате получается:
let fac n =
let rec fack n k =
if n <= 1 then k 1 1
else fack (n - 1) ((fun k n m z -> k (m * n)) k n)
in
fack n (fun x y -> x)
*)

  $ ./demoClosure.exe <<-EOF
  > let fac n =
  >   let rec fack n k =
  >   if n <= 1 then k 1 1
  >   else fack (n - 1) (fun m z -> k (m * n))
  >   in
  > fack n (fun x y -> x)
  > EOF
  [(Define ("fac",
      (Func ("n",
         (RecLetIn ("fack",
            (Func ("n",
               (Func ("k",
                  (IfThenElse ((LessOrEq ((Variable "n"), (Value (VInt 1)))),
                     (Apply ((Apply ((Variable "k"), (Value (VInt 1)))),
                        (Value (VInt 1)))),
                     (Apply (
                        (Apply ((Variable "fack"),
                           (Sub ((Variable "n"), (Value (VInt 1)))))),
                        (Apply (
                           (Apply (
                              (Lambda
                                 (Func ("k",
                                    (Func ("n",
                                       (Func ("m",
                                          (Func ("z",
                                             (Apply ((Variable "k"),
                                                (Mul ((Variable "m"),
                                                   (Variable "n")))
                                                ))
                                             ))
                                          ))
                                       ))
                                    ))),
                              (Variable "k"))),
                           (Variable "n")))
                        ))
                     ))
                  ))
               )),
            (Apply ((Apply ((Variable "fack"), (Variable "n"))),
               (Lambda (Func ("x", (Func ("y", (Variable "x"))))))))
            ))
         ))
      ))
    ]

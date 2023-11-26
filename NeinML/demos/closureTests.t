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
            (LetIn ("m",
               (BinOp ((Variable ("c", )), (Variable ("d", )), Add, )),
               (LetIn ("xx",
                  (Func ("y",
                     (BinOp ((Value ((VInt 1), )), (Variable ("y", )), Add, )), 
                     )),
                  (LetIn ("k",
                     (Func ("m",
                        (Func ("xx",
                           (Func ("l",
                              (BinOp ((Variable ("l", )),
                                 (BinOp ((Variable ("m", )),
                                    (Apply ((Variable ("xx", )),
                                       (Variable ("l", )), )),
                                    Add, )),
                                 Add, )),
                              )),
                           )),
                        )),
                     (Apply (
                        (Apply (
                           (Apply ((Variable ("k", )), (Variable ("m", )), )),
                           (Variable ("xx", )), )),
                        (BinOp ((Value ((VInt 5), )), (Variable ("m", )), Add, 
                           )),
                        )),
                     )),
                  )),
               )),
            )),
         )),
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
                  (IfThenElse (
                     (BinOp ((Variable ("n", )), (Value ((VInt 1), )),
                        LessOrEq, )),
                     (Apply (
                        (Apply ((Variable ("k", )), (Value ((VInt 1), )), )),
                        (Value ((VInt 1), )), )),
                     (Apply (
                        (Apply ((Variable ("fack", )),
                           (BinOp ((Variable ("n", )), (Value ((VInt 1), )),
                              Sub, )),
                           )),
                        (Apply (
                           (Apply (
                              (Lambda (
                                 (Func ("k",
                                    (Func ("n",
                                       (Func ("m",
                                          (Func ("z",
                                             (Apply ((Variable ("k", )),
                                                (BinOp ((Variable ("m", )),
                                                   (Variable ("n", )), Mul, )),
                                                )),
                                             )),
                                          )),
                                       )),
                                    )),
                                 )),
                              (Variable ("k", )), )),
                           (Variable ("n", )), )),
                        )),
                     )),
                  )),
               )),
            (Apply ((Apply ((Variable ("fack", )), (Variable ("n", )), )),
               (Lambda ((Func ("x", (Func ("y", (Variable ("x", )), )), )), )), 
               )),
            )),
         )),
      ))
    ]



  $ ./demoClosure.exe <<-EOF
  > let f x =
  >   let g = 4 in
  >   let g y = x + y + g in
  >   g x
  > EOF
  kek



  $ ./demoClosure.exe <<-EOF
  > let f x =
  >   let rec g y =
  >      if y <= 1 then x 
  >      else
  >         x * y + g (y - 1)
  >   in g x
  > EOF
  kek

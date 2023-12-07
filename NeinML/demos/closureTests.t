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
               (BinOp ((Variable ("c", int)), (Variable ("d", int)), Add, int)),
               (LetIn ("xx",
                  (Func ("y",
                     (BinOp ((Value ((VInt 1), int)), (Variable ("y", int)),
                        Add, int)),
                     int -> int)),
                  (LetIn ("k",
                     (Func ("m",
                        (Func ("xx",
                           (Func ("l",
                              (BinOp ((Variable ("l", int)),
                                 (BinOp ((Variable ("m", int)),
                                    (Apply ((Variable ("xx", int -> int)),
                                       (Variable ("l", int)), int)),
                                    Add, int)),
                                 Add, int)),
                              int -> int)),
                           (int -> int) -> int -> int)),
                        int -> (int -> int) -> int -> int)),
                     (Apply (
                        (Apply (
                           (Apply (
                              (Variable ("k", int -> (int -> int) -> int -> int
                                 )),
                              (Variable ("m", int)), (int -> int) -> int -> int
                              )),
                           (Variable ("xx", int -> int)), int -> int)),
                        (BinOp ((Value ((VInt 5), int)), (Variable ("m", int)),
                           Add, int)),
                        int)),
                     int)),
                  int)),
               int)),
            int -> int)),
         int -> int -> int)),
      int -> int -> int))
    ]




(* В результате получается:
let fac n =
let rec fack n k =
if n <= 1 then k 1 1
else fack (n - 1) ((fun k n m z -> k (m * n) z) k n)
in
fack n (fun x y -> x)
*)

  $ ./demoClosure.exe <<-EOF
  > let fac n =
  >   let rec fack n k =
  >   if n <= 1 then k 1 1
  >   else fack (n - 1) (fun m z -> k (m * n) z)
  >   in
  > fack n (fun x y -> x)
  > EOF
  [(Define ("fac",
      (Func ("n",
         (RecLetIn ("fack",
            (Func ("__neinml_uni0n",
               (Func ("k",
                  (IfThenElse (
                     (BinOp ((Variable ("__neinml_uni0n", int)),
                        (Value ((VInt 1), int)), LessOrEq, bool)),
                     (Apply (
                        (Apply ((Variable ("k", int -> int -> int)),
                           (Value ((VInt 1), int)), int -> int)),
                        (Value ((VInt 1), int)), int)),
                     (Apply (
                        (Apply (
                           (Variable ("fack", int -> (int -> int -> int) -> int
                              )),
                           (BinOp ((Variable ("__neinml_uni0n", int)),
                              (Value ((VInt 1), int)), Sub, int)),
                           (int -> int -> int) -> int)),
                        (Apply (
                           (Apply (
                              (Func ("__neinml_uni0n",
                                 (Func ("k",
                                    (Func ("m",
                                       (Func ("z",
                                          (Apply (
                                             (Apply (
                                                (Variable ("k",
                                                   int -> int -> int)),
                                                (BinOp ((Variable ("m", int)),
                                                   (Variable ("__neinml_uni0n",
                                                      int)),
                                                   Mul, int)),
                                                int -> int)),
                                             (Variable ("z", int)), int)),
                                          int -> int)),
                                       int -> int -> int)),
                                    (int -> int -> int) -> int -> int -> int)),
                                 int -> (int -> int -> int) -> int -> int -> int
                                 )),
                              (Variable ("__neinml_uni0n", int)),
                              (int -> int -> int) -> int -> int -> int)),
                           (Variable ("k", int -> int -> int)),
                           int -> int -> int)),
                        int)),
                     int)),
                  (int -> int -> int) -> int)),
               int -> (int -> int -> int) -> int)),
            (Apply (
               (Apply ((Variable ("fack", int -> (int -> int -> int) -> int)),
                  (Variable ("n", int)), (int -> int -> int) -> int)),
               (Func ("x", (Func ("y", (Variable ("x", int)), int -> int)),
                  int -> int -> int)),
               int)),
            int)),
         int -> int)),
      int -> int))
    ]

  $ ./demoClosure.exe <<-EOF
  > let f x =
  >   let rec g y =
  >      if y <= 1 then x 
  >      else
  >         x * y + g (y - 1)
  >   in g x
  > EOF
  [(Define ("f",
      (Func ("x",
         (RecLetIn ("g",
            (Func ("x",
               (Func ("y",
                  (IfThenElse (
                     (BinOp ((Variable ("y", int)), (Value ((VInt 1), int)),
                        LessOrEq, bool)),
                     (Variable ("x", int)),
                     (BinOp (
                        (BinOp ((Variable ("x", int)), (Variable ("y", int)),
                           Mul, int)),
                        (Apply (
                           (Apply ((Variable ("g", int -> int -> int)),
                              (Variable ("x", int)), int -> int)),
                           (BinOp ((Variable ("y", int)),
                              (Value ((VInt 1), int)), Sub, int)),
                           int)),
                        Add, int)),
                     int)),
                  int -> int)),
               int -> int -> int)),
            (Apply (
               (Apply ((Variable ("g", int -> int -> int)),
                  (Variable ("x", int)), int -> int)),
               (Variable ("x", int)), int)),
            int)),
         int -> int)),
      int -> int))
    ]

  $ ./demoClosure.exe <<-EOF
  > let f x =
  >   let g = 4 in
  >   let g y = x + y + g in
  >   g x
  > EOF
  [(Define ("f",
      (Func ("x",
         (LetIn ("g", (Value ((VInt 4), int)),
            (LetIn ("__neinml_uni0g",
               (Func ("g",
                  (Func ("x",
                     (Func ("y",
                        (BinOp ((Variable ("x", int)),
                           (BinOp ((Variable ("y", int)),
                              (Variable ("g", int)), Add, int)),
                           Add, int)),
                        int -> int)),
                     int -> int -> int)),
                  int -> int -> int -> int)),
               (Apply (
                  (Apply (
                     (Apply (
                        (Variable ("__neinml_uni0g", int -> int -> int -> int)),
                        (Variable ("g", int)), int -> int -> int)),
                     (Variable ("x", int)), int -> int)),
                  (Variable ("x", int)), int)),
               int)),
            int)),
         int -> int)),
      int -> int))
    ]

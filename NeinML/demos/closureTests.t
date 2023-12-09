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

получается
let func1 x y z = x + y + z
let func2 __neinml_apply0 __neinml_apply1 = func1 4 __neinml_apply0 __neinml_apply1
let func3 __neinml_apply0 = func2 5 __neinml_apply0

  $ ./demoClosure.exe <<-EOF
  > let func1 x y z = x + y + z
  > let func2 = func1 4
  > let func3 = func2 5
  > EOF
  [(Define ("func1",
      (Func ("x",
         (Func ("y",
            (Func ("z",
               (BinOp ((Variable ("x", int)),
                  (BinOp ((Variable ("y", int)), (Variable ("z", int)), Add,
                     int)),
                  Add, int)),
               int -> int)),
            int -> int -> int)),
         int -> int -> int -> int)),
      int -> int -> int -> int));
    (Define ("func2",
       (Func ("__neinml_apply_0",
          (Func ("__neinml_apply_1",
             (Apply (
                (Apply (
                   (Apply ((Variable ("func1", int -> int -> int -> int)),
                      (Value ((VInt 4), int)), int -> int -> int)),
                   (Variable ("__neinml_apply_0", int)), int -> int)),
                (Variable ("__neinml_apply_1", int)), int)),
             int -> int)),
          int -> int -> int)),
       int -> int -> int));
    (Define ("func3",
       (Func ("__neinml_apply_0",
          (Apply (
             (Apply ((Variable ("func2", int -> int -> int)),
                (Value ((VInt 5), int)), int -> int)),
             (Variable ("__neinml_apply_0", int)), int)),
          int -> int)),
       int -> int))
    ]

получается
let gg x = 
let func1 __neinml_uni0x y z = __neinml_uni0x + y + z in
let func2 func1 __neinml_apply0 __neinml_apply1 = func1 4 __neinml_apply0 __neinml_apply1
func2 func1 5 x


  $ ./demoClosure.exe <<-EOF
  > let gg x =
  > let func1 x y z = x + y + z in
  > let func2 = func1 4 in
  > func2 5 x
  > EOF
  [(Define ("gg",
      (Func ("x",
         (LetIn ("func1",
            (Func ("__neinml_uni0x",
               (Func ("y",
                  (Func ("z",
                     (BinOp ((Variable ("__neinml_uni0x", int)),
                        (BinOp ((Variable ("y", int)), (Variable ("z", int)),
                           Add, int)),
                        Add, int)),
                     int -> int)),
                  int -> int -> int)),
               int -> int -> int -> int)),
            (LetIn ("func2",
               (Func ("func1",
                  (Func ("__neinml_apply_0",
                     (Func ("__neinml_apply_1",
                        (Apply (
                           (Apply (
                              (Apply (
                                 (Variable ("func1", int -> int -> int -> int)),
                                 (Value ((VInt 4), int)), int -> int -> int)),
                              (Variable ("__neinml_apply_0", int)), int -> int
                              )),
                           (Variable ("__neinml_apply_1", int)), int)),
                        int -> int)),
                     int -> int -> int)),
                  (int -> int -> int -> int) -> int -> int -> int)),
               (Apply (
                  (Apply (
                     (Apply (
                        (Variable ("func2",
                           (int -> int -> int -> int) -> int -> int -> int)),
                        (Variable ("func1", int -> int -> int -> int)),
                        int -> int -> int)),
                     (Value ((VInt 5), int)), int -> int)),
                  (Variable ("x", int)), int)),
               int)),
            int)),
         int -> int)),
      int -> int))
    ]

let func1 x y z = x + y + z
let func2 x y z = x - y - z
let func3 __neinml_apply0 __neinml_apply1 = 
if 1 > 2 then func1 5 __neinml_apply0 __neinml_apply1 
else func2 6 __neinml_apply0 __neinml_apply1

  $ ./demoClosure.exe <<-EOF
  > let func1 x y z = x + y + z
  > let func2 x y z = x - y - z
  > let func3 = if 1 > 2 then func1 5 else func2 6
  > EOF
  [(Define ("func1",
      (Func ("x",
         (Func ("y",
            (Func ("z",
               (BinOp ((Variable ("x", int)),
                  (BinOp ((Variable ("y", int)), (Variable ("z", int)), Add,
                     int)),
                  Add, int)),
               int -> int)),
            int -> int -> int)),
         int -> int -> int -> int)),
      int -> int -> int -> int));
    (Define ("func2",
       (Func ("__neinml_uni0x",
          (Func ("__neinml_uni1y",
             (Func ("__neinml_uni2z",
                (BinOp ((Variable ("__neinml_uni0x", int)),
                   (BinOp ((Variable ("__neinml_uni1y", int)),
                      (Variable ("__neinml_uni2z", int)), Sub, int)),
                   Sub, int)),
                int -> int)),
             int -> int -> int)),
          int -> int -> int -> int)),
       int -> int -> int -> int));
    (Define ("func3",
       (Func ("__neinml_apply_0",
          (Func ("__neinml_apply_1",
             (IfThenElse (
                (BinOp ((Value ((VInt 1), int)), (Value ((VInt 2), int)), More,
                   bool)),
                (Apply (
                   (Apply (
                      (Apply ((Variable ("func1", int -> int -> int -> int)),
                         (Value ((VInt 5), int)), int -> int -> int)),
                      (Variable ("__neinml_apply_0", int)), int -> int)),
                   (Variable ("__neinml_apply_1", int)), int)),
                (Apply (
                   (Apply (
                      (Apply ((Variable ("func2", int -> int -> int -> int)),
                         (Value ((VInt 6), int)), int -> int -> int)),
                      (Variable ("__neinml_apply_0", int)), int -> int)),
                   (Variable ("__neinml_apply_1", int)), int)),
                int)),
             int -> int)),
          int -> int -> int)),
       int -> int -> int))
    ]

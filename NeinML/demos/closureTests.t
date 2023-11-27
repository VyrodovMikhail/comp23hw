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
               (BinOp ((Variable ("c", 'var0)), (Variable ("d", 'var1)), Add,
                  int)),
               (LetIn ("xx",
                  (Func ("y",
                     (BinOp ((Value ((VInt 1), int)), (Variable ("y", 'var3)),
                        Add, int)),
                     int -> int)),
                  (LetIn ("k",
                     (Func ("m",
                        (Func ("xx",
                           (Func ("l",
                              (BinOp ((Variable ("l", 'var5)),
                                 (BinOp ((Variable ("m", int)),
                                    (Apply ((Variable ("xx", int -> int)),
                                       (Variable ("l", 'var5)), int)),
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
else fack (n - 1) ((fun k n m z -> k (m * n)) k n)
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
                     (BinOp ((Variable ("__neinml_uni0n", 'var2)),
                        (Value ((VInt 1), int)), LessOrEq, bool)),
                     (Apply (
                        (Apply ((Variable ("k", 'var3)),
                           (Value ((VInt 1), int)), 'var5)),
                        (Value ((VInt 1), int)), 'var6)),
                     (Apply (
                        (Apply (
                           (Variable ("fack",
                              int -> (int -> int -> 'var14) -> 'var14)),
                           (BinOp ((Variable ("__neinml_uni0n", 'var2)),
                              (Value ((VInt 1), int)), Sub, int)),
                           (int -> int -> 'var14) -> 'var14)),
                        (Apply (
                           (Apply (
                              (Func ("__neinml_uni0n",
                                 (Func ("k",
                                    (Func ("m",
                                       (Func ("z",
                                          (Apply (
                                             (Apply ((Variable ("k", 'var3)),
                                                (BinOp (
                                                   (Variable ("m", 'var9)),
                                                   (Variable ("__neinml_uni0n",
                                                      int)),
                                                   Mul, int)),
                                                'var12)),
                                             (Variable ("z", 'var10)), 'var13)),
                                          'var10 -> 'var13)),
                                       int -> 'var10 -> 'var13)),
                                    'var3 -> int -> 'var10 -> 'var13)),
                                 int -> 'var3 -> int -> 'var10 -> 'var13)),
                              (Variable ("__neinml_uni0n", int)),
                              'var3 -> int -> 'var10 -> 'var13)),
                           (Variable ("k", 'var3)), int -> 'var10 -> 'var13)),
                        'var14)),
                     'var14)),
                  (int -> int -> 'var14) -> 'var14)),
               int -> (int -> int -> 'var14) -> 'var14)),
            (Apply (
               (Apply (
                  (Variable ("fack", int -> (int -> int -> 'var14) -> 'var14)),
                  (Variable ("n", 'var0)), (int -> int -> 'var14) -> 'var14)),
               (Func ("x",
                  (Func ("y", (Variable ("x", 'var16)), 'var17 -> 'var16)),
                  'var16 -> 'var17 -> 'var16)),
               int)),
            int)),
         int -> int)),
      int -> int))
    ]


тут ошибка в моей программе
g должно иметь тип int -> int -> int, но
closure conversion определяет, что у переменной x в функции g тип 'var0.
Это потому что в изначальномм typed ast у неё и есть тип 'var0, но потом
когда уже происходит apply, переменная x становится интом, а в моём алгоритме
она сохраняет такой же тип в apply, который у функции в definition.
Так что вот проблемка, пока не знаю, как решить...
Можно тупо переделать это аст в unit ast и снова прогнать через тайпчекер, чтобы
типы правильные были, но это как-то слишком тупо.

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
                     (BinOp ((Variable ("y", 'var2)), (Value ((VInt 1), int)),
                        LessOrEq, bool)),
                     (Variable ("x", 'var0)),
                     (BinOp (
                        (BinOp ((Variable ("x", 'var0)),
                           (Variable ("y", 'var2)), Mul, int)),
                        (Apply (
                           (Apply ((Variable ("g", 'var0 -> int -> int)),
                              (Variable ("x", 'var0)), int -> int)),
                           (BinOp ((Variable ("y", 'var2)),
                              (Value ((VInt 1), int)), Sub, int)),
                           int)),
                        Add, int)),
                     int)),
                  int -> int)),
               'var0 -> int -> int)),
            (Apply (
               (Apply ((Variable ("g", 'var0 -> int -> int)),
                  (Variable ("x", int)), int -> int)),
               (Variable ("x", int)), int)),
            int)),
         int -> int)),
      int -> int))
    ]


тут тоже с типом x проблема

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
                        (BinOp ((Variable ("x", 'var0)),
                           (BinOp ((Variable ("y", 'var1)),
                              (Variable ("g", int)), Add, int)),
                           Add, int)),
                        int -> int)),
                     'var0 -> int -> int)),
                  int -> 'var0 -> int -> int)),
               (Apply (
                  (Apply (
                     (Apply (
                        (Variable ("__neinml_uni0g", int -> 'var0 -> int -> int
                           )),
                        (Variable ("g", int)), 'var0 -> int -> int)),
                     (Variable ("x", int)), int -> int)),
                  (Variable ("x", int)), int)),
               int)),
            int)),
         int -> int)),
      int -> int))
    ]

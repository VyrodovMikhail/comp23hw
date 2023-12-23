  $ ./demoAnf.exe <<-EOF
  > let condition x = if x % 2 = 0 then true else false
  > let test x = if condition x then x else x * 2
  > EOF
  [(Define ("condition", ["x"],
      [("__neinml_anf_2",
        (CBinOp ((ImmVar ("x", (Prim TInt))), (ImmInt 2), Mod, (Prim TInt))),
        (Prim TInt));
        ("__neinml_anf_1",
         (CBinOp ((ImmVar ("__neinml_anf_2", (Prim TInt))), (ImmInt 0), Equal,
            (Prim TBool))),
         (Prim TBool));
        ("__neinml_anf_0",
         (CIfThenElse ((ImmVar ("__neinml_anf_1", (Prim TBool))),
            (ImmBool true), (ImmBool false), (Prim TBool))),
         (Prim TBool))
        ],
      (CImm (ImmVar ("__neinml_anf_0", (Prim TBool)))),
      (Arrow ((Prim TInt), (Prim TBool)))));
    (Define ("test", ["__neinml_uni0x"],
       [("__neinml_anf_2",
         (CBinOp ((ImmVar ("__neinml_uni0x", (Prim TInt))), (ImmInt 2), Mul,
            (Prim TInt))),
         (Prim TInt));
         ("__neinml_anf_1",
          (CApply ((ImmVar ("condition", (Arrow ((Prim TInt), (Prim TBool))))),
             (ImmVar ("__neinml_uni0x", (Prim TInt))), [], (Prim TBool))),
          (Prim TBool));
         ("__neinml_anf_0",
          (CIfThenElse ((ImmVar ("__neinml_anf_1", (Prim TBool))),
             (ImmVar ("__neinml_uni0x", (Prim TInt))),
             (ImmVar ("__neinml_anf_2", (Prim TInt))), (Prim TInt))),
          (Prim TInt))
         ],
       (CImm (ImmVar ("__neinml_anf_0", (Prim TInt)))),
       (Arrow ((Prim TInt), (Prim TInt)))))
    ]

  $ ./demoAnf.exe <<-EOF
  > let rec fac n =
  > let aboba = 1 + 5 * 7 in
  > let biba = aboba + n in
  > if n = 1 then 1 else n * fac (n - 1)
  > EOF
  [(RecDefine ("fac", ["n"],
      [("__neinml_anf_6", (CBinOp ((ImmInt 5), (ImmInt 7), Mul, (Prim TInt))),
        (Prim TInt));
        ("__neinml_anf_5",
         (CBinOp ((ImmInt 1), (ImmVar ("__neinml_anf_6", (Prim TInt))), Add,
            (Prim TInt))),
         (Prim TInt));
        ("aboba", (CImm (ImmVar ("__neinml_anf_5", (Prim TInt)))), (Prim TInt));
        ("__neinml_anf_7",
         (CBinOp ((ImmVar ("aboba", (Prim TInt))), (ImmVar ("n", (Prim TInt))),
            Add, (Prim TInt))),
         (Prim TInt));
        ("biba", (CImm (ImmVar ("__neinml_anf_7", (Prim TInt)))), (Prim TInt));
        ("__neinml_anf_4",
         (CBinOp ((ImmVar ("n", (Prim TInt))), (ImmInt 1), Sub, (Prim TInt))),
         (Prim TInt));
        ("__neinml_anf_3",
         (CApply ((ImmVar ("fac", (Arrow ((Prim TInt), (Prim TInt))))),
            (ImmVar ("__neinml_anf_4", (Prim TInt))), [], (Prim TInt))),
         (Prim TInt));
        ("__neinml_anf_2",
         (CBinOp ((ImmVar ("n", (Prim TInt))),
            (ImmVar ("__neinml_anf_3", (Prim TInt))), Mul, (Prim TInt))),
         (Prim TInt));
        ("__neinml_anf_1",
         (CBinOp ((ImmVar ("n", (Prim TInt))), (ImmInt 1), Equal, (Prim TBool)
            )),
         (Prim TBool));
        ("__neinml_anf_0",
         (CIfThenElse ((ImmVar ("__neinml_anf_1", (Prim TBool))), (ImmInt 1),
            (ImmVar ("__neinml_anf_2", (Prim TInt))), (Prim TInt))),
         (Prim TInt))
        ],
      (CImm (ImmVar ("__neinml_anf_0", (Prim TInt)))),
      (Arrow ((Prim TInt), (Prim TInt)))))
    ]

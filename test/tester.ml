open OUnit2
open Ast
open Token
open Interpreter

let suite =
  "suite" >::: ["eval test" >::: (List.map
                                    (fun (title,res,arg) ->
                                     title >::
                                       (fun test_ctxt ->
                                        assert_equal ~printer:Interpreter.val_to_str
                                          res (Interpreter.eval arg)))

                                    ["Interpreter 1", IntVal 3,
                                     (VerDecl ("x", IntLiteral 3, None));
                                     "Interpreter 2", IntVal 6,
                                     (VerDecl ("x", IntLiteral 3, (Some (AddIntExpr(Id "x", IntLiteral 3)))));
                                     "Interpreter 3", IntVal 13,
                                     (VerDecl
                                        ("x", IntLiteral 3,
                                         (Some (VerDecl
                                                  ("y", IntLiteral 5,
                                                   (Some (VerDecl
                                                            ("x", IntLiteral 8,
                                                             (Some (AddIntExpr (Id "x", Id "y")))))))))));
                                     "Interpreter 4", IntVal 5,
                                     (AddIntExpr (IntLiteral 3, IntLiteral 2));
                                     "Interpreter 5", IntVal 8,
                                     (AddIntExpr (AddIntExpr (IntLiteral 3, IntLiteral 3), IntLiteral 2));
                                     "Interpreter 6", IntVal 1,
                                     (SubIntExpr (IntLiteral 3, IntLiteral 2));
                                     "Interpreter 7", IntVal 6,
                                     (MulIntExpr (IntLiteral 3, IntLiteral 2));
                                     "Interpreter 8", IntVal 4,
                                     (SubIntExpr (MulIntExpr (IntLiteral 3, IntLiteral 2), IntLiteral 2));
                                     "Interpreter 9", IntVal 5,
                                     (DivIntExpr (IntLiteral 10, IntLiteral 2));
                                     "Interpreter 10", FloatVal 5.,
                                     (AddFloatExpr (FloatLiteral 3.0, FloatLiteral 2.0));
                                     "Interpreter 11", FloatVal 8.,
                                     (AddFloatExpr (AddFloatExpr(FloatLiteral 3., FloatLiteral 3.),
                                                    FloatLiteral 2.));
                                     "Interpreter 12", FloatVal 1.,
                                     (SubFloatExpr (FloatLiteral 3., FloatLiteral 2.));
                                     "Interpreter 13", FloatVal 6.,
                                     (MulFloatExpr (FloatLiteral 3., FloatLiteral 2.));
                                     "Interpreter 14", FloatVal 5.,
                                     (DivFloatExpr (FloatLiteral 10., FloatLiteral 2.));
                                     "Interpreter 15", BoolVal true,
                                     (EqualExpr (IntLiteral 3, IntLiteral 3));
                                     "Interpreter 16", BoolVal false,
                                     (EqualExpr (IntLiteral 3, IntLiteral 2));
                                     "Interpreter 17", BoolVal true,
                                     (EqualExpr (FloatLiteral 3., FloatLiteral 3.));
                                     "Interpreter 18", BoolVal false,
                                     (EqualExpr (FloatLiteral 3., FloatLiteral 2.));
                                     "Interpreter 19", BoolVal false,
                                     (EqualExpr (BoolLiteral true, BoolLiteral false));
                                     "Interpreter 20", BoolVal true,
                                     (EqualExpr (BoolLiteral true, BoolLiteral true));
                                     "Interpreter 21", BoolVal false,
                                     (NotEqualExpr (IntLiteral 3, IntLiteral 3));
                                     "Interpreter 22", BoolVal false,
                                     (NotEqualExpr (IntLiteral 3, IntLiteral 3));
                                     "Interpreter 23", BoolVal true,
                                     (NotEqualExpr (IntLiteral 3, IntLiteral 2));
                                     "Interpreter 24", BoolVal false,
                                     (NotEqualExpr (FloatLiteral 3., FloatLiteral 3.));
                                     "Interpreter 25", BoolVal true,
                                     (NotEqualExpr (FloatLiteral 3., FloatLiteral 2.));
                                     "Interpreter 26", BoolVal true,
                                     (NotEqualExpr (BoolLiteral true, BoolLiteral false));
                                     "Interpreter 26", BoolVal false,
                                     (NotEqualExpr (BoolLiteral true, BoolLiteral true));
                                     "Interpreter 27", BoolVal true ,
                                     (LessExpr (IntLiteral 3, IntLiteral 5));
                                     "Interpreter 28", BoolVal false,
                                     (LessExpr (IntLiteral 5, IntLiteral 3));
                                     "Interpreter 29", BoolVal true,
                                     (LessExpr (BoolLiteral false, BoolLiteral true));
                                     "Interpreter 30", BoolVal true,
                                     (LessEqualExpr (IntLiteral 3, IntLiteral 3));
                                     "Interpreter 31", BoolVal true,
                                     (LessEqualExpr (IntLiteral 3, IntLiteral 3));
                                     "Interpreter 32", BoolVal true,
                                     (LessEqualExpr (IntLiteral 3, IntLiteral 5));
                                     "Interpreter 33", BoolVal false,
                                     (LessEqualExpr (IntLiteral 5, IntLiteral 3));
                                     "Interpreter 34", BoolVal true,
                                     (GreaterExpr (IntLiteral 5, IntLiteral 3));
                                     "Interpreter 35", BoolVal false,
                                     (GreaterExpr (IntLiteral 3, IntLiteral 5));
                                     "Interpreter 36", BoolVal true,
                                     (GreaterExpr (BoolLiteral true, BoolLiteral false));
                                     "Interpreter 37", BoolVal true,
                                     (GreaterEqualExpr (IntLiteral 3, IntLiteral 3));
                                     "Interpreter 38", BoolVal true,
                                     (GreaterEqualExpr (IntLiteral 5, IntLiteral 3));
                                     "Interpreter 39", BoolVal true,
                                     (GreaterEqualExpr (IntLiteral 5, IntLiteral 3));
                                     "Interpreter 40", BoolVal false,
                                     (GreaterEqualExpr (IntLiteral 3, IntLiteral 5));
                                     "Interpreter 41", BoolVal true,
                                     (LogicOrExpr (BoolLiteral true, BoolLiteral true));
                                     "Interpreter 42", BoolVal true,
                                     (LogicOrExpr (BoolLiteral false, BoolLiteral true));
                                     "Interpreter 43", BoolVal true,
                                     (LogicAndExpr (BoolLiteral true, BoolLiteral true));
                                     "Interpreter 44", BoolVal false,
                                     (LogicAndExpr (BoolLiteral false, BoolLiteral true));
                                     "Interpreter 45", IntVal 6,
                                     (CondExpr (EqualExpr (IntLiteral 3, IntLiteral 3),
                                                (MulIntExpr (IntLiteral 2, IntLiteral 3)),
                                                (DivIntExpr (IntLiteral 10, IntLiteral 2))));
                                     "Interpreter 46", IntVal 5,
                                     (CondExpr (GreaterExpr (IntLiteral 3, IntLiteral 3),
                                                (MulIntExpr (IntLiteral 2, IntLiteral 3)),
                                                (DivIntExpr (IntLiteral 10, IntLiteral 2))));
                                     "Interpreter 46", IntVal 7,
                                     (AddIntExpr (CondExpr (EqualExpr (IntLiteral 3, IntLiteral 3),
                                                            (MulIntExpr (IntLiteral 2, IntLiteral 2)),
                                                            (DivIntExpr (IntLiteral 10, IntLiteral 2))),
                                                  IntLiteral 3));
                                     "Interpreter 47", IntVal 8,
                                     (AddIntExpr (CondExpr (EqualExpr (IntLiteral 3, IntLiteral 2),
                                                            (MulIntExpr (IntLiteral 2, IntLiteral 2)),
                                                            (DivIntExpr (IntLiteral 10, IntLiteral 2))),
                                                  IntLiteral 3));
                                    ]);
                "lexer test" >::: (List.map
                                     (fun (title,res,arg) ->
                                      title >::
                                        (fun test_ctxt ->
                                         assert_equal
                                           res
                                           (Lexer.lex (Lexer.char_list_of_string arg))))
                                     ["Lexer 1", [IntLiteral 1; Op AddInt; IntLiteral 1],
                                      "1 + 1";
                                      "Lexer 2", [Keyword Let; Identifier "x"; Op Assign; IntLiteral 3],
                                      "let x = 3";
                                     ]);
                (* "perser test"; *)
                (* "analyze test"; *)
                (* "codegen test"; *)
               ]

let run_test = run_test_tt_main suite

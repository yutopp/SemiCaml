open OUnit2
open Ast
open Token
open Interpreter
open Analyzer

let suite =
  "suite" >::: ["eval test" >::: (List.map
                                      (fun (title,res,arg) ->
                                       title >::
                                         (fun test_ctxt ->
                                          assert_equal
                                            ~printer:Interpreter.val_to_str
                                            res
                                            (Interpreter.eval arg)))
                                      ["eval 1", IntVal 3,
                                       (Program [IntLiteral 3]);
                                       "eval 2", IntVal 5,
                                       (Program [AddIntExpr (IntLiteral 2,IntLiteral 3)]);
                                       "eval 3", IntVal 10,
                                       (Program [AddIntExpr (IntLiteral 3,AddIntExpr(IntLiteral 2,IntLiteral 5))]);
                                       "eval 4", IntVal 3,
                                       (Program [SubIntExpr (IntLiteral 5,IntLiteral 2)]);
                                       "eval 5", IntVal 6,
                                       (Program [MulIntExpr (IntLiteral 2,IntLiteral 3)]);
                                       "eval 6", IntVal 2,
                                       (Program [DivIntExpr (IntLiteral 10,IntLiteral 5)]);
                                       "eval 7", FloatVal 5.,
                                       (Program [AddFloatExpr (FloatLiteral 2.,FloatLiteral 3.)]);
                                       "eval 8", FloatVal 7.,
                                       (Program [AddFloatExpr (FloatLiteral 3.,AddFloatExpr(FloatLiteral 3.,FloatLiteral 1.))]);
                                       "eval 9", FloatVal 2.,
                                       (Program [SubFloatExpr (FloatLiteral 5.,FloatLiteral 3.)]);
                                       "eval 10", FloatVal 6.,
                                       (Program [MulFloatExpr (FloatLiteral 2.,FloatLiteral 3.)]);
                                       "eval 11", FloatVal 2.,
                                       (Program [DivFloatExpr (FloatLiteral 6.,FloatLiteral 3.)]);
                                       "eval 12", BoolVal true,
                                       (Program [EqualExpr (IntLiteral 3, IntLiteral 3)]);
                                       "eval 16", BoolVal false,
                                       (Program [EqualExpr (IntLiteral 3, IntLiteral 2)]);
                                       "eval 17", BoolVal true,
                                       (Program [EqualExpr (FloatLiteral 3., FloatLiteral 3.)]);
                                       "eval 18", BoolVal false,
                                       (Program [EqualExpr (FloatLiteral 3., FloatLiteral 2.)]);
                                       (* "eval 19", BoolVal false, *)
                                       (* (Program [EqualExpr (BoolLiteral true, BoolLiteral false)]); *)
                                       (* "eval 20", BoolVal true, *)
                                       (* (Program [EqualExpr (BoolLiteral true, BoolLiteral true)]); *)
                                       "eval 21", BoolVal false,
                                       (Program [NotEqualExpr (IntLiteral 3, IntLiteral 3)]);
                                       "eval 22", BoolVal false,
                                       (Program [NotEqualExpr (IntLiteral 3, IntLiteral 3)]);
                                       "eval 23", BoolVal true,
                                       (Program [NotEqualExpr (IntLiteral 3, IntLiteral 2)]);
                                       "eval 24", BoolVal false,
                                       (Program [NotEqualExpr (FloatLiteral 3., FloatLiteral 3.)]);
                                       "eval 25", BoolVal true,
                                       (Program [NotEqualExpr (FloatLiteral 3., FloatLiteral 2.)]);
                                       (* "eval 26", BoolVal true, *)
                                       (* (Program [NotEqualExpr (BoolLiteral true, BoolLiteral false)]); *)
                                       (* "eval 27", BoolVal false, *)
                                       (* (Program [NotEqualExpr (BoolLiteral true, BoolLiteral true)]); *)
                                       "eval 28", BoolVal true ,
                                       (Program [LessExpr (IntLiteral 3, IntLiteral 5)]);
                                       "eval 29", BoolVal false,
                                       (Program [LessExpr (IntLiteral 5, IntLiteral 3)]);
                                       (* "eval 30", BoolVal true, *)
                                       (* (Program [LessExpr (BoolLiteral false, BoolLiteral true)]); *)
                                       "eval 31", BoolVal true,
                                       (Program [LessEqualExpr (IntLiteral 3, IntLiteral 3)]);
                                       "eval 32", BoolVal true,
                                       (Program [LessEqualExpr (IntLiteral 3, IntLiteral 3)]);
                                       "eval 33", BoolVal true,
                                       (Program [LessEqualExpr (IntLiteral 3, IntLiteral 5)]);
                                       "eval 34", BoolVal false,
                                       (Program [LessEqualExpr (IntLiteral 5, IntLiteral 3)]);
                                       "eval 35", BoolVal true,
                                       (Program [GreaterExpr (IntLiteral 5, IntLiteral 3)]);
                                       "eval 36", BoolVal false,
                                       (Program [GreaterExpr (IntLiteral 3, IntLiteral 5)]);
                                       (* "eval 37", BoolVal true, *)
                                       (* (Program [GreaterExpr (BoolLiteral true, BoolLiteral false)]); *)
                                       "eval 38", BoolVal true,
                                       (Program [GreaterEqualExpr (IntLiteral 3, IntLiteral 3)]);
                                       "eval 39", BoolVal true,
                                       (Program [GreaterEqualExpr (IntLiteral 5, IntLiteral 3)]);
                                       "eval 40", BoolVal true,
                                       (Program [GreaterEqualExpr (IntLiteral 5, IntLiteral 3)]);
                                       "eval 41", BoolVal false,
                                       (Program [GreaterEqualExpr (IntLiteral 3, IntLiteral 5)]);
                                       "eval 42", BoolVal true,
                                       (Program [LogicOrExpr (BoolLiteral true, BoolLiteral true)]);
                                       "eval 43", BoolVal true,
                                       (Program [LogicOrExpr (BoolLiteral false, BoolLiteral true)]);
                                       "eval 44", BoolVal true,
                                       (Program [LogicAndExpr (BoolLiteral true, BoolLiteral true)]);
                                       "eval 45", BoolVal false,
                                       (Program [LogicAndExpr (BoolLiteral false, BoolLiteral true)]);
                                       "eval 46", IntVal 6,
                                       (Program [CondExpr (EqualExpr (IntLiteral 3, IntLiteral 3),
                                                           (MulIntExpr (IntLiteral 2, IntLiteral 3)),
                                                           (DivIntExpr (IntLiteral 10, IntLiteral 2)))]);
                                       "eval 47", IntVal 5,
                                       (Program [CondExpr (GreaterExpr (IntLiteral 3, IntLiteral 3),
                                                           (MulIntExpr (IntLiteral 2, IntLiteral 3)),
                                                           (DivIntExpr (IntLiteral 10, IntLiteral 2)))]);
                                       "eval 48", IntVal 7,
                                       (Program [AddIntExpr (CondExpr (EqualExpr (IntLiteral 3, IntLiteral 3),
                                                                       (MulIntExpr (IntLiteral 2, IntLiteral 2)),
                                                                       (DivIntExpr (IntLiteral 10, IntLiteral 2))),
                                                             IntLiteral 3)]);
                                       "eval 47", IntVal 8,
                                       (Program [AddIntExpr (CondExpr (EqualExpr (IntLiteral 3, IntLiteral 2),
                                                                       (MulIntExpr (IntLiteral 2, IntLiteral 2)),
                                                                       (DivIntExpr (IntLiteral 10, IntLiteral 2))),
                                                             IntLiteral 3)]);
                                       "eval 48", IntVal 6,
                                       (Program [VerDecl ("x", IntLiteral 3, Some (AddIntExpr(Id "x",IntLiteral 3)))]);
                                       "eval 49", IntVal 13,
                                       (Program [VerDecl ("x", IntLiteral 3,
                                                          Some (VerDecl ("y", IntLiteral 5,
                                                                         Some (VerDecl ("x", IntLiteral 8,
                                                                                        Some (AddIntExpr (Id "x", Id "y")))))))]);
                                       "eval 50", IntVal 3,
                                       (Program [VerDecl ("x", IntLiteral 3, Some (Id "x"))]);
                                       (* Can't test because analyzed variable is changed *)
                                       (* "eval 51", FunVal (["x";"y"], BinOp (IdTerm ("x", Int), IdTerm ("y", Int), Add Int, Int), Int), *)
                                       (* (Program [FuncDecl ("func", ["x";"y";], AddIntExpr(Id "x", Id "y"), None)]); *)
                                       "eval 51", IntVal 5,
                                       (Program [
                                            FuncDecl ("func", ["x";"y";], AddIntExpr (Id "x", Id "y"),
                                                      Some (FuncCall ("func", [IntLiteral 3; IntLiteral 2])))]);
                                       (* "eval 52", IntVal 2, *)
                                       (* (Program [ *)
                                       (*     (\* let f a b = a / b in f 10 5 *\) *)
                                       (*     FuncDecl ("f", ["a"; "b"], DivIntExpr (Id "a", Id "b"), None); *)
                                       (*     FuncCall ("f", [IntLiteral 10; IntLiteral 5]); *)
                                       (*    ]); *)
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
                "analyze test" >::: (List.map
                                       (fun (title,res,arg) ->
                                        title >::
                                          (fun test_ctxt ->
                                           assert_equal
                                             res
                                             (Analyzer.analyze arg)))
                                       ["analyze 1",Flow [BinOp (Term (IntLiteral 2, Int), Term (IntLiteral 3, Int), Add Int, Int)],
                                        (Program [AddIntExpr(IntLiteral 2,IntLiteral 3)])]);
                (* "codegen test"; *)
               ]

let run_test = run_test_tt_main suite

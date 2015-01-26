open OUnit2
open Ast
open Token
open Interpreter
open Analyzer
open Llvm

let suite =
  "suite" >::: ["eval test" >::: (List.map
                                    (fun (title,res,arg) ->
                                     "eval" ^ title >::
                                       (fun test_ctxt ->
                                        assert_equal
                                          ~printer:Interpreter.rustic_val_to_str
                                          res
                                          (Interpreter.eval arg)))
                                    ["1",
                                     IntVal 3,
                                     (Program [IntLiteral 3]);

                                     "2",
                                     IntVal 10,
                                     (Program [
                                          AddIntExpr (
                                              IntLiteral 3,
                                              AddIntExpr (
                                                  IntLiteral 2,
                                                  IntLiteral 5))]);

                                     "3",
                                     IntVal 3,
                                     (Program [
                                          SubIntExpr (
                                              IntLiteral 5,
                                              IntLiteral 2)]);

                                     "4",
                                     IntVal 6,
                                     (Program [
                                          MulIntExpr (
                                              IntLiteral 2,
                                              IntLiteral 3)]);

                                     "5",
                                     IntVal 2,
                                     (Program [
                                          DivIntExpr (
                                              IntLiteral 10,
                                              IntLiteral 5)]);

                                     "6",
                                     FloatVal 5.,
                                     (Program [
                                          AddFloatExpr (
                                              FloatLiteral 2.,FloatLiteral 3.)]);

                                     "7",
                                     FloatVal 2.,
                                     (Program [
                                          SubFloatExpr (
                                              FloatLiteral 5.,
                                              FloatLiteral 3.)]);

                                     "8",
                                     FloatVal 6.,
                                     (Program [
                                          MulFloatExpr (
                                              FloatLiteral 2.,
                                              FloatLiteral 3.)]);

                                     "9",
                                     FloatVal 2.,
                                     (Program [
                                          DivFloatExpr (
                                              FloatLiteral 6.,
                                              FloatLiteral 3.)]);

                                     "10",
                                     BoolVal true,
                                     (Program [
                                          EqualExpr (
                                              IntLiteral 3,
                                              IntLiteral 3)]);

                                     (* "11", *)
                                     (* BoolVal false, *)
                                     (* (Program [ *)
                                     (*      EqualExpr ( *)
                                     (*          BoolLiteral true, *)
                                     (*          BoolLiteral false)]); *)

                                     (* "12", *)
                                     (* BoolVal true, *)
                                     (* (Program [ *)
                                     (*      EqualExpr ( *)
                                     (*          BoolLiteral true, *)
                                     (*          BoolLiteral true)]); *)

                                     "13",
                                     BoolVal true,
                                     (Program [
                                          NotEqualExpr (
                                              IntLiteral 3,
                                              IntLiteral 2)]);

                                     (* "14", *)
                                     (* BoolVal true, *)
                                     (* (Program [ *)
                                     (*      NotEqualExpr ( *)
                                     (*          BoolLiteral true, *)
                                     (*          BoolLiteral false)]); *)

                                     (* "15", *)
                                     (* BoolVal false, *)
                                     (* (Program [ *)
                                     (*      NotEqualExpr ( *)
                                     (*          BoolLiteral true, *)
                                     (*          BoolLiteral true)]); *)

                                     "16",
                                     BoolVal true ,
                                     (Program [
                                          LessExpr (
                                              IntLiteral 3,
                                              IntLiteral 5)]);

                                     "17",
                                     BoolVal true,
                                     (Program [
                                          LessEqualExpr (
                                              IntLiteral 3,
                                              IntLiteral 3)]);

                                     "18",
                                     BoolVal true,
                                     (Program [
                                          GreaterExpr (
                                              IntLiteral 5,
                                              IntLiteral 3)]);

                                     "19",
                                     BoolVal true,
                                     (Program [
                                          LogicOrExpr (
                                              BoolLiteral true,
                                              BoolLiteral true)]);

                                     "20",
                                     BoolVal true,
                                     (Program [
                                          LogicAndExpr (
                                              BoolLiteral true,
                                              BoolLiteral true)]);

                                     "21",
                                     IntVal 6,
                                     (Program [
                                          CondExpr (
                                              EqualExpr (
                                                  IntLiteral 3,
                                                  IntLiteral 3),
                                              (MulIntExpr (
                                                   IntLiteral 2,
                                                   IntLiteral 3)),
                                              (DivIntExpr (
                                                   IntLiteral 10,
                                                   IntLiteral 2)))]);

                                     "22",
                                     IntVal 5,
                                     (Program [
                                          CondExpr (
                                              GreaterExpr (
                                                  IntLiteral 3,
                                                  IntLiteral 3),
                                              (MulIntExpr (
                                                   IntLiteral 2,
                                                   IntLiteral 3)),
                                              (DivIntExpr (
                                                   IntLiteral 10,
                                                   IntLiteral 2)))]);

                                     "23",
                                     IntVal 7,
                                     (Program [
                                          AddIntExpr (
                                              CondExpr (
                                                  EqualExpr (
                                                      IntLiteral 3,
                                                      IntLiteral 3),
                                                  (MulIntExpr (
                                                       IntLiteral 2,
                                                       IntLiteral 2)),
                                                  (DivIntExpr (
                                                       IntLiteral 10,
                                                       IntLiteral 2))),
                                              IntLiteral 3)]);

                                     "24",
                                     IntVal 6,
                                     (Program [
                                          VerDecl (
                                              "x",
                                              IntLiteral 3,
                                              Some (AddIntExpr (
                                                        Id "x",
                                                        IntLiteral 3)))]);

                                     "25",
                                     IntVal 13,
                                     (* let x = 3 in let y = 5 in let x = 8 in x + y *)
                                     (Program [
                                          VerDecl (
                                              "x",
                                              IntLiteral 3,
                                              Some (
                                                  VerDecl (
                                                      "y",
                                                      IntLiteral 5,
                                                      Some (
                                                          VerDecl (
                                                              "x",
                                                              IntLiteral 8,
                                                              Some (
                                                                  AddIntExpr (
                                                                      Id "x",
                                                                      Id "y")))))))]);

                                     "26",
                                     IntVal 3,
                                     (Program [
                                          VerDecl (
                                              "x",
                                              IntLiteral 3,
                                              Some (
                                                  Id "x"))]);

                                     "27",
                                     IntVal 5,
                                     (* let func x y = x + y in func 3 2 *)
                                     (Program [
                                          FuncDecl (
                                              "func",
                                              ["x";"y";],
                                              AddIntExpr (
                                                  Id "x",
                                                  Id "y"),
                                              Some (
                                                  FuncCall (
                                                      "func",
                                                      [IntLiteral 3; IntLiteral 2])))]);

                                     "28",
                                     IntVal 2,
                                     (* let f a b = a / b;; f 10 5 *)
                                     (Program [
                                          FuncDecl (
                                              "f",
                                              ["a"; "b"],
                                              DivIntExpr (
                                                  Id "a",
                                                  Id "b"),
                                              None);
                                          FuncCall (
                                              "f",
                                              [IntLiteral 10; IntLiteral 5])]);

                                     "29",
                                     UnitVal,
                                     (* (); (); *)
                                     (Program [
                                          Sequence (
                                              UnitLiteral,
                                              UnitLiteral)]);

                                     "30",
                                     FloatVal 0.,
                                     (* let arr = Array.make 10 0;; arr.(2) *)
                                     (Program [
                                          VerDecl (
                                              "arr",
                                              ArrayNew (
                                                  "float",
                                                  IntLiteral 10),
                                              None);
                                          ArrayGet (
                                              "arr",
                                              IntLiteral 2)]);

                                     "31",
                                     FloatVal 3.14,
                                     (* let arr = Array.make 10 0.;; arr.(2) <- 3.14;; arr.(2) *)
                                     (Program [
                                          VerDecl (
                                              "array",
                                              ArrayNew (
                                                  "float",
                                                  IntLiteral 10),
                                              None);
                                          ArrayAssign (
                                              "array",
                                              IntLiteral 2,
                                              FloatLiteral 3.14);
                                          ArrayGet (
                                              "array",
                                              IntLiteral 2)]);

                                     "32",
                                     UnitVal,
                                     (* let arr = Array.make 5 0. in arr.(0) <- 0. *)
                                     (Program [
                                          VerDecl (
                                              "arr",
                                              ArrayNew (
                                                  "float",
                                                  IntLiteral 5),
                                              Some (
                                                  ArrayAssign (
                                                      "arr",
                                                      IntLiteral 0,
                                                      FloatLiteral 0.)))]);

                                     "33",
                                     BoolVal false,
                                     (Program [
                                          VerDecl (
                                              "arr",
                                              ArrayNew (
                                                  "bool",
                                                  IntLiteral 3),
                                              None);
                                          ArrayAssign (
                                              "arr",
                                              IntLiteral 0,
                                              BoolLiteral false);
                                          ArrayGet (
                                              "arr",
                                              IntLiteral 0)]);

                                    ]);
                "interpreter test" >::: (List.map
                                           (fun (title,res,arg) ->
                                            "interpreter " ^ title >::
                                              (fun test_ctxt ->
                                               assert_equal
                                                 ~printer:(fun s -> Printf.sprintf "%s" s)
                                                 res
                                                 (Interpreter.interpreter' arg)))
                                           ["1",
                                            "- : int = 1",
                                            (Program [
                                                 IntLiteral 1]);

                                            "2",
                                            "- : float = 13.4",
                                            (Program [
                                                 FloatLiteral 13.4]);

                                            "3",
                                            "- : float = 3.1415",
                                            (Program [
                                                 FloatLiteral 3.1415]);

                                            "4",
                                            "val func : int -> int -> int = <fun>",
                                            (Program [
                                                 FuncDecl (
                                                     "func",
                                                     ["a";"b"],
                                                     AddIntExpr (
                                                         Id "a",
                                                         Id "b"),
                                                     None)]);

                                            "5",
                                            "- : bool = true",
                                            (Program [
                                                 BoolLiteral true]);
                                            "6",
                                            "- : unit = ()",
                                            (Program [
                                                 UnitLiteral]);
                                            "7",
                                            "- : int array = [|0; 0; 0|]",
                                            (Program [
                                                 ArrayNew (
                                                     "int",
                                                     IntLiteral 3)]);                                            
                                            "8",
                                            "- : unit = ()",
                                            (* let a = 1 in print_int (a + 6) *)
                                            (Program [VerDecl (
                                                          "a",
                                                          IntLiteral 1,
                                                          Some (
                                                              FuncCall (
                                                                  "print_int",
                                                                  [AddIntExpr (
                                                                       Id "a",
                                                                       IntLiteral 6)])))]);
                                            "9",
                                            "- : unit = ()",
                                            (Program [
                                                 Sequence (
                                                     FuncCall (
                                                         "print_newline",
                                                         [UnitLiteral]),
                                                     Sequence (
                                                         FuncCall (
                                                             "print_float",
                                                             [FloatLiteral 3.1]),
                                                         FuncCall (
                                                             "print_bool",
                                                             [BoolLiteral true])))]);
                                            "10",
                                            "val a : int = 3",
                                            (Program [
                                                 VerDecl (
                                                     "a",
                                                     IntLiteral 3,
                                                     None)]);
                                           ]);
                "lexer test" >::: (List.map
                                     (fun (title,res,arg) ->
                                      "Lexer " ^ title >::
                                        (fun test_ctxt ->
                                         assert_equal
                                           res
                                           (Lexer.lex (Lexer.char_list_of_string arg))))
                                     ["1",
                                      [IntLiteral 1; Op AddInt; IntLiteral 1],
                                      "1 + 1";

                                      "2",
                                      [Keyword Let; Identifier "x"; Op Assign; IntLiteral 3],
                                      "let x = 3";
                                     ]);
                (* "perser test"; *)
                "left associative test" >::: (List.map
                                                (fun (title,res,arg1,arg2) ->
                                                 "left associative " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Make_left_associative_tree.make_left_associative_tree arg1 arg2)))
                                                ["1",
                                                 AddIntExpr (
                                                     SubIntExpr (
                                                         AddIntExpr (
                                                             IntLiteral 1,
                                                             IntLiteral 3),
                                                         IntLiteral 2),
                                                     IntLiteral 1),
                                                 IntLiteral 1,
                                                 [Op AddInt, IntLiteral 3;
                                                  Op SubInt, IntLiteral 2;
                                                  Op AddInt, IntLiteral 1];

                                                 "2",
                                                 SubFloatExpr (
                                                     SubFloatExpr (
                                                         AddFloatExpr (
                                                             FloatLiteral 3.2,
                                                             FloatLiteral 2.1),
                                                         FloatLiteral 3.3),
                                                     FloatLiteral 2.1),
                                                 FloatLiteral 3.2,
                                                 [Op AddFloat, FloatLiteral 2.1;
                                                  Op SubFloat, FloatLiteral 3.3;
                                                  Op SubFloat, FloatLiteral 2.1];

                                                 "3",
                                                 MulIntExpr (
                                                     DivIntExpr (
                                                         DivIntExpr (
                                                             MulIntExpr (
                                                                 IntLiteral 4,
                                                                 IntLiteral 3),
                                                             IntLiteral 2),
                                                         IntLiteral 1),
                                                     IntLiteral 0),
                                                 IntLiteral 4,
                                                 [Op MulInt, IntLiteral 3;
                                                  Op DivInt, IntLiteral 2;
                                                  Op DivInt, IntLiteral 1;
                                                  Op MulInt, IntLiteral 0];

                                                 "4",
                                                 MulFloatExpr (
                                                     DivFloatExpr (
                                                         DivFloatExpr (
                                                             MulFloatExpr (
                                                                 FloatLiteral 4.1,
                                                                 FloatLiteral 3.2),
                                                             FloatLiteral 2.3),
                                                         FloatLiteral 1.4),
                                                     FloatLiteral 0.5),
                                                 FloatLiteral 4.1,
                                                 [Op MulFloat, FloatLiteral 3.2;
                                                  Op DivFloat, FloatLiteral 2.3;
                                                  Op DivFloat, FloatLiteral 1.4;
                                                  Op MulFloat, FloatLiteral 0.5];

                                                ]);

                "prim_expr_rule test" >::: (List.map
                                                (fun (title,res,arg) ->
                                                 "prim_expr_rule " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Parser.prim_expr_rule (Lexer.lex (Lexer.char_list_of_string arg)))))
                                                ["1",
                                                 (IntLiteral 1, []),
                                                 "1";

                                                 "2",
                                                 (FloatLiteral 1.0, []),
                                                 "1.0";

                                                 "3",
                                                 (BoolLiteral true, []),
                                                 "true";

                                                 "4",
                                                 (BoolLiteral false, []),
                                                 "false";

                                                 "5",
                                                 (Id "a", []),
                                                 "a";
                                                ]);

                "func_call_expr_rule test" >::: (List.map
                                                (fun (title,res,arg) ->
                                                 "func_call_expr_rule " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Parser.func_call_expr_rule (Lexer.lex (Lexer.char_list_of_string arg)))))
                                                ["1",
                                                 (IntLiteral 1, []),
                                                 "1";

                                                 "2",
                                                 (Id "f", []),
                                                 "f";

                                                 "3",
                                                 (FuncCall ("f", [IntLiteral 1]), []),
                                                 "f 1";

                                                 "4",
                                                 (FuncCall ("f", [IntLiteral 1; IntLiteral 2]), []),
                                                 "f 1 2";
                                                ]);

                "mul_expr_rule test" >::: (List.map
                                                (fun (title,res,arg) ->
                                                 "mul_expr_rule " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Parser.mul_expr_rule (Lexer.lex (Lexer.char_list_of_string arg)))))
                                                ["1",
                                                 (IntLiteral 1, []),
                                                 "1";

                                                 "2",
                                                 (MulIntExpr (IntLiteral 1, IntLiteral 2), []),
                                                 "1 * 2";

                                                 "3",
                                                 (MulIntExpr (MulIntExpr (IntLiteral 1, IntLiteral 2), IntLiteral 3), []),
                                                 "1 * 2 * 3";

                                                 "4",
                                                 (MulIntExpr (MulIntExpr (IntLiteral 1, IntLiteral 2), IntLiteral 3), [Op AddInt; IntLiteral 4]),
                                                 "1 * 2 * 3 + 4";

                                                 "5",
                                                 (MulIntExpr (FuncCall ("f", [IntLiteral 1]), FuncCall ("f", [IntLiteral 2])), []),
                                                 "f 1 * f 2";
                                                ]);

                "add_expr_rule test" >::: (List.map
                                                (fun (title,res,arg) ->
                                                 "add_expr_rule " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Parser.add_expr_rule (Lexer.lex (Lexer.char_list_of_string arg)))))
                                                ["1",
                                                 (IntLiteral 1, []),
                                                 "1";

                                                 "2",
                                                 (AddIntExpr (IntLiteral 1, IntLiteral 2), []),
                                                 "1 + 2";

                                                 "3",
                                                 (AddIntExpr (AddIntExpr (IntLiteral 1, IntLiteral 2), IntLiteral 3), []),
                                                 "1 + 2 + 3";

                                                 "4",
                                                 (AddIntExpr (IntLiteral 1, MulIntExpr (IntLiteral 2, IntLiteral 3)), []),
                                                 "1 + 2 * 3";
                                                ]);

                "comp_expr_rule test" >::: (List.map
                                                (fun (title,res,arg) ->
                                                 "comp_expr_rule " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Parser.comp_expr_rule (Lexer.lex (Lexer.char_list_of_string arg)))))
                                                ["1",
                                                 (IntLiteral 1, []),
                                                 "1";

                                                 "2",
                                                 (LessExpr (IntLiteral 1, IntLiteral 2), []),
                                                 "1 < 2"
                                                ]);

                "equal_expr_rule test" >::: (List.map
                                                (fun (title,res,arg) ->
                                                 "equal_expr_rule " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Parser.equal_expr_rule (Lexer.lex (Lexer.char_list_of_string arg)))))
                                                ["1",
                                                 (IntLiteral 1, []),
                                                 "1";

                                                 "2",
                                                 (EqualExpr (IntLiteral 1, IntLiteral 2), []),
                                                 "1 == 2"
                                                ]);

                "logic_and_expr_rule test" >::: (List.map
                                                (fun (title,res,arg) ->
                                                "logic_and_expr_rule " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Parser.logic_and_expr_rule (Lexer.lex (Lexer.char_list_of_string arg)))))
                                                ["1",
                                                 (IntLiteral 1, []),
                                                 "1";

                                                 "2",
                                                 (LogicAndExpr (IntLiteral 1, IntLiteral 2), []),
                                                 "1 && 2"
                                                ]);

                "logic_or_expr_rule test" >::: (List.map
                                                (fun (title,res,arg) ->
                                                 "logic_or_expr_rule " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Parser.logic_or_expr_rule (Lexer.lex (Lexer.char_list_of_string arg)))))
                                                ["1",
                                                 (IntLiteral 1, []),
                                                 "1";

                                                 "2",
                                                 (LogicOrExpr (IntLiteral 1, IntLiteral 2), []),
                                                 "1 || 2"
                                                ]);

                "cond_expr_rule test" >::: (List.map
                                                (fun (title,res,arg) ->
                                                 "cond_expr_rule " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Parser.cond_expr_rule (Lexer.lex (Lexer.char_list_of_string arg)))))
                                                ["1",
                                                 (IntLiteral 1, []),
                                                 "1";

                                                 "2",
                                                 (CondExpr (BoolLiteral true, IntLiteral 1, IntLiteral 2), []),
                                                 "if true then 1 else 2"
                                                ]);

                "seq_expr_rule test" >::: (List.map
                                                (fun (title,res,arg) ->
                                                 "seq_expr_rule " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Parser.seq_expr_rule (Lexer.lex (Lexer.char_list_of_string arg)))))
                                                ["1",
                                                 (IntLiteral 1, []),
                                                 "1";

                                                 "2",
                                                 (Sequence (IntLiteral 1, IntLiteral 2), []),
                                                 "1;2";
                                                ]);

                "decl_rule test" >::: (List.map
                                                (fun (title,res,arg) ->
                                                 "decl_rule " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Parser.decl_rule (Lexer.lex (Lexer.char_list_of_string arg)))))
                                                ["1",
                                                 (VerDecl ("a", Id "b", None), []),
                                                 "let a = b";

                                                 "2",
                                                 (FuncDecl ("mul", ["x"; "y"], MulIntExpr (Id "x", Id "y"), None), []),
                                                 "let mul(x y) = x * y"
                                                ]);

                "let_expr_rule test" >::: (List.map
                                                (fun (title,res,arg) ->
                                                 "let_expr_rule " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Parser.let_expr_rule (Lexer.lex (Lexer.char_list_of_string arg)))))
                                                ["1",
                                                 (VerDecl ("pi", FloatLiteral 3.14, Some (MulFloatExpr (Id "pi", Id "pi"))), []),
                                                 "let pi = 3.14 in pi *. pi";

                                                 "2",
                                                 (FuncDecl ("mul", ["x"; "y"], MulIntExpr (Id "x", Id "y"), Some (FuncCall ("mul", [IntLiteral 1; IntLiteral 1]))), []),
                                                 "let mul(x y) = x * y in mul 1 1"
                                                ]);

                "top_let_expr_rule test" >::: (List.map
                                                (fun (title,res,arg) ->
                                                 "top_let_expr_rule " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Parser.top_let_expr_rule (Lexer.lex (Lexer.char_list_of_string arg)))))
                                                ["1",
                                                 (VerDecl ("pi", FloatLiteral 3.14, Some (MulFloatExpr (Id "pi", Id "pi"))), []),
                                                 "let pi = 3.14 in pi *. pi";

                                                 "2",
                                                 (FuncDecl ("mul", ["x"; "y"], MulIntExpr (Id "x", Id "y"), Some (FuncCall ("mul", [IntLiteral 1; IntLiteral 1]))), []),
                                                 "let mul(x y) = x * y in mul 1 1";

                                                 "3",
                                                 (VerDecl ("a", Id "b", None), []),
                                                 "let a = b";

                                                 "4",
                                                 (FuncDecl ("mul", ["x"; "y"], MulIntExpr (Id "x", Id "y"), None), []),
                                                 "let mul(x y) = x * y"
                                                ]);

                "program_rule test" >::: (List.map
                                                (fun (title,res,arg) ->
                                                 "program_rule " ^ title >::
                                                   (fun test_ctxt ->
                                                    assert_equal
                                                      res
                                                      (Parser.program_rule (Lexer.lex (Lexer.char_list_of_string arg)))))
                                                ["1",
                                                 (Program [
                                                     FuncDecl ("double", ["n"], MulIntExpr (Id "n", IntLiteral 2), None);
                                                     FuncCall ("double", [IntLiteral 2])
                                                 ], []),
                                                 "let double(n) = n * 2;;
                                                 double(2)";

                                                 "2",
                                                 (Program [
                                                      FuncDecl ("fib", ["n"],
                                                          CondExpr (EqualExpr (Id "n", IntLiteral 0), IntLiteral 0,
                                                              CondExpr (EqualExpr (Id "n", IntLiteral 1), IntLiteral 1,
                                                                  AddIntExpr (FuncCall ("fib", [SubIntExpr (Id "n", IntLiteral 1)]), FuncCall("fib", [SubIntExpr (Id "n", IntLiteral 2)])))), None);
                                                      FuncCall ("fib", [IntLiteral 10])], []),
                                                 "let fib(n) =
                                                          if n == 0 then 0
                                                     else if n == 1 then 1
                                                     else fib (n - 1) + fib (n - 2);;

                                                  fib 10";

                                                  "3",
                                                  (Program [], []),
                                                  ";;";

                                                  "4",
                                                  (Program [Id "a"], []),
                                                  "a";

                                                  "5",
                                                  (Program [Id "a"], []),
                                                  "a;;";
                                                ]);

                "analyze test" >::: (List.map
                                       (fun (title,res,arg) ->
                                        "analyze " ^ title >::
                                          (fun test_ctxt ->
                                           assert_equal
                                             res
                                             (Analyzer.analyze arg)))
                                       ["1",
                                        (Flow [
                                             BinOp (
                                                 Term (
                                                     IntLiteral 2,
                                                     Int),
                                                 Term (
                                                     IntLiteral 3,
                                                     Int),
                                                 Add Int,
                                                 Int)]),
                                        (Program [
                                             AddIntExpr (
                                                 IntLiteral 2,
                                                 IntLiteral 3)]);
                                        "2",
                                        (Flow [
                                             Seq (
                                                 Cond (
                                                     BinOp (
                                                         Term (
                                                             FloatLiteral 3.2,
                                                             Float),
                                                         Term (
                                                             FloatLiteral 3.2,
                                                             Float),
                                                         Eq Float,
                                                         Boolean),
                                                     Term (
                                                         UnitLiteral,
                                                         Unit),
                                                     Term (
                                                         UnitLiteral,
                                                         Unit)),
                                                 Term (
                                                     UnitLiteral,
                                                     Unit))]),
                                        (Program [
                                             Sequence (CondExpr (
                                                           EqualExpr (
                                                               FloatLiteral 3.2,
                                                               FloatLiteral 3.2),
                                                           UnitLiteral,
                                                           UnitLiteral),
                                                       UnitLiteral)]);
                                       ]);
                (* "codegen test"; *)
               ]

let run_test = run_test_tt_main suite

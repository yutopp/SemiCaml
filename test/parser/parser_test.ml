open OUnit2
open Ast
open Token

let suite = 
  "suite" >::: ["lexer test" >::: (List.map
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

                                      "3",
                                      [Keyword Array; Op Dot; Keyword New; Identifier "int"; IntLiteral 3],
                                      "Array.new int 3";
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

                                               "6",
                                               (ArrayNew ("int", AddIntExpr (IntLiteral 3, IntLiteral 1)), []),
                                               "Array.new int 3 + 1";

                                               "7",
                                               (ArrayGet ("ary", Id "a"), []),
                                               "ary.(a)";

                                               "8",
                                               (ArrayAssign ("ary", Id "a", AddIntExpr (Id "b", Id "c")), []),
                                               "ary.(a) <- b + c";

                                               "9",
                                               (UnitLiteral, []),
                                               "()";
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

                                                    "5",
                                                    (FuncCall ("f", [Id "a"; Id "b"]), []),
                                                    "f a b";

                                                    "6",
                                                    (FuncCall ("print_newline", [UnitLiteral]), []),
                                                    "print_newline ()";
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
                                              "1; 2";

                                              "3",
                                              (Sequence (Id "a", Id "a"), []),
                                              "a; a";

                                              "4",
                                              (Sequence (AddIntExpr (IntLiteral 1, IntLiteral 1), IntLiteral 2), []),
                                              "1 + 1; 2";

                                              "5",
                                              (Sequence (FuncCall ("f", [IntLiteral 1]), IntLiteral 2), []),
                                              "f 1; 2";

                                              "6",
                                              (Sequence (FuncCall ("f", [Id "a"]), Id "b"), []),
                                              "f a; b";

                                              "7",
                                              (Sequence (ArrayNew ("int", AddIntExpr (Id "a", Id "b")), FuncCall ("print", [Id "a"])), []),
                                              "Array.new int a + b; print a";

                                              "8",
                                              (Sequence (ArrayAssign ("hoge", Id "a", AddIntExpr (Id "b", Id "c")), FuncCall ("print", [Id "hoge"])), []),
                                              "hoge.(a) <- b + c; print hoge"
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
                                          (FuncDecl (false, "mul", ["x"; "y"], MulIntExpr (Id "x", Id "y"), None), []),
                                          "let mul x y = x * y"
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
                                              (FuncDecl (false, "mul", ["x"; "y"], MulIntExpr (Id "x", Id "y"), Some (FuncCall ("mul", [IntLiteral 1; IntLiteral 1]))), []),
                                              "let mul x y = x * y in mul 1 1";

                                              "3",
                                              (VerDecl ("a", IntLiteral 0, Some (Sequence (FuncCall ("print", [Id "a"]), MulIntExpr (Id "a", IntLiteral 2)))), []),
                                              "let a = 0 in print a; a * 2";

                                              "4",
                                              (VerDecl ("a", IntLiteral 2, Some (Sequence (FuncCall ("print", [Id "a"]), VerDecl ("b", IntLiteral 3, Some (FuncCall ("print", [AddIntExpr (Id "a", Id "b")])))))), []),
                                              "let a = 2 in print a; let b = 3 in print (a + b)"
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
                                                  (FuncDecl (false, "mul", ["x"; "y"], MulIntExpr (Id "x", Id "y"), Some (FuncCall ("mul", [IntLiteral 1; IntLiteral 1]))), []),
                                                  "let mul x y = x * y in mul 1 1";

                                                  "3",
                                                  (VerDecl ("a", Id "b", None), []),
                                                  "let a = b";

                                                  "4",
                                                  (FuncDecl (false, "mul", ["x"; "y"], MulIntExpr (Id "x", Id "y"), None), []),
                                                  "let mul x y = x * y"
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
                                                  FuncDecl (false, "double", ["n"], MulIntExpr (Id "n", IntLiteral 2), None);
                                                  FuncCall ("double", [IntLiteral 2])
                                                ], []),
                                             "let double n = n * 2;;
                                              double(2)";

                                             "2",
                                             (Program [
                                                  FuncDecl (false, "fib", ["n"],
                                                            CondExpr (EqualExpr (Id "n", IntLiteral 0), IntLiteral 0,
                                                                      CondExpr (EqualExpr (Id "n", IntLiteral 1), IntLiteral 1,
                                                                                AddIntExpr (FuncCall ("fib", [SubIntExpr (Id "n", IntLiteral 1)]), FuncCall("fib", [SubIntExpr (Id "n", IntLiteral 2)])))), None);
                                                  FuncCall ("fib", [IntLiteral 10])], []),
                                             "let fib n =
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
                                            ]);]

let run_test = run_test_tt_main suite

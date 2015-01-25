open OUnit2
open Ast
open Token
open Interpreter
open Analyzer
open Associate_left
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
                                     (* "14", BoolVal true, *)
                                     (* (Program [NotEqualExpr (BoolLiteral true, BoolLiteral false)]); *)
                                     (* "15", BoolVal false, *)
                                     (* (Program [NotEqualExpr (BoolLiteral true, BoolLiteral true)]); *)
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
                                              "arr",
                                              ArrayNew (
                                                  "float",
                                                  IntLiteral 10),
                                              None);
                                          ArrayAssign (
                                              "arr",
                                              IntLiteral 2,
                                              FloatLiteral 3.14);
                                          ArrayGet (
                                              "arr",
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

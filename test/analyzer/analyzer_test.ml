open OUnit2
open Ast
open Token
open Analyzer

let suite =
  "suite" >::: ["analyze test" >::: (List.map
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
                                             Sequence (
                                                 CondExpr (
                                                     EqualExpr (
                                                         FloatLiteral 3.2,
                                                         FloatLiteral 3.2),
                                                     UnitLiteral,
                                                     UnitLiteral),
                                                 UnitLiteral)]);
                                        "3",
                                        (Flow [
                                             Seq (
                                                 Seq (
                                                     CallFunc (
                                                         IdTerm ("print_int", Func [Int; Unit]),
                                                         [Term (IntLiteral 1, Int)]
                                                       ),
                                                     CallFunc (
                                                         IdTerm ("print_int", Func [Int; Unit]),
                                                         [Term (IntLiteral 2, Int)]
                                                       )
                                                   ),
                                                 CallFunc (
                                                     IdTerm ("print_int", Func [Int; Unit]),
                                                     [Term (IntLiteral 3, Int)]
                                                   )
                                               )]
                                           ),
                                        (Program [
                                             Sequence (
                                                 Sequence (
                                                     FuncCall (
                                                         "print_int",
                                                         [IntLiteral 1]),
                                                     FuncCall (
                                                         "print_int",
                                                         [IntLiteral 2])),
                                                 FuncCall (
                                                     "print_int",
                                                     [IntLiteral 3]))]);
                                       ]);
                (* "codegen test"; *)
               ]

let run_test = run_test_tt_main suite

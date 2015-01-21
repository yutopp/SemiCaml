open OUnit2
open Ast
open Token
(* todo *)
(* lexer, perser, <- string *)
(* code gen <- *)
(* analyze' <- ast *)

let test_inter1 test_ctxt =
  assert_equal ~msg:"Interpreter 1"
               ~printer:Interpreter.printer
               (Interpreter.IntVal 3)
               (Interpreter.interpreter
                  (VerDecl ("x", IntLiteral 3, None)))
               
let test_inter2 test_ctxt =
  assert_equal ~msg:"Interpreter 2"
               ~printer:Interpreter.printer
               (Interpreter.IntVal 6)
               (Interpreter.interpreter
                  (VerDecl ("x", IntLiteral 3, (Some (AddIntExpr(Id "x", IntLiteral 3))))))
               
let test_inter3 test_ctxt =
  assert_equal ~msg:"Interpreter 3"
               ~printer:Interpreter.printer
               (Interpreter.IntVal 13)
               (Interpreter.interpreter
                  (VerDecl
                     ("x", IntLiteral 3,
                      (Some (VerDecl
                               ("y", IntLiteral 5,
                                (Some (VerDecl
                                         ("x", IntLiteral 8,
                                          (Some (AddIntExpr (Id "x", Id "y"))))))))))))
               
let test_lex1 test_ctxt =
  assert_equal ~msg:"Lexer 1"
               [IntLiteral 1; Op AddInt; IntLiteral 1]
               (Lexer.lex (Lexer.char_list_of_string "1 + 1"))
               
let test_lex2 test_ctxt =
  assert_equal ~msg:"Lexer 2"
               [Keyword Let; Identifier "x"; Op Assign; IntLiteral 3]
               (Lexer.lex (Lexer.char_list_of_string "let x = 3"))
               
let suite =
  "suite" >::: ["eval test" >::: ["interpreter test 1" >:: test_inter1;
                                  "interpreter test 2" >:: test_inter2;
                                  "interpreter test 3" >:: test_inter3;];
                "lexer test" >::: ["lexer test 1" >:: test_lex1;
                                   "lexer test 2" >:: test_lex2];]
                                       
let run_test = run_test_tt_main suite


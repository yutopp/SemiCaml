open OUnit2
open Ast
open Token
(* todo *)
(* lexer, perser, <- string *)
(* code gen <- *)
(* analyze' <- ast *)

let test1 test_ctxt = assert_equal ~msg:"Interpreter.eval"
                                   ~printer:Interpreter.printer
                                   (Interpreter.IntVal 3)
                                   (Interpreter.interpreter (VerDecl ("x", IntLiteral 3, None)))

let test_lex1 test_ctxt = assert_equal ~msg:"Lexer 1"
                                       [IntLiteral 1; Op AddInt; IntLiteral 1]
                                       (Lexer.lex (Lexer.char_list_of_string "1 + 1"))

let test_lex2 test_ctxt = assert_equal ~msg:"Lexer 2"
                                       [Keyword Let; Identifier "x"; Op Assign; IntLiteral 3]
                                       (Lexer.lex (Lexer.char_list_of_string "let x = 3"))

let suite = "suite" >::: ["eval test" >:: test1;
                          "lexer test" >::: ["lexer test 1" >:: test_lex1;
                                             "lexer test 2" >:: test_lex2];]
                                       
let _ = run_test_tt_main suite

                        







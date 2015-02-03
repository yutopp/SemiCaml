open OUnit2
open Interpreter

let suite =
  "suite" >::: ["combine test" >::: (List.map
                                       (fun (title,res,arg) ->
                                        "combine " ^ title >::
                                          (fun test_ctxt ->
                                           assert_equal
                                             ~printer:rustic_val_to_str
                                             res
                                             (Interpreter.eval
                                                (Parser.parse
                                                   (Lexer.lex
                                                      (Lexer.char_list_of_string arg))))))
                                       ["1",
                                        IntVal 10,
                                        "let rec comb n r =
                                           if n = r || r = 0
                                           then 1
                                           else comb n (r - 1) * (n - r + 1) / r;;
                                         
                                         comb 5 2;;";
                                        
                                        "2",
                                        IntVal 20,
                                        "let f x = let a = x in
                                         let g = a + 10 in
                                         g;;
                                         
                                         f 10;;";

                                        "3",
                                        UnitVal,
                                        "let rec f n =
                                           if n = 0 then ()
                                           else (f (n - 1); print_int n; print_newline());;
                                         f 10;;";

                                        "4",
                                        IntVal 0,
                                        "let rec f a =
                                           if a = 0 then 0 else f(a-1);;
                                         f 10";

                                        "5",
                                        UnitVal,
                                        "let double n = n *. 2.0 ;;

                                         print_newline() ;;

                                         let rec f n =
                                           if n = 0 then ()
                                           else (f (n - 1); print_int n; print_newline());;

                                         f 10;;

                                         print_float (double 3.14);;

                                         print_newline();;

                                         let rec sum n =
                                           if n = 0 then 0 else n + sum (n-1);;

                                         print_int (sum 10);;
                                         print_newline();;
                                         
                                         print_int (
                                           let rec sum1 x =
                                             if x = 1 then 1
                                             else x + sum1 (x - 1) in sum1 10);;
                                         print_newline();;";

                                        "6",
                                        IntVal 10946,
                                        "let rec comb n r =
                                         if n = r || r = 0
                                         then 1
                                         else comb n (r - 1) * (n - r + 1) / r;;
                                         
                                         print_int (comb 5 2);;
                                         
                                         print_newline ();;    
                                         
                                         let rec fibonacci n =
                                         if n = 0 || n = 1
                                         then 1
                                         else fibonacci (n - 1) + fibonacci (n - 2);;
                                         
                                         print_int (fibonacci 10);;
                                         
                                         print_newline ();;
                                         
                                         fibonacci (comb 6 3);;";

                                        "7",
                                        UnitVal,
                                        "let double n = n *. 2.0 ;;
                                         
                                         print_newline() ;;
                                         
                                         let rec f n =
                                           if n = 0 then ()
                                           else f (n - 1); print_int n; print_newline();;
                                         
                                         f 10;;";

                                        "8",
                                        IntVal 10,
                                        "let f a = let b = a in b ;;                                         
                                         f 10 ;;";

                                        "9",
                                        IntVal 10,
                                        "let f a = 10 ;;
                                         f 10 ;;"                                          
                                       ])] 

let run_test = run_test_tt_main suite

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
                                         f 10 ;;";

                                        "10",
                                        IntVal 9,
                                        "let arr = Array.new int 10;;

                                         let init i = arr.(i) <- i * i;;

                                         init 3;;
                                         arr.(3);;";

                                        "11",
                                        IntVal 285,
                                        "let arr = Array.new int 10;;
                                         let rec init i = if i = 10 then () else (arr.(i) <- i * i; init (i+1));;
                                         let rec show i acc = if i = 10 then acc else (let v = arr.(i) in show(i+1) (acc + v));;
                                         init 0;;
                                         show 0 0;;";

                                        "12",
                                        FloatVal 3.14,
                                        "let arr = Array.new float 10;;
                                         arr.(2) <- 3.14;;
                                         arr.(2);;";

                                        "13",
                                        IntVal 3,
                                        "3";

                                        "14",
                                        IntVal 10,
                                        "let a = 10;;
                                         let f _ = a;;
                                         f ()";

                                        "15",
                                        IntVal 20,
                                        "let f x = let a = x in let g = a + 10 in g ;;
                                         let p = f 10 in p;;";

                                        "16",
                                        IntVal 10,
                                        "let p x = let f a = a in f;;
                                         let f = p 10 in f 10;;";

                                        "17",
                                        IntVal 10,
                                        "let f x = 10;;
                                         let p a = let a = 10 in (let x _ = (let nn = f 0 in a) in x);;
                                         let f = p 10;;
                                         f 72";

                                        "18",
                                        IntVal 13,
                                        "let f a =
                                           let a1 = a in
                                           let g x y = a + x + y in
                                           g;;

                                         let ff = f 10;;
                                           ff 1 2";

                                        "19",
                                        IntVal 33,
                                        "let f a b =
                                           let a1 = a in
                                           let b1 = b in
                                           let g x y = a + b + x + y in
                                           g;;

                                         let ff = f 10 20;;
                                           ff 1 2";

                                        "20",
                                        IntVal 12,
                                        "let a = 10 in
                                         let b = a + 1 in
                                         let c = b + 1 in
                                         c";

                                        "21",
                                        IntVal 55,
                                        "let rec f n acc = if n = 0 then acc else (f (n - 1) (n + acc));;
                                         f 10 0;;";

                                        "22",
                                        IntVal 0,
                                        "let rec f a = if a = 0 then 0 else f(a-1);;
                                         f 10";

                                        "23",
                                        IntVal 55,
                                        "let rec times f n acc = if n = 0 then acc else (f n; times f (n - 1) (n + acc));;

                                         times print_int 10 0;;";

                                        "24",
                                        IntVal 444,
                                        "let x = let f x = x + 123 in (let f x = x * x in f 10); f 321;;
                                         x;;";

                                        "25",
                                        FloatVal 6.28,
                                        "let double n = n *. 2.0 ;;
                                         double 3.14;;";

                                        "26",
                                        IntVal 10,
                                        "let f a = let b = a in b ;;
                                         f 10 ;;";

                                        "27",
                                        IntVal 10,
                                        "let f a = 10 ;;
                                         f 10;;"

                                       ])]

let run_test = run_test_tt_main suite

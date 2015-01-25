open Ast
open Token

(* ast -> (token * ast) list -> ast *)
let rec make_left_associative_tree ast token_asts = match token_asts with
  | [] -> ast
  | (Op AddInt, any_ast) :: rest ->
     make_left_associative_tree
       (AddIntExpr (ast, any_ast))
       rest
  | (Op SubInt, any_ast) :: rest ->
     make_left_associative_tree
       (SubIntExpr (ast, any_ast))
       rest
  | (Op AddFloat, any_ast) :: rest ->
     make_left_associative_tree
       (AddFloatExpr (ast, any_ast))
       rest
  | (Op SubFloat, any_ast) :: rest ->
     make_left_associative_tree
       (SubFloatExpr (ast, any_ast))
       rest
  | (Op MulInt, any_ast) :: rest ->
     make_left_associative_tree
       (MulIntExpr (ast, any_ast))
       rest
  | (Op DivInt, any_ast) :: rest ->
     make_left_associative_tree
       (DivIntExpr (ast, any_ast))
       rest
  | (Op MulFloat, any_ast) :: rest ->
     make_left_associative_tree
       (MulFloatExpr (ast, any_ast))
       rest
  | (Op DivFloat, any_ast) :: rest ->
     make_left_associative_tree
       (DivFloatExpr (ast, any_ast))
       rest
  | _ -> failwith "not expected type"

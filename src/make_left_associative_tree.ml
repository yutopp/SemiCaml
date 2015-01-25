open Ast
open Token

(* ast -> (token * ast) list -> ast *)
let rec make_left_associative_tree ast token_asts = match token_asts with
  | [] -> ast
  | (Op AddInt, IntLiteral n) :: rest ->
     make_left_associative_tree
       (AddIntExpr (ast, IntLiteral n))
       rest
  | (Op SubInt, IntLiteral n) :: rest ->
     make_left_associative_tree
       (SubIntExpr (ast, IntLiteral n))
       rest
  | (Op AddFloat, FloatLiteral r) :: rest ->
     make_left_associative_tree
       (AddFloatExpr (ast, FloatLiteral r))
       rest
  | (Op SubFloat, FloatLiteral r) :: rest ->
     make_left_associative_tree
       (SubFloatExpr (ast, FloatLiteral r))
       rest
  | (Op MulInt, IntLiteral n) :: rest ->
     make_left_associative_tree
       (MulIntExpr (ast, IntLiteral n))
       rest
  | (Op DivInt, IntLiteral n) :: rest ->
     make_left_associative_tree
       (DivIntExpr (ast, IntLiteral n))
       rest
  | (Op MulFloat, FloatLiteral r) :: rest ->
     make_left_associative_tree
       (MulFloatExpr (ast, FloatLiteral r))
       rest
  | (Op DivFloat, FloatLiteral r) :: rest ->
     make_left_associative_tree
       (DivFloatExpr (ast, FloatLiteral r))
       rest
  | _ -> failwith "not expected type"


open Ast

(* type ast = *)
(*     Program of ast list *)
(*   | Seq of ast list *)
(*   | VerDecl of string * ast ignore *)
(*   | FuncDecl of string * string list * ast *)
(*   | ArrayNew of string * ast *) 

(*   | ArrayGet of string * ast *)
(*   | ArrayAssign of string * ast * ast *)
(*   | FuncCall of string * ast list *)
       
type value =
  | IntVal of int
  | FloatVal of float
  | BoolVal of bool
                 
let rec eval input =
  let intop f e1 e2 = match (eval e1, eval e2) with
    | (IntVal n1, IntVal n2) -> IntVal (f n1 n2)
    | _ -> failwith "Integer value expected"
  in
  let floatop f e1 e2 = match (eval e1, eval e2) with
    | (FloatVal n1, FloatVal n2) -> FloatVal (f n1 n2)
    | _ -> failwith "float value expected"
  in
  let boolop f e1 e2 = match (eval e1, eval e2) with
    | (BoolVal b1, BoolVal b2) -> BoolVal (f b1 b2)
    | _ -> failwith "bool value expected"
  in
  match input with
  | IntLiteral n -> IntVal n
  | FloatLiteral n -> FloatVal n
  | BoolLiteral b -> BoolVal b
  | AddIntExpr (e1,e2) -> intop ( + ) e1 e2
  | SubIntExpr (e1,e2) -> intop ( - ) e1 e2
  | MulIntExpr (e1,e2) -> intop ( * ) e1 e2
  | DivIntExpr (e1,e2) when (eval e2 = IntVal 0) -> failwith "0 Division"
  | DivIntExpr (e1,e2) -> intop ( / ) e1 e2
  | AddFloatExpr (e1,e2) -> floatop ( +. ) e1 e2
  | SubFloatExpr (e1,e2) -> floatop ( -. ) e1 e2
  | MulFloatExpr (e1,e2) -> floatop ( *. ) e1 e2
  | DivFloatExpr (e1,e2) when (eval e2 = FloatVal 0.0) -> failwith "0 Division"
  | DivFloatExpr (e1,e2) -> floatop ( /. ) e1 e2
  | EqualExpr (e1,e2) ->
     (match (eval e1, eval e2) with
      | (IntVal n1, IntVal n2) -> BoolVal (n1 = n2)
      | (FloatVal n1, FloatVal n2) -> BoolVal (n1 = n2)
      | (BoolVal b1, BoolVal b2) -> BoolVal (b1 = b2)
      | _ -> failwith "Integer, Float or Bool values expected"
     )
  | NotEqualExpr (e1,e2) ->
     (match (eval e1, eval e2) with
      | (IntVal n1, IntVal n2) -> BoolVal (n1 <> n2)
      | (FloatVal n1, FloatVal n2) -> BoolVal (n1 <> n2)
      | (BoolVal b1, BoolVal b2) -> BoolVal (b1 <> b2)
      | _ -> failwith "Integer, Float or Bool values expected"
     )
  | LessExpr (e1,e2) ->
     (match (eval e1, eval e2) with
      | (IntVal n1, IntVal n2) -> BoolVal (n1 < n2)
      | (FloatVal n1, FloatVal n2) -> BoolVal (n1 < n2)
      | _ -> failwith "Integer or Float values expected"
     )
  | LessEqualExpr (e1,e2) ->
     (match (eval e1, eval e2) with
      | (IntVal n1, IntVal n2) -> BoolVal (n1 <= n2)
      | (FloatVal n1, FloatVal n2) -> BoolVal (n1 <= n2)
      | _ -> failwith "Integer or Float values expected"
     )       
  | GreaterExpr (e1,e2) ->
     (match (eval e1, eval e2) with
      | (IntVal n1, IntVal n2) -> BoolVal (n1 > n2)
      | (FloatVal n1, FloatVal n2) -> BoolVal (n1 > n2)
      | _ -> failwith "Integer or Float values expected"
     )     
  | GreaterEqualExpr (e1,e2) ->
     (match (eval e1, eval e2) with
      | (IntVal n1, IntVal n2) -> BoolVal (n1 >= n2)
      | (FloatVal n1, FloatVal n2) -> BoolVal (n1 >= n2)
      | _ -> failwith "Integer or Float values expected"
     )          
  | LogicOrExpr (e1,e2) -> boolop ( || ) e1 e2
  | LogicAndExpr (e1,e2) -> boolop ( && ) e1 e2
  | CondExpr (e1,e2,e3) ->
     (match (eval e1) with
      | BoolVal true -> eval e2
      | BoolVal false -> eval e3
      | _ -> failwith "first exp bool value expected"
     )                             
  | _ -> failwith "no support"
                  
let _ = eval (AddIntExpr (IntLiteral 3, IntLiteral 2))                             = IntVal 5                
let _ = eval (AddIntExpr (AddIntExpr (IntLiteral 3, IntLiteral 3), IntLiteral 2))  = IntVal 8                
let _ = eval (SubIntExpr (IntLiteral 3, IntLiteral 2))                             = IntVal 1                
let _ = eval (MulIntExpr (IntLiteral 3, IntLiteral 2))                             = IntVal 6                
let _ = eval (SubIntExpr (MulIntExpr (IntLiteral 3, IntLiteral 2), IntLiteral 2))  = IntVal 4                
let _ = eval (DivIntExpr (IntLiteral 10, IntLiteral 2))                            = IntVal 5                
let _ = eval (DivIntExpr (IntLiteral 10, IntLiteral 0)) (* Exception "0 Division" *)
                                                                                                  
let _ = eval (AddFloatExpr (FloatLiteral 3.0, FloatLiteral 2.0))                   = FloatVal 5.             
let _ = eval (AddFloatExpr (AddFloatExpr(FloatLiteral 3., FloatLiteral 3.),                       
                            FloatLiteral 2.))                                      = FloatVal 8.             
let _ = eval (SubFloatExpr (FloatLiteral 3., FloatLiteral 2.))                     = FloatVal 1.             
let _ = eval (MulFloatExpr (FloatLiteral 3., FloatLiteral 2.))                     = FloatVal 6.             
let _ = eval (DivFloatExpr (FloatLiteral 10., FloatLiteral 2.))                    = FloatVal 5.             
let _ = eval (DivFloatExpr (FloatLiteral 10., FloatLiteral 0.)) (* Exception "0 Division" *)
                                                                                                  
let _ = eval (EqualExpr (IntLiteral 3, IntLiteral 3))                              = BoolVal true            
let _ = eval (EqualExpr (IntLiteral 3, IntLiteral 2))                              = BoolVal false           
let _ = eval (EqualExpr (FloatLiteral 3., FloatLiteral 3.))                        = BoolVal true            
let _ = eval (EqualExpr (FloatLiteral 3., FloatLiteral 2.))                        = BoolVal false           
let _ = eval (EqualExpr (BoolLiteral true, BoolLiteral false))                     = BoolVal false           
let _ = eval (EqualExpr (BoolLiteral true, BoolLiteral true))                      = BoolVal true             
let _ = eval (EqualExpr (IntLiteral 3, BoolLiteral true)) (* Exceptino Integer, Float or Bool values expected" *)

let _ = eval (NotEqualExpr (IntLiteral 3, IntLiteral 3))                           = BoolVal false 
let _ = eval (NotEqualExpr (IntLiteral 3, IntLiteral 2))                           = BoolVal true 
let _ = eval (NotEqualExpr (FloatLiteral 3., FloatLiteral 3.))                     = BoolVal false
let _ = eval (NotEqualExpr (FloatLiteral 3., FloatLiteral 2.))                     = BoolVal true 
let _ = eval (NotEqualExpr (BoolLiteral true, BoolLiteral false))                  = BoolVal true 
let _ = eval (NotEqualExpr (BoolLiteral true, BoolLiteral true))                   = BoolVal false

let _ = eval (LessExpr (IntLiteral 3, IntLiteral 5))                               = BoolVal true 
let _ = eval (LessExpr (IntLiteral 5, IntLiteral 3))                               = BoolVal false
let _ = eval (LessEqualExpr (IntLiteral 3, IntLiteral 3))                          = BoolVal true 
let _ = eval (LessEqualExpr (IntLiteral 3, IntLiteral 5))                          = BoolVal true 
let _ = eval (LessEqualExpr (IntLiteral 5, IntLiteral 3))                          = BoolVal false
let _ = eval (GreaterExpr (IntLiteral 5, IntLiteral 3))                            = BoolVal true 
let _ = eval (GreaterExpr (IntLiteral 3, IntLiteral 5))                            = BoolVal false
let _ = eval (GreaterEqualExpr (IntLiteral 3, IntLiteral 3))                       = BoolVal true 
let _ = eval (GreaterEqualExpr (IntLiteral 5, IntLiteral 3))                       = BoolVal true 
let _ = eval (GreaterEqualExpr (IntLiteral 3, IntLiteral 5))                       = BoolVal false 
                                                                  
let _ = eval (LogicOrExpr (BoolLiteral true, BoolLiteral true))                    = BoolVal true  
let _ = eval (LogicOrExpr (BoolLiteral false, BoolLiteral true))                   = BoolVal true 
let _ = eval (LogicAndExpr (BoolLiteral true, BoolLiteral true))                   = BoolVal true 
let _ = eval (LogicAndExpr (BoolLiteral false, BoolLiteral true))                  = BoolVal false

let _ = eval (CondExpr (EqualExpr (IntLiteral 3, IntLiteral 3),
                        (MulIntExpr (IntLiteral 2, IntLiteral 3)),
                        (DivIntExpr (IntLiteral 10, IntLiteral 2))))               = IntVal 6
                                                                                             
let _ = eval (CondExpr (GreaterExpr (IntLiteral 3, IntLiteral 3),                            
                        (MulIntExpr (IntLiteral 2, IntLiteral 3)),                           
                        (DivIntExpr (IntLiteral 10, IntLiteral 2))))               = IntVal 5

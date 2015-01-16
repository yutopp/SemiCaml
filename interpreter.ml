open Ast

(* rest *)
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
  | StringVal of string

type constant_folder = {
  int : int -> int -> bool;
  float : float -> float -> bool;
  bool : bool -> bool -> bool;
}
                 
let rec eval input env =
  let intop f e1 e2 = match (eval e1 env, eval e2 env) with
    | (IntVal n1, IntVal n2) -> IntVal (f n1 n2)
    | _ -> failwith "Integer value expected"
  in
  let floatop f e1 e2 = match (eval e1 env, eval e2 env) with
    | (FloatVal n1, FloatVal n2) -> FloatVal (f n1 n2)
    | _ -> failwith "float value expected"
  in
  let boolop f e1 e2 = match (eval e1 env, eval e2 env) with
    | (BoolVal b1, BoolVal b2) -> BoolVal (f b1 b2)
    | _ -> failwith "bool value expected"
  in
  let compop f e1 e2 = match (eval e1 env, eval e2 env) with
    | (IntVal n1, IntVal n2) -> BoolVal (f.int n1 n2)
    | (FloatVal n1, FloatVal n2) -> BoolVal (f.float n1 n2)
    | (BoolVal b1, BoolVal b2) -> BoolVal (f.bool b1 b2)
    | _ -> failwith "bool value expected"
  in
  let equal =
    { int = ( = ); float = ( = ); bool = ( = ) }
  in
  let not_equal =
    { int = ( <> ); float = ( <> ); bool = ( <> ) }
  in
  let less =
    { int = ( < ); float = ( < ); bool = ( < ) }
  in
  let less_equal = 
    { int = ( <= ); float = ( <= ); bool = ( <= ) }
  in
  let greater =
    { int = ( > ); float = ( > ); bool = ( > ) }
  in
  let greater_equal = 
    { int = ( >= ); float = ( >= ); bool = ( >= ) }
  in
  match input with
  | IntLiteral n -> IntVal n
  | FloatLiteral n -> FloatVal n
  | BoolLiteral b -> BoolVal b
                             
  | AddIntExpr (e1,e2) -> intop ( + ) e1 e2
  | SubIntExpr (e1,e2) -> intop ( - ) e1 e2
  | MulIntExpr (e1,e2) -> intop ( * ) e1 e2
  | DivIntExpr (e1,e2) when (eval e2 env = IntVal 0) -> failwith "0 Division"
  | DivIntExpr (e1,e2) -> intop ( / ) e1 e2
  | AddFloatExpr (e1,e2) -> floatop ( +. ) e1 e2
  | SubFloatExpr (e1,e2) -> floatop ( -. ) e1 e2
  | MulFloatExpr (e1,e2) -> floatop ( *. ) e1 e2
  | DivFloatExpr (e1,e2) when (eval e2 env = FloatVal 0.0) -> failwith "0 Division"
  | DivFloatExpr (e1,e2) -> floatop ( /. ) e1 e2
                                    
  | LogicOrExpr (e1,e2) -> boolop ( || ) e1 e2
  | LogicAndExpr (e1,e2) -> boolop ( && ) e1 e2                                    
  | EqualExpr (e1,e2) -> compop ( equal ) e1 e2
  | NotEqualExpr (e1,e2) -> compop ( not_equal ) e1 e2
  | LessExpr (e1,e2) -> compop ( less ) e1 e2
  | LessEqualExpr (e1,e2) -> compop ( less_equal ) e1 e2
  | GreaterExpr (e1,e2) -> compop ( greater ) e1 e2
  | GreaterEqualExpr (e1,e2) -> compop ( greater_equal ) e1 e2
                                       
  | CondExpr (e1,e2,e3) ->
     (match (eval e1 env) with
      | BoolVal true -> eval e2 env
      | BoolVal false -> eval e3 env
      | _ -> failwith "first exp bool value expected"
     )
  (* | VerDecl (s,e1) -> eval e1 (env_ext env s (eval e1)) *)
  | _ -> failwith "unknown exp"

module MyMap = Map.Make(String)

let emptyenv () = MyMap.empty

let env_ext env x v = MyMap.add x v env

let lookup x env = MyMap.find x env
                    
let _ = eval (AddIntExpr (IntLiteral 3, IntLiteral 2)) (emptyenv ())                            = IntVal 5                
let _ = eval (AddIntExpr (AddIntExpr (IntLiteral 3, IntLiteral 3), IntLiteral 2)) (emptyenv ()) = IntVal 8                
let _ = eval (SubIntExpr (IntLiteral 3, IntLiteral 2)) (emptyenv ())                            = IntVal 1                
let _ = eval (MulIntExpr (IntLiteral 3, IntLiteral 2)) (emptyenv ())                            = IntVal 6                
let _ = eval (SubIntExpr (MulIntExpr (IntLiteral 3, IntLiteral 2), IntLiteral 2)) (emptyenv ()) = IntVal 4                
let _ = eval (DivIntExpr (IntLiteral 10, IntLiteral 2)) (emptyenv ())                           = IntVal 5                
let _ = eval (DivIntExpr (IntLiteral 10, IntLiteral 0)) (emptyenv ()) (* Exception "0 Division" *)
                                                                                                  
let _ = eval (AddFloatExpr (FloatLiteral 3.0, FloatLiteral 2.0)) (emptyenv ())                  = FloatVal 5.             
let _ = eval (AddFloatExpr (AddFloatExpr(FloatLiteral 3., FloatLiteral 3.),                       
                            FloatLiteral 2.)) (emptyenv ())                                     = FloatVal 8.             
let _ = eval (SubFloatExpr (FloatLiteral 3., FloatLiteral 2.)) (emptyenv ())                    = FloatVal 1.             
let _ = eval (MulFloatExpr (FloatLiteral 3., FloatLiteral 2.)) (emptyenv ())                    = FloatVal 6.             
let _ = eval (DivFloatExpr (FloatLiteral 10., FloatLiteral 2.)) (emptyenv ())                   = FloatVal 5.             
let _ = eval (DivFloatExpr (FloatLiteral 10., FloatLiteral 0.)) (emptyenv ()) (* Exception "0 Division" *)
                                                                                                  
let _ = eval (EqualExpr (IntLiteral 3, IntLiteral 3)) (emptyenv ())                             = BoolVal true            
let _ = eval (EqualExpr (IntLiteral 3, IntLiteral 2)) (emptyenv ())                             = BoolVal false           
let _ = eval (EqualExpr (FloatLiteral 3., FloatLiteral 3.)) (emptyenv ())                       = BoolVal true            
let _ = eval (EqualExpr (FloatLiteral 3., FloatLiteral 2.)) (emptyenv ())                       = BoolVal false           
let _ = eval (EqualExpr (BoolLiteral true, BoolLiteral false)) (emptyenv ())                    = BoolVal false           
let _ = eval (EqualExpr (BoolLiteral true, BoolLiteral true)) (emptyenv ())                     = BoolVal true             
let _ = eval (EqualExpr (IntLiteral 3, BoolLiteral true)) (emptyenv ())  (* Exceptino Integer, Float or Bool values expected" *)

let _ = eval (NotEqualExpr (IntLiteral 3, IntLiteral 3)) (emptyenv ())                          = BoolVal false 
let _ = eval (NotEqualExpr (IntLiteral 3, IntLiteral 2)) (emptyenv ())                          = BoolVal true 
let _ = eval (NotEqualExpr (FloatLiteral 3., FloatLiteral 3.)) (emptyenv ())                    = BoolVal false
let _ = eval (NotEqualExpr (FloatLiteral 3., FloatLiteral 2.)) (emptyenv ())                    = BoolVal true 
let _ = eval (NotEqualExpr (BoolLiteral true, BoolLiteral false)) (emptyenv ())                 = BoolVal true 
let _ = eval (NotEqualExpr (BoolLiteral true, BoolLiteral true)) (emptyenv ())                  = BoolVal false

let _ = eval (LessExpr (IntLiteral 3, IntLiteral 5)) (emptyenv ())                              = BoolVal true 
let _ = eval (LessExpr (IntLiteral 5, IntLiteral 3)) (emptyenv ())                              = BoolVal false
let _ = eval (LessExpr (BoolLiteral false, BoolLiteral true)) (emptyenv ())                     = BoolVal true
let _ = eval (LessEqualExpr (IntLiteral 3, IntLiteral 3)) (emptyenv ())                         = BoolVal true 
let _ = eval (LessEqualExpr (IntLiteral 3, IntLiteral 5)) (emptyenv ())                         = BoolVal true 
let _ = eval (LessEqualExpr (IntLiteral 5, IntLiteral 3)) (emptyenv ())                         = BoolVal false
let _ = eval (GreaterExpr (IntLiteral 5, IntLiteral 3)) (emptyenv ())                           = BoolVal true 
let _ = eval (GreaterExpr (IntLiteral 3, IntLiteral 5)) (emptyenv ())                           = BoolVal false
let _ = eval (GreaterExpr (BoolLiteral true, BoolLiteral false)) (emptyenv ())                  = BoolVal true
let _ = eval (GreaterEqualExpr (IntLiteral 3, IntLiteral 3)) (emptyenv ())                      = BoolVal true 
let _ = eval (GreaterEqualExpr (IntLiteral 5, IntLiteral 3)) (emptyenv ())                      = BoolVal true 
let _ = eval (GreaterEqualExpr (IntLiteral 3, IntLiteral 5)) (emptyenv ())                      = BoolVal false 
                                                                  
let _ = eval (LogicOrExpr (BoolLiteral true, BoolLiteral true)) (emptyenv ())                   = BoolVal true  
let _ = eval (LogicOrExpr (BoolLiteral false, BoolLiteral true)) (emptyenv ())                  = BoolVal true 
let _ = eval (LogicAndExpr (BoolLiteral true, BoolLiteral true)) (emptyenv ())                  = BoolVal true 
let _ = eval (LogicAndExpr (BoolLiteral false, BoolLiteral true)) (emptyenv ())                 = BoolVal false

let _ = eval (CondExpr (EqualExpr (IntLiteral 3, IntLiteral 3),
                        (MulIntExpr (IntLiteral 2, IntLiteral 3)),
                        (DivIntExpr (IntLiteral 10, IntLiteral 2)))) (emptyenv ())              = IntVal 6
                                                                                             
let _ = eval (CondExpr (GreaterExpr (IntLiteral 3, IntLiteral 3),                            
                        (MulIntExpr (IntLiteral 2, IntLiteral 3)),                           
                        (DivIntExpr (IntLiteral 10, IntLiteral 2)))) (emptyenv ())              = IntVal 5

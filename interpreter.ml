open Ast

(* type ast = *)
(*     Program of ast list *)
                   
(*   | VerDecl of string * ast * ast option (\* done *\) *)
(*   | FuncDecl of string * string list * ast * ast option *)

(*   | Sequence of ast * ast *)

(*   | ArrayNew of string * ast *)
(*   | CondExpr of ast * ast * ast (\* done *\) *)

(*   | LogicOrExpr of ast * ast    (\* done *\) *)
(*   | LogicAndExpr of ast * ast   (\* done *\) *)

(*   | EqualExpr of ast * ast      (\* done *\) *)
(*   | NotEqualExpr of ast * ast   (\* done *\) *)

(*   | LessExpr of ast * ast       (\* done *\) *)
(*   | LessEqualExpr of ast * ast  (\* done *\) *)
(*   | GreaterExpr of ast * ast    (\* done *\) *)
(*   | GreaterEqualExpr of ast * ast (\* done *\) *)

(*   | AddIntExpr of ast * ast     (\* done *\) *)
(*   | SubIntExpr of ast * ast     (\* done *\) *)
(*   | MulIntExpr of ast * ast     (\* done *\) *)
(*   | DivIntExpr of ast * ast     (\* done *\) *)
(*   | AddFloatExpr of ast * ast   (\* done *\) *)
(*   | SubFloatExpr of ast * ast   (\* done *\) *)
(*   | MulFloatExpr of ast * ast   (\* done *\) *)
(*   | DivFloatExpr of ast * ast   (\* done *\) *)

(*   | IntLiteral of int           (\* done *\) *)
(*   | FloatLiteral of float       (\* done *\) *)
(*   | BoolLiteral of bool         (\* done *\) *)

(*   | ArrayGet of string * ast *)
(*   | ArrayAssign of string * ast * ast *)
(*   | FuncCall of string * ast list *)
(*   | Id of string                (\* done *\) *)

(* todo *)
(* return type to ast *)
       
type value =
  | IntVal of int
  | FloatVal of float
  | BoolVal of bool

let printer values = match values with
  | IntVal n -> Printf.printf "IntVal %d\n" n
  | FloatVal n -> Printf.printf "FloatVal %f\n" n
  | BoolVal b -> Printf.printf "FloatVal %b\n" b

type constant_folder = {
  int : int -> int -> bool;
  float : float -> float -> bool;
  bool : bool -> bool -> bool;
}

let emptyenv () = Hashtbl.create 10

let env_ext env x v = Hashtbl.add env x v; env
                                  
let lookup x env = Hashtbl.find env x                                
                         
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
  let eq = { int = ( = ); float = ( = ); bool = ( = ) } in
  let neq = { int = ( <> ); float = ( <> ); bool = ( <> ) } in
  let lt = { int = ( < ); float = ( < ); bool = ( < ) } in
  let lte = { int = ( <= ); float = ( <= ); bool = ( <= ) } in
  let gt = { int = ( > ); float = ( > ); bool = ( > ) } in
  let gte =  { int = ( >= ); float = ( >= ); bool = ( >= ) } in
  match input with
  | IntLiteral n -> IntVal n
  | FloatLiteral n -> FloatVal n
  | BoolLiteral b -> BoolVal b
  | Id id -> lookup id env
  (* | Sequence (ehead,erest) ->  *)
  | VerDecl (id,e1,Some e2) -> eval e2 (env_ext env id (eval e1 env))
  | VerDecl (id,e1,None) -> lookup id (env_ext env id (eval e1 env))
  (* | FuncDecl (id,args,e1) = *)
  (* | ArrayNew (str,e1) ->  *)
                                   
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
  | EqualExpr (e1,e2) -> compop ( eq ) e1 e2
  | NotEqualExpr (e1,e2) -> compop ( neq ) e1 e2
  | LessExpr (e1,e2) -> compop ( lt ) e1 e2
  | LessEqualExpr (e1,e2) -> compop ( lte ) e1 e2
  | GreaterExpr (e1,e2) -> compop ( gt ) e1 e2
  | GreaterEqualExpr (e1,e2) -> compop ( gte ) e1 e2
                                       
  | CondExpr (e1,e2,e3) ->
     begin
       match (eval e1 env) with
       | BoolVal true -> eval e2 env
       | BoolVal false -> eval e3 env
       | _ -> failwith "first exp bool value expected"
     end
  | _ -> failwith "unknown exp"                  
              
let rec interpreter input =
  printer (eval input (emptyenv ()))
          
          
let _ = interpreter (VerDecl ("x", IntLiteral 3, None))                     
let _ = interpreter (VerDecl ("x", IntLiteral 3, (Some (AddIntExpr(Id "x", IntLiteral 3)))))
let _ = interpreter (VerDecl ("x", IntLiteral 3,
                              (Some (VerDecl ("y", IntLiteral 5,
                                              (Some (VerDecl ("x", IntLiteral 8,
                                                              (Some (AddIntExpr (Id "x", Id "y")))))))))))

(* let _ =  *)
          
let _ = eval (AddIntExpr (IntLiteral 3, IntLiteral 2)) (emptyenv ())                            = IntVal 5                
let _ = eval (AddIntExpr (AddIntExpr (IntLiteral 3, IntLiteral 3), IntLiteral 2)) (emptyenv ()) = IntVal 8                
let _ = eval (SubIntExpr (IntLiteral 3, IntLiteral 2)) (emptyenv ())                            = IntVal 1                
let _ = eval (MulIntExpr (IntLiteral 3, IntLiteral 2)) (emptyenv ())                            = IntVal 6                
let _ = eval (SubIntExpr (MulIntExpr (IntLiteral 3, IntLiteral 2), IntLiteral 2)) (emptyenv ()) = IntVal 4                
let _ = eval (DivIntExpr (IntLiteral 10, IntLiteral 2)) (emptyenv ())                           = IntVal 5                
(* let _ = eval (DivIntExpr (IntLiteral 10, IntLiteral 0)) (emptyenv ()) (\* Exception "0 Division" *\) *)
                                                                                                  
let _ = eval (AddFloatExpr (FloatLiteral 3.0, FloatLiteral 2.0)) (emptyenv ())                  = FloatVal 5.             
let _ = eval (AddFloatExpr (AddFloatExpr(FloatLiteral 3., FloatLiteral 3.),                       
                            FloatLiteral 2.)) (emptyenv ())                                     = FloatVal 8.             
let _ = eval (SubFloatExpr (FloatLiteral 3., FloatLiteral 2.)) (emptyenv ())                    = FloatVal 1.             
let _ = eval (MulFloatExpr (FloatLiteral 3., FloatLiteral 2.)) (emptyenv ())                    = FloatVal 6.             
let _ = eval (DivFloatExpr (FloatLiteral 10., FloatLiteral 2.)) (emptyenv ())                   = FloatVal 5.             
(* let _ = eval (DivFloatExpr (FloatLiteral 10., FloatLiteral 0.)) (emptyenv ()) (\* Exception "0 Division" *\) *)
                                                                                                  
let _ = eval (EqualExpr (IntLiteral 3, IntLiteral 3)) (emptyenv ())                             = BoolVal true            
let _ = eval (EqualExpr (IntLiteral 3, IntLiteral 2)) (emptyenv ())                             = BoolVal false           
let _ = eval (EqualExpr (FloatLiteral 3., FloatLiteral 3.)) (emptyenv ())                       = BoolVal true            
let _ = eval (EqualExpr (FloatLiteral 3., FloatLiteral 2.)) (emptyenv ())                       = BoolVal false           
let _ = eval (EqualExpr (BoolLiteral true, BoolLiteral false)) (emptyenv ())                    = BoolVal false           
let _ = eval (EqualExpr (BoolLiteral true, BoolLiteral true)) (emptyenv ())                     = BoolVal true             
(* let _ = eval (EqualExpr (IntLiteral 3, BoolLiteral true)) (emptyenv ())  (\* Exceptino Integer, Float or Bool values expected *\) *)

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

let _ = eval (AddIntExpr (CondExpr (EqualExpr (IntLiteral 3, IntLiteral 3),
                                    (MulIntExpr (IntLiteral 2, IntLiteral 2)),
                                    (DivIntExpr (IntLiteral 10, IntLiteral 2))),
                          IntLiteral 3)) (emptyenv ())                

let _ = eval (AddIntExpr (CondExpr (EqualExpr (IntLiteral 3, IntLiteral 2),
                                    (MulIntExpr (IntLiteral 2, IntLiteral 2)),
                                    (DivIntExpr (IntLiteral 10, IntLiteral 2))),
                          IntLiteral 3)) (emptyenv ())

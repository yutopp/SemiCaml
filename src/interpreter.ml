open Ast
open Analyzer
(* todo *)

type value =
  | IntVal of int
  | FloatVal of float
  | BoolVal of bool

let val_to_str values = match values with
  | IntVal n -> Printf.sprintf "%d\n" n
  | FloatVal n -> Printf.sprintf "%f\n" n
  | BoolVal b -> Printf.sprintf "%b\n" b

type constant_folder = {
  int : int -> int -> bool;
  float : float -> float -> bool;
  bool : bool -> bool -> bool;
}

let emptyenv () = Hashtbl.create 10

let env_ext env x v = Hashtbl.add env x v; env

let lookup x env = Hashtbl.find env x

let rec eval' input env =
  let intop f e1 e2 = match (eval' e1 env, eval' e2 env) with
    | (IntVal n1, IntVal n2) -> IntVal (f n1 n2)
    | _ -> failwith "Integer value expected"
  in
  let floatop f e1 e2 = match (eval' e1 env, eval' e2 env) with
    | (FloatVal n1, FloatVal n2) -> FloatVal (f n1 n2)
    | _ -> failwith "float value expected"
  in
  let boolop f e1 e2 = match (eval' e1 env, eval' e2 env) with
    | (BoolVal b1, BoolVal b2) -> BoolVal (f b1 b2)
    | _ -> failwith "bool value expected"
  in
  let compop f e1 e2 = match (eval' e1 env, eval' e2 env) with
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
  | VerDecl (id,e1,Some e2) -> eval' e2 (env_ext env id (eval' e1 env))
  | VerDecl (id,e1,None) -> lookup id (env_ext env id (eval' e1 env))
  (* | FuncDecl (id,args,e1,Some e2) = eval' e2 (env_ext env id (eval' e1 env)) *)
  (* | ArrayNew (str,e1) ->  *)

  | AddIntExpr (e1,e2) -> intop ( + ) e1 e2
  | SubIntExpr (e1,e2) -> intop ( - ) e1 e2
  | MulIntExpr (e1,e2) -> intop ( * ) e1 e2
  | DivIntExpr (e1,e2) when (eval' e2 env = IntVal 0) -> failwith "0 Division"
  | DivIntExpr (e1,e2) -> intop ( / ) e1 e2
  | AddFloatExpr (e1,e2) -> floatop ( +. ) e1 e2
  | SubFloatExpr (e1,e2) -> floatop ( -. ) e1 e2
  | MulFloatExpr (e1,e2) -> floatop ( *. ) e1 e2
  | DivFloatExpr (e1,e2) when (eval' e2 env = FloatVal 0.0) -> failwith "0 Division"
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
       match (eval' e1 env) with
       | BoolVal true -> eval' e2 env
       | BoolVal false -> eval' e3 env
       | _ -> failwith "first exp bool value expected"
     end
  | _ -> failwith "unknown exp"

let rec eval input =
  eval' input (emptyenv ())

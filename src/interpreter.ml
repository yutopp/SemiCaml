open Ast
open Analyzer
(* todo *)

type value =
  | IntVal of int
  | FloatVal of float
  | BoolVal of bool
  | FunVal of string list * a_ast * Analyzer.type_kind
  | ANone

let val_to_str values = match values with
  | IntVal n -> Printf.sprintf "%d\n" n
  | FloatVal n -> Printf.sprintf "%f\n" n
  | BoolVal b -> Printf.sprintf "%b\n" b
  | FunVal (_,_,Func types) -> List.fold_left ( ^ ) "func" (List.map (fun t -> Printf.sprintf ":" ^ Analyzer.to_string t) types)
  | _ -> failwith "Undefined Value"


type constant_folder = {
  int : int -> int -> bool;
  float : float -> float -> bool;
  bool : bool -> bool -> bool;
}

let val_table: (string, value) Hashtbl.t = Hashtbl.create 10

let env_ext env x v = Hashtbl.add val_table x v

let lookup x env = Hashtbl.find val_table x

let rec eval' input =
  let intop f e1 e2 = match (eval' e1, eval' e2) with
    | (IntVal n1, IntVal n2) -> IntVal (f n1 n2)
    | _ -> failwith "Integer value expected"
  in
  let floatop f e1 e2 = match (eval' e1, eval' e2) with
    | (FloatVal n1, FloatVal n2) -> FloatVal (f n1 n2)
    | _ -> failwith "float value expected"
  in
  let boolop f e1 e2 = match (eval' e1, eval' e2) with
    | (BoolVal b1, BoolVal b2) -> BoolVal (f b1 b2)
    | _ -> failwith "bool value expected"
  in
  let compop f e1 e2 = match (eval' e1, eval' e2) with
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
  | Flow [e] -> eval' e
  | Flow (e :: rest) -> eval' (Flow rest)
  | Term (e,_) ->
     begin
       match e with
       | IntLiteral n -> IntVal n
       | FloatLiteral r -> FloatVal r
       | BoolLiteral b -> BoolVal b
       | _ -> failwith "not expected type"
     end

  | BinOp (e1,e2,Add Int,_) -> intop ( + ) e1 e2
  | BinOp (e1,e2,Sub Int,_) -> intop ( - ) e1 e2
  | BinOp (e1,e2,Mul Int,_) -> intop ( * ) e1 e2
  | BinOp (e1,e2,Div Int,_) when (eval' e2 = IntVal 0) -> failwith "0 division"
  | BinOp (e1,e2,Div Int,_) -> intop ( / ) e1 e2

  | BinOp (e1,e2,Add Float,_) -> floatop ( +. ) e1 e2
  | BinOp (e1,e2,Sub Float,_) -> floatop ( -. ) e1 e2
  | BinOp (e1,e2,Mul Float,_) -> floatop ( *. ) e1 e2
  | BinOp (e1,e2,Div Float,_) when (eval' e2 = FloatVal 0.) -> failwith "0 Division"
  | BinOp (e1,e2,Div Float,_) -> floatop ( /. ) e1 e2

  | BinOp (e1,e2,OrLogic,_) -> boolop ( || ) e1 e2
  | BinOp (e1,e2,AndLogic,_) -> boolop ( && ) e1 e2

  | BinOp (e1,e2,Eq _,_) -> compop ( eq ) e1 e2
  | BinOp (e1,e2,NotEq _,_) -> compop ( neq ) e1 e2
  | BinOp (e1,e2,Gte _,_) -> compop ( gte ) e1 e2
  | BinOp (e1,e2,Gt _,_) -> compop ( gt ) e1 e2
  | BinOp (e1,e2,Lte _,_) -> compop ( lte ) e1 e2
  | BinOp (e1,e2,Lt _,_) -> compop ( lt ) e1 e2

  | Cond (cond,e1,e2) ->
     begin
       match (eval' cond) with
       | BoolVal true -> eval' e1
       | BoolVal false -> eval' e2
       | _ -> failwith "first exp bool value expected"
     end
  | IdTerm (id,_) -> lookup id val_table
  | VarDecl (id,e1,_,None) -> lookup id (env_ext val_table id (eval' e1))
  | VarDecl (id,e1,_,Some e2) ->
     env_ext val_table id (eval' e1);
     eval' e2
  | FuncDecl (id,args,e1,t,_,None) -> lookup id (env_ext val_table id (FunVal (List.map Analyzer.get_id_of args, e1, t)))
  | FuncDecl (id,args,e1,t,_,Some e2) ->
     (env_ext val_table id (FunVal (List.map Analyzer.get_id_of args, e1, t)));
     eval' e2
  | CallFunc (id,call_args,_) ->
     let func = lookup id val_table in
     begin
       match func with
       | FunVal (pro_args,e1,_) ->
          ignore (List.map2 (fun x v -> env_ext val_table x v) pro_args (List.map eval' call_args));
          eval' e1
       | _ -> failwith "func value expected"
     end
  | _ -> failwith "unknow exp"

let eval input =
  eval' (Analyzer.analyze input)

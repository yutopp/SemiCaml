open Ast
open Analyzer

type value =
  | IntVal of int
  | FloatVal of float
  | BoolVal of bool
  | ArrayVal of value array * Analyzer.type_kind
  | FunVal of string * string list * Analyzer.a_ast * Analyzer.type_kind
  | UnitVal

let rec recursive_to_string list printer delimiter =
  match list with
  | [] -> ""
  | head :: [] -> printer head
  | head :: tail -> (printer head) ^ delimiter ^ (recursive_to_string tail printer delimiter)

let rustic_val_to_str values = match values with
  | IntVal n -> Printf.sprintf "%d" n
  | FloatVal r -> Printf.sprintf "%F" r
  | BoolVal b -> Printf.sprintf "%b" b
  | UnitVal -> Printf.sprintf "()"
  | _ -> failwith "not expected type"

let rec val_to_str values = match values with
  | IntVal n -> Printf.sprintf "- : int = %d" n
  | FloatVal n -> Printf.sprintf "- : float = %F" n
  | BoolVal b -> Printf.sprintf "- : bool = %b" b
  | ArrayVal (arr, tk) ->
     Printf.sprintf
       "- : %s array = [|%s|]"
       (Analyzer.to_string tk)
       (recursive_to_string (Array.to_list arr) rustic_val_to_str "; ")
  | FunVal (name,_,_,Func types) ->
     let dot_pos = String.index name '.' in
     let erase_number_name = String.sub name 0 dot_pos in
     Printf.sprintf
       "val %s : %s = <fun>"
       erase_number_name
       (recursive_to_string types Analyzer.to_string " -> ")
  | UnitVal -> Printf.sprintf "- : unit = ()"
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
    | _ -> failwith "Integer value is expected"
  in
  let floatop f e1 e2 = match (eval' e1, eval' e2) with
    | (FloatVal n1, FloatVal n2) -> FloatVal (f n1 n2)
    | _ -> failwith "float value is expected"
  in
  let boolop f e1 e2 = match (eval' e1, eval' e2) with
    | (BoolVal b1, BoolVal b2) -> BoolVal (f b1 b2)
    | _ -> failwith "bool value is expected"
  in
  let compop f e1 e2 = match (eval' e1, eval' e2) with
    | (IntVal n1, IntVal n2) -> BoolVal (f.int n1 n2)
    | (FloatVal n1, FloatVal n2) -> BoolVal (f.float n1 n2)
    | (BoolVal b1, BoolVal b2) -> BoolVal (f.bool b1 b2)
    | _ -> failwith "bool value is expected"
  in
  let eq = { int = ( = ); float = ( = ); bool = ( = ) } in
  let neq = { int = ( <> ); float = ( <> ); bool = ( <> ) } in
  let lt = { int = ( < ); float = ( < ); bool = ( < ) } in
  let lte = { int = ( <= ); float = ( <= ); bool = ( <= ) } in
  let gt = { int = ( > ); float = ( > ); bool = ( > ) } in
  let gte =  { int = ( >= ); float = ( >= ); bool = ( >= ) } in
  match input with
  | Flow [e] -> eval' e
  | Flow (e :: rest) -> ignore (eval' e); eval' (Flow rest)
  | Seq (e1,e2) -> ignore(eval' e1); eval' e2
  | Term (e,_) ->
     begin
       match e with
       | IntLiteral n -> IntVal n
       | FloatLiteral r -> FloatVal r
       | BoolLiteral b -> BoolVal b
       | UnitLiteral -> UnitVal
       | _ -> failwith "not expected type"
     end

  | BinOp (e1,e2,Add Int,Int) -> intop ( + ) e1 e2
  | BinOp (e1,e2,Sub Int,Int) -> intop ( - ) e1 e2
  | BinOp (e1,e2,Mul Int,Int) -> intop ( * ) e1 e2
  | BinOp (e1,e2,Div Int,Int) when (eval' e2 = IntVal 0) ->
     failwith "0 division"
  | BinOp (e1,e2,Div Int,Int) -> intop ( / ) e1 e2

  | BinOp (e1,e2,Add Float,Float) -> floatop ( +. ) e1 e2
  | BinOp (e1,e2,Sub Float,Float) -> floatop ( -. ) e1 e2
  | BinOp (e1,e2,Mul Float,Float) -> floatop ( *. ) e1 e2
  | BinOp (e1,e2,Div Float,Float) when (eval' e2 = FloatVal 0.) ->
     failwith "0 Division"
  | BinOp (e1,e2,Div Float,Float) -> floatop ( /. ) e1 e2

  | BinOp (e1,e2,OrLogic,Boolean) -> boolop ( || ) e1 e2
  | BinOp (e1,e2,AndLogic,Boolean) -> boolop ( && ) e1 e2

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
       | _ -> failwith "first exp is expected bool value"
     end
  | IdTerm (id,_) -> lookup id val_table
  | VarDecl (id,e1,_,None) -> lookup id (env_ext val_table id (eval' e1))
  | VarDecl (id,e1,_,Some e2) ->
     env_ext val_table id (eval' e1);
     eval' e2
  | FuncDecl (id,args,e1,t,_,None) ->
     let arg_ids = List.map Analyzer.get_id_of
                            args
     in
     lookup id (env_ext val_table id (FunVal (id, arg_ids, e1, t)))
  | FuncDecl (id,args,e1,t,_,Some e2) ->
     let arg_ids = List.map Analyzer.get_id_of
                            args
     in
     (env_ext val_table id (FunVal (id, arg_ids, e1, t)));
     eval' e2
  | CallFunc (id,call_args,_) ->
     let func = lookup id val_table in
     begin
       match func with
       | FunVal (_,pro_args,e1,_) ->
          let evaled_args = List.map eval' call_args in
          ignore (List.map2 (fun x v -> env_ext val_table x v)
                            pro_args
                            evaled_args);
          eval' e1
       | _ -> failwith "func value is expected"
     end
  | ArrayCreate (size,t) ->
     begin
       match (eval' size, t) with
       | (IntVal n, Array Int) -> ArrayVal (Array.make n (IntVal 0), Int)
       | (IntVal n, Array Float) -> ArrayVal (Array.make n (FloatVal 0.), Float)
       | (IntVal n, Array Boolean) -> ArrayVal (Array.make n (BoolVal true), Boolean)
       | _ -> failwith "not expected type"
     end
  | ArrayRef (id,index,t) ->
     begin
       match (eval' index) with
       | IntVal n ->
          let arr = lookup id val_table in
          begin
            match arr with
            | ArrayVal (arr, t) -> arr.(n)
            | _ -> failwith "variable is not ArrayVal"
          end
       | _ -> failwith "array ref expected Int"
     end
  | ArrayAssign (id,index,new_val,_) ->
     begin
       let arr = lookup id val_table in
       begin
         match (eval' index, eval' new_val, arr) with
         | (IntVal n, new_val, ArrayVal (arr, _)) ->
            arr.(n) <- new_val;
            UnitVal
         | _ -> failwith "not expected type"
       end
     end
  | _ -> failwith "unknow exp"

let eval input =
  eval' (Analyzer.analyze input)

let interpreter' input =
  val_to_str (eval input)

let interpreter input =
  print_string (interpreter' input);
  print_newline ()

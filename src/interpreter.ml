open Ast
open Analyzer

type value =
  | IntVal of int
  | FloatVal of float
  | BoolVal of bool
  | ArrayVal of value array * Analyzer.type_kind
  | TopVarVal of string * value * Analyzer.type_kind
  | FunVal of string * string list * Analyzer.a_ast * Analyzer.type_kind
  | IntrinsicFunVal of Analyzer.type_kind list
  | UnitVal

let rec recursive_to_string list printer delimiter =
  match list with
  | [] -> ""
  | head :: [] -> printer head
  | head :: tail -> (printer head) ^ delimiter ^ (recursive_to_string tail printer delimiter)

let rec rustic_val_to_str values = match values with
  | IntVal n -> Printf.sprintf "%d" n
  | FloatVal r -> Printf.sprintf "%F" r
  | BoolVal b -> Printf.sprintf "%b" b
  | ArrayVal (arr, tk) ->
     Printf.sprintf
       "%s array = [|%s|]"
       (Analyzer.to_string tk)
       (recursive_to_string (Array.to_list arr) rustic_val_to_str "; ")
  | UnitVal -> "()"
  | _ -> failwith "not expected type in rustic_val_to_str"

let delete_num_in_str id =
  let dot_pos = String.index id '.' in
  let erase_num_id = String.sub id 0 dot_pos in
  erase_num_id

let rec val_to_str values = match values with
  | IntVal n -> Printf.sprintf "- : int = %d" n
  | FloatVal n -> Printf.sprintf "- : float = %F" n
  | BoolVal b -> Printf.sprintf "- : bool = %b" b
  | ArrayVal (arr, tk) ->
     Printf.sprintf
       "- : %s array = [|%s|]"
       (Analyzer.to_string tk)
       (recursive_to_string (Array.to_list arr) rustic_val_to_str "; ")
  | TopVarVal (id, value, t) ->
     Printf.sprintf
       (* val arr : int array = [||] *)
       "val %s : %s = %s"
       id
       (Analyzer.to_string t)
       (rustic_val_to_str value)
  | FunVal (name,_,_,Analyzer.Func types) ->
     begin
       try
         let name_ = delete_num_in_str name in
         Printf.sprintf
           "val %s : %s = <fun>"
           name_
           (recursive_to_string types Analyzer.to_string " -> ")
       with
       | Not_found ->
          Printf.sprintf
            "val %s : %s = <fun>"
            name
            (recursive_to_string types Analyzer.to_string " -> ")
       | _ -> failwith "not allow id"
     end
  | UnitVal -> Printf.sprintf "- : unit = ()"
  | _ -> failwith "Undefined Value"

type constant_folder = {
  int : int -> int -> bool;
  float : float -> float -> bool;
  bool : bool -> bool -> bool;
}

let val_table: (string, value) Hashtbl.t = Hashtbl.create 10

let env_ext env x v = Hashtbl.add val_table x v

let lookup x env =
  try
    Hashtbl.find val_table x
  with
  | Not_found ->
     begin
       let x_ = delete_num_in_str x in
       try
         Hashtbl.find val_table x_
       with
       | Not_found -> failwith ("undefined variable " ^ x)
     end
  | _ -> failwith ("Hashtbl.find other \"Not_found\" exception with" ^ x)

let parent_func = ref "top_level"

let intrinsic_func =
  ignore(env_ext val_table "print_int" (IntrinsicFunVal [Int;Unit]));
  ignore(env_ext val_table "print_float" (IntrinsicFunVal [Float;Unit]));
  ignore(env_ext val_table "print_bool" (IntrinsicFunVal [Boolean;Unit]));
  ignore(env_ext val_table "print_newline" (IntrinsicFunVal [Unit;Unit]))

let rec eval' input rec_depth =
  let intop f e1 e2 = match (eval' e1 rec_depth, eval' e2 rec_depth) with
    | (IntVal n1, IntVal n2) -> IntVal (f n1 n2)
    | _ -> failwith "Integer value is expected"
  in
  let floatop f e1 e2 = match (eval' e1 rec_depth, eval' e2 rec_depth) with
    | (FloatVal n1, FloatVal n2) -> FloatVal (f n1 n2)
    | _ -> failwith "float value is expected"
  in
  let boolop f e1 e2 = match (eval' e1 rec_depth, eval' e2 rec_depth) with
    | (BoolVal b1, BoolVal b2) -> BoolVal (f b1 b2)
    | _ -> failwith "bool value is expected"
  in
  let compop f e1 e2 = match (eval' e1 rec_depth, eval' e2 rec_depth) with
    | (IntVal n1, IntVal n2) -> BoolVal (f.int n1 n2)
    | (FloatVal n1, FloatVal n2) -> BoolVal (f.float n1 n2)
    | (BoolVal b1, BoolVal b2) -> BoolVal (f.bool b1 b2)
    | _ -> failwith "bool value is expected"
  in
  let add_depth_to_id id depth = id ^ "." ^ string_of_int depth in
  let eq = { int = ( = ); float = ( = ); bool = ( = ) } in
  let neq = { int = ( <> ); float = ( <> ); bool = ( <> ) } in
  let lt = { int = ( < ); float = ( < ); bool = ( < ) } in
  let lte = { int = ( <= ); float = ( <= ); bool = ( <= ) } in
  let gt = { int = ( > ); float = ( > ); bool = ( > ) } in
  let gte =  { int = ( >= ); float = ( >= ); bool = ( >= ) } in
  match input with
  | Flow [e] -> eval' e rec_depth
  | Flow (e :: rest) -> ignore (eval' e rec_depth); eval' (Flow rest) rec_depth
  | Seq (e1,e2) -> ignore (eval' e1 rec_depth); eval' (Flow [e2]) rec_depth
  | Term (e,_) ->
     begin
       match e with
       | IntLiteral n -> IntVal n
       | FloatLiteral r -> FloatVal r
       | BoolLiteral b -> BoolVal b
       | UnitLiteral -> UnitVal
       | _ -> failwith "not expected type in term e1"
     end

  | BinOp (e1,e2,Add Int,Int) -> intop ( + ) e1 e2
  | BinOp (e1,e2,Sub Int,Int) -> intop ( - ) e1 e2
  | BinOp (e1,e2,Mul Int,Int) -> intop ( * ) e1 e2
  | BinOp (e1,e2,Div Int,Int) when (eval' e2 rec_depth = IntVal 0) ->
     failwith "0 division"
  | BinOp (e1,e2,Div Int,Int) -> intop ( / ) e1 e2

  | BinOp (e1,e2,Add Float,Float) -> floatop ( +. ) e1 e2
  | BinOp (e1,e2,Sub Float,Float) -> floatop ( -. ) e1 e2
  | BinOp (e1,e2,Mul Float,Float) -> floatop ( *. ) e1 e2
  | BinOp (e1,e2,Div Float,Float) when (eval' e2 rec_depth = FloatVal 0.) ->
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
       match (eval' cond rec_depth) with
       | BoolVal true -> eval' e1 rec_depth
       | BoolVal false -> eval' e2 rec_depth
       | _ -> failwith "first exp is expected bool value"
     end
  | IdTerm (id,_) ->
     let id_ = add_depth_to_id id rec_depth in
     begin
       match lookup id_ val_table with
       | TopVarVal (_, value, _) -> value
       | x -> x
     end
  | VarDecl (id,e1,t,None) ->
     let id_ = delete_num_in_str id in
     lookup id_ (env_ext val_table id_ (TopVarVal (id_, eval' e1 rec_depth, t)))
  | VarDecl (id,e1,_,Some e2) ->
     let id_ = add_depth_to_id id rec_depth in
     env_ext val_table id_ (eval' e1 rec_depth);
     eval' e2 rec_depth
  | FuncDecl (id,args,e1,t,_,None) ->
     let arg_ids = List.map Analyzer.get_id_of args in
     let id_ = delete_num_in_str id in
     lookup id_ (env_ext val_table id_ (FunVal (id_, arg_ids, e1, t)))
  | FuncDecl (id,args,e1,t,_,Some e2) ->
     let arg_ids = List.map Analyzer.get_id_of args in
     (env_ext val_table id (FunVal (id, arg_ids, e1, t)));
     eval' e2 rec_depth
  | CallFunc (id,call_args,_) ->
     let func = lookup id val_table in
     let evaled_args = List.map (fun arg -> eval' arg rec_depth) call_args in     
     begin
       match func with
       | FunVal (_,pro_args,e1,_) ->
          let recursive_flag = !parent_func = id in
          let parent_func_tmp = !parent_func in
          parent_func := id;
          begin
            match recursive_flag with
            | true ->
               let ex_pro_args = List.map (fun arg -> add_depth_to_id arg (rec_depth + 1)) pro_args  in
               ignore (List.map2 (fun x v -> env_ext val_table x v)
                                 ex_pro_args
                                 evaled_args);
               let result = eval' e1 (rec_depth + 1) in
               parent_func := parent_func_tmp;
               result
            | false ->
               let ex_pro_args = List.map (fun arg -> add_depth_to_id arg rec_depth) pro_args in               
               ignore (List.map2 (fun x v -> env_ext val_table x v)
                                 ex_pro_args
                                 evaled_args);
               let result = eval' e1 rec_depth in
               parent_func := parent_func_tmp;
               result
          end            
       | IntrinsicFunVal _ ->
          begin
            match (id, evaled_args) with
            | ("print_int", [IntVal n]) ->
               print_int n;
               UnitVal
            | ("print_float", [FloatVal r]) ->
               print_float r;
               UnitVal
            | ("print_bool", [BoolVal b]) ->
               Printf.printf "%b" b;
               UnitVal
            | ("print_newline", [UnitVal]) ->
               print_newline ();
               UnitVal
            | _ -> failwith "not march args type"
          end
       | _ -> failwith "func value is expected"
     end
  | ArrayCreate (size,t) ->
     begin
       match (eval' size rec_depth, t) with
       | (IntVal n, Array Int) -> ArrayVal (Array.make n (IntVal 0), Int)
       | (IntVal n, Array Float) -> ArrayVal (Array.make n (FloatVal 0.), Float)
       | (IntVal n, Array Boolean) -> ArrayVal (Array.make n (BoolVal true), Boolean)
       | _ -> failwith "not expected type in ArrayCreate"
     end
  | ArrayRef (id,index,t) ->
     begin
       match (eval' index rec_depth) with
       | IntVal n ->
          let id_ = add_depth_to_id id rec_depth in
          let array = lookup id_ val_table in
          begin
            match array with
            | ArrayVal (arr, _) -> arr.(n)
            | TopVarVal (_,ArrayVal (arr, _),_) -> arr.(n)
            | _ -> failwith "variable is not ArrayVal"
          end
       | _ -> failwith "array ref expected Int"
     end
  | ArrayAssign (id,index,new_val,_) ->
     begin
       let id_ = add_depth_to_id id rec_depth in
       let array = lookup id_ val_table in
       begin
         match (eval' index rec_depth, eval' new_val rec_depth, array) with
         | (IntVal n, new_val, ArrayVal (arr, _)) ->
            arr.(n) <- new_val;
            UnitVal
         | (IntVal n, new_val, TopVarVal (_, ArrayVal (arr, _), _)) ->
            arr.(n) <- new_val;
            UnitVal
         | _ -> failwith "not expected type in ArrayAssign"
       end
     end
  | _ -> failwith "unknow exp"

let eval input =
  eval' (Analyzer.analyze input) 0

let interpreter' input =
  val_to_str (eval input)

let interpreter input =
  print_string (interpreter' input);
  print_newline ()

let top_level_eval input =
  eval' input 0

let top_level_interpreter env input =
  match input with
  | Program xs ->
     print_string (val_to_str (top_level_eval (Flow (List.map
                                                       (fun x -> Analyzer.analyze_as_top_level env
                                                                                               x)
                                                       xs))));
     print_newline ()
  | _ -> failwith "not expected input form"

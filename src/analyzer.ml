open Ast

exception UnexpectedEnvKind
exception UnexpectedEnv
exception UnexpectedAttributedAST of string

exception SemanticError of string


type type_kind =
    Int
  | String
  | Array of type_kind
  | Func of type_kind list
  | IntrinsicFunc of type_kind list
  | Float
  | Boolean
  | Unit
  | Undefined

let rec to_string tk = match tk with
    Int -> "int"
  | String -> "string"
  | Array inner_tk -> "array<" ^ (to_string inner_tk) ^ ">"
  | Func px -> "function"
  | IntrinsicFunc params_tk -> "intrinsic function"
  | Float -> "float"
  | Boolean -> "boolean"
  | Unit -> "unit"
  | Undefined -> "!undefined"

let dump_type_kind tk =
  Printf.printf "%s" (to_string tk)


type environment =
    EModule of symbol_table
  | ETemp of environment * int * symbol_table
  | EItem of environment * string * type_kind * int * symbol_table
  | ETerm of string * type_kind * int
and symbol_table = (string, environment) Hashtbl.t

let depth_of env = match env with
    EModule _ -> 0
  | ETemp (_, d, _) -> d
  | EItem (_, _, _, d, _) -> d
  | ETerm (_, _, d) -> d

let id_of env = match env with
    EItem (_, id, _, _, _) -> id
  | ETerm (id, _, _) -> id
  | _ -> raise UnexpectedEnvKind

let rec lookup env name =
  let find sym =
    try
      Some (Hashtbl.find sym name, sym)
    with
      Not_found -> None
  in
  let find_with_retry p_env sym =
    let fe = find sym in
    match fe with
      None -> lookup p_env name
    | v -> v
  in
  match env with
    EModule sym -> find sym
  | ETemp (p_env, _, sym) -> find_with_retry p_env sym
  | EItem (p_env, _, _, _, sym) -> find_with_retry p_env sym
  | ETerm (_, _, _) -> None


let flatten_symbol_table: (string, environment) Hashtbl.t = Hashtbl.create 10
let save_env_as_flat id env =
  begin
    try
      let _ = Hashtbl.find flatten_symbol_table id in
      Hashtbl.remove flatten_symbol_table id
    with
      _ -> ()
  end;
  Hashtbl.add flatten_symbol_table id env

let find_tk_from_id id =
  let env = Hashtbl.find flatten_symbol_table id in
  match env with
    EItem (_, _, tk, _, _) -> tk
  | ETerm (_, tk, _) -> tk
  | _ -> raise UnexpectedEnv


let update_type sym id target tk =
  Hashtbl.remove sym id;
  let n = match target with
      EItem (e, s, _, depth, st) -> EItem (e, s, tk, depth, st)
    | ETerm (s, _, depth) -> ETerm (s, tk, depth)
    | _ -> raise UnexpectedEnv
  in
  Hashtbl.add sym id n;
  Printf.printf "NEW! %s: %s\n" id (to_string tk);
  save_env_as_flat id n


let rec dump_env ?(offset=0) env = match env with
    EModule sym ->
    begin
      Printf.printf("module\n");
      dump_sym ~offset:(offset+1) sym
    end

  | ETemp (_, depth, sym) ->
     begin
       dump_sym ~offset:(offset+1) sym
     end

  | EItem (_, id, tk, depth, sym) ->
     begin
       dump_type_kind tk;
       Printf.printf " - id(%s) depth=%d\n" id depth;
       dump_sym ~offset:(offset+1) sym
     end

  | ETerm (id, tk, depth) ->
     begin
       dump_type_kind tk;
       Printf.printf " - id(%s) depth=%d\n" id depth;
     end

and dump_sym ?(offset=0) sym =
  let off_s = String.make (offset*2) ' ' in
  Hashtbl.iter (fun k v ->
                Printf.printf "%skey: %s :: " off_s k;
                dump_env ~offset:(offset+1) v;
                Printf.printf "\n%s---------\n" off_s;
               ) sym

let make_tmp_env env =
  let nd = (depth_of env) + 1 in
  match env with
    EModule _ -> ETemp(env, nd, (Hashtbl.create 10))
  | EItem _ -> ETemp(env, nd, (Hashtbl.create 10))
  | ETemp _ -> ETemp(env, nd, (Hashtbl.create 10))
  | _ -> raise UnexpectedEnvKind

let get_sym_table env = match env with
    EModule sym -> sym
  | ETemp (_, _, sym) -> sym
  | EItem (_, _, _, _, sym) -> sym
  | _ -> raise UnexpectedEnvKind

let index = ref 0

let save_item parent_env name tk target_sym depth =
  let p_sym = get_sym_table parent_env in
  let id = Printf.sprintf "%s.%d" name !index in
  let new_env = EItem (parent_env, id, tk, depth, target_sym) in

  Hashtbl.add p_sym name new_env;
  save_env_as_flat id  new_env;

  index := !index + 1;
  id

let save_term_item parent_env name tk depth =
  let p_sym = get_sym_table parent_env in
  let id = Printf.sprintf "%s.%d" name !index in
  let new_env = ETerm (id, tk, depth) in

  Hashtbl.add p_sym name new_env;
  save_env_as_flat id new_env;

  index := !index + 1;
  id


let save_intrinsic_term_item parent_env name tk =
  let p_sym = get_sym_table parent_env in
  let new_env = ETerm (name, tk, 0) in

  Hashtbl.add p_sym name new_env;

  name


type t =
    GT
  | LT
  | GTE
  | LTE
  | EQ
  | NEQ

type operator =
    Add of type_kind
  | Sub of type_kind
  | Mul of type_kind
  | Div of type_kind
  | OrLogic
  | AndLogic
  | Eq of type_kind
  | NotEq of type_kind
  | Gte of type_kind
  | Gt of type_kind
  | Lte of type_kind
  | Lt of type_kind

let make_op tag tk = match tag with
    GT -> Gt tk
  | LT -> Lt tk
  | GTE -> Gte tk
  | LTE -> Lte tk
  | EQ -> Eq tk
  | NEQ -> NotEq tk


type a_ast =
    Flow of a_ast list
  | Term of ast * type_kind
  | BinOp of a_ast * a_ast * operator * type_kind
  | Cond of a_ast * a_ast * a_ast
  | CallFunc of string * a_ast list * type_kind
  | ArrayCreate of a_ast * type_kind
  | ArrayRef of string * a_ast * type_kind
  | ArrayAssign of string * a_ast * a_ast * type_kind
  | VarDecl of string * a_ast * type_kind * a_ast option
  | FuncDecl of string * a_ast list * a_ast * type_kind * (string * int) list * a_ast option
  | Seq of a_ast * a_ast
  | IdTerm of string * type_kind
  | ANone


let get_id_of a = match a with
    Flow _ -> raise (UnexpectedAttributedAST "Flow")
  | Term _ -> raise (UnexpectedAttributedAST "Term")
  | BinOp _ -> raise (UnexpectedAttributedAST "BinOp")
  | Cond _ -> raise (UnexpectedAttributedAST "Cond")
  | CallFunc (id, _, _) -> id
  | ArrayCreate _ -> raise (UnexpectedAttributedAST "ArrayCreate")
  | ArrayRef _ -> raise (UnexpectedAttributedAST "ArrayRef")
  | ArrayAssign _ -> raise (UnexpectedAttributedAST "ArrayAssign")
  | VarDecl (id, _, _, _) -> id
  | FuncDecl (id, _, _, _, _, _) -> id
  | Seq _ -> raise (UnexpectedAttributedAST "Seq")
  | IdTerm (id, _) -> id
  | ANone -> raise (UnexpectedAttributedAST "None")

let rec type_kind_of a = match a with
    Flow _ -> raise (UnexpectedAttributedAST "Flow")
  | Term (_, tk) -> tk
  | BinOp (_, _, _, tk) -> tk
  | Cond (cond, a, b) -> type_kind_of a
  | CallFunc (_, _, tk) -> tk
  | ArrayCreate (_, tk) -> tk
  | ArrayRef (_, _, tk) -> tk
  | ArrayAssign (_, _, _, tk) -> tk
  | VarDecl (_, _, tk, None) -> tk
  | VarDecl (_, _, tk, Some ia) -> type_kind_of ia
  | FuncDecl (_, _, _, tk, _, None) -> tk
  | FuncDecl (_, _, _, tk, _, Some ia) -> type_kind_of ia
  | Seq (lhs, rhs) -> type_kind_of rhs
  | IdTerm (_, tk) -> tk
  | ANone -> raise (UnexpectedAttributedAST "ANone")


let rec analyze' ast env depth ottk oenc =
  let term_check ast tk ottk = match ottk with
      (* if expected type is specified and actual type is different from expected type, it is error *)
      Some ttk -> if tk = ttk then Term (ast, tk) else raise (SemanticError "type is not matched")
    | None -> Term (ast, tk)
  in
  let binary_op_check ast lhs rhs env op tk =
    let l = analyze' lhs env depth (Some tk) oenc in
    let r = analyze' rhs env depth (Some tk) oenc in
    BinOp (l, r, op, tk)
  in
  let cond_binary_op ast lhs rhs env tag =
    let l = analyze' lhs env depth None oenc in
    let r = analyze' rhs env depth None oenc in
    let op = match (type_kind_of l, type_kind_of r) with
        (Int, Int) -> make_op tag Int
      | (Float, Float) -> make_op tag Float
      | (_, _) -> raise (SemanticError "invalid binary operation")
    in
    BinOp (l, r, op, Boolean)
  in
  let logic_binary_op ast lhs rhs env op =
    let l = analyze' lhs env depth (Some Boolean) oenc in
    let r = analyze' rhs env depth (Some Boolean) oenc in
    BinOp (l, r, op, Boolean)
  in
  let get_id_and_tk env = match env with
      EItem (_, id, tk, depth, _) -> (id, tk)
    | ETerm (id, tk, depth) -> (id, tk)
    | _ -> raise (SemanticError "[ice] invalid env kind")
  in
  let reference_array name index =
    let oe = lookup env name in
    match oe with
      Some (e, _) ->
      begin
        let array_get id tk = match tk with
            Array inner_tk ->
            begin
              let a_index = analyze' index env depth (Some Int) oenc in
              match (type_kind_of a_index) = Int with
                true -> ArrayRef (id, a_index, inner_tk)
              | _ -> raise (SemanticError "type of array index is mismatched")
            end
          | _ -> raise (SemanticError "id is not array")
        in
        match e with
          EItem (_, id, tk, depth, _) -> array_get id tk
        | ETerm (id, tk, depth) -> array_get id tk
        | _ -> raise (SemanticError "[ice] invalid env kind")
      end
    | None -> raise (SemanticError "array id was not found")
  in

  match ast with
    VerDecl (name, expr, in_clause) ->
    begin
      let inner_depth = depth + 1 in
      let v_env = make_tmp_env env in
      let attr_ast = analyze' expr v_env inner_depth None oenc in
      let tk = type_kind_of attr_ast in
      match in_clause with
        Some a ->
        begin
          (* analyze 'in' clause. hide the environment, so name cannot be seen from outside *)
          let inner_env = make_tmp_env env in
          let id = save_item inner_env name tk (get_sym_table v_env) inner_depth in
          let c_a = analyze' a inner_env inner_depth None oenc in
          VarDecl (id, attr_ast, tk, Some c_a)
        end
      | None ->
         begin
           let id = save_item env name tk (get_sym_table v_env) inner_depth in
           VarDecl (id, attr_ast, tk, None)
         end
    end

  | FuncDecl (name, params, expr, in_clause) ->
     begin
       let inner_depth = depth + 1 in
       let decl_param_var v_name f_env =
         let tk = Undefined in
         let id = save_term_item f_env v_name tk inner_depth in
         VarDecl (id, ANone, tk, None)
       in
       let complete_param aast = match aast with
           VarDecl (id, inner, tk, None) ->
           begin
             (* check and update types to determine types of parameters. types will be inferred by analyzing the function body. *)
             let ptk = find_tk_from_id id in
             match ptk with
               Undefined -> raise (SemanticError (Printf.sprintf "type of %s cannot be determined." id))
             | _ -> VarDecl (id, ANone, ptk, None)
           end
         | _ -> raise (SemanticError "[ice]")
       in
       let f_env = make_tmp_env env in
       let incomplete_param_envs = List.map (fun p -> decl_param_var p f_env) params in
       let captured_envs = ref [] in
       let attr_ast = analyze' expr f_env inner_depth None (Some captured_envs) in
       let ret_tk = type_kind_of attr_ast in
       let param_nodes = List.map (fun a -> complete_param a) incomplete_param_envs in
       let id_and_indexes = List.mapi (fun i e -> (id_of e, i)) !captured_envs in
       let tk = Func ((List.map (fun a -> type_kind_of a) param_nodes) @ [ret_tk]) in
       match in_clause with
         Some a ->
         begin
           (* analyze 'in' clause. hide the environment, so name cannot be seen from outside *)
           let inner_env = make_tmp_env env in
           let id = save_item inner_env name tk (get_sym_table f_env) inner_depth in
           let c_a = analyze' a inner_env inner_depth None oenc in
           FuncDecl (id, param_nodes, attr_ast, tk, id_and_indexes, Some c_a)
         end
       | None ->
          begin
            let id = save_item env name tk (get_sym_table f_env) inner_depth in
            FuncDecl (id, param_nodes, attr_ast, tk, id_and_indexes, None)
          end
     end

  | Sequence (lhs, rhs) ->
     begin
       let l_attr = analyze' lhs env depth None oenc in
       let r_attr = analyze' rhs env depth None oenc in
       Seq (l_attr, r_attr)
     end

  | ArrayNew (typename, element_num) ->
     begin
       let inner_tk = match typename with
           "int" -> Int
         | "float" -> Float
         | "bool" -> Boolean
         | _ -> raise (SemanticError (Printf.sprintf "type of %s cannot be used for Array." typename))
       in
       let n = analyze' element_num env depth (Some Int) oenc in
       let tk = Array inner_tk in
       ArrayCreate (n, tk)
     end

  | CondExpr (cond, a, b) ->
     begin
       let cond_attr = analyze' cond env depth (Some Boolean) oenc in
       if (type_kind_of cond_attr) = Boolean then
         begin
           let a_attr = analyze' a env depth None oenc in
           let b_attr = analyze' b env depth None oenc in
           match (type_kind_of a_attr) = (type_kind_of b_attr) with
             true -> Cond (cond_attr, a_attr, b_attr)
           | _ -> raise (SemanticError "types of rhs or lhs is different")
         end
       else raise (SemanticError "condition must be 'boolean'")
     end

  | LogicOrExpr (lhs, rhs) -> logic_binary_op ast lhs rhs env OrLogic
  | LogicAndExpr (lhs, rhs) -> logic_binary_op ast lhs rhs env AndLogic

  | EqualExpr (lhs, rhs) -> cond_binary_op ast lhs rhs env EQ
  | NotEqualExpr (lhs, rhs) -> cond_binary_op ast lhs rhs env NEQ

  | LessExpr (lhs, rhs) -> cond_binary_op ast lhs rhs env LT
  | LessEqualExpr (lhs, rhs) -> cond_binary_op ast lhs rhs env LTE
  | GreaterExpr (lhs, rhs) -> cond_binary_op ast lhs rhs env GT
  | GreaterEqualExpr (lhs, rhs) -> cond_binary_op ast lhs rhs env GTE

  | AddIntExpr (lhs, rhs) -> binary_op_check ast lhs rhs env (Add Int) Int
  | SubIntExpr (lhs, rhs) -> binary_op_check ast lhs rhs env (Sub Int) Int
  | MulIntExpr (lhs, rhs) -> binary_op_check ast lhs rhs env (Mul Int) Int
  | DivIntExpr (lhs, rhs) -> binary_op_check ast lhs rhs env (Div Int) Int
  | AddFloatExpr (lhs, rhs) -> binary_op_check ast lhs rhs env (Add Float) Float
  | SubFloatExpr (lhs, rhs) -> binary_op_check ast lhs rhs env (Sub Float) Float
  | MulFloatExpr (lhs, rhs) -> binary_op_check ast lhs rhs env (Mul Float) Float
  | DivFloatExpr (lhs, rhs) -> binary_op_check ast lhs rhs env (Div Float) Float

  | IntLiteral _ -> term_check ast Int ottk
  | FloatLiteral _ -> term_check ast Float ottk
  | BoolLiteral _ -> term_check ast Boolean ottk
  | UnitLiteral -> term_check ast Unit ottk

  | ArrayGet (name, index) -> reference_array name index

  | ArrayAssign (name, index, expr) ->
     begin
       let elem = reference_array name index in
       match elem with
         ArrayRef (id, a_index, inner_tk) ->
         begin
           let a_value = analyze' expr env depth (Some inner_tk) oenc in
           match (type_kind_of a_value) = inner_tk with
             true -> ArrayAssign (id, a_index, a_value, Unit)
           | _ -> raise (SemanticError "type of rhs of array is mismatched")
         end
       | _ -> raise (SemanticError "ice")
     end

  | FuncCall (name, args) ->
     begin
       let call_function e id params =
         let params_len = List.length params in
         if List.length params < 1 then raise (SemanticError "Function must have at least 1 param");
         if List.length args <> (params_len - 1) then raise (SemanticError "langth of args and params is different");
         (* check semantics of args *)
         let eval_arg i a =
           let param_tk = (List.nth params i) in
           let ea = analyze' a env depth (Some param_tk) oenc in
           match (type_kind_of ea) = param_tk with
             true -> ea
           | _ -> raise (SemanticError (Printf.sprintf "type of index %d is mismatched" i))
         in
         let a_args = List.mapi eval_arg args in
         let return_ty = List.nth params (params_len - 1) in
         CallFunc (id, a_args, return_ty)
       in
       let oe = lookup env name in
       match oe with
         Some (e, _) ->
         begin
           let apply id tk = match tk with
               Func params -> call_function e id params
             | IntrinsicFunc params -> call_function e id params
             | _ -> raise (SemanticError "function is not callable")
           in
           match e with
             EItem (_, id, tk, depth, _) -> apply id tk
           | ETerm (id, tk, depth) -> apply id tk
           | _ -> raise (SemanticError "[ice] invalid env kind")
         end
       | None -> raise (SemanticError "function id was not found")
     end

  | Id name ->
     begin
       let oe = lookup env name in
       match oe with
         Some (e, sym) ->
         begin
           let id, tk = get_id_and_tk e in
           let id_depth = depth_of e in
           (* if depth of symbol that is found is shallower than depth of current expression, that symbol is placed at the out of current scope. When we are analyzing 'function', we must capture these values to make a closure *)
           if id_depth < depth then
             begin
               match oenc with
                 Some enc ->
                 begin
                   (* enc holds env of captured values *)
                   enc := !enc @ [e]
                 end
               | _ -> ()
             end;

           match ottk with
             Some ttk ->
             begin
               if tk = Undefined then
                 begin
                   (* update typeinfo to determine unsolved type *)
                   update_type sym id e ttk;
                   IdTerm (id, ttk)
                 end
               else if tk <> ttk then
                 raise (SemanticError "type missmatch")
               else
                 IdTerm (id, tk)
             end
           | None -> IdTerm (id, tk)
         end
       | None -> raise (SemanticError (Printf.sprintf "id(%s) was not found" name))
     end

  | _ -> raise (SemanticError "Unsupported AST")

let init_analyzer () =
  (* environment for the module *)
  let env = EModule (Hashtbl.create 10) in
  (* register intrinsic functions to the symbol table *)
  ignore (save_intrinsic_term_item env "print_int" (IntrinsicFunc [Int; Unit]));
  ignore (save_intrinsic_term_item env "print_bool" (IntrinsicFunc [Boolean; Unit]));
  ignore (save_intrinsic_term_item env "print_float" (IntrinsicFunc [Float; Unit]));
  ignore (save_intrinsic_term_item env "print_newline" (IntrinsicFunc [Unit; Unit]));

  env

let analyze_as_top_level env node =
  analyze' node env 0 None None

let analyze ast =
  let env = init_analyzer () in
  let attr_ast = match ast with
      Program xs -> Flow (List.map (fun x -> analyze_as_top_level env x) xs)
    | _ -> raise (SemanticError "some exceptions are raised")
  in

  (* debug *)
  dump_env env;

  (* result *)
  attr_ast

open Ast

type type_kind =
    Int
  | String
  | Array of type_kind
  | Func of type_kind list
  | IntrinsicFunc of string * type_kind list
  | Float
  | Boolean
  | Unit
  | Undefined

let rec to_string tk = match tk with
    Int -> "int"
  | String -> "string"
  | Array inner -> "array<" ^ (to_string tk) ^ ">"
  | Func px -> "function"
  | IntrinsicFunc (name, params) -> "intrinsic function"
  | Float -> "float"
  | Boolean -> "boolean"
  | Unit -> "unit"
  | Undefined -> "!undefined"

let dump_type_kind tk =
  Printf.printf "%s" (to_string tk)


exception UnexpectedEnvKind

type environment =
    EModule of symbol_table
  | ETemp of environment * symbol_table
  | EItem of environment * string * type_kind * symbol_table
  | ETerm of string * type_kind

and symbol_table = (string, environment) Hashtbl.t


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
  | ETemp(p_env, sym) -> find_with_retry p_env sym
  | EItem(p_env, _, _, sym) -> find_with_retry p_env sym
  | ETerm(_, _) -> None


exception UnexpectedEnv

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
    EItem (_, _, tk, _) -> tk
  | ETerm (_, tk) -> tk
  | _ -> raise UnexpectedEnv


let update_type sym id target tk =
  Hashtbl.remove sym id;
  let n = match target with
      EItem (e, s, _, st) -> EItem (e, s, tk, st)
    | ETerm (s, _ ) -> ETerm (s, tk)
    | _ -> raise UnexpectedEnv
  in
  Hashtbl.add sym id n;
  Printf.printf "NEW! %s: %s\n" id (to_string tk);
  save_env_as_flat id n


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
  | CallFunc of string * a_ast list * type_kind
  | VarDecl of string * a_ast * type_kind * a_ast option
  | FuncDecl of string * a_ast list * a_ast * type_kind * a_ast option
  | IdTerm of string * type_kind
  | ANone

exception InvaridAttributedAST

let type_kind_of a = match a with
    Term (_, tk) -> tk
  | BinOp (_, _, _, tk) -> tk
  | CallFunc (_, _, tk) -> tk
  | VarDecl (_, _, tk, None) -> tk
  | FuncDecl (_, _, _, tk, None) -> tk
  | IdTerm (_, tk) -> tk
  | _ -> raise InvaridAttributedAST

let get_id_of a = match a with
    CallFunc (id, _, _) -> id
  | VarDecl (id, _, _, _) -> id
  | FuncDecl (id, _, _, _, _) -> id
  | IdTerm (id, _) -> id
  | _ -> raise UnexpectedEnvKind


let rec dump_env ?(offset=0) env = match env with
    EModule sym -> (
    Printf.printf("module\n");
    dump_sym ~offset:(offset+1) sym
  )
  | ETemp(_, sym) -> (
  )
  | EItem(_, id, tk, sym) ->
     begin
       dump_type_kind tk;
       Printf.printf " - id(%s)\n" id;
       dump_sym ~offset:(offset+1) sym
     end

  | ETerm (id, tk) ->
     begin
       dump_type_kind tk;
       Printf.printf " - id(%s)\n" id;
     end

and dump_sym ?(offset=0) sym =
  let off_s = String.make (offset*2) ' ' in
  Hashtbl.iter (fun k v ->
                Printf.printf "%skey: %s :: " off_s k;
                dump_env ~offset:(offset+1) v;
                Printf.printf "\n%s---------\n" off_s;
               ) sym

exception InvaridEnv

let make_tmp_env env = match env with
    EModule sym -> ETemp(env, (Hashtbl.create 10))
  | EItem(p_env, _, _, sym) -> ETemp(p_env, (Hashtbl.create 10))
  | ETemp(p_env, sym) -> ETemp(p_env, (Hashtbl.create 10))
  | _ -> raise InvaridEnv

let get_sym_table env = match env with
    EModule sym -> sym
  | ETemp(_, sym) -> sym
  | EItem(_, _, _, sym) -> sym
  | _ -> raise InvaridEnv

let index = ref 0

let save_item parent_env name tk target_sym =
  let p_sym = get_sym_table parent_env in
  let id = Printf.sprintf "%s.%d" name !index in
  let new_env = EItem(parent_env, id, tk, target_sym) in

  Hashtbl.add p_sym name new_env;
  save_env_as_flat id  new_env;

  index := !index + 1;
  id

let save_term_item parent_env name tk =
  let p_sym = get_sym_table parent_env in
  let id = Printf.sprintf "%s.%d" name !index in
  Printf.printf "id: %s\n" id;
  let new_env = ETerm (id, tk) in

  Hashtbl.add p_sym name new_env;
  save_env_as_flat id new_env;

  index := !index + 1;
  id


let save_intrinsic_term_item parent_env name tk =
  let p_sym = get_sym_table parent_env in
  let new_env = ETerm (name, tk) in

  Hashtbl.add p_sym name new_env;

  name



exception SemanticError of string

let rec analyze' ast env ottk =
  let term_check ast tk ottk = match ottk with
      Some ttk -> if tk = ttk then Term (ast, tk) else raise (SemanticError "is not matched")
    | None -> Term (ast, tk)
  in
  let binary_op_check ast lhs rhs env op tk =
    let l = analyze' lhs env (Some tk) in
    let r = analyze' rhs env (Some tk) in
    BinOp (l, r, op, tk)
  in
  let cond_binary_op ast lhs rhs env tag =
    let l = analyze' lhs env None in
    let r = analyze' rhs env None in
    let op = match (type_kind_of l, type_kind_of r) with
        (Int, Int) -> make_op tag Int
      | (_, _) -> raise (SemanticError "invalid binary operation")
    in
    BinOp (l, r, op, Boolean)
  in
  let logic_binary_op ast lhs rhs env op =
    let l = analyze' lhs env (Some Boolean) in
    let r = analyze' rhs env (Some Boolean) in
    BinOp (l, r, op, Boolean)
  in
  let get_id_and_tk env = match env with
      EItem (_, id, tk, _) -> (id, tk)
    | ETerm (id, tk) -> (id, tk)
    | _ -> raise (SemanticError "[ice] invalid env kind")
  in
  match ast with
    VerDecl (name, expr, in_clause) ->
    begin
      let inner_env = make_tmp_env env in
      let attr_ast = analyze' expr inner_env None in
      let tk = type_kind_of attr_ast in
      match in_clause with
        Some a ->
        begin
          let dummy_id = save_item inner_env name tk (get_sym_table inner_env) in
          let c_a = analyze' a inner_env None in
          VarDecl (dummy_id, attr_ast, tk, Some c_a)
        end
      | None ->
         begin
           let id = save_item env name tk (get_sym_table inner_env) in
           VarDecl (id, attr_ast, tk, None)
         end
    end

  | FuncDecl (name, params, expr, in_clause) ->
     begin
       let decl_param_var v_name f_env =
         let tk = Undefined in
         let id = save_term_item f_env v_name tk in
         Printf.printf "> %s\n" id;
         VarDecl (id, ANone, tk, None)
       in
       let complete_param aast = match aast with
           VarDecl (id, inner, tk, None) ->
           begin
             Printf.printf "%s: %s [%s]\n" id (to_string tk) (to_string (find_tk_from_id id));
             VarDecl (id, ANone, find_tk_from_id id, None)
           end
         | _ -> raise (SemanticError "[ice]")
       in
       let f_env = make_tmp_env env in
       let incomplete_param_envs = List.map (fun p -> decl_param_var p f_env) params in
       let attr_ast = analyze' expr f_env None in
       let ret_tk = type_kind_of attr_ast in
       Printf.printf "ret ty: %s\n" (to_string ret_tk);
       let param_nodes = List.map (fun a -> complete_param a) incomplete_param_envs in
       let tk = Func ((List.map (fun a -> type_kind_of a) param_nodes) @ [ret_tk]) in
       match in_clause with
         Some a ->
         begin
           let dummy_id = save_item f_env name tk (get_sym_table f_env) in
           let c_a = analyze' a f_env None in
           FuncDecl (dummy_id, param_nodes, attr_ast, tk, Some c_a)
         end
       | None ->
          begin
            let id = save_item env name tk (get_sym_table f_env) in
            FuncDecl (id, param_nodes, attr_ast, tk, None)
          end
     end

  | CondExpr (cond, a, b) -> (
    let cond_attr = analyze' cond env (Some Boolean) in
    if (type_kind_of cond_attr) = Boolean then (
      let a_attr = analyze' a env None in
      let b_attr = analyze' b env None in
      if (type_kind_of a_attr) = (type_kind_of b_attr) then
        Term (ast, type_kind_of a_attr)
      else raise (SemanticError "types of rhs or lhs is different")
    ) else raise (SemanticError "condition must be 'boolean'")
  )

  | LogicOrExpr(lhs, rhs) -> logic_binary_op ast lhs rhs env OrLogic
  | LogicAndExpr(lhs, rhs) -> logic_binary_op ast lhs rhs env AndLogic

  | EqualExpr(lhs, rhs) -> cond_binary_op ast lhs rhs env EQ
  | NotEqualExpr(lhs, rhs) -> cond_binary_op ast lhs rhs env NEQ

  | LessExpr(lhs, rhs) -> cond_binary_op ast lhs rhs env LT
  | LessEqualExpr(lhs, rhs) -> cond_binary_op ast lhs rhs env LTE
  | GreaterExpr(lhs, rhs) -> cond_binary_op ast lhs rhs env GT
  | GreaterEqualExpr(lhs, rhs) -> cond_binary_op ast lhs rhs env GTE

  | AddIntExpr(lhs, rhs) -> binary_op_check ast lhs rhs env (Add Int) Int
  | SubIntExpr(lhs, rhs) -> binary_op_check ast lhs rhs env (Sub Int) Int
  | MulIntExpr(lhs, rhs) -> binary_op_check ast lhs rhs env (Mul Int) Int
  | DivIntExpr(lhs, rhs) -> binary_op_check ast lhs rhs env (Div Int) Int
  | AddFloatExpr(lhs, rhs) -> binary_op_check ast lhs rhs env (Add Float) Float
  | SubFloatExpr(lhs, rhs) -> binary_op_check ast lhs rhs env (Sub Float) Float
  | MulFloatExpr(lhs, rhs) -> binary_op_check ast lhs rhs env (Mul Float) Float
  | DivFloatExpr(lhs, rhs) -> binary_op_check ast lhs rhs env (Div Float) Float

  | IntLiteral _ -> term_check ast Int ottk
  | FloatLiteral _ -> term_check ast Float ottk
  | BoolLiteral _ -> term_check ast Boolean ottk

  | FuncCall(name, args) ->
     begin
       let call_function e id params =
         let params_len = List.length params in
         if List.length params < 1 then raise (SemanticError "Function must have at least 1 param");
         if List.length args <> (params_len - 1) then raise (SemanticError "langth of args and params is different");
         (* check semantics of args *)
         let a_args = List.mapi (fun i a -> analyze' a env (Some (List.nth params i))) args in
         CallFunc (id, a_args, (List.nth params (params_len - 1)))
       in
       let oe = lookup env name in
       match oe with
         Some (e, _) ->
         begin
           let apply id tk = match tk with
               Func params -> call_function e id params
             | IntrinsicFunc (_, params) -> call_function e id params
             | _ -> raise (SemanticError "function is not callable")
           in
           match e with
             EItem (_, id, tk, _) -> apply id tk
           | ETerm (id, tk) -> apply id tk
           | _ -> raise (SemanticError "[ice] invalid env kind")
         end
       | None -> raise (SemanticError "function id was not found")
     end

  | Id name -> (
    let oe = lookup env name in
    match oe with
      Some (e, sym) ->
      begin
        let id, tk = get_id_and_tk e in
        match ottk with
          Some ttk ->
          begin
            Printf.printf ">> %s\n" id;

            if tk = Undefined then
              begin
                (* update typekind... *)
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
    | None -> raise (SemanticError "id was not found")
  )
  | _ -> raise (SemanticError "unsupported")

let analyze ast =
  let env = EModule (Hashtbl.create 10) in
  ignore (save_intrinsic_term_item env "print_int" (IntrinsicFunc ("print", [Int; Unit])));

  let attr_ast = match ast with
      Program xs -> Flow (List.map (fun x -> analyze' x env None) xs)
    | _ -> raise (SemanticError "some exceptions are raised")
  in
  dump_env env;
  attr_ast

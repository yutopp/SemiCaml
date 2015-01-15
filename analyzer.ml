open Ast

type type_kind =
    Int
  | String
  | Array of type_kind
  | IntrinsicFunc of string * type_kind list
  | Float
  | Boolean
  | Unit

let rec to_string tk = match tk with
    Int -> "int"
  | String -> "string"
  | Array inner -> "array<" ^ (to_string tk) ^ ">"
  | IntrinsicFunc (name, params) -> "intrinsic function"
  | Float -> "float"
  | Boolean -> "boolean"
  | Unit -> "unit"

let dump_type_kind tk =
  Printf.printf "%s" (to_string tk)

type environment =
    EModule of symbol_table
  | ETemp of environment * symbol_table
  | EItem of environment * string * type_kind * symbol_table
  | ETerm of string * type_kind

and symbol_table = (string, environment) Hashtbl.t

let rec lookup env name =
  let find sym =
    try
      Some (Hashtbl.find sym name)
    with
      Not_found -> None
  in
  let find_retry p_env sym =
    let fe = find sym in
    match fe with
      None -> lookup p_env name
    | v -> v
  in
  match env with
    EModule sym -> find sym
  | ETemp(p_env, sym) -> find_retry p_env sym
  | EItem(p_env, _, _, sym) -> find_retry p_env sym
  | ETerm(_, _) -> None

type a_ast =
    Flow of a_ast list
  | Term of ast * type_kind
  | BinOp of a_ast * a_ast * type_kind
  | CallFunc of string * a_ast list * type_kind(* return type *)
  | VarDecl of string * a_ast * type_kind
  | IdTerm of ast * string * type_kind

exception InvaridAttributedAST

let type_kind_of a = match a with
    Term(_, tk) -> tk
  | BinOp(_, _, tk) -> tk
  | IdTerm(_, _, tk) -> tk
  | VarDecl(_, _, tk) -> tk
  | _ -> raise InvaridAttributedAST


let rec dump_env ?(offset=0) env = match env with
    EModule sym -> (
    Printf.printf("module\n");
    dump_sym ~offset:(offset+1) sym
  )
  | ETemp(_, sym) -> (
  )
  | EItem(_, id, tk, sym) -> (
    dump_type_kind tk;
    Printf.printf "id(%s)" id;
    dump_sym ~offset:(offset+1) sym
  )
  | ETerm (id, tk) -> dump_type_kind tk

and dump_sym ?(offset=0) sym =
  let off_s = String.make (offset*2) ' ' in
  Hashtbl.iter (fun k v ->
                Printf.printf "%skey: %s :: " off_s k;
                dump_env ~offset:(offset+1) v
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
  index := !index + 1;
  id

let save_term_item parent_env name tk =
  let p_sym = get_sym_table parent_env in
  let new_env = ETerm (name, tk) in
  Hashtbl.add p_sym name new_env

let get_type_kind a = match a with
    Term(_, tk) -> tk
  | _ -> Unit


exception SemanticError of string

let rec analyze' ast env ottk =
  let term_check ast tk ottk = match ottk with
      Some ttk -> if tk = ttk then Term (ast, tk) else raise (SemanticError "is not matched")
    | None -> Term (ast, tk)
  in
  let binary_op_check ast lhs rhs env tk =
    let l = analyze' lhs env (Some tk) in
    let r = analyze' rhs env (Some tk) in
    BinOp (l, r, tk)
  in
  let cond_binary_op ast lhs rhs env =
    let l = analyze' lhs env None in
    let r = analyze' rhs env None in
    BinOp (l, r, Boolean)
  in
  let logic_binary_op ast lhs rhs env =
    let l = analyze' lhs env (Some Boolean) in
    let r = analyze' rhs env (Some Boolean) in
    BinOp (l, r, Boolean)
  in
  let get_id_and_tk env = match env with
      EItem (_, id, tk, _) -> (id, tk)
    | _ -> raise (SemanticError "[ice] invalid env kind")
  in
  match ast with
    Seq xs -> Flow(List.map (fun x -> analyze' x env None) xs)
  | VerDecl(name, expr) -> (
     let inner_env = make_tmp_env env in
     let attr_ast = analyze' expr inner_env None in
     let tk = get_type_kind attr_ast in
     let id = save_item env name tk (get_sym_table inner_env) in
     VarDecl (id, attr_ast, tk)
  )
  | CondExpr(cond, a, b) -> (
    let cond_attr = analyze' cond env (Some Boolean) in
    if (type_kind_of cond_attr) = Boolean then (
      let a_attr = analyze' a env None in
      let b_attr = analyze' b env None in
      if (type_kind_of a_attr) = (type_kind_of b_attr) then
        Term (ast, type_kind_of a_attr)
      else raise (SemanticError "types of rhs or lhs is different")
    ) else raise (SemanticError "condition must be 'boolean'")
  )
  | ExprStmt expr -> analyze' expr env None

  | LogicOrExpr(lhs, rhs) -> logic_binary_op ast lhs rhs env
  | LogicAndExpr(lhs, rhs) -> logic_binary_op ast lhs rhs env

  | EqualExpr(lhs, rhs) -> cond_binary_op ast lhs rhs env
  | NotEqualExpr(lhs, rhs) -> cond_binary_op ast lhs rhs env

  | LessExpr(lhs, rhs) -> cond_binary_op ast lhs rhs env
  | LessEqualExpr(lhs, rhs) -> cond_binary_op ast lhs rhs env
  | GreaterExpr(lhs, rhs) -> cond_binary_op ast lhs rhs env
  | GreaterEqualExpr(lhs, rhs) -> cond_binary_op ast lhs rhs env

  | AddIntExpr(lhs, rhs) -> binary_op_check ast lhs rhs env Int
  | SubIntExpr(lhs, rhs) -> binary_op_check ast lhs rhs env Int
  | MulIntExpr(lhs, rhs) -> binary_op_check ast lhs rhs env Int
  | DivIntExpr(lhs, rhs) -> binary_op_check ast lhs rhs env Int
  | AddFloatExpr(lhs, rhs) -> binary_op_check ast lhs rhs env Float
  | SubFloatExpr(lhs, rhs) -> binary_op_check ast lhs rhs env Float
  | MulFloatExpr(lhs, rhs) -> binary_op_check ast lhs rhs env Float
  | DivFloatExpr(lhs, rhs) -> binary_op_check ast lhs rhs env Float

  | IntLiteral _ -> term_check ast Int ottk
  | FloatLiteral _ -> term_check ast Float ottk
  | BoolLiteral _ -> term_check ast Boolean ottk

  | FuncCall(name, args) -> (
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
      Some e -> (
      match e with
        ETerm (id, tk) -> (
        match tk with
          IntrinsicFunc(_, params) -> call_function e id params
        | _ -> raise (SemanticError "function is not callable")
      )
      | _ -> raise (SemanticError "[ice] invalid env kind")
    )
    | None -> raise (SemanticError "function id was not found")
  )
  | Id name -> (
    let oe = lookup env name in
    match oe with
      Some e -> let id, tk = get_id_and_tk e in
                IdTerm (ast, id, tk)
    | None -> raise (SemanticError "id was not found")
  )
  | _ -> raise (SemanticError "unsupported")

let analyze ast =
  let env = EModule (Hashtbl.create 10) in
  save_term_item env "print_int" (IntrinsicFunc ("print", [Int; Unit]));

  let attr_ast = match ast with
      Program xs -> Flow (List.map (fun x -> analyze' x env None) xs)
    | _ -> raise (SemanticError "some exceptions are raised")
  in
  dump_env env;
  attr_ast

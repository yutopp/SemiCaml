module L = Llvm
module A = Analyzer

let context = L.global_context ()
let s_module = L.create_module context "SemiCaml"
let builder = L.builder context

let i32_ty = L.i32_type context
let void_ty = L.void_type context

let val_table: (string, L.llvalue) Hashtbl.t = Hashtbl.create 10

exception NotSupportedNode

let to_llvm_ty tk = match tk with
    A.Int -> i32_ty
  | A.String -> void_ty
  | A.Array itk -> void_ty
  | A.IntrinsicFunc (name, params) -> void_ty
  | A.Float -> void_ty
  | A.Boolean -> void_ty
  | A.Unit -> void_ty

let rec make_llvm_ir aast = match aast with
    A.Term(ast, tk) -> (
    Printf.printf "term\n";
    match ast with
      Ast.IntLiteral v -> L.const_int i32_ty v
    | Ast.FloatLiteral v -> L.const_int i32_ty 21
    | Ast.BoolLiteral v -> L.const_int i32_ty 21
    | _ -> raise NotSupportedNode
  )
  | A.VarDecl(id, expr, tk) -> (
    let v = make_llvm_ir expr in
    let alloca_inst = L.build_alloca (to_llvm_ty tk) "" builder in
    let _ = L.build_store alloca_inst v builder in
    Hashtbl.add val_table id alloca_inst;
    alloca_inst
  )
  | A.BinOp(lhs, rhs, tk) -> (
    let _ = make_llvm_ir lhs in
    let _ = make_llvm_ir rhs in
    L.const_int i32_ty 72
  )
  | A.CallFunc(id, args, tk) -> (
    let f = Hashtbl.find val_table id in
    let e_args = List.map (fun a -> make_llvm_ir a) args in
    L.build_call f (Array.of_list e_args) "" builder
  )
  | A.IdTerm(ast, env, tk) -> (
    Printf.printf "id_term\n";
    L.const_int i32_ty 72
  )
  | _ -> raise NotSupportedNode

let rec gyo aast = match aast with
    A.Flow ax -> List.iter (fun a -> gyo a) ax
  | v -> ignore (make_llvm_ir v)

let compile aast =
  Printf.printf "startllvm\n";

  (* decl intrinsics *)
  let decl_print_int () =
    let params = Array.make 1 i32_ty in
    let func_ty = L.function_type void_ty params in
    let f = L.declare_function "_semi_caml_print_int" func_ty s_module in
    Hashtbl.add val_table "print_int" f
  in
  decl_print_int();

  let params = Array.make 0 void_ty in
  let ft = L.function_type void_ty params in
  let f = L.declare_function "_semi_entry" ft s_module in
  let bb = L.append_block context "entry" f in

  L.position_at_end bb builder;

  gyo aast;

  L.dump_module s_module

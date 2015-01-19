module L = Llvm
module LBW = Llvm_bitwriter
module A = Analyzer

let context = L.global_context ()
let s_module = L.create_module context "SemiCaml"
let builder = L.builder context

let i32_ty = L.i32_type context
let void_ty = L.void_type context

let val_table: (string, L.llvalue) Hashtbl.t = Hashtbl.create 10

exception NotSupportedNode
exception UnexpectedType

let to_llvm_ty tk = match tk with
    A.Int -> i32_ty
  | A.String -> void_ty
  | A.Array itk -> void_ty
  | A.Func px -> void_ty
  | A.IntrinsicFunc (name, params) -> void_ty
  | A.Float -> void_ty
  | A.Boolean -> void_ty
  | A.Unit -> void_ty
  | A.Undefined -> raise UnexpectedType

exception InvalidOp

let make_binary_op l r op =
  let make_add tk = match tk with
      A.Int -> L.build_add l r "" builder
    | A.Float -> L.build_fadd l r "" builder
    | _ -> raise InvalidOp
  in
  let make_sub tk = match tk with
      A.Int -> L.build_add l r "" builder
    | A.Float -> L.build_fadd l r "" builder
    | _ -> raise InvalidOp
  in
  let make_mul tk = match tk with
      A.Int -> L.build_add l r "" builder
    | A.Float -> L.build_fadd l r "" builder
    | _ -> raise InvalidOp
  in
  let make_div tk = match tk with
      A.Int -> L.build_add l r "" builder
    | A.Float -> L.build_fadd l r "" builder
    | _ -> raise InvalidOp
  in
  let make_or () = L.build_or l r "" builder
  in
  let make_and () = L.build_and l r "" builder
  in
  let make_eq tk = match tk with
      A.Int -> L.build_add l r "" builder
    | A.Float -> L.build_fadd l r "" builder
    | _ -> raise InvalidOp
  in
  let make_not_eq tk = match tk with
      A.Int -> L.build_add l r "" builder
    | A.Float -> L.build_fadd l r "" builder
    | _ -> raise InvalidOp
  in
  let make_gte tk = match tk with
      A.Int -> L.build_add l r "" builder
    | A.Float -> L.build_fadd l r "" builder
    | _ -> raise InvalidOp
  in
  let make_gt tk = match tk with
      A.Int -> L.build_add l r "" builder
    | A.Float -> L.build_fadd l r "" builder
    | _ -> raise InvalidOp
  in
  let make_lte tk = match tk with
      A.Int -> L.build_add l r "" builder
    | A.Float -> L.build_fadd l r "" builder
    | _ -> raise InvalidOp
  in
  let make_lt tk = match tk with
      A.Int -> L.build_add l r "" builder
    | A.Float -> L.build_fadd l r "" builder
    | _ -> raise InvalidOp
  in
  match op with
    A.Add tk -> make_add tk
  | A.Sub tk -> make_sub tk
  | A.Mul tk -> make_mul tk
  | A.Div tk -> make_div tk
  | A.OrLogic -> make_or ()
  | A.AndLogic -> make_and ()
  | A.Eq tk -> make_eq tk
  | A.NotEq tk -> make_not_eq tk
  | A.Gte tk -> make_gte tk
  | A.Gt tk -> make_gt tk
  | A.Lte tk -> make_lte tk
  | A.Lt tk -> make_lt tk

let rec make_llvm_ir aast = match aast with
    A.Term(ast, tk) -> (
    Printf.printf "term\n";
    match ast with
      Ast.IntLiteral v -> L.const_int i32_ty v
    | Ast.FloatLiteral v -> L.const_int i32_ty 21
    | Ast.BoolLiteral v -> L.const_int i32_ty 21
    | _ -> raise NotSupportedNode
  )
  | A.VarDecl(id, expr, tk, in_clause) ->
     begin
       let v = make_llvm_ir expr in
       let alloca_inst = L.build_alloca (to_llvm_ty tk) "" builder in
       let _ = L.build_store v alloca_inst builder in
       Hashtbl.add val_table id alloca_inst;
       match in_clause with
         Some a -> make_llvm_ir a
       | None -> alloca_inst
     end
  | A.BinOp(lhs, rhs, op, tk) -> (
    let l = make_llvm_ir lhs in
    let r = make_llvm_ir rhs in
    make_binary_op l r op
  )
  | A.CallFunc(id, args, tk) -> (
    Printf.printf "CallFunc\n";
    let f = Hashtbl.find val_table id in
    let e_args = List.map (fun a -> make_llvm_ir a) args in
    L.build_call f (Array.of_list e_args) "" builder
  )
  | A.IdTerm(id, tk) -> (
    Printf.printf "IdTerm\n";
    let v = Hashtbl.find val_table id in
    L.build_load v "" builder
  )
  | _ -> raise NotSupportedNode

let rec make_llvm_ir_seq aast = match aast with
    A.Flow ax -> List.iter (fun a -> make_llvm_ir_seq a) ax
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
  let f = L.declare_function "_semi_caml_entry" ft s_module in
  let bb = L.append_block context "entry" f in

  L.position_at_end bb builder;

  make_llvm_ir_seq aast;
  L.dump_module s_module;

  ignore (L.build_ret_void builder);

  Llvm_analysis.assert_valid_function f;

  s_module

exception FailedToWriteBitcode
exception FailedToBuildBitcode
exception FailedToBuildExecutable

let create_executable m =
  let bc_wrote = LBW.write_bitcode_file m "a.bc" in
  if not bc_wrote then raise FailedToWriteBitcode;
  let sc = Sys.command "llc -filetype=obj a.bc" in
  if sc <> 0 then raise FailedToBuildBitcode;
  let sc = Sys.command "g++ a.o libsemiruntime.a" in
  if sc <> 0 then raise FailedToBuildExecutable;
  ()

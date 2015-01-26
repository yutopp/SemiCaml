module L = Llvm
module LBW = Llvm_bitwriter
module A = Analyzer

let context = L.global_context ()
let s_module = L.create_module context "SemiCaml"
let builder = L.builder context

let i8_ty = L.i8_type context
let i32_ty = L.i32_type context
let float_ty = L.float_type context
let bool_ty = L.i1_type context
let void_ty = L.void_type context

let m_i32_ty = L.pointer_type i32_ty
let m_float_ty = L.pointer_type float_ty
let m_bool_ty = L.pointer_type bool_ty
let m_unit_ty = L.pointer_type i8_ty

let unit_value = L.const_pointer_null m_unit_ty

let closure_ty = L.struct_type context (Array.make 2 (L.pointer_type void_ty))

type t_value =
    Normal of L.llvalue
  | Address of L.llvalue
  | Element of L.llvalue * int
  | Function of L.llvalue * int * L.lltype
  | BuiltinFunction of L.llvalue

let val_table: (string, t_value) Hashtbl.t = Hashtbl.create 10


(* *)
let ptr_to_function_ty =
  (* i32(...) *)
  let f_ty = L.var_arg_function_type i32_ty [||] in
  (* i32(,,,)* *)
  L.pointer_type f_ty

let ptr_to_vals_ty =
  (* i32* *)
  let val_p_ty = L.pointer_type i32_ty in
  (* i32** *)
  L.pointer_type val_p_ty

let function_bag_ty =
  L.struct_type context [|
                  ptr_to_function_ty; (* fp *)
                  ptr_to_vals_ty; (* captured *)
                  ptr_to_vals_ty; (* args *)
                 |]


(* intrinsic *)
let f_new_int32 =
  let params = [|i32_ty|] in
  let func_ty = L.function_type m_i32_ty params in
  L.declare_function "_semi_caml_new_int32" func_ty s_module

let f_new_float =
  let params = [|float_ty|] in
  let func_ty = L.function_type m_float_ty params in
  L.declare_function "_semi_caml_new_float" func_ty s_module

let f_new_bool =
  let params = [|bool_ty|] in
  let func_ty = L.function_type m_bool_ty params in
  L.declare_function "_semi_caml_new_bool" func_ty s_module

let f_new_array =
  let params = [|i32_ty; L.pointer_type i8_ty|] in
  let func_ty = L.function_type ptr_to_vals_ty params in
  L.declare_function "_semi_caml_new_array" func_ty s_module

let f_ref_array_elem =
  let params = [|ptr_to_vals_ty; i32_ty|] in
  let func_ty = L.function_type (L.pointer_type i8_ty) params in
  L.declare_function "_semi_caml_ref_array_element" func_ty s_module

let f_assign_array_elem =
  let params = [|ptr_to_vals_ty; i32_ty; L.pointer_type i8_ty|] in
  let func_ty = L.function_type void_ty params in
  L.declare_function "_semi_caml_assign_array_element" func_ty s_module

let f_new_value_holder_list =
  let params = [|i32_ty|] in
  let func_ty = L.function_type ptr_to_vals_ty params in
  L.declare_function "_semi_caml_new_value_holder_list" func_ty s_module

let f_new_closure_bag =
  let params = [|L.pointer_type void_ty; i32_ty|] in
  let func_ty = L.function_type (L.pointer_type function_bag_ty) params in
  L.declare_function "_semi_caml_new_closure_bag" func_ty s_module


exception NotSupportedNode
exception UnexpectedType of string
exception UnexpectedAAst




let rec to_llvm_ty tk = match tk with
    A.Int -> i32_ty
  | A.String -> (*; void_ty *)raise (UnexpectedType "string")
  | A.Array itk -> ptr_to_vals_ty
  | A.Func px ->
     begin
       let rev = List.rev px in
       let ret_tk = List.hd rev in
       let ret_ty = to_llvm_ty ret_tk in
       let param_tk = List.rev (List.tl rev) in
       let params_ty = List.map (fun ttk -> to_llvm_ty ttk) param_tk in
       L.function_type ret_ty (Array.of_list params_ty)
     end
  | A.IntrinsicFunc params -> (*; void_ty *) raise (UnexpectedType "instfunc")
  | A.Float -> float_ty
  | A.Boolean -> bool_ty
  | A.Unit -> void_ty
  | A.Undefined -> raise (UnexpectedType "undefined")

let to_p_llvm_ty tk = L.pointer_type (to_llvm_ty tk)

let aast_to_p_llvm_ty aast = match aast with
    A.VarDecl (_, _, tk, None) -> to_p_llvm_ty tk
  | _ -> raise UnexpectedAAst

exception InvalidOp
exception InvalidType
exception InvalidValue


let to_ptr_val rv = match rv with
    Normal _ -> raise InvalidValue
  | Address v -> v
  | Element (v, index) ->
     begin
       let p = L.const_in_bounds_gep v [|L.const_int i32_ty index|] in
       L.build_load p "" builder
     end
  | Function (v, index, ty) -> v
  | BuiltinFunction f -> f

let make_binary_op l r op =
  let make_add tk = match tk with
      A.Int -> L.build_add l r "" builder
    | A.Float -> L.build_fadd l r "" builder
    | _ -> raise InvalidOp
  in
  let make_sub tk = match tk with
      A.Int -> L.build_sub l r "" builder
    | A.Float -> L.build_fsub l r "" builder
    | _ -> raise InvalidOp
  in
  let make_mul tk = match tk with
      A.Int -> L.build_mul l r "" builder
    | A.Float -> L.build_fmul l r "" builder
    | _ -> raise InvalidOp
  in
  let make_div tk = match tk with
      A.Int -> L.build_sdiv l r "" builder (* signed div *)
    | A.Float -> L.build_fdiv l r "" builder
    | _ -> raise InvalidOp
  in
  let make_or () = L.build_or l r "" builder
  in
  let make_and () = L.build_and l r "" builder
  in
  let make_eq tk = match tk with
      A.Int -> L.build_icmp L.Icmp.Eq l r "" builder
    | A.Float -> L.build_fcmp L.Fcmp.Ueq l r "" builder
    | _ -> raise InvalidOp
  in
  let make_not_eq tk = match tk with
      A.Int -> L.build_icmp L.Icmp.Ne l r "" builder
    | A.Float -> L.build_fcmp L.Fcmp.Une l r "" builder
    | _ -> raise InvalidOp
  in
  let make_gte tk = match tk with
      A.Int -> L.build_icmp L.Icmp.Sge l r "" builder
    | A.Float -> L.build_fcmp L.Fcmp.Uge l r "" builder
    | _ -> raise InvalidOp
  in
  let make_gt tk = match tk with
      A.Int -> L.build_icmp L.Icmp.Sgt l r "" builder
    | A.Float -> L.build_fcmp L.Fcmp.Ugt l r "" builder
    | _ -> raise InvalidOp
  in
  let make_lte tk = match tk with
      A.Int -> L.build_icmp L.Icmp.Sle l r "" builder
    | A.Float -> L.build_fcmp L.Fcmp.Ule l r "" builder
    | _ -> raise InvalidOp
  in
  let make_lt tk = match tk with
      A.Int -> L.build_icmp L.Icmp.Slt l r "" builder
    | A.Float -> L.build_fcmp L.Fcmp.Ult l r "" builder
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

let make_managed_value tk v = match tk with
    A.Int -> L.build_call f_new_int32 [|v|] "" builder
  | A.Float -> L.build_call f_new_float [|v|] "" builder
  | A.Boolean -> L.build_call f_new_bool [|v|] "" builder
  | _ -> raise InvalidValue

let rec make_llvm_ir aast ip___ = match aast with
    A.Term(ast, tk) ->
    begin
      Printf.printf "term\n";
      let v = match ast with
          Ast.IntLiteral v -> make_managed_value A.Int (L.const_int i32_ty v)
        | Ast.FloatLiteral v -> make_managed_value A.Float (L.const_float float_ty v)
        | Ast.BoolLiteral v -> make_managed_value A.Boolean (L.const_int bool_ty (if v then 1 else 0))
        | Ast.UnitLiteral -> unit_value
        | _ -> raise NotSupportedNode
      in
      Address v
    end

  | A.VarDecl(id, expr, tk, in_clause) ->
     begin
       let v = make_llvm_ir expr ip___ in
       Hashtbl.add val_table id v;
       match in_clause with
         Some a -> make_llvm_ir a ip___
       | None -> v
     end

  | A.FuncDecl (id, params, expr, tk, captured_ids, in_clause) ->
     begin
       let tks = match tk with
           A.Func xs -> xs
         | _ -> raise InvalidType
       in
       let ret_tk = List.hd (List.rev tks) in
       let params_list = List.map (fun x -> aast_to_p_llvm_ty x) params in
       let ll_params_ty = Array.of_list ([ptr_to_vals_ty] @ params_list) in
       let ft = L.function_type (to_p_llvm_ty ret_tk) ll_params_ty in
       let f = L.declare_function id ft s_module in
       Hashtbl.add val_table id (Normal f);
       let ll_params = L.params f in
       let f_context = ll_params.(0) in
       L.set_value_name "__context" f_context;
       List.iteri (fun i p -> L.set_value_name (A.get_id_of p) ll_params.(i+1)) params;
       List.iteri (fun i p -> Hashtbl.add val_table (A.get_id_of p) (Address ll_params.(i+1))) params;
       let bb = L.append_block context "entry" f in
       let f_ip = L.instr_begin bb in
       L.position_builder f_ip builder;

       (* *)
       let pre_map = List.map (fun (id, index) -> (id, Hashtbl.find val_table id)) captured_ids in
       List.iter (fun (id, index) -> Hashtbl.remove val_table id) captured_ids;

       (* set captured value *)
       List.iter (fun (id, index) -> Hashtbl.add val_table id (Element (f_context, index))) captured_ids;

       let l_expr = to_ptr_val (make_llvm_ir expr f_ip) in
       ignore (L.build_ret l_expr builder);

       L.dump_module s_module;
       Llvm_analysis.assert_valid_function f;

       (* *)
       List.iter (fun (id, index) -> Hashtbl.remove val_table id) captured_ids;
       List.iter (fun (id, v) -> Hashtbl.add val_table id v) pre_map;

       (* restore ip *)
       L.position_builder ip___ builder;

       let v_fp = L.build_pointercast f (L.pointer_type void_ty) "" builder in
       let len = L.const_int i32_ty (List.length captured_ids) in

       (* closure bag *)
       let cl_bag = L.build_call f_new_closure_bag [|v_fp; len|] "" builder in

       (* capture *)
       let p_captured_ptrs = L.build_in_bounds_gep cl_bag [|L.const_int i32_ty 0; L.const_int i32_ty 1|] "" builder in
       let captured_ptrs = L.build_load p_captured_ptrs "" builder in
       let set_captured_value (id, index) =
         let to_ptr = L.build_in_bounds_gep captured_ptrs [|L.const_int i32_ty index|] "" builder in
         let c_raw_val = Hashtbl.find val_table id in
         let c_val = to_ptr_val c_raw_val in
         ignore (L.build_store c_val to_ptr builder)
       in
       List.iter set_captured_value captured_ids;

       let v = (Function (cl_bag, 0, ft)) in
       Hashtbl.add val_table id v;
       match in_clause with
         Some a -> make_llvm_ir a ip___
       | None -> v
     end

  | A.Seq (lhs, rhs) ->
     begin
       let l = to_ptr_val (make_llvm_ir lhs ip___) in
       let l_ip = L.instr_succ l in
       make_llvm_ir rhs l_ip
     end

  | A.BinOp (lhs, rhs, op, tk) ->
     begin
       let l = to_ptr_val (make_llvm_ir lhs ip___) in
       let l_ip = L.instr_succ l in
       let r = to_ptr_val (make_llvm_ir rhs l_ip) in
       let ll = L.build_load l "" builder in
       let lr = L.build_load r "" builder in
       let v = make_binary_op ll lr op in

       Address (make_managed_value tk v)
     end

  | A.Cond (cond, a, b) ->
     begin
       let cond_v_p = to_ptr_val (make_llvm_ir cond ip___) in
       let cond_v = L.build_load cond_v_p "" builder in
       let cond_v_ip = L.instr_succ cond_v in

       let start_bb = L.insertion_block builder in
       let current_func = L.block_parent start_bb in

       (* create basic blocks *)
       let then_bb = L.append_block context "then" current_func in
       let else_bb = L.append_block context "else" current_func in
       let merge_bb = L.append_block context "merge" current_func in

       (* create cond *)
       ignore (L.build_cond_br cond_v then_bb else_bb builder);

       (* create 'then' block *)
       L.position_at_end then_bb builder;
       let then_v = to_ptr_val (make_llvm_ir a cond_v_ip) in
       ignore (L.build_br merge_bb builder);

       (* create 'else' block *)
       L.position_at_end else_bb builder;
       let else_v = to_ptr_val (make_llvm_ir b cond_v_ip) in
       ignore (L.build_br merge_bb builder);

       (* create merge block *)
       L.position_at_end merge_bb builder;
       let phi = L.build_phi [(then_v, then_bb); (else_v, else_bb)] "" builder in
       Address phi
     end

  | A.CallFunc (id, args, tk) ->
     begin
       Printf.printf "CallFunc %s\n" id;
       let rf = Hashtbl.find val_table id in

       let c_ip = ref ip___ in
       let seq a =
         let v = to_ptr_val (make_llvm_ir a !c_ip) in
         let v_ip = L.instr_succ v in
         c_ip := v_ip;
         v
       in

       match rf with
         Function (bag, index, f_ty) ->
         begin
           let fpp = L.build_in_bounds_gep bag [|L.const_int i32_ty 0; L.const_int i32_ty 0|] "" builder in
           let rf = L.build_load fpp "" builder in

           flush stdout;
           let f = L.build_pointercast rf (L.pointer_type f_ty) "" builder in

           let captured_ptrs = L.build_in_bounds_gep bag [|L.const_int i32_ty 0; L.const_int i32_ty 1|] "" builder in
           let captured_context = L.build_load captured_ptrs "" builder in

           let e_args = [captured_context] @ (List.map seq args) in
           Address (L.build_call f (Array.of_list e_args) "" builder)
         end

       | BuiltinFunction f ->
          begin
            let e_args = List.map seq args in
            Address (L.build_call f (Array.of_list e_args) "" builder)
          end

       | _ -> raise InvalidValue
     end

  | A.ArrayCreate (elem_num, A.Array inner_tk) ->
     begin
       let len_p = to_ptr_val (make_llvm_ir elem_num ip___) in
       let len = L.build_load len_p "" builder in
       let initial_value = match inner_tk with
           A.Int -> make_managed_value A.Int (L.const_int i32_ty 0)
         | A.Float -> make_managed_value A.Float (L.const_float float_ty 0.0)
         | A.Boolean -> make_managed_value A.Boolean (L.const_int bool_ty 0)
         | _ -> raise NotSupportedNode
       in
       let generic_iv = L.build_bitcast initial_value (L.pointer_type i8_ty) "" builder in
       (* elem 0 of array contains length of it *)
       let array_p = L.build_call f_new_array [|len; generic_iv|] "" builder in
       Address (array_p)
     end

  | A.ArrayRef (id, elem_index, tk) ->
     begin
       let arr = to_ptr_val (Hashtbl.find val_table id) in
       let index_p = to_ptr_val (make_llvm_ir elem_index ip___) in
       let index = L.build_load index_p "" builder in
       let generic_v = L.build_call f_ref_array_elem [|arr; index|] "" builder in
       let v = L.build_bitcast generic_v (L.pointer_type (to_llvm_ty tk)) "" builder in
       Address (v)
     end

  | A.ArrayAssign (id, elem_index, expr, tk) ->
     begin
       let arr = to_ptr_val (Hashtbl.find val_table id) in
       let index_p = to_ptr_val (make_llvm_ir elem_index ip___) in
       let index = L.build_load index_p "" builder in
       let next_ip = L.instr_succ index in
       let value = to_ptr_val (make_llvm_ir expr next_ip) in
       let generic_v = L.build_bitcast value (L.pointer_type i8_ty) "" builder in
       ignore (L.build_call f_assign_array_elem [|arr; index; generic_v|] "" builder);

       Address unit_value
     end

  | A.IdTerm (id, tk) ->
     begin
       Printf.printf "IdTerm\n";
       Hashtbl.find val_table id
     end

  | _ -> raise NotSupportedNode

let rec make_llvm_ir_seq aast ip = match aast with
    A.Flow ax ->
    begin
      let seq b_ip a =
        let v = to_ptr_val (make_llvm_ir a b_ip) in
        L.instr_succ v
      in
      ignore (List.fold_left seq ip ax)
    end
  | v -> ignore (make_llvm_ir v ip)

let compile aast =
  Printf.printf "startllvm\n";

  (* decl intrinsics *)
  let decl_print_int () =
    let params = [|m_i32_ty|] in
    let func_ty = L.function_type m_unit_ty params in
    let f = L.declare_function "_semi_caml_print_int" func_ty s_module in
    Hashtbl.add val_table "print_int" (BuiltinFunction f)
  in
  decl_print_int();

  let decl_print_bool () =
    let params = [|m_bool_ty|] in
    let func_ty = L.function_type m_unit_ty params in
    let f = L.declare_function "_semi_caml_print_bool" func_ty s_module in
    Hashtbl.add val_table "print_bool" (BuiltinFunction f)
  in
  decl_print_bool();

  let decl_print_float () =
    let params = [|m_float_ty|] in
    let func_ty = L.function_type m_unit_ty params in
    let f = L.declare_function "_semi_caml_print_float" func_ty s_module in
    Hashtbl.add val_table "print_float" (BuiltinFunction f)
  in
  decl_print_float();

  let decl_print_newline () =
    let params = [|m_unit_ty|] in
    let func_ty = L.function_type m_unit_ty params in
    let f = L.declare_function "_semi_caml_print_newline" func_ty s_module in
    Hashtbl.add val_table "print_newline" (BuiltinFunction f)
  in
  decl_print_newline();

  let params = Array.make 0 void_ty in
  let ft = L.function_type void_ty params in
  let f = L.declare_function "_semi_caml_entry" ft s_module in
  let bb = L.append_block context "entry" f in

  let ip = L.instr_begin bb in
  L.position_builder ip builder;

  make_llvm_ir_seq aast ip;
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

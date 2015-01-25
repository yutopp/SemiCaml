open Ast
open Token
open Make_left_associative_tree

let rec dump ?(offset=0) a =
  let off_s = String.make (offset*2) ' ' in
  match a with
    Program xs ->
    begin
      Printf.printf "%sProgram[\n" off_s;
      List.iter (fun x -> Printf.printf "%s  " off_s; dump ~offset:(offset+2) x) xs;
      Printf.printf "%s]\n" off_s;
    end

  | VerDecl (name, expr, in_clause) ->
     begin
       Printf.printf "let(var) %s = " name;
       dump ~offset:(offset+1) expr;
       match in_clause with
         Some a ->
         begin
           Printf.printf " in\n";
           Printf.printf "%s" off_s;
           dump ~offset:(offset+1) a;
           Printf.printf "\n";
         end
       | None -> Printf.printf "\n";
     end

  | FuncDecl (name, params, expr, in_clause) ->
     begin
       Printf.printf "let(func) %s " name;
       List.iter (fun id -> Printf.printf "%s " id) params;
       Printf.printf "= ";
       dump ~offset:(offset+1) expr;
       match in_clause with
         Some a ->
         begin
           Printf.printf " in\n";
           Printf.printf "%s" off_s;
           dump ~offset:(offset+1) a;
           Printf.printf "\n";
         end
       | None -> Printf.printf "\n";
     end

  | AddIntExpr(lhs, rhs) -> dump(lhs); Printf.printf " + "; dump(rhs)
  | SubIntExpr(lhs, rhs) -> dump(lhs); Printf.printf " - "; dump(rhs)
  | MulIntExpr(lhs, rhs) -> dump(lhs); Printf.printf " * "; dump(rhs)
  | DivIntExpr(lhs, rhs) -> dump(lhs); Printf.printf " / "; dump(rhs)
  | AddFloatExpr(lhs, rhs) -> dump(lhs); Printf.printf " +. "; dump(rhs)
  | SubFloatExpr(lhs, rhs) -> dump(lhs); Printf.printf " -. "; dump(rhs)
  | MulFloatExpr(lhs, rhs) -> dump(lhs); Printf.printf " *. "; dump(rhs)
  | DivFloatExpr(lhs, rhs) -> dump(lhs); Printf.printf " /. "; dump(rhs)

  | IntLiteral v -> Printf.printf "%d" v
  | FloatLiteral v -> Printf.printf "%f" v
  | BoolLiteral v -> Printf.printf "%b" v
  | FuncCall(name, args) -> (
    Printf.printf "invoke[%s]( " name;
    List.iter (fun x -> dump ~offset:(offset+1) x; Printf.printf ", ") args;
    Printf.printf ")";
  )
  | Id name -> Printf.printf "ID(%s)" name
  | _ -> Printf.printf "%sNot supported\n" off_s

let rec program_rule = function
  | Op DoubleSemicolon :: tail -> (Program [], tail)
  | tokens -> begin match top_let_expr_rule tokens with
    | (head_ast, Op DoubleSemicolon :: tail) -> begin match program_rule tail with
      | (Program tail_asts, tail') -> (Program (head_ast :: tail_asts), tail')
      | _                          -> failwith "you shouldn't see this message"
    end
    | (head_ast, []) -> (Program [head_ast], [])
    | _              -> failwith "';;' expected"
  end

and top_let_expr_rule tokens = match tokens with
  | Keyword Let :: _ -> begin match decl_rule tokens with
    | (VerDecl (id, ast, None), Keyword In :: tail) -> begin match let_expr_rule tail with
      | (ast', tail') -> (VerDecl (id, ast, Some ast'), tail')
    end
    | (FuncDecl (id, ids, ast, None), Keyword In :: tail) -> begin match let_expr_rule tail with
      | (ast', tail') -> (FuncDecl (id, ids, ast, Some ast'), tail')
    end
    | result -> result
  end
  | _ -> seq_expr_rule tokens

and let_expr_rule tokens = match tokens with
  | Keyword Let :: _ -> begin match decl_rule tokens with
    | (VerDecl (id, ast, None), Keyword In :: tail) -> begin match let_expr_rule tail with
      | (ast', tail') -> (VerDecl (id, ast, Some ast'), tail')
    end
    | (FuncDecl (id, ids, ast, None), Keyword In :: tail) -> begin match let_expr_rule tail with
      | (ast', tail') -> (FuncDecl (id, ids, ast, Some ast'), tail')
    end
      | _ -> failwith "'in' expected"
  end
  | _ -> seq_expr_rule tokens

and decl_rule = function
  | Keyword Let :: Identifier id :: Op Assign :: tail -> begin match let_expr_rule tail with
    | (ast, tail') -> (VerDecl (id, ast, None), tail')
  end
  | Keyword Let :: Identifier id :: ParenOpen :: tail -> begin match ids_rule tail with
    | (ids, tail') -> begin match tail' with
      | ParenClose :: Op Assign :: tail'' -> begin match let_expr_rule tail'' with
        | (ast, tail''') -> (FuncDecl (id, ids, ast, None), tail''')
      end
      | _ -> failwith "')' expected"
    end
  end
  | _ -> failwith "'let' expected"

and ids_rule = function
  | Identifier id :: tail -> begin match ids_rule tail with
    | (ids, tail') -> (id :: ids, tail')
  end
  | _ as tokens -> ([], tokens)

and seq_expr_rule tokens = match cond_expr_rule tokens with
  | (ast, Op Semicolon :: tail) -> begin match seq_expr_rule tail with
    | (ast', tail') -> (Sequence (ast, ast'), tail')
  end
  | result -> result

and cond_expr_rule tokens = match tokens with
  | Keyword If :: tail -> begin match let_expr_rule tail with
    | (cond_ast, Keyword Then :: tail') -> begin match let_expr_rule tail' with
      | (ok_ast, Keyword Else :: tail'') -> begin match let_expr_rule tail'' with
        | (ng_ast, tail''') -> (CondExpr (cond_ast, ok_ast, ng_ast), tail''')
      end
      | _ -> failwith "'else' expected"
    end
    | _ -> failwith "'then' expected"
  end
  | _ -> logic_or_expr_rule tokens

and logic_or_expr_rule tokens = match logic_and_expr_rule tokens with
  | (ast, tail) -> begin match tail with
    | Op Or :: tail' -> begin match logic_or_expr_rule tail' with
      | (ast', tail'') -> (LogicOrExpr (ast, ast'), tail'')
    end
    | _ -> (ast, tail)
  end

and logic_and_expr_rule tokens = match equal_expr_rule tokens with
  | (ast, tail) -> begin match tail with
    | Op And :: tail' -> begin match logic_and_expr_rule tail' with
      | (ast', tail'') -> (LogicAndExpr (ast, ast'), tail'')
    end
    | _ -> (ast, tail)
  end

and equal_expr_rule tokens = match comp_expr_rule tokens with
  | (ast, tail) -> begin match tail with
    | Op Equal :: tail' -> begin match equal_expr_rule tail' with
      | (ast', tail'') -> (EqualExpr (ast, ast'), tail'')
    end
    | _ -> (ast, tail)
  end

and comp_expr_rule tokens = match add_expr_rule tokens with
  | (ast, tail) -> begin match tail with
    | Op Less :: tail' -> begin match add_expr_rule tail' with
      | (ast', tail'') -> (LessExpr (ast, ast'), tail'')
    end
    | Op LessEqual :: tail' -> begin match add_expr_rule tail' with
      | (ast', tail'') -> (LessEqualExpr (ast, ast'), tail'')
    end
    | Op Greater  :: tail' -> begin match add_expr_rule tail' with
      | (ast', tail'') -> (GreaterExpr (ast, ast'), tail'')
    end
    | Op GreaterEqual :: tail' -> begin match add_expr_rule tail' with
      | (ast', tail'') -> (GreaterEqualExpr (ast, ast'), tail'')
    end
    | _ -> (ast, tail)
  end

and add_expr_rule tokens = match mul_expr_rule tokens with
  | (ast, tail) -> begin match op_with_mul_exprs_rule tail with
    | (op_with_mul_exprs, tail') -> (make_left_associative_tree ast op_with_mul_exprs, tail')
  end

and op_with_mul_exprs_rule tokens = match tokens with
  | op :: tail when isAddSubOp op -> begin match mul_expr_rule tail with
    | (ast, tail') -> begin match op_with_mul_exprs_rule tail' with
      | (op_with_mul_exprs, tail'') -> ((op, ast) :: op_with_mul_exprs, tail'')
    end
  end
  | _ -> ([], tokens)

and mul_expr_rule tokens = match func_call_expr_rule tokens with
  | (ast, tail) -> begin match op_with_func_call_exprs_rule tail with
    | (op_with_func_call_exprs, tail') -> (make_left_associative_tree ast op_with_func_call_exprs, tail')
  end

and op_with_func_call_exprs_rule tokens = match tokens with
  | op :: tail when isMulDivOp op -> begin match func_call_expr_rule tail with
    (ast, tail') -> begin match op_with_func_call_exprs_rule tail' with
      | (op_with_func_call_exprs, tail'') -> ((op, ast) :: op_with_func_call_exprs, tail'')
    end
  end
  | _ -> ([], tokens)

and func_call_expr_rule tokens = match tokens with
  | Identifier id :: tail -> begin match prim_exprs_rule tail with
    | ([], _) -> prim_expr_rule tokens
    | (prim_exprs, tail') -> (FuncCall (id, prim_exprs), tail')
  end
  | _ -> prim_expr_rule tokens

and prim_exprs_rule tokens = match tokens with
  | token :: _ when is_first_of_prim_expr token -> begin match prim_expr_rule tokens with
    | (prim_expr, tail) -> begin match prim_exprs_rule tail with
      | (prim_exprs, tail') -> (prim_expr :: prim_exprs, tail')
    end
  end
  | _ -> ([], tokens)

and prim_expr_rule = function
  | ParenOpen :: tail -> begin match let_expr_rule tail with
    | (ast, ParenClose :: tail') -> (ast, tail')
    | _                          -> failwith "')' expected"
  end
  | Keyword Array :: Op Dot :: Keyword New :: Identifier id :: tail-> begin match let_expr_rule tail with
    | (ast, tail') -> (ArrayNew (id, ast), tail')
  end
  | Keyword True :: tail -> (BoolLiteral true, tail)
  | Keyword False :: tail -> (BoolLiteral false, tail)
  | IntLiteral value :: tail -> (IntLiteral value, tail)
  | FloatLiteral value :: tail -> (FloatLiteral value, tail)
  | Identifier id :: Op Dot :: ParenOpen :: tail -> begin match let_expr_rule tail with
    | (index_ast, Op ArrayAssign :: tail') -> begin match let_expr_rule tail' with
      | (value_ast, tail'') -> (ArrayAssign (id, index_ast, value_ast), tail'')
    end
    | (index_ast, tail') -> (ArrayGet (id, index_ast), tail')
  end
  | Identifier id :: tail ->  (Id id, tail)
  | _ -> failwith "identifier expected"

let parse tokens = match program_rule tokens with
  | (ast, []) -> ast
  | _ -> failwith "rest of input"

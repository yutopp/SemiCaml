open Ast
open Token
open Make_left_associative_tree

(*
let parse filename =
  Program [
      (* let f a b = a + b *)
      FuncDecl (
          "f",
          ["a"; "b"],
          AddIntExpr (Id "a", Id "b"),
          None
        );

      (*
      (* failed case *)
      (* let fi a b c = a + b *)
      FuncDecl (
          "fi",
          ["a"; "b"; "c"],
          AddIntExpr (Id "a", Id "b"),
          None
        );
       *)

      (* let a = 1 in print_int (a + 6) *)
      VerDecl (
          "a",
          IntLiteral 1,
          Some (
              FuncCall (
                  "print_int",
                  [
                    AddIntExpr (Id "a", IntLiteral 6)
                  ]
                )
            )
        );
      (* print_newline () *)
      FuncCall ("print_newline", [UnitLiteral] );


      (* let b = 1 = 2 *)
      VerDecl (
          "b",
          EqualExpr (IntLiteral 1, IntLiteral 2),
          None
        );
      (* print_bool b *)
      FuncCall ("print_bool", [Id "b"]);

      (* print_newline () *)
      FuncCall ("print_newline", [UnitLiteral]);


      (* let b = 1 = 1 in print_bool b *)
      VerDecl (
          "b",
          EqualExpr (IntLiteral 1, IntLiteral 1),
          Some (
              FuncCall ("print_bool", [Id "b"]);
            )
        );

      (* print_newline () *)
      FuncCall ("print_newline", [UnitLiteral]);

      (* let hoge = f 10 20 in print_int hoge *)
      VerDecl (
          "hoge",
          FuncCall (
              "f",
              [
                IntLiteral 10;
                IntLiteral 20;
              ]
            ),
          Some (
              FuncCall (
                  "print_int",
                  [
                    Id "hoge"
                  ]
                )
            )
        );


      (* print_newline () *)
      FuncCall ("print_newline", [UnitLiteral] );


      (* let f = let n = 10 in let g a = a + n in g *)
      VerDecl (
          "f",
          VerDecl (
              "n",
              IntLiteral 10,
              Some (
                  FuncDecl (
                      "g",
                      ["a"],
                      AddIntExpr (Id "a", Id "n"),
                      Some (
                          Id "g"
                        )
                    )
                )
            ),
          None
        );


      (* print_int (f 5) *)
      FuncCall (
          "print_int",
          [
            FuncCall (
                "f",
                [
                  IntLiteral 5
                ]
              )
          ]
        );


      (* print_newline () *)
      FuncCall ("print_newline", [UnitLiteral] );


      (* let g = let a = 10 in let b = 20 in let g c = a + b + c in print_int (g 10) *)
      VerDecl (
          "g",
          VerDecl (
              "a",
              IntLiteral 10,
              Some (
                  VerDecl (
                      "b",
                      IntLiteral 20,

                      Some (
                          FuncDecl (
                              "g",
                              ["c"],
                              AddIntExpr (AddIntExpr( Id "a", Id "b"), Id "c"),
                              Some (
                                  FuncCall ("print_int", [FuncCall ("g", [IntLiteral 10])])
                                )
                            )
                        )
                    )
                )
            ),
          None
        );


      (* print_newline () *)
      FuncCall ("print_newline", [UnitLiteral]);


      (* let g = print_int 1; print_newline (); print_int 2; print_newline () *)
      VerDecl (
          "g",
          Sequence (
              FuncCall ("print_int", [IntLiteral 1]),
              Sequence (
                  FuncCall ("print_newline", [UnitLiteral]),
                  Sequence (
                      FuncCall ("print_int", [IntLiteral 2]),
                      FuncCall ("print_newline", [UnitLiteral])
                    )
                )
            ),
          None
        );

      (* print_int (if 1 = 1 then 1 else 2) *)
      FuncCall (
          "print_int",
          [
            CondExpr (
                EqualExpr (IntLiteral 1, IntLiteral 1),
                IntLiteral 1,
                IntLiteral 2
              )
          ]
        );

      (* print_newline () *)
      FuncCall ("print_newline", [UnitLiteral]);

      (* print_int (if 1 = 10 then 1 else 2) *)
      FuncCall (
          "print_int",
          [
            CondExpr (
                EqualExpr (IntLiteral 1, IntLiteral 10),
                IntLiteral 1,
                IntLiteral 2
              )
          ]
        );

      (* print_newline () *)
      FuncCall ("print_newline", [UnitLiteral]);

      (* let arr = Array.new float 10 *)
      VerDecl (
          "arr",
          ArrayNew ("float", IntLiteral 10),
          None
        );

      (* print_float (arr.(2)) *)
      FuncCall ("print_float", [ArrayGet ("arr", IntLiteral 2)]);

      (* print_newline () *)
      FuncCall ("print_newline", [UnitLiteral]);

      (* arr.(2) <- 3.14 *)
      ArrayAssign ("arr", IntLiteral 2, FloatLiteral 3.14);

      (* print_float (arr.(2)) *)
      FuncCall ("print_float", [ArrayGet ("arr", IntLiteral 2)]);

      (* print_newline () *)
      FuncCall ("print_newline", [UnitLiteral]);
    ]
*)


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

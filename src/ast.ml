type ast =
    Program of ast list

  | VerDecl of string * ast * ast option
  | FuncDecl of bool * string * string list * ast * ast option

  | Sequence of ast * ast

  | ArrayNew of string * ast
  | CondExpr of ast * ast * ast

  | LogicOrExpr of ast * ast
  | LogicAndExpr of ast * ast

  | EqualExpr of ast * ast
  | NotEqualExpr of ast * ast

  | LessExpr of ast * ast
  | LessEqualExpr of ast * ast
  | GreaterExpr of ast * ast
  | GreaterEqualExpr of ast * ast

  | AddIntExpr of ast * ast
  | SubIntExpr of ast * ast
  | MulIntExpr of ast * ast
  | DivIntExpr of ast * ast
  | AddFloatExpr of ast * ast
  | SubFloatExpr of ast * ast
  | MulFloatExpr of ast * ast
  | DivFloatExpr of ast * ast

  | IntLiteral of int
  | FloatLiteral of float
  | BoolLiteral of bool
  | UnitLiteral

  | ArrayGet of string * ast
  | ArrayAssign of string * ast * ast
  | FuncCall of string * ast list
  | Id of string


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

  | FuncDecl (is_rec, name, params, expr, in_clause) ->
     begin
       Printf.printf "let(func) %s%s " (if is_rec then "rec " else "") name;
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

  | CondExpr (cond, t, f) -> Printf.printf "if "; dump(cond); Printf.printf " then "; dump(t); Printf.printf " else "; dump(f)

  | EqualExpr (lhs, rhs) -> dump(lhs); Printf.printf " = "; dump(rhs)

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
  | UnitLiteral -> Printf.printf "()"

  | FuncCall(name, args) ->
     begin
       Printf.printf "invoke[%s]( " name;
       List.iter (fun x -> dump ~offset:(offset+1) x; Printf.printf ", ") args;
       Printf.printf ")";
     end

  | Id name -> Printf.printf "ID(%s)" name
  | ArrayNew (ty, index) -> Printf.printf "Array.new %s (" ty; dump index; Printf.printf ")"
  | ArrayGet (name, index) -> Printf.printf "%s.(" name; dump index; Printf.printf ")"
  | ArrayAssign (name, index, expr) -> Printf.printf "%s.(" name; dump index; Printf.printf ") <- "; dump expr
  | Sequence (e1, e2) -> dump e1; Printf.printf "; "; dump e2
  | _ -> Printf.printf "%sNot supported\n" off_s

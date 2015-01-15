open Ast

let parse filename =
  Program [
      Seq [
          VerDecl("a", IntLiteral 1);
          ExprStmt (FuncCall ("print_int", [AddIntExpr(Id("a"), IntLiteral 4)]))
        ]
    ]

let rec dump ?(offset=0) a =
  let off_s = String.make (offset*2) ' ' in
  match a with
    Program xs -> (
    Printf.printf "%sProgram[\n" off_s;
    List.iter (fun x -> dump ~offset:(offset+1) x) xs;
    Printf.printf "%s]\n" off_s;
  )
  | Seq xs -> (
    Printf.printf "%sSequence[\n" off_s;
    List.iter (fun x -> dump ~offset:(offset+1) x; Printf.printf "%s,\n" off_s) xs;
    Printf.printf "%s]\n" off_s;
  )
  | VerDecl (id, expr) -> (
    Printf.printf "%sVerDecl %s = " off_s id;
    dump ~offset:(offset+1) expr;
    Printf.printf "\n";
  )
  | FuncDecl( id, args, body ) -> (
    Printf.printf "%sFuncDecl\n" off_s;
  )
  | ExprStmt ast -> Printf.printf "%sExp: " off_s; dump ast; Printf.printf "\n";

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
    Printf.printf "%sFuncCall[ %s( " off_s name;
    List.iter (fun x -> dump ~offset:(offset+1) x; Printf.printf ", ") args;
    Printf.printf "%s) ]" off_s;
  )
  | Id name -> Printf.printf "ID(%s)" name
  | _ -> Printf.printf "%sNot supported\n" off_s;

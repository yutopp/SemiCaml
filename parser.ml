open Ast

let parse filename =
  Program [Seq [VerDecl("a", IntLiteral 1)]]

let rec dump a = match a with
    Program xs -> (
    Printf.printf "Program[\n";
    List.iter (fun x -> dump x) xs;
    Printf.printf "]\n";
  )
  | Seq xs -> (
    Printf.printf "Sequence[\n";
    List.iter (fun x -> dump x; Printf.printf "::\n") xs;
    Printf.printf "]\n";
  )
  | VerDecl( id, expr ) -> (
    Printf.printf "VerDecl = \n";
    dump expr;
  )
  | FuncDecl( id, args, body ) -> (
    Printf.printf "FuncDecl\n";
  )
  | _ -> Printf.printf "Not supported\n";

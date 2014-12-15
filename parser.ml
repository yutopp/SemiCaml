open Ast

let parse filename =
  Program [Seq [VerDecl "a"]]

let rec dump_program a = match a with
    Program xs -> (
    Printf.printf "Program[\n";
    List.iter (fun x -> dump_sequence x) xs;
    Printf.printf "]\n";
  )
and dump_sequence a = match a with
    Seq xs -> (
    Printf.printf "Sequence[\n";
    List.iter (fun x -> dump_expression x) xs;
    Printf.printf "]\n";
  )
and dump_expression a = match a with
  | VerDecl id -> (
    Printf.printf "VerDecl\n";
  )
  | FuncDecl( id, args, body ) -> (
    Printf.printf "FuncDecl\n";
  )

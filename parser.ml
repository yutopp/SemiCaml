open Ast

let parse filename =
  Program [Seq [VerDecl("a", IntLiteral 1)]]

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
  | VerDecl( id, expr ) -> (
    Printf.printf "%sVerDecl = \n" off_s;
    dump ~offset:(offset+1) expr;
  )
  | FuncDecl( id, args, body ) -> (
    Printf.printf "%sFuncDecl\n" off_s;
  )
  | _ -> Printf.printf "%sNot supported\n" off_s;

open Ast

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
                                  FuncCall ("print_int", [FuncCall ("g", [IntLiteral 10])] )
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
              FuncCall ("print_int", [IntLiteral 1] ),
              Sequence (
                  FuncCall ("print_newline", [UnitLiteral] ),
                  Sequence (
                      FuncCall ("print_int", [IntLiteral 2] ),
                      FuncCall ("print_newline", [UnitLiteral] )
                    )
                )
            ),
          None
        );
    ]

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
  | _ -> Printf.printf "%sNot supported\n" off_s;

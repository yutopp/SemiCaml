type program =
  Program of seq list

and seq =
  Seq of expr list

and expr =
    VerDecl of string
  | FuncDecl of string * string list * seq

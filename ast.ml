type ast =
    Program of ast list

  | VerDecl of string * ast * ast option
  | FuncDecl of string * string list * ast * ast option

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

  | ArrayGet of string * ast
  | ArrayAssign of string * ast * ast
  | FuncCall of string * ast list
  | Id of string

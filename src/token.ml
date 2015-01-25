type opcode =
  | DoubleSemicolon | Semicolon
  | Or | And
  | Equal | NotEqual
  | Assign | ArrayAssign
  | LessEqual| GreaterEqual | Less | Greater
  | AddFloat | AddInt | SubFloat | SubInt
  | MulFloat | MulInt | DivFloat | DivInt
  | Dot

type keyword =
  | If | Then | Else | Let | Array | New | In
  | True | False
  | Int | Float | Bool

type token =
  | ParenOpen | ParenClose
  | Op of opcode
  | IntLiteral of int
  | FloatLiteral of float
  | Keyword of keyword
  | Identifier of string

let isAddSubOp = function
  | Op AddInt -> true
  | Op AddFloat -> true
  | Op SubInt -> true
  | Op SubFloat -> true
  | _ -> false

let isMulDivOp = function
  | Op MulInt -> true
  | Op MulFloat -> true
  | Op DivInt -> true
  | Op DivFloat -> true
  | _ -> false

let is_first_of_prim_expr = function
  | ParenOpen -> true
  | IntLiteral _ -> true
  | FloatLiteral _ -> true
  | Keyword Array -> true
  | Keyword True -> true
  | Keyword False -> true
  | _ -> false

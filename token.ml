type opcode =
    | DoubleSemicolon | Semicolon
    | Or | And
    | Equal | NotEqual
    | Assign | ArrayAssign
    | LessEqual| GreaterEqual | Less | Greater
    | AddFloat | AddInt | SubFloat | SubInt
    | MulFloat | MulInt | DivFloat | DivInt
    | Dot
;;

type keyword = If | Then | Else | Let | Array | New

type token =
    | ParenOpen | ParenClose
    | Op of opcode
    | IntLiteral of int
    | FloatLiteral of float
    | Keyword of keyword
    | Identifier of string
;;

type opcode =
    | DoubleSemicolon | Semicolon
    | Or | And
    | Equal | NotEqual
    | Assign | ArrayAssign
    | LessEqualFloat | LessEqualInt | GreaterEqualFloat | GreaterEqualInt
    | LessFloat | LessInt | GreaterFloat | GreaterInt
    | AddFloat | AddInt | SubFloat | SubInt
    | MulFloat | MulInt | DivFloat | DivInt
    | Dot
;;

type token =
    | ParenOpen | ParenClose
    | Op of opcode
    | IntLiteral of int
    | FloatLiteral of float
    | Identifier of string
;;

open Token;;

let string_of_char = String.make 1;;

(* for debugging *)
let char_list_of_string str =
    let rec explode char_list = function
        | -1 -> char_list
        | index -> explode (str.[index] :: char_list) (index - 1)
    in
    explode [] (String.length str - 1)
;;

let is_whitespace = function
    | ' ' -> true
    | '\n' -> true
    | '\r' -> true
    | '\t' -> true
    | _ -> false
;;

let is_alpha_or_underscore = function
    | '_' -> true
    | c when 'a' <= c && c <= 'z' -> true
    | c when 'A' <= c && c <= 'Z' -> true
    | _ -> false
;;

let is_digit = function
    | c when '0' <= c && c <= '9' -> true
    | _ -> false
;;

let rec state_start tokens input = match input with
    | head :: tail when is_whitespace head -> state_start tokens tail

    | '(' :: tail -> state_start (ParenOpen :: tokens) tail
    | ')' :: tail -> state_start (ParenClose :: tokens) tail

    | ';' :: ';' :: tail -> state_start (Op DoubleSemicolon :: tokens) tail
    | ';' ::        tail -> state_start (Op Semicolon :: tokens) tail

    | '|' :: '|' :: tail -> state_start (Op Or :: tokens) tail
    | '&' :: '&' :: tail -> state_start (Op And :: tokens) tail

    | '=' :: '=' :: tail -> state_start (Op Equal :: tokens) tail
    | '!' :: '=' :: tail -> state_start (Op NotEqual :: tokens) tail

    | '=' ::        tail -> state_start (Op Assign :: tokens) tail
    | '<' :: '-' :: tail -> state_start (Op ArrayAssign :: tokens) tail

    | '<' :: '=' :: tail -> state_start (Op LessEqual :: tokens) tail
    | '<' ::        tail -> state_start (Op Less :: tokens) tail
    | '>' :: '=' :: tail -> state_start (Op GreaterEqual :: tokens) tail
    | '>' ::        tail -> state_start (Op Greater :: tokens) tail

    | '+' :: '.' :: tail -> state_start (Op AddFloat :: tokens) tail
    | '+' ::        tail -> state_start (Op AddInt :: tokens) tail

    | '-' :: '.' :: tail -> state_start (Op SubFloat :: tokens) tail
    | '-' ::        tail -> state_start (Op SubInt :: tokens) tail

    | '*' :: '.' :: tail -> state_start (Op MulFloat :: tokens) tail
    | '*' ::        tail -> state_start (Op MulInt :: tokens) tail

    | '/' :: '.' :: tail -> state_start (Op DivFloat :: tokens) tail
    | '/' ::        tail -> state_start (Op DivInt :: tokens) tail

    | '.' :: tail -> state_start (Op Dot :: tokens) tail

    | head :: tail when is_digit head               -> state_int tokens "" input
    | head :: tail when is_alpha_or_underscore head -> state_identifier tokens "" input

    | [] -> List.rev tokens
    | _ -> failwith "invalid EOF"

and state_int tokens working_string input = match input with
    | head :: tail when is_digit head -> state_int tokens (working_string ^ string_of_char head) tail
    | '.'  :: tail                    -> state_float tokens (working_string ^ ".") tail
    | _                               -> state_start (IntLiteral (int_of_string working_string) :: tokens) input

and state_float tokens working_string input = match input with
    | head :: tail when is_digit head -> state_float tokens (working_string ^ string_of_char head) tail
    | _                               -> state_start (FloatLiteral (float_of_string working_string) :: tokens) input

and state_identifier tokens working_string input = match input with
    | head :: tail when is_alpha_or_underscore head || is_digit head -> state_identifier tokens (working_string ^ string_of_char head) tail
    | _ -> match working_string with
        | "if"    -> state_start (Keyword If                :: tokens) input
        | "then"  -> state_start (Keyword Then              :: tokens) input
        | "else"  -> state_start (Keyword Else              :: tokens) input
        | "let"   -> state_start (Keyword Let               :: tokens) input
        | "Array" -> state_start (Keyword Array             :: tokens) input
        | "new"   -> state_start (Keyword New               :: tokens) input
        | "true"  -> state_start (Keyword True              :: tokens) input
        | "false" -> state_start (Keyword False             :: tokens) input
        | "int"   -> state_start (Keyword Int               :: tokens) input
        | "float" -> state_start (Keyword Float             :: tokens) input
        | "bool"  -> state_start (Keyword Bool              :: tokens) input
        | _       -> state_start (Identifier working_string :: tokens) input
;;

let lex = state_start [];;

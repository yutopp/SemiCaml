let rec char_list_of_in_channel in_channel =
  try
    let read_char = input_char in_channel in read_char :: char_list_of_in_channel in_channel
  with End_of_file -> []

let env = Analyzer.init_analyzer ()
        
let rec repl' () =
  print_string "# ";
  let input = (Parser.parse (Lexer.lex (Lexer.char_list_of_string (read_line ())))) in
  repl' (Interpreter.top_level_interpreter env input)

let in_file_repl charlist =
  let input = (Parser.parse (Lexer.lex charlist)) in
  repl' (Interpreter.top_level_interpreter env input)

let repl () =
  print_string "    \\SemiCaml/";
  print_newline ();
  repl' ()

let () =
  let argv_len = Array.length Sys.argv in
  match argv_len with
  | 1 ->
     Printf.printf "mean mean\n";
     let in_channel = open_in "test.seml" in
     let chars = char_list_of_in_channel in_channel in
     let tokens = Lexer.lex chars in
     let ast = Parser.parse tokens in
     Parser.dump ast;
     let attr_ast = Analyzer.analyze ast in
     flush stdout;
     let llvm_module = Codegen.compile attr_ast in
     Codegen.create_executable llvm_module
  | 2 ->
     let arg = Sys.argv.(1) in
     begin
       match arg with
       | "--repl" -> repl ()
       | _ -> failwith "invalid argument"
     end
  | 3 ->
     let arg1 = Sys.argv.(1) in
     let arg2 = Sys.argv.(2) in
     begin
       match (arg1, arg2) with         
       | ("--repl", filename) ->
          let in_channel =  open_in filename in
          let chars = char_list_of_in_channel in_channel in
          in_file_repl chars
       | _ -> failwith "invalid argument"
     end
  | _ -> failwith "invalid argument"

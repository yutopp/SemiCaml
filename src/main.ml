let rec char_list_of_in_channel in_channel =
  try
    let read_char = input_char in_channel in read_char :: char_list_of_in_channel in_channel
  with End_of_file -> []

let analyzer = Analyzer.create_analyzer ()

let rec repl' () =
  print_string "# ";
  let input = (Parser.parse (Lexer.lex (Lexer.char_list_of_string (read_line ())))) in
  repl' (Interpreter.top_level_interpreter analyzer input)

let in_file_repl charlist =
  let input = (Parser.parse (Lexer.lex charlist)) in
  repl' (Interpreter.top_level_interpreter analyzer input)

let repl () =
  print_string "    \\SemiCaml/";
  print_newline ();
  repl' ()

let build filename =
  print_string "\\SemiCaml/";
  print_string "mean mean\n";
  let in_channel = open_in filename in
  let chars = char_list_of_in_channel in_channel in

  print_string "=== Parsing...\n"; flush stdout;
  flush stdout;
  let tokens = Lexer.lex chars in
  let ast = Parser.parse tokens in
  Ast.dump ast;

  print_string "=== Analyzing...\n";
  flush stdout;
  let attr_ast = Analyzer.analyze ast in

  print_string "=== Generating...\n";
  flush stdout;
  let llvm_module = Codegen.compile attr_ast in
  Codegen.create_executable llvm_module;

  print_string "=== Executable a.out was generated successfully!\n";
  flush stdout

let show_usage () =
  print_string "Usage: semicaml <options>\n";
  print_string "options are\n";
  print_string "  <filename>         Build <filename>\n";
  print_string "  --repl [filename]  Start repl environment\n";
  print_string "\n";
  print_string "example\n";
  print_string "  semicaml samples/rec.seml\n";
  print_string "  semicaml --repl\n";
  print_string "  semicaml --repl samples/rec.seml\n";
  flush stdout

let () =
  let argv_len = Array.length Sys.argv in
  match argv_len with
  | 1 -> show_usage ()
  | 2 ->
     let arg = Sys.argv.(1) in
     begin
       match arg with
       | "--repl" -> repl ()
       | filename -> build filename
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

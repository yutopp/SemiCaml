let rec char_list_of_in_channel in_channel =
  try
    let read_char = input_char in_channel in read_char :: char_list_of_in_channel in_channel
  with End_of_file -> []

let analyzer = Analyzer.create_analyzer ()

let rec repl' () =
  print_string "# ";
  try
    let input = (Parser.parse (Lexer.lex (Lexer.char_list_of_string (read_line ())))) in
    repl' (Interpreter.top_level_interpreter analyzer input)
  with
  | ext ->
     print_string (Printexc.to_string ext);
     print_newline ();
     repl' ()

let in_file_repl charlist =
  let input = (Parser.parse (Lexer.lex charlist)) in
  repl' (Interpreter.top_level_interpreter analyzer input)

let repl () =
  print_string "    \\SemiCaml/";
  print_newline ();
  repl' ()

let build filename lib_path out_name =
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
  Codegen.create_executable llvm_module lib_path out_name;

  print_string (Printf.sprintf "=== Executable %s was generated successfully!\n" (Filename.quote out_name));
  flush stdout


let () =
  let input_file = ref None in
  let lib_path = ref "libsemiruntime.a" in
  let output_file = ref "a.out" in
  let repl_file = ref None in

  let usagemsg = "Usage: semicaml [filename] <options>\n"; in
  let speclist = [
    ("-o", Arg.Set_string output_file, " specify output file name");
    ("--lib", Arg.String (fun s -> lib_path := s), "set lib path");
    ("--repl", Arg.String (fun s -> repl_file := Some s), "filename  execute 'filename' in repl environment");
  ] in
  Arg.parse speclist (fun s -> (input_file := Some s)) usagemsg;

  match !input_file with
    Some filename ->
    begin
      (* compilation mode *)
      build filename !lib_path !output_file
    end

  | None ->
    begin
      (* repl *)
      match !repl_file with
        Some filename ->
        begin
          let in_channel = open_in filename in
          let chars = char_list_of_in_channel in_channel in
          in_file_repl chars
        end
      | None -> repl ()
    end

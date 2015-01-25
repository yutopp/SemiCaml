let rec char_list_of_in_channel in_channel = 
  try
    let read_char = input_char in_channel in read_char :: char_list_of_in_channel in_channel
  with End_of_file -> []

let () =
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

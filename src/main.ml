
let () =
  Printf.printf "mean mean\n";
  let ast = Parser.parse "test.seml" in
  Parser.dump ast;
  let attr_ast = Analyzer.analyze ast in
  flush stdout;
  let llvm_module = Codegen.compile attr_ast in
  Codegen.create_executable llvm_module

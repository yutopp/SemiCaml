
let () =
  Printf.printf "mean mean\n";
  let ast = Parser.parse "test.seml" in
  Parser.dump ast;
  let attr_ast = Analyzer.analyze ast in
  Codegen.compile attr_ast;
  ()


let () =
  Printf.printf "mean mean\n";
  let ast = Parser.parse "test.seml" in
  Parser.dump ast;
  let _ = Analyzer.analyze ast in
  ()

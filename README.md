# Documentation
[https://docs.google.com/document/d/1T98PsEW2-yOkTIdiHs8N2qvNJiBTmdihtPkQ_0kj4Ac/edit?usp=sharing](https://docs.google.com/document/d/1T98PsEW2-yOkTIdiHs8N2qvNJiBTmdihtPkQ_0kj4Ac/edit?usp=sharing)

# Requirement

+ OCaml 
+ LLVM [>= 3.5]
+ GCC (or Clang) [supports C++11]
+ Ruby (for testing, optional)

# How to build
```
opam update
opam install omake llvm ounit
omake
omake local
```

# How to test
after build  

all test
```
omake check
```

each test
```
omake check_parser
omake check_analyzer
omake check_interpreter
omake check_combine
ruby test/e2e_compiler/runtest.rb
```

# Usage
```
./semicaml <filename>
./semicaml <filename> -o output
./semicaml --repl [filename]
```

# License
NYSL

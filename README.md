# Documentation
[https://docs.google.com/document/d/1T98PsEW2-yOkTIdiHs8N2qvNJiBTmdihtPkQ_0kj4Ac/edit?usp=sharing](https://docs.google.com/document/d/1T98PsEW2-yOkTIdiHs8N2qvNJiBTmdihtPkQ_0kj4Ac/edit?usp=sharing)

# Requirement

+ OCaml 
+ LLVM [>= 3.5]
+ GCC (or Clang) [supports C++11]

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
```
```
omake check_analyzer
```
```
omake check_interpreter
```

# Usage
```
./semicaml
```
# REPL

```
./semicaml --repl
```

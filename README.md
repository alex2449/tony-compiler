### About

Compiler for a simple programming language called Tony, written for the purposes of the NTUA ECE undergraduate compilers course. The specification of the language can be found under ```docs/tony2020.pdf```.

### Dependencies
1. C++17
2. LLVM, version 10 or higher (tested up to version 17).
3. fmt library, https://fmt.dev/11.1/get-started/#installation

### Build
Simply run ```make``` to create the compiler executable, **tc**.

### Usage
Run ```./tc testcase.tony```. A file named **ir.ll** is created which contains the LLVM IR of the input program. You can display the usage of **tc** by running ```./tc --help```

### Useful resource
Modern Compiler Implementation in C, by Andrew W. Appel
https://www.cs.princeton.edu/~appel/modern/c/

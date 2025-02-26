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

### Features

- Readable error messages, trying to replicate clang's error messages, such as the following:

```
invalid.tony:4:19: error: invalid operand types to binary operator '+' ('int' and 'char')
        return 42 + "Hello World!\n"[0]
               ~~ ^ ~~~~~~~~~~~~~~~~~~~
invalid.tony:6:24: error: prepending 'int' to list of incompatible element type 'char'
        return head(17 # 'A' # nil)
                    ~~ ^ ~~~~~~~~~
invalid.tony:13:13: error: passing 'bool' to parameter of incompatible type 'char'
    bar(42, 42 > 17, '\n')
            ^~~~~~~
invalid.tony:9:25: note: passing argument to parameter 'c' here
    def bar(int i; char c; bool b):
                        ^
3 errors generated.
```

- Pretty printing of the AST. For example, with input file ```testcases/reverse.tony``` and the ```--dump-ast``` option given, the following output is produced:

```
FunctionDef <1:5> main
├─DeclarationList
│ └─FunctionDef <3:14> reverse char[]
│   ├─DeclarationList
│   │ └─VariableDecl <3:30> s char[]
│   ├─DeclarationList
│   │ ├─VariableDecl <4:12> t char[]
│   │ ├─VariableDecl <5:9> i int
│   │ └─VariableDecl <5:12> l int
│   └─StatementList
│     ├─AssignmentStmt <7:5-7:18>
│     │ ├─IdentifierExpr <7:5-7:5> int l
│     │ └─CallExpr <7:10-7:18> int strlen
│     │   └─ExpressionList
│     │     └─IdentifierExpr <7:17-7:17> char[] s
│     ├─AssignmentStmt <8:5-8:22>
│     │ ├─IdentifierExpr <8:5-8:5> char[] t
│     │ └─NewExpr <8:10-8:22> char[] char
│     │   └─BinaryExpr <8:19-8:21> int +
│     │     ├─IdentifierExpr <8:19-8:19> int l
│     │     └─IntegerExpr <8:21-8:21> int 1
│     ├─ForStmt <9:5-9:53>
│     │ ├─StatementList
│     │ │ └─AssignmentStmt <9:9-9:14>
│     │ │   ├─IdentifierExpr <9:9-9:9> int i
│     │ │   └─IntegerExpr <9:14-9:14> int 0
│     │ ├─BinaryExpr <9:17-9:21> bool <
│     │ │ ├─IdentifierExpr <9:17-9:17> int i
│     │ │ └─IdentifierExpr <9:21-9:21> int l
│     │ ├─StatementList
│     │ │ └─AssignmentStmt <9:24-9:31>
│     │ │   ├─IdentifierExpr <9:24-9:24> int i
│     │ │   └─BinaryExpr <9:29-9:31> int +
│     │ │     ├─IdentifierExpr <9:29-9:29> int i
│     │ │     └─IntegerExpr <9:31-9:31> int 1
│     │ └─StatementList
│     │   └─AssignmentStmt <9:34-9:49>
│     │     ├─IndexAccessExpr <9:34-9:37> char
│     │     │ ├─IdentifierExpr <9:34-9:34> char[] t
│     │     │ └─IdentifierExpr <9:36-9:36> int i
│     │     └─IndexAccessExpr <9:42-9:49> char
│     │       ├─IdentifierExpr <9:42-9:42> char[] s
│     │       └─BinaryExpr <9:44-9:48> int -
│     │         ├─BinaryExpr <9:44-9:46> int -
│     │         │ ├─IdentifierExpr <9:44-9:44> int l
│     │         │ └─IdentifierExpr <9:46-9:46> int i
│     │         └─IntegerExpr <9:48-9:48> int 1
│     ├─AssignmentStmt <10:5-10:16>
│     │ ├─IndexAccessExpr <10:5-10:8> char
│     │ │ ├─IdentifierExpr <10:5-10:5> char[] t
│     │ │ └─IdentifierExpr <10:7-10:7> int i
│     │ └─CharacterExpr <10:13-10:16> char '\x00'
│     └─ReturnStmt <11:5-11:12>
│       └─IdentifierExpr <11:12-11:12> char[] t
└─StatementList
  └─CallStmt <14:3-14:33> puts
    └─ExpressionList
      └─CallExpr <14:8-14:32> char[] reverse
        └─ExpressionList
          └─StringExpr <14:16-14:31> char[] "\x0A!dlrow olleH"
```

### Useful resource
Modern Compiler Implementation in C, by Andrew W. Appel
https://www.cs.princeton.edu/~appel/modern/c/

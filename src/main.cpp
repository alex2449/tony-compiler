#include <unistd.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include "ast-dump.hpp"
#include "ast.hpp"
#include "diagnostics.hpp"
#include "fmt/core.h"
#include "lexer.hpp"
#include "parser.hpp"
#include "source.hpp"
#include "translate.hpp"
#include "type-check.hpp"

static void PrintHelp(const char *path) {
  printf("Usage: %s [options] file\n"
         "Options:\n"
         "  --dump-ast     Display the AST to stdout\n"
         "  --dump-tokens  Run only the lexer stage and display the token sequence to stdout\n"
         "  -h, --help     Display this information\n", path);
  exit(0);
}

static void ParseCommandLineArguments(int argc, char *argv[], bool *dump_ast, bool *dump_tokens,
                                      const char **filename, Diagnostics &diagnostics) {
  // This should be replaced with something better at some point.
  for (int i = 1; i < argc; ++i) {
    if (strcmp(argv[i], "--dump-ast") == 0) {
      *dump_ast = true;
    } else if (strcmp(argv[i], "--dump-tokens") == 0) {
      *dump_tokens = true;
    } else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
      PrintHelp(argv[0]);
    } else {
      if (argv[i][0] == '-')
        diagnostics.Fatal(fmt::format("unknown option '{}'", argv[i]));

      if (*filename != nullptr)
        diagnostics.Fatal("only a single input file must be specified");

      *filename = argv[i];
    }
  }

  if (*filename == nullptr)
    diagnostics.Fatal("no input file specified");
}

static Source ReadFileContents(const char *filename, Diagnostics &diagnostics) {
  std::ifstream file;
  file.open(filename);

  // We check only if the file exists and not whether it is an ordinary file
  // whose format we can read. For example, this will not fail if the file is
  // a directory. The file will be read anyway and it will fail at lexing/parsing
  // with some weird error message.
  if (file.fail())
    diagnostics.Fatal(fmt::format("no such file: '{}'", filename));

  std::stringstream buffer;
  buffer << file.rdbuf();
  file.close();

  std::string contents = buffer.str();
  // Add a null byte at the end. Needed for the lexer and for EOF to have a valid source location.
  contents += '\0';
  return Source(filename, contents);
}

int main(int argc, char *argv[]) {
  bool dump_ast = false;
  bool dump_tokens = false;
  const char *filename = nullptr;

  Diagnostics diagnostics(isatty(STDERR_FILENO) == 1);

  ParseCommandLineArguments(argc, argv, &dump_ast, &dump_tokens, &filename, diagnostics);

  Source source = ReadFileContents(filename, diagnostics);

  Lexer lexer(source, diagnostics);

  if (dump_tokens) {
    lexer.LexSourceAndDumpTokens(true);
    return 0;
  }

  Parser parser(lexer, diagnostics);

  FunctionDefinition *root = parser.ParseProgram();

  std::vector<FunctionDefinition *> builtins = Declaration::CreateBuiltins();
  std::vector<FunctionDefinition *> func_defs = TypeCheck(root, builtins, diagnostics);

  if (dump_ast) {
    ASTDumpContext ctx(isatty(STDOUT_FILENO) == 1);
    root->Dump(ctx);
  }

  int number_of_errors = diagnostics.GetNumberOfErrors();

  if (number_of_errors > 0) {
    std::cerr << fmt::format("{} error{} generated.\n", number_of_errors, number_of_errors > 1 ? "s" : "");
    return 1;
  }

  Translate(func_defs, builtins, filename, diagnostics);
  return 0;
}

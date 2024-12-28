#include "ast.hpp"
#include "type.hpp"

std::vector<VariableDeclaration *> FunctionDefinition::GetLocalVariables() const {
  assert(this->locals != nullptr);
  std::vector<VariableDeclaration *> local_vars;
  for (Declaration *local : *this->locals)
    if (auto *local_var = dynamic_cast<VariableDeclaration *>(local))
      local_vars.push_back(local_var);
  return local_vars;
}

std::vector<FunctionDefinition *> Declaration::CreateBuiltins() {
  std::vector<FunctionDefinition *> builtins;

  // Output functions.
  {
    // decl puti (int n)
    std::vector<VariableDeclaration *> puti_parameters{
      new VariableDeclaration({}, "n", new IntType(), true, false)
    };
    FunctionDefinition *puti = new FunctionDefinition({}, "puti", nullptr, puti_parameters,
                                                      nullptr, nullptr, true);
    builtins.push_back(puti);
  }

  {
    // decl putb (bool b)
    std::vector<VariableDeclaration *> putb_parameters{
      new VariableDeclaration({}, "b", new BoolType(), true, false)
    };
    FunctionDefinition *putb = new FunctionDefinition({}, "putb", nullptr, putb_parameters,
                                                      nullptr, nullptr, true);
    builtins.push_back(putb);
  }

  {
    // decl putc (char c)
    std::vector<VariableDeclaration *> putc_parameters{
      new VariableDeclaration({}, "c", new CharType(), true, false)
    };
    FunctionDefinition *putc = new FunctionDefinition({}, "putc", nullptr, putc_parameters,
                                                      nullptr, nullptr, true);
    builtins.push_back(putc);
  }

  {
    // decl puts (char[] s)
    std::vector<VariableDeclaration *> puts_parameters{
      new VariableDeclaration({}, "s", new ArrayType(new CharType()), true, false)
    };
    FunctionDefinition *puts = new FunctionDefinition({}, "puts", nullptr, puts_parameters,
                                                      nullptr, nullptr, true);
    builtins.push_back(puts);
  }

  // Input functions.
  {
    // decl int geti ()
    FunctionDefinition *geti = new FunctionDefinition({}, "geti", new IntType(), {},
                                                      nullptr, nullptr, true);
    builtins.push_back(geti);
  }

  {
    // decl bool getb ()
    FunctionDefinition *getb = new FunctionDefinition({}, "getb", new BoolType(), {},
                                                      nullptr, nullptr, true);
    builtins.push_back(getb);
  }

  {
    // decl char getc ()
    FunctionDefinition *getc = new FunctionDefinition({}, "getc", new CharType(), {},
                                                      nullptr, nullptr, true);
    builtins.push_back(getc);
  }

  {
    // decl gets (int n, char[] s)
    std::vector<VariableDeclaration *> gets_parameters{
      new VariableDeclaration({}, "n", new IntType(), true, false),
      new VariableDeclaration({}, "s", new ArrayType(new CharType()), true, false)
    };
    FunctionDefinition *gets = new FunctionDefinition({}, "gets", nullptr, gets_parameters,
                                                      nullptr, nullptr, true);
    builtins.push_back(gets);
  }

  // Conversion functions.
  {
    // decl int abs (int n)
    std::vector<VariableDeclaration *> abs_parameters{
      new VariableDeclaration({}, "n", new IntType(), true, false)
    };
    FunctionDefinition *abs = new FunctionDefinition({}, "abs", new IntType(), abs_parameters,
                                                    nullptr, nullptr, true);
    builtins.push_back(abs);
  }

  {
    // decl int ord (char c)
    std::vector<VariableDeclaration *> ord_parameters{
      new VariableDeclaration({}, "c", new CharType(), true, false)
    };
    FunctionDefinition *ord = new FunctionDefinition({}, "ord", new IntType(), ord_parameters,
                                                    nullptr, nullptr, true);
    builtins.push_back(ord);
  }

  {
    // decl char chr (int n)
    std::vector<VariableDeclaration *> chr_parameters{
      new VariableDeclaration({}, "n", new IntType(), true, false)
    };
    FunctionDefinition *chr = new FunctionDefinition({}, "chr", new CharType(), chr_parameters,
                                                    nullptr, nullptr, true);
    builtins.push_back(chr);
  }

  // String handling functions.
  {
    // decl int strlen (char[] s)
    std::vector<VariableDeclaration *> strlen_parameters{
      new VariableDeclaration({}, "s", new ArrayType(new CharType()), true, false)
    };
    FunctionDefinition *strlen = new FunctionDefinition({}, "strlen", new IntType(), strlen_parameters,
                                                        nullptr, nullptr, true);
    builtins.push_back(strlen);
  }

  {
    // decl int strcmp (char[] s1, s2)
    std::vector<VariableDeclaration *> strcmp_parameters{
      new VariableDeclaration({}, "s1", new ArrayType(new CharType()), true, false),
      new VariableDeclaration({}, "s2", new ArrayType(new CharType()), true, false)
    };
    FunctionDefinition *strcmp = new FunctionDefinition({}, "strcmp", new IntType(), strcmp_parameters,
                                                        nullptr, nullptr, true);
    builtins.push_back(strcmp);
  }

  {
    // decl strcpy (char[] trg, src)
    std::vector<VariableDeclaration *> strcpy_parameters{
      new VariableDeclaration({}, "trg", new ArrayType(new CharType()), true, false),
      new VariableDeclaration({}, "src", new ArrayType(new CharType()), true, false)
    };
    FunctionDefinition *strcpy = new FunctionDefinition({}, "strcpy", nullptr, strcpy_parameters,
                                                        nullptr, nullptr, true);
    builtins.push_back(strcpy);
  }

  {
    // decl strcat (char[] trg, src)
    std::vector<VariableDeclaration *> strcat_parameters{
      new VariableDeclaration({}, "trg", new ArrayType(new CharType()), true, false),
      new VariableDeclaration({}, "src", new ArrayType(new CharType()), true, false)
    };
    FunctionDefinition *strcat = new FunctionDefinition({}, "strcat", nullptr, strcat_parameters,
                                                        nullptr, nullptr, true);
    builtins.push_back(strcat);
  }

  return builtins;
}

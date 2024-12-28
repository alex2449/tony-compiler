#ifndef SRC_TYPE_CHECK_HPP_
#define SRC_TYPE_CHECK_HPP_

#include <cassert>
#include <vector>
#include "bindings.hpp"

class Diagnostics;
class FunctionDefinition;

struct Context {
  explicit Context(Diagnostics &diagnostics) : diagnostics(diagnostics) {}

  bool AtTopLevel() const { return this->open_functions.empty(); }

  FunctionDefinition *CurrentFunction() const {
    assert(!this->AtTopLevel());
    return this->open_functions.back();
  }

  Diagnostics &diagnostics;
  // Stack of open functions we are currently inside of.
  std::vector<FunctionDefinition *> open_functions;
  // List of all defined functions we have processed.
  std::vector<FunctionDefinition *> func_defs;
  Bindings bindings;
};

std::vector<FunctionDefinition *> TypeCheck(FunctionDefinition *root,
                                            const std::vector<FunctionDefinition *> &builtins,
                                            Diagnostics &diagnostics);

#endif  // SRC_TYPE_CHECK_HPP_

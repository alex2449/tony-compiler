#ifndef SRC_AST_DUMP_HPP_
#define SRC_AST_DUMP_HPP_

#include <cassert>
#include <iostream>
#include <vector>
#include "color.hpp"

class ASTDumpContext {
 public:
  explicit ASTDumpContext(bool with_color) : with_color(with_color) {}

  ColorScope WithColor(const char *color) const {
    return ColorScope(std::cout, this->with_color, color);
  }

  void PrintIndent();

  void Indent(unsigned number_of_children) {
    assert(number_of_children > 0);
    this->indents.push_back(number_of_children);
  }

  void Unindent() {
    assert(!this->indents.empty());
    this->indents.pop_back();
  }

 private:
  // We use this vector to print the tree structure.
  std::vector<unsigned> indents;
  bool with_color;
};

#endif  // SRC_AST_DUMP_HPP_

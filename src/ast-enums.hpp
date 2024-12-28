#ifndef SRC_AST_ENUMS_HPP_
#define SRC_AST_ENUMS_HPP_

#include <string>

enum class UnaryOperator {
  Plus,   // +
  Minus,  // -
  Not,    // not
  NilQm,  // nil?
  Head,   // head
  Tail    // tail
};

enum class BinaryOperator {
  Plus,                // +
  Minus,               // -
  Mult,                // *
  Div,                 // /
  Mod,                 // mod
  Equal,               // =
  NotEqual,            // <>
  LessThan,            // <
  GreaterThan,         // >
  LessThanOrEqual,     // <=
  GreaterThanOrEqual,  // >=
  And,                 // and
  Or,                  // or
  Cons                 // #
};

std::string UnaryOperatorToString(UnaryOperator op);

std::string BinaryOperatorToString(BinaryOperator op);

#endif  // SRC_AST_ENUMS_HPP_

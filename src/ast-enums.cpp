#include "ast-enums.hpp"
#include <cassert>

std::string UnaryOperatorToString(UnaryOperator op) {
  switch (op) {
  case UnaryOperator::Plus:
    return "+";
  case UnaryOperator::Minus:
    return "-";
  case UnaryOperator::Not:
    return "not";
  case UnaryOperator::NilQm:
    return "nil?";
  case UnaryOperator::Head:
    return "head";
  case UnaryOperator::Tail:
    return "tail";
  default:
    assert(false && "unknown unary operator");
  }
}

std::string BinaryOperatorToString(BinaryOperator op) {
  switch (op) {
  case BinaryOperator::Plus:
    return "+";
  case BinaryOperator::Minus:
    return "-";
  case BinaryOperator::Mult:
    return "*";
  case BinaryOperator::Div:
    return "/";
  case BinaryOperator::Mod:
    return "mod";
  case BinaryOperator::Equal:
    return "=";
  case BinaryOperator::NotEqual:
    return "<>";
  case BinaryOperator::LessThan:
    return "<";
  case BinaryOperator::GreaterThan:
    return ">";
  case BinaryOperator::LessThanOrEqual:
    return "<=";
  case BinaryOperator::GreaterThanOrEqual:
    return ">=";
  case BinaryOperator::And:
    return "and";
  case BinaryOperator::Or:
    return "or";
  case BinaryOperator::Cons:
    return "#";
  default:
    assert(false && "unknown binary operator");
  }
}


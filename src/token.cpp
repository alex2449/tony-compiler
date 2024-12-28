#include "token.hpp"
#include <iostream>
#include "util.hpp"

Token::~Token() {
  if (this->kind == Kind::Identifier) {
    delete this->u.identifier_value;
  } else if (this->kind == Kind::String) {
    delete this->u.string_value;
  }
}

static std::string KeywordToString(Keyword keyword) {
  switch (keyword) {
  case Keyword::And:
    return "and";
  case Keyword::Bool:
    return "bool";
  case Keyword::Char:
    return "char";
  case Keyword::Decl:
    return "decl";
  case Keyword::Def:
    return "def";
  case Keyword::Else:
    return "else";
  case Keyword::Elsif:
    return "elsif";
  case Keyword::End:
    return "end";
  case Keyword::Exit:
    return "exit";
  case Keyword::False:
    return "false";
  case Keyword::For:
    return "for";
  case Keyword::Head:
    return "head";
  case Keyword::If:
    return "if";
  case Keyword::Int:
    return "int";
  case Keyword::List:
    return "list";
  case Keyword::Mod:
    return "mod";
  case Keyword::New:
    return "new";
  case Keyword::Nil:
    return "nil";
  case Keyword::NilQm:
    return "nil?";
  case Keyword::Not:
    return "not";
  case Keyword::Or:
    return "or";
  case Keyword::Ref:
    return "ref";
  case Keyword::Return:
    return "return";
  case Keyword::Skip:
    return "skip";
  case Keyword::Tail:
    return "tail";
  case Keyword::True:
    return "true";
  default:
    assert(false && "unknown keyword");
  }
}

static std::string OperatorToString(Operator op) {
  switch (op) {
  case Operator::Plus:
    return "+";
  case Operator::Minus:
    return "-";
  case Operator::Mult:
    return "*";
  case Operator::Div:
    return "/";
  case Operator::Cons:
    return "#";
  case Operator::Equal:
    return "=";
  case Operator::NotEqual:
    return "<>";
  case Operator::LessThan:
    return "<";
  case Operator::GreaterThan:
    return ">";
  case Operator::LessThanOrEqual:
    return "<=";
  case Operator::GreaterThanOrEqual:
    return ">=";
  default:
    assert(false && "unknown operator");
  }
}

static std::string SeparatorToString(Separator separator) {
  switch (separator) {
  case Separator::Lparen:
    return "(";
  case Separator::Rparen:
    return ")";
  case Separator::Lbracket:
    return "[";
  case Separator::Rbracket:
    return "]";
  case Separator::Comma:
    return ",";
  case Separator::Semicolon:
    return ";";
  case Separator::Colon:
    return ":";
  case Separator::Assign:
    return ":=";
  default:
    assert(false && "unknown separator");
  }
}

std::string Token::KindToString(Kind kind) {
  switch (kind) {
  case Kind::Keyword:
    return "Keyword";
  case Kind::Identifier:
    return "Identifier";
  case Kind::Integer:
    return "Integer";
  case Kind::Character:
    return "Character";
  case Kind::String:
    return "String";
  case Kind::Operator:
    return "Operator";
  case Kind::Separator:
    return "Separator";
  case Kind::EndOfFile:
    return "EndOfFile";
  case Kind::Error:
    return "Error";
  default:
    assert(false && "unknown token kind");
  }
}

void Token::Dump() const {
  std::cout << KindToString(this->kind) << " " << this->range.ToReadableString();

  switch (this->kind) {
  case Kind::Keyword:
    std::cout << " " << KeywordToString(this->u.keyword);
    break;
  case Kind::Identifier:
    std::cout << " " << *this->u.identifier_value;
    break;
  case Kind::Integer:
    std::cout << " " << this->u.integer_value;
    break;
  case Kind::Character:
    std::cout << " " << CharValueToLiteral(this->u.character_value);
    break;
  case Kind::String:
    std::cout << " " << StringValueToLiteral(*this->u.string_value);
    break;
  case Kind::Operator:
    std::cout << " " << OperatorToString(this->u.op);
    break;
  case Kind::Separator:
    std::cout << " " << SeparatorToString(this->u.separator);
    break;
  case Kind::EndOfFile:
    break;
  case Kind::Error:
    break;
  default:
    assert(false && "unknown token kind");
  }

  std::cout << "\n";
}

UnaryOperator Token::ToUnaryOperator() const {
  if (this->kind == Kind::Operator) {
    switch (this->u.op) {
    case Operator::Plus:
      return UnaryOperator::Plus;
    case Operator::Minus:
      return UnaryOperator::Minus;
    default:
      break;
    }
  }

  if (this->kind == Kind::Keyword) {
    switch (this->u.keyword) {
    case Keyword::Not:
      return UnaryOperator::Not;
    case Keyword::NilQm:
      return UnaryOperator::NilQm;
    case Keyword::Head:
      return UnaryOperator::Head;
    case Keyword::Tail:
      return UnaryOperator::Tail;
    default:
      break;
    }
  }

  assert(false && "token is not a unary operator");
}

BinaryOperator Token::ToBinaryOperator() const {
  if (this->kind == Kind::Operator) {
    switch (this->u.op) {
    case Operator::Plus:
      return BinaryOperator::Plus;
    case Operator::Minus:
      return BinaryOperator::Minus;
    case Operator::Mult:
      return BinaryOperator::Mult;
    case Operator::Div:
      return BinaryOperator::Div;
    case Operator::Equal:
      return BinaryOperator::Equal;
    case Operator::NotEqual:
      return BinaryOperator::NotEqual;
    case Operator::LessThan:
      return BinaryOperator::LessThan;
    case Operator::GreaterThan:
      return BinaryOperator::GreaterThan;
    case Operator::LessThanOrEqual:
      return BinaryOperator::LessThanOrEqual;
    case Operator::GreaterThanOrEqual:
      return BinaryOperator::GreaterThanOrEqual;
    case Operator::Cons:
      return BinaryOperator::Cons;
    default:
      break;
    }
  }

  if (this->kind == Kind::Keyword) {
    switch (this->u.keyword) {
    case Keyword::Mod:
      return BinaryOperator::Mod;
    case Keyword::And:
      return BinaryOperator::And;
    case Keyword::Or:
      return BinaryOperator::Or;
    default:
      break;
    }
  }

  assert(false && "token is not a binary operator");
}

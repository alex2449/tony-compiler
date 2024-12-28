#ifndef SRC_TOKEN_HPP_
#define SRC_TOKEN_HPP_

#include <cassert>
#include <string>
#include "ast-enums.hpp"
#include "source-location.hpp"

enum class Keyword {
  And,
  Bool,
  Char,
  Decl,
  Def,
  Else,
  Elsif,
  End,
  Exit,
  False,
  For,
  Head,
  If,
  Int,
  List,
  Mod,
  New,
  Nil,
  NilQm,
  Not,
  Or,
  Ref,
  Return,
  Skip,
  Tail,
  True
};

// Symbolic operators.

enum class Operator {
  Plus,                // +
  Minus,               // -
  Mult,                // *
  Div,                 // /
  Cons,                // #
  Equal,               // =
  NotEqual,            // <>
  LessThan,            // <
  GreaterThan,         // >
  LessThanOrEqual,     // <=
  GreaterThanOrEqual,  // >=
};

enum class Separator {
  Lparen,     // (
  Rparen,     // )
  Lbracket,   // [
  Rbracket,   // ]
  Comma,      // ,
  Semicolon,  // ;
  Colon,      // :
  Assign      // :=
};

class Token {
 private:
  enum class Kind {
    Keyword,
    Identifier,
    Integer,
    Character,
    String,
    Operator,
    Separator,
    EndOfFile,
    // The error token is used when the lexer can't return a valid token.
    // The parser should treat it as a token that doesn't match with any rule
    // and go into error recovery.
    Error
  };

 public:
  static Token *MakeKeyword(const SourceRange &range, Keyword keyword) {
    Token *token = new Token(Kind::Keyword, range);
    token->u.keyword = keyword;
    return token;
  }

  static Token *MakeIdentifier(const SourceRange &range, const std::string &value) {
    Token *token = new Token(Kind::Identifier, range);
    token->u.identifier_value = new std::string(value);
    return token;
  }

  static Token *MakeInteger(const SourceRange &range, int value) {
    Token *token = new Token(Kind::Integer, range);
    token->u.integer_value = value;
    return token;
  }

  static Token *MakeCharacter(const SourceRange &range, char value) {
    Token *token = new Token(Kind::Character, range);
    token->u.character_value = value;
    return token;
  }

  static Token *MakeString(const SourceRange &range, const std::string &value) {
    Token *token = new Token(Kind::String, range);
    token->u.string_value = new std::string(value);
    return token;
  }

  static Token *MakeOperator(const SourceRange &range, Operator op) {
    Token *token = new Token(Kind::Operator, range);
    token->u.op = op;
    return token;
  }

  static Token *MakeSeparator(const SourceRange &range, Separator separator) {
    Token *token = new Token(Kind::Separator, range);
    token->u.separator = separator;
    return token;
  }

  static Token *MakeEndOfFile(const SourceRange &range) {
    return new Token(Kind::EndOfFile, range);
  }

  static Token *MakeError(const SourceRange &range) {
    return new Token(Kind::Error, range);
  }

  ~Token();

  bool IsKeyword() const { return this->kind == Kind::Keyword; }
  bool IsIdentifier() const { return this->kind == Kind::Identifier; }
  bool IsInteger() const { return this->kind == Kind::Integer; }
  bool IsCharacter() const { return this->kind == Kind::Character; }
  bool IsString() const {return this->kind == Kind::String; }
  bool IsOperator() const { return this->kind == Kind::Operator; }
  bool IsSeparator() const { return this->kind == Kind::Separator; }
  bool IsEndOfFile() const { return this->kind == Kind::EndOfFile; }
  bool IsError() const { return this->kind == Kind::Error; }

  bool IsKeyword(Keyword keyword) const {
    return this->kind == Kind::Keyword && this->u.keyword == keyword;
  }

  bool IsOperator(Operator op) const {
    return this->kind == Kind::Operator && this->u.op == op;
  }

  bool IsSeparator(Separator separator) const {
    return this->kind == Kind::Separator && this->u.separator == separator;
  }

  const SourceRange &GetSourceRange() const {
    return this->range;
  }

  const std::string &GetIdentifierValue() const {
    assert(this->kind == Kind::Identifier);
    return *this->u.identifier_value;
  }

  int GetIntegerValue() const {
    assert(this->kind == Kind::Integer);
    return this->u.integer_value;
  }

  char GetCharacterValue() const {
    assert(this->kind == Kind::Character);
    return this->u.character_value;
  }

  const std::string &GetStringValue() const {
    assert(this->kind == Kind::String);
    return *this->u.string_value;
  }

  UnaryOperator ToUnaryOperator() const;
  BinaryOperator ToBinaryOperator() const;

  void Dump() const;

 private:
  Token(Kind kind, const SourceRange &range) : kind(kind), range(range) {}

  static std::string KindToString(Kind kind);

  Kind kind;
  // Range in source code.
  SourceRange range;
  union {
    Keyword keyword;
    std::string *identifier_value;
    int integer_value;
    char character_value;
    std::string *string_value;
    Operator op;
    Separator separator;
  } u;
};

#endif  // SRC_TOKEN_HPP_

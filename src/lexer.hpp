#ifndef SRC_LEXER_HPP_
#define SRC_LEXER_HPP_

#include <string>
#include "token.hpp"

class Diagnostics;
class Source;

class Lexer {
 public:
  Lexer(Source &source, Diagnostics &diagnostics)
      : source(source), diagnostics(diagnostics) {}

  void LexSourceAndDumpTokens(bool print_source);

  Token *LexNextToken();

 private:
  static bool IsCommon(char c);
  static bool IsWhiteSpace(char c);
  static bool IsDigit(char c);
  static bool IsHexDigit(char c);
  static bool IsLetter(char c);
  static char HexDigitToInt(char c);

  char CurrentChar() const;
  char PeekChar() const;
  char AdvanceChar();

  void SkipMultiLineComment();
  std::string LexQuotedLiteral(bool &saw_error, unsigned &end_pos);
  void LexEscapeSequence(std::string &value, bool &saw_error);

  Token *ConsumeOneCharacterOperator(Operator op);
  Token *ConsumeTwoCharacterOperator(Operator op);
  Token *ConsumeOneCharacterSeparator(Separator separator);
  Token *ConsumeTwoCharacterSeparator(Separator separator);

  // The source we are lexing.
  Source &source;
  Diagnostics &diagnostics;
  // The current position in source.
  unsigned position = 0;
  bool saw_eof = false;
};

#endif  // SRC_LEXER_HPP_

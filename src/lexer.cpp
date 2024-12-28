#include "lexer.hpp"
#include <cassert>
#include <unordered_map>
#include <utility>
#include "diagnostics.hpp"
#include "fmt/core.h"
#include "source-location.hpp"
#include "source.hpp"
#include "util.hpp"

bool Lexer::IsCommon(char c) {
  return IsAsciiPrintable(c) && c != '\'' && c != '\"' && c != '\\';
}

bool Lexer::IsWhiteSpace(char c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

bool Lexer::IsDigit(char c) {
  return '0' <= c && c <= '9';
}

bool Lexer::IsHexDigit(char c) {
  return Lexer::IsDigit(c) || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f');
}

bool Lexer::IsLetter(char c) {
  return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z');
}

char Lexer::HexDigitToInt(char c) {
  if ('0' <= c && c <= '9') return c - '0';
  if ('A' <= c && c <= 'F') return c - 'A' + 10;
  if ('a' <= c && c <= 'f') return c - 'a' + 10;
  assert(false && "c is not a hex digit");
}

static const std::unordered_map<std::string, Keyword> keywords_map = {
  {"and", Keyword::And},
  {"bool", Keyword::Bool},
  {"char", Keyword::Char},
  {"decl", Keyword::Decl},
  {"def", Keyword::Def},
  {"else", Keyword::Else},
  {"elsif", Keyword::Elsif},
  {"end", Keyword::End},
  {"exit", Keyword::Exit},
  {"false", Keyword::False},
  {"for", Keyword::For},
  {"head", Keyword::Head},
  {"if", Keyword::If},
  {"int", Keyword::Int},
  {"list", Keyword::List},
  {"mod", Keyword::Mod},
  {"new", Keyword::New},
  {"nil", Keyword::Nil},
  {"nil?", Keyword::NilQm},
  {"not", Keyword::Not},
  {"or", Keyword::Or},
  {"ref", Keyword::Ref},
  {"return", Keyword::Return},
  {"skip", Keyword::Skip},
  {"tail", Keyword::Tail},
  {"true", Keyword::True}
};

void Lexer::LexSourceAndDumpTokens(bool print_source) {
  Token *token;

  do {
    token = this->LexNextToken();
    token->Dump();

    if (print_source)
      this->diagnostics.PrintSource(token->GetSourceRange().GetStartLoc(), {token->GetSourceRange()});
  } while (!token->IsEndOfFile());
}

char Lexer::CurrentChar() const {
  return this->source[this->position];
}

char Lexer::PeekChar() const {
  return this->source[this->position + 1];
}

char Lexer::AdvanceChar() {
  if (this->CurrentChar() == '\n')
    this->source.NewLine(this->position);
  return this->source[++this->position];
}

Token *Lexer::LexNextToken() {
  assert(!this->saw_eof && "nothing left to lex");

  // Skip whitespace and comments.
  while (true) {
    char c = this->CurrentChar();

    if (Lexer::IsWhiteSpace(c)) {
      this->AdvanceChar();
    } else if (c == '%') {
      // Skip single-line comment.
      while (true) {
        c = this->AdvanceChar();
        if (c == '\n' || c == '\0')
          break;
      }
    } else if (c == '<' && this->PeekChar() == '*') {
      this->SkipMultiLineComment();
    } else {
      break;
    }
  }

  char c = this->CurrentChar();

  if (Lexer::IsLetter(c)) {
    unsigned start_pos = this->position;

    std::string value;
    do {
      value += c;
      c = this->AdvanceChar();
    } while (Lexer::IsLetter(c) || Lexer::IsDigit(c) || c == '_' || c == '?');

    SourceRange range(start_pos, this->position - 1, this->source);

    std::unordered_map<std::string, Keyword>::const_iterator it = keywords_map.find(value);
    if (it != keywords_map.end())
      return Token::MakeKeyword(range, it->second);

    return Token::MakeIdentifier(range, value);
  }

  if (Lexer::IsDigit(c)) {
    unsigned start_pos = this->position;

    int value = 0;
    do {
      // This will overflow for large values.
      value = value * 10 + (c - '0');
      c = this->AdvanceChar();
    } while (Lexer::IsDigit(c));

    SourceRange range(start_pos, this->position - 1, this->source);
    return Token::MakeInteger(range, value);
  }

  if (c == '\'' || c == '\"') {
    // We lex character literals and string literals similarly.
    // For character literals, we check if exactly one character was lexed.
    unsigned start_pos = this->position;
    unsigned end_pos;

    bool saw_error = false;

    std::string value = this->LexQuotedLiteral(saw_error, end_pos);

    SourceRange range(start_pos, end_pos, this->source);

    // Check for empty or multi-character character literals.
    // Only report an error if an error was not already reported.
    if (c == '\'' && !saw_error) {
      if (value.empty()) {
        saw_error = true;
        this->diagnostics.ReportError(range, "empty character literal");
      }

      if (value.size() > 1) {
        saw_error = true;
        this->diagnostics.ReportError(range, "multi-character character literal");
      }
    }

    if (saw_error)
      return Token::MakeError(range);

    return (c == '\''
            ? Token::MakeCharacter(range, value[0])
            : Token::MakeString(range, value));
  }

  switch (c) {
  case '+':
    return this->ConsumeOneCharacterOperator(Operator::Plus);
  case '-':
    return this->ConsumeOneCharacterOperator(Operator::Minus);
  case '*':
    return this->ConsumeOneCharacterOperator(Operator::Mult);
  case '/':
    return this->ConsumeOneCharacterOperator(Operator::Div);
  case '#':
    return this->ConsumeOneCharacterOperator(Operator::Cons);
  case '=':
    return this->ConsumeOneCharacterOperator(Operator::Equal);
  case '<':
    if (this->PeekChar() == '>')
      return this->ConsumeTwoCharacterOperator(Operator::NotEqual);
    if (this->PeekChar() == '=')
      return this->ConsumeTwoCharacterOperator(Operator::LessThanOrEqual);
    return this->ConsumeOneCharacterOperator(Operator::LessThan);
  case '>':
    if (this->PeekChar() == '=')
      return this->ConsumeTwoCharacterOperator(Operator::GreaterThanOrEqual);
    return this->ConsumeOneCharacterOperator(Operator::GreaterThan);
  }

  switch (c) {
  case '(':
    return this->ConsumeOneCharacterSeparator(Separator::Lparen);
  case ')':
    return this->ConsumeOneCharacterSeparator(Separator::Rparen);
  case '[':
    return this->ConsumeOneCharacterSeparator(Separator::Lbracket);
  case ']':
    return this->ConsumeOneCharacterSeparator(Separator::Rbracket);
  case ',':
    return this->ConsumeOneCharacterSeparator(Separator::Comma);
  case ';':
    return this->ConsumeOneCharacterSeparator(Separator::Semicolon);
  case ':':
    if (this->PeekChar() == '=')
      return this->ConsumeTwoCharacterSeparator(Separator::Assign);
    return this->ConsumeOneCharacterSeparator(Separator::Colon);
  }

  if (c == '\0') {
    this->saw_eof = true;
    SourceRange range(this->position, this->position, this->source);
    return Token::MakeEndOfFile(range);
  }

  SourceLocation location(this->position, this->source);
  this->diagnostics.ReportError(location, fmt::format("can't match character '{}'", c));

  SourceRange range(this->position, this->position, this->source);
  this->AdvanceChar();

  return Token::MakeError(range);
}

void Lexer::SkipMultiLineComment() {
  assert(this->CurrentChar() == '<');

  unsigned hold_position = this->position;

  this->AdvanceChar();
  assert(this->CurrentChar() == '*');

  this->AdvanceChar();

  while (true) {
    char c = this->CurrentChar();

    if (c == '*' && this->PeekChar() == '>') {
      // Consume both '*' and '>'.
      this->AdvanceChar();
      this->AdvanceChar();
      return;
    } else if (c == '<' && this->PeekChar() == '*') {
      // Multi-line comments can be nested.
      this->SkipMultiLineComment();
    } else if (c == '\0') {
      SourceLocation location(hold_position, this->source);
      this->diagnostics.ReportError(location, "unterminated comment");
      return;
    } else {
      this->AdvanceChar();
    }
  }
}

// Lex a sequence of zero or more common characters or escape sequences within single or double quotes.

std::string Lexer::LexQuotedLiteral(bool &saw_error, unsigned &end_pos) {
  char quotes = this->CurrentChar();
  assert(quotes == '\'' || quotes == '\"');

  unsigned start_pos = this->position;

  std::string value;

  this->AdvanceChar();

  while (true) {
    char c = this->CurrentChar();

    if (c == '\n' || c == '\0') {
      SourceRange range(start_pos, this->position - 1, this->source);
      std::string message = fmt::format("unterminated {} literal", quotes == '\'' ? "character" : "string");
      this->diagnostics.ReportError(range, message);
      saw_error = true;
      end_pos = this->position - 1;
      break;
    } else if (c == quotes) {
      end_pos = this->position;
      this->AdvanceChar();
      break;
    } else if (Lexer::IsCommon(c)) {
      value += c;
      this->AdvanceChar();
    } else if (c == '\\') {
      this->LexEscapeSequence(value, saw_error);
    } else {
      if (!saw_error) {
        saw_error = true;
        SourceLocation location(this->position, this->source);
        if (c == '\t') {
          std::string message = fmt::format("tabs are not allowed within {} literals, use the '\\t' escape "
                                            "sequence instead", quotes == '\'' ? "character" : "string");
          this->diagnostics.ReportError(location, message);
        } else {
          std::string message = fmt::format("expected common character or escape sequence "
                                            "(found value {})", CharValueToLiteral(c));
          this->diagnostics.ReportError(location, message);
        }
      }
      this->AdvanceChar();
    }
  }

  return value;
}

void Lexer::LexEscapeSequence(std::string &value, bool &saw_error) {
  assert(this->CurrentChar() == '\\');

  unsigned start_pos = this->position;

  char c = this->AdvanceChar();
  switch (c) {
  case 'n':
    value += '\n';
    this->AdvanceChar();
    return;
  case 't':
    value += '\t';
    this->AdvanceChar();
    return;
  case 'r':
    value += '\r';
    this->AdvanceChar();
    return;
  case '0':
    value += '\0';
    this->AdvanceChar();
    return;
  case '\\':
    value += '\\';
    this->AdvanceChar();
    return;
  case '\'':
    value += '\'';
    this->AdvanceChar();
    return;
  case '\"':
    value += '\"';
    this->AdvanceChar();
    return;
  case 'x': {
    c = this->AdvanceChar();
    if (!Lexer::IsHexDigit(c))
      break;
    char high = Lexer::HexDigitToInt(c);

    c = this->AdvanceChar();
    if (!Lexer::IsHexDigit(c))
      break;
    char low = Lexer::HexDigitToInt(c);

    value += (high << 4) | low;
    this->AdvanceChar();
    return;
  }
  default:
    break;
  }

  if (!saw_error) {
    saw_error = true;
    unsigned end_pos = (c == '\n' || c == '\0') ? this->position - 1 : this->position;
    SourceRange range(start_pos, end_pos, this->source);
    this->diagnostics.ReportError(range, "invalid escape sequence");
  }
}

Token *Lexer::ConsumeOneCharacterOperator(Operator op) {
  SourceRange range(this->position, this->position, this->source);
  this->AdvanceChar();
  return Token::MakeOperator(range, op);
}

Token *Lexer::ConsumeTwoCharacterOperator(Operator op) {
  SourceRange range(this->position, this->position + 1, this->source);
  this->AdvanceChar();
  this->AdvanceChar();
  return Token::MakeOperator(range, op);
}

Token *Lexer::ConsumeOneCharacterSeparator(Separator separator) {
  SourceRange range(this->position, this->position, this->source);
  this->AdvanceChar();
  return Token::MakeSeparator(range, separator);
}

Token *Lexer::ConsumeTwoCharacterSeparator(Separator separator) {
  SourceRange range(this->position, this->position + 1, this->source);
  this->AdvanceChar();
  this->AdvanceChar();
  return Token::MakeSeparator(range, separator);
}

#include "util.hpp"
#include <cassert>

bool IsAsciiPrintable(char c) {
  return 0x20 <= c && c <= 0x7E;
}

static char IntToHexDigit(char c) {
  if (0 <= c && c <= 9)
    return c + '0';
  if (10 <= c && c <= 15)
    return c - 10 + 'A';
  assert(false && "c is larger than a hex digit");
}

static std::string CharValueToLiteralWithoutQuotes(char c) {
  if (IsAsciiPrintable(c)) {
    if (c == '\'' || c == '\"' || c == '\\')
      return {'\\', c};
    return {c};
  }

  char high = IntToHexDigit((c >> 4) & 0x0F);
  char low = IntToHexDigit(c & 0x0F);
  return {'\\', 'x', high, low};
}

std::string CharValueToLiteral(char c) {
  return "\'" + CharValueToLiteralWithoutQuotes(c) + "\'";
}

std::string StringValueToLiteral(const std::string &s) {
  std::string buffer = "\"";
  for (char c : s)
    buffer += CharValueToLiteralWithoutQuotes(c);
  return buffer + "\"";
}

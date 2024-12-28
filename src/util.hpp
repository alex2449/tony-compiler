#ifndef SRC_UTIL_HPP_
#define SRC_UTIL_HPP_

#include <string>

bool IsAsciiPrintable(char c);

std::string CharValueToLiteral(char c);

std::string StringValueToLiteral(const std::string &s);

#endif  // SRC_UTIL_HPP_

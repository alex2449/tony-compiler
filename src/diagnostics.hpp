#ifndef SRC_DIAGNOSTICS_HPP_
#define SRC_DIAGNOSTICS_HPP_

#include <iostream>
#include <string>
#include <vector>
#include "color.hpp"

class SourceLocation;
class SourceRange;

class Diagnostics {
 public:
  explicit Diagnostics(bool with_color) : with_color(with_color) {}

  void ReportError(const SourceLocation &caret, const std::vector<SourceRange> &highlight, const std::string &message);
  void ReportError(const SourceLocation &caret, const std::string &message);
  void ReportError(const SourceRange &caret, const std::string &message);

  void Warn(const SourceLocation &caret, const std::string &message) const;

  void Inform(const SourceLocation &caret, const std::string &message) const;

  [[ noreturn ]] void Fatal(const SourceLocation &caret, const std::string &message) const;
  [[ noreturn ]] void Fatal(const std::string &message) const;

  int GetNumberOfErrors() const { return this->number_of_errors; }

 private:
  enum class DiagnosticKind {
    Error,
    Warning,
    Note,
    Fatal
  };

  void PrintMessage(DiagnosticKind kind, const SourceLocation *caret, const std::string &message) const;

 public:
  void PrintSource(const SourceLocation &caret, const std::vector<SourceRange> &highlight) const;

 private:
  ColorScope WithColor(const char *color) const {
    return ColorScope(std::cerr, this->with_color, color);
  }

  static std::string DiagnosticKindToString(DiagnosticKind kind);

  static const char *DiagnosticKindColor(DiagnosticKind kind);

  bool with_color;
  int number_of_errors = 0;
  static const int max_number_of_errors = 10;
};

#endif  // SRC_DIAGNOSTICS_HPP_

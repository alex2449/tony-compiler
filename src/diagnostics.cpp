#include "diagnostics.hpp"
#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <utility>
#include "fmt/core.h"
#include "source-location.hpp"
#include "source.hpp"

void Diagnostics::ReportError(const SourceLocation &caret,
                              const std::vector<SourceRange> &highlight,
                              const std::string &message) {
  this->PrintMessage(DiagnosticKind::Error, &caret, message);
  this->PrintSource(caret, highlight);
  if (++this->number_of_errors == Diagnostics::max_number_of_errors)
    this->Fatal("too many errors emitted, stopping now");
}

void Diagnostics::ReportError(const SourceLocation &caret, const std::string &message) {
  this->ReportError(caret, {}, message);
}

void Diagnostics::ReportError(const SourceRange &caret, const std::string &message) {
  this->ReportError(caret.GetStartLoc(), {caret}, message);
}

void Diagnostics::Warn(const SourceLocation &caret, const std::string &message) const {
  this->PrintMessage(DiagnosticKind::Warning, &caret, message);
  this->PrintSource(caret, {});
}

void Diagnostics::Inform(const SourceLocation &caret, const std::string &message) const {
  this->PrintMessage(DiagnosticKind::Note, &caret, message);
  this->PrintSource(caret, {});
}

void Diagnostics::Fatal(const SourceLocation &caret, const std::string &message) const {
  this->PrintMessage(DiagnosticKind::Fatal, &caret, message);
  this->PrintSource(caret, {});
  exit(1);
}

void Diagnostics::Fatal(const std::string &message) const {
  this->PrintMessage(DiagnosticKind::Fatal, nullptr, message);
  exit(1);
}

void Diagnostics::PrintMessage(DiagnosticKind kind, const SourceLocation *caret, const std::string &message) const {
  if (caret != nullptr) {
    const std::string &filename = caret->GetSource().GetFilename();
    auto [line, column] = caret->ConvertToLineAndColumn();
    this->WithColor(BOLD_HIGH_INTENSITY_WHITE) << fmt::format("{}:{}:{}: ", filename, line, column);
  }

  this->WithColor(DiagnosticKindColor(kind)) << DiagnosticKindToString(kind) << ": ";
  this->WithColor(BOLD_HIGH_INTENSITY_WHITE) << message << "\n";
}

void Diagnostics::PrintSource(const SourceLocation &caret, const std::vector<SourceRange> &highlight) const {
  const Source &source = caret.GetSource();
  unsigned caret_line = std::get<0>(caret.ConvertToLineAndColumn());
  unsigned line_start_pos = source.GetLineStartPosition(caret_line);

  // Print source.
  for (unsigned p = line_start_pos; source[p] != '\n' && source[p] != '\0'; ++p)
    std::cerr << source[p];
  std::cerr << "\n";

  // Check if the provided source location and source ranges are not entirely in a single line.
  bool spans_multiple_lines = std::any_of(highlight.begin(), highlight.end(),
                                          [=](const SourceRange &range) {
                                            return !range.IsWithinLine(caret_line);
                                          });

  // Print the caret character '^' and highlight the source ranges.
  for (unsigned p = line_start_pos, column = 1;; ++p) {
    bool is_highlight_position = std::any_of(highlight.begin(), highlight.end(),
                                             [=](const SourceRange &range) {
                                               return range.Contains(p);
                                             });

    if (source[p] == '\t') {
      // Round to next greater multiple of 8 (and add one).
      unsigned tab_stop_column = ((column + 7) & (-8)) + 1;
      // Number of characters needed to reach the next tab stop.
      unsigned n = tab_stop_column - column;

      if (p == caret.GetPosition()) {
        this->WithColor(BOLD_GREEN) << '^' << std::string(n - 1, '~');
      } else if (is_highlight_position && !spans_multiple_lines) {
        this->WithColor(BOLD_GREEN) << std::string(n, '~');
      } else {
        // Just print the tab character itself.
        std::cerr << '\t';
      }

      column = tab_stop_column;
    } else {
      if (p == caret.GetPosition()) {
        this->WithColor(BOLD_GREEN) << '^';
      } else if (is_highlight_position && !spans_multiple_lines) {
        this->WithColor(BOLD_GREEN) << '~';
      } else {
        std::cerr << ' ';
      }

      ++column;
    }

    // Break here and not in the for-loop condition to allow printing the caret
    // on a new line character (never actually happens currently) or EOF.
    if (source[p] == '\n' || source[p] == '\0')
      break;
  }

  std::cerr << "\n";
}

std::string Diagnostics::DiagnosticKindToString(DiagnosticKind kind) {
  switch (kind) {
  case DiagnosticKind::Error:
    return "error";
  case DiagnosticKind::Warning:
    return "warning";
  case DiagnosticKind::Note:
    return "note";
  case DiagnosticKind::Fatal:
    return "fatal error";
  default:
    assert(false && "unknown diagnostic kind");
  }
}

const char *Diagnostics::DiagnosticKindColor(DiagnosticKind kind) {
  switch (kind) {
  case DiagnosticKind::Error:
    return BOLD_RED;
  case DiagnosticKind::Warning:
    return BOLD_PURPLE;
  case DiagnosticKind::Note:
    return BOLD_CYAN;
  case DiagnosticKind::Fatal:
    return BOLD_RED;
  default:
    assert(false && "unknown diagnostic kind");
  }
}

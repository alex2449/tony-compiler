#ifndef SRC_SOURCE_LOCATION_HPP_
#define SRC_SOURCE_LOCATION_HPP_

#include <cassert>
#include <string>
#include <utility>
#include "fmt/core.h"
#include "source.hpp"

class SourceLocation {
 public:
  SourceLocation(unsigned position, const Source &source)
      : position(position), source(source) {
    assert(position < source.GetSize());
  }

  unsigned GetPosition() const { return this->position; }
  const Source &GetSource() const { return this->source; }

  std::pair<unsigned, unsigned> ConvertToLineAndColumn() const {
    return this->source.ConvertPositionToLineAndColumm(this->position);
  }

  std::string ToReadableString() const {
    auto [line, column] = this->ConvertToLineAndColumn();
    return fmt::format("<{}:{}>", line, column);
  }

 private:
  unsigned position;
  const Source &source;
};

class SourceRange {
 public:
  SourceRange(unsigned start_pos, unsigned end_pos, const Source &source)
      : start_pos(start_pos), end_pos(end_pos), source(source) {
    assert(start_pos < source.GetSize() && end_pos < source.GetSize() && start_pos <= end_pos);
  }

  SourceRange(const SourceLocation &start_loc, const SourceLocation &end_loc)
      : SourceRange(start_loc.GetPosition(), end_loc.GetPosition(), start_loc.GetSource()) {}

  SourceLocation GetStartLoc() const { return SourceLocation(this->start_pos, this->source); }
  SourceLocation GetEndLoc() const { return SourceLocation(this->end_pos, this->source); }

  bool Contains(unsigned position) const {
    return (this->start_pos <= position) && (position <= this->end_pos);
  }

  bool IsWithinLine(unsigned line) const {
    unsigned start_line = std::get<0>(this->source.ConvertPositionToLineAndColumm(this->start_pos));
    unsigned end_line = std::get<0>(this->source.ConvertPositionToLineAndColumm(this->end_pos));
    return (start_line == end_line) && (start_line == line);
  }

  std::string ToReadableString() const {
    auto [start_line, start_column] = this->source.ConvertPositionToLineAndColumm(this->start_pos);
    auto [end_line, end_column] = this->source.ConvertPositionToLineAndColumm(this->end_pos);
    return fmt::format("<{}:{}-{}:{}>", start_line, start_column, end_line, end_column);
  }

 private:
  unsigned start_pos;
  unsigned end_pos;
  const Source &source;
};

#endif  // SRC_SOURCE_LOCATION_HPP_

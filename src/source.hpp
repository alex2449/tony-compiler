#ifndef SRC_SOURCE_HPP_
#define SRC_SOURCE_HPP_

#include <cassert>
#include <string>
#include <utility>
#include <vector>

class Source {
 public:
  Source(const std::string &filename, const std::string &contents)
      : filename(filename), contents(contents) {
    assert(!contents.empty() && contents.back() == '\0');
  }

  const std::string &GetFilename() const { return this->filename; }

  unsigned GetSize() const { return this->contents.size(); }

  char operator[](unsigned position) const { return this->contents.at(position); }

  void NewLine(unsigned position) { this->new_line_positions.push_back(position); }

  // Returns the position of the first character of the given line.
  unsigned GetLineStartPosition(unsigned line) const {
    assert(line > 0);
    if (line == 1)
      return 0;
    return this->new_line_positions.at(line - 2) + 1;
  }

  std::pair<unsigned, unsigned> ConvertPositionToLineAndColumm(unsigned position) const {
    assert(position < this->contents.size());

    if (this->new_line_positions.empty() || position <= this->new_line_positions[0]) {
      // Position is in the first line.
      return {1, position + 1};
    }

    // Find the first new line position before the position.
    for (unsigned i = this->new_line_positions.size(); i-- > 0;) {
      if (this->new_line_positions[i] < position) {
        unsigned line = i + 2;
        unsigned column = position - this->new_line_positions[i];
        return {line, column};
      }
    }

    assert(false);
  }

 private:
  std::string filename;
  std::string contents;
  // Populated by the lexer as it reads the contents.
  std::vector<unsigned> new_line_positions;
};

#endif  // SRC_SOURCE_HPP_

#ifndef SRC_BINDINGS_HPP_
#define SRC_BINDINGS_HPP_

#include <string>
#include <unordered_map>
#include <vector>

class Declaration;
class FunctionDefinition;

class Bindings {
 private:
  class Scope {
   public:
    void Register(Declaration *decl);
    Declaration *Lookup(const std::string &name) const;
    std::vector<FunctionDefinition *> GetUndefinedFunctions() const;
   private:
    // We map names to their node of declaration.
    std::unordered_map<std::string, Declaration *> bindings;
  };

 public:
  void OpenScope();
  void CloseScope();
  void Register(Declaration *decl);
  Declaration *Lookup(const std::string &name) const;
  Declaration *LookupScope(const std::string &name) const;
  std::vector<FunctionDefinition *> GetUndefinedFunctionsInScope() const;
 private:
  // Stack of scopes.
  std::vector<Scope *> scopes;
};

#endif  // SRC_BINDINGS_HPP_

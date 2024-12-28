#include "bindings.hpp"
#include <cassert>
#include <utility>
#include "ast.hpp"

void Bindings::Scope::Register(Declaration *decl) {
  assert(this->bindings.count(decl->GetName()) == 0);
  this->bindings.insert({decl->GetName(), decl});
}

Declaration *Bindings::Scope::Lookup(const std::string &name) const {
  std::unordered_map<std::string, Declaration *>::const_iterator it = this->bindings.find(name);
  if (it != this->bindings.end())
    return it->second;
  return nullptr;
}

std::vector<FunctionDefinition *> Bindings::Scope::GetUndefinedFunctions() const {
  std::vector<FunctionDefinition *> func_defs;
  for (const auto &[key, value] : this->bindings) {
    FunctionDefinition *func_def = dynamic_cast<FunctionDefinition *>(value);
    if (func_def != nullptr && !func_def->IsDefined())
      func_defs.push_back(func_def);
  }
  return func_defs;
}

void Bindings::OpenScope() {
  this->scopes.push_back(new Scope());
}

void Bindings::CloseScope() {
  assert(!this->scopes.empty());
  delete this->scopes.back();
  this->scopes.pop_back();
}

void Bindings::Register(Declaration *decl) {
  assert(!this->scopes.empty());
  this->scopes.back()->Register(decl);
}

Declaration *Bindings::Lookup(const std::string &name) const {
  for (auto it = this->scopes.rbegin(), end = this->scopes.rend(); it < end; ++it) {
    Declaration *decl = (*it)->Lookup(name);
    if (decl != nullptr)
      return decl;
  }
  return nullptr;
}

Declaration *Bindings::LookupScope(const std::string &name) const {
  assert(!this->scopes.empty());
  return this->scopes.back()->Lookup(name);
}

std::vector<FunctionDefinition *> Bindings::GetUndefinedFunctionsInScope() const {
  assert(!this->scopes.empty());
  return this->scopes.back()->GetUndefinedFunctions();
}

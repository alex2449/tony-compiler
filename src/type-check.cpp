#include "type-check.hpp"
#include <string>
#include "ast-enums.hpp"
#include "ast.hpp"
#include "diagnostics.hpp"
#include "fmt/core.h"
#include "source-location.hpp"
#include "type.hpp"

std::vector<FunctionDefinition *> TypeCheck(FunctionDefinition *root,
                                            const std::vector<FunctionDefinition *> &builtins,
                                            Diagnostics &diagnostics) {
  Context ctx(diagnostics);

  ctx.bindings.OpenScope();
  for (FunctionDefinition *builtin : builtins)
    ctx.bindings.Register(builtin);
  ctx.bindings.OpenScope();

  if (root->GetReturnType() != nullptr) {
    std::string message = fmt::format("outermost function '{}' must not have a return type", root->GetName());
    ctx.diagnostics.ReportError(root->GetSourceLocation(), message);
  }

  if (!root->GetParameters().empty()) {
    std::string message = fmt::format("outermost function '{}' must not have any parameters", root->GetName());
    ctx.diagnostics.ReportError(root->GetSourceLocation(), message);
  }

  root->TypeCheck(ctx);

  ctx.bindings.CloseScope();
  ctx.bindings.CloseScope();

  return ctx.func_defs;
}

// Declarations.

void FunctionDefinition::TypeCheck(Context &ctx) {
  Declaration *decl = ctx.bindings.LookupScope(this->name);

  if (decl == nullptr) {
    ctx.bindings.Register(this);

    if (!this->IsForwardDecl())
      ctx.func_defs.push_back(this);
  } else {
    FunctionDefinition *func_def = dynamic_cast<FunctionDefinition *>(decl);

    if (func_def == nullptr) {
      ctx.diagnostics.ReportError(this->GetSourceLocation(),
                                  fmt::format("redeclaration of '{}' as a different kind of symbol", this->name));
      ctx.diagnostics.Inform(decl->GetSourceLocation(), "previous declaration is here");
    } else if (func_def->IsDefined() && !this->IsForwardDecl()) {
      ctx.diagnostics.ReportError(this->GetSourceLocation(),
                                  fmt::format("redefinition of function '{}'", this->name));
      ctx.diagnostics.Inform(func_def->GetDefinition()->GetSourceLocation(), "previous definition is here");
    } else if (!this->IsValidRedeclaration(func_def)) {
      ctx.diagnostics.ReportError(this->GetSourceLocation(),
                                  fmt::format("invalid redeclaration of function '{}'", this->name));
      ctx.diagnostics.Inform(func_def->GetSourceLocation(), "previous declaration is here");
    } else if (this->IsForwardDecl()) {
      // A forward declaration after a function has been defined or already forward declared is unnecessary.
      ctx.diagnostics.Warn(this->GetSourceLocation(),
                           fmt::format("redeclaration of '{}' is redundant", this->name));
      ctx.diagnostics.Inform(func_def->GetSourceLocation(), "previous declaration is here");
    } else if (!func_def->IsDefined() && !this->IsForwardDecl()) {
      // We found the definition for a forward declaration.
      func_def->SetDefinition(this);
      ctx.func_defs.push_back(this);
    }
  }

  if (!ctx.AtTopLevel())
    this->enclosing = ctx.CurrentFunction();

  ctx.open_functions.push_back(this);
  ctx.bindings.OpenScope();

  for (VariableDeclaration *parameter : this->parameters)
    parameter->TypeCheck(ctx);

  if (this->IsForwardDecl()) {
    ctx.open_functions.pop_back();
    ctx.bindings.CloseScope();
    return;
  }

  for (Declaration *decl : *this->locals)
    decl->TypeCheck(ctx);
  for (Statement *stmt : *this->body)
    stmt->TypeCheck(ctx);

  for (const FunctionDefinition *func_decl : ctx.bindings.GetUndefinedFunctionsInScope()) {
    std::string message = fmt::format("missing definition for function '{}'", func_decl->GetName());
    ctx.diagnostics.ReportError(func_decl->GetSourceLocation(), message);
  }

  ctx.open_functions.pop_back();
  ctx.bindings.CloseScope();
}

bool FunctionDefinition::IsValidRedeclaration(const FunctionDefinition *other) const {
  if ((this->return_type == nullptr) != (other->return_type == nullptr))
    return false;

  if (this->return_type != nullptr && !this->return_type->IsEqual(other->return_type))
    return false;

  if (this->parameters.size() != other->parameters.size())
    return false;

  for (unsigned i = 0, n = this->parameters.size(); i < n; ++i) {
    // We do not require that the parameters have the same name, only that their types
    // match and they are passed in the same way (both by value or both by reference).
    if (!this->parameters[i]->GetType()->IsEqual(other->parameters[i]->GetType()))
      return false;

    if (this->parameters[i]->IsByReference() != other->parameters[i]->IsByReference())
      return false;
  }

  return true;
}

void VariableDeclaration::TypeCheck(Context &ctx) {
  Declaration *decl = ctx.bindings.LookupScope(this->name);

  if (decl != nullptr) {
    ctx.diagnostics.ReportError(this->GetSourceLocation(),
                                fmt::format("redeclaration of '{}'", this->name));
    ctx.diagnostics.Inform(decl->GetSourceLocation(), "previous declaration is here");
    return;
  }

  ctx.bindings.Register(this);

  this->enclosing = ctx.CurrentFunction();
}

// Statements.

void SkipStatement::TypeCheck(Context &) {
  // Nothing to do.
}

void AssignmentStatement::TypeCheck(Context &ctx) {
  this->left->TypeCheck(ctx);
  this->right->TypeCheck(ctx);

  const Type *left_type = this->left->GetType();
  const Type *right_type = this->right->GetType();

  if (left_type == nullptr || right_type == nullptr)
    return;

  if (!this->left->IsLvalue()) {
    ctx.diagnostics.ReportError(this->assign_loc, {this->left->GetSourceRange()},
                                "expression can't be assigned to (must be an l-value)");
    return;
  }

  if (!right_type->IsEqual(left_type)) {
    std::string message = fmt::format("assigning to '{}' from incompatible type '{}'",
                                      left_type->ToString(), right_type->ToString());
    ctx.diagnostics.ReportError(this->assign_loc,
                                {this->left->GetSourceRange(), this->right->GetSourceRange()},
                                message);
    return;
  }

  const IndexAccessExpression *index_access = dynamic_cast<const IndexAccessExpression *>(this->left);
  if (index_access != nullptr && dynamic_cast<const StringExpression *>(index_access->GetBase()) != nullptr) {
    ctx.diagnostics.ReportError(this->assign_loc, {this->left->GetSourceRange()},
                                "assignment modifies contents of a string literal");
    return;
  }

  this->left->SetLvalueRequired();
}

void Call::TypeCheck(Context &ctx, const SourceLocation &location) {
  Declaration *decl = ctx.bindings.Lookup(this->name);

  if (decl == nullptr) {
    ctx.diagnostics.ReportError(location, fmt::format("use of undeclared function '{}'", this->name));
    return;
  }

  FunctionDefinition *func_def = dynamic_cast<FunctionDefinition *>(decl);

  if (func_def == nullptr) {
    ctx.diagnostics.ReportError(location, fmt::format("'{}' is not a function", decl->GetName()));
    ctx.diagnostics.Inform(decl->GetSourceLocation(), fmt::format("'{}' declared here", decl->GetName()));
    return;
  }

  bool arguments_contain_error = false;
  std::vector<SourceRange> arg_ranges;
  for (Expression *argument : this->arguments) {
    argument->TypeCheck(ctx);
    if (argument->GetType() == nullptr && !arguments_contain_error)
      arguments_contain_error = true;
    arg_ranges.push_back(argument->GetSourceRange());
  }

  if (arguments_contain_error)
    return;

  const std::vector<VariableDeclaration *> &parameters = func_def->GetParameters();

  if (this->arguments.size() != parameters.size()) {
    std::string message = fmt::format("invalid number of arguments to function call, expected {}, have {}",
                                      parameters.size(), this->arguments.size());
    ctx.diagnostics.ReportError(location, arg_ranges, message);
    if (!func_def->IsBuiltin())
      ctx.diagnostics.Inform(func_def->GetSourceLocation(), fmt::format("'{}' declared here", func_def->GetName()));
    return;
  }

  for (unsigned i = 0, n = this->arguments.size(); i < n; ++i) {
    const Type *type = this->arguments[i]->GetType();

    if (!type->IsEqual(parameters[i]->GetType())) {
      std::string message = fmt::format("passing '{}' to parameter of incompatible type '{}'",
                                        type->ToString(), parameters[i]->GetType()->ToString());
      ctx.diagnostics.ReportError(this->arguments[i]->GetSourceRange(), message);
      if (!func_def->IsBuiltin()) {
        std::string message = fmt::format("passing argument to parameter '{}' here", parameters[i]->GetName());
        ctx.diagnostics.Inform(parameters[i]->GetSourceLocation(), message);
      }
      return;
    }

    if (parameters[i]->IsByReference() && !this->arguments[i]->IsLvalue()) {
      ctx.diagnostics.ReportError(this->arguments[i]->GetSourceRange(),
                                  "expression can't be passed by reference (must be an l-value)");
      return;
    }

    if (parameters[i]->IsByReference())
      this->arguments[i]->SetLvalueRequired();
  }

  this->func_def = func_def;
}

void CallStatement::TypeCheck(Context &ctx) {
  this->call.TypeCheck(ctx, this->range.GetStartLoc());

  if (this->call.func_def == nullptr)
    return;

  if (this->call.func_def->GetReturnType() != nullptr)
    ctx.diagnostics.ReportError(this->range, "call to function with a return type can't be used as a statement");
}

void ExitStatement::TypeCheck(Context &ctx) {
  if (ctx.CurrentFunction()->GetReturnType() != nullptr)
    ctx.diagnostics.ReportError(this->range, "exit statement can't be used inside a function with a return type");
}

void ReturnStatement::TypeCheck(Context &ctx) {
  this->expr->TypeCheck(ctx);

  if (this->expr->GetType() == nullptr)
    return;

  const Type *return_type = ctx.CurrentFunction()->GetReturnType();

  if (return_type == nullptr) {
    ctx.diagnostics.ReportError(this->range, "function without return type must not return a value");
    return;
  }

  if (!this->expr->GetType()->IsEqual(return_type)) {
    std::string message = fmt::format("returning '{}' from a function with incompatible return type '{}'",
                                      this->expr->GetType()->ToString(), return_type->ToString());
    ctx.diagnostics.ReportError(this->return_loc, {this->expr->GetSourceRange()}, message);
  }
}

void IfStatement::TypeCheck(Context &ctx) {
  this->cond->TypeCheck(ctx);

  if (this->cond->GetType() != nullptr && !this->cond->GetType()->IsBool())
    ctx.diagnostics.ReportError(this->cond->GetSourceRange(),
                                fmt::format("expression must be of type 'bool' (have '{}')",
                                            this->cond->GetType()->ToString()));

  for (Statement *stmt : this->true_body)
    stmt->TypeCheck(ctx);

  for (const auto &[cond, body] : this->elsifs) {
    cond->TypeCheck(ctx);

    if (cond->GetType() != nullptr && !cond->GetType()->IsBool())
      ctx.diagnostics.ReportError(cond->GetSourceRange(),
                                  fmt::format("expression must be of type 'bool' (have '{}')",
                                              cond->GetType()->ToString()));

    for (Statement *stmt : body)
      stmt->TypeCheck(ctx);
  }

  if (this->else_body != nullptr)
    for (Statement *stmt : *this->else_body)
      stmt->TypeCheck(ctx);
}

void ForStatement::TypeCheck(Context &ctx) {
  for (Statement *stmt : this->init)
    stmt->TypeCheck(ctx);

  this->cond->TypeCheck(ctx);

  if (this->cond->GetType() != nullptr && !this->cond->GetType()->IsBool())
    ctx.diagnostics.ReportError(this->cond->GetSourceRange(),
                                fmt::format("expression must be of type 'bool' (have '{}')",
                                            this->cond->GetType()->ToString()));

  for (Statement *stmt : this->step)
    stmt->TypeCheck(ctx);
  for (Statement *stmt : this->body)
    stmt->TypeCheck(ctx);
}

// Expressions.

void IdentifierExpression::TypeCheck(Context &ctx) {
  Declaration *decl = ctx.bindings.Lookup(this->name);

  if (decl == nullptr) {
    ctx.diagnostics.ReportError(this->range.GetStartLoc(),
                                fmt::format("use of undeclared identifier '{}'", this->name));
    return;
  }

  VariableDeclaration *var_decl = dynamic_cast<VariableDeclaration *>(decl);

  if (var_decl == nullptr) {
    ctx.diagnostics.ReportError(this->range.GetStartLoc(), fmt::format("'{}' is not a variable", decl->GetName()));
    FunctionDefinition *func_def = static_cast<FunctionDefinition *>(decl);
    if (!func_def->IsBuiltin())
      ctx.diagnostics.Inform(func_def->GetSourceLocation(), fmt::format("'{}' declared here", func_def->GetName()));
    return;
  }

  // Check if the variable is being referenced from a nested function.
  if (ctx.CurrentFunction() != var_decl->GetEnclosing() && !var_decl->Escapes())
    var_decl->SetEscapes();

  this->type = var_decl->GetType()->Clone();
  this->is_lvalue = true;
  this->var_decl = var_decl;
}

void StringExpression::TypeCheck(Context &) {
  this->type = new ArrayType(new CharType());
}

void IndexAccessExpression::TypeCheck(Context &ctx) {
  this->base->TypeCheck(ctx);
  this->index->TypeCheck(ctx);

  const Type *base_type = this->base->GetType();
  const Type *index_type = this->index->GetType();

  if (base_type == nullptr || index_type == nullptr)
    return;

  if (!base_type->IsArray()) {
    ctx.diagnostics.ReportError(this->lbracket_loc, {this->base->GetSourceRange()},
                                fmt::format("indexed expression must be an array (have '{}')", base_type->ToString()));
    return;
  }

  if (!index_type->IsInt()) {
    ctx.diagnostics.ReportError(this->lbracket_loc, {this->index->GetSourceRange()},
                                fmt::format("index must be of type 'int' (have '{}')", index_type->ToString()));
    return;
  }

  this->type = base_type->GetArrayElement()->Clone();
  this->is_lvalue = true;
}

void CallExpression::TypeCheck(Context &ctx) {
  this->call.TypeCheck(ctx, this->range.GetStartLoc());

  if (this->call.func_def == nullptr)
    return;

  if (this->call.func_def->GetReturnType() == nullptr) {
    ctx.diagnostics.ReportError(this->range, "call to function without return type can't be used as an expression");
    return;
  }

  this->type = this->call.func_def->GetReturnType()->Clone();
}

void IntegerExpression::TypeCheck(Context &) {
  this->type = new IntType();
}

void CharacterExpression::TypeCheck(Context &) {
  this->type = new CharType();
}

void UnaryExpression::TypeCheck(Context &ctx) {
  this->expr->TypeCheck(ctx);

  const Type *type = this->expr->GetType();

  if (type == nullptr)
    return;

  switch (this->op) {
  case UnaryOperator::Plus:
  case UnaryOperator::Minus: {
    if (type->IsInt()) {
      this->type = new IntType();
      return;
    }
    break;
  }
  case UnaryOperator::Not: {
    if (type->IsBool()) {
      this->type = new BoolType();
      return;
    }
    break;
  }
  case UnaryOperator::NilQm: {
    if (type->IsList() || type->IsNil()) {
      this->type = new BoolType();
      return;
    }
    break;
  }
  case UnaryOperator::Head: {
    if (type->IsList()) {
      this->type = type->GetListElement()->Clone();
      return;
    }
    if (type->IsNil()) {
      ctx.diagnostics.ReportError(this->expr->GetSourceRange(), "untyped list not allowed here");
      return;
    }
    break;
  }
  case UnaryOperator::Tail: {
    if (type->IsList() || type->IsNil()) {
      this->type = type->Clone();
      return;
    }
    break;
  }
  default:
    assert(false && "unknown unary operator");
  }

  std::string message = fmt::format("invalid operand type '{}' to unary operator '{}'",
                                    type->ToString(), UnaryOperatorToString(this->op));
  ctx.diagnostics.ReportError(this->op_loc, {this->expr->GetSourceRange()}, message);
}

void BinaryExpression::TypeCheck(Context &ctx) {
  this->left->TypeCheck(ctx);
  this->right->TypeCheck(ctx);

  const Type *left_type = this->left->GetType();
  const Type *right_type = this->right->GetType();

  if (left_type == nullptr || right_type == nullptr)
    return;

  switch (this->op) {
  case BinaryOperator::Plus:
  case BinaryOperator::Minus:
  case BinaryOperator::Mult:
  case BinaryOperator::Div:
  case BinaryOperator::Mod: {
    if (left_type->IsInt() && right_type->IsInt()) {
      this->type = new IntType();
      return;
    }
    break;
  }
  case BinaryOperator::Equal:
  case BinaryOperator::NotEqual:
  case BinaryOperator::LessThan:
  case BinaryOperator::GreaterThan:
  case BinaryOperator::LessThanOrEqual:
  case BinaryOperator::GreaterThanOrEqual: {
    if (left_type->IsBasic() && right_type->IsBasic() && left_type->IsEqual(right_type)) {
      this->type = new BoolType();
      return;
    }
    break;
  }
  case BinaryOperator::And:
  case BinaryOperator::Or: {
    if (left_type->IsBool() && right_type->IsBool()) {
      this->type = new BoolType();
      return;
    }
    break;
  }
  case BinaryOperator::Cons: {
    if (!right_type->IsList() && !right_type->IsNil()) {
      std::string message = fmt::format("expression must be a list (have '{}')", right_type->ToString());
      ctx.diagnostics.ReportError(this->right->GetSourceRange(), message);
      return;
    }

    if (right_type->IsList() && !left_type->IsEqual(right_type->GetListElement())) {
      std::string message = fmt::format("prepending '{}' to list of incompatible element type '{}'",
                                        left_type->ToString(), right_type->GetListElement()->ToString());
      ctx.diagnostics.ReportError(this->op_loc,
                                  {this->left->GetSourceRange(), this->right->GetSourceRange()},
                                  message);
      return;
    }

    const Type *a = left_type;
    const Type *b = right_type;

    while (a->IsList() && b->IsList()) {
      a = a->GetListElement();
      b = b->GetListElement();
    }
    this->type = (b->IsList()
                  ? right_type->Clone()
                  : new ListType(left_type->Clone()));
    return;
  }
  default:
    assert(false && "unknown binary operator");
  }

  std::string message = fmt::format("invalid operand types to binary operator '{}' ('{}' and '{}')",
                                    BinaryOperatorToString(this->op), left_type->ToString(), right_type->ToString());
  ctx.diagnostics.ReportError(this->op_loc, {this->left->GetSourceRange(), this->right->GetSourceRange()}, message);
}

void BooleanExpression::TypeCheck(Context &) {
  this->type = new BoolType();
}

void NewExpression::TypeCheck(Context &ctx) {
  this->size->TypeCheck(ctx);

  if (this->size->GetType() == nullptr)
    return;

  if (!this->size->GetType()->IsInt()) {
    std::string message = fmt::format("expression must be of type 'int' (have '{}')",
                                      this->size->GetType()->ToString());
    ctx.diagnostics.ReportError(this->size->GetSourceRange(), message);
    return;
  }

  this->type = new ArrayType(this->element->Clone());
}

void NilExpression::TypeCheck(Context &) {
  this->type = new NilType();
}

void ParenthesizedExpression::TypeCheck(Context &ctx) {
  this->expr->TypeCheck(ctx);
  if (this->expr->GetType() != nullptr)
    this->type = this->expr->GetType()->Clone();
}

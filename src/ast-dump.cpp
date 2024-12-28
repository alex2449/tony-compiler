#include "ast-dump.hpp"
#include <optional>
#include <string>
#include "ast-enums.hpp"
#include "ast.hpp"
#include "source-location.hpp"
#include "type.hpp"
#include "util.hpp"

void ASTDumpContext::PrintIndent() {
  // Unicode characters taken from https://en.wikipedia.org/wiki/Box-drawing_characters
  ColorScope color(std::cout, this->with_color, BOLD_HIGH_INTENSITY_BLACK);

  for (unsigned i = 0, n = this->indents.size(); i < n; ++i) {
    unsigned &ttl = this->indents[i];

    if (ttl == 0) {
      std::cout << "  ";
    } else if (i < n - 1) {
      std::cout << "│ ";
    } else {
      std::cout << (ttl > 1 ? "├─" : "└─");
      --ttl;
    }
  }
}

// Declarations.

void Declaration::DumpList(const std::vector<Declaration *> &decls, ASTDumpContext &ctx) {
  if (decls.empty())
    return;

  ctx.PrintIndent();
  ctx.WithColor(BOLD_HIGH_INTENSITY_YELLOW) << "DeclarationList\n";

  ctx.Indent(decls.size());
  for (const Declaration *decl : decls)
    decl->Dump(ctx);
  ctx.Unindent();
}

void Declaration::DumpBase(ASTDumpContext &ctx, const std::string &kind) const {
  ctx.PrintIndent();
  ctx.WithColor(BOLD_YELLOW) << kind;
  ctx.WithColor(BOLD_HIGH_INTENSITY_BLACK) << " " << this->location->ToReadableString();
  ctx.WithColor(BOLD_CYAN) << " " << this->name;
}

void FunctionDefinition::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "FunctionDef");
  if (this->return_type != nullptr)
    ctx.WithColor(BOLD_PURPLE) << " " << this->return_type->ToString();
  std::cout << "\n";

  unsigned number_of_children = 0;
  if (!this->parameters.empty())
    ++number_of_children;
  if (this->locals != nullptr && !this->locals->empty())
    ++number_of_children;
  if (this->body != nullptr && !this->body->empty())
    ++number_of_children;

  if (number_of_children == 0)
    return;

  ctx.Indent(number_of_children);

  std::vector<Declaration *> to_decls{this->parameters.begin(), this->parameters.end()};
  Declaration::DumpList(to_decls, ctx);

  if (!this->IsForwardDecl()) {
    Declaration::DumpList(*this->locals, ctx);
    Statement::DumpList(*this->body, ctx);
  }

  ctx.Unindent();
}

void VariableDeclaration::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "VariableDecl");
  ctx.WithColor(BOLD_PURPLE) << " " << this->type->ToString();
  if (this->is_parameter && this->is_by_reference)
    ctx.WithColor(BOLD_CYAN) << " by_reference";
  std::cout << "\n";
}

// Statements.

void Statement::DumpList(const std::vector<Statement *> &stmts, ASTDumpContext &ctx) {
  ctx.PrintIndent();
  ctx.WithColor(BOLD_HIGH_INTENSITY_BLUE) << "StatementList\n";

  ctx.Indent(stmts.size());
  for (const Statement *stmt : stmts)
    stmt->Dump(ctx);
  ctx.Unindent();
}

void Statement::DumpBase(ASTDumpContext &ctx, const std::string &kind) const {
  ctx.PrintIndent();
  ctx.WithColor(BOLD_BLUE) << kind;
  ctx.WithColor(BOLD_HIGH_INTENSITY_BLACK) << " " << this->range.ToReadableString();
}

void SkipStatement::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "SkipStmt");
  std::cout << "\n";
}

void AssignmentStatement::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "AssignmentStmt");
  std::cout << "\n";

  ctx.Indent(2);
  this->left->Dump(ctx);
  this->right->Dump(ctx);
  ctx.Unindent();
}

void CallStatement::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "CallStmt");
  ctx.WithColor(BOLD_CYAN) << " " << this->call.name << "\n";

  if (this->call.arguments.empty())
    return;

  ctx.Indent(1);
  Expression::DumpList(this->call.arguments, ctx);
  ctx.Unindent();
}

void ExitStatement::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "ExitStmt");
  std::cout << "\n";
}

void ReturnStatement::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "ReturnStmt");
  std::cout << "\n";

  ctx.Indent(1);
  this->expr->Dump(ctx);
  ctx.Unindent();
}

void IfStatement::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "IfStmt");
  std::cout << "\n";

  unsigned number_of_children = 2 + (this->elsifs.size() * 2) + (this->else_body != nullptr ? 1 : 0);
  ctx.Indent(number_of_children);

  this->cond->Dump(ctx);
  Statement::DumpList(this->true_body, ctx);

  for (const auto &[cond, body] : this->elsifs) {
    cond->Dump(ctx);
    Statement::DumpList(body, ctx);
  }

  if (this->else_body != nullptr)
    Statement::DumpList(*this->else_body, ctx);

  ctx.Unindent();
}

void ForStatement::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "ForStmt");
  std::cout << "\n";

  ctx.Indent(4);
  Statement::DumpList(this->init, ctx);
  this->cond->Dump(ctx);
  Statement::DumpList(this->step, ctx);
  Statement::DumpList(this->body, ctx);
  ctx.Unindent();
}

// Expressions.

void Expression::DumpList(const std::vector<Expression *> &exprs, ASTDumpContext &ctx) {
  if (exprs.empty())
    return;

  ctx.PrintIndent();
  ctx.WithColor(BOLD_HIGH_INTENSITY_GREEN) << "ExpressionList\n";

  ctx.Indent(exprs.size());
  for (const Expression *expr : exprs)
    expr->Dump(ctx);
  ctx.Unindent();
}

void Expression::DumpBase(ASTDumpContext &ctx, const std::string &kind) const {
  ctx.PrintIndent();
  ctx.WithColor(BOLD_GREEN) << kind;
  ctx.WithColor(BOLD_HIGH_INTENSITY_BLACK) << " " << this->range.ToReadableString();
  if (this->type != nullptr)
    ctx.WithColor(BOLD_PURPLE) << " " << this->type->ToString();
}

void IdentifierExpression::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "IdentifierExpr");
  ctx.WithColor(BOLD_CYAN) << " " << this->name << "\n";
}

void StringExpression::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "StringExpr");
  ctx.WithColor(BOLD_CYAN) << " " << StringValueToLiteral(this->value) << "\n";
}

void IndexAccessExpression::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "IndexAccessExpr");
  std::cout << "\n";

  ctx.Indent(2);
  this->base->Dump(ctx);
  this->index->Dump(ctx);
  ctx.Unindent();
}

void CallExpression::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "CallExpr");
  ctx.WithColor(BOLD_CYAN) << " " << this->call.name << "\n";

  if (this->call.arguments.empty())
    return;

  ctx.Indent(1);
  Expression::DumpList(this->call.arguments, ctx);
  ctx.Unindent();
}

void IntegerExpression::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "IntegerExpr");
  ctx.WithColor(BOLD_CYAN) << " " << this->value << "\n";
}

void CharacterExpression::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "CharacterExpr");
  ctx.WithColor(BOLD_CYAN) << " " << CharValueToLiteral(this->value) << "\n";
}

void UnaryExpression::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "UnaryExpr");
  ctx.WithColor(BOLD_CYAN) << " " << UnaryOperatorToString(this->op) << "\n";

  ctx.Indent(1);
  this->expr->Dump(ctx);
  ctx.Unindent();
}

void BinaryExpression::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "BinaryExpr");
  ctx.WithColor(BOLD_CYAN) << " " << BinaryOperatorToString(this->op) << "\n";

  ctx.Indent(2);
  this->left->Dump(ctx);
  this->right->Dump(ctx);
  ctx.Unindent();
}

void BooleanExpression::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "BooleanExpr");
  ctx.WithColor(BOLD_CYAN) << " " << (this->value ? "true" : "false") << "\n";
}

void NewExpression::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "NewExpr");
  ctx.WithColor(BOLD_PURPLE) << " " << this->element->ToString() << "\n";

  ctx.Indent(1);
  this->size->Dump(ctx);
  ctx.Unindent();
}

void NilExpression::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "NilExpr");
  std::cout << "\n";
}

void ParenthesizedExpression::Dump(ASTDumpContext &ctx) const {
  this->DumpBase(ctx, "ParenthesizedExpr");
  std::cout << "\n";

  ctx.Indent(1);
  this->expr->Dump(ctx);
  ctx.Unindent();
}

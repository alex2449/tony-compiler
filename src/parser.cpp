#include "parser.hpp"
#include <cassert>
#include "ast-enums.hpp"
#include "ast.hpp"
#include "diagnostics.hpp"
#include "lexer.hpp"
#include "source-location.hpp"
#include "token.hpp"
#include "type.hpp"

Token *Parser::CurrentToken() const {
  return this->token;
}

Token *Parser::AdvanceToken() {
  delete this->token;
  if (this->unget_token != nullptr) {
    this->token = this->unget_token;
    this->unget_token = nullptr;
  } else {
    this->token = this->lexer.LexNextToken();
  }
  return this->token;
}

void Parser::UngetToken(Token *previous) {
  assert(this->unget_token == nullptr);
  this->unget_token = this->token;
  this->token = previous;
}

void Parser::Error(const std::string &message) const {
  // We currently abort on syntax errors.
  this->diagnostics.Fatal(this->CurrentToken()->GetSourceRange().GetStartLoc(), message);
}

FunctionDefinition *Parser::ParseProgram() {
  this->AdvanceToken();
  FunctionDefinition *func_def = this->ParseFunctionDef();
  if (!this->CurrentToken()->IsEndOfFile())
    this->Error("found unexpected token instead of end of file");
  return func_def;
}

// Declarations.

bool Parser::BeginsDeclaration() const {
  const Token *token = this->CurrentToken();
  return (this->BeginsType() ||
          token->IsKeyword(Keyword::Decl) ||
          token->IsKeyword(Keyword::Def));
}

std::vector<Declaration *> Parser::ParseDeclarationList() {
  std::vector<Declaration *> decls;

  while (this->BeginsDeclaration())
    this->ParseDeclaration(decls);

  return decls;
}

void Parser::ParseDeclaration(std::vector<Declaration *> &decls) {
  if (this->BeginsType()) {
    this->ParseVariableDecl(decls);
    return;
  }

  const Token *token = this->CurrentToken();

  if (token->IsKeyword(Keyword::Decl)) {
    decls.push_back(this->ParseFunctionDecl());
    return;
  }

  if (token->IsKeyword(Keyword::Def)) {
    decls.push_back(this->ParseFunctionDef());
    return;
  }

  assert(false);
}

FunctionDefinition *Parser::ParseFunctionDef() {
  const Token *token = this->CurrentToken();
  if (!token->IsKeyword(Keyword::Def))
    this->Error("expected keyword 'def'");

  this->AdvanceToken();

  const auto &[location, name, return_type, parameters] = this->ParseHeader();

  if (!this->CurrentToken()->IsSeparator(Separator::Colon))
    this->Error("expected ':'");

  this->AdvanceToken();
  std::vector<Declaration *> locals = this->ParseDeclarationList();
  std::vector<Statement *> body = this->ParseStatementList();
  if (!this->CurrentToken()->IsKeyword(Keyword::End))
    this->Error("expected keyword 'end'");

  this->AdvanceToken();

  return new FunctionDefinition(location, name, return_type, parameters,
                                new std::vector<Declaration *>(locals),
                                new std::vector<Statement *>(body), false);
}

FunctionDefinition *Parser::ParseFunctionDecl() {
  const Token *token = this->CurrentToken();
  assert(token->IsKeyword(Keyword::Decl));
  this->AdvanceToken();

  const auto &[location, name, return_type, parameters] = this->ParseHeader();

  return new FunctionDefinition(location, name, return_type, parameters, nullptr, nullptr, false);
}

Parser::Header Parser::ParseHeader() {
  Type *return_type = (this->BeginsType() ? this->ParseType(false) : nullptr);

  if (!this->CurrentToken()->IsIdentifier())
    this->Error("expected identifier");

  SourceLocation location = this->CurrentToken()->GetSourceRange().GetStartLoc();
  std::string name = this->CurrentToken()->GetIdentifierValue();

  if (!this->AdvanceToken()->IsSeparator(Separator::Lparen))
    this->Error("expected '('");

  this->AdvanceToken();
  std::vector<VariableDeclaration *> parameters = this->ParseParameterList();
  if (!this->CurrentToken()->IsSeparator(Separator::Rparen))
    this->Error("expected ')'");

  this->AdvanceToken();
  return {location, name, return_type, parameters};
}

std::vector<VariableDeclaration *> Parser::ParseParameterList() {
  std::vector<VariableDeclaration *> parameters;
  if (this->CurrentToken()->IsSeparator(Separator::Rparen))
    return parameters;

  while (true) {
    this->ParseParameter(parameters);

    if (!this->CurrentToken()->IsSeparator(Separator::Semicolon))
      return parameters;
    this->AdvanceToken();
  }
}

void Parser::ParseParameter(std::vector<VariableDeclaration *> &parameters) {
  bool is_by_reference = false;
  if (this->CurrentToken()->IsKeyword(Keyword::Ref)) {
    is_by_reference = true;
    this->AdvanceToken();
  }
  Type *type = this->ParseType(false);

  while (true) {
    const Token *token = this->CurrentToken();
    if (!token->IsIdentifier())
      this->Error("expected identifier");

    const std::string &name = token->GetIdentifierValue();
    VariableDeclaration *parameter = new VariableDeclaration(token->GetSourceRange().GetStartLoc(),
                                                             name, type->Clone(), true, is_by_reference);
    parameters.push_back(parameter);

    if (!this->AdvanceToken()->IsSeparator(Separator::Comma))
      return;
    this->AdvanceToken();
  }
  delete type;
}

void Parser::ParseVariableDecl(std::vector<Declaration *> &decls) {
  assert(this->BeginsType());
  Type *type = this->ParseType(false);

  while (true) {
    const Token *token = this->CurrentToken();
    if (!token->IsIdentifier())
      this->Error("expected identifier");

    const std::string &name = token->GetIdentifierValue();
    VariableDeclaration *var_decl = new VariableDeclaration(token->GetSourceRange().GetStartLoc(),
                                                            name, type->Clone(), false, false);
    decls.push_back(var_decl);

    if (!this->AdvanceToken()->IsSeparator(Separator::Comma))
      return;
    this->AdvanceToken();
  }
  delete type;
}

// Types.

bool Parser::BeginsType() const {
  const Token *token = this->CurrentToken();
  return (token->IsKeyword(Keyword::Int) ||
          token->IsKeyword(Keyword::Bool) ||
          token->IsKeyword(Keyword::Char) ||
          token->IsKeyword(Keyword::List));
}

Type *Parser::ParseType(bool is_new_expr_element) {
  const Token *token = this->CurrentToken();

  if (token->IsKeyword(Keyword::Int)) {
    Type *type = new IntType();
    this->AdvanceToken();
    return this->ParseTypeTail(type, is_new_expr_element);
  }

  if (token->IsKeyword(Keyword::Bool)) {
    Type *type = new BoolType();
    this->AdvanceToken();
    return this->ParseTypeTail(type, is_new_expr_element);
  }

  if (token->IsKeyword(Keyword::Char)) {
    Type *type = new CharType();
    this->AdvanceToken();
    return this->ParseTypeTail(type, is_new_expr_element);
  }

  if (token->IsKeyword(Keyword::List)) {
    if (!this->AdvanceToken()->IsSeparator(Separator::Lbracket))
      this->Error("expected '['");

    this->AdvanceToken();
    Type *element = this->ParseType(false);
    if (!this->CurrentToken()->IsSeparator(Separator::Rbracket))
      this->Error("expected ']'");

    this->AdvanceToken();
    return this->ParseTypeTail(new ListType(element), is_new_expr_element);
  }

  this->Error("expected type");
}

Type *Parser::ParseTypeTail(Type *type, bool is_new_expr_element) {
  while (this->CurrentToken()->IsSeparator(Separator::Lbracket)) {
    SourceRange hold_range = this->CurrentToken()->GetSourceRange();

    if (!this->AdvanceToken()->IsSeparator(Separator::Rbracket)) {
      if (is_new_expr_element) {
        Token *previous = Token::MakeSeparator(hold_range, Separator::Lbracket);
        this->UngetToken(previous);
        return type;
      }

      this->Error("expected ']'");
    }

    type = new ArrayType(type);
    this->AdvanceToken();
  }

  return type;
}

// Statements.

bool Parser::BeginsStatement() const {
  const Token *token = this->CurrentToken();
  return (token->IsIdentifier() ||
          token->IsString() ||
          token->IsKeyword(Keyword::Skip) ||
          token->IsKeyword(Keyword::Exit) ||
          token->IsKeyword(Keyword::Return) ||
          token->IsKeyword(Keyword::If) ||
          token->IsKeyword(Keyword::For));
}

std::vector<Statement *> Parser::ParseStatementList() {
  std::vector<Statement *> stmts;

  while (true) {
    stmts.push_back(this->ParseStatement());

    if (!this->BeginsStatement())
      return stmts;
  }
}

std::vector<Statement *> Parser::ParseSimpleStatementList() {
  std::vector<Statement *> stmts;

  while (true) {
    stmts.push_back(this->ParseSimpleStmt());

    if (!this->CurrentToken()->IsSeparator(Separator::Comma))
      return stmts;
    this->AdvanceToken();
  }
}

Statement *Parser::ParseStatement() {
  const Token *token = this->CurrentToken();

  if (token->IsKeyword(Keyword::Skip) || token->IsIdentifier() || token->IsString())
    return this->ParseSimpleStmt();

  if (token->IsKeyword(Keyword::Exit)) {
    Statement *stmt = new ExitStatement(token->GetSourceRange());
    this->AdvanceToken();
    return stmt;
  }

  if (token->IsKeyword(Keyword::Return)) {
    SourceLocation start_loc = token->GetSourceRange().GetStartLoc();
    this->AdvanceToken();
    Expression *expr = this->ParseExpression();
    SourceRange range(start_loc, expr->GetSourceRange().GetEndLoc());
    return new ReturnStatement(range, expr, start_loc);
  }

  if (token->IsKeyword(Keyword::If))
    return this->ParseIfStmt();

  if (token->IsKeyword(Keyword::For))
    return this->ParseForStmt();

  this->Error("expected statement");
}

Statement *Parser::ParseSimpleStmt() {
  const Token *token = this->CurrentToken();

  if (token->IsKeyword(Keyword::Skip)) {
    Statement *stmt = new SkipStatement(token->GetSourceRange());
    this->AdvanceToken();
    return stmt;
  }

  if (token->IsString()) {
    Expression *expr = new StringExpression(token->GetSourceRange(),
                                            token->GetStringValue());
    this->AdvanceToken();
    return this->ParseSimpleStatementTail(expr);
  }

  if (token->IsIdentifier()) {
    std::string name = token->GetIdentifierValue();

    SourceRange name_range = token->GetSourceRange();

    if (!this->AdvanceToken()->IsSeparator(Separator::Lparen)) {
      Expression *expr = new IdentifierExpression(name_range, name);
      return this->ParseSimpleStatementTail(expr);
    }

    this->AdvanceToken();
    std::vector<Expression *> arguments = this->ParseExpressionList();
    if (!this->CurrentToken()->IsSeparator(Separator::Rparen))
      this->Error("expected ')'");

    SourceLocation end_loc = this->CurrentToken()->GetSourceRange().GetEndLoc();
    SourceRange range(name_range.GetStartLoc(), end_loc);
    token = this->AdvanceToken();

    // We have parsed a call but we need to check whether it will
    // be part of an assignment or it is a call statement by itself.
    if (token->IsSeparator(Separator::Lbracket) ||
        token->IsSeparator(Separator::Assign)) {
      Expression *expr = new CallExpression(range, Call(name, arguments));
      return this->ParseSimpleStatementTail(expr);
    }

    return new CallStatement(range, Call(name, arguments));
  }

  this->Error("expected simple statement");
}

Statement *Parser::ParseSimpleStatementTail(Expression *base) {
  Expression *left = this->ParseAtomExpressionTail(base);
  if (!this->CurrentToken()->IsSeparator(Separator::Assign))
    this->Error("expected ':='");

  SourceLocation assign_loc = this->CurrentToken()->GetSourceRange().GetStartLoc();

  this->AdvanceToken();
  Expression *right = this->ParseExpression();

  SourceRange range(left->GetSourceRange().GetStartLoc(), right->GetSourceRange().GetEndLoc());
  return new AssignmentStatement(range, left, right, assign_loc);
}

Statement *Parser::ParseIfStmt() {
  const Token *token = this->CurrentToken();
  assert(token->IsKeyword(Keyword::If));

  SourceLocation start_loc = token->GetSourceRange().GetStartLoc();

  this->AdvanceToken();
  Expression *cond = this->ParseExpression();
  if (!this->CurrentToken()->IsSeparator(Separator::Colon))
    this->Error("expected ':'");

  this->AdvanceToken();
  std::vector<Statement *> true_body = this->ParseStatementList();

  std::vector<IfStatement::Elsif> elsifs;
  while (this->CurrentToken()->IsKeyword(Keyword::Elsif)) {
    this->AdvanceToken();

    Expression *cond = this->ParseExpression();
    if (!this->CurrentToken()->IsSeparator(Separator::Colon))
      this->Error("expected ':'");

    this->AdvanceToken();
    std::vector<Statement *> true_body = this->ParseStatementList();

    elsifs.emplace_back(cond, true_body);
  }

  std::vector<Statement *> *else_body = nullptr;
  if (this->CurrentToken()->IsKeyword(Keyword::Else)) {
    if (!this->AdvanceToken()->IsSeparator(Separator::Colon))
      this->Error("expected ':'");

    this->AdvanceToken();
    else_body = new std::vector<Statement *>(this->ParseStatementList());
  }

  if (!this->CurrentToken()->IsKeyword(Keyword::End))
    this->Error("expected keyword 'end'");

  SourceLocation end_loc = this->CurrentToken()->GetSourceRange().GetEndLoc();
  this->AdvanceToken();

  SourceRange range(start_loc, end_loc);
  return new IfStatement(range, cond, true_body, elsifs, else_body);
}

Statement *Parser::ParseForStmt() {
  const Token *token = this->CurrentToken();
  assert(token->IsKeyword(Keyword::For));

  SourceLocation start_loc = token->GetSourceRange().GetStartLoc();

  this->AdvanceToken();
  std::vector<Statement *> init = this->ParseSimpleStatementList();
  if (!this->CurrentToken()->IsSeparator(Separator::Semicolon))
    this->Error("expected ';'");

  this->AdvanceToken();
  Expression *cond = this->ParseExpression();
  if (!this->CurrentToken()->IsSeparator(Separator::Semicolon))
    this->Error("expected ';'");

  this->AdvanceToken();
  std::vector<Statement *> step = this->ParseSimpleStatementList();
  if (!this->CurrentToken()->IsSeparator(Separator::Colon))
    this->Error("expected ':'");

  this->AdvanceToken();
  std::vector<Statement *> body = this->ParseStatementList();
  if (!this->CurrentToken()->IsKeyword(Keyword::End))
    this->Error("expected keyword 'end'");

  SourceLocation end_loc = this->CurrentToken()->GetSourceRange().GetEndLoc();
  this->AdvanceToken();

  SourceRange range(start_loc, end_loc);
  return new ForStatement(range, init, cond, step, body);
}

// Expressions.

std::vector<Expression *> Parser::ParseExpressionList() {
  std::vector<Expression *> exprs;
  if (this->CurrentToken()->IsSeparator(Separator::Rparen))
    return exprs;

  while (true) {
    exprs.push_back(this->ParseExpression());

    if (!this->CurrentToken()->IsSeparator(Separator::Comma))
      return exprs;
    this->AdvanceToken();
  }
}

Expression *Parser::ParseExpression() {
  return this->ParseOrExpr();
}

Expression *Parser::ParseOrExpr() {
  Expression *left = this->ParseAndExpr();

  while (this->CurrentToken()->IsKeyword(Keyword::Or)) {
    SourceLocation op_loc = this->CurrentToken()->GetSourceRange().GetStartLoc();
    this->AdvanceToken();
    Expression *right = this->ParseAndExpr();
    SourceRange range(left->GetSourceRange().GetStartLoc(), right->GetSourceRange().GetEndLoc());
    left = new BinaryExpression(range, BinaryOperator::Or, left, right, op_loc);
  }
  return left;
}

Expression *Parser::ParseAndExpr() {
  Expression *left = this->ParseRelationalExpr();

  while (this->CurrentToken()->IsKeyword(Keyword::And)) {
    SourceLocation op_loc = this->CurrentToken()->GetSourceRange().GetStartLoc();
    this->AdvanceToken();
    Expression *right = this->ParseRelationalExpr();
    SourceRange range(left->GetSourceRange().GetStartLoc(), right->GetSourceRange().GetEndLoc());
    left = new BinaryExpression(range, BinaryOperator::And, left, right, op_loc);
  }
  return left;
}

Expression *Parser::ParseRelationalExpr() {
  Expression *left = this->ParseConsExpr();

  const Token *token = this->CurrentToken();
  if (token->IsOperator(Operator::Equal) ||
      token->IsOperator(Operator::NotEqual) ||
      token->IsOperator(Operator::GreaterThan) ||
      token->IsOperator(Operator::LessThan) ||
      token->IsOperator(Operator::GreaterThanOrEqual) ||
      token->IsOperator(Operator::LessThanOrEqual)) {
    SourceLocation op_loc = this->CurrentToken()->GetSourceRange().GetStartLoc();
    BinaryOperator op = token->ToBinaryOperator();
    this->AdvanceToken();
    Expression *right = this->ParseConsExpr();
    SourceRange range(left->GetSourceRange().GetStartLoc(), right->GetSourceRange().GetEndLoc());
    return new BinaryExpression(range, op, left, right, op_loc);
  }
  return left;
}

Expression *Parser::ParseConsExpr() {
  Expression *left = this->ParseAdditiveExpr();

  if (this->CurrentToken()->IsOperator(Operator::Cons)) {
    SourceLocation op_loc = this->CurrentToken()->GetSourceRange().GetStartLoc();
    this->AdvanceToken();
    Expression *right = this->ParseConsExpr();
    SourceRange range(left->GetSourceRange().GetStartLoc(), right->GetSourceRange().GetEndLoc());
    return new BinaryExpression(range, BinaryOperator::Cons, left, right, op_loc);
  }
  return left;
}

Expression *Parser::ParseAdditiveExpr() {
  Expression *left = this->ParseMultiplicativeExpr();

  const Token *token = this->CurrentToken();
  while (token->IsOperator(Operator::Plus) || token->IsOperator(Operator::Minus)) {
    SourceLocation op_loc = this->CurrentToken()->GetSourceRange().GetStartLoc();
    BinaryOperator op = token->ToBinaryOperator();
    this->AdvanceToken();
    Expression *right = this->ParseMultiplicativeExpr();
    SourceRange range(left->GetSourceRange().GetStartLoc(), right->GetSourceRange().GetEndLoc());
    left = new BinaryExpression(range, op, left, right, op_loc);
    token = this->CurrentToken();
  }
  return left;
}

Expression *Parser::ParseMultiplicativeExpr() {
  Expression *left = this->ParseUnaryExpr();

  const Token *token = this->CurrentToken();
  while (token->IsOperator(Operator::Mult) || token->IsOperator(Operator::Div) || token->IsKeyword(Keyword::Mod)) {
    SourceLocation op_loc = this->CurrentToken()->GetSourceRange().GetStartLoc();
    BinaryOperator op = token->ToBinaryOperator();
    this->AdvanceToken();
    Expression *right = this->ParseUnaryExpr();
    SourceRange range(left->GetSourceRange().GetStartLoc(), right->GetSourceRange().GetEndLoc());
    left = new BinaryExpression(range, op, left, right, op_loc);
    token = this->CurrentToken();
  }
  return left;
}

Expression *Parser::ParseUnaryExpr() {
  const Token *token = this->CurrentToken();

  if (token->IsOperator(Operator::Plus) || token->IsOperator(Operator::Minus)) {
    SourceLocation start_loc = token->GetSourceRange().GetStartLoc();
    UnaryOperator op = token->ToUnaryOperator();
    this->AdvanceToken();
    Expression *expr = this->ParseUnaryExpr();
    SourceRange range(start_loc, expr->GetSourceRange().GetEndLoc());
    return new UnaryExpression(range, op, expr, start_loc);
  }

  if (token->IsKeyword(Keyword::Not)) {
    SourceLocation start_loc = token->GetSourceRange().GetStartLoc();
    this->AdvanceToken();
    Expression *expr = this->ParseRelationalExpr();
    SourceRange range(start_loc, expr->GetSourceRange().GetEndLoc());
    return new UnaryExpression(range, UnaryOperator::Not, expr, start_loc);
  }

  return this->ParsePrimaryExpr();
}

Expression *Parser::ParsePrimaryExpr() {
  const Token *token = this->CurrentToken();

  if (token->IsIdentifier() || token->IsString())
    return this->ParseAtomExpr();

  if (token->IsInteger()) {
    Expression *expr = new IntegerExpression(token->GetSourceRange(),
                                             token->GetIntegerValue());
    this->AdvanceToken();
    return expr;
  }

  if (token->IsCharacter()) {
    Expression *expr = new CharacterExpression(token->GetSourceRange(),
                                               token->GetCharacterValue());
    this->AdvanceToken();
    return expr;
  }

  if (token->IsSeparator(Separator::Lparen)) {
    SourceLocation start_loc = token->GetSourceRange().GetStartLoc();

    this->AdvanceToken();
    Expression *expr = this->ParseExpression();
    if (!this->CurrentToken()->IsSeparator(Separator::Rparen))
      this->Error("expected ')'");

    SourceLocation end_loc = this->CurrentToken()->GetSourceRange().GetEndLoc();
    this->AdvanceToken();

    SourceRange range(start_loc, end_loc);
    return new ParenthesizedExpression(range, expr);
  }

  if (token->IsKeyword(Keyword::True)) {
    Expression *expr = new BooleanExpression(token->GetSourceRange(), true);
    this->AdvanceToken();
    return expr;
  }

  if (token->IsKeyword(Keyword::False)) {
    Expression *expr = new BooleanExpression(token->GetSourceRange(), false);
    this->AdvanceToken();
    return expr;
  }

  if (token->IsKeyword(Keyword::New)) {
    SourceLocation start_loc = token->GetSourceRange().GetStartLoc();

    this->AdvanceToken();
    Type *element = this->ParseType(true);
    if (!this->CurrentToken()->IsSeparator(Separator::Lbracket))
      this->Error("expected '['");

    this->AdvanceToken();
    Expression *size = this->ParseExpression();
    if (!this->CurrentToken()->IsSeparator(Separator::Rbracket))
      this->Error("expected ']'");

    SourceLocation end_loc = this->CurrentToken()->GetSourceRange().GetEndLoc();
    this->AdvanceToken();

    SourceRange range(start_loc, end_loc);
    return new NewExpression(range, element, size);
  }

  if (token->IsKeyword(Keyword::Nil)) {
    Expression *expr = new NilExpression(token->GetSourceRange());
    this->AdvanceToken();
    return expr;
  }

  if (token->IsKeyword(Keyword::NilQm) ||
      token->IsKeyword(Keyword::Head) ||
      token->IsKeyword(Keyword::Tail)) {
    SourceLocation start_loc = token->GetSourceRange().GetStartLoc();
    UnaryOperator op = token->ToUnaryOperator();
    if (!this->AdvanceToken()->IsSeparator(Separator::Lparen))
      this->Error("expected '('");

    this->AdvanceToken();
    Expression *expr = this->ParseExpression();
    if (!this->CurrentToken()->IsSeparator(Separator::Rparen))
      this->Error("expected ')'");

    SourceLocation end_loc = this->CurrentToken()->GetSourceRange().GetEndLoc();
    this->AdvanceToken();

    SourceRange range(start_loc, end_loc);
    return new UnaryExpression(range, op, expr, start_loc);
  }

  this->Error("expected expression");
}

Expression *Parser::ParseAtomExpr() {
  const Token *token = this->CurrentToken();
  assert(token->IsString() || token->IsIdentifier());

  if (token->IsString()) {
    Expression *expr = new StringExpression(token->GetSourceRange(),
                                            token->GetStringValue());
    this->AdvanceToken();
    return this->ParseAtomExpressionTail(expr);
  }

  std::string name = token->GetIdentifierValue();

  SourceRange name_range = token->GetSourceRange();

  if (!this->AdvanceToken()->IsSeparator(Separator::Lparen)) {
    Expression *expr = new IdentifierExpression(name_range, name);
    return this->ParseAtomExpressionTail(expr);
  }

  this->AdvanceToken();
  std::vector<Expression *> arguments = this->ParseExpressionList();
  if (!this->CurrentToken()->IsSeparator(Separator::Rparen))
    this->Error("expected ')'");

  SourceLocation end_loc = this->CurrentToken()->GetSourceRange().GetEndLoc();
  SourceRange range(name_range.GetStartLoc(), end_loc);
  this->AdvanceToken();

  Expression *expr = new CallExpression(range, Call(name, arguments));
  return this->ParseAtomExpressionTail(expr);
}

Expression *Parser::ParseAtomExpressionTail(Expression *base) {
  while (this->CurrentToken()->IsSeparator(Separator::Lbracket)) {
    SourceLocation lbracket_loc = this->CurrentToken()->GetSourceRange().GetStartLoc();

    this->AdvanceToken();
    Expression *index = this->ParseExpression();
    if (!this->CurrentToken()->IsSeparator(Separator::Rbracket))
      this->Error("expected ']'");

    SourceLocation end_loc = this->CurrentToken()->GetSourceRange().GetEndLoc();
    SourceRange range(base->GetSourceRange().GetStartLoc(), end_loc);
    base = new IndexAccessExpression(range, base, index, lbracket_loc);

    this->AdvanceToken();
  }
  return base;
}

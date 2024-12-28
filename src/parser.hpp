#ifndef SRC_PARSER_HPP_
#define SRC_PARSER_HPP_

#include <string>
#include <tuple>
#include <vector>

class Declaration;
class Diagnostics;
class Expression;
class FunctionDefinition;
class Lexer;
class SourceLocation;
class Statement;
class Token;
class Type;
class VariableDeclaration;

class Parser {
 public:
  Parser(Lexer &lexer, Diagnostics &diagnostics)
      : lexer(lexer), diagnostics(diagnostics) {}

  FunctionDefinition *ParseProgram();

 private:
  Token *CurrentToken() const;
  Token *AdvanceToken();
  void UngetToken(Token *previous);
  [[ noreturn ]] void Error(const std::string &message) const;

  // Declarations.
  bool BeginsDeclaration() const;
  std::vector<Declaration *> ParseDeclarationList();
  void ParseDeclaration(std::vector<Declaration *> &decls);
  FunctionDefinition *ParseFunctionDef();
  FunctionDefinition *ParseFunctionDecl();

  // Function header consists of the function's location, name, return type and parameters.
  using Header = std::tuple<SourceLocation, std::string, Type *, std::vector<VariableDeclaration *>>;
  Header ParseHeader();

  std::vector<VariableDeclaration *> ParseParameterList();
  void ParseParameter(std::vector<VariableDeclaration *> &parameters);
  void ParseVariableDecl(std::vector<Declaration *> &decls);

  // Types.
  bool BeginsType() const;
  Type *ParseType(bool is_new_expr_element);
  Type *ParseTypeTail(Type *type, bool is_new_expr_element);

  // Statements.
  bool BeginsStatement() const;
  std::vector<Statement *> ParseStatementList();
  std::vector<Statement *> ParseSimpleStatementList();
  Statement *ParseStatement();
  Statement *ParseSimpleStmt();
  Statement *ParseSimpleStatementTail(Expression *base);
  Statement *ParseIfStmt();
  Statement *ParseForStmt();

  // Expressions.
  std::vector<Expression *> ParseExpressionList();
  Expression *ParseExpression();
  Expression *ParseOrExpr();
  Expression *ParseAndExpr();
  Expression *ParseRelationalExpr();
  Expression *ParseConsExpr();
  Expression *ParseAdditiveExpr();
  Expression *ParseMultiplicativeExpr();
  Expression *ParseUnaryExpr();
  Expression *ParsePrimaryExpr();
  Expression *ParseAtomExpr();
  Expression *ParseAtomExpressionTail(Expression *base);

  Lexer &lexer;
  Diagnostics &diagnostics;
  // The current token.
  Token *token = nullptr;
  // A token that is "pushed back" to the input stream. If this is not null, we use
  // this as the next token instead of requesting one from the lexer. This is only
  // needed when parsing the type within a "new" expression.
  Token *unget_token = nullptr;
};

#endif  // SRC_PARSER_HPP_

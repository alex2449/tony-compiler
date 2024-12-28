#ifndef SRC_AST_HPP_
#define SRC_AST_HPP_

#include <cassert>
#include <optional>
#include <string>
#include <utility>
#include <vector>
#include "ast-enums.hpp"
#include "source-location.hpp"

class ASTDumpContext;
class Expression;
class FunctionDefinition;
class Statement;
class Type;
class VariableDeclaration;
namespace llvm { class AllocaInst; };
namespace llvm { class Function; };
namespace llvm { class StructType; };
namespace llvm { class Type; };
namespace llvm { class Value; };
struct Context;
struct TranslateContext;

class Declaration {
 public:
  Declaration(const std::optional<SourceLocation> &location, const std::string &name)
      : location(location), name(name) {}

  const SourceLocation &GetSourceLocation() const { return this->location.value(); }
  const std::string &GetName() const { return this->name; }

  const FunctionDefinition *GetEnclosing() const { return this->enclosing; }

  virtual void TypeCheck(Context &ctx) = 0;
  virtual void Dump(ASTDumpContext &ctx) const = 0;

  static void DumpList(const std::vector<Declaration *> &decls, ASTDumpContext &ctx);

  static std::vector<FunctionDefinition *> CreateBuiltins();

 protected:
  void DumpBase(ASTDumpContext &ctx, const std::string &kind) const;

  // Location in source code. The location of the declaration's name is used for this.
  // It is optional due to builtin functions (and their parameters) not having a valid location.
  std::optional<SourceLocation> location;
  // The declaration's name.
  std::string name;
  // The function definition in which this declaration exists. Will
  // be null for the outermost function. Set during type checking.
  const FunctionDefinition *enclosing = nullptr;
};

// A function definition or a function declaration (known as a forward declaration).

class FunctionDefinition : public Declaration {
 public:
  FunctionDefinition(const std::optional<SourceLocation> &location,
                     const std::string &name,
                     Type *return_type,
                     const std::vector<VariableDeclaration *> &parameters,
                     std::vector<Declaration *> *locals,
                     std::vector<Statement *> *body,
                     bool is_builtin)
      : Declaration(location, name), return_type(return_type), parameters(parameters),
        locals(locals), body(body), is_builtin(is_builtin) {}

  const Type *GetReturnType() const { return this->return_type; }
  const std::vector<VariableDeclaration *> &GetParameters() const { return this->parameters; }

  std::vector<VariableDeclaration *> GetLocalVariables() const;

  bool IsBuiltin() const { return this->is_builtin; }

  void SetDefinition(FunctionDefinition *definition) { this->definition = definition; }

  bool IsForwardDecl() const { return this->locals == nullptr; }
  bool IsValidRedeclaration(const FunctionDefinition *other) const;

  bool IsDefined() const { return !this->IsForwardDecl() || this->definition != nullptr; }

  const FunctionDefinition *GetDefinition() const {
    if (!this->IsForwardDecl())
      return this;
    assert(this->definition != nullptr);
    return this->definition;
  }

  llvm::Function *GetLLVMFunction() const {
    if (this->is_builtin || !this->IsForwardDecl())
      return this->llvm_function;
    return this->definition->llvm_function;
  }

  llvm::StructType *GetFrameType() const { return this->frame_type; }
  llvm::AllocaInst *GetFramePointer() const { return this->frame_pointer; }

  void TypeCheck(Context &ctx) override;
  void TranslateHeader(TranslateContext &ctx);
  void TranslateBody(TranslateContext &ctx);
  void Dump(ASTDumpContext &ctx) const override;

 private:
  void BuildFrame(TranslateContext &ctx);

  // The function's return type. May be null (for procedures).
  Type *return_type;
  // The function's parameters.
  std::vector<VariableDeclaration *> parameters;
  // The function's local declarations. Will be null if this is a forward declaration.
  std::vector<Declaration *> *locals;
  // The function's body. Will be null of this is a forward declaration.
  std::vector<Statement *> *body;
  // Whether this is a builtin function.
  bool is_builtin;
  // If this is a forward declaration, points to the node of definition. Set during type checking.
  const FunctionDefinition *definition = nullptr;
  // The LLVM function of this function. Set during code generation.
  llvm::Function *llvm_function;
  // Type of the struct that contains local variables and parameters that escape. Set during code generation.
  llvm::StructType *frame_type;
  // Pointer to the allocated struct. Set during code generation.
  llvm::AllocaInst *frame_pointer;
};

// A local variable or parameter declaration.

class VariableDeclaration : public Declaration {
 public:
  VariableDeclaration(const std::optional<SourceLocation> &location,
                      const std::string &name,
                      Type *type,
                      bool is_parameter,
                      bool is_by_reference)
      : Declaration(location, name), type(type), is_parameter(is_parameter), is_by_reference(is_by_reference) {
    assert(!is_by_reference || is_parameter);
  }

  const Type *GetType() const { return this->type; }
  bool IsParameter() const { return this->is_parameter; }
  bool IsByReference() const { assert(this->is_parameter); return this->is_by_reference; }

  bool Escapes() const { return this->escapes; }
  void SetEscapes() { this->escapes = true; }

  unsigned GetOffset() const { assert(this->escapes); return this->u.offset; }
  llvm::AllocaInst *GetAlloca() const { assert(!this->escapes); return this->u.alloca; }

  void TypeCheck(Context &ctx) override;
  // For variables that escape.
  void AllocateInStackFrame(TranslateContext &ctx, std::vector<llvm::Type *> &frame_body);
  // For variable that don't escape.
  void AllocateInStackFrame(TranslateContext &ctx);
  void Dump(ASTDumpContext &ctx) const override;

 private:
  // Type of the variable.
  Type *type;
  // Whether this is a parameter declaration.
  bool is_parameter;
  // If this is a parameter declaration, whether it is passed by reference.
  bool is_by_reference;
  // Whether this variable declaration is referenced in nested functions. Set during type checking.
  bool escapes = false;
  union {
    // For escaping variables, this is the offset within the enclosing
    // function's "frame" where this variable lives. Set during code generation.
    unsigned offset;
    // Variables that don't escape can be allocated on the stack
    // with an alloca instruction. Set during code generation.
    llvm::AllocaInst *alloca;
  } u;
};

class Statement {
 public:
  explicit Statement(const SourceRange &range) : range(range) {}

  const SourceRange &GetSourceRange() const { return this->range; }

  virtual void TypeCheck(Context &ctx) = 0;
  virtual void Translate(TranslateContext &ctx) const = 0;
  virtual void Dump(ASTDumpContext &ctx) const = 0;

  static void TranslateList(const std::vector<Statement *> &stmts, TranslateContext &ctx);
  static void DumpList(const std::vector<Statement *> &stmts, ASTDumpContext &ctx);

 protected:
  void DumpBase(ASTDumpContext &ctx, const std::string &kind) const;
  // Range in source code.
  SourceRange range;
};

class SkipStatement : public Statement {
 public:
  explicit SkipStatement(const SourceRange &range) : Statement(range) {}
  void TypeCheck(Context &ctx) override;
  void Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
};

class AssignmentStatement : public Statement {
 public:
  AssignmentStatement(const SourceRange &range,
                      Expression *left,
                      Expression *right,
                      const SourceLocation &assign_loc)
      : Statement(range), left(left), right(right), assign_loc(assign_loc) {}
  void TypeCheck(Context &ctx) override;
  void Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  Expression *left;
  Expression *right;
  // Location of ':='.
  SourceLocation assign_loc;
};

// A function call, used by CallStatement and CallExpression. Example: foo(42, true)

struct Call {
  Call(const std::string &name, const std::vector<Expression *> &arguments)
      : name(name), arguments(arguments) {}
  void TypeCheck(Context &ctx, const SourceLocation &location);
  llvm::Value *Translate(TranslateContext &ctx) const;
  std::string name;
  std::vector<Expression *> arguments;
  // The referenced function definition. Set during type checking.
  const FunctionDefinition *func_def = nullptr;
};

// A function call in the context of a statement.

class CallStatement : public Statement {
 public:
  CallStatement(const SourceRange &range, const Call &call) : Statement(range), call(call) {}
  void TypeCheck(Context &ctx) override;
  void Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  Call call;
};

class ExitStatement : public Statement {
 public:
  explicit ExitStatement(const SourceRange &range) : Statement(range) {}
  void TypeCheck(Context &ctx) override;
  void Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
};

class ReturnStatement : public Statement {
 public:
  ReturnStatement(const SourceRange &range, Expression *expr, const SourceLocation &return_loc)
      : Statement(range), expr(expr), return_loc(return_loc) {}
  void TypeCheck(Context &ctx) override;
  void Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  Expression *expr;
  // Location of the return keyword.
  SourceLocation return_loc;
};

class IfStatement : public Statement {
 public:
  // An elsif clause consists of an expression (the condition) and a list of statements (the body).
  using Elsif = std::pair<Expression *, std::vector<Statement *>>;
  IfStatement(const SourceRange &range,
              Expression *cond,
              const std::vector<Statement *> &true_body,
              const std::vector<Elsif> &elsifs,
              std::vector<Statement *> *else_body)
      : Statement(range), cond(cond), true_body(true_body), elsifs(elsifs), else_body(else_body) {}
  void TypeCheck(Context &ctx) override;
  void Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  // The condition.
  Expression *cond;
  // Statements to run if the condition is true.
  std::vector<Statement *> true_body;
  // List of elsif clauses (may be empty).
  std::vector<Elsif> elsifs;
  // Statements to run if no condition was true. May be null.
  std::vector<Statement *> *else_body;
};

class ForStatement : public Statement {
 public:
  ForStatement(const SourceRange &range,
               const std::vector<Statement *> &init,
               Expression *cond,
               const std::vector<Statement *> &step,
               const std::vector<Statement *> &body)
      : Statement(range), init(init), cond(cond), step(step), body(body) {}
  void TypeCheck(Context &ctx) override;
  void Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  // Statements to run on initialization.
  std::vector<Statement *> init;
  // The loop condition.
  Expression *cond;
  // Statements to run after each iteration.
  std::vector<Statement *> step;
  // Statements in the loop itself.
  std::vector<Statement *> body;
};

class Expression {
 public:
  explicit Expression(const SourceRange &range) : range(range) {}

  const SourceRange &GetSourceRange() const { return this->range; }
  const Type *GetType() const { return this->type; }
  bool IsLvalue() const { return this->is_lvalue; }

  void SetLvalueRequired() { assert(this->is_lvalue); this->lvalue_required = true; }

  virtual void TypeCheck(Context &ctx) = 0;
  virtual llvm::Value *Translate(TranslateContext &ctx) const = 0;
  virtual void Dump(ASTDumpContext &ctx) const = 0;

  static void DumpList(const std::vector<Expression *> &exprs, ASTDumpContext &ctx);

 protected:
  void DumpBase(ASTDumpContext &ctx, const std::string &kind) const;
  // Range in source code.
  SourceRange range;
  // The type of the expression. Set during type checking.
  // Will be null if no type can be given to the expression (on type error).
  Type *type = nullptr;
  // Whether the expression is an lvalue. Set during type checking.
  bool is_lvalue = false;
  // Whether the expression is used in a context where the lvalue is required.
  // This happens if the expression is the LHS of an assignment or the expression
  // is an argument for a parameter that is passed by reference. Set during type checking.
  bool lvalue_required = false;
};

// A single identifier. Can only be used to reference variables (local and parameters), not functions.

class IdentifierExpression : public Expression {
 public:
  IdentifierExpression(const SourceRange &range, const std::string &name)
      : Expression(range), name(name) {}
  void TypeCheck(Context &ctx) override;
  llvm::Value *Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  std::string name;
  // The referenced variable declaration. Set during type checking.
  VariableDeclaration *var_decl;
};

// A string literal. Examples: "abc", "Helloworld!\n"

class StringExpression : public Expression {
 public:
  StringExpression(const SourceRange &range, const std::string &value)
      : Expression(range), value(value) {}
  void TypeCheck(Context &ctx) override;
  llvm::Value *Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  std::string value;
};

// Access of an array element by index. Examples: x[0], "abc"[2], A[42][17]

class IndexAccessExpression : public Expression {
 public:
  IndexAccessExpression(const SourceRange &range,
                        Expression *base,
                        Expression *index,
                        const SourceLocation &lbracket_loc)
      : Expression(range), base(base), index(index), lbracket_loc(lbracket_loc) {}
  void TypeCheck(Context &ctx) override;
  llvm::Value *Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
  const Expression *GetBase() const { return this->base; }
 private:
  // The expression we are indexing.
  Expression *base;
  Expression *index;
  // Location of '['.
  SourceLocation lbracket_loc;
};

// A function call in the context of an expression.

class CallExpression : public Expression {
 public:
  CallExpression(const SourceRange &range, const Call &call) : Expression(range), call(call) {}
  void TypeCheck(Context &ctx) override;
  llvm::Value *Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  Call call;
};

// An integer literal. Examples: 0, 42, 00200

class IntegerExpression : public Expression {
 public:
  IntegerExpression(const SourceRange &range, int value)
      : Expression(range), value(value) {}
  void TypeCheck(Context &ctx) override;
  llvm::Value *Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  int value;
};

// A character literal. Examples: 'a', '1', '\n'

class CharacterExpression : public Expression {
 public:
  CharacterExpression(const SourceRange &range, char value)
      : Expression(range), value(value) {}
  void TypeCheck(Context &ctx) override;
  llvm::Value *Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  char value;
};

// A unary operator applied to an operand. Examples: +1, not true, head(q)

class UnaryExpression : public Expression {
 public:
  UnaryExpression(const SourceRange &range,
                  UnaryOperator op,
                  Expression *expr,
                  const SourceLocation &op_loc)
      : Expression(range), op(op), expr(expr), op_loc(op_loc) {}
  void TypeCheck(Context &ctx) override;
  llvm::Value *Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  UnaryOperator op;
  Expression *expr;
  // Location of operator.
  SourceLocation op_loc;
};

// A binary operator applied to two operands. Examples: 1 + 2, true or false, 42 # nil

class BinaryExpression : public Expression {
 public:
  BinaryExpression(const SourceRange &range,
                   BinaryOperator op,
                   Expression *left,
                   Expression *right,
                   const SourceLocation &op_loc)
      : Expression(range), op(op), left(left), right(right), op_loc(op_loc) {}
  void TypeCheck(Context &ctx) override;
  llvm::Value *Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  BinaryOperator op;
  Expression *left;
  Expression *right;
  // Location of operator.
  SourceLocation op_loc;
};

// A boolean literal. Can be "true" or "false".

class BooleanExpression : public Expression {
 public:
  BooleanExpression(const SourceRange &range, bool value) : Expression(range), value(value) {}
  void TypeCheck(Context &ctx) override;
  llvm::Value *Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  bool value;
};

// Dynamic array allocation. Example: new int[100]

class NewExpression : public Expression {
 public:
  NewExpression(const SourceRange &range, Type *element, Expression *size)
      : Expression(range), element(element), size(size) {}
  void TypeCheck(Context &ctx) override;
  llvm::Value *Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  Type *element;
  Expression *size;
};

// The empty list, "nil".

class NilExpression : public Expression {
 public:
  explicit NilExpression(const SourceRange &range) : Expression(range) {}
  void TypeCheck(Context &ctx) override;
  llvm::Value *Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
};

// An expression within parenthesis.

class ParenthesizedExpression : public Expression {
 public:
  ParenthesizedExpression(const SourceRange &range, Expression *expr)
      : Expression(range), expr(expr) {}
  void TypeCheck(Context &ctx) override;
  llvm::Value *Translate(TranslateContext &ctx) const override;
  void Dump(ASTDumpContext &ctx) const override;
 private:
  Expression *expr;
};

#endif  // SRC_AST_HPP_

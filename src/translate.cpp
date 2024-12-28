#include "translate.hpp"
#include <cassert>
#include <system_error>
#include <utility>
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "ast-enums.hpp"
#include "ast.hpp"
#include "diagnostics.hpp"
#include "type.hpp"

TranslateContext::TranslateContext(const char *filename)
    : module(std::make_unique<llvm::Module>(filename, this->llvm_ctx)),
      builder(this->llvm_ctx),
      list_node_type(llvm::StructType::create(this->llvm_ctx, "list_node")) {
  llvm::Type *malloc_return_type = llvm::Type::getInt8PtrTy(this->llvm_ctx);
  std::vector<llvm::Type *> malloc_parameters{llvm::Type::getInt32Ty(this->llvm_ctx)};
  llvm::FunctionType *malloc_type = llvm::FunctionType::get(malloc_return_type, malloc_parameters, false);
  this->malloc_ = llvm::Function::Create(malloc_type, llvm::Function::ExternalLinkage, "malloc", this->module.get());

  std::vector<llvm::Type *> body{
    llvm::Type::getInt64Ty(this->llvm_ctx),
    llvm::PointerType::get(this->list_node_type, 0)
  };
  this->list_node_type->setBody(body);
}

void Translate(const std::vector<FunctionDefinition *> &func_defs,
               const std::vector<FunctionDefinition *> &builtins,
               const char *filename,
               Diagnostics &diagnostics) {
  TranslateContext ctx(filename);

  // Create the main function, which calls the outermost Tony function.
  llvm::FunctionType *main_type = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx.llvm_ctx), false);
  llvm::Function *main = llvm::Function::Create(main_type, llvm::Function::ExternalLinkage, "main", ctx.module.get());

  for (FunctionDefinition *builtin : builtins)
    builtin->TranslateHeader(ctx);

  for (FunctionDefinition *func_def : func_defs)
    func_def->TranslateHeader(ctx);

  for (FunctionDefinition *func_def : func_defs) {
    ctx.current = func_def;
    func_def->TranslateBody(ctx);
    ctx.current = nullptr;
  }

  llvm::BasicBlock *entry = llvm::BasicBlock::Create(ctx.llvm_ctx, "", main);
  ctx.builder.SetInsertPoint(entry);
  ctx.builder.CreateCall(func_defs[0]->GetLLVMFunction());
  ctx.builder.CreateRet(llvm::ConstantInt::get(ctx.llvm_ctx, llvm::APInt(32, 0, true)));

  std::string str;
  llvm::raw_string_ostream output(str);
  if (llvm::verifyModule(*ctx.module, &output))
    diagnostics.Fatal("module verification failed:\n" + str);

  std::error_code ec;
  llvm::raw_fd_ostream out("ir.ll", ec);
  ctx.module->print(out, nullptr);
}

// Functions.

void FunctionDefinition::TranslateHeader(TranslateContext &ctx) {
  assert(!this->IsForwardDecl() || this->is_builtin);

  llvm::Type *return_type = (this->return_type != nullptr
                             ? this->return_type->ToLLVMType(ctx)
                             : llvm::Type::getVoidTy(ctx.llvm_ctx));

  std::vector<llvm::Type *> parameters;

  if (this->enclosing != nullptr) {
    // Functions that are statically enclosed by another function receive a pointer
    // to the enclosing function's frame as the 1st argument. This is true for all
    // functions except for the outermost function and all builtin functions.
    parameters.push_back(this->enclosing->GetFrameType()->getPointerTo());
  }

  for (const VariableDeclaration *parameter : this->parameters) {
    llvm::Type *type = parameter->GetType()->ToLLVMType(ctx);
    parameters.push_back(parameter->IsByReference() ? type->getPointerTo() : type);
  }

  llvm::FunctionType *type = llvm::FunctionType::get(return_type, parameters, false);
  llvm::GlobalValue::LinkageTypes linkage = (this->is_builtin
                                             ? llvm::Function::ExternalLinkage
                                             : llvm::Function::InternalLinkage);
  std::string name = this->is_builtin ? ("tony_" + this->name) : this->name;
  this->llvm_function = llvm::Function::Create(type, linkage, name, ctx.module.get());

  // Declare the function's frame type because nested functions will need it for their header.
  if (!this->is_builtin)
    this->frame_type = llvm::StructType::create(ctx.llvm_ctx, this->llvm_function->getName().str() + "_locals");
}

void FunctionDefinition::TranslateBody(TranslateContext &ctx) {
  llvm::BasicBlock *entry = llvm::BasicBlock::Create(ctx.llvm_ctx, "", this->llvm_function);
  ctx.builder.SetInsertPoint(entry);

  // Allocate parameters and local variables on the stack frame.
  this->BuildFrame(ctx);

  // Translate the function's body.
  Statement::TranslateList(*this->body, ctx);

  // Build a return if missing.
  if (ctx.builder.GetInsertBlock()->getTerminator() == nullptr) {
    if (this->return_type == nullptr) {
      ctx.builder.CreateRetVoid();
    } else {
      ctx.builder.CreateRet(llvm::Constant::getNullValue(this->llvm_function->getReturnType()));
    }
  }
}

void FunctionDefinition::BuildFrame(TranslateContext &ctx) {
  std::vector<VariableDeclaration *> locals = this->GetLocalVariables();
  std::vector<llvm::Type *> frame_body;

  if (this->enclosing != nullptr)
    frame_body.push_back(this->enclosing->GetFrameType()->getPointerTo());

  for (VariableDeclaration *parameter : this->parameters)
    if (parameter->Escapes())
      parameter->AllocateInStackFrame(ctx, frame_body);

  for (VariableDeclaration *local : locals)
    if (local->Escapes())
      local->AllocateInStackFrame(ctx, frame_body);

  this->frame_type->setBody(frame_body);
  // Allocate the frame on the stack.
  this->frame_pointer = ctx.builder.CreateAlloca(this->frame_type);

  for (VariableDeclaration *parameter : this->parameters)
    if (!parameter->Escapes())
      parameter->AllocateInStackFrame(ctx);

  for (VariableDeclaration *local : locals)
    if (!local->Escapes())
      local->AllocateInStackFrame(ctx);

  // Initialize parameters on stack from the received parameters.
  int i = 0;

  // Initialize static link (if it exists).
  if (this->enclosing != nullptr) {
    llvm::Value *static_link_ptr = ctx.builder.CreateStructGEP(this->frame_type, this->frame_pointer, 0);
    ctx.builder.CreateStore(this->llvm_function->getArg(i++), static_link_ptr);
  }

  // Initialize rest of parameters.
  for (const VariableDeclaration *parameter : this->parameters) {
    llvm::Value *ptr;

    if (!parameter->Escapes()) {
      ptr = parameter->GetAlloca();
    } else {
      ptr = ctx.builder.CreateStructGEP(this->frame_type, this->frame_pointer, parameter->GetOffset());
    }

    ctx.builder.CreateStore(this->llvm_function->getArg(i++), ptr);
  }
}

// Variables.

void VariableDeclaration::AllocateInStackFrame(TranslateContext &ctx, std::vector<llvm::Type *> &frame_body) {
  assert(this->escapes);
  llvm::Type *type = this->type->ToLLVMType(ctx);
  llvm::Type *alloc_type = (this->is_parameter && this->is_by_reference ? type->getPointerTo() : type);
  frame_body.push_back(alloc_type);
  this->u.offset = frame_body.size() - 1;
}

void VariableDeclaration::AllocateInStackFrame(TranslateContext &ctx) {
  assert(!this->escapes);
  llvm::Type *type = this->type->ToLLVMType(ctx);
  llvm::Type *alloc_type = (this->is_parameter && this->is_by_reference ? type->getPointerTo() : type);
  this->u.alloca = ctx.builder.CreateAlloca(alloc_type);
}

// Statements.

void Statement::TranslateList(const std::vector<Statement *> &stmts, TranslateContext &ctx) {
  for (const Statement *stmt : stmts) {
    stmt->Translate(ctx);
    if (ctx.builder.GetInsertBlock()->getTerminator() != nullptr)
      break;
  }
}

void SkipStatement::Translate(TranslateContext &) const {
  // Nothing to do.
}

void AssignmentStatement::Translate(TranslateContext &ctx) const {
  // All assignments in Tony copy a single value, including assignments to arrays and lists,
  // in which case we only copy a pointer, we never copy element-for-element.
  llvm::Value *right = this->right->Translate(ctx);
  llvm::Value *left = this->left->Translate(ctx);
  ctx.builder.CreateStore(right, left);
}

llvm::Value *Call::Translate(TranslateContext &ctx) const {
  std::vector<llvm::Value *> values;

  if (this->func_def->GetEnclosing() != nullptr) {
    llvm::Value *frame_ptr = ctx.current->GetFramePointer();

    const FunctionDefinition *current = ctx.current;
    while (current != this->func_def->GetEnclosing()) {
      frame_ptr = ctx.builder.CreateStructGEP(current->GetFrameType(), frame_ptr, 0);
      frame_ptr = ctx.builder.CreateLoad(current->GetEnclosing()->GetFrameType()->getPointerTo(), frame_ptr);
      current = current->GetEnclosing();
    }

    values.push_back(frame_ptr);
  }

  for (const Expression *argument : this->arguments)
    values.push_back(argument->Translate(ctx));

  return ctx.builder.CreateCall(this->func_def->GetLLVMFunction(), values);
}

void CallStatement::Translate(TranslateContext &ctx) const {
  this->call.Translate(ctx);
}

void ExitStatement::Translate(TranslateContext &ctx) const {
  ctx.builder.CreateRetVoid();
}

void ReturnStatement::Translate(TranslateContext &ctx) const {
  llvm::Value *value = this->expr->Translate(ctx);
  ctx.builder.CreateRet(value);
}

static bool IfStatementRequiresEndBlock(llvm::BasicBlock *true_body_exit,
                                        const std::vector<llvm::BasicBlock *> &elsif_body_exits,
                                        llvm::BasicBlock *else_body_exit) {
  if (else_body_exit == nullptr)
    return true;

  if (true_body_exit->getTerminator() == nullptr)
    return true;

  for (llvm::BasicBlock *block : elsif_body_exits)
    if (block->getTerminator() == nullptr)
      return true;

  return else_body_exit->getTerminator() == nullptr;
}

void IfStatement::Translate(TranslateContext &ctx) const {
  llvm::Function *function = ctx.builder.GetInsertBlock()->getParent();

  llvm::Value *cond = this->cond->Translate(ctx);
  llvm::BasicBlock *cond_exit = ctx.builder.GetInsertBlock();

  // Create all blocks without any jumps.
  llvm::BasicBlock *true_body_entry = llvm::BasicBlock::Create(ctx.llvm_ctx, "", function);
  ctx.builder.SetInsertPoint(true_body_entry);
  Statement::TranslateList(this->true_body, ctx);
  llvm::BasicBlock *true_body_exit = ctx.builder.GetInsertBlock();

  std::vector<llvm::Value *> elsif_conds(this->elsifs.size());
  std::vector<llvm::BasicBlock *> elsif_cond_entries(this->elsifs.size());
  std::vector<llvm::BasicBlock *> elsif_cond_exits(this->elsifs.size());
  std::vector<llvm::BasicBlock *> elsif_body_entries(this->elsifs.size());
  std::vector<llvm::BasicBlock *> elsif_body_exits(this->elsifs.size());

  for (unsigned i = 0, n = this->elsifs.size(); i < n; ++i) {
    const Elsif &elsif = this->elsifs[i];

    elsif_cond_entries[i] = llvm::BasicBlock::Create(ctx.llvm_ctx, "", function);
    ctx.builder.SetInsertPoint(elsif_cond_entries[i]);
    elsif_conds[i] = elsif.first->Translate(ctx);
    elsif_cond_exits[i] = ctx.builder.GetInsertBlock();

    elsif_body_entries[i] = llvm::BasicBlock::Create(ctx.llvm_ctx, "", function);
    ctx.builder.SetInsertPoint(elsif_body_entries[i]);
    Statement::TranslateList(elsif.second, ctx);
    elsif_body_exits[i] = ctx.builder.GetInsertBlock();
  }

  llvm::BasicBlock *else_body_entry = nullptr;
  llvm::BasicBlock *else_body_exit = nullptr;

  if (this->else_body != nullptr) {
    else_body_entry = llvm::BasicBlock::Create(ctx.llvm_ctx, "", function);
    ctx.builder.SetInsertPoint(else_body_entry);
    Statement::TranslateList(*this->else_body, ctx);
    else_body_exit = ctx.builder.GetInsertBlock();
  }

  llvm::BasicBlock *end_block = (IfStatementRequiresEndBlock(true_body_exit, elsif_body_exits, else_body_exit)
                                 ? llvm::BasicBlock::Create(ctx.llvm_ctx, "", function)
                                 : nullptr);

  // Build all necessary jumps.
  ctx.builder.SetInsertPoint(cond_exit);
  if (!this->elsifs.empty()) {
    ctx.builder.CreateCondBr(cond, true_body_entry, elsif_cond_entries[0]);
  } else if (this->else_body != nullptr) {
    ctx.builder.CreateCondBr(cond, true_body_entry, else_body_entry);
  } else {
    ctx.builder.CreateCondBr(cond, true_body_entry, end_block);
  }

  if (true_body_exit->getTerminator() == nullptr) {
    ctx.builder.SetInsertPoint(true_body_exit);
    ctx.builder.CreateBr(end_block);
  }

  for (unsigned i = 0, n = this->elsifs.size(); i < n; ++i) {
    ctx.builder.SetInsertPoint(elsif_cond_exits[i]);
    if (i < n - 1) {
      // Jump to the elsif's body entry if true, otherwise jump to next elsif condition.
      ctx.builder.CreateCondBr(elsif_conds[i], elsif_body_entries[i], elsif_cond_entries[i + 1]);
    } else if (this->else_body != nullptr) {
      ctx.builder.CreateCondBr(elsif_conds[i], elsif_body_entries[i], else_body_entry);
    } else {
      ctx.builder.CreateCondBr(elsif_conds[i], elsif_body_entries[i], end_block);
    }

    if (elsif_body_exits[i]->getTerminator() == nullptr) {
      ctx.builder.SetInsertPoint(elsif_body_exits[i]);
      ctx.builder.CreateBr(end_block);
    }
  }

  if (this->else_body != nullptr && else_body_exit->getTerminator() == nullptr) {
    ctx.builder.SetInsertPoint(else_body_exit);
    ctx.builder.CreateBr(end_block);
  }

  if (end_block != nullptr)
    ctx.builder.SetInsertPoint(end_block);
}

void ForStatement::Translate(TranslateContext &ctx) const {
  llvm::Function *function = ctx.builder.GetInsertBlock()->getParent();

  Statement::TranslateList(this->init, ctx);

  llvm::BasicBlock *cond = llvm::BasicBlock::Create(ctx.llvm_ctx, "", function);
  ctx.builder.CreateBr(cond);
  ctx.builder.SetInsertPoint(cond);
  llvm::Value *expr = this->cond->Translate(ctx);
  llvm::BasicBlock *cond_end = ctx.builder.GetInsertBlock();

  llvm::BasicBlock *body = llvm::BasicBlock::Create(ctx.llvm_ctx, "", function);
  ctx.builder.SetInsertPoint(body);
  Statement::TranslateList(this->body, ctx);

  if (ctx.builder.GetInsertBlock()->getTerminator() == nullptr) {
    Statement::TranslateList(this->step, ctx);
    ctx.builder.CreateBr(cond);
  }

  llvm::BasicBlock *cont = llvm::BasicBlock::Create(ctx.llvm_ctx, "", function);
  ctx.builder.SetInsertPoint(cond_end);
  ctx.builder.CreateCondBr(expr, body, cont);

  ctx.builder.SetInsertPoint(cont);
}

// Expressions.

llvm::Value *IdentifierExpression::Translate(TranslateContext &ctx) const {
  llvm::Value *ptr;

  if (!this->var_decl->Escapes()) {
    ptr = this->var_decl->GetAlloca();
  } else {
    llvm::Value *frame_ptr = ctx.current->GetFramePointer();

    // Climb up the stack frames until we reach the stack frame where the variable lives.
    const FunctionDefinition *current = ctx.current;
    while (current != this->var_decl->GetEnclosing()) {
      frame_ptr = ctx.builder.CreateStructGEP(current->GetFrameType(), frame_ptr, 0);
      frame_ptr = ctx.builder.CreateLoad(current->GetEnclosing()->GetFrameType()->getPointerTo(), frame_ptr);
      current = current->GetEnclosing();
    }

    // And get a pointer within the stack frame at the proper offset.
    ptr = ctx.builder.CreateStructGEP(current->GetFrameType(), frame_ptr, this->var_decl->GetOffset());
  }

  llvm::Type *type = this->type->ToLLVMType(ctx);

  // Parameters that are passed by reference need to be automatically dereferenced.
  if (this->var_decl->IsParameter() && this->var_decl->IsByReference())
    ptr = ctx.builder.CreateLoad(type->getPointerTo(), ptr);

  if (this->lvalue_required)
    return ptr;

  return ctx.builder.CreateLoad(type, ptr);
}

llvm::Value *StringExpression::Translate(TranslateContext &ctx) const {
  std::unordered_map<std::string, llvm::Constant *>::const_iterator it = ctx.strings.find(this->value);
  if (it != ctx.strings.end())
    return it->second;

  llvm::Constant *string_ptr = ctx.builder.CreateGlobalStringPtr(this->value, ".str");
  ctx.strings.insert({this->value, string_ptr});
  return string_ptr;
}

llvm::Value *IndexAccessExpression::Translate(TranslateContext &ctx) const {
  llvm::Value *base = this->base->Translate(ctx);
  llvm::Value *index = this->index->Translate(ctx);

  llvm::Type *type = this->type->ToLLVMType(ctx);

  llvm::Value *ptr = ctx.builder.CreateGEP(type, base, index);

  if (this->lvalue_required)
    return ptr;

  return ctx.builder.CreateLoad(type, ptr);
}

llvm::Value *CallExpression::Translate(TranslateContext &ctx) const {
  return this->call.Translate(ctx);
}

llvm::Value *IntegerExpression::Translate(TranslateContext &ctx) const {
  return llvm::ConstantInt::get(ctx.llvm_ctx, llvm::APInt(32, this->value, true));
}

llvm::Value *CharacterExpression::Translate(TranslateContext &ctx) const {
  return llvm::ConstantInt::get(ctx.llvm_ctx, llvm::APInt(8, this->value, true));
}

llvm::Value *UnaryExpression::Translate(TranslateContext &ctx) const {
  llvm::Value *value = this->expr->Translate(ctx);

  switch (this->op) {
  case UnaryOperator::Plus:
    return value;
  case UnaryOperator::Minus:
    return ctx.builder.CreateNeg(value);
  case UnaryOperator::Not:
    return ctx.builder.CreateNot(value);
  case UnaryOperator::NilQm:
    return ctx.builder.CreateIsNull(value);
  case UnaryOperator::Head: {
    llvm::Value *head_ptr = ctx.builder.CreateStructGEP(ctx.list_node_type, value, 0);
    llvm::Value *head = ctx.builder.CreateLoad(llvm::Type::getInt64Ty(ctx.llvm_ctx), head_ptr);

    // Type of head is i64, need to cast it to this expression's type.
    llvm::Type *target = this->type->ToLLVMType(ctx);

    if (this->type->IsArray() || this->type->IsList() || this->type->IsNil())
      return ctx.builder.CreateIntToPtr(head, target);

    return ctx.builder.CreateCast(llvm::Instruction::CastOps::Trunc, head, target);
  }
  case UnaryOperator::Tail: {
    llvm::Value *tail_ptr = ctx.builder.CreateStructGEP(ctx.list_node_type, value, 1);
    return ctx.builder.CreateLoad(ctx.list_node_type->getPointerTo(), tail_ptr);
  }
  default:
    assert(false && "unknown unary operator");
  }
}

llvm::Value *BinaryExpression::Translate(TranslateContext &ctx) const {
  if (this->op == BinaryOperator::And || this->op == BinaryOperator::Or) {
    llvm::Function *function = ctx.builder.GetInsertBlock()->getParent();

    llvm::Value *left = this->left->Translate(ctx);
    llvm::BasicBlock *left_block_end = ctx.builder.GetInsertBlock();

    llvm::BasicBlock *right_block = llvm::BasicBlock::Create(ctx.llvm_ctx, "", function);
    ctx.builder.SetInsertPoint(right_block);
    llvm::Value *right = this->right->Translate(ctx);
    llvm::BasicBlock *right_block_end = ctx.builder.GetInsertBlock();

    llvm::BasicBlock *merge_block = llvm::BasicBlock::Create(ctx.llvm_ctx, "", function);
    ctx.builder.SetInsertPoint(left_block_end);

    llvm::BasicBlock *true_block = (this->op == BinaryOperator::And) ? right_block : merge_block;
    llvm::BasicBlock *false_block = (this->op == BinaryOperator::And) ? merge_block : right_block;
    ctx.builder.CreateCondBr(left, true_block, false_block);

    ctx.builder.SetInsertPoint(right_block_end);
    ctx.builder.CreateBr(merge_block);

    ctx.builder.SetInsertPoint(merge_block);
    llvm::PHINode *phi = ctx.builder.CreatePHI(llvm::Type::getInt1Ty(ctx.llvm_ctx), 2);
    phi->addIncoming(left, left_block_end);
    phi->addIncoming(right, right_block_end);

    return phi;
  }

  llvm::Value *left = this->left->Translate(ctx);
  llvm::Value *right = this->right->Translate(ctx);

  switch (this->op) {
  case BinaryOperator::Plus:
    return ctx.builder.CreateAdd(left, right);
  case BinaryOperator::Minus:
    return ctx.builder.CreateSub(left, right);
  case BinaryOperator::Mult:
    return ctx.builder.CreateMul(left, right);
  case BinaryOperator::Div:
    return ctx.builder.CreateSDiv(left, right);
  case BinaryOperator::Mod:
    return ctx.builder.CreateSRem(left, right);
  case BinaryOperator::Equal:
    return ctx.builder.CreateICmpEQ(left, right);
  case BinaryOperator::NotEqual:
    return ctx.builder.CreateICmpNE(left, right);
  case BinaryOperator::LessThan:
    return ctx.builder.CreateICmpSLT(left, right);
  case BinaryOperator::GreaterThan:
    return ctx.builder.CreateICmpSGT(left, right);
  case BinaryOperator::LessThanOrEqual:
    return ctx.builder.CreateICmpSLE(left, right);
  case BinaryOperator::GreaterThanOrEqual:
    return ctx.builder.CreateICmpSGE(left, right);
  case BinaryOperator::Cons: {
    // Dynamically allocate a new list node in heap.
    llvm::Value *number_of_bytes = llvm::ConstantInt::get(ctx.llvm_ctx, llvm::APInt(32, 8 + 8, true));
    llvm::Value *node_ptr = ctx.builder.CreateCall(ctx.malloc_, {number_of_bytes});

    // Type of node_ptr is i8*, cast it list_node_type*.
    node_ptr = ctx.builder.CreatePointerCast(node_ptr, ctx.list_node_type->getPointerTo());

    // Cast the value of left to i64.
    llvm::Type *target = llvm::Type::getInt64Ty(ctx.llvm_ctx);
    if (this->left->GetType()->IsArray() || this->left->GetType()->IsList() || this->left->GetType()->IsNil()) {
      left = ctx.builder.CreatePtrToInt(left, target);
    } else {
      left = ctx.builder.CreateCast(llvm::Instruction::CastOps::SExt, left, target);
    }

    // Store the value of left in the first field (the head) of the new node.
    llvm::Value *head_ptr = ctx.builder.CreateStructGEP(ctx.list_node_type, node_ptr, 0);
    ctx.builder.CreateStore(left, head_ptr);

    // Store the value of right in the second field (the tail) of the new node.
    llvm::Value *tail_ptr = ctx.builder.CreateStructGEP(ctx.list_node_type, node_ptr, 1);
    ctx.builder.CreateStore(right, tail_ptr);

    return node_ptr;
  }
  default:
    assert(false && "unknown binary operator");
  }
}

llvm::Value *BooleanExpression::Translate(TranslateContext &ctx) const {
  return llvm::ConstantInt::get(ctx.llvm_ctx, llvm::APInt(1, this->value ? 1 : 0, true));
}

llvm::Value *NewExpression::Translate(TranslateContext &ctx) const {
  llvm::Value *array_size = this->size->Translate(ctx);
  llvm::Value *element_size = llvm::ConstantInt::get(ctx.llvm_ctx, llvm::APInt(32, this->element->GetSize(), true));

  llvm::Value *number_of_bytes = ctx.builder.CreateMul(array_size, element_size);
  llvm::Value *ptr = ctx.builder.CreateCall(ctx.malloc_, {number_of_bytes});

  // Call to malloc_ returns i8*, cast to this expression's type.
  return ctx.builder.CreatePointerCast(ptr, this->type->ToLLVMType(ctx));
}

llvm::Value *NilExpression::Translate(TranslateContext &ctx) const {
  return llvm::Constant::getNullValue(llvm::PointerType::get(ctx.list_node_type, 0));
}

llvm::Value *ParenthesizedExpression::Translate(TranslateContext &ctx) const {
  return this->expr->Translate(ctx);
}

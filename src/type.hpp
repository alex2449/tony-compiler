#ifndef SRC_TYPE_HPP_
#define SRC_TYPE_HPP_

#include <cassert>
#include <string>
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"
#include "translate.hpp"

class Type {
 protected:
  enum class Kind {
    Int,
    Char,
    Bool,
    Array,
    List,
    Nil
  };

  explicit Type(Kind kind) : kind(kind) {}

 public:
  virtual ~Type() = default;

  bool IsInt() const { return this->kind == Kind::Int; }
  bool IsChar() const { return this->kind == Kind::Char; }
  bool IsBool() const { return this->kind == Kind::Bool; }
  bool IsArray() const { return this->kind == Kind::Array; }
  bool IsList() const { return this->kind == Kind::List; }
  bool IsNil() const { return this->kind == Kind::Nil; }

  const Type *GetArrayElement() const;
  const Type *GetListElement() const;

  virtual Type *Clone() const = 0;
  virtual bool IsBasic() const = 0;
  virtual bool IsEqual(const Type *other) const = 0;
  virtual std::string ToString() const = 0;
  virtual llvm::Type *ToLLVMType(TranslateContext &ctx) const = 0;
  virtual int GetSize() const = 0;

 private:
  Kind kind;
};

class IntType : public Type {
 public:
  IntType() : Type(Kind::Int) {}

  Type *Clone() const override { return new IntType(); }
  bool IsBasic() const override { return true; }

  bool IsEqual(const Type *other) const override {
    return other->IsInt();
  }

  std::string ToString() const override { return "int"; }

  llvm::Type *ToLLVMType(TranslateContext &ctx) const override {
    return llvm::Type::getInt32Ty(ctx.llvm_ctx);
  }

  int GetSize() const override { return 4; }
};

class CharType : public Type {
 public:
  CharType() : Type(Kind::Char) {}

  Type *Clone() const override { return new CharType(); }
  bool IsBasic() const override { return true; }

  bool IsEqual(const Type *other) const override {
    return other->IsChar();
  }

  std::string ToString() const override { return "char"; }

  llvm::Type *ToLLVMType(TranslateContext &ctx) const override {
    return llvm::Type::getInt8Ty(ctx.llvm_ctx);
  }

  int GetSize() const override { return 1; }
};

class BoolType : public Type {
 public:
  BoolType() : Type(Kind::Bool) {}

  Type *Clone() const override { return new BoolType(); }
  bool IsBasic() const override { return true; }

  bool IsEqual(const Type *other) const override {
    return other->IsBool();
  }

  std::string ToString() const override { return "bool"; }

  llvm::Type *ToLLVMType(TranslateContext &ctx) const override {
    return llvm::Type::getInt1Ty(ctx.llvm_ctx);
  }

  int GetSize() const override { return 1; }
};

class ArrayType : public Type {
 public:
  explicit ArrayType(Type *element) : Type(Kind::Array), element(element) {}
  ~ArrayType() override { delete this->element; }

  Type *Clone() const override { return new ArrayType(this->element->Clone()); }
  bool IsBasic() const override { return false; }

  bool IsEqual(const Type *other) const override {
    return other->IsArray() && this->element->IsEqual(other->GetArrayElement());
  }

  std::string ToString() const override { return this->element->ToString() + "[]"; }

  llvm::Type *ToLLVMType(TranslateContext &ctx) const override {
    return llvm::PointerType::get(this->element->ToLLVMType(ctx), 0);
  }

  int GetSize() const override { return 8; }

  const Type *GetElement() const { return this->element; }

 private:
  Type *element;
};

class ListType : public Type {
 public:
  explicit ListType(Type *element) : Type(Kind::List), element(element) {}
  ~ListType() override { delete this->element; }

  Type *Clone() const override { return new ListType(this->element->Clone()); }
  bool IsBasic() const override { return false; }

  bool IsEqual(const Type *other) const override {
    return ((other->IsList() && this->element->IsEqual(other->GetListElement())) ||
             other->IsNil());
  }

  std::string ToString() const override { return "list[" + this->element->ToString() + "]"; }

  llvm::Type *ToLLVMType(TranslateContext &ctx) const override {
    return llvm::PointerType::get(ctx.list_node_type, 0);
  }

  int GetSize() const override { return 8; }

  const Type *GetElement() const { return this->element; }

 private:
  Type *element;
};

class NilType : public Type {
 public:
  NilType() : Type(Kind::Nil) {}

  Type *Clone() const override { return new NilType(); }
  bool IsBasic() const override { return false; }

  bool IsEqual(const Type *other) const override {
    return other->IsList() || other->IsNil();
  }

  std::string ToString() const override { return "nil"; }

  llvm::Type *ToLLVMType(TranslateContext &ctx) const override {
    return llvm::PointerType::get(ctx.list_node_type, 0);
  }

  int GetSize() const override { assert(false); }
};

#endif  // SRC_TYPE_HPP_

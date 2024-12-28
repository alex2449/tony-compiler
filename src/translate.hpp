#ifndef SRC_TRANSLATE_HPP_
#define SRC_TRANSLATE_HPP_

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

class Diagnostics;
class FunctionDefinition;
namespace llvm { class Constant; }
namespace llvm { class Function; }
namespace llvm { class StructType; }

struct TranslateContext {
  explicit TranslateContext(const char *filename);

  llvm::LLVMContext llvm_ctx;
  std::unique_ptr<llvm::Module> module;
  llvm::IRBuilder<> builder;

  // The current function definition that we are translating.
  FunctionDefinition *current = nullptr;
  std::unordered_map<std::string, llvm::Constant *> strings;
  llvm::Function *malloc_;
  llvm::StructType *list_node_type;
};

void Translate(const std::vector<FunctionDefinition *> &func_defs,
               const std::vector<FunctionDefinition *> &builtins,
               const char *filename,
               Diagnostics &diagnostics);

#endif  // SRC_TRANSLATE_HPP_

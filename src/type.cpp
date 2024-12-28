#include "type.hpp"

const Type *Type::GetArrayElement() const {
  assert(this->kind == Kind::Array);
  const ArrayType *to_array = static_cast<const ArrayType *>(this);
  return to_array->GetElement();
}

const Type *Type::GetListElement() const {
  assert(this->kind == Kind::List);
  const ListType *to_list = static_cast<const ListType *>(this);
  return to_list->GetElement();
}

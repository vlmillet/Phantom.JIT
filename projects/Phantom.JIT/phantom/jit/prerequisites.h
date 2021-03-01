// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

#pragma once

#include "plugin.h"

#if defined(LLVM_SUPPORT_COMPILER_H)
#    error "plugin.h must be included before LLVM"
#endif

#if defined(_MSC_VER)
#    pragma warning(push, 0)
#elif defined(__clang__)
#    pragma clang diagnostic push
#    pragma clang diagnostic ignored "-Wall"
#    pragma clang diagnostic ignored "-Wextra"
#endif
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"

namespace llvm
{
namespace codeview
{
struct GUID;
}
} // namespace llvm
#if defined(_MSC_VER)
#    pragma warning(pop)
#elif defined(__clang__)
#    pragma clang diagnostic pop
#endif

#include <phantom/lang/reflection.h>

namespace llvm
{
class Value;
class Function;
class Type;
class BasicBlock;
class LLVMContext;
class IRBuilderBase;
class DIBuilder;
class DISubprogram;
class JITPDBMemoryManager;
} // namespace llvm

namespace phantom
{
namespace jit
{
class CodeGenerator;
struct CodeGeneratorPrivate;

/// \brief  lang::CodeGenerator temporary data associated with a language element.
class PHANTOM_EXPORT_PHANTOM_JIT CodeGeneratorData
{
    friend struct CodeGeneratorPrivate;

public:
    void setCodeGenerator(CodeGeneratorPrivate* a_pCodeGenerator);

    CodeGeneratorData(lang::LanguageElement* a_pElement) : m_pElement(a_pElement), m_pCodeGenerator(nullptr) {}
    virtual ~CodeGeneratorData() {}

protected:
    lang::LanguageElement* m_pElement;
    virtual void           compilerAboutToBeChanged(CodeGeneratorPrivate* a_pCodeGenerator) { (void)a_pCodeGenerator; }
    virtual void           compilerChanged(CodeGeneratorPrivate* a_pCodeGenerator) { (void)a_pCodeGenerator; }
    CodeGeneratorPrivate*  m_pCodeGenerator;
};

class Subroutine;

struct PHANTOM_EXPORT_PHANTOM_JIT Label
{
    bool isUndefined() const;
    Label();
    llvm::BasicBlock*   block;
    phantom::StringView name;
};

struct PHANTOM_EXPORT_PHANTOM_JIT FunctionEntry
{
    FunctionEntry();
    void set(llvm::Function* f, llvm::DISubprogram* di);

    bool operator<(FunctionEntry const& a_Other) const { return function < a_Other.function; }

    void*                getAddress(CodeGeneratorPrivate* a_pCodeGen) const;
    mutable void*        cachedAddress = 0;
    llvm::Function*      function = nullptr;
    llvm::IRBuilderBase* builder;
    llvm::DIBuilder*     di_builder;
};

struct PHANTOM_EXPORT_PHANTOM_JIT Value
{
    Value();
    Value(llvm::Value* a_Value, lang::Type* a_pType);

    bool isNull() const { return value == 0; }
    bool isNullPtr() const { return type == nullptr; }

    void setType(lang::Type* a_type);

    bool operator==(const Value& ptr) const { return value == ptr.value; }
    bool operator!=(const Value& ptr) const { return value != ptr.value; }

    llvm::Value* value;
    lang::Type*  type;
};

PHANTOM_EXPORT_PHANTOM_JIT phantom::String sessionFolder();

typedef phantom::schar        schar;
typedef phantom::uchar        uchar;
typedef phantom::ushort       ushort;
typedef phantom::uint         uint;
typedef phantom::ulong        ulong;
typedef phantom::ulonglong    ulonglong;
typedef phantom::longlong     longlong;
typedef phantom::longdouble   longdouble;
typedef phantom::byte         byte;
typedef phantom::String       String;
typedef phantom::StringView   StringView;
typedef phantom::StringBuffer StringBuffer;
using phantom::SmallVector;
using phantom::SmallMap;
using phantom::SmallSet;
class Context;

inline String getDebugName(lang::Symbol* a_pSymbol)
{
    if (a_pSymbol->asSubroutine())
        return a_pSymbol->getName();
    return a_pSymbol->getDecoratedName();
}

inline llvm::StringRef toStringRef(phantom::StringView a_View)
{
    return llvm::StringRef(a_View.begin(), a_View.size());
}

template<class T, size_t S>
inline llvm::ArrayRef<T> toArrayRef(SmallVector<T, S> const& vec)
{
    return llvm::ArrayRef<T>(vec.begin(), vec.end());
}

phantom::String GUIDToText(llvm::codeview::GUID const& guid);

enum class ArgumentPassingMode
{
    Default,
    ByPointer,
    ByInt,
};

ArgumentPassingMode GetArgumentPassingMode(lang::Type* a_pType);

enum class CppManglerFlag
{
    Thunk = 0x1,
    Virtual = 0x2,
    Apply = 0x4,
    Final = 0x8,
};
PHANTOM_DECLARE_FLAGS(CppManglerFlags, CppManglerFlag);

} // namespace jit
} // namespace phantom
#if PHANTOM_OPERATING_SYSTEM == PHANTOM_OPERATING_SYSTEM_ORBIS && !defined(LLVM_ON_UNIX)
#    define LLVM_ON_UNIX
#endif

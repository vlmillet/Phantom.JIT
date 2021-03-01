#pragma once

#include "DebugContext.h"
#include "prerequisites.h"

#include "llvm/IR/CallingConv.h"

namespace llvm
{
class FunctionType;
class Type;
class DICompileUnit;
class LLVMContext;
class Module;
} // namespace llvm

namespace phantom
{
namespace lang
{
class Type;
class FunctionType;
} // namespace lang
} // namespace phantom

namespace phantom
{
namespace jit
{
struct CodeGeneratorPrivate;
class Context
{
    friend class DebugContext;

public:
    static llvm::LLVMContext* Main();
    Context(CodeGeneratorPrivate* a_pCodeGeneratorPrivate, llvm::LLVMContext& context, llvm::Module* a_pLLVMModule);
    ~Context();
    lang::Type*           fromJitType(llvm::Type* type);
    llvm::Type*           toJitType(lang::Type* a_pType);
    llvm::Type*           toJitArgumentType(lang::Type* a_pType);
    llvm::CallingConv::ID toJitABI(lang::ABI a_eABI);
    llvm::Type*           getJitIntType(size_t a_Size) const;

    llvm::FunctionType* toJitFunctionCallType(lang::FunctionType* a_pSignature, lang::ClassType* a_pThisType);

    llvm::FunctionType* toJitFunctionType(lang::FunctionType* a_pSignature, lang::ClassType* a_pThisType = nullptr);

    llvm::LLVMContext&    m_LLVMContext;
    CodeGeneratorPrivate* m_pCodeGeneratorPrivate = nullptr;
    DebugContext*         m_pDebugContext = nullptr;
    llvm::DICompileUnit*  m_pDICompilationUnit = nullptr;

protected:
    llvm::Type* _toJitStruct(lang::ClassType* a_pClassType);
    llvm::Type* _toJitBool(size_t a_uiSize);
    llvm::Type* _toJitType(lang::Type* a_pType);

    SmallMap<lang::Type*, llvm::Type*, 2048> m_toJitTypeMap;
    SmallMap<llvm::Type*, lang::Type*, 2048> m_fromJitTypeMap;

    std::aligned_storage_t<sizeof(DebugContext), alignof(DebugContext)> m_DebugContextMem;
};

} // namespace jit
} // namespace phantom

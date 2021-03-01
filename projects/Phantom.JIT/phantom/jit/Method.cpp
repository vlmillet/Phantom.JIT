// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

/* ******************* Includes ****************** */
#include "Method.h"

#include "CodeGenerator.h"
#include "CodeGeneratorPrivate.h"

#include <phantom/lang/VirtualMethodTable.h>
#pragma warning(push, 0)
#include <llvm/IR/IRBuilder.h>
#pragma warning(pop)
/* *********************************************** */
#define o_ir_vbuilder ((llvm::IRBuilder<>*)m_jit_virtual_indirection_function.builder)

#if PHANTOM_SIZE_OF_VOID_P == 8
#    define getSizeT(...) getInt64(__VA_ARGS__)
#    define getSizeTTy(...) llvm::Type::getInt64Ty(getContext()->m_LLVMContext)
#else
#    define getSizeT(...) getInt32(__VA_ARGS__)
#    define getSizeTTy(...) llvm::Type::getInt32Ty(getContext()->m_LLVMContext)
#endif

namespace phantom
{
namespace jit
{
using namespace phantom::lang;

Method::Method(lang::Method* a_pMethod) : Subroutine(a_pMethod)
{
    PHANTOM_ASSERT(!a_pMethod->testModifiers(Modifier::Deleted));
}

Method::~Method()
{
    //     if(m_pCodeGenerator && m_jit_virtual_indirection_function.function)
    //     {
    //         m_pCodeGenerator->unregisterJitFunction(m_jit_virtual_indirection_function);
    //     }
    if (m_jit_virtual_indirection_function.builder)
        delete (llvm::IRBuilder<>*)m_jit_virtual_indirection_function.builder;
}

void Method::compilerAboutToBeChanged(CodeGeneratorPrivate* a_pCodeGenerator)
{
    //     if (m_pCodeGenerator && m_jit_virtual_indirection_function.function)
    //     {
    //         m_pCodeGenerator->unregisterJitFunction(m_jit_virtual_indirection_function);
    //     }
    Subroutine::compilerAboutToBeChanged(a_pCodeGenerator);
}

void Method::compilerChanged(CodeGeneratorPrivate* a_pCodeGenerator)
{
    if (a_pCodeGenerator)
    {
        lang::Method* pMethod = getMethod();
        auto pJitFunctionType = toJitFunctionCallType(getSignature()->getFunctionType(), pMethod->getOwnerClassType());
        auto mangledName = getMangledName();
        llvm::Function* pFunc = nullptr;
        if (!(pMethod->isPureVirtual()))
        {
            CodePosition pos = getSubroutine()->getCodePosition();
            if (auto dbg = getDebugContext())
                m_debugInfo = dbg->toDISubprogram(pMethod);
        }
        if (getMethod()->isVirtual())
        {
            if (getMethod()->getBlock())
            {
                pFunc = llvm::cast<llvm::Function>(
                m_pCodeGenerator->m_Module->getOrInsertFunction(mangledName.c_str(), pJitFunctionType).getCallee());
                pFunc->setCallingConv(toJitABI(getSubroutine()->getABI()));
                m_jit_function.set(pFunc, m_debugInfo);
                _createAllocaHeader();
            }
            compileVTableIndirectionFunction();
            m_jit_call_function = m_jit_virtual_indirection_function;
        }
        else
        {
            pFunc = llvm::cast<llvm::Function>(
            m_pCodeGenerator->m_Module->getOrInsertFunction(mangledName.c_str(), pJitFunctionType).getCallee());
            pFunc->setCallingConv(toJitABI(getSubroutine()->getABI()));
            pFunc->setSubprogram(m_debugInfo);
            m_jit_function.set(pFunc, m_debugInfo);
            _createAllocaHeader();
            m_jit_call_function = m_jit_function;
        }
        if (pFunc) // can be abstract and then null
            ApplyFunctionAttributes(pFunc, true);

        if (m_jit_function.function)
        {
            m_pCodeGenerator->registerJitFunction(m_jit_function, this);
        }
        createApplyFunction();
    }
}

phantom::MethodClosure Method::getGenericMethodPointer() const
{
    phantom::MethodClosure ptr;
    if (getSubroutine()->isVirtual())
    {
        ptr.setClosurePointer(m_jit_virtual_indirection_function.getAddress(m_pCodeGenerator));
    }
    else
    {
        ptr.setClosurePointer(m_jit_function.getAddress(m_pCodeGenerator));
    }
    return ptr;
}

void Method::compileVTableIndirectionFunction()
{
    auto pFuncJitType =
    toJitFunctionCallType(getSubroutine()->getSignature()->getFunctionType(), getMethod()->getOwnerClassType());
    auto            uniqueName = getMangledName(CppManglerFlag::Virtual);
    llvm::Function* pFunc = llvm::cast<llvm::Function>(
    m_pCodeGenerator->m_Module->getOrInsertFunction(uniqueName.c_str(), pFuncJitType).getCallee());
    pFunc->setCallingConv(toJitABI(ABI::MethodCall));
    m_jit_virtual_indirection_function.set(pFunc, nullptr);

    ApplyFunctionAttributes(pFunc);
    // auto Attrs = pFunc->getAttributes();
    // Attrs = Attrs.addAttribute(pFunc->getContext(), llvm::AttributeList::FunctionIndex, llvm::Attribute::Naked);
    // pFunc->setAttributes(Attrs);
    PHANTOM_ASSERT(m_jit_virtual_indirection_function.function);
    // m_pCodeGenerator->registerJitFunction(m_jit_virtual_indirection_function, this);

    // find first method vtable index by looking vtables starting from the first base to the last
    lang::Method* m = getMethod();
    llvm::Value*  this_pointer = &*m_jit_virtual_indirection_function.function->arg_begin();
    size_t        vidx = -1;
    auto&         vtables = getMethod()->getOwnerClass()->getVirtualMethodTables();
    size_t        vtableOffset = 0;
    for (size_t i = 0; i < vtables.size(); ++i)
    {
        if ((vidx = vtables[i]->getMethodIndex(m)) != -1)
        {
            vtableOffset = vtables[i]->getOffset();
            break;
        }
    }
    PHANTOM_ASSERT(vidx != -1);
    llvm::Value* vtable_index = o_ir_vbuilder->getSizeT(vidx);
    llvm::Value* vtable_array;
    if (vtableOffset != 0)
    {
        // this = size_t(this) + vtable_offset
        this_pointer = o_ir_vbuilder->CreateIntToPtr(
        o_ir_vbuilder->CreateAdd(o_ir_vbuilder->CreatePtrToInt(this_pointer, getSizeTTy()),
                                 o_ir_vbuilder->getSizeT(vtableOffset)),
        this_pointer->getType());
    }
    // func** vtable_array = *reinterpret_cast<func***>(this)
    vtable_array = o_ir_vbuilder->CreateLoad(o_ir_vbuilder->CreatePointerCast(
    this_pointer, pFunc->getFunctionType()->getPointerTo()->getPointerTo()->getPointerTo()));

    llvm::Value* vtable_pointer = o_ir_vbuilder->CreateLoad(o_ir_vbuilder->CreateGEP(vtable_array, vtable_index));

    // The indirection has the same signature as the indirected one
    size_t argCount = getSignature()->getParameterCount() + 1 + getSignature()->isRVOCandidate(); // parameters+this
    std::vector<llvm::Value*> args;
    args.push_back(this_pointer); // we may have changed 'this', so we push ours
    size_t i = 1;                 // start after 'this'
    for (; i < argCount; ++i)
    {
        auto it = m_jit_virtual_indirection_function.function->arg_begin();
        std::advance(it, i);
        args.push_back(&*it);
    }
    auto call = o_ir_vbuilder->CreateCall(pFuncJitType, vtable_pointer, args);
    call->setCallingConv(toJitABI(ABI::MethodCall));
    if (getSubroutine()->getReturnType() == PHANTOM_TYPEOF(void))
        o_ir_vbuilder->CreateRetVoid();
    else
        o_ir_vbuilder->CreateRet(call);
}

void Method::compileThisAdjustementThunk(std::ptrdiff_t a_iThisOffset) const
{
    PHANTOM_ASSERT(m_ThisAdjustmentThunks.find(a_iThisOffset) == m_ThisAdjustmentThunks.end());

    size_t                    argCount = getSignature()->getParameterCount() + 1; // parameters+this
    std::vector<llvm::Value*> args;

    // The fixing function has the same signature as the indirected one
    String          name = getMangledName(CppManglerFlag::Thunk, a_iThisOffset);
    llvm::Function* pFunc = llvm::cast<llvm::Function>(
    m_pCodeGenerator->m_Module
    ->getOrInsertFunction(
    name.c_str(),
    toJitFunctionCallType(getSubroutine()->getSignature()->getFunctionType(), getMethod()->getOwnerClassType()))
    .getCallee());
    m_ThisAdjustmentThunks[a_iThisOffset].set(pFunc, nullptr);
    pFunc->setCallingConv(toJitABI(ABI::MethodCall));
    ApplyFunctionAttributes(pFunc);
    auto Attrs = pFunc->getAttributes();
    Attrs = Attrs.addAttribute(pFunc->getContext(), llvm::AttributeList::FunctionIndex, "thunk");
    pFunc->setAttributes(Attrs);

    llvm::IRBuilder<>* builder = (llvm::IRBuilder<>*)m_ThisAdjustmentThunks[a_iThisOffset].builder;

    auto         arg_it = m_ThisAdjustmentThunks[a_iThisOffset].function->arg_begin();
    llvm::Value* new_this = builder->CreateIntToPtr(
    builder->CreateAdd(builder->CreatePtrToInt(&*arg_it, getSizeTTy()), builder->getSizeT(-a_iThisOffset)),
    arg_it->getType());

    args.push_back(new_this);

    size_t i = 1;
    for (; i < argCount; ++i)
    {
        auto it = arg_it;
        std::advance(it, i);
        args.push_back(&*it);
    }
    auto call = builder->CreateCall(m_jit_function.function, args);
    call->setCallingConv(toJitABI(ABI::MethodCall));
    if (getSubroutine()->getReturnType() == PHANTOM_TYPEOF(void))
        builder->CreateRetVoid();
    else
        builder->CreateRet(call);
}

lang::Method* Method::getMethod() const
{
    return static_cast<lang::Method*>(m_pElement);
}

void Method::codeGen()
{
    Subroutine::codeGen();
    if (getMethod()->isVirtual())
    {
        Class* pClass = getMethod()->getOwnerClass();
        for (auto pVT : pClass->getVirtualMethodTables())
        {
            size_t uiOffset = pVT->getOffset();

            // pure virtuals have null closures
            if (!getMethod()->isPureVirtual())
            {
                auto found = m_ThisAdjustmentThunks.find(uiOffset);
                if (found == m_ThisAdjustmentThunks.end())
                {
                    // No thunk needed, use directly the function as the vtable pointer
                    m_jit_function.getAddress(m_pCodeGenerator);
                }
                else
                {
                    // Thunk needed
                    found->second.getAddress(m_pCodeGenerator);
                }
            }
        }
    }
}

void Method::setupFunctionPointers()
{
    Subroutine::setupFunctionPointers();
    if (getMethod()->isVirtual())
    {
        Class* pClass = getMethod()->getOwnerClass();
        for (auto pVT : pClass->getVirtualMethodTables())
        {
            size_t uiOffset = pVT->getOffset();

            // pure virtuals have null closures
            if (getMethod()->isPureVirtual())
            {
                getMethod()->setVTableClosure(uiOffset, nullptr);
            }
            else
            {
                auto found = m_ThisAdjustmentThunks.find(uiOffset);
                if (found == m_ThisAdjustmentThunks.end())
                {
                    // No thunk needed, use directly the function as the vtable pointer
                    getMethod()->setVTableClosure(uiOffset, m_jit_function.getAddress(m_pCodeGenerator));
                }
                else
                {
                    // Thunk needed
                    getMethod()->setVTableClosure(uiOffset, found->second.getAddress(m_pCodeGenerator));
                }
            }
        }
    }
}

} // namespace jit
} // namespace phantom

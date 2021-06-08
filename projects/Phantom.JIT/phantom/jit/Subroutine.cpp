// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

/* ******************* Includes ****************** */
#include "Subroutine.h"

#pragma warning(push, 0)
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/IR/DebugInfoMetaData.h"
#include "llvm/IR/IRBuilder.h"
#pragma warning(pop)
#include "CodeGenerator.h"
#include "CodeGeneratorPrivate.h"
#include "LocalVariable.h"
#include "phantom/lang/FunctionType.h"
#include "phantom/lang/LocalVariable.h"

#include <iostream>
#include <phantom/lang/Block.h>
#include <phantom/lang/Compiler.h>
#include <phantom/lang/Expression.h>
#include <phantom/lang/Module.h>
#include <phantom/lang/Parameter.h>
#include <phantom/lang/Pointer.h>
#include <phantom/lang/Structure.h>
#include <windows.h>
/* *********************************************** */
extern int s_inc;

namespace phantom
{
namespace jit
{
using namespace phantom::lang;

#if PHANTOM_SIZE_OF_VOID_P == 8
#    define getSizeT(...) o_ir_builder->getInt64(__VA_ARGS__)
#    define getSizeTa(...) o_ir_abuilder->getInt64(__VA_ARGS__)
#    define getSizeTTy(...) llvm::Type::getInt64Ty(getContext()->m_LLVMContext)
#else
#    define getSizeT(...) o_ir_builder->getInt32(__VA_ARGS__)
#    define getSizeTa(...) o_ir_abuilder->getInt32(__VA_ARGS__)
#    define getSizeTTy(...) llvm::Type::getInt32Ty(getContext()->context)
#endif

#define o_ir_builder ((llvm::IRBuilder<>*)m_jit_function.builder)
#define o_ir_abuilder ((llvm::IRBuilder<>*)m_jit_apply_function.builder)
#define o_di_builder (getDebugContext()->m_DIBuilder)
#define o_di_cu (getContext()->m_pDICompilationUnit)

#define o_ir_void(...) Value(__VA_ARGS__, PHANTOM_TYPEOF(void))

#define o_ir_value(...) Value(__VA_ARGS__, pOpType)

Subroutine::Subroutine(lang::Subroutine* a_pSubroutine) : CodeGeneratorData(a_pSubroutine), m_debugInfo(nullptr) {}

Subroutine::~Subroutine(void)
{
    PHANTOM_ASSERT(m_pCodeGenerator == nullptr);
}

void Subroutine::compilerAboutToBeChanged(CodeGeneratorPrivate* a_pCodeGenerator)
{
    if (m_pCodeGenerator)
    {
        if (m_jit_function.function)
        {
            _destroyAllocaHeader();
            m_pCodeGenerator->unregisterJitFunction(m_jit_function);
        }
    }
}

void Subroutine::compilerChanged(CodeGeneratorPrivate* a_pCodeGenerator)
{
    if (m_pCodeGenerator)
    {
        llvm::Function* pFunc = llvm::cast<llvm::Function>(
        m_pCodeGenerator->m_Module
        ->getOrInsertFunction(getMangledName().c_str(), toJitFunctionCallType(getFunctionType(), nullptr))
        .getCallee());

        ApplyFunctionAttributes(pFunc, true);

        pFunc->setCallingConv(toJitABI(getSubroutine()->getABI()));
        CodePosition pos = getSubroutine()->getCodePosition();
        if (lang::Function* pFunc = getSubroutine()->asFunction())
        {
            if (auto dbg = getDebugContext())
                m_debugInfo = dbg->toDISubprogram(pFunc);
        }
        m_jit_function.set(pFunc, m_debugInfo);
        _createAllocaHeader();

        m_pCodeGenerator->registerJitFunction(m_jit_function, this);
        m_jit_call_function = m_jit_function;
        m_jit_function.function->setSubprogram(m_debugInfo);
        createApplyFunction();
    }
}

void Subroutine::createApplyFunction()
{
    auto            name = getMangledName(CppManglerFlag::Apply);
    llvm::Function* pApplyFunc = llvm::cast<llvm::Function>(
    m_pCodeGenerator->m_Module
    ->getOrInsertFunction((name + "@apply").c_str(), llvm::Type::getVoidTy(getContext()->m_LLVMContext),
                          llvm::Type::getInt8Ty(getContext()->m_LLVMContext)->getPointerTo()->getPointerTo(),
                          llvm::Type::getInt8Ty(getContext()->m_LLVMContext)->getPointerTo())
    .getCallee());
    ApplyFunctionAttributes(pApplyFunc, true, false, false);
    m_jit_apply_function.set(pApplyFunc, nullptr);

    Signature* pSignature = getSubroutine()->getSignature();
    Types      types = pSignature->getParameterTypes();
    Type*      returnType = getSubroutine()->getReturnType();
    if (getSubroutine()->isRVOCandidate())
    {
        types.insert(types.begin(), getSubroutine()->getReturnType()->addPointer());
        returnType = PHANTOM_TYPEOF(void);
    }
    else
    {
        if (returnType->asReference())
            returnType = returnType->removeReference()->addPointer(); /// transform reference return type into pointer
    }
    if (auto pMethod = getSubroutine()->asMethod())
    {
        types.insert(types.begin(), pMethod->getOwnerClassType()->addPointer());
    }

    auto arg_it = m_jit_apply_function.function->arg_begin();

    auto params_it = arg_it;
    arg_it++;

    auto ret_it = arg_it;
    arg_it++;

    SmallVector<llvm::Value*> args;

    llvm::Value* ppChar = &*params_it;

    for (int i = 0; i < (int)types.size(); ++i)
    {
        llvm::Value* ppArg = i != 0
        ? o_ir_abuilder->CreateIntToPtr(o_ir_abuilder->CreateAdd(o_ir_abuilder->CreatePtrToInt(ppChar, getSizeTTy()),
                                                                 getSizeTa(PHANTOM_SIZE_OF_VOID_P * i)),
                                        types[i]->asReference()
                                        ? getContext()->toJitArgumentType(types[i])->getPointerTo()
                                        : getContext()->toJitArgumentType(types[i])->getPointerTo()->getPointerTo())

        : o_ir_abuilder->CreatePointerCast(
          ppChar, getContext()->toJitArgumentType(types[i]->removeReference())->getPointerTo()->getPointerTo());
        llvm::Value* pArg = o_ir_abuilder->CreateLoad(ppArg);
        llvm::Value* arg;

        if (types[i]->asReference() || GetArgumentPassingMode(types[i]) == ArgumentPassingMode::ByPointer)
            arg = pArg;
        else
            arg = o_ir_abuilder->CreateLoad(pArg);
        args.push_back(arg);
    }

    llvm::CallInst* ret = o_ir_abuilder->CreateCall(m_jit_call_function.function, toArrayRef(args));
    // ret->setDebugLoc(createDebugLoc(this, CodePosition(1, 1)));
    ret->setCallingConv(m_jit_call_function.function->getCallingConv());
    if (returnType != PHANTOM_TYPEOF(void))
        o_ir_abuilder->CreateStore(ret, o_ir_abuilder->CreatePointerCast(&*ret_it, ret->getType()->getPointerTo()));

    o_ir_abuilder->CreateRetVoid();
}

namespace
{
void DebugPrint(const char* _txt)
{
    printf(_txt);
}
int  traceInc;
void TraceFuncBegin(const char* _txt)
{
    int inc = traceInc;
    while (inc--)
        printf("  ");
    printf(_txt);
    traceInc++;
}
void TraceFuncEnd()
{
    traceInc--;
}
} // namespace

void Subroutine::_compileTraceCallBegin(StringView func)
{
    static std::set<String> strings;
    static FunctionType*    pFuncType =
    getSubroutine()->getSource()->functionType(PHANTOM_TYPEOF(void), {PHANTOM_TYPEOF(const char*)});
    const char* c_str = strings.insert(func).first->c_str();
    PHANTOM_ASSERT(c_str);
    Value arg = convert(createVoidPtrConstant((void*)c_str), PHANTOM_TYPEOF(const char*));
    _callNativePtr(c_str, (void*)TraceFuncBegin, phantom::lang::CDecl, pFuncType, nullptr, &arg, 1);
}

void Subroutine::_compileTraceCallEnd()
{
    static FunctionType* pFuncType = getSubroutine()->getSource()->functionType(PHANTOM_TYPEOF(void), TypesView{});
    _callNativePtr("bibi", (void*)TraceFuncEnd, phantom::lang::CDecl, pFuncType, nullptr, nullptr, 0);
}

void Subroutine::compileDebugPrint(StringView to_print)
{
    static std::set<String> strings;
    static FunctionType*    pFuncType =
    getSubroutine()->getSource()->functionType(PHANTOM_TYPEOF(void), {PHANTOM_TYPEOF(const char*)});
    const char* c_str = strings.insert(to_print).first->c_str();
    Value       arg = convert(createVoidPtrConstant((void*)c_str), PHANTOM_TYPEOF(const char*));
    _callNativePtr(c_str, (void*)DebugPrint, phantom::lang::CDecl, pFuncType, nullptr, &arg, 1);
}

Context* Subroutine::getContext() const
{
    return &m_pCodeGenerator->m_Context;
}

DebugContext* Subroutine::getDebugContext() const
{
    return getContext()->m_pDebugContext;
}

void Subroutine::instructionCompilationCallback(void* insn, byte* start, byte* end)
{
    /*if(start == end) return; // empty instruction => skip
    FunctionEntry func;
    jit_value_t value1 = jit_insn_get_value1((jit_insn_t)insn);
    jit_value_t value2 = jit_insn_get_value2((jit_insn_t)insn);
    if(value1)
        func = jit_value_get_function(value1);
    if(value2)
        func = jit_value_get_function(value2);

    if(func.function == nullptr)
    {
        func = jitFunctionFromMemoryAddressEnd(start);
    }
    if(func.function)
    {
        Subroutine* pCompiledMethod = jitSubroutine(func);
        lang::Subroutine* pSubroutine = pCompiledMethod->getSubroutine();
        PHANTOM_ASSERT(pCompiledMethod);
        if(pSubroutine->getMemoryStart() == 0)
            pSubroutine->setMemoryStart(start);
        pSubroutine->setMemoryEnd(end);
        unsigned short start_line;
        unsigned short start_column;
        unsigned short end_line;
        unsigned short end_column;
        jit_insn_get_code_location((jit_insn_t)insn, &start_line, &start_column, &end_line, &end_column);
        Source* pSource = pSubroutine->getSource();
        CodeRangeLocation codeLocation(CodePosition(start_line, start_column, pSource)
            , CodePosition(end_line, end_column, pSource));
        pSubroutine->addInstruction(new Instruction(jit_insn_get_opcode((jit_insn_t)insn), codeLocation,
    MemoryLocation(start, end)));
    }
    else
    {
        PHANTOM_WARNING(false, "func not_ found");
    }*/
}

void Subroutine::compilationSuccessCallback(void* func)
{
    /*Subroutine* pCompiledMethod = jitSubroutine(func);
    PHANTOM_ASSERT(pCompiledMethod);
    lang::Subroutine* pSubroutine = pCompiledMethod->getSubroutine();
    PHANTOM_ASSERT(pSubroutine);
    if(pSubroutine->getBlock() == nullptr)
        return ;
    SmallVector<lang::LocalVariable*> localVariables;
    pSubroutine->getBlock()->getLocalVariablesCascade(localVariables);
    size_t count = pSubroutine->getInstructionCount();
    SmallVector<Instruction*> undefinedInstructions;
    CodeRangeLocation replacementCodeRangeLocation;
    for(size_t i = 0; i<count; ++i)
    {
        Instruction* pInstruction = pSubroutine->getInstruction(i);
        if(pInstruction->getCodeRangeLocation() == CodeRangeLocation())
        {
            if(replacementCodeRangeLocation != CodeRangeLocation())
            {
                // worst hack ever because i forgot to define setCodeRangeLocation...
                *(CodeRangeLocation*)((byte*)pInstruction+8) = replacementCodeRangeLocation;
            }
            undefinedInstructions.push_back(pInstruction);
        }
        else
        {
            replacementCodeRangeLocation = pInstruction->getCodeRangeLocation();
            for(auto it = undefinedInstructions.begin(); it != undefinedInstructions.end(); ++it)
            {
                *(CodeRangeLocation*)((byte*)(*it)+8) = replacementCodeRangeLocation;
            }
            undefinedInstructions.clear();
        }
    }
    for(auto it = localVariables.begin(); it != localVariables.end(); ++it)
    {
        JitLocalVariable* pLocalVariable =
    static_cast<JitLocalVariable*>(pCompiledMethod->m_pCodeGenerator->getData(*it)); pLocalVariable->setupFrame();
    }*/
}

void Subroutine::finish()
{
    PHANTOM_ASSERT(m_OrphanBlocks.empty());
    fallbackReturn();
    //
    //     m_AllocaInsertingPoint  =
    //
    //     auto prevInsertBlock = o_ir_builder->GetInsertBlock();
    //     auto prevInsertPoint = o_ir_builder->GetInsertPoint();
    //     // connect Allocas block with First block to ensure completeness of the block chain
    //     o_ir_builder->SetInsertPoint(m_pAllocaBlock, m_pAllocaBlock->end());
    //     o_ir_builder->CreateBr(m_pFirstBlock);
    //
    //     o_ir_builder->SetInsertPoint(prevInsertBlock, prevInsertPoint);
    // reset debug loc
    o_ir_builder->SetCurrentDebugLocation(llvm::DebugLoc());
}

void Subroutine::codeGen()
{
    m_jit_call_function.getAddress(m_pCodeGenerator);
}

void Subroutine::setupFunctionPointers()
{
    phantom::Closure c;
    c.address = (void*)m_jit_call_function.getAddress(m_pCodeGenerator);
    getSubroutine()->setApplyPointer((lang::Subroutine::ApplyPointer)m_jit_apply_function.getAddress(m_pCodeGenerator));
    getSubroutine()->setClosure(c);
}

void Subroutine::release()
{
    if (m_jit_function.builder)
    {
        delete (llvm::IRBuilder<>*)m_jit_function.builder;
        m_jit_function.builder = nullptr;
    }
}

lang::Subroutine* Subroutine::getSubroutine() const
{
    return static_cast<lang::Subroutine*>(m_pElement);
}

Signature* Subroutine::getSignature() const
{
    return getSubroutine()->getSignature();
}

FunctionType* Subroutine::getFunctionType() const
{
    return getSubroutine()->getSignature()->getFunctionType();
}

Value Subroutine::load(Value value) const
{
    PHANTOM_ASSERT(value.type->asAddressType());
    if (value.type->getTypeKind() == TypeKind::LValueReference ||
        value.type->getTypeKind() == TypeKind::RValueReference)
    {
        Type* pSubType = value.type->getUnderlyingType();
        if (pSubType->removeQualifiers()->getTypeKind() == TypeKind::Array)
        {
            value.type = pSubType;
            return value; // int(&)[2] hides in fact int[2] for llvm, so loading is a no-op
        }
    }
    return Value(o_ir_builder->CreateLoad(value.value), value.type->removeAddress());
}

Value Subroutine::store(Value dest, Value value)
{
    return Value(o_ir_builder->CreateStore(value.value, dest.value), PHANTOM_TYPEOF(void));
}

Value Subroutine::loadRelative(Value value, ptrdiff_t offset, Type* type)
{
    return Value(o_ir_builder->CreateLoad(adjustPointer(value, offset, type->addLValueReference()).value), type);
}

Value Subroutine::storeRelative(Value dest, ptrdiff_t offset, Value value)
{
    Value adjusted = adjustPointer(dest, offset, value.type->addLValueReference());
    PHANTOM_ASSERT(adjusted.type->removeAddress() == value.type);
    adjusted.value = o_ir_builder->CreatePointerCast(adjusted.value, value.value->getType()->getPointerTo());
    return Value(o_ir_builder->CreateStore(value.value, adjusted.value), PHANTOM_TYPEOF(void));
}

Value Subroutine::adjustPointer(Value value, Value offset, Type* a_pType)
{
    PHANTOM_ASSERT(value.type->asPointer() || value.type->asReference());
    PHANTOM_ASSERT(a_pType->asPointer() || a_pType->asReference());
    PHANTOM_ASSERT(offset.type->asIntegralType());
    if (offset.type != PHANTOM_TYPEOF(ptrdiff_t))
    {
        offset.value = o_ir_builder->CreateIntCast(offset.value, getSizeTTy(), offset.type->isSignedInteger());
        offset.setType(PHANTOM_TYPEOF(ptrdiff_t));
    }
    return Value(
    o_ir_builder->CreateIntToPtr(
    o_ir_builder->CreateAdd(o_ir_builder->CreatePtrToInt(value.value, getSizeTTy()), offset.value), toJitType(a_pType)),
    a_pType);
}

Value Subroutine::adjustPointer(Value value, ptrdiff_t offset, Type* a_pType)
{
    // TODO : uncomment when finished fixing other bugs
    //     if (offset == 0)
    //         return convert(value, a_pType);
    return adjustPointer(value, Value(getSizeT(offset), PHANTOM_TYPEOF(ptrdiff_t)), a_pType);
}

Value Subroutine::loadElem(Value base_addr, Value index, Type* elem_type)
{
    return Value(o_ir_builder->CreateLoad(loadElemAddress(base_addr, index, elem_type).value), elem_type);
}

Value Subroutine::loadElemAddress(Value base_addr, Value index, Type* result_type)
{
    return Value(o_ir_builder->CreateGEP(toJitType(result_type->removeAddress()),
                                         o_ir_builder->CreateBitCast(base_addr.value, toJitType(result_type)),
                                         index.value),
                 result_type);
}

Value Subroutine::loadElemAddress(Value base_addr, size_t index, Type* result_type)
{
    return loadElemAddress(base_addr, createSizeTConstant(index), result_type);
}

Value Subroutine::storeElem(Value base_addr, Value index, Value value)
{
    return Value(o_ir_builder->CreateStore(value.value, loadElemAddress(base_addr, index, value.type).value),
                 PHANTOM_TYPEOF(void));
}

Value Subroutine::checkNull(Value value)
{
    return Value(o_ir_builder->CreateIsNull(value.value), PHANTOM_TYPEOF(bool));
}

Type* Subroutine::prepareArithmeticBinOp(Value& value1, Value& value2)
{
    //     If either is      long          double the other is promoted to      long          double
    //     If either is                    double the other is promoted to                    double
    //     If either is                    float  the other is promoted to                    float
    //     If either is long long unsigned int    the other is promoted to long long unsigned int
    //     If either is long long          int    the other is promoted to long long          int
    //     If either is long      unsigned int    the other is promoted to long      unsigned int
    //     If either is long               int    the other is promoted to long               int
    //     if either is           unsigned int    the other is promoted to           unsigned int
    //     If either is                    int    the other is promoted to                    int

    if (value1.type == value2.type)
        return value1.type;
    bool val1ptr = value1.type->asPointer() != nullptr || value1.type->asMethodPointer() != nullptr;
    bool val2ptr = value2.type->asPointer() != nullptr || value2.type->asMethodPointer() != nullptr;
    if (val1ptr && value2.type == PHANTOM_TYPEOF(std::nullptr_t))
    {
        value2 = convert(value2, value1.type);
        return value1.type;
    }
    else if (val2ptr || value1.type == PHANTOM_TYPEOF(std::nullptr_t))
    {
        value1 = convert(value1, value2.type);
        return value2.type;
    }
    else if (val1ptr && val2ptr)
    {
        if (value1.type->removePointer()->isA(value2.type->removePointer()))
        {
            value1 = convert(value1, value2.type);
            return value2.type;
        }
        else
        {
            value2 = convert(value2, value1.type);
            return value1.type;
        }
    }
    else if (val1ptr)
    {
        // arithmetic operation on pointer
        PHANTOM_ASSERT(value2.type->asIntegralType());
        return value1.type;
    }
    else if (val2ptr)
    {
        // arithmetic operation on pointer
        PHANTOM_ASSERT(value1.type->asIntegralType());
        return value2.type;
    }

    PHANTOM_ASSERT(value1.type->asEnum() == nullptr && value2.type->asEnum() == nullptr,
                   "value shouldn't be of enum type, never");
    PHANTOM_ASSERT(value1.type->asArithmeticType() && value2.type->asArithmeticType());
    if (value1.type->getTypeKind() > value2.type->getTypeKind())
    {
        value2 = convert(value2, value1.type);
        return value1.type;
    }
    else
    {
        PHANTOM_ASSERT(value1.type->getTypeKind() < value2.type->getTypeKind());
        value1 = convert(value1, value2.type);
        return value2.type;
    }
}

Value Subroutine::add(Value value1, Value value2)
{
    if (value1.type->asPointer())
    {
        return loadElemAddress(value1, value2, value1.type);
    }
    else if (value2.type->asPointer())
    {
        return loadElemAddress(value2, value1, value2.type);
    }
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    if (value1.type->asFloatingPointType())
    {
        return o_ir_value(o_ir_builder->CreateFAdd(value1.value, value2.value));
    }
    PHANTOM_ASSERT(value1.type->asIntegralType());
    return o_ir_value(o_ir_builder->CreateAdd(value1.value, value2.value));
}

Value Subroutine::sub(Value value1, Value value2)
{
    if (value1.type->asPointer())
    {
        if (value2.type->asPointer()) // pointer subtraction
        {
            Pointer* pPtrType = static_cast<Pointer*>(value1.type);
            PHANTOM_ASSERT(value2.type == pPtrType || value2.type->asIntegralType());
            value1 = convert(value1, PHANTOM_TYPEOF(size_t));
            value2 = convert(value2, PHANTOM_TYPEOF(size_t));
            Type* pOpType = PHANTOM_TYPEOF(ptrdiff_t);
            if (pPtrType->getPointeeType() != PHANTOM_TYPEOF(void))
            {
                Value sub = o_ir_value(o_ir_builder->CreateSub(value1.value, value2.value));
                return o_ir_value(o_ir_builder->CreateExactUDiv(
                sub.value, createSizeTConstant(pPtrType->getPointeeType()->getSize()).value));
            }
        }
        return loadElemAddress(value1, neg(value2), value1.type);
    }
    else if (value2.type->asPointer())
    {
        return loadElemAddress(value2, neg(value1), value2.type);
    }

    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    if (value1.type->asFloatingPointType())
    {
        return o_ir_value(o_ir_builder->CreateFSub(value1.value, value2.value));
    }
    return o_ir_value(o_ir_builder->CreateSub(value1.value, value2.value));
    PHANTOM_ASSERT(value1.type->asIntegralType());
}

Value Subroutine::mul(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    if (value1.type->asFloatingPointType())
    {
        return o_ir_value(o_ir_builder->CreateFMul(value1.value, value2.value));
    }
    PHANTOM_ASSERT(value1.type->asIntegralType());
    return o_ir_value(o_ir_builder->CreateMul(value1.value, value2.value));
}

Value Subroutine::div(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    if (value1.type->asIntegralType())
    {
        if (value1.type->isSignedInteger())
        {
            return o_ir_value(o_ir_builder->CreateSDiv(value1.value, value2.value));
        }
        else
        {
            return o_ir_value(o_ir_builder->CreateUDiv(value1.value, value2.value));
        }
    }
    else
    {
        return o_ir_value(o_ir_builder->CreateFDiv(value1.value, value2.value));
    }
}

Value Subroutine::rem(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    if (value1.type->asIntegralType())
    {
        if (value1.type->isSignedInteger() || value2.type->isSignedInteger())
        {
            return o_ir_value(o_ir_builder->CreateSRem(value1.value, value2.value));
        }
        else
        {
            return o_ir_value(o_ir_builder->CreateURem(value1.value, value2.value));
        }
    }
    else
    {
        return o_ir_value(o_ir_builder->CreateFRem(value1.value, value2.value));
    }
}

Value Subroutine::neg(Value value1)
{
    Type* pOpType = value1.type;
    if (value1.type->asFloatingPointType())
    {
        return o_ir_value(o_ir_builder->CreateFNeg(value1.value));
    }
    PHANTOM_ASSERT(value1.type->asIntegralType());
    return o_ir_value(o_ir_builder->CreateNeg(value1.value));
}

#undef and_
#undef or_
#undef xor_
#undef not_

Value Subroutine::and_(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    return o_ir_value(o_ir_builder->CreateAnd(value1.value, value2.value));
}

Value Subroutine::or_(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    return o_ir_value(o_ir_builder->CreateOr(value1.value, value2.value));
}

Value Subroutine::xor_(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    return o_ir_value(o_ir_builder->CreateXor(value1.value, value2.value));
}

Value Subroutine::not_(Value value1)
{
    if (value1.type == phantom::lang::BuiltInTypes::TYPE_BOOL)
        return toNotBool(value1);
    //     {
    //         return Value(o_ir_builder->CreateAnd(o_ir_builder->CreateXor(value1.value,
    //         o_ir_builder->getInt8(-1)),
    //                                                     o_ir_builder->getInt8(1)),
    //                             phantom::lang::BuiltInTypes::TYPE_BOOL);
    //     }
    Type* pOpType = value1.type;
    return o_ir_value(o_ir_builder->CreateNot(value1.value));
}

Value Subroutine::shl(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    return o_ir_value(o_ir_builder->CreateShl(value1.value, value2.value));
}

Value Subroutine::shr(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    return o_ir_value(o_ir_builder->CreateAShr(value1.value, value2.value));
}

Value Subroutine::ushr(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    return o_ir_value(o_ir_builder->CreateLShr(value1.value, value2.value));
}

Value Subroutine::sshr(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    return o_ir_value(o_ir_builder->CreateAShr(value1.value, value2.value));
}

Value Subroutine::eq(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    pOpType = PHANTOM_TYPEOF(bool);
    if (value1.type->asIntegralType() || value1.type->asPointer() || value1.type->asMethodPointer())
    {
        return o_ir_value(o_ir_builder->CreateICmpEQ(value1.value, value2.value));
    }
    else
    {
        return o_ir_value(o_ir_builder->CreateFCmpUEQ(value1.value, value2.value));
    }
}

Value Subroutine::ne(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    pOpType = PHANTOM_TYPEOF(bool);
    if (value1.type->asIntegralType() || value1.type->asPointer() || value1.type->asMethodPointer())
    {
        return o_ir_value(o_ir_builder->CreateICmpNE(value1.value, value2.value));
    }
    else
    {
        return o_ir_value(o_ir_builder->CreateFCmpUNE(value1.value, value2.value));
    }
}

Value Subroutine::lt(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    pOpType = PHANTOM_TYPEOF(bool);
    if (value1.type->asIntegralType() || value1.type->asPointer())
    {
        if (value1.type->isSignedInteger())
        {
            return o_ir_value(o_ir_builder->CreateICmpSLT(value1.value, value2.value));
        }
        else
        {
            return o_ir_value(o_ir_builder->CreateICmpULT(value1.value, value2.value));
        }
    }
    else
    {
        return o_ir_value(o_ir_builder->CreateFCmpULT(value1.value, value2.value));
    }
}

Value Subroutine::le(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    pOpType = PHANTOM_TYPEOF(bool);
    if (value1.type->asIntegralType() || value1.type->asPointer())
    {
        if (value1.type->isSignedInteger())
        {
            return o_ir_value(o_ir_builder->CreateICmpSLE(value1.value, value2.value));
        }
        else
        {
            return o_ir_value(o_ir_builder->CreateICmpULE(value1.value, value2.value));
        }
    }
    else
    {
        return o_ir_value(o_ir_builder->CreateFCmpULE(value1.value, value2.value));
    }
}

Value Subroutine::gt(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    pOpType = PHANTOM_TYPEOF(bool);
    if (value1.type->asIntegralType() || value1.type->asPointer())
    {
        if (value1.type->isSignedInteger())
        {
            return o_ir_value(o_ir_builder->CreateICmpSGT(value1.value, value2.value));
        }
        else
        {
            return o_ir_value(o_ir_builder->CreateICmpUGT(value1.value, value2.value));
        }
    }
    else
    {
        return o_ir_value(o_ir_builder->CreateFCmpUGT(value1.value, value2.value));
    }
}

Value Subroutine::ge(Value value1, Value value2)
{
    Type* pOpType = prepareArithmeticBinOp(value1, value2);
    pOpType = PHANTOM_TYPEOF(bool);
    if (value1.type->asIntegralType() || value1.type->asPointer())
    {
        if (value1.type->isSignedInteger() || value1.type->asPointer())
        {
            return o_ir_value(o_ir_builder->CreateICmpSGE(value1.value, value2.value));
        }
        else
        {
            return o_ir_value(o_ir_builder->CreateICmpUGE(value1.value, value2.value));
        }
    }
    else
    {
        return o_ir_value(o_ir_builder->CreateFCmpUGE(value1.value, value2.value));
    }
}

Value Subroutine::toBool(Value value1)
{
    PHANTOM_ASSERT(value1.type->asReference() == nullptr, "references cannot be converted to bool");
    Type* pOpType = PHANTOM_TYPEOF(bool);
    if (value1.type == pOpType)
        return value1;
    /* Perform a comparison to determine if the value is non-zero */
    if (value1.type->asPointerType() || value1.type->asMethodPointer())
    {
        return o_ir_value(o_ir_builder->CreateIsNotNull(value1.value));
    }
    if (value1.type->asIntegralType())
    {
        switch (value1.type->getSize())
        {
        case 1:
            return o_ir_value(o_ir_builder->CreateICmpNE(value1.value, o_ir_builder->getInt8(0)));
        case 2:
            return o_ir_value(o_ir_builder->CreateICmpNE(value1.value, o_ir_builder->getInt16(0)));
        case 4:
            return o_ir_value(o_ir_builder->CreateICmpNE(value1.value, o_ir_builder->getInt32(0)));
        case 8:
            return o_ir_value(o_ir_builder->CreateICmpNE(value1.value, o_ir_builder->getInt64(0)));
        default:
            return ne(value1, o_ir_value(o_ir_builder->getInt64(0)));
        }
    }
    else if (value1.type->getTypeKind() == TypeKind::Float || value1.type->getTypeKind() == TypeKind::Double)
        return o_ir_value(o_ir_builder->CreateFCmpUEQ(value1.value, createFloatConstant(0.f).value));
    PHANTOM_ASSERT(false, "invalid toBool conversion or not implemented yet");
    return ne(value1, o_ir_value(o_ir_builder->getInt64(0)));
}

Value Subroutine::toNotBool(Value value1)
{
    Type* pOpType = PHANTOM_TYPEOF(bool);
    if (value1.type == PHANTOM_TYPEOF(bool))
    {
        PHANTOM_ASSERT(value1.value->getType() == o_ir_builder->getInt1Ty());
        return o_ir_value(o_ir_builder->CreateICmpEQ(value1.value, o_ir_builder->getInt1(0)));
    }
    /* Perform a comparison to determine if the value is non-zero */
    if (value1.type->asPointerType())
    {
        return o_ir_value(
        o_ir_builder->CreateICmpEQ(o_ir_builder->CreatePtrToInt(value1.value, getSizeTTy()), getSizeT(0)));
    }
    if (value1.type->asIntegralType())
    {
        switch (value1.type->getSize())
        {
        case 1:
            return o_ir_value(o_ir_builder->CreateICmpEQ(value1.value, o_ir_builder->getInt8(0)));
        case 2:
            return o_ir_value(o_ir_builder->CreateICmpEQ(value1.value, o_ir_builder->getInt16(0)));
        case 4:
            return o_ir_value(o_ir_builder->CreateICmpEQ(value1.value, o_ir_builder->getInt32(0)));
        case 8:
            return o_ir_value(o_ir_builder->CreateICmpEQ(value1.value, o_ir_builder->getInt64(0)));
        default:
            return eq(value1, o_ir_value(o_ir_builder->getInt64(0)));
        }
    }
    return eq(value1, o_ir_value(o_ir_builder->getInt64(0)));
}

Value Subroutine::branch(Label* l)
{
    if (l->block == nullptr)
    {
        l->block = llvm::BasicBlock::Create(getContext()->m_LLVMContext, "");
        PHANTOM_VERIFY(m_OrphanBlocks.insert(l).second);
    }
    llvm::BasicBlock* pNewBlock = llvm::BasicBlock::Create(getContext()->m_LLVMContext, "");
    llvm::Value*      v = o_ir_builder->CreateBr(l->block);
    pNewBlock->insertInto(m_jit_function.function);
    o_ir_builder->SetInsertPoint(pNewBlock);
    return o_ir_void(v);
}

Value Subroutine::branchIf(Value value, Label* l)
{
    /// if label not_ defined yet, allocate a detached block which will be inserted on label() call
    if (l->block == nullptr)
    {
        l->block = llvm::BasicBlock::Create(getContext()->m_LLVMContext, "");
        PHANTOM_VERIFY(m_OrphanBlocks.insert(l).second);
    }
    llvm::BasicBlock* pFalseBlock = llvm::BasicBlock::Create(getContext()->m_LLVMContext, "");
    llvm::Value*      v = o_ir_builder->CreateCondBr(value.value, l->block, pFalseBlock);
    pFalseBlock->insertInto(m_jit_function.function);
    o_ir_builder->SetInsertPoint(pFalseBlock);
    return o_ir_void(v);
}

Value Subroutine::branchIfNot(Value value, Label* l)
{
    /// if label not_ defined yet, allocate a detached block which will be inserted on label() call
    if (l->block == nullptr)
    {
        l->block = llvm::BasicBlock::Create(getContext()->m_LLVMContext, "");
        PHANTOM_VERIFY(m_OrphanBlocks.insert(l).second);
    }
    llvm::BasicBlock* pTrueBlock = llvm::BasicBlock::Create(getContext()->m_LLVMContext, "");
    llvm::Value*      v = o_ir_builder->CreateCondBr(value.value, pTrueBlock, l->block);
    pTrueBlock->insertInto(m_jit_function.function);
    o_ir_builder->SetInsertPoint(pTrueBlock);
    return o_ir_void(v);
}

int Subroutine::label(Label* l)
{
    if (l->block == nullptr)
    {
        l->block = llvm::BasicBlock::Create(getContext()->m_LLVMContext, "");
    }
    else
    {
        PHANTOM_VERIFY(m_OrphanBlocks.erase(l) == 1);
    }
    o_ir_builder->CreateBr(l->block);
    l->block->insertInto(m_jit_function.function);
    o_ir_builder->SetInsertPoint(l->block);
    return 1;
}

Value Subroutine::jumpTable(Value value, Label* labels, unsigned int num_labels)
{
    return o_ir_void(o_ir_builder->CreateSwitch(value.value, labels[0].block, num_labels));
}

Value Subroutine::convert(Value value, Type* a_pDestType)
{
    Type* pOpType = a_pDestType->removeAllQualifiers();
    if (value.type == pOpType)
        return value;
    if (pOpType->asIntegralType())
    {
        if (value.type->asIntegralType())
        {
            return o_ir_value(
            o_ir_builder->CreateIntCast(value.value, toJitType(pOpType), value.type->isSignedInteger()));
        }
        else if (value.type->asPointerType())
        {
            return o_ir_value(o_ir_builder->CreatePtrToInt(value.value, toJitType(pOpType)));
        }
        else if (value.type->asFloatingPointType())
        {
            return a_pDestType->isSignedInteger()
            ? o_ir_value(o_ir_builder->CreateFPToSI(value.value, toJitType(pOpType)))
            : o_ir_value(o_ir_builder->CreateFPToUI(value.value, toJitType(pOpType)));
        }
    }
    else if (a_pDestType->asFloatingPointType())
    {
        if (value.type->asIntegralType())
        {
            return value.type->isSignedInteger()
            ? o_ir_value(o_ir_builder->CreateSIToFP(value.value, toJitType(pOpType)))
            : o_ir_value(o_ir_builder->CreateUIToFP(value.value, toJitType(pOpType)));
        }
        else if (value.type->asFloatingPointType())
        {
            return o_ir_value(o_ir_builder->CreateFPCast(value.value, toJitType(pOpType)));
        }
    }
    else if (pOpType->asMethodPointer())
    {
        if (value.type->asPointerType())
        {
            return o_ir_value(o_ir_builder->CreatePointerCast(value.value, toJitType(pOpType)));
        }
    }
    else if (pOpType->asPointerType())
    {
        if (value.type->asIntegralType())
        {
            return o_ir_value(o_ir_builder->CreateIntToPtr(value.value, toJitType(pOpType)));
        }
        else if (value.type->asPointerType())
        {
            return o_ir_value(o_ir_builder->CreatePointerBitCastOrAddrSpaceCast(value.value, toJitType(pOpType)));
        }
    }
    else if (pOpType->asReference())
    {
        if (auto pArrOut = pOpType->removeReference()->asArray())
        {
            PHANTOM_ASSERT(value.type->asReference());
            auto pDstTypeNoExt = pArrOut->getUnderlyingType()->removeAllExtents();
            if (value.type->getUnderlyingType() == pDstTypeNoExt)
            {
                return value;
            }
            else
            {
                return convert(value, pDstTypeNoExt);
            }
        }
    }
    else if (auto pArrOut = pOpType->asArray())
    {
        if (pArrOut->getItemCount() == 0)
            if (auto pArrIn = value.type->removeReference()->asArray())
            {
                if (pArrIn->getItemType() == pArrOut->getItemType())
                {
                    value.type = pOpType;
                    return value;
                }
            }
    }
    return o_ir_value(o_ir_builder->CreateBitOrPointerCast(value.value, toJitType(pOpType)));
}

#define PHANTOM_JIT_DEBUG_CALL_ENABLED 0

#if PHANTOM_JIT_DEBUG_CALL_ENABLED
#    define PHANTOM_JIT_DEBUG_TRACE_CALL_SCOPED(str)                                                                   \
        _compileTraceCallBegin((String(str) + '\n').c_str());                                                          \
        auto scopeExit = phantom::makeScopeExit([this] { _compileTraceCallEnd(); });
#else
#    define PHANTOM_JIT_DEBUG_TRACE_CALL_SCOPED(str)
#endif

Value Subroutine::callSubroutine(lang::Subroutine* a_pSubroutine, Value* args, uint a_uiArgCount, int flags,
                                 bool a_bForceNonVirtual)
{
    lang::Subroutine* pCalledSubroutine = a_pSubroutine;
    Type*             pReturnType = a_pSubroutine->getSignature()->getReturnType();
    CodeGenerator*    pSubroutineCodeGenerator =
    static_cast<CodeGenerator*>(Compiler::Get()->getCodeGenerator(a_pSubroutine->getModule()));
    Subroutine* pSubroutine = nullptr;

    if (!a_pSubroutine->isNative() && pSubroutineCodeGenerator &&
        pSubroutineCodeGenerator->m_private == m_pCodeGenerator)
    {
        pSubroutine = static_cast<Subroutine*>(pSubroutineCodeGenerator->m_private->getData(a_pSubroutine));
        PHANTOM_ASSERT(pSubroutine);
    }
    Value returnedValue;
    PHANTOM_JIT_DEBUG_TRACE_CALL_SCOPED(a_pSubroutine->getQualifiedName().c_str());
    if (pSubroutine)
    {
        SmallVector<llvm::Value*> values;
        for (size_t i = 0; i < a_uiArgCount; ++i)
        {
            values.push_back(args[i].value);
        }
        llvm::Function* pFunc =
        (a_bForceNonVirtual ? pSubroutine->m_jit_function : pSubroutine->m_jit_call_function).function;
        llvm::CallInst* pCall = o_ir_builder->CreateCall(pFunc, toArrayRef(values));
        pCall->setCallingConv(pFunc->getCallingConv());
        return Value(pCall,
                     a_pSubroutine->isRVOCandidate() ? a_pSubroutine->getReturnType()->addPointer()
                                                     : a_pSubroutine->getReturnType());
    }
    else
    {
        return callNative(a_pSubroutine, args, a_uiArgCount, a_bForceNonVirtual);
    }

    return returnedValue;
}

Value Subroutine::callNative(lang::Subroutine* a_pSubroutine, Value* a_pArgs, unsigned int a_uiArgCount,
                             bool a_bForceNonVirtual)
{
    lang::Method* pMethod = a_pSubroutine->asMethod();
    ClassType*    pThisType = pMethod ? pMethod->getOwnerClassType() : nullptr;
    if (a_bForceNonVirtual && a_pSubroutine->isVirtual())
    {
        PHANTOM_ASSERT(pMethod);
        Class*             pMethodClass = PHANTOM_CLASSOF(lang::Method);
        Value              methodPtrVvalue = createPtrConstant(pMethod);
        SmallVector<Value> args;
        args.push_back(methodPtrVvalue);
        args.push_back(createSizeTConstant(0));
        Value vTableClosureValue = callNative(pMethodClass->getMethod("getVTableClosure"), args.data(), 2, false);
        SmallVector<llvm::Value*> values;
        for (size_t i = 0; i < a_uiArgCount; ++i)
        {
            values.push_back(a_pArgs[i].value);
        }
        auto callType = toJitFunctionCallType(a_pSubroutine->getSignature()->getFunctionType(), pThisType);
        auto pCall = o_ir_builder->CreateCall(
        callType, o_ir_builder->CreatePointerCast(vTableClosureValue.value, callType->getPointerTo()),
        toArrayRef(values));
        return Value(pCall,
                     a_pSubroutine->isRVOCandidate() ? a_pSubroutine->getReturnType()->addPointer()
                                                     : a_pSubroutine->getReturnType());
    }
    else
    {
        auto c = a_pSubroutine->getClosure();
        if (c.offset)
        {
            a_pArgs[0] = this->adjustPointer(a_pArgs[0], c.offset, a_pArgs[0].type);
        }
        void* pClosure = c.address;
        return _callNativePtr(a_pSubroutine->getUniqueName().c_str(), pClosure, a_pSubroutine->getABI(),
                              a_pSubroutine->getSignature()->getFunctionType(),
                              a_pSubroutine->asMethod() ? a_pSubroutine->getOwner()->asClassType() : nullptr, a_pArgs,
                              a_uiArgCount);
    }
}

Value Subroutine::_callNativePtr(const char* a_pName, void* a_pNativePtr, ABI abi, FunctionType* a_pFunctionType,
                                 ClassType* a_pThisType, Value* args, unsigned int a_uiArgCount)
{
    PHANTOM_ASSERT(a_pNativePtr);

    llvm::Function*& pNativeFunction = m_pCodeGenerator->m_GlobalLLVMFunctions[a_pNativePtr];
    if (pNativeFunction == nullptr)
    {
        pNativeFunction =
        llvm::Function::Create(toJitFunctionCallType(a_pFunctionType, a_pThisType), llvm::Function::LinkOnceAnyLinkage,
                               a_pName, m_pCodeGenerator->m_Module);
        llvm::CallingConv::ID conv;
        if (abi == ABI::ThisCall || abi == ABI::Win64)
        {
            conv = toJitABI(abi);
        }
        else
        {
            conv = llvm::CallingConv::C;
        }
        pNativeFunction->setCallingConv(conv);
        m_pCodeGenerator->m_ExecutionEngine->addGlobalMapping(
        pNativeFunction, const_cast<void*>(a_pNativePtr)); // LLVM always takes non-const pointers
    }
    SmallVector<llvm::Value*> values;
    for (size_t i = 0; i < a_uiArgCount; ++i)
    {
        values.push_back(args[i].value);
    }
    auto pCall = o_ir_builder->CreateCall(pNativeFunction, toArrayRef(values));
    pCall->setCallingConv(pNativeFunction->getCallingConv());
    return Value(pCall,
                 a_pFunctionType->isRVOCandidate() ? a_pFunctionType->getReturnType()->addPointer()
                                                   : a_pFunctionType->getReturnType());
}

Value Subroutine::callNativePtr(const char* a_pName, void* a_pNativePtr, ABI abi, FunctionType* a_pFunctionType,
                                ClassType* a_pThisType, Value* args, unsigned int a_uiArgCount)
{
    return _callNativePtr(a_pName, a_pNativePtr, abi, a_pFunctionType, a_pThisType, args, a_uiArgCount);
}

Value Subroutine::returnValue(Value value)
{
    Value r(o_ir_builder->CreateRet(value.value), PHANTOM_TYPEOF(void));
    PHANTOM_ASSERT(value.value->getType() == m_jit_call_function.function->getReturnType());
    llvm::BasicBlock* pNewBlock = llvm::BasicBlock::Create(getContext()->m_LLVMContext, "", m_jit_function.function);
    o_ir_builder->SetInsertPoint(pNewBlock);
    return r;
}

Value Subroutine::returnVoid()
{
    PHANTOM_ASSERT(getSubroutine()->getReturnType() == PHANTOM_TYPEOF(void));
    Value             r(o_ir_builder->CreateRetVoid(), PHANTOM_TYPEOF(void));
    llvm::BasicBlock* pNewBlock = llvm::BasicBlock::Create(getContext()->m_LLVMContext, "", m_jit_function.function);
    o_ir_builder->SetInsertPoint(pNewBlock);
    return r;
}

Value Subroutine::fallbackReturn()
{
    Value ret;
    // if (getSubroutine()->isRVOCandidate() || getSubroutine()->getReturnType() == PHANTOM_TYPEOF(void))
    ret = o_ir_void(o_ir_builder->CreateRetVoid());
    //     else
    //         ret = o_ir_void(o_ir_builder->CreateRet(
    //         o_ir_builder->CreateLoad(getOrCreateAlloca(getSubroutine(),
    //         getSubroutine()->getReturnType()).value)));
    return ret;
}
//
// int         Subroutine::throwValue(Value value)
// {
//     return jit_insn_throw((jit_function_t)m_jit_function.function, (jit_value_t)value.value);
// }
//
// Value   Subroutine::getCallStack ()
// {
//     return jit_insn_get_call_stack((jit_function_t)m_jit_function.function);
// }
//
// Value   Subroutine::thrownException ()
// {
//     return jit_insn_thrown_exception((jit_function_t)m_jit_function.function);
// }
//
// int         Subroutine::usesCatcher ()
// {
//     return jit_insn_uses_catcher((jit_function_t)m_jit_function.function);
// }
//
// Value   Subroutine::startCatcher ()
// {
//     return jit_insn_start_catcher((jit_function_t)m_jit_function.function);
// }
//
// int         Subroutine::branchIfPCNotInRange (Label start_label, Label end_label,
// Label *label)
// {
//     return jit_insn_branch_if_pc_not_in_range((jit_function_t)m_jit_function.function, start_label.label,
//     end_label.label, (jit_label_t*)label);
// }
//
// int         Subroutine::rethrowUnhandled ()
// {
//     return jit_insn_rethrow_unhandled((jit_function_t)m_jit_function.function);
// }
//
// int         Subroutine::startFinally (Label *finally_label)
// {
//     return jit_insn_start_finally((jit_function_t)m_jit_function.function, (jit_label_t*)finally_label);
// }
//
// int         Subroutine::returnFromFinally ()
// {
//     return jit_insn_return_from_finally((jit_function_t)m_jit_function.function);
// }
//
// int         Subroutine::callFinally (Label *finally_label)
// {
//     return jit_insn_call_finally((jit_function_t)m_jit_function.function, (jit_label_t*)finally_label);
// }
//
// Value   Subroutine::startFilter (Label *label, Type* type)
// {
//     return jit_insn_start_filter((jit_function_t)m_jit_function.function, (jit_label_t*)label, toJitType(type));
// }
//
// int         Subroutine::returnFromFilter (Value value)
// {
//     return jit_insn_return_from_filter((jit_function_t)m_jit_function.function, (jit_value_t)value.value);
// }
//
// Value   Subroutine::callFilter (Label *label, Value value, Type* type)
// {
//     return jit_insn_call_filter((jit_function_t)m_jit_function.function, (jit_label_t*)label,
//     (jit_value_t)value.value, toJitType(type));
// }

Value Subroutine::memcpy PHANTOM_PREVENT_MACRO_SUBSTITUTION(Value dest, Value src, Value size)
{
    return o_ir_void(
    o_ir_builder->CreateMemCpy(dest.value, llvm::MaybeAlign(1), src.value, llvm::MaybeAlign(1), size.value));
}

Value Subroutine::memmove PHANTOM_PREVENT_MACRO_SUBSTITUTION(Value dest, Value src, Value size)
{
    return o_ir_void(
    o_ir_builder->CreateMemMove(dest.value, llvm::MaybeAlign(1), src.value, llvm::MaybeAlign(1), size.value));
}

Value Subroutine::memset PHANTOM_PREVENT_MACRO_SUBSTITUTION(Value dest, Value value, Value size)
{
    return o_ir_void(o_ir_builder->CreateMemSet(dest.value, value.value, size.value, llvm::MaybeAlign(1)));
}

Value Subroutine::alloca PHANTOM_PREVENT_MACRO_SUBSTITUTION(Value size)
{
    return Value(o_ir_builder->CreateAlloca(llvm::Type::getInt8Ty(getContext()->m_LLVMContext), size.value),
                 PHANTOM_TYPEOF(void*));
}
//
// int         Subroutine::moveBlocksToEnd (Label from_label, Label to_label)
// {
//     return jit_insn_move_blocks_to_end((jit_function_t)m_jit_function.function, from_label.label,
//     to_label.label);
// }
//
// int         Subroutine::moveBlocksToStart (Label from_label, Label to_label)
// {
//     return jit_insn_move_blocks_to_start((jit_function_t)m_jit_function.function, from_label.label,
//     to_label.label);
// }

// int         Subroutine::markOffset (int offset)
// {
//     return jit_insn_mark_offset((jit_function_t)m_jit_function.function, offset);
// }
/*

void        Subroutine::iterInit (iter_t *iter, jit_block block)
{
    PHANTOM_ASSERT(false);
}

void        Subroutine::iterInitLast (iter_t *iter, jit_block block)
{
    PHANTOM_ASSERT(false);
}

jit_insn    Subroutine::iterNext (iter_t *iter)
{
    PHANTOM_ASSERT(false);
}

jit_insn    Subroutine::iterPrevious (iter_t *iter)
{
    PHANTOM_ASSERT(false);
}*/

void Subroutine::createArgumentDebugInfo(Parameter* a_pParameter, Value a_Address)
{
    PHANTOM_ASSERT(getDebugContext());
    Type* pType = a_pParameter->getValueType();
    if (GetArgumentPassingMode(pType) == ArgumentPassingMode::ByPointer)
        pType = pType->addPointer();
    auto                   loc = a_pParameter->getCodeRange().begin;
    llvm::DILocalVariable* var;
    auto const&            name = a_pParameter->getName();
    auto                   pSubroutine = getSubroutine();
    size_t                 i = 1 + (pSubroutine->asMethod() != nullptr) + pSubroutine->isRVOCandidate();
    for (auto p : pSubroutine->getParameters())
    {
        if (p == a_pParameter)
            break;
        ++i;
    }
    auto pSubProgram = getDebugContext()->toDISubprogram(getSubroutine());
    var = o_di_builder.createParameterVariable(pSubProgram, toStringRef(name), i, m_debugInfo->getFile(), loc.line,
                                               getDebugContext()->toDIType(pType), true, llvm::DINode::FlagZero);
    o_di_builder.insertDeclare(a_Address.value, var, o_di_builder.createExpression(),
                               llvm::DebugLoc::get(loc.line, loc.column, pSubProgram),
                               &m_jit_function.function->getEntryBlock());
}

void Subroutine::createLocalVariableDebugInfo(lang::LocalVariable* a_pLocalVariable, Value a_Address)
{
    // TODO : make 'this' a parameter inside Phantom
    if (a_pLocalVariable->isThis())
    {
        PHANTOM_ASSERT(getDebugContext());
        auto const& name = a_pLocalVariable->getName();
        auto        loc = a_pLocalVariable->getCodeRange().begin;
        auto        pSubProgram = getDebugContext()->toDISubprogram(getSubroutine());
        auto        var = o_di_builder.createParameterVariable(
        pSubProgram, toStringRef(name), 1, m_debugInfo->getFile(), loc.line,
        o_di_builder.createObjectPointerType(getDebugContext()->toDIType(a_pLocalVariable->getValueType())), true,
        llvm::DINode::FlagArtificial | llvm::DINode::FlagObjectPointer);
        o_di_builder.insertDeclare(a_Address.value, var, o_di_builder.createExpression(),
                                   llvm::DebugLoc::get(loc.line, loc.column, pSubProgram),
                                   &m_jit_function.function->getEntryBlock());
    }
    else if (Parameter* pParam = a_pLocalVariable->asParameter())
    {
        return createArgumentDebugInfo(pParam, a_Address);
    }
    else
    {
        return createBlockVariableDebugInfo(a_pLocalVariable, a_Address);
    }
}

void Subroutine::createBlockVariableDebugInfo(lang::LocalVariable* a_pLocalVariable, Value a_Address)
{
    PHANTOM_ASSERT(getDebugContext());
    auto const& name = a_pLocalVariable->getName();
    auto        loc = a_pLocalVariable->getCodeRange().begin;
    auto        var = o_di_builder.createAutoVariable(
    getDebugContext()->getOrCreateDIScope(a_pLocalVariable->getOwner()), llvm::StringRef(name.data(), name.size()),
    m_debugInfo->getFile(), loc.line, getDebugContext()->toDIType(a_pLocalVariable->getValueType()), true,
    llvm::DINode::FlagZero, a_pLocalVariable->getValueType()->getAlignment() * 8);
    auto pScope = getDebugContext()->getOrCreateDIScope(a_pLocalVariable->getOwner());
    o_di_builder.insertDeclare(a_Address.value, var, o_di_builder.createExpression(),
                               llvm::DebugLoc::get(loc.line, 1, pScope), &m_jit_function.function->getEntryBlock());
}

Value Subroutine::getOrCreateAlloca(LanguageElement* a_pRelatedBlockElement, Type* type, size_t a_Idx,
                                    phantom::Functor<void(Value)> _InsertDeclare)
{
    auto& values = m_Allocas[a_pRelatedBlockElement];
    values.resize(std::max(values.size(), a_Idx + 1));
    if (values[a_Idx].isNull())
        values[a_Idx] = _createAlloca(type, _InsertDeclare);
    return values[a_Idx];
}

Value Subroutine::_createAlloca(Type* type, phantom::Functor<void(Value)> _InsertDeclare)
{
    Value allocaValue;

    // insert all locals alloca into entry block start

    llvm::IRBuilder<> TmpB(&m_jit_function.function->getEntryBlock(), m_jit_function.function->getEntryBlock().begin());
    TmpB.SetCurrentDebugLocation(llvm::DebugLoc());

    if (type->asReference())
    {
        allocaValue =
        Value(TmpB.CreateAlloca(toJitType(type)), type->removeReference()->addPointer()->addLValueReference());
    }
    else
    {
        unsigned AddrSpace = TmpB.GetInsertBlock()->getParent()->getParent()->getDataLayout().getAllocaAddrSpace();
        unsigned Align = type->getAlignment();
        Type*    noArray = type->removeAllExtents();
        if (noArray->getTypeKind() == TypeKind::Float || noArray->getTypeKind() == TypeKind::Double)
        {
            Align = 16;
        }
        if (noArray != type)
        {
            allocaValue =
            Value(TmpB.Insert(new llvm::AllocaInst(toJitType(type->getUnderlyingType()), AddrSpace,
                                                   createSizeTConstant(static_cast<Array*>(type)->getItemCount()).value,
                                                   llvm::Align(Align)),
                              ""),
                  type->addLValueReference());
        }
        else
        {
            allocaValue =
            Value(TmpB.Insert(new llvm::AllocaInst(toJitType(type), AddrSpace, nullptr, llvm::Align(Align)), ""),
                  type->addLValueReference());
        }
    }
    if (_InsertDeclare)
    {
        _InsertDeclare(allocaValue);
    }

    return allocaValue;
}

Value Subroutine::createCharConstant(char const_value)
{
    return Value(o_ir_builder->getInt8(const_value), PHANTOM_TYPEOF(char));
}

Value Subroutine::createUCharConstant(uchar const_value)
{
    return Value(o_ir_builder->getInt8(const_value), PHANTOM_TYPEOF(uchar));
}

Value Subroutine::createShortConstant(short const_value)
{
    return Value(o_ir_builder->getInt16(const_value), PHANTOM_TYPEOF(short));
}

Value Subroutine::createUShortConstant(ushort const_value)
{
    return Value(o_ir_builder->getInt16(const_value), PHANTOM_TYPEOF(ushort));
}

Value Subroutine::createIntConstant(int const_value)
{
    return Value(o_ir_builder->getInt32(const_value), PHANTOM_TYPEOF(int));
}

Value Subroutine::createUIntConstant(uint const_value)
{
    return Value(o_ir_builder->getInt32(const_value), PHANTOM_TYPEOF(uint));
}

Value Subroutine::createSizeTConstant(size_t const_value)
{
    return Value(getSizeT(const_value), PHANTOM_TYPEOF(size_t));
}

Value Subroutine::createPtrDiffTConstant(ptrdiff_t const_value)
{
    return Value(getSizeT(const_value), PHANTOM_TYPEOF(ptrdiff_t));
}

Value Subroutine::createVoidPtrConstant(void* const_value)
{
    return Value(o_ir_builder->CreateIntToPtr(getSizeT(size_t(const_value)), toJitType(PHANTOM_TYPEOF(char*))),
                 PHANTOM_TYPEOF(void*));
}

Value Subroutine::createPtrConstant(void* const_value, Pointer* a_pType)
{
    return Value(o_ir_builder->CreateIntToPtr(getSizeT(size_t(const_value)), toJitType(a_pType)), a_pType);
}

Value Subroutine::createLongLongConstant(longlong const_value)
{
    return Value(o_ir_builder->getInt64(const_value), PHANTOM_TYPEOF(longlong));
}

Value Subroutine::createULongLongConstant(ulonglong const_value)
{
    return Value(o_ir_builder->getInt64(const_value), PHANTOM_TYPEOF(ulonglong));
}

Value Subroutine::createFloatConstant(float const_value)
{
    return Value(llvm::ConstantFP::get(llvm::Type::getFloatTy(getContext()->m_LLVMContext), const_value),
                 PHANTOM_TYPEOF(float));
}

Value Subroutine::createDoubleConstant(double const_value)
{
    return Value(llvm::ConstantFP::get(llvm::Type::getDoubleTy(getContext()->m_LLVMContext), const_value),
                 PHANTOM_TYPEOF(double));
}

Value Subroutine::getThis() const
{
    PHANTOM_ASSERT(getSubroutine()->getOwner());
    PHANTOM_ASSERT(getSubroutine()->asMethod());

    return getSubroutine()->asMethod() ? Value(&*m_jit_function.function->arg_begin(),
                                               static_cast<lang::Method*>(getSubroutine())->getThis()->getValueType())
                                       : Value();
}

Value Subroutine::getRVOParameter() const
{
    uint jitParamIndex = (getSubroutine()->asMethod() != nullptr);
    auto arg_begin = m_jit_function.function->arg_begin();
    std::advance(arg_begin, jitParamIndex);
    return getSubroutine()->isRVOCandidate() ? Value(&*arg_begin, getSubroutine()->getReturnType()->makePointer())
                                             : Value();
}

Value Subroutine::returnAddress(Type* a_pReturnType)
{
    llvm::Value*    args[] = {createIntConstant(0).value};
    llvm::Function* intrinsic =
    llvm::Intrinsic::getDeclaration(m_pCodeGenerator->m_Module, llvm::Intrinsic::returnaddress);
    return Value(
    o_ir_builder->CreateCall(intrinsic, llvm::ArrayRef<llvm::Value*>(&args[0], 1), "intrinsic[returnaddress]"),
    a_pReturnType->addPointer());
}

ABI Subroutine::getABI() const
{
    return getSubroutine()->getABI();
}

Value Subroutine::getParameter(uint param) const
{
    uint jitParamIndex = param + (getSubroutine()->asMethod() != nullptr) + getSubroutine()->isRVOCandidate();
    auto arg_begin = m_jit_function.function->arg_begin();
    std::advance(arg_begin, jitParamIndex);
    return Value(&*arg_begin, m_pCodeGenerator->getArgumentType(getSubroutine()->getParameterType(param)));
}

Value Subroutine::getDebugInfoParameterValue(uint param) const
{
    uint jitParamIndex = param + (getSubroutine()->asMethod() != nullptr) + getSubroutine()->isRVOCandidate();
    auto arg_begin = m_jit_function.function->arg_begin();
    std::advance(arg_begin, jitParamIndex);
    Type* pParamType = getSubroutine()->getParameterType(param);
    Type* pArgType = m_pCodeGenerator->getArgumentType(pParamType);
    Value defaultValue(&*arg_begin, m_pCodeGenerator->getArgumentType(getSubroutine()->getParameterType(param)));
    if (pParamType->addPointer() == pArgType)
        return load(defaultValue);
    return defaultValue;
}

llvm::BasicBlock* Subroutine::new_block()
{
    return llvm::BasicBlock::Create(getContext()->m_LLVMContext, "", m_jit_function.function);
}

bool Subroutine::isCompiled() const
{
    return false; // jit_function_is_compiled((jit_function_t)m_jit_function.function) != 0;
}

llvm::Type* Subroutine::getJitIntType(size_t a_Size) const
{
    return getContext()->getJitIntType(a_Size);
}

String Subroutine::convertIntrinsicName(StringView a_strInstrinsicName)
{
    if (a_strInstrinsicName.size() == 1)
    {
        switch (a_strInstrinsicName[0])
        {
        case '+':
            return "add";
        case '-':
            return "sub";
        case '/':
            return "div";
        case '*':
            return "mul";
        }
    }
    return a_strInstrinsicName;
}

void Subroutine::toGV(void* a_pSrc, Type* a_pType, llvm::GenericValue& gv)
{
    switch (a_pType->getTypeKind())
    {
    case TypeKind::Bool:
    case TypeKind::Char:
    case TypeKind::UChar:
    case TypeKind::SChar:
        gv.IntVal = llvm::APInt(sizeof(char) * 4, *(char*)(a_pSrc));
        break;

    case TypeKind::Short:
    case TypeKind::UShort:
        gv.IntVal = llvm::APInt(sizeof(short) * 4, *(short*)(a_pSrc));
        break;

    case TypeKind::Int:
    case TypeKind::UInt:
        gv.IntVal = llvm::APInt(sizeof(int) * 4, *(int*)(a_pSrc));
        break;

    case TypeKind::Pointer:
    case TypeKind::NullPtr:
    case TypeKind::LValueReference:
    case TypeKind::RValueReference:
    case TypeKind::Array:
        gv = llvm::PTOGV(*(void**)(a_pSrc));
        break;

    case TypeKind::Long:
    case TypeKind::ULong:
        gv.IntVal = llvm::APInt(sizeof(long) * 4, *(long*)(a_pSrc));
        break;

    case TypeKind::LongLong:
    case TypeKind::ULongLong:
        gv.IntVal = llvm::APInt(sizeof(longlong) * 4, *(longlong*)(a_pSrc));
        break;

    case TypeKind::Float:
        gv.FloatVal = *(float*)(a_pSrc);
        break;

    case TypeKind::Double:
        gv.DoubleVal = *(double*)(a_pSrc);
        break;

    case TypeKind::LongDouble:
        if (sizeof(longdouble) == sizeof(longlong))
        {
            gv.IntVal = llvm::APInt(sizeof(longlong) * 4, *(longlong*)(a_pSrc));
        }
        else if (sizeof(longdouble) == sizeof(long))
        {
            gv.IntVal = llvm::APInt(sizeof(long) * 4, *(long*)(a_pSrc));
        }
        break;
    case TypeKind::Enum:
        if (sizeof(int) == a_pType->getSize())
        {
            gv.IntVal = llvm::APInt(sizeof(int) * 4, *(int*)(a_pSrc));
        }
        else if (sizeof(longlong) == a_pType->getSize())
        {
            gv.IntVal = llvm::APInt(sizeof(longlong) * 4, *(longlong*)(a_pSrc));
        }
        break;

#if PHANTOM_HAS_BUILT_IN_WCHAR_T
    case TypeKind::WChar:
        gv.IntVal = llvm::APInt(sizeof(wchar_t) * 4, *(wchar_t*)(a_pSrc));
        break;
#endif
#if PHANTOM_HAS_BUILT_IN_CHAR16_T
    case TypeKind::Char16:
        gv.IntVal = llvm::APInt(sizeof(char16_t) * 4, *(char16_t*)(a_pSrc));
        break;
#endif
#if PHANTOM_HAS_BUILT_IN_CHAR32_T
    case TypeKind::Char32:
        gv.IntVal = llvm::APInt(sizeof(char32_t) * 4, *(char32_t*)(a_pSrc));
        break;
#endif
    case TypeKind::Structure:
    {
        Structure* pStruct = static_cast<Structure*>(a_pType);
        for (auto p : pStruct->getFields())
        {
            size_t offset = p->getOffset();
            gv.AggregateVal.resize(gv.AggregateVal.size() + 1);
            toGV((byte*)a_pSrc + offset, p->getValueType(), gv.AggregateVal.back());
        }
    }
    break;
    case TypeKind::Class:
    case TypeKind::VectorClass:
    case TypeKind::MapClass:
    case TypeKind::SetClass:
    case TypeKind::StringClass:
    {
        Class* pClass = static_cast<Class*>(a_pType);
        for (auto& p : pClass->getBaseClasses())
        {
            size_t offset = p.offset;
            gv.AggregateVal.resize(gv.AggregateVal.size() + 1);
            toGV((byte*)a_pSrc + offset, p.baseClass, gv.AggregateVal.back());
        }
        for (auto p : pClass->getFields())
        {
            size_t offset = p->getOffset();
            gv.AggregateVal.resize(gv.AggregateVal.size() + 1);
            toGV((byte*)a_pSrc + offset, p->getValueType(), gv.AggregateVal.back());
        }
    }
    break;
    default:
        PHANTOM_ERROR(false, "unsupported generic value");
        break;
    }
}

void Subroutine::fromGV(void* a_pDest, Type* a_pType, const llvm::GenericValue& gv)
{
    switch (a_pType->getTypeKind())
    {
    case TypeKind::Bool:
    case TypeKind::Char:
    case TypeKind::UChar:
    case TypeKind::SChar:
        *(char*)(a_pDest) = *(char*)gv.IntVal.getRawData();
        break;

    case TypeKind::Short:
    case TypeKind::UShort:
        *(short*)(a_pDest) = *(short*)gv.IntVal.getRawData();
        break;

    case TypeKind::Int:
    case TypeKind::UInt:
        *(int*)(a_pDest) = *(int*)gv.IntVal.getRawData();
        break;

    case TypeKind::Pointer:
    case TypeKind::NullPtr:
    case TypeKind::LValueReference:
    case TypeKind::RValueReference:
    case TypeKind::Array:
        *(void**)a_pDest = llvm::GVTOP(gv);
        break;

    case TypeKind::Long:
    case TypeKind::ULong:
        *(long*)a_pDest = *(long*)gv.IntVal.getRawData();
        break;

    case TypeKind::LongLong:
    case TypeKind::ULongLong:
        *(longlong*)(a_pDest) = *(longlong*)gv.IntVal.getRawData();
        break;

    case TypeKind::Float:
        *(float*)(a_pDest) = gv.FloatVal;
        break;

    case TypeKind::Double:
        *(double*)(a_pDest) = gv.DoubleVal;
        break;

    case TypeKind::LongDouble:
        if (sizeof(longdouble) == sizeof(longlong))
        {
            *(longlong*)(a_pDest) = *(longlong*)gv.IntVal.getRawData();
        }
        else if (sizeof(longdouble) == sizeof(long))
        {
            *(long*)(a_pDest) = *(long*)gv.IntVal.getRawData();
        }
        break;
    case TypeKind::Enum:
        if (sizeof(int) == a_pType->getSize())
        {
            *(int*)(a_pDest) = *(int*)gv.IntVal.getRawData();
        }
        else if (sizeof(longlong) == a_pType->getSize())
        {
            *(longlong*)(a_pDest) = *(longlong*)gv.IntVal.getRawData();
        }
        break;

#if PHANTOM_HAS_BUILT_IN_WCHAR_T
    case TypeKind::WChar:
        *(wchar_t*)(a_pDest) = *(wchar_t*)gv.IntVal.getRawData();
        break;
#endif
#if PHANTOM_HAS_BUILT_IN_CHAR16_T
    case TypeKind::Char16:
        *(char16_t*)(a_pDest) = *(char16_t*)gv.IntVal.getRawData();
        break;
#endif
#if PHANTOM_HAS_BUILT_IN_CHAR32_T
    case TypeKind::Char32:
        *(char32_t*)(a_pDest) = *(char32_t*)gv.IntVal.getRawData();
        break;
#endif
    case TypeKind::Structure:
    {
        Structure* pStruct = static_cast<Structure*>(a_pType);
        int        i = 0;
        for (auto p : pStruct->getFields())
        {
            size_t offset = p->getOffset();
            fromGV((byte*)a_pDest + offset, p->getValueType(), gv.AggregateVal[i]);
        }
    }
    break;
    case TypeKind::Class:
    case TypeKind::VectorClass:
    case TypeKind::MapClass:
    case TypeKind::SetClass:
    case TypeKind::StringClass:
    {
        int    i = 0;
        Class* pClass = static_cast<Class*>(a_pType);
        for (auto& p : pClass->getBaseClasses())
        {
            size_t offset = p.offset;
            fromGV((byte*)a_pDest + offset, p.baseClass, gv.AggregateVal[i]);
        }

        for (auto p : pClass->getFields())
        {
            size_t offset = p->getOffset();
            fromGV((byte*)a_pDest + offset, p->getValueType(), gv.AggregateVal[i]);
        }
    }
    break;
    default:
        PHANTOM_ERROR(false, "unsupported generic value");
        break;
    }
}

#if !defined(NDEBUG)

static SmallVector<ulonglong> g_Subroutine_calls;

#endif

void Subroutine::apply(ExecutionContext& context, void** args, size_t count)
{
    PHANTOM_ASSERT_DEBUG(getSubroutine()->getReturnType() == PHANTOM_TYPEOF(void) ||
                         context.resultPointer() != nullptr);
    ulonglong funcCallAddress = 0;
    ulonglong funcApplyAddress = 0;
    PHANTOM_ASSERT_DEBUG(funcCallAddress = m_pCodeGenerator->m_ExecutionEngine->getFunctionAddress(
                         m_jit_call_function.function->getName().str()));
    PHANTOM_ASSERT_DEBUG(funcApplyAddress = m_pCodeGenerator->m_ExecutionEngine->getFunctionAddress(
                         m_jit_apply_function.function->getName().str()));

#if PHANTOM_DEBUG_LEVEL == PHANTOM_DEBUG_LEVEL_FULL
    g_Subroutine_calls.push_back(funcApplyAddress);
    g_Subroutine_calls.push_back(funcCallAddress);
#endif
    bool               rvo = getSubroutine()->isRVOCandidate();
    llvm::GenericValue values[3];
    bool               memberFunction = (getSubroutine()->asMethod() != nullptr);
    char*              new_args[64];
    size_t             argcount = count + rvo;
    PHANTOM_ASSERT(argcount < 64);
    void*  resultPointer = context.resultPointer();
    size_t c = 0;
    for (size_t i = 0; i < argcount;)
    {
        if ((c == (int)memberFunction) && rvo)
            new_args[c++] = ((char*)&resultPointer);
        else
            new_args[c++] = (char*)args[i++];
    }
    values[0].IntVal = llvm::APInt(sizeof(int) * 8, (int)argcount);
    values[1] = llvm::PTOGV(new_args);
    values[2] = llvm::PTOGV(rvo ? nullptr : resultPointer);

    m_pCodeGenerator->m_ExecutionEngine->runFunction(m_jit_apply_function.function, values);
#if PHANTOM_DEBUG_LEVEL == PHANTOM_DEBUG_LEVEL_FULL
    g_Subroutine_calls.pop_back();
    g_Subroutine_calls.pop_back();
#endif
}

String Subroutine::getMangledName(CppManglerFlags a_Flags /*= 0*/, int a_Discriminator /*= 0*/) const
{
    String prefix;
    if (a_Flags & CppManglerFlag::Thunk)
    {
        prefix += "[thunk:" + phantom::StringUtil::ToString(a_Discriminator) + "]";
    }
    if (a_Flags & CppManglerFlag::Virtual)
        prefix += "[virtual]";
    if (a_Flags & CppManglerFlag::Apply)
        prefix += "[apply]";
    if (a_Flags & CppManglerFlag::Final)
        prefix += "[final]";
    return prefix + getSubroutine()->getQualifiedDecoratedName();
}

void Subroutine::registerTemporary(Expression* a_pExpression, Value a_Temp)
{
#if 0
    int inc = s_inc;
    while (inc--)
        printf("   ");
    printf(" +++ %.*s : %p\n", PHANTOM_STRING_AS_PRINTF_ARG(a_pExpression->getValueType()->getDecoratedName()),
           a_Temp.value);
#endif
    m_Temporaries[a_pExpression] = a_Temp;
}

Value Subroutine::takeTemporary(Expression* a_pExpression)
{
    auto found = m_Temporaries.find(a_pExpression);
    PHANTOM_ASSERT(found != m_Temporaries.end());
    Value v = found->second;
#if 0
    int          inc = s_inc;
    while (inc--)
        printf("   ");
    printf(" --- %.*s : %p\n", PHANTOM_STRING_AS_PRINTF_ARG(a_pExpression->getValueType()->getDecoratedName()),
           found->second.value);
#endif
    // m_Temporaries.erase(found);
    return v;
}

llvm::Type* Subroutine::toJitType(Type* a_pType) const
{
    return getContext()->toJitType(a_pType);
}

llvm::FunctionType* Subroutine::toJitFunctionCallType(FunctionType* a_pFunctionType, ClassType* a_pThisType) const
{
    return getContext()->toJitFunctionCallType(a_pFunctionType, a_pThisType);
}

unsigned Subroutine::toJitABI(ABI abi) const
{
    return getContext()->toJitABI(abi);
}

void Subroutine::_createAllocaHeader()
{
    // m_AllocaInsertingPoint = m_jit_function.function->getEntryBlock().begin();
    // printf("entry block : %p", &m_jit_function.function->getEntryBlock());
}

void Subroutine::_destroyAllocaHeader() {}

void Subroutine::ApplyFunctionAttributes(llvm::Function* pFunc, bool a_UnwindTable, bool a_Naked /* = false*/,
                                         bool a_Align /*=true*/)
{
    auto  idx = llvm::AttributeList::FunctionIndex;
    auto& ctx = pFunc->getContext();

    pFunc->addAttribute(idx, llvm::Attribute::OptimizeNone);
    pFunc->addAttribute(idx, llvm::Attribute::NoInline);
    if (a_Align)
        pFunc->addAttribute(
        llvm::AttributeList::FunctionIndex,
        llvm::Attribute::get(pFunc->getContext(), llvm::Attribute::StackAlignment,
                             16)); // Win64 requires stack aligned on 16 bytes (for xmm instructions among others)

    // ensure good debugging experience in VisualStudio
    if (a_UnwindTable && !a_Naked)
        pFunc->addAttribute(idx, llvm::Attribute::UWTable);
    else
        pFunc->addAttribute(idx, llvm::Attribute::NoUnwind);

    if (a_Naked)
        pFunc->addAttribute(idx, llvm::Attribute::Naked);
}

} // namespace jit
} // namespace phantom

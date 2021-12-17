#include "Context.h"

#include "CodeGeneratorPrivate.h"
#include "phantom/lang/MethodPointer.h"

#include <phantom/lang/Block.h>
#include <phantom/lang/FunctionType.h>
#include <phantom/lang/Pointer.h>

namespace phantom
{
namespace jit
{
using namespace phantom::lang;

llvm::LLVMContext* Context::Main()
{
    static llvm::LLVMContext main;
    return &main;
}

Context::Context(CodeGeneratorPrivate* a_pCodeGeneratorPrivate, llvm::LLVMContext& context, llvm::Module* a_pLLVMModule)
    : m_pCodeGeneratorPrivate(a_pCodeGeneratorPrivate),
      m_LLVMContext(context),
      m_pDebugContext(a_pCodeGeneratorPrivate->m_pCodeGenerator->getOption("configuration") == "Debug"
                      ? (new (&m_DebugContextMem) DebugContext(this, a_pLLVMModule))
                      : nullptr)
{
    toJitType(PHANTOM_TYPEOF(bool));
    toJitType(PHANTOM_TYPEOF(char));
    toJitType(PHANTOM_TYPEOF(schar));
    toJitType(PHANTOM_TYPEOF(uchar));
    toJitType(PHANTOM_TYPEOF(short));
    toJitType(PHANTOM_TYPEOF(ushort));
    toJitType(PHANTOM_TYPEOF(int));
    toJitType(PHANTOM_TYPEOF(uint));
    toJitType(PHANTOM_TYPEOF(long));
    toJitType(PHANTOM_TYPEOF(ulong));
    toJitType(PHANTOM_TYPEOF(longlong));
    toJitType(PHANTOM_TYPEOF(ulonglong));
    toJitType(PHANTOM_TYPEOF(float));
    toJitType(PHANTOM_TYPEOF(double));
    toJitType(PHANTOM_TYPEOF(longdouble));
#if PHANTOM_HAS_BUILT_IN_WCHAR_T
    toJitType(PHANTOM_TYPEOF(wchar_t));
#endif
#if PHANTOM_HAS_BUILT_IN_CHAR16_T
    toJitType(PHANTOM_TYPEOF(char16_t));
#endif
#if PHANTOM_HAS_BUILT_IN_CHAR32_T
    toJitType(PHANTOM_TYPEOF(char32_t));
#endif
}

Context::~Context()
{
    if (m_pDebugContext)
        m_pDebugContext->~DebugContext();
}

llvm::Type* Context::_toJitStruct(ClassType* a_pClassType)
{
    size_t                   size = a_pClassType->getSize();
    SmallVector<llvm::Type*> dataMemberTypes;
    if (a_pClassType->isAbstract())
    {
        dataMemberTypes.resize(size, llvm::Type::getInt8Ty(m_LLVMContext));
        return llvm::StructType::create(m_LLVMContext, toArrayRef(dataMemberTypes));
    }

    switch (a_pClassType->getAlignment())
    {
#pragma message(PHANTOM_TODO "find a way to fix the 0 alignment of abstract classes")
    case 0: // abstract classes have 0 alignment
    case 1:
    {
        dataMemberTypes.resize(size, llvm::Type::getInt8Ty(m_LLVMContext));
        break;
    }
    case 2:
    {
        dataMemberTypes.resize(size / 2, llvm::Type::getInt16Ty(m_LLVMContext));
        break;
    }
    case 4:
    {
        dataMemberTypes.resize(size / 4, llvm::Type::getInt32Ty(m_LLVMContext));
        break;
    }
    case 8:
    {
        dataMemberTypes.resize(size / 8, llvm::Type::getInt64Ty(m_LLVMContext));
        break;
    }
    case 16:
    {
        dataMemberTypes.resize(size / 16, llvm::Type::getInt128Ty(m_LLVMContext));
        break;
    }
    default:
        PHANTOM_ASSERT(false);
    }
    return llvm::StructType::create(m_LLVMContext, toArrayRef(dataMemberTypes));
}

llvm::Type* Context::_toJitBool(size_t a_uiSize)
{
    switch (a_uiSize)
    {
    case 1:
        return llvm::Type::getInt8Ty(m_LLVMContext);
    case 2:
        return llvm::Type::getInt16Ty(m_LLVMContext);
    case 4:
        return llvm::Type::getInt32Ty(m_LLVMContext);
    default:
        PHANTOM_ASSERT(false);
    }
    return 0;
}

llvm::Type* Context::_toJitType(Type* a_pType)
{
    switch (a_pType->getTypeKind())
    {
    case TypeKind::Void:
        return llvm::Type::getVoidTy(m_LLVMContext);
    case TypeKind::Bool:
        return llvm::Type::getInt1Ty(m_LLVMContext);
    case TypeKind::Char:
    case TypeKind::Int8:
    case TypeKind::UInt8:
        return llvm::Type::getInt8Ty(m_LLVMContext);
    case TypeKind::Int16:
#if PHANTOM_HAS_BUILT_IN_WCHAR_T
    case TypeKind::WChar:
#endif
#if PHANTOM_HAS_BUILT_IN_CHAR16_T
    case TypeKind::Char16:
#endif
    case TypeKind::UInt16:
        return llvm::Type::getInt16Ty(m_LLVMContext);
    case TypeKind::Int32:
#if PHANTOM_HAS_BUILT_IN_CHAR32_T
    case TypeKind::Char32:
#endif
    case TypeKind::UInt32:
#if PHANTOM_SIZE_OF_LONG == 4
    case TypeKind::Long:
    case TypeKind::ULong:
        return llvm::Type::getInt32Ty(m_LLVMContext);
#else
    case TypeKind::Long:
    case TypeKind::ULong:
#endif
    case TypeKind::LongLong:
    case TypeKind::ULongLong:
        return llvm::Type::getInt64Ty(m_LLVMContext);

    case TypeKind::Float:
        return llvm::Type::getFloatTy(m_LLVMContext);

    case TypeKind::Double:
        return llvm::Type::getDoubleTy(m_LLVMContext);

    case TypeKind::LongDouble:
        if (sizeof(longdouble) == 8)
            return llvm::Type::getDoubleTy(m_LLVMContext);
        else if (sizeof(longdouble) == 16)
            return llvm::Type::getFP128Ty(m_LLVMContext);
        else
            break;

    case TypeKind::Enum:
        return toJitType(a_pType->getUnderlyingType());

    case TypeKind::LValueReference:
    case TypeKind::RValueReference:
    case TypeKind::Pointer:
    {
        Type* pSubType = a_pType->getUnderlyingType();
        switch (pSubType->getTypeKind())
        {
        case TypeKind::Void:     // void*
        case TypeKind::Function: // function pointer
            return llvm::Type::getInt8Ty(m_LLVMContext)->getPointerTo();
        default:
            return toJitType(static_cast<Pointer*>(a_pType)->getPointeeType())->getPointerTo();
        }
    }
    break;

    case TypeKind::FunctionPointer:
    case TypeKind::NullPtr:
        return llvm::Type::getInt8Ty(m_LLVMContext)->getPointerTo();

    case TypeKind::FieldPointer:
        return _toJitType(PHANTOM_TYPEOF(size_t));
    case TypeKind::MethodPointer:
    {
        MethodPointer* p = static_cast<MethodPointer*>(a_pType);
        return toJitFunctionCallType(p->getFunctionType(), p->getObjectType())->getPointerTo();
    }
    case TypeKind::Array:
        return llvm::ArrayType::get(toJitType(static_cast<Array*>(a_pType)->getItemType()),
                                    static_cast<Array*>(a_pType)->getItemCount());

    case TypeKind::Structure:
    case TypeKind::Union:
    case TypeKind::Class:
    case TypeKind::VectorClass:
    case TypeKind::SetClass:
    case TypeKind::MapClass:
    case TypeKind::ArrayClass:
    case TypeKind::StringClass:
        return _toJitStruct(a_pType->asClassType());

    case TypeKind::Function:
    default:
        break;
    }
    PHANTOM_ASSERT(false, "Given phantom type cannot be converted to jit type");
    return llvm::Type::getVoidTy(m_LLVMContext);
}

llvm::Type* Context::toJitType(Type* a_pType)
{
    a_pType = a_pType->removeAllQualifiers();
    auto found = m_toJitTypeMap.find(a_pType);
    if (found != m_toJitTypeMap.end())
        return found->second;
    llvm::Type* jitType = _toJitType(a_pType);
    m_fromJitTypeMap[jitType] = a_pType;
    return (m_toJitTypeMap[a_pType] = jitType);
}

llvm::Type* Context::toJitArgumentType(Type* a_pType)
{
    switch (GetArgumentPassingMode(a_pType))
    {
    case ArgumentPassingMode::ByInt:
        return getJitIntType(a_pType->getSize());
    case ArgumentPassingMode::ByPointer:
        return toJitType(a_pType)->getPointerTo();
    default:
        if (Array* pArray = a_pType->asArray())
        {
            return toJitType(pArray->getUnderlyingType()->removeAllExtents()->makePointer());
        }
        return toJitType(a_pType);
    }
}

llvm::CallingConv::ID Context::toJitABI(ABI abi)
{
    switch (abi)
    {
    case ABI::ThisCall:
        return llvm::CallingConv::X86_ThisCall;
    case ABI::Win64:
        return llvm::CallingConv::Win64;
    case ABI::FastCall:
        return llvm::CallingConv::X86_FastCall;
    case ABI::CDecl:
        return llvm::CallingConv::C;
    case ABI::StdCall:
        return llvm::CallingConv::X86_StdCall;
    default:
        PHANTOM_ASSERT(false, "defaulting to C calling convention");
        return llvm::CallingConv::C;
    }
}

llvm::Type* Context::getJitIntType(size_t a_Size) const
{
    switch (a_Size)
    {
    case 1:
        return llvm::Type::getInt8Ty(m_LLVMContext);
    case 2:
        return llvm::Type::getInt16Ty(m_LLVMContext);
    case 4:
        return llvm::Type::getInt32Ty(m_LLVMContext);
    case 8:
        return llvm::Type::getInt64Ty(m_LLVMContext);
    }
    PHANTOM_ASSERT(false);
    return nullptr;
}

llvm::FunctionType* Context::toJitFunctionCallType(FunctionType* a_pFunctionType, ClassType* a_pThisType)
{
    bool                     isRVOCandidate = a_pFunctionType->isRVOCandidate();
    bool                     isThisCall = (a_pThisType != nullptr);
    size_t                   count = a_pFunctionType->getParameterTypeCount() + isThisCall + isRVOCandidate;
    SmallVector<llvm::Type*> params;
    params.resize(count);
    {
        size_t i = (size_t)isThisCall + (size_t)isRVOCandidate;
        for (; i < count; ++i)
        {
            Type* pParamType = a_pFunctionType->getParameterType(i - (size_t)isThisCall - (size_t)isRVOCandidate);

            // Win 64 call convention
            // TODO : apply other calling conventions
            // TODO : factorize some code with CodeGenerator 'compileArgument' function
            params[i] = toJitArgumentType(pParamType);
        }
        if (isRVOCandidate)
        {
        }
    }
    if (isThisCall)
    {
        params[0] = toJitType(a_pThisType)->getPointerTo();
    }
    if (isRVOCandidate)
    {
        params[isThisCall] = toJitType(a_pFunctionType->getReturnType())->getPointerTo();
        return llvm::FunctionType::get(params[isThisCall], toArrayRef(params), false);
    }

    auto pRetType = a_pFunctionType->getReturnType();
    if (Array* pArray = pRetType->removeAddress()->asArray()) // T(&)[N]...[M] or T(*)[N]...[M] or T[N]...[M] => T*
        pRetType = pArray->getUnderlyingType()->removeAllExtents()->makePointer();

    return llvm::FunctionType::get(toJitType(pRetType), toArrayRef(params), false);
}

llvm::FunctionType* Context::toJitFunctionType(FunctionType* a_pFunctionType, ClassType* a_pThisType)
{
    bool                     isThisCall = (a_pThisType != nullptr);
    size_t                   count = a_pFunctionType->getParameterTypeCount() + isThisCall;
    SmallVector<llvm::Type*> params;
    params.resize(count);
    {
        size_t i = (size_t)isThisCall;
        for (; i < count; ++i)
        {
            params[i] = toJitType(a_pFunctionType->getParameterType(i - (size_t)isThisCall));
        }
    }
    if (a_pThisType)
    {
        params[0] = toJitType(a_pThisType)->getPointerTo();
    }
    return llvm::FunctionType::get(toJitType(a_pFunctionType->getReturnType()), toArrayRef(params), false);
}

} // namespace jit
} // namespace phantom

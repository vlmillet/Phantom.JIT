// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

#include "prerequisites.h"
#if PHANTOM_OPERATING_SYSTEM == PHANTOM_OPERATING_SYSTEM_ORBIS
#    define getenv(...) string(0).c_str()
#endif
#if defined(_MSC_VER)
#    pragma warning(push, 0)
#elif defined(__clang__)
#    pragma clang diagnostic push
#    pragma clang diagnostic ignored "-Wall"
#    pragma clang diagnostic ignored "-Wextra"
#endif
#include <llvm/DebugInfo/CodeView/GUID.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/TargetSelect.h>
#if defined(_MSC_VER)
#    pragma warning(pop)
#elif defined(__clang__)
#    pragma clang diagnostic pop
#endif
#if PHANTOM_OPERATING_SYSTEM == PHANTOM_OPERATING_SYSTEM_ORBIS
#    undef getenv
#endif
#include "CodeGeneratorPrivate.h"
#include "phantom/lang/Compiler.h"
#include "phantom/utils/Path.h"

#include <phantom/plugin>
#if PHANTOM_OPERATING_SYSTEM == PHANTOM_OPERATING_SYSTEM_WINDOWS
#    include <windows.h>
#endif
#include "Context.h"
#include "autoload.h"

#include <llvm/Support/ManagedStatic.h>

namespace phantom
{
namespace jit
{
using namespace phantom::lang;

phantom::String GUIDToText(llvm::codeview::GUID const& guid)
{
    static const char* Lookup = "0123456789ABCDEF";
    phantom::String    res;
    for (int i = 0; i < 16;)
    {
        uint8_t Byte = guid.Guid[i];
        uint8_t HighNibble = (Byte >> 4) & 0xF;
        uint8_t LowNibble = Byte & 0xF;
        res += Lookup[HighNibble];
        res += Lookup[LowNibble];
        ++i;
        if (i >= 4 && i <= 10 && i % 2 == 0)
            res += '-';
    }
    return res;
}

PHANTOM_STRUCT(Value){};
PHANTOM_STRUCT(Label){};

namespace
{
phantom::String g_session_id;
}

PHANTOM_EXPORT_PHANTOM_JIT phantom::String sessionFolder()
{
    if (g_session_id.empty())
    {
        char buffer[256];
        phantom::random::str(buffer);
    }
    return phantom::Path::TempFolder().childPath("Phantom.JIT.Sessions").childPath(g_session_id).genericString();
}

PHANTOM_EXPORT_PHANTOM_JIT void setSessionId(phantom::String _session_id)
{
    g_session_id = _session_id;
}

ArgumentPassingMode GetArgumentPassingMode(Type* a_pType)
{
    if (a_pType->asClassType())
    {
        if (Class* pClass = a_pType->asClass())
        {
            if (auto pCCtor = pClass->getCopyConstructor())
            {
                if (not pCCtor->testFlags(PHANTOM_R_FLAG_IMPLICIT)     // user declared
                    && not pCCtor->testModifiers(Modifier::Defaulted)) // not defaulted
                    return ArgumentPassingMode::ByPointer;             // => pass by pointer
            }
            else
            {
                // no copy constructor at all => pass by pointer
                return ArgumentPassingMode::ByPointer; // => pass by pointer
            }
        }

        if (a_pType->getSize() <= sizeof(void*) && ((a_pType->getSize() % 2) == 0))
        {
            return ArgumentPassingMode::ByInt;
        }
        return ArgumentPassingMode::ByPointer;
    }
    return ArgumentPassingMode::Default;
}

void CodeGeneratorData::setCodeGenerator(CodeGeneratorPrivate* a_pCodeGenerator)
{
    if (m_pCodeGenerator == a_pCodeGenerator)
        return;
    compilerAboutToBeChanged(a_pCodeGenerator);
    if (m_pCodeGenerator)
        m_pCodeGenerator->_removeData(this);
    m_pCodeGenerator = a_pCodeGenerator;
    if (m_pCodeGenerator)
        m_pCodeGenerator->_addData(this);
    compilerChanged(a_pCodeGenerator);
}

bool areCompatibles(llvm::Type* type, Type* a_pType)
{
    a_pType = a_pType->removeAllQualifiers();
    if (a_pType == PHANTOM_TYPEOF(bool))
        return type->getIntegerBitWidth() == 1;
    if (type->getTypeID() == llvm::Type::IntegerTyID)
    {
        if (a_pType->asIntegralType())
        {
            return ((a_pType->getSize() * 8) == type->getIntegerBitWidth());
        }
        return false;
    }
    return (type->getTypeID() == llvm::Type::DoubleTyID) && a_pType == PHANTOM_TYPEOF(double) ||
    (type->getTypeID() == llvm::Type::FloatTyID) && a_pType == PHANTOM_TYPEOF(float) ||
    (type->getTypeID() == llvm::Type::PointerTyID) &&
    (a_pType->asAddressType() || a_pType->asArray() || a_pType->asFunctionPointer() || a_pType->asMethodPointer() ||
     a_pType == PHANTOM_TYPEOF(std::nullptr_t)) ||
    (type->getTypeID() == llvm::Type::ArrayTyID) && a_pType->asArray() ||
    (type->getTypeID() == llvm::Type::StructTyID) && a_pType->asClassType() ||
    (type->getTypeID() == llvm::Type::VoidTyID) && a_pType == PHANTOM_TYPEOF(void);
}

Value::Value() : value(0), type(0) {}

Value::Value(llvm::Value* a_Value, Type* a_pType)
    : value(a_Value),
      type(
      a_pType->removeAllConst()) /// we don't care about const for code generation (only a compile time check modifier)
{
    if (type->getTypeKind() == TypeKind::Enum)
        type = static_cast<Enum*>(type)->getUnderlyingIntType();
    PHANTOM_ASSERT(areCompatibles(a_Value->getType(), type));
}

void Value::setType(Type* a_type)
{
    PHANTOM_ASSERT(a_type);
    type = a_type->removeAllQualifiers();
}

Label::Label() : block(0) {}

bool Label::isUndefined() const
{
    return block == nullptr;
}

FunctionEntry::FunctionEntry() : function(0), builder(0) {}

void FunctionEntry::set(llvm::Function* f, llvm::DISubprogram* di)
{
    function = (f);
    llvm::BasicBlock* pBlock = llvm::BasicBlock::Create(f->getContext(), "", f);
    builder = (new llvm::IRBuilder<>(pBlock));
    function->setSubprogram(di);
}

void* FunctionEntry::getAddress(CodeGeneratorPrivate* a_pCodeGen) const
{
    if (cachedAddress == 0)
    {
        auto n = function->getName().str();
        cachedAddress = (void*)a_pCodeGen->m_ExecutionEngine->getFunctionAddress(n);
    }
    return cachedAddress;
}

} // namespace jit
} // namespace phantom

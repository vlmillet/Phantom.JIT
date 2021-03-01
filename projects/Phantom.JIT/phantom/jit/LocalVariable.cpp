// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

/* ******************* Includes ****************** */
#include "LocalVariable.h"

#include "Method.h"
#include "phantom/lang/Parameter.h"
#pragma warning(push, 0)
#include <llvm/IR/DerivedTypes.h>
#pragma warning(pop)
/* *********************************************** */
namespace phantom
{
namespace jit
{
using namespace phantom::lang;

LocalVariable::LocalVariable(Subroutine* a_pJitSubroutine, lang::LocalVariable* a_pLocalVariable, Value a_Value)
    : CodeGeneratorData(a_pLocalVariable), m_pJitSubroutine(a_pJitSubroutine)
{
    // passing arg by pointer (struct)

    Parameter* asParam = a_pLocalVariable->asParameter();
    if (asParam && GetArgumentPassingMode(a_pLocalVariable->getValueType()) == ArgumentPassingMode::ByPointer)
    {
        m_Value = a_Value;
        if (a_pJitSubroutine->getDebugContext())
        {
            a_pJitSubroutine->getOrCreateAlloca(
            a_pLocalVariable, a_pLocalVariable->getValueType()->addPointer(), 0, [&](Value _debugValue) {
                if (!a_Value.isNull())
                {
                    a_pJitSubroutine->store(
                    _debugValue,
                    a_Value); // copy the argument in the stack allocated memory for future mutations
                }
                a_pJitSubroutine->createLocalVariableDebugInfo(a_pLocalVariable, _debugValue);
            });
        }
    }
    else
    {
        Type* pStorageType = a_pLocalVariable->getValueType();
        if (asParam && pStorageType->asArray())
        {
            pStorageType = pStorageType->getUnderlyingType()->removeAllExtents()->addPointer();
        }
        m_Value = a_pJitSubroutine->getOrCreateAlloca(a_pLocalVariable, pStorageType, 0, [&](Value _debugValue) {
            if (!a_Value.isNull())
            {
                a_pJitSubroutine->store(
                _debugValue,
                a_Value); // copy the argument in the stack allocated memory for future mutations
            }
            if (a_pJitSubroutine->getDebugContext())
                a_pJitSubroutine->createLocalVariableDebugInfo(a_pLocalVariable, _debugValue);
        });
    }
}

Value LocalVariable::store(Value value) const
{
    PHANTOM_ASSERT(m_pJitSubroutine->toJitType(value.type)->getPointerTo() ==
                   m_pJitSubroutine->toJitType(m_Value.type));
    return getJitSubroutine()->store(m_Value, value);
}

Value LocalVariable::address() const
{
    return m_Value;
}

void LocalVariable::setupFrame()
{
    // getLocalVariable()->setNativeFrameOffset(m_Value.getFrameOffset());
}

Value LocalVariable::load() const
{
    return m_pJitSubroutine->load(m_Value);
}

lang::LocalVariable* LocalVariable::getLocalVariable() const
{
    return static_cast<lang::LocalVariable*>(m_pElement);
}

} // namespace jit
} // namespace phantom

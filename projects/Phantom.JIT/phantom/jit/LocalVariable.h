// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

#pragma once

/* ****************** Includes ******************* */
#include "prerequisites.h"
/* **************** Declarations ***************** */
/* *********************************************** */

namespace phantom
{
namespace jit
{
class PHANTOM_EXPORT_PHANTOM_JIT LocalVariable : public CodeGeneratorData
{
public:
    LocalVariable(Subroutine* a_pJitSubroutine, lang::LocalVariable* a_pLocalVariable, Value a_Value);
    ~LocalVariable(void) override {}

    inline Subroutine* getJitSubroutine() const { return m_pJitSubroutine; }

    Value getValue() const { return m_Value; }

    virtual Value store(Value value) const;
    virtual Value load() const;
    virtual Value address() const;

    void                 setupFrame();
    lang::LocalVariable* getLocalVariable() const;

protected:
    Value       m_Value;
    Subroutine* m_pJitSubroutine;
};

} // namespace jit
} // namespace phantom

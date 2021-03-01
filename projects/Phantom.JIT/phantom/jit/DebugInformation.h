// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

#pragma once

#include "prerequisites.h"

#include <phantom/lang/DebugInformation.h>

namespace phantom
{
namespace jit
{
class DebugInformationPrivate;
class DebugInformation : public lang::DebugInformation
{
    friend class DebugInformationPrivate;
    friend class CodeGenerator;
    friend struct CodeGeneratorPrivate;

public:
    DebugInformation();
    ~DebugInformation();

    virtual lang::CodeLocation getCodeLocationForAddress(void* a_pAddress) const override;
    virtual void*              getAddressForCodeLocation(lang::CodeLocation a_CodeLocation) const override;

    virtual bool findStepOverInstructions(byte* a_pReturnAddress, byte* a_pAddress, byte*& a_pNext,
                                          byte*& a_pConditionalBranchTarget) const override;
    virtual bool findStepIntoInstruction(byte* a_pAddress, byte*& a_pCallAddress,
                                         void* a_pGenericThreadContext) const override;

    CodeGenerator* getCodeGenerator() const;

    virtual byte* getLocalVariableAddress(lang::LocalVariable* a_pLocalVariable,
                                          size_t               a_iDebuggerFrameIndex) const override;

private:
    void begin();
    void abort();
    void end();

private:
    DebugInformationPrivate* m_private;
};
} // namespace jit
} // namespace phantom

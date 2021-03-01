// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

#pragma once

/* ****************** Includes ******************* */
#include "prerequisites.h"

#include <phantom/lang/CodeGenerator.h>
#include <phantom/utils/Flags.h>
/* **************** Declarations ***************** */
/* *********************************************** */

namespace llvm
{
class Module;
class ExecutionEngine;
} // namespace llvm
namespace phantom
{
namespace jit
{
class Subroutine;
class CaseLabel;
class Context;
struct CodeGeneratorPrivate;
class DebugContext;

class PHANTOM_EXPORT_PHANTOM_JIT CodeGenerator : public lang::CodeGenerator
{
public:
    friend struct CodeGeneratorPrivate;
    friend class DebugInformation;
    friend class DebugInformationPrivate;
    friend class Subroutine;
    friend class Method;
    friend class DebugContext;

    enum EPass
    {
        e_Pass_Subroutines,
        e_Pass_Blocks,
        e_Pass_CodeGen,
        e_Pass_Count
    };

    CodeGenerator();
    ~CodeGenerator();

    virtual void begin() override;
    virtual void abort() override;
    virtual void end() override;

    uint getPass() const { return m_uiPass; }

    virtual lang::DebugInformation* createDebugInformation() override;
    virtual void                    destroyDebugInformation(lang::DebugInformation*) override;

    virtual lang::ExecutionContext* createExecutionContext() const;
    virtual void                    destroyExecutionContext(lang::ExecutionContext* a_pContext) const;

protected:
    Error compile(uint a_iPass) override;
    void  onOutdated() override;

private:
    uint                  m_uiPass = 0;
    CodeGeneratorPrivate* m_private;
    CodeGeneratorPrivate* m_prev_private = nullptr;
};

} // namespace jit
} // namespace phantom

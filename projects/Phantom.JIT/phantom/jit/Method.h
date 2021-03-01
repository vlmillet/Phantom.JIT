// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

#pragma once

/* ****************** Includes ******************* */
#include "Subroutine.h"
/* **************** Declarations ***************** */
/* *********************************************** */

namespace phantom
{
namespace jit
{
class PHANTOM_EXPORT_PHANTOM_JIT Method : public Subroutine
{
public:
    Method(lang::Method* a_pMethod);
    ~Method() override;

    void compilerAboutToBeChanged(CodeGeneratorPrivate* a_pCodeGenerator);
    void compilerChanged(CodeGeneratorPrivate* a_pCodeGenerator);

    lang::Method* getMethod() const;

    phantom::MethodClosure getGenericMethodPointer() const;

    void compileVTableIndirectionFunction();
    void compileThisAdjustementThunk(std::ptrdiff_t a_iThisOffset) const;

protected:
    void                                            codeGen() override;
    void                                            setupFunctionPointers() override;
    typedef SmallMap<std::ptrdiff_t, FunctionEntry> vtable_indirection_function_map;
    mutable vtable_indirection_function_map         m_ThisAdjustmentThunks;
    FunctionEntry                                   m_jit_virtual_indirection_function;
};

} // namespace jit
} // namespace phantom

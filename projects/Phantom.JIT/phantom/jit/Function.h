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
class PHANTOM_EXPORT_PHANTOM_JIT Function : public Subroutine
{
public:
    Function(lang::Function* a_pFunction);
    ~Function(void) override;
};

} // namespace jit
} // namespace phantom

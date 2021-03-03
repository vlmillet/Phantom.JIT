#pragma once

// haunt {

// clang-format off

#include "CodeGenerator.h"

#if defined(_MSC_VER)
#   pragma warning(push, 0)
#elif defined(__clang__)
#   pragma clang diagnostic push
#   pragma clang diagnostic ignored "-Wall"
#   pragma clang diagnostic ignored "-Wextra"
#endif

#include <phantom/namespace>
#include <phantom/package>
#include <phantom/source>
#include <phantom/class>
#include <phantom/enum>
#include <phantom/method>
#include <phantom/constructor>
#include <phantom/friend>

namespace phantom {
namespace jit {
PHANTOM_PACKAGE("phantom.jit")
    PHANTOM_SOURCE("CodeGenerator")

        #if PHANTOM_NOT_TEMPLATE
        PHANTOM_CLASS(CodeGenerator)
        {
            using EPass = typedef_<_::EPass>;
            this_()
            .inherits<::phantom::lang::CodeGenerator>()
        .public_()
            .enum_<EPass>().values({
                {"e_Pass_Subroutines",_::e_Pass_Subroutines},
                {"e_Pass_Blocks",_::e_Pass_Blocks},
                {"e_Pass_CodeGen",_::e_Pass_CodeGen},
                {"e_Pass_Count",_::e_Pass_Count}})
            .end()
            .constructor<void()>()
            .method<void(), virtual_|override_>("begin", &_::begin)
            .method<void(), virtual_|override_>("abort", &_::abort)
            .method<void(), virtual_|override_>("end", &_::end)
            .method<uint() const>("getPass", &_::getPass)
            .method<::phantom::lang::DebugInformation *(), virtual_|override_>("createDebugInformation", &_::createDebugInformation)
            .method<void(::phantom::lang::DebugInformation *), virtual_|override_>("destroyDebugInformation", &_::destroyDebugInformation)
            .method<::phantom::lang::ExecutionContext *() const, virtual_>("createExecutionContext", &_::createExecutionContext)
            .method<void(::phantom::lang::ExecutionContext *) const, virtual_>("destroyExecutionContext", &_::destroyExecutionContext)
        
        .protected_()
            .method<Error(uint), virtual_|override_>("compile", &_::compile)
            .method<void(), virtual_|override_>("onOutdated", &_::onOutdated)
            ;
        }
        #endif // PHANTOM_NOT_TEMPLATE
    PHANTOM_END("CodeGenerator")
PHANTOM_END("phantom.jit")
}
}

#if defined(_MSC_VER)
#   pragma warning(pop)
#elif defined(__clang__)
#   pragma clang diagnostic pop
#endif

// clang-format on

// haunt }

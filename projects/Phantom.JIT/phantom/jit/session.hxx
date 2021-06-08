#pragma once

// haunt {

// clang-format off

#include "session.h"

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
#include <phantom/function>

#include <phantom/template-only-push>

#include <phantom/utils/SmallString.hxx>
#include <phantom/utils/StringView.hxx>

#include <phantom/template-only-pop>

namespace phantom {
namespace jit {
PHANTOM_PACKAGE("phantom.jit")
    PHANTOM_SOURCE("session")

        #if PHANTOM_NOT_TEMPLATE
        PHANTOM_REGISTER(Functions) { this_().function<::phantom::String()>("sessionFolder", sessionFolder);}
        PHANTOM_REGISTER(Functions) { this_().function<void(::phantom::StringView)>("setSessionId", setSessionId)({"_id"});}
        #endif // PHANTOM_NOT_TEMPLATE
    PHANTOM_END("session")
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

#pragma once

#include <phantom/lang/plugin.h>
#include <phantom/plugin.h>

#if !defined(PHANTOM_STATIC_LINK_PHANTOM_JIT)
#    if defined(PHANTOM_LIB_PHANTOM_JIT)
#        define PHANTOM_EXPORT_PHANTOM_JIT PHANTOM_VISIBILITY_EXPORT
#    else
#        define PHANTOM_EXPORT_PHANTOM_JIT PHANTOM_VISIBILITY_IMPORT
#    endif

#else
#    define PHANTOM_EXPORT_PHANTOM_JIT

#endif

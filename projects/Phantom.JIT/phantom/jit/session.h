#pragma once
#include "plugin.h"

#include <phantom/utils/String.h>
#include <phantom/utils/StringView.h>

namespace phantom
{
namespace jit
{
PHANTOM_EXPORT_PHANTOM_JIT phantom::String sessionFolder();
PHANTOM_EXPORT_PHANTOM_JIT void            setSessionId(phantom::StringView _id);
} // namespace jit
} // namespace phantom

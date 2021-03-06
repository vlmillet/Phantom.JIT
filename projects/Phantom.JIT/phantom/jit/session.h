#pragma once
#include "plugin.h"

#include <phantom/utils/String.h>

namespace phantom
{
namespace jit
{
PHANTOM_EXPORT_PHANTOM_JIT phantom::String sessionFolder();
PHANTOM_EXPORT_PHANTOM_JIT void            setSessionId(phantom::String _id);
} // namespace jit
} // namespace phantom

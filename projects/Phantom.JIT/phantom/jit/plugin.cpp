// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

#include "plugin.h"

#include "Context.h"

#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/TargetSelect.h>
#include <phantom/plugin>

namespace phantom
{
namespace jit
{
void load()
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetDisassembler();
    Context::Main();
}

void unload()
{
    llvm::llvm_shutdown();
}
PHANTOM_EXPORT_PHANTOM_JIT void autoload() {}

} // namespace jit
} // namespace phantom

PHANTOM_PLUGIN("Phantom.JIT", PHANTOM_PLUGIN_REGISTER_CLASS_MEMBERS_ON_ACCESS, phantom::jit::load,
               phantom::jit::unload);

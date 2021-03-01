// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

/* ******************* Includes ****************** */
#include "CodeGenerator.h"

#include "CodeGeneratorPrivate.h"
#include "DebugInformation.h"

#include <llvm/JITPDB/JITPDBMemoryManager.h>
#include <phantom/lang/Block.h>
#include <phantom/lang/Compiler.h>
#include <phantom/lang/Module.h>

namespace phantom
{
namespace jit
{
using namespace phantom::lang;

CodeGenerator::CodeGenerator() : lang::CodeGenerator(e_Pass_Count), m_private(nullptr) {}

CodeGenerator::~CodeGenerator()
{
    phantom::delete_<CodeGeneratorPrivate>(m_private);
}

void CodeGenerator::begin()
{
    // WARNING : we ignore source parameters as MCJIT cannot rebuild a part of the module, we
    // rebuild every source :(

    m_prev_private = m_private;
    m_private = phantom::new_<CodeGeneratorPrivate>(this);
    static_cast<DebugInformation*>(phantom::lang::Compiler::Get()->getDebugInformation(getModule()))->begin();
}

void CodeGenerator::abort()
{
    phantom::delete_<CodeGeneratorPrivate>(m_private);
    m_private = m_prev_private;
    m_prev_private = nullptr;
    static_cast<DebugInformation*>(phantom::lang::Compiler::Get()->getDebugInformation(getModule()))->abort();
}

void CodeGenerator::end()
{
    if (m_prev_private)
        phantom::delete_<CodeGeneratorPrivate>(m_prev_private);
    m_prev_private = nullptr;
    // only set closures if codegen succeeded
    for (auto it = m_private->m_Blocks.begin(); it != m_private->m_Blocks.end(); ++it)
    {
        it->second->setupFunctionPointers();
    }
    for (auto it = m_private->m_Blocks.begin(); it != m_private->m_Blocks.end(); ++it)
    {
        it->second->release();
    }

    static_cast<DebugInformation*>(phantom::lang::Compiler::Get()->getDebugInformation(getModule()))->end();
}

lang::DebugInformation* CodeGenerator::createDebugInformation()
{
    return phantom::new_<DebugInformation>();
}

void CodeGenerator::destroyDebugInformation(lang::DebugInformation* a_pDI)
{
    phantom::delete_<DebugInformation>(static_cast<DebugInformation*>(a_pDI));
}

ExecutionContext* CodeGenerator::createExecutionContext() const
{
    return nullptr; // Phantom.JIT uses jit apply function pointer, no execution context needed as for an interpreter
}

void CodeGenerator::destroyExecutionContext(ExecutionContext*) const {}

CodeGenerator::Error CodeGenerator::compile(uint a_uiPass)
{
    m_uiPass = a_uiPass;
    switch (a_uiPass)
    {
    case e_Pass_Subroutines:
    {
        Sources sources;
        getModule()->getSources(sources);
        for (auto pSource : sources)
        {
            if (!pSource->isNative())
                pSource->visit(m_private, VisitorData());
        }
    }
    break;
    case e_Pass_Blocks:
    {
        for (size_t i = 0; i < m_private->m_Blocks.size(); ++i)
        {
            auto pair = m_private->m_Blocks[i];
            m_private->m_DIScopeStack.push_back(pair.second->getJitFunction().function->getSubprogram());
            m_private->pushDebugPos(pair.second, pair.second->getSubroutine());
            VisitorData data;
            Value       dump;
            const void* in[1] = {pair.second};
            void*       out[1] = {&dump};
            data.in = in;
            data.out = out;
            pair.first->visit(m_private, data);
            m_private->setCurrentDebugPos(pair.second, pair.second->getSubroutine()->getCodeRange().end);
            pair.second->finish();
            m_private->popDebugPos(pair.second, pair.second->getSubroutine());
            m_private->m_DIScopeStack.pop_back();
        }
    }
    break;
    case e_Pass_CodeGen:
    {
        m_private->finalizeDebugInfo();
        for (auto it = m_private->m_Blocks.begin(); it != m_private->m_Blocks.end(); ++it)
        {
            it->second->codeGen();
        }

        PHANTOM_VERIFY(!m_private->m_ExecutionEngine->hasError(), "%s",
                       m_private->m_ExecutionEngine->getErrorMessage().c_str());

        if (m_private->m_ExecutionEngine->hasError())
        {
            PHANTOM_LOG(Error, "%s", m_private->m_ExecutionEngine->getErrorMessage().c_str());
            return Error::UnresolvedSymbol;
        }

#if PHANTOM_OPERATING_SYSTEM == PHANTOM_OPERATING_SYSTEM_WINDOWS
        // llvm::outs() << "We just constructed this LLVM module:\n\n" << *m_private->m_Module;
        if (m_private->m_Context.m_pDebugContext)
        {
            switch (m_private->m_Context.m_pDebugContext->m_pDebugDllMemMgr->getStatus())
            {
            case llvm::JITPDBMemoryManager::Status::MemoryNotReady:
                return Error::MemoryNotReady;
            case llvm::JITPDBMemoryManager::Status::OutOfMemory:
                return Error::OutOfMemory;
            default:
                return Error::OK;
            }
        }
#endif
        return Error::OK;
    }
    break;
    }
    return Error::OK;
} // namespace phantom { namespace jit

void CodeGenerator::onOutdated()
{
    begin();
    end();
}

} // namespace jit
} // namespace phantom

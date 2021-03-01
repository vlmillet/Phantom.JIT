// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

#pragma once

#include "prerequisites.h"

#if defined(_MSC_VER)
#    pragma warning(push, 0)
#elif defined(__clang__)
#    pragma clang diagnostic push
#    pragma clang diagnostic ignored "-Wall"
#    pragma clang diagnostic ignored "-Wextra"
#endif
#include <llvm/DebugInfo/CodeView/CodeView.h>
#include <llvm/DebugInfo/DWARF/DWARFContext.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITEventListener.h>
#if defined(_MSC_VER)
#    pragma warning(pop)
#elif defined(__clang__)
#    pragma clang diagnostic pop
#endif

#include "CodeGenerator.h"
#include "Context.h"
#include "Subroutine.h"
#include "phantom/utils/random.h"

#include <phantom/lang/LanguageElementVisitorEx.h>

#define PHANTOM_JIT_DUMP_CONTEXT 0

namespace llvm
{
class DIFile;
class JITPDBMemoryManager;
} // namespace llvm

namespace phantom
{
namespace jit
{
class JITPDBFileBuilder;
struct CodeGeneratorPrivate : public lang::LanguageElementVisitorEx, public llvm::JITEventListener
{
    friend class CodeGeneratorData;
    friend class CodeGenerator;
    friend class Subroutine;
    friend class Method;
    friend class DebugInformation;
    friend class DebugInformationPrivate;
    friend class JITPDBFileBuilder;

    CodeGeneratorPrivate(CodeGenerator* a_pCodeGenerator);
    ~CodeGeneratorPrivate();

    phantom::Path getOutPath();
    phantom::Path getDllPath();
    phantom::Path getPdbPath();
    phantom::Path getNatvisPath();

    virtual void notifyObjectLoaded(ObjectKey K, const llvm::object::ObjectFile& Obj,
                                    const llvm::RuntimeDyld::LoadedObjectInfo& L) override;

    void pushDebugPos(Subroutine* a_pJitSubroutine, lang::LanguageElement* a_pElem, bool a_bCol = false);
    void popDebugPos(Subroutine* a_pJitSubroutine, lang::LanguageElement* a_pElem, bool a_bCol = false);
    void lockDebugPos() { m_lockDebugPos++; }
    void unlockDebugPos() { m_lockDebugPos--; }
    void setCurrentDebugPos(Subroutine* a_pJitSubroutine, const lang::CodePosition& pos, bool a_bCol = false);

    virtual void notifyFreeingObject(ObjectKey K) override {}
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    /// \brief  Gets a temporary data associated with a language element.
    ///
    /// \param [in,out] a_pElement  The element associated with the data we are requesting.
    ///
    /// \return null if no temporary data is associated with the given language element, else the associated language
    /// element.
    ////////////////////////////////////////////////////////////////////////////////////////////////////

    CodeGeneratorData* getData(const lang::LanguageElement* a_pElement) const
    {
        for (auto pData : m_Data)
        {
            if (pData->m_pElement == a_pElement)
                return pData;
        }
        return nullptr;
    }

    virtual void visit(lang::AllocateExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::ArrayExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::BaseConstructorCallStatement* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::Block* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::BranchIfNotStatement* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::BranchIfStatement* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::BranchStatement* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::BuiltInConversionExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::BuiltInOperatorExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::CallExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::Class* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::ClassListInitializationExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::ClassType* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::ClassTypeListInitializationExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::ConditionalExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::ConstantExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::ConstructorCallExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::ControlStatement* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::FieldExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::FieldInitializationStatement* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::FieldPointerExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::DeallocateExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::DeleteExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::Expression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::ExpressionStatement* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::Function* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::IdentityExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::InitializerListExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::Label* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::LanguageElement* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::LoadExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::LocalVariable* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::LocalVariableExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::LocalVariableInitializationStatement* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::Method* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::MethodPointerCallExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::MemCopyStatement* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::NewExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::Symbol* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::Parameter* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::PlacementNewExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::PointerAdjustmentExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::PropertyExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::ReturnStatement* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::RValueToConstLValueExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::RValueReferenceExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::Scope* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::Source* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::Statement* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::StringLiteralExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::Subroutine* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::SubroutinePointerExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::SymbolReferenceExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::TemplateSpecialization* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::TemporaryObjectDestructionExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::VariableExpression* a_pInput, lang::VisitorData a_Data) override;
    virtual void visit(lang::VirtualMethodTableSetupStatement* a_pInput, lang::VisitorData a_Data) override;

    Subroutine*     getSubroutine(FunctionEntry func) const;
    llvm::Function* getFunction(lang::Subroutine* a_pSubroutine) const;

    void queueBlock(lang::Block* a_pBlock, Subroutine* a_pSubroutine)
    {
        m_Blocks.push_back(_BlockData(a_pBlock, a_pSubroutine));
    }
    enum EExpressionFlag
    {
        e_Expression_None = 0x0,
        e_Expression_InPlace = 0x1,
        e_Expression_Address = 0x2,
        e_Expression_LValue = 0x4,
        e_Expression_RValue = 0x8,
        e_Expression_KeepRefAddress =
        0x10, // this a hack for RValueToConstValue on explicit addressing ... (TODO : find a cleaner way)
    };
    PHANTOM_DECLARE_FLAGS(ExpressionFlags, EExpressionFlag);

    phantom::lang::Type* getArgumentType(phantom::lang::Type* a_pParamType) const;

    Value      compileArgument(Subroutine* a_pSubroutine, lang::Type* a_pParamType, lang::Expression* a_pExpression);
    Value      compileExpression(Subroutine* a_pSubroutine, lang::Expression* a_pExpression,
                                 ExpressionFlags a_Flags = e_Expression_RValue);
    Value      compileExpression(Subroutine* a_pSubroutine, lang::Expression* a_pExpression, Value place,
                                 ExpressionFlags a_Flags = ExpressionFlags(e_Expression_RValue | e_Expression_InPlace));
    CaseLabel* caseLabel(lang::Label* a_pLabel);

    void _addData(CodeGeneratorData* a_pCodeGeneratorData) { m_Data.push_back(a_pCodeGeneratorData); }
    void _removeData(CodeGeneratorData* a_pCodeGeneratorData)
    {
        m_Data.erase(std::find(m_Data.begin(), m_Data.end(), a_pCodeGeneratorData));
    }

    template<typename t_It>
    Value compileCall(Subroutine* in_pSubroutine, lang::LanguageElement* a_pAllocaOwner,
                      lang::Subroutine* a_pSubroutine, t_It argRBegin, t_It argREnd, Value a_RVOStorage, int flags,
                      bool a_bForceNonVirtualCall = false);
    void  pushPropertyStack();
    void  pushProperty(lang::PropertyExpression* a_pInput, Value a_LHS, Value a_Value, bool a_requiresSetCall);
    void  popPropertyStack();
    void  flushProperties(Subroutine* in_pSubroutine);
    void  registerJitFunction(FunctionEntry, class Subroutine*);
    void  unregisterJitFunction(FunctionEntry func);

    void offsetFunctions(ptrdiff_t a_Offset);
    void finalizeDebugInfo();

    CodeGeneratorPrivate* prev_impl = nullptr;

    uint64_t m_TempPathGUID[2] = {phantom::random::integer(), phantom::random::integer()};

    CodeGenerator*                      m_pCodeGenerator;
    llvm::LLVMContext                   m_LLVMContext;
    std::unique_ptr<llvm::Module>       m_ModuleUniquePtr;
    llvm::Module*                       m_Module;
    llvm::ExecutionEngine*              m_ExecutionEngine;
    std::unique_ptr<llvm::DWARFContext> m_pDWARFContext;
    Context                             m_Context;
    lang::Class*                        m_pVirtualMethodTableClass{};
    lang::Method*                       m_pExtractNativeVTableMeth{};

    using _BlockData = std::pair<lang::Block*, Subroutine*>;
    SmallMap<void*, llvm::Function*>           m_GlobalLLVMFunctions;
    std::vector<CodeGeneratorData*>            m_Data;
    std::vector<_BlockData>                    m_Blocks;
    std::vector<void*>                         m_PropertyExpressionPairs;
    SmallMap<FunctionEntry, Subroutine*>       m_JitSubroutineMap;
    SmallMap<lang::Subroutine*, FunctionEntry> m_LLVMFunctionMap;
    SmallVector<llvm::DIScope*, 8>             m_DIScopeStack;
    ptrdiff_t                                  m_FunctionOffset = 0;
    int                                        m_lockDebugPos = 0;
};

} // namespace jit
} // namespace phantom

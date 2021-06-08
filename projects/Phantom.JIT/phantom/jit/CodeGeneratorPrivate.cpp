// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

/* ******************* Includes ****************** */
#include "CodeGeneratorPrivate.h"

#include "DebugInformation.h"
#include "Function.h"
#include "LocalVariable.h"
#include "Method.h"
#include "Subroutine.h"

#include <phantom/lang/AllocateExpression.h>
#include <phantom/lang/BaseConstructorCallStatement.h>
#include <phantom/lang/Block.h>
#include <phantom/lang/BranchIfNotStatement.h>
#include <phantom/lang/BranchIfStatement.h>
#include <phantom/lang/BuiltInConversionExpression.h>
#include <phantom/lang/BuiltInOperator.h>
#include <phantom/lang/BuiltInOperatorExpression.h>
#include <phantom/lang/CallExpression.h>
#include <phantom/lang/ConditionalExpression.h>
#include <phantom/lang/ConstantExpression.h>
#include <phantom/lang/ConstructorCallExpression.h>
#include <phantom/lang/DeallocateExpression.h>
#include <phantom/lang/DeleteExpression.h>
#include <phantom/lang/ExpressionStatement.h>
#include <phantom/lang/FieldExpression.h>
#include <phantom/lang/FieldInitializationStatement.h>
#include <phantom/lang/FunctionType.h>
#include <phantom/lang/IdentityExpression.h>
#include <phantom/lang/Label.h>
#include <phantom/lang/LoadExpression.h>
#include <phantom/lang/LocalVariable.h>
#include <phantom/lang/LocalVariableExpression.h>
#include <phantom/lang/LocalVariableInitializationStatement.h>
#include <phantom/lang/MemCopyStatement.h>
#include <phantom/lang/Message.h>
#include <phantom/lang/MethodPointer.h>
#include <phantom/lang/MethodPointerCallExpression.h>
#include <phantom/lang/Module.h>
#include <phantom/lang/NewExpression.h>
#include <phantom/lang/Parameter.h>
#include <phantom/lang/PlacementNewExpression.h>
#include <phantom/lang/Plugin.h>
#include <phantom/lang/Pointer.h>
#include <phantom/lang/PointerAdjustmentExpression.h>
#include <phantom/lang/Property.h>
#include <phantom/lang/PropertyExpression.h>
#include <phantom/lang/RValueReferenceExpression.h>
#include <phantom/lang/RValueToConstLValueExpression.h>
#include <phantom/lang/ReturnStatement.h>
#include <phantom/lang/Semantic.h>
#include <phantom/lang/SourceFile.h>
#include <phantom/lang/StringLiteralExpression.h>
#include <phantom/lang/SubroutinePointerExpression.h>
#include <phantom/lang/SymbolReferenceExpression.h>
#include <phantom/lang/VariableExpression.h>
#include <phantom/lang/VirtualMethodTable.h>
#include <phantom/lang/VirtualMethodTableSetupStatement.h>
#include <phantom/utils/Path.h>
#include <stdarg.h>

#if defined(_MSC_VER)
#    pragma warning(push, 0)
#elif defined(__clang__)
#    pragma clang diagnostic push
#    pragma clang diagnostic ignored "-Wall"
#    pragma clang diagnostic ignored "-Wextra"
#endif
#include <llvm/DebugInfo/CodeView/GUID.h>
#include <llvm/ExecutionEngine/RTDyldMemoryManager.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/MCRegisterInfo.h>
#include <llvm/Object/COFF.h>
#include <llvm/Support/MemoryBuffer.h>
#if defined(_MSC_VER)
#    pragma warning(pop)
#elif defined(__clang__)
#    pragma clang diagnostic pop
#endif
#include "phantom/lang/Application.h"
#include "phantom/lang/Compiler.h"
#include "phantom/utils/random.h"

#include <phantom/lang/ArrayExpression.h>
#include <phantom/lang/ClassListInitializationExpression.h>
#include <phantom/lang/ClassTypeListInitializationExpression.h>
#include <phantom/lang/Structure.h>
#include <phantom/lang/TemplateSpecialization.h>
#include <phantom/lang/Variable.h>

#if PHANTOM_OPERATING_SYSTEM == PHANTOM_OPERATING_SYSTEM_WINDOWS
#    include "windows.h"

#    include <llvm/JITPDB/JITPDBMemoryManager.h>
#    include <psapi.h>
#endif

#include "session.h"

#include <iostream>
#include <phantom/lang/InitializerListExpression.h>
#include <phantom/lang/TemporaryObjectDestructionExpression.h>

namespace phantom
{
PHANTOM_EXPORT_PHANTOM void dynamicDelete(void* in);
}

int s_inc = 0;

/* *********************************************** */
namespace phantom
{
namespace jit
{
using namespace phantom::lang;

#define o_ir_builder ((llvm::IRBuilder<>*)(in_pSubroutine->getJitFunction().builder))

struct PropertyAccessPair
{
    PropertyExpression* propertyExpression;
    Value               lhsAddress;
    Value               valueAddress;
    bool                requiresSetCall;
};

typedef phantom::SmallVector<PropertyAccessPair> PropertyAccessPairs;

class CaseLabel : public CodeGeneratorData
{
public:
    CaseLabel(lang::Label* a_pStatement) : CodeGeneratorData(a_pStatement)
    {
        if (a_pStatement)
            m_jit_Label.name = a_pStatement->getLabelName();
        else
            m_jit_Label.name = "null";
    }
    Label m_jit_Label;
};

CodeGeneratorPrivate::CodeGeneratorPrivate(CodeGenerator* a_pCodeGenerator)
    : m_ModuleUniquePtr(std::make_unique<llvm::Module>(
      phantom::lexical_cast<String>(a_pCodeGenerator->getModule()).c_str(), m_LLVMContext)),
      m_Module(m_ModuleUniquePtr.get()),
      m_pCodeGenerator(a_pCodeGenerator),
      m_Context(this, m_LLVMContext, m_Module)
{
#if PHANTOM_COMPILER == PHANTOM_COMPILER_VISUAL_STUDIO
    m_Module->addModuleFlag(llvm::Module::Warning, "CodeView", 1);
#endif
    phantom::Path p(phantom::lang::Application::Get()->getDefaultBinaryPath() + '/' +
                    m_pCodeGenerator->getModule()->getName() + ".debug");

    if (m_Context.m_pDebugContext)
    {
        Sources sources;
        m_pCodeGenerator->getModule()->getSources(sources);
        for (auto pSource : sources)
        {
            m_Context.m_pDebugContext->getOrCreateDIFile(pSource);
        }
        m_Context.m_pDebugContext->createCompileUnit(p.absolute().genericString());
#if PHANTOM_OPERATING_SYSTEM == PHANTOM_OPERATING_SYSTEM_WINDOWS

#    if PHANTOM_JIT_USE_STANDALONE_JITMEM
        char buf[256];
        GetModuleFileNameA(NULL, buf, 256);
        phantom::Path exePath = buf;
        auto          memMgr = std::make_unique<llvm::JITPDBMemoryManager>(
        toStringRef(getPdbPath().genericString()), exePath.parentPath().childPath("jitmem.pdb").genericString().c_str(),
        [this](void* a_pBaseAddress) { m_pCodeGenerator->getModule()->setBaseAddress(a_pBaseAddress); });

#    else
        auto memMgr = std::make_unique<llvm::JITPDBMemoryManager>(
        toStringRef(getPdbPath().genericString()), "",
        [this](void* a_pBaseAddress) { m_pCodeGenerator->getModule()->setBaseAddress(a_pBaseAddress); });
#    endif
        m_Context.m_pDebugContext->m_pDebugDllMemMgr = memMgr.get();
        m_Context.m_pDebugContext->m_pDebugDllMemMgr->setVerbose(true);
        m_ExecutionEngine =
        llvm::EngineBuilder(std::move(m_ModuleUniquePtr)).setMemoryManager(std::move(memMgr)).create();
    }
    else
    {
#endif
        m_ExecutionEngine = llvm::EngineBuilder(std::move(m_ModuleUniquePtr)).create();
    }

    auto pTarget = m_ExecutionEngine->getTargetMachine();
    m_Module->setTargetTriple(pTarget->getTargetTriple().str());
    m_ExecutionEngine->RegisterJITEventListener(this);

    m_pVirtualMethodTableClass = PHANTOM_CLASSOF(VirtualMethodTable);
    m_pExtractNativeVTableMeth = m_pVirtualMethodTableClass->getMethod("extractNativeVTable");
}

CodeGeneratorPrivate::~CodeGeneratorPrivate()
{
    PHANTOM_ASSERT(!prev_impl);
    while (m_Data.size())
    {
        auto pData = (m_Data.begin() + (m_Data.size() - 1))->second;
        pData->setCodeGenerator(nullptr);
        phantom::deleteVirtual(pData);
    }
    if (m_ExecutionEngine)
        m_ExecutionEngine->UnregisterJITEventListener(this);
    // delete m_pDWARFContext;
    if (m_Context.m_pDebugContext)
        m_Context.m_pDebugContext->m_pDebugDllMemMgr = nullptr;
    // delete m_ExecutionEngine; // keep alive (FIXME : find a way to release this at some time)
}

phantom::Path CodeGeneratorPrivate::getOutPath()
{
#if PHANTOM_OPERATING_SYSTEM == PHANTOM_OPERATING_SYSTEM_WINDOWS
    auto str = GUIDToText(*(llvm::codeview::GUID*)m_TempPathGUID);
    return phantom::Path(sessionFolder()).childPath(str);
#else
    return "";
#endif
}

phantom::Path CodeGeneratorPrivate::getDllPath()
{
    phantom::Path p = getOutPath().childPath(String(m_pCodeGenerator->getModule()->getName()) + ".dll");
    return p;
}

phantom::Path CodeGeneratorPrivate::getPdbPath()
{
    phantom::Path p = getOutPath().childPath(String(m_pCodeGenerator->getModule()->getName()) + ".pdb");
    return p;
}

phantom::Path CodeGeneratorPrivate::getNatvisPath()
{
    phantom::Path p = getOutPath().childPath(String(m_pCodeGenerator->getModule()->getName()) + ".natvis");
    return p;
}

struct Entry
{
    /// The beginning address of the instruction range.
    uint64_t Begin;
    /// The ending address of the instruction range.
    uint64_t End;
    /// The location of the variable within the specified range.
    llvm::SmallVector<unsigned char, 4> Loc;
};

/// A list of locations that contain one variable.
struct LocationList
{
    /// The beginning offset where this location list is stored in the debug_loc
    /// section.
    unsigned Offset;
    /// All the locations in which the variable is stored.
    llvm::SmallVector<Entry, 2> Entries;
};

typedef llvm::SmallVector<LocationList, 4> LocationLists;

template<class T>
static LocationList const* getLocationListAtOffset(T& Locations, uint64_t Offset)
{
    auto It = std::lower_bound(Locations.begin(), Locations.end(), Offset,
                               [](const LocationList& L, uint64_t Offset) { return L.Offset < Offset; });
    if (It != Locations.end() && It->Offset == Offset)
        return &(*It);
    return nullptr;
}

void CodeGeneratorPrivate::notifyObjectLoaded(ObjectKey, const llvm::object::ObjectFile& Obj,
                                              const llvm::RuntimeDyld::LoadedObjectInfo& L)
{
    if (m_Context.m_pDebugContext)
    {
        // create a Dwarf cache
        // m_pDWARFContext = llvm::DWARFContext::create(Obj, &L);
#define PHANTOM_JIT_EMIT_OBJFILE 0

#if PHANTOM_JIT_EMIT_OBJFILE
        llvm::legacy::PassManager pass;
        auto                      FileType = llvm::TargetMachine::CGFT_ObjectFile;

        std::error_code      ec;
        llvm::raw_fd_ostream stream(
        (phantom::String(m_pCodeGenerator->getModule()->getName()) + ".Phantom.JIT.obj").c_str(), ec);

        if (m_ExecutionEngine->getTargetMachine()->addPassesToEmitFile(pass, stream, nullptr, FileType))
        {
            PHANTOM_LOG(Error, "TargetMachine can't emit a file of this type");
        }
        else
        {
            pass.run(*m_Module);
            stream.flush();
            stream.close();
        }
#endif
    }
}

void CodeGeneratorPrivate::pushDebugPos(Subroutine* a_pJitSubroutine, LanguageElement* a_pElem, bool a_bCol)
{
    if (m_lockDebugPos == 0 && a_pElem->getCodeRange().begin.isValid())
    {
        setCurrentDebugPos(a_pJitSubroutine, a_pElem->getCodeRange().begin, a_bCol);
    }
}

void CodeGeneratorPrivate::popDebugPos(Subroutine* a_pJitSubroutine, LanguageElement* a_pElem, bool a_bCol)
{
    if (m_lockDebugPos == 0 && a_pElem->getCodeRange().end.isValid())
    {
        setCurrentDebugPos(a_pJitSubroutine, a_pElem->getCodeRange().end, a_bCol);
    }
}

void CodeGeneratorPrivate::setCurrentDebugPos(Subroutine* a_pJitSubroutine, const CodePosition& pos, bool a_bCol)
{
    CodePosition posNoCol = pos;
    posNoCol.column = a_bCol ? pos.column : 1;
    llvm::DebugLoc loc = llvm::DebugLoc::get(posNoCol.line, posNoCol.column, m_DIScopeStack.back());
    a_pJitSubroutine->m_jit_function.builder->SetCurrentDebugLocation(loc);
}

void CodeGeneratorPrivate::visit(AllocateExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    Value       count = a_pInput->getSizeExpression()
    ? compileExpression(in_pSubroutine, a_pInput->getSizeExpression(), e_Expression_RValue)
    : in_pSubroutine->createSizeTConstant(1);

    Value args[] = {in_pSubroutine->createPtrConstant(a_pInput->getType()), count};

    out_value =
    in_pSubroutine->callSubroutine(PHANTOM_CLASSOF(Type)->getMethodCascade("allocate(size_t) const"), args, 2, 0);
    out_value = in_pSubroutine->convert(out_value, a_pInput->getValueType());
}

void CodeGeneratorPrivate::visit(ArrayExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];

    pushDebugPos(in_pSubroutine, a_pInput);

    Array* pArrayType = static_cast<Array*>(a_pInput->getValueType()->removeRValueReference());
    Type*  pArrayItemType = pArrayType->getItemType();
    size_t stride = pArrayType->getItemType()->getSize();
    if (out_value.isNull())
        out_value = in_pSubroutine->getOrCreateAlloca(a_pInput, pArrayType);
    in_pSubroutine->memset(out_value, in_pSubroutine->createIntConstant(0),
                           in_pSubroutine->createSizeTConstant(pArrayType->getSize())); // zero init first

    auto const& expressions = a_pInput->getValues();
    size_t      c = 0;
    for (size_t i = 0; i < pArrayType->getItemCount(); ++i)
    {
        if (Expression* pExp = expressions[c++])
        {
            size_t offset = i * stride;
            auto   pType = pArrayItemType->addPointer();
            Value  adjust = (offset == 0)
            ? Value(o_ir_builder->CreatePointerCast(out_value.value, in_pSubroutine->toJitType(pType)), pType)
            : in_pSubroutine->loadElemAddress(out_value, i, pType);
            if (pArrayItemType->asClassType())
            {
                compileExpression(in_pSubroutine, pExp, adjust);
            }
            else
            {
                in_pSubroutine->store(adjust, compileExpression(in_pSubroutine, pExp));
            }
        }
    }

    popDebugPos(in_pSubroutine, a_pInput);
}

void CodeGeneratorPrivate::visit(BaseConstructorCallStatement* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    pushDebugPos(in_pSubroutine, a_pInput);
    Value  _this = in_pSubroutine->getThis();
    Class* pBaseClass = a_pInput->getConstructorCallExpression()->getValueType()->removeRValueReference()->asClass();
    auto   offset = a_pInput->getDerivedClass()->getBaseClassOffset(pBaseClass);

    auto  pType = pBaseClass->addPointer();
    Value _this_adjust = (offset == 0)
    ? Value(o_ir_builder->CreatePointerCast(_this.value, in_pSubroutine->toJitType(pType)), pType)
    : in_pSubroutine->adjustPointer(_this, offset, pType);

    compileExpression(in_pSubroutine, a_pInput->getConstructorCallExpression(), _this_adjust);
    popDebugPos(in_pSubroutine, a_pInput);
}

void CodeGeneratorPrivate::visit(ClassListInitializationExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];

    pushDebugPos(in_pSubroutine, a_pInput);

    Class* pClass = a_pInput->getClass();

    if (out_value.isNull())
    {
        PHANTOM_ASSERT(((a_Data.flags & e_Expression_Address) != 0),
                       "any class value cannot be requested as an RValue");
        out_value = in_pSubroutine->getOrCreateAlloca(a_pInput, pClass);
        in_pSubroutine->registerTemporary(a_pInput, out_value);
    }
    in_pSubroutine->memset(out_value, in_pSubroutine->createIntConstant(0),
                           in_pSubroutine->createSizeTConstant(pClass->getSize())); // zero init first

    auto const& expressions = a_pInput->getExpressions();
    size_t      c = 0;
    for (auto baseClass : pClass->getBaseClasses())
    {
        if (Expression* pExp = expressions[c++])
        {
            size_t offset = baseClass.offset;
            auto   pType = baseClass.baseClass->addPointer();
            Value  adjust = (offset == 0)
            ? Value(o_ir_builder->CreatePointerCast(out_value.value, in_pSubroutine->toJitType(pType)), pType)
            : in_pSubroutine->adjustPointer(out_value, offset, pType);
            compileExpression(in_pSubroutine, pExp, adjust);
        }
    }
    for (auto pField : pClass->getFields())
    {
        if (Expression* pExp = expressions[c++])
        {
            size_t offset = pField->getOffset();
            auto   pType = pField->getValueType()->addPointer();
            Value  adjust = (offset == 0)
            ? Value(o_ir_builder->CreatePointerCast(out_value.value, in_pSubroutine->toJitType(pType)), pType)
            : in_pSubroutine->adjustPointer(out_value, offset, pType);
            if (pField->getValueType()->asClassType())
            {
                compileExpression(in_pSubroutine, pExp, adjust);
            }
            else
            {
                in_pSubroutine->store(adjust, compileExpression(in_pSubroutine, pExp));
            }
        }
    }

    popDebugPos(in_pSubroutine, a_pInput);
}

void CodeGeneratorPrivate::visit(ClassTypeListInitializationExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];

    pushDebugPos(in_pSubroutine, a_pInput);

    ClassType* pClassType = a_pInput->getClassType();

    if (out_value.isNull())
    {
        PHANTOM_ASSERT(((a_Data.flags & e_Expression_Address) != 0),
                       "any class value cannot be requested as an RValue");
        out_value = in_pSubroutine->getOrCreateAlloca(a_pInput, pClassType);
    }
    in_pSubroutine->memset(out_value, in_pSubroutine->createIntConstant(0),
                           in_pSubroutine->createSizeTConstant(pClassType->getSize())); // zero init first

    auto const& expressions = a_pInput->getExpressions();
    size_t      c = 0;
    for (auto pField : pClassType->getFields())
    {
        if (Expression* pExp = expressions[c++])
        {
            size_t offset = pField->getOffset();
            auto   pType = pField->getValueType()->addPointer();
            Value  adjust = (offset == 0)
            ? Value(o_ir_builder->CreatePointerCast(out_value.value, in_pSubroutine->toJitType(pType)), pType)
            : in_pSubroutine->adjustPointer(out_value, offset, pType);
            if (pField->getValueType()->asClassType())
            {
                compileExpression(in_pSubroutine, pExp, adjust);
            }
            else
            {
                in_pSubroutine->store(adjust, compileExpression(in_pSubroutine, pExp));
            }
        }
    }

    popDebugPos(in_pSubroutine, a_pInput);
}

void CodeGeneratorPrivate::visit(Block* a_pInput, VisitorData a_Data)
{
#if 0
        int inc = s_inc++;
        while (inc--)
            printf("   ");
        printf("{\n");
#endif
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];

    pushDebugPos(in_pSubroutine, a_pInput);

    auto dbgCtx = in_pSubroutine->getDebugContext();
    if (dbgCtx)
    {
        auto* diBlock = dbgCtx->getOrCreateDIScope(a_pInput);
        m_DIScopeStack.push_back(diBlock);
    }

    Value& out_value = *(Value*)a_Data.out[0];

    for (Type* pType : a_pInput->getTypes())
    {
        pType->visit(this, a_Data);
    }

    auto loc = a_pInput->getCodeRange();

    for (lang::LocalVariable* pLoc : a_pInput->getLocalVariables())
    {
        pLoc->visit(this, a_Data);
    }

    for (Statement* pStatement : a_pInput->getStatements())
    {
        pStatement->visit(this, a_Data);
    }

    visit(static_cast<Statement*>(a_pInput), a_Data);

    if (dbgCtx)
    {
        m_DIScopeStack.pop_back();
    }

    popDebugPos(in_pSubroutine, a_pInput);

#if 0
        inc = --s_inc;
        while (inc--)
            printf("   ");
        printf("}\n");
#endif
}

void CodeGeneratorPrivate::visit(BranchIfNotStatement* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    Value       expr = compileExpression(in_pSubroutine, a_pInput->getExpression());
    CaseLabel*  pCaseLabel = caseLabel(a_pInput->getLabel());
    visit(static_cast<Statement*>(a_pInput), a_Data);
    pushDebugPos(in_pSubroutine, a_pInput);
    for (auto pStmt : a_pInput->getRAIIDestructions())
    {
        pStmt->visit(this, a_Data);
    }
    PHANTOM_VERIFY(in_pSubroutine->branchIfNot(in_pSubroutine->toBool(expr), &(pCaseLabel->m_jit_Label)).value,
                   "jit compile error");
    popDebugPos(in_pSubroutine, a_pInput);
}

void CodeGeneratorPrivate::visit(BranchIfStatement* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    pushDebugPos(in_pSubroutine, a_pInput);
    Value&     out_value = *(Value*)a_Data.out[0];
    Value      expr = compileExpression(in_pSubroutine, a_pInput->getExpression());
    CaseLabel* pCaseLabel = caseLabel(a_pInput->getLabel());
    visit(static_cast<ControlStatement*>(a_pInput), a_Data);
    PHANTOM_VERIFY(in_pSubroutine->branchIf(in_pSubroutine->toBool(expr), &(pCaseLabel->m_jit_Label)).value,
                   "jit compile error");
    popDebugPos(in_pSubroutine, a_pInput);
}

void CodeGeneratorPrivate::visit(BranchStatement* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    pushDebugPos(in_pSubroutine, a_pInput);
    Value&     out_value = *(Value*)a_Data.out[0];
    CaseLabel* pCaseLabel = caseLabel(a_pInput->getLabel());
    visit(static_cast<ControlStatement*>(a_pInput), a_Data);
    PHANTOM_VERIFY(in_pSubroutine->branch(&(pCaseLabel->m_jit_Label)).value, "jit compile error");
    popDebugPos(in_pSubroutine, a_pInput);
}

void CodeGeneratorPrivate::visit(BuiltInConversionExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    PHANTOM_ASSERT(out_value.isNull());

    Value in_value = compileExpression(in_pSubroutine, a_pInput->getInputExpression());

    if (a_pInput->getValueType() == PHANTOM_TYPEOF(bool))
    {
        out_value = in_pSubroutine->toBool(in_value);
    }
    else
    {
        out_value = in_pSubroutine->convert(in_value, a_pInput->getValueType());
    }
    // in case of a r-value requested, we can just return the conversion result value

    // but in case of an address required, we first need to create a temporary to store the converted value
    // before returning the temporary address
    if (a_Data.flags & e_Expression_Address)
    {
        Value temp = in_pSubroutine->getOrCreateAlloca(a_pInput, a_pInput->getValueType());
        in_pSubroutine->store(temp, out_value);
        out_value = temp;
    }
}

#define ADDRESS_FALLBACK()                                                                                             \
    if (a_Data.flags & e_Expression_Address)                                                                           \
    {                                                                                                                  \
        Value temp = in_pSubroutine->getOrCreateAlloca(a_pInput, a_pInput->getValueType());                            \
        in_pSubroutine->store(temp, result);                                                                           \
        result = temp;                                                                                                 \
    }

#define UNARY(n)                                                                                                       \
    {                                                                                                                  \
        Value v0 = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0));                                    \
        if (v0.type->asReference())                                                                                    \
            v0 = in_pSubroutine->load(v0);                                                                             \
        if (Enum* pEnum = v0.type->asEnum())                                                                           \
            v0.type = pEnum->getUnderlyingIntType();                                                                   \
        result = in_pSubroutine->n(v0);                                                                                \
        if (out_value.isNull())                                                                                        \
            ADDRESS_FALLBACK();                                                                                        \
    }

#define BINARY(n)                                                                                                      \
    {                                                                                                                  \
        Value v0 = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0));                                    \
        Value v1 = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(1));                                    \
        if (v0.type->asReference())                                                                                    \
            v0 = in_pSubroutine->load(v0);                                                                             \
        if (v1.type->asReference())                                                                                    \
            v1 = in_pSubroutine->load(v1);                                                                             \
        if (Enum* pEnum = v0.type->asEnum())                                                                           \
            v0.type = pEnum->getUnderlyingIntType();                                                                   \
        if (Enum* pEnum = v1.type->asEnum())                                                                           \
            v1.type = pEnum->getUnderlyingIntType();                                                                   \
        result = in_pSubroutine->n(v0, v1);                                                                            \
        if (out_value.isNull())                                                                                        \
            ADDRESS_FALLBACK();                                                                                        \
    }

#define BINARY_ASSIGN(n)                                                                                               \
    {                                                                                                                  \
        pushPropertyStack();                                                                                           \
        result = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0), e_Expression_LValue);                 \
        Value v1 = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(1));                                    \
        PHANTOM_ASSERT(result.type->asReference());                                                                    \
        if (Enum* pEnum = v1.type->asEnum())                                                                           \
            v1.type = pEnum->getUnderlyingIntType();                                                                   \
        Value opResult = in_pSubroutine->n(in_pSubroutine->load(result), v1);                                          \
        PHANTOM_VERIFY(                                                                                                \
        in_pSubroutine->store(result, in_pSubroutine->convert(opResult, result.type->removeReference())).value,        \
        "storeRelative failed");                                                                                       \
        flushProperties(in_pSubroutine);                                                                               \
        popPropertyStack();                                                                                            \
    }

#define UNARY_LOG(n)                                                                                                   \
    result = in_pSubroutine->toBool(in_pSubroutine->n(compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0))))

void CodeGeneratorPrivate::visit(BuiltInOperatorExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];

    Value result;

    auto pOp = a_pInput->getOperator();

    switch (pOp->getId())
    {
    case Operator::Plus:
        result = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0));
        break;
    case Operator::Minus:
        UNARY(neg);
        break;
    case Operator::Add:
        BINARY(add);
        break;
    case Operator::Subtract:
        BINARY(sub);
        break;
    case Operator::Multiply:
        BINARY(mul);
        break;
    case Operator::Divide:
        BINARY(div);
        break;
    case Operator::Address:
        result = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0),
                                   ExpressionFlags((a_Data.flags & e_Expression_Address)
                                                   ? (e_Expression_Address | e_Expression_KeepRefAddress)
                                                   : e_Expression_Address));
        PHANTOM_ASSERT(result.type->asAddressType());
        break;
    case Operator::Dereference:
        result = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0), e_Expression_RValue);
        if (result.type->asReference())
        {
            result = in_pSubroutine->load(result);
        }
        PHANTOM_ASSERT(result.type->asPointer() || result.type->asArray());

        // at this point the dereferencement is silent => we haven't done any change to the JIT
        // value returned by the deferenced expression (indeed in C++ '*ptr-to-class' has type
        // 'class&', both are address to 'class')

        break;
    case Operator::Arrow:
        PHANTOM_ASSERT(false, "built in '->' operator should have been resolved at precompilation level");
        break;
    case Operator::ArrowStar:
        PHANTOM_ASSERT(false);
        break;
    case Operator::PreDecrement:
        result = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0), e_Expression_LValue);
        in_pSubroutine->store(result,
                              in_pSubroutine->sub(in_pSubroutine->load(result), in_pSubroutine->createIntConstant(1)));
        break;
    case Operator::PreIncrement:
        result = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0), e_Expression_LValue);
        in_pSubroutine->store(result,
                              in_pSubroutine->add(in_pSubroutine->load(result), in_pSubroutine->createIntConstant(1)));
        break;
    case Operator::PostDecrement:
    {
        Value pre_value = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0), e_Expression_LValue);
        result = in_pSubroutine->load(pre_value);
        in_pSubroutine->store(
        pre_value, in_pSubroutine->sub(in_pSubroutine->load(pre_value), in_pSubroutine->createIntConstant(1)));
    }
    break;
    case Operator::PostIncrement:
    {
        Value pre_value = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0), e_Expression_LValue);
        result = in_pSubroutine->load(pre_value);
        in_pSubroutine->store(
        pre_value, in_pSubroutine->add(in_pSubroutine->load(pre_value), in_pSubroutine->createIntConstant(1)));
    }
    break;
    case Operator::Equal:
        BINARY(eq);
        break;
    case Operator::NotEqual:
        BINARY(ne);
        break;
    case Operator::Greater:
        BINARY(gt);
        break;
    case Operator::Less:
        BINARY(lt);
        break;
    case Operator::GreaterEqual:
        BINARY(ge);
        break;
    case Operator::LessEqual:
        BINARY(le);
        break;
    case Operator::LogicalAnd:
    {
        Value first = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0));
        if (first.type->asReference())
            first = in_pSubroutine->load(first);
        if (a_pInput->getEvalArgument(1)->getSemantic()->hasSideEffects(a_pInput->getEvalArgument(1)))
        {
            // second operand has side effects => need branch
            result = in_pSubroutine->getOrCreateAlloca(a_pInput, PHANTOM_TYPEOF(bool));
            auto firstBool = in_pSubroutine->toBool(first);
            in_pSubroutine->store(result, firstBool);
            llvm::BasicBlock* true_ = llvm::BasicBlock::Create(m_LLVMContext, "true");
            llvm::BasicBlock* false_ = llvm::BasicBlock::Create(m_LLVMContext, "false");

            // if first operand is true...
            o_ir_builder->CreateCondBr(firstBool.value, true_, false_);
            true_->insertInto(in_pSubroutine->getJitFunction().function);
            o_ir_builder->SetInsertPoint(true_);

            /////////////////

            //                         static lang::Function* pPrintFunc =
            //                         phantom::cplusplus()->findFunction("phantom::print(const
            //                         char*,int,int)"); PHANTOM_ASSERT(pPrintFunc);
            //
            //                         Expressions args;
            //                         args.push_back(New<StringLiteralExpression>("\"" +
            //                         in_pSubroutine->getSubroutine()->getName() +" branch
            //                         TRUE\n\""));
            //                         args.push_back(New<ConstantExpression>(Constant::Create<int>(0)));
            //                         args.push_back(New<ConstantExpression>(Constant::Create<int>(0)));
            //
            //                         compileExpression(in_pSubroutine,
            //                         New<CallExpression>(pPrintFunc, args));

            /////////////////

            //... we evaluate second one and_ performs "logical &&"
            Value second = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(1));
            if (second.type->asReference())
                second = in_pSubroutine->load(second);

            in_pSubroutine->store(
            result,
            in_pSubroutine->toBool(in_pSubroutine->and_(in_pSubroutine->load(result), in_pSubroutine->toBool(second))));
            o_ir_builder->CreateBr(false_);
            false_->insertInto(in_pSubroutine->getJitFunction().function);
            o_ir_builder->SetInsertPoint(false_);

            if (a_Data.hasFlag(e_Expression_RValue))
                result = in_pSubroutine->load(result);
        }
        else
        {
            //... we evaluate second one and_ performs "logical &&"
            Value second = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(1));
            if (second.type->asReference())
                second = in_pSubroutine->load(second);

            // no side effects => no branch needed
            result =
            in_pSubroutine->toBool(in_pSubroutine->and_(in_pSubroutine->toBool(first), in_pSubroutine->toBool(second)));
        }
    }
    break;
    case Operator::LogicalOr:
    {
        Value first = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0));
        if (first.type->asReference())
            first = in_pSubroutine->load(first);
        if (a_pInput->getEvalArgument(1)->getSemantic()->hasSideEffects(a_pInput->getEvalArgument(1)))
        {
            // second operand has side effects => need branch
            result = in_pSubroutine->getOrCreateAlloca(a_pInput, PHANTOM_TYPEOF(bool));
            auto firstBool = in_pSubroutine->toBool(first);
            in_pSubroutine->store(result, firstBool);
            llvm::BasicBlock* true_ = llvm::BasicBlock::Create(m_LLVMContext, "true");
            llvm::BasicBlock* false_ = llvm::BasicBlock::Create(m_LLVMContext, "false");

            // if first operand ! true...
            o_ir_builder->CreateCondBr(firstBool.value, false_, true_);
            true_->insertInto(in_pSubroutine->getJitFunction().function);
            o_ir_builder->SetInsertPoint(true_);

            //... we evaluate second one and_ performs "logical ||"
            Value second = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(1));
            if (second.type->asReference())
                second = in_pSubroutine->load(second);
            in_pSubroutine->store(result,
                                  in_pSubroutine->toBool(in_pSubroutine->or_(in_pSubroutine->load(result), second)));
            o_ir_builder->CreateBr(false_);
            false_->insertInto(in_pSubroutine->getJitFunction().function);
            o_ir_builder->SetInsertPoint(false_);

            if (a_Data.hasFlag(e_Expression_RValue))
                result = in_pSubroutine->load(result);
        }
        else
        {
            // no side effects => no branch needed
            Value second = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(1));
            if (second.type->asReference())
                second = in_pSubroutine->load(second);
            result =
            in_pSubroutine->toBool(in_pSubroutine->or_(in_pSubroutine->toBool(first), in_pSubroutine->toBool(second)));
        }
    }
    break;
    case Operator::XOr:
        BINARY(xor_);
        break;
    case Operator::Not:
    {
        Value toBoolExp = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0));
        if (toBoolExp.type->asReference())
            toBoolExp = in_pSubroutine->load(toBoolExp);
        result = in_pSubroutine->toNotBool(toBoolExp);
        break;
    }
    case Operator::BitAnd:
        BINARY(and_);
        break;
    case Operator::BitOr:
        BINARY(or_);
        break;
    case Operator::Complement:
        UNARY(not_);
        break;
    case Operator::Modulo:
        BINARY(rem);
        break;
    case Operator::ShiftLeft:
        BINARY(shl);
        break;
    case Operator::ShiftRight:
        BINARY(shr);
        break;
    case Operator::Assignment:
    {
        pushPropertyStack();
        result = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0), e_Expression_LValue);
        Value v1 = compileExpression(in_pSubroutine, a_pInput->getEvalArgument(1));
        if (v1.type->asReference())
        {
            v1 = in_pSubroutine->load(v1);
        }
        PHANTOM_ASSERT(result.type->asReference()); // we can only assign to l-values
        v1 = in_pSubroutine->convert(v1, result.type->removeReference());
        in_pSubroutine->store(result, v1);
        flushProperties(in_pSubroutine);
        popPropertyStack();
    }
    break;
    case Operator::AssignmentAdd:
        BINARY_ASSIGN(add);
        break;
    case Operator::AssignmentSubtract:
        BINARY_ASSIGN(sub);
        break;
    case Operator::AssignmentMultiply:
        BINARY_ASSIGN(mul);
        break;
    case Operator::AssignmentDivide:
        BINARY_ASSIGN(div);
        break;
    case Operator::AssignmentBitAnd:
        BINARY_ASSIGN(and_);
        break;
    case Operator::AssignmentBitOr:
        BINARY_ASSIGN(or_);
        break;
    case Operator::AssignmentModulo:
        BINARY_ASSIGN(rem);
        break;
    case Operator::AssignmentShiftLeft:
        BINARY_ASSIGN(shl);
        break;
    case Operator::AssignmentShiftRight:
        BINARY_ASSIGN(shr);
        break;
    case Operator::AssignmentXOr:
        BINARY_ASSIGN(xor_);
        break;
    case Operator::Bracket:
        PHANTOM_ASSERT(a_pInput->getEvalArgument(0)->getValueType()->removeConstReference()->asArray() ||
                       a_pInput->getEvalArgument(0)->getValueType()->removeConstReference()->asPointer());
        result = in_pSubroutine->loadElemAddress(
        compileExpression(in_pSubroutine, a_pInput->getEvalArgument(0)),
        /*in_pSubroutine->mul(
              in_pSubroutine->createSizeTConstant(a_pInput->getInputArgument(0)->getValueType()->removeConstReference()->removePointerOrArray()->getSize())
              , */
        compileExpression(in_pSubroutine, a_pInput->getEvalArgument(1))
        /*)*/,
        a_pInput->getValueType());
        break;
    }

    //         if (a_pInput->getValueType()->asReference() && (a_Data.flags&e_Expression_RValue))
    //         // if we expect an r-value, we need to apply a 'load' to the JIT value (now we
    //         changed something because we wan't the content of the pointer)
    //         {
    //             result = in_pSubroutine->load(result);
    //             result.type = a_pInput->getValueType()->removeReference();
    //         }
    //         else
    result.setType(a_pInput->getValueType());

    if (!out_value.isNull())
        in_pSubroutine->store(out_value, result);
    else
    {
        out_value = result;
    }
}

Subroutine* CodeGeneratorPrivate::getSubroutine(FunctionEntry func) const
{
    auto found = m_JitSubroutineMap.find(func);
    return found != m_JitSubroutineMap.end() ? found->second : nullptr;
}

#undef UNARY

llvm::Function* CodeGeneratorPrivate::getFunction(lang::Subroutine* a_pSubroutine) const
{
    auto found = m_LLVMFunctionMap.find(a_pSubroutine);
    return found != m_LLVMFunctionMap.end() ? found->second.function : nullptr;
}

#undef BINARY
#undef BINARY_ASSIGN
#undef BINARY_LOG

template<typename t_It>
Value CodeGeneratorPrivate::compileCall(Subroutine* in_pSubroutine, LanguageElement* a_pAllocaOwner,
                                        lang::Subroutine* a_pSubroutine, t_It argRBegin, t_It argREnd,
                                        Value a_RVOstorage, int flags, bool a_bForceNonVirtualCall)
{
    Value out_value;

    std::vector<Value> arguments;

    pushPropertyStack();

    /// Return Value Optimization ?
    bool bRVO = a_pSubroutine->isRVOCandidate();

    size_t argCount = 0;
    arguments.resize(argCount = (argREnd - argRBegin) + bRVO);

    bool bThisCall = a_pSubroutine->asMethod() != nullptr;

    // if(true)
    {
        // Evaluate arguments from right to left
        for (auto it = argRBegin; it != argREnd; ++it)
        {
            Value argumentValue;
            if (bThisCall)
            {
                /// this with RVO
                if (bRVO && argCount == 2)
                {
                    argumentValue = compileExpression(in_pSubroutine, *it);
                    argumentValue = in_pSubroutine->convert(
                    argumentValue, static_cast<lang::Method*>(a_pSubroutine)->getOwnerClassType()->makePointer());
                    PHANTOM_ASSERT(!argumentValue.isNull());
                    --argCount;                            // rvo
                    arguments[--argCount] = argumentValue; /// 'this' must stay the first argument in case of RVO
                    continue;
                }
                /// this without RVO
                else if (!bRVO && argCount == 1)
                {
                    argumentValue = compileExpression(in_pSubroutine, *it);
                    argumentValue = in_pSubroutine->convert(
                    argumentValue, static_cast<lang::Method*>(a_pSubroutine)->getOwnerClassType()->makePointer());
                    PHANTOM_ASSERT(!argumentValue.isNull());
                    arguments[--argCount] = argumentValue;
                    continue;
                }
            }
            Type* pParamType = a_pSubroutine->getParameterType(argCount - 1 - bThisCall - bRVO);
            argumentValue = compileArgument(in_pSubroutine, pParamType, *it);
            PHANTOM_ASSERT(!argumentValue.isNull());
            arguments[--argCount] = argumentValue;
        }
    }

    if (bRVO)
    {
        if (a_RVOstorage.isNull())
        {
            // discarding result inside the stack
            out_value = in_pSubroutine->getOrCreateAlloca(a_pAllocaOwner, a_pSubroutine->getReturnType());
        }
        else
        {
            out_value = a_RVOstorage;
        }
        arguments[bThisCall] = out_value;
        argCount -= !bThisCall;
    }

    //     else
    //     {
    //         if(bRVO)
    //         {
    //             arguments[--argCount] = out_value;
    //         }
    //         for(auto it = argRBegin; it != argREnd; ++it)
    //         {
    //             if(bThisCall && argCount == 1)
    //             {
    //                 Value argumentValue = compileExpression(in_pSubroutine, *it);
    //                 PHANTOM_ASSERT(!argumentValue.isNull());
    //                 arguments[--argCount] = argumentValue;
    //             }
    //             else
    //             {
    //                 Type* pParamType = a_pSubroutine->getParameterType(argCount-1-bThisCall);
    //                 Value argumentValue = compileExpression(in_pSubroutine, *it,
    //                 pParamType->asReference() ? e_Expression_Address : e_Expression_RValue);
    //                 PHANTOM_ASSERT(!argumentValue.isNull());
    //                 arguments[--argCount] = argumentValue;
    //             }
    //         }
    //     }

    PHANTOM_ASSERT(argCount == 0);

    Value returnValue =
    in_pSubroutine->callSubroutine(a_pSubroutine, arguments.data(), arguments.size(), 0, a_bForceNonVirtualCall);
    PHANTOM_ASSERT(!bRVO || returnValue.type == a_pSubroutine->getReturnType()->removeQualifiers()->addPointer());
    if (flags & e_Expression_Address)
    {
        if (a_pSubroutine->getReturnType()->asReference())
        {
            PHANTOM_ASSERT(!bRVO);
            out_value = returnValue;
        }
        else
        {
            if (!bRVO)
            {
                Type* pRetType = a_pSubroutine->getReturnType();
                // make a raw copy of the return value to a temporary address
                if (!pRetType->isVoid())
                {
                    if (Array* pArr = pRetType->removeReference()->asArray())
                        pRetType = pArr->getUnderlyingType()->removeAllExtents()->makePointer();
                    out_value = in_pSubroutine->getOrCreateAlloca(a_pAllocaOwner, pRetType);
                    in_pSubroutine->store(out_value, returnValue);
                }
            }
        }
    }
    else if (flags & e_Expression_LValue)
    {
        PHANTOM_ASSERT(a_pSubroutine->getReturnType()->asLValueReference());
        if (!bRVO)
        {
            out_value = returnValue;
        }
    }
    else
    {
        if (!bRVO)
        {
            out_value = returnValue;
        }
    }
    flushProperties(in_pSubroutine);
    popPropertyStack();
    return out_value;
}

void CodeGeneratorPrivate::visit(CallExpression* a_pInput, VisitorData a_Data)
{
    Subroutine*       in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&            out_value = *(Value*)a_Data.out[0];
    lang::Subroutine* pSubroutine = a_pInput->getSubroutine();
    lang::Method*     pMethod = pSubroutine->asMethod();
    bool              temp = false;
    bool              forceNonVirtualCall = pMethod && a_pInput->isFinal();

    std::vector<Expression*> args(a_pInput->getArguments().begin(), a_pInput->getArguments().end());
    /// Temp or_ not_ temp ?
    if (out_value.isNull()) /// temp
    {
        /// request call solving with temporary address return
        out_value = compileCall(in_pSubroutine, a_pInput, pSubroutine, args.rbegin(), args.rend(), Value(),
                                a_Data.flags | e_Expression_Address, forceNonVirtualCall);
        PHANTOM_ASSERT(!(pSubroutine->isRVOCandidate()) || out_value.type->asAddressType(),
                       "RVO call must return a pointer type");
        temp = true;
    }
    else
    {
        PHANTOM_ASSERT(out_value.type->asAddressType());
        PHANTOM_ASSERT(a_pInput->getValueType()->removeRValueReference()->removeQualifiers()->asClass());
        PHANTOM_VERIFY(out_value ==
                       compileCall(in_pSubroutine, a_pInput, pSubroutine, args.rbegin(), args.rend(), out_value,
                                   a_Data.flags, forceNonVirtualCall));
    }
    if (!(pSubroutine->isRVOCandidate())) /// No RVO
    {
        /// return type is a class and_ we have built a temporary value => register the temporary
        /// for destruction
        if (a_pInput->getValueType()->removeRValueReference()->removeQualifiers()->asClass() && temp)
        {
            in_pSubroutine->registerTemporary(a_pInput, out_value);
        }
        /// We are requesting an address => take address of return value if not_ a reference type
        /// (indeed addressing a reference returns the reference value itself)
        if ((a_Data.flags & e_Expression_Address) == 0)
        {
            if (pSubroutine->getReturnType()->asReference() == nullptr &&
                pSubroutine->getReturnType() != PHANTOM_TYPEOF(void))
            {
                out_value = in_pSubroutine->load(out_value);
            }
        }
    }
    else
    {
        /// RVO + temporary => register temporary for destruction
        if (temp)
        {
            in_pSubroutine->registerTemporary(a_pInput, out_value);
        }
    }
}

void CodeGeneratorPrivate::visit(Class* a_pInput, VisitorData a_Data)
{
    if (a_pInput->isTemplateDependant())
        return;

    visit(static_cast<ClassType*>(a_pInput), a_Data);
    //         lang::Method* pInitialize = a_pInput->getMethod(PHANTOM_PP_QUOTE(initialize), Types());
    //         lang::Method* pTerminate = a_pInput->getMethod(PHANTOM_PP_QUOTE(PHANTOM_TERMINATE),
    //         Types()); lang::Method* pRestore = a_pInput->getRestoreFunction(); if(pInitialize)
    //         a_pInput->setInitializeFunctionDelegate(pInitialize->getCallDelegate());
    //         if(pTerminate) a_pInput->setTerminateFunctionDelegate(pTerminate->getCallDelegate());
    //         if(pRestore) a_pInput->setRestoreFunctionDelegate(pRestore->getCallDelegate());
}

void CodeGeneratorPrivate::visit(ClassType* a_pInput, VisitorData a_Data)
{
    visit(static_cast<Type*>(a_pInput), a_Data);
}

void CodeGeneratorPrivate::visit(ConditionalExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    out_value = in_pSubroutine->getOrCreateAlloca(a_pInput, a_pInput->getValueType());
    Value             cond_ = compileExpression(in_pSubroutine, a_pInput->getConditionExpression());
    llvm::BasicBlock* true_ = llvm::BasicBlock::Create(m_LLVMContext, "true");
    llvm::BasicBlock* false_ = llvm::BasicBlock::Create(m_LLVMContext, "false");
    llvm::BasicBlock* end_ = llvm::BasicBlock::Create(m_LLVMContext, "end");
    o_ir_builder->CreateCondBr(cond_.value, true_, false_);
    true_->insertInto(in_pSubroutine->getJitFunction().function);
    o_ir_builder->SetInsertPoint(true_);
    Value then_ = compileExpression(in_pSubroutine, a_pInput->getThenExpression());
    in_pSubroutine->store(out_value, then_);
    o_ir_builder->CreateBr(end_);
    false_->insertInto(in_pSubroutine->getJitFunction().function);
    o_ir_builder->SetInsertPoint(false_);
    Value else_ = compileExpression(in_pSubroutine, a_pInput->getElseExpression());
    in_pSubroutine->store(out_value, else_);
    o_ir_builder->CreateBr(end_);
    end_->insertInto(in_pSubroutine->getJitFunction().function);
    o_ir_builder->SetInsertPoint(end_);
    if (a_Data.hasFlag(e_Expression_RValue))
        out_value = in_pSubroutine->load(out_value);
}

void CodeGeneratorPrivate::visit(ConstantExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    if (a_Data.flags & e_Expression_RValue)
    {
        llvm::Type* pLLVMType = in_pSubroutine->toJitType(a_pInput->getValueType());
        uint64_t    buffer = 0;
        if (a_pInput->getValueType()->asIntegralType())
        {
            a_pInput->getConstant()->getValue(&buffer);
            out_value = Value(llvm::ConstantInt::get(pLLVMType, buffer, a_pInput->getValueType()->isSignedInteger()),
                              a_pInput->getValueType());
        }
        else
        {
            switch (a_pInput->getValueType()->getTypeKind())
            {
            case TypeKind::Float:
            {
                float buffer;
                a_pInput->getConstant()->getValue(&buffer);
                out_value = Value(llvm::ConstantFP::get(pLLVMType, buffer), a_pInput->getValueType());
            }
            break;
            case TypeKind::Double:
            {
                double buffer;
                a_pInput->getConstant()->getValue(&buffer);
                out_value = Value(llvm::ConstantFP::get(pLLVMType, buffer), a_pInput->getValueType());
            }
            break;
            case TypeKind::LongDouble:
            {
                longdouble buffer;
                a_pInput->getConstant()->getValue(&buffer);
                out_value = Value(llvm::ConstantFP::get(pLLVMType, buffer), a_pInput->getValueType());
            }
            break;
            case TypeKind::Pointer:
            {
                size_t buffer;
                a_pInput->getConstant()->getValue(&buffer);
                out_value = Value(in_pSubroutine->createVoidPtrConstant((void*)buffer).value, a_pInput->getValueType());
            }
            break;
            case TypeKind::NullPtr:
            {
                size_t buffer;
                a_pInput->getConstant()->getValue(&buffer);
                out_value = Value(in_pSubroutine->createVoidPtrConstant((void*)buffer).value, a_pInput->getValueType());
            }
            break;
            default:
                PHANTOM_ASSERT(false);
            }
        }
    }
    else
    {
        out_value =
        in_pSubroutine->convert(in_pSubroutine->createVoidPtrConstant((void*)a_pInput->getConstant()->getAddress()),
                                a_pInput->getValueType()->addPointer());
    }
}

void CodeGeneratorPrivate::visit(ConstructorCallExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];

    phantom::SmallVector<Value> arguments;
    size_t                      argCount = a_pInput->getConstructor()->getParameters().size();

    phantom::SmallVector<Expression*> args(a_pInput->beginArguments(), a_pInput->endArguments());
    // Evaluate arguments from right to left
    for (auto it = args.rbegin(); it != args.rend(); ++it)
    {
        Type* pParamType = a_pInput->getConstructor()->getParameterType(--argCount);
        Value argumentValue = compileArgument(in_pSubroutine, pParamType, *it);
        PHANTOM_ASSERT(!argumentValue.isNull());
        arguments.insert(arguments.begin(),
                         argumentValue); /// insert at beginning to keep left-to-right order
    }

    bool noOut = out_value.isNull();

    /// temporary
    if (noOut) /// In place or_ temp ?
    {
        out_value = in_pSubroutine->getOrCreateAlloca(a_pInput, a_pInput->getConstructor()->getOwner()->asType());
        arguments.insert(arguments.begin(), out_value);
        in_pSubroutine->registerTemporary(a_pInput, out_value);
    }
    else
    {
        arguments.insert(arguments.begin(), out_value);
    }

    // invoke constructor
    in_pSubroutine->callSubroutine(a_pInput->getConstructor(), arguments.data(), arguments.size(), 0);

    // if native constructor, use this moment to extract virtual method tables
    if (a_pInput->getConstructor()->isNative() && a_pInput->getConstructor()->getOwner())
    {
        for (auto pVtable : a_pInput->getConstructor()->getOwnerClass()->getVirtualMethodTables())
        {
            Value vtable_ptr = in_pSubroutine->convert(in_pSubroutine->createVoidPtrConstant(pVtable),
                                                       m_pVirtualMethodTableClass->makePointer());

            phantom::SmallVector<Value> vtableCallArgs;
            vtableCallArgs.push_back(vtable_ptr);
            vtableCallArgs.push_back(out_value); // pointer to constructed instance
            in_pSubroutine->callNative(m_pExtractNativeVTableMeth, vtableCallArgs.data(), 2, true);
        }
    }

    if (noOut && (a_Data.flags & e_Expression_Address) == 0)
    {
        /// value is stored in a reference => life time is until the end of the block
        out_value = in_pSubroutine->load(out_value);
    }
}

void CodeGeneratorPrivate::visit(ControlStatement* a_pInput, VisitorData a_Data)
{
    visit(static_cast<Statement*>(a_pInput), a_Data);
    for (auto pStmt : a_pInput->getRAIIDestructions())
    {
        pStmt->visit(this, a_Data);
    }
}

void CodeGeneratorPrivate::visit(FieldExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    Value       left = compileExpression(in_pSubroutine, a_pInput->getObjectExpression(), e_Expression_Address);
    PHANTOM_ASSERT(left.type->asReference() != nullptr);
    Type* pFieldType = a_pInput->getField()->getValueType();
    if (a_Data.flags & e_Expression_RValue)
    {
        if (Array* pArr = pFieldType->asArray())
        {
            PHANTOM_ASSERT(false);
            //             out_value =
            //             in_pSubroutine->adjustPointer(left, a_pInput->getField()->getOffset(),
            //             pArr->getUnderlyingType()->removeAllExtents()->makePointer());
        }
        else if (pFieldType->asReference())
        {
            // we finally load this X* to have X (as we are asked for an rvalue)
            out_value = in_pSubroutine->load(
            // we load this double ref X** value to have X* value
            in_pSubroutine->load(
            // we make a fake adjustment of class ptr to field type X & & (but ref to ref does not exist, to replace by
            // X**)
            in_pSubroutine->adjustPointer(left, a_pInput->getField()->getOffset(),
                                          pFieldType->getUnderlyingType()->addPointer()->addPointer())));
        }
        else
        {
            out_value = in_pSubroutine->loadRelative(left, a_pInput->getField()->getOffset(),
                                                     a_pInput->getValueType()->removeReference());
        }
    }
    else
    {
        Type* pAdjutAddrType = a_pInput->getValueType();
        if (Array* pArr = pAdjutAddrType->removeReference()->asArray()) // at this point we have someting like X(&)[N]
        {
            pAdjutAddrType = pAdjutAddrType->removeReference()->removeAllExtents()->addPointer();
        }
        if (pFieldType->asReference())
        {
            // we make a fake adjustment of class ptr to field type X & & (but ref to ref does not exist, so  we replace
            // by X**)
            auto adjustedThis = in_pSubroutine->adjustPointer(
            left, a_pInput->getField()->getOffset(), pFieldType->getUnderlyingType()->addPointer()->addPointer());

            if ((a_Data.flags & e_Expression_KeepRefAddress) == 0)
                // we load this double ref value to have X*
                out_value = in_pSubroutine->load(adjustedThis);
            else
                out_value = adjustedThis;
            // we want a reference as output, at least for debug (at the code gen level, ref or pointers are the same)
        }
        else
        {
            out_value = in_pSubroutine->adjustPointer(left, a_pInput->getField()->getOffset(), pAdjutAddrType);
        }
        PHANTOM_ASSERT(out_value.type->asPointer() || out_value.type->asLValueReference());
        out_value.type = out_value.type->getUnderlyingType()->addLValueReference();
    }
}

void CodeGeneratorPrivate::visit(FieldInitializationStatement* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    Value       this_ = in_pSubroutine->getThis();
    Type*       pFieldType = a_pInput->getField()->getValueType();
    Expression* pInitExp = a_pInput->getInitializationExpression();
    Value       adjust =
    in_pSubroutine->adjustPointer(this_, ptrdiff_t(a_pInput->getField()->getOffset()), pFieldType->addPointer());

    if (pFieldType->asReference())
    {
        adjust = in_pSubroutine->adjustPointer(this_, ptrdiff_t(a_pInput->getField()->getOffset()),
                                               pFieldType->getUnderlyingType()->makePointer()->makePointer());
        in_pSubroutine->store(adjust, compileExpression(in_pSubroutine, pInitExp, e_Expression_Address));
    }
    else
    {
        adjust =
        in_pSubroutine->adjustPointer(this_, ptrdiff_t(a_pInput->getField()->getOffset()), pFieldType->addPointer());
        if (pFieldType->asClassType() || pFieldType->asArray())
        {
            compileExpression(in_pSubroutine, pInitExp, adjust);
        }
        else
        {
            in_pSubroutine->store(adjust, compileExpression(in_pSubroutine, pInitExp, e_Expression_RValue));
        }
    }
    visit(static_cast<Statement*>(a_pInput), a_Data);
    popDebugPos(in_pSubroutine, a_pInput);
}

void CodeGeneratorPrivate::visit(FieldPointerExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    PHANTOM_ASSERT(false);
}

void CodeGeneratorPrivate::visit(DeallocateExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value       count = a_pInput->getSizeExpression()
    ? compileExpression(in_pSubroutine, a_pInput->getSizeExpression(), e_Expression_RValue)
    : in_pSubroutine->createSizeTConstant(1);
    Value ptr = compileExpression(in_pSubroutine, a_pInput->getAddressExpression(), e_Expression_RValue);
    Value args[] = {in_pSubroutine->createPtrConstant(a_pInput->getType()),
                    in_pSubroutine->convert(ptr, PHANTOM_TYPEOF(void*)), count};
    in_pSubroutine->callSubroutine(PHANTOM_CLASSOF(Type)->getMethodCascade("deallocate(void*,size_t) const"), args, 3,
                                   0);
}

void CodeGeneratorPrivate::visit(DeleteExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    if (a_pInput->getClass())
    {
        Value class_ptr =
        in_pSubroutine->convert(in_pSubroutine->createVoidPtrConstant(a_pInput->getClass()), PHANTOM_TYPEOF(Class*));
        Value args[2] = {
        class_ptr,
        in_pSubroutine->convert(compileExpression(in_pSubroutine, a_pInput->getExpression()), PHANTOM_TYPEOF(void*))};
        // FIXME : create signature cache (leaking here)
        Signature* pSignature = Signature::Create(a_pInput->getSource(), PHANTOM_TYPEOF(void), PHANTOM_TYPEOF(void*));
        in_pSubroutine->callNativePtr("&Class::deleteInstance",
                                      phantom::Closure(phantom::MethodClosure(&Class::deleteInstance)).address,
                                      ABI::MethodCall, pSignature->getFunctionType(), PHANTOM_CLASSOF(Class), args, 2);
    }
    else
    {
        PHANTOM_ASSERT_FORBIDDEN_CALL();
    }
}

void CodeGeneratorPrivate::visit(Expression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    {
        PHANTOM_ASSERT(false);
    }
}

void CodeGeneratorPrivate::visit(ExpressionStatement* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    compileExpression(in_pSubroutine, a_pInput->getExpression());
    visit(static_cast<Statement*>(a_pInput), a_Data);
}

void CodeGeneratorPrivate::visit(lang::Function* a_pInput, VisitorData a_Data)
{
    Function* pJitFunction = phantom::new_<Function>(a_pInput);
    pJitFunction->setCodeGenerator(this);
    if (a_pInput->getBlock())
        queueBlock(a_pInput->getBlock(), pJitFunction);
}

void CodeGeneratorPrivate::visit(IdentityExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    a_pInput->getExpression()->visit(this, a_Data);
    if (a_pInput->getValueType()->removeQualifiers() != a_pInput->getExpression()->getValueType()->removeQualifiers())
        if (out_value.type != a_pInput->getValueType())
        {
            out_value = in_pSubroutine->convert(out_value, a_pInput->getValueType());
            uint8_t l0 = out_value.type->getAddressLevel();
            uint8_t l1 = a_pInput->getValueType()->getAddressLevel();
            PHANTOM_ASSERT((l0 == l1) || (l0 && (l1 == 0xff)) || (l1 && (l0 == 0xff)));
        }
}

void CodeGeneratorPrivate::visit(InitializerListExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];

    pushDebugPos(in_pSubroutine, a_pInput);

    Class* pInitializerListType = static_cast<Class*>(a_pInput->getValueType());
    Type*  pContentType = a_pInput->getContentType();
    size_t stride = pContentType->getSize();

    Array* pRelatedArrayType = a_pInput->getContentType()->makeArray(a_pInput->getExpressions().size());

    Value begin = in_pSubroutine->getOrCreateAlloca(a_pInput, pRelatedArrayType, 0);

    // zero initialize the list
    in_pSubroutine->memset(begin, in_pSubroutine->createIntConstant(0),
                           in_pSubroutine->createSizeTConstant(pRelatedArrayType->getSize())); // zero init first

    // construct elements
    auto const& expressions = a_pInput->getExpressions();
    size_t      c = 0;
    for (size_t i = 0; i < expressions.size(); ++i)
    {
        if (Expression* pExp = expressions[i])
        {
            size_t offset = i * stride;
            auto   pType = pContentType->addPointer();
            Value  adjust = (offset == 0)
            ? Value(o_ir_builder->CreatePointerCast(begin.value, in_pSubroutine->toJitType(pType)), pType)
            : in_pSubroutine->loadElemAddress(begin, i, pType);
            if (pContentType->removeQualifiers()->asClassType())
            {
                compileExpression(in_pSubroutine, pExp, adjust);
            }
            else
            {
                in_pSubroutine->store(adjust, compileExpression(in_pSubroutine, pExp));
            }
        }
    }

    // reserve std::initializer_list space
    if (out_value.isNull())
        out_value = in_pSubroutine->getOrCreateAlloca(a_pInput, pInitializerListType, 1);

    Type* pCtorParmType = pContentType->addConst()->addPointer();

    Value end = in_pSubroutine->loadElemAddress(begin, pRelatedArrayType->getItemCount(), pContentType->addPointer());

    Value args[3] = {out_value, begin, end};

    if (a_pInput->isTemporary())
        in_pSubroutine->registerTemporary(a_pInput, begin);

    // set begin and end fields (as supposed to stand in memory, not really standard compliant)

    in_pSubroutine->storeRelative(out_value, 0, in_pSubroutine->convert(begin, pContentType->addPointer()));
    in_pSubroutine->storeRelative(out_value, sizeof(void*), end);

    // that's all !
    popDebugPos(in_pSubroutine, a_pInput);
}

void CodeGeneratorPrivate::visit(lang::Label* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    pushDebugPos(in_pSubroutine, a_pInput);
    Value&     out_value = *(Value*)a_Data.out[0];
    CaseLabel* pLabel = caseLabel(a_pInput);
    in_pSubroutine->label(&(pLabel->m_jit_Label));
    visit(static_cast<Statement*>(a_pInput), a_Data);
    popDebugPos(in_pSubroutine, a_pInput);
}

void CodeGeneratorPrivate::visit(LanguageElement* a_pInput, VisitorData a_Data) {}

void CodeGeneratorPrivate::visit(LoadExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    if (out_value.isNull())
    {
        if (a_Data.flags & e_Expression_RValue)
        {
            if (a_pInput->getValueType()->asArray())
            {
                out_value = compileExpression(in_pSubroutine, a_pInput->getLoadedExpression(), e_Expression_LValue);
            }
            else
            {
                out_value = compileExpression(in_pSubroutine, a_pInput->getLoadedExpression(), e_Expression_Address);
                PHANTOM_ASSERT(out_value.type->asReference());
                out_value = in_pSubroutine->load(out_value);
            }
        }
        else
        {
            out_value =
            compileExpression(in_pSubroutine, a_pInput->getLoadedExpression(), (ExpressionFlags)a_Data.flags);
        }
    }
    else
    {
        Value result = compileExpression(in_pSubroutine, a_pInput->getLoadedExpression(), e_Expression_Address);
        in_pSubroutine->store(out_value, in_pSubroutine->load(result));
    }
}

void CodeGeneratorPrivate::visit(lang::LocalVariable* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];

    (phantom::new_<LocalVariable>(in_pSubroutine, a_pInput, a_pInput->isThis() ? in_pSubroutine->getThis() : Value()))
    ->setCodeGenerator(this);
}

void CodeGeneratorPrivate::visit(LocalVariableExpression* a_pInput, VisitorData a_Data)
{
    Subroutine*          in_pSubroutine = (Subroutine*)a_Data.in[0];
    lang::LocalVariable* pLocalVar = a_pInput->getLocalVariable();
    if (!pLocalVar)
        pLocalVar = static_cast<lang::Method*>(in_pSubroutine->getSubroutine())->getThis();
    PHANTOM_ASSERT(pLocalVar);

    Value& out_value = *(Value*)a_Data.out[0];
    Value  localValue;
    auto   pJitLocalVar = static_cast<LocalVariable*>(getData(pLocalVar));
    PHANTOM_ASSERT(pJitLocalVar);
    localValue = pJitLocalVar->getValue();
    bool isLocalReference = pLocalVar && (pLocalVar->getValueType()->asReference() != nullptr);
    bool isStructParameterByPtr = pLocalVar && pLocalVar->asParameter() &&
    GetArgumentPassingMode(pLocalVar->getValueType()) == ArgumentPassingMode::ByPointer;
    if (out_value.isNull())
    {
        if (pLocalVar->asParameter() && pLocalVar->getValueType()->asArray())
        {
            PHANTOM_ASSERT((a_Data.flags & (e_Expression_Address | e_Expression_LValue)));
            out_value = in_pSubroutine->load(localValue);
        }
        else if (a_Data.flags & e_Expression_Address)
        {
            if (isStructParameterByPtr)
            // structs are passed by pointer if size > pointer size
            {
                out_value = localValue;
                PHANTOM_ASSERT(out_value.type->asAddressType());
                out_value.setType(a_pInput->getValueType());
            }
            else if (isLocalReference)
            {
                if ((a_Data.flags & e_Expression_KeepRefAddress))
                {
                    out_value = localValue;
                    PHANTOM_ASSERT(out_value.type->asAddressType());
                    out_value.setType(a_pInput->getValueType()->addLValueReference());
                }
                else
                {
                    out_value = in_pSubroutine->load(localValue);
                    PHANTOM_ASSERT(out_value.type->asAddressType());
                    out_value.setType(a_pInput->getValueType());
                }
            }
            else
            {
                out_value = localValue;
            }
        }
        else if (a_Data.flags & e_Expression_LValue)
        {
            if (isStructParameterByPtr)
            // structs are passed by pointer if size > pointer size
            {
                out_value = localValue;
                PHANTOM_ASSERT(out_value.type->asPointer());
                out_value.setType(a_pInput->getValueType());
            }
            else if (isLocalReference)
            {
                out_value = in_pSubroutine->load(localValue);
                PHANTOM_ASSERT(out_value.type->asPointer());
                out_value.setType(a_pInput->getValueType());
            }
            else
            {
                out_value = localValue;
            }
        }
        else
        {
            PHANTOM_ASSERT(!isStructParameterByPtr, "we should never ask a struct as r-value");
            out_value = localValue;
            if (isLocalReference)
            {
                out_value = in_pSubroutine->load(out_value);
            }
            if (pLocalVar) // not_ 'this'
                out_value = in_pSubroutine->load(out_value);
            out_value.setType(a_pInput->getValueType()->removeReference()->removeQualifiers());
        }
    }
    else
    {
        PHANTOM_ASSERT(!isStructParameterByPtr, "we should never ask a struct as r-value");
        PHANTOM_ASSERT(localValue.type->removeReference()->removeQualifiers()->asClass() == nullptr,
                       "E.S.T build has a missing user defined conversion (constructor/conversion function)");
        Value in_value =
        isLocalReference ? in_pSubroutine->load(in_pSubroutine->load(localValue)) : in_pSubroutine->load(localValue);
        in_value = in_pSubroutine->convert(in_value, out_value.type->removePointer());
        PHANTOM_VERIFY(in_pSubroutine->store(out_value, in_value).value);
    }
}

void CodeGeneratorPrivate::visit(LocalVariableInitializationStatement* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    pushDebugPos(in_pSubroutine, a_pInput);
    Value& out_value = *(Value*)a_Data.out[0];

    lang::LocalVariable* pLocalVariable = a_pInput->getLocalVariable();
    LocalVariable*       pJitLocalVariable = static_cast<LocalVariable*>(getData(pLocalVariable));
    bool                 isRef = a_pInput->getLocalVariable()->getValueType()->asReference() != nullptr;
    if (a_pInput->getLocalVariable()->getValueType()->asClassType() == nullptr &&
        a_pInput->getLocalVariable()->getValueType()->asArray() == nullptr)
    {
        Value v = compileExpression(in_pSubroutine, a_pInput->getInitializationExpression(), Value(),
                                    isRef ? e_Expression_Address : e_Expression_RValue);
        PHANTOM_VERIFY(in_pSubroutine->store(pJitLocalVariable->getValue(), v).value, "");
        if (a_pInput->isTemporaryContainer())
        {
            in_pSubroutine->registerTemporary(a_pInput->getInitializationExpression()->removeRValueToLValueExpression(),
                                              v);
        }
    }
    else
    {
        Value v =
        compileExpression(in_pSubroutine, a_pInput->getInitializationExpression(), pJitLocalVariable->getValue());
    }
    visit(static_cast<Statement*>(a_pInput), a_Data);
    popDebugPos(in_pSubroutine, a_pInput);
}

// void CodeGeneratorPrivate::visit(LValueExpression* a_pInput, VisitorData a_Data)
// {
//     Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
//     Value& out_value = *(Value*)a_Data.out[0];
// }
//

// void CodeGeneratorPrivate::visit(LValueInitializationStatement*a_pInput, VisitorData
// a_Data)
// {
//     Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
//     Value& out_value = *(Value*)a_Data.out[0];
// }

void CodeGeneratorPrivate::visit(lang::Method* a_pInput, VisitorData a_Data)
{
    if (a_pInput->testModifiers(Modifier::Deleted)) // skip deleted functions
        return;
    Method* pJitMethod = phantom::new_<Method>(a_pInput);
    pJitMethod->setCodeGenerator(this);
    if (a_pInput->getBlock())
    {
        PHANTOM_ASSERT(a_pInput->getOwnerClassType());
        Methods originalOverriddenMethods;
        a_pInput->getOriginalOverriddenMethods(originalOverriddenMethods);
        // if(originalOverriddenMethods.size() > 1) // more than one root overridden method => thunk
        // adjustor needed
        {
            // MSVC convention : generate thunks if more than 1 override (because ambiguity on how
            // to adjust this pointer in last override)
            for (auto it = originalOverriddenMethods.begin(); it != originalOverriddenMethods.end(); ++it)
            {
                lang::Method* pOverridden = *it;
                size_t uiOffset = a_pInput->getOwnerClass()->getBaseClassOffsetCascade(pOverridden->getOwnerClass());
                if (uiOffset != 0)
                {
                    pJitMethod->compileThisAdjustementThunk(uiOffset);
                }
            }
        }
        //             else if(originalOverriddenMethods.size() == 1)
        //             {
        //                 // MSVC convention : readjust this pointer inside the function code if
        //                 only one override if(originalOverriddenMethods.back() != a_pInput)
        //                 {
        //                     size_t uiOffset =
        //                     a_pInput->getOwnerClass()->getBaseClassOffsetCascade(originalOverriddenMethods.back()->getOwnerClass());
        //                     if(uiOffset != 0)
        //                     {
        //                         pJitMethod->store(pJitMethod->getThis(),
        //                             pJitMethod->adjustPointer(pJitMethod->getThis(),
        //                             pJitMethod->createIntConstant(-(int)uiOffset),
        //                             pJitMethod->getThis().type)
        //                         );
        //                     }
        //                 }
        //             }
        queueBlock(a_pInput->getBlock(), pJitMethod);
    }
    else
    {
        PHANTOM_ASSERT(a_pInput->isPureVirtual() || a_pInput->testModifiers(Modifier::Deleted));
    }
}

void CodeGeneratorPrivate::visit(MethodPointerCallExpression* a_pInput, VisitorData a_Data)
{
    Subroutine*               in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&                    out_value = *(Value*)a_Data.out[0];
    size_t                    i = a_pInput->getArguments().size();
    std::vector<llvm::Value*> args;
    while (i--)
    {
        Value argumentValue = compileExpression(in_pSubroutine, a_pInput->getArguments()[i]);
        args.insert(args.begin(), argumentValue.value);
    }
    args.insert(args.begin(),
                compileExpression(in_pSubroutine, a_pInput->getObjectExpression(), e_Expression_Address).value);

    Value func = compileExpression(in_pSubroutine, a_pInput->getMemberExpression(), e_Expression_RValue);

    Value returnedValue;

    llvm::CallInst* pCall = o_ir_builder->CreateCall(
    in_pSubroutine->toJitFunctionCallType(
    a_pInput->getMethodPointer()->getFunctionType(),
    static_cast<ClassType*>(a_pInput->getObjectExpression()->getValueType()->removeReference()->removeQualifiers())),
    func.value, args);
    pCall->setCallingConv(in_pSubroutine->toJitABI(ABI::MethodCall));
    out_value = Value(pCall, a_pInput->getMethodPointer()->getReturnType());
}

void CodeGeneratorPrivate::visit(Symbol* a_pInput, VisitorData a_Data)
{
    if (a_pInput->isTemplateDependant())
        return;
    for (size_t i = 0; i < a_pInput->getElements().size(); ++i)
    {
        LanguageElement* pElem = a_pInput->getElements()[i];
        if (!(pElem->isNative()) && (a_Data.id == CodeGenerator::e_Pass_Blocks || pElem->asSymbol()))
            pElem->visit(this, a_Data);
    }
}

void CodeGeneratorPrivate::visit(Parameter* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    size_t      index = 0;
    Signature*  pSignature = in_pSubroutine->getSignature();
    for (Parameter* pParam : pSignature->getParameters())
    {
        if (pParam == a_pInput)
            break;
        index++;
    }
    (phantom::new_<LocalVariable>(in_pSubroutine, a_pInput, in_pSubroutine->getParameter(index)))
    ->setCodeGenerator(this);
}

void CodeGeneratorPrivate::visit(PlacementNewExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    Value       place = compileExpression(in_pSubroutine, a_pInput->getPlaceExpression());
    Value       args[1] = {place};
    Value       temp = compileExpression(in_pSubroutine, a_pInput->getConstructionExpression(), place);
    out_value = place;
}

void CodeGeneratorPrivate::visit(PointerAdjustmentExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    Expression* pExp = a_pInput->getPointerExpression();
#pragma message("TODO : maybe optimize this (search for static_cast implementations in clang/msvc compilers)")
    Value pointer = compileExpression(in_pSubroutine, pExp,
                                      pExp->getValueType()->asReference() ? e_Expression_Address : e_Expression_RValue);
    Value offset = in_pSubroutine->createPtrDiffTConstant(a_pInput->getOffset());
    Value is_not_null = Value(o_ir_builder->CreateSExt(o_ir_builder->CreateIsNotNull(pointer.value),
                                                       in_pSubroutine->toJitType(PHANTOM_TYPEOF(ptrdiff_t))),
                              PHANTOM_TYPEOF(ptrdiff_t));
    offset = in_pSubroutine->and_(is_not_null, offset);
    out_value = in_pSubroutine->adjustPointer(pointer, offset, a_pInput->getValueType());
    if ((a_Data.flags & (e_Expression_Address | e_Expression_LValue)) &&
        a_pInput->getValueType()->getTypeKind() == TypeKind::Pointer)
    {
        auto tmp = in_pSubroutine->getOrCreateAlloca(a_pInput, a_pInput->getValueType(), 0);
        in_pSubroutine->store(tmp, out_value);
        out_value = tmp;
    }
}

void CodeGeneratorPrivate::visit(PropertyExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value       left = compileExpression(in_pSubroutine, a_pInput->getObjectExpression(), e_Expression_Address);

    Value leftTemp = in_pSubroutine->getOrCreateAlloca(a_pInput, left.type, 0);
    in_pSubroutine->store(leftTemp, left);

    Value get_caller = in_pSubroutine->load(leftTemp);

    PHANTOM_ASSERT(!left.isNull());
    PHANTOM_ASSERT(a_pInput->getProperty()->getGet());
    bool   bRVO = a_pInput->getProperty()->getGet()->isRVOCandidate();
    Value& out_value = *(Value*)a_Data.out[0];
    Class* pPropertyRawClass = a_pInput->getValueType()->asClass();
    Class* pPropertyClass = a_pInput->getValueType()->removeReference()->removeQualifiers()->asClass();

    out_value = in_pSubroutine->getOrCreateAlloca(
    a_pInput, a_pInput->getProperty()->getGet()->getReturnType()->removeReference()->removeQualifiers(), 1);
    if (!bRVO)
    {
        Value callResult = in_pSubroutine->callSubroutine(a_pInput->getProperty()->getGet(), &get_caller, 1, 0);
        if (pPropertyRawClass)
            callResult =
            in_pSubroutine->returnAddress(pPropertyRawClass); // if raw class return, we remember the return
                                                              // address for later destructor call on it
        if (pPropertyClass)                                   // if a class, call copy constructor (must be provided)
        {
            lang::Constructor* pCtor = pPropertyClass->getCopyConstructor();
            PHANTOM_ASSERT(pCtor);
            Value args[2] = {out_value, callResult};
            in_pSubroutine->callSubroutine(pCtor, args, 2, 0);
            if (pPropertyRawClass)
            {
                in_pSubroutine->callSubroutine(pPropertyClass->getDestructor(), &callResult, 1,
                                               0); // destroy return buffer
            }
        }
        else // else, just make a raw copy
        {
            in_pSubroutine->store(out_value, callResult);
        }
    }
    else
    {
        Value args[2] = {get_caller, out_value};
        in_pSubroutine->callSubroutine(a_pInput->getProperty()->getGet(), args, 2, 0);
    }

    if (a_Data.flags & (e_Expression_Address | e_Expression_LValue))
    {
        pushProperty(a_pInput, leftTemp, out_value, true);
    }
    else if (a_Data.flags & e_Expression_RValue)
    {
        if (pPropertyRawClass == nullptr)
            out_value = in_pSubroutine->load(out_value);
        pushProperty(a_pInput, leftTemp, out_value, false);
    }
}

void CodeGeneratorPrivate::visit(ReturnStatement* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    pushDebugPos(in_pSubroutine, a_pInput);
    Value& out_value = *(Value*)a_Data.out[0];
    bool   bRVO = in_pSubroutine->getSubroutine()->isRVOCandidate();
    if (bRVO)
    {
        compileExpression(in_pSubroutine, a_pInput->getExpression(), in_pSubroutine->getRVOParameter());
        visit(static_cast<Statement*>(a_pInput), a_Data);
        for (auto pStmt : a_pInput->getRAIIDestructions())
        {
            pStmt->visit(this, a_Data);
        }
        in_pSubroutine->returnValue(in_pSubroutine->getRVOParameter()); // RVO returns its own parameter
        popDebugPos(in_pSubroutine, a_pInput);
        return;
    }
    Value ret;
    if (auto pExp = a_pInput->getExpression())
    {
        bool retRef = in_pSubroutine->getSubroutine()->getReturnType()->asReference() != nullptr;
        ret = compileExpression(in_pSubroutine, a_pInput->getExpression(),
                                retRef ? e_Expression_LValue : e_Expression_RValue);
    }

    PHANTOM_ASSERT(ret.type == nullptr ||
                   ((ret.type->asReference() != nullptr) ==
                    (in_pSubroutine->getSubroutine()->getReturnType()->asReference() != nullptr)));
    visit(static_cast<ControlStatement*>(a_pInput), a_Data);
    setCurrentDebugPos(in_pSubroutine, a_pInput->getSubroutine()->getCodeRange().end);
    if (ret.value && !ret.type->isVoid())
    {
        in_pSubroutine->returnValue(ret);
    }
    else
    {
        in_pSubroutine->returnVoid();
    }
    popDebugPos(in_pSubroutine, a_pInput);
}

void CodeGeneratorPrivate::visit(RValueToConstLValueExpression* a_pInput, VisitorData a_Data)
{
    a_pInput->getRValueExpression()->visit(this, a_Data);
}

void CodeGeneratorPrivate::visit(RValueReferenceExpression* a_pInput, VisitorData a_Data)
{
    a_pInput->getRValueExpression()->visit(this, a_Data);
}

void CodeGeneratorPrivate::visit(Scope* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
}

void CodeGeneratorPrivate::visit(Source* a_pInput, VisitorData a_Data)
{
    visit(static_cast<Symbol*>(a_pInput), a_Data);
}

void CodeGeneratorPrivate::visit(Statement* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];

    /// Destroy class temporaries
    for (auto pTemp : a_pInput->getScopedDestructions())
    {
        pTemp->visit(this, a_Data);
    }
}

void CodeGeneratorPrivate::visit(MemCopyStatement* a_pInput, VisitorData a_Data)
{
    Subroutine*            in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&                 out_value = *(Value*)a_Data.out[0];
    in_pSubroutine->memcpy PHANTOM_PREVENT_MACRO_SUBSTITUTION(
    compileExpression(in_pSubroutine, a_pInput->getDestinationAddressExpression()),
    compileExpression(in_pSubroutine, a_pInput->getSourceAddressExpression()),
    in_pSubroutine->createSizeTConstant(a_pInput->getSize()));
    visit(static_cast<Statement*>(a_pInput), a_Data);
}

void CodeGeneratorPrivate::visit(NewExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    Class*      pClass = a_pInput->getValueType()->removePointer()->asClass();
    PHANTOM_ASSERT(pClass);
    Value         void_ptr = in_pSubroutine->createVoidPtrConstant(pClass);
    Value         class_ptr = in_pSubroutine->convert(void_ptr, PHANTOM_TYPEOF(Class)->addPointer());
    FunctionType* pFuncType = a_pInput->getSource()->functionType(PHANTOM_TYPEOF(void*), TypesView{});
    void* (Class::*allocate_ptr)() const = &Class::allocate;
    out_value =
    in_pSubroutine->callNativePtr("&Class::allocate", phantom::Closure(phantom::MethodClosure(allocate_ptr)).address,
                                  ABI::MethodCall, pFuncType, PHANTOM_CLASSOF(Class), &class_ptr, 1);

    Value conv_out_value = in_pSubroutine->convert(out_value, pClass->addPointer());
    /// CONSTRUCTOR
    compileExpression(in_pSubroutine, a_pInput->getInitializationExpression(), conv_out_value);

    out_value = conv_out_value;
}

void CodeGeneratorPrivate::visit(StringLiteralExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    if (a_Data.flags & e_Expression_RValue)
    {
        out_value = in_pSubroutine->createVoidPtrConstant((void*)a_pInput->getString());
        out_value.setType(PHANTOM_TYPEOF(const char*));
    }
    else
    {
        out_value = in_pSubroutine->createVoidPtrConstant((void**)a_pInput->getAddress());
        out_value.setType(PHANTOM_TYPEOF(const char* const&));
    }
}

void CodeGeneratorPrivate::visit(lang::Subroutine* a_pInput, VisitorData a_Data)
{
    Subroutine* pJitSubroutine = phantom::new_<Subroutine>(a_pInput);
    pJitSubroutine->setCodeGenerator(this);
    if (a_pInput->getBlock())
        queueBlock(a_pInput->getBlock(), pJitSubroutine);
}

void CodeGeneratorPrivate::visit(SubroutinePointerExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    auto        found = m_LLVMFunctionMap.find(a_pInput->getSubroutine());
    if (found != m_LLVMFunctionMap.end())
    {
        out_value = Value(found->second.function, a_pInput->getValueType());
    }
    else
    {
        PHANTOM_ASSERT(a_pInput->getSubroutine()->getClosure().address);
        out_value =
        in_pSubroutine->convert(in_pSubroutine->createVoidPtrConstant(a_pInput->getSubroutine()->getClosure().address),
                                a_pInput->getValueType());
    }
}

void CodeGeneratorPrivate::visit(SymbolReferenceExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    out_value =
    in_pSubroutine->convert(in_pSubroutine->createVoidPtrConstant(a_pInput->getSymbol()), a_pInput->getValueType());
}

void CodeGeneratorPrivate::visit(TemplateSpecialization* a_pInput, VisitorData a_Data)
{
    if (a_pInput->isFull())
    {
        a_pInput->getTemplated()->visit(this, a_Data);
    }
}

void CodeGeneratorPrivate::visit(TemporaryObjectDestructionExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Expression* pExp = a_pInput->getExpression();

    Value  tempAddress = in_pSubroutine->takeTemporary(pExp);
    Class* pClass = pExp->getValueType()->removeRValueReference()->removeQualifiers()->asClass();
    in_pSubroutine->callSubroutine(pClass->getDestructor(), &tempAddress, 1, 0, false);
    ;
}

void CodeGeneratorPrivate::visit(VariableExpression* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    Value&      out_value = *(Value*)a_Data.out[0];
    out_value.value =
    o_ir_builder->CreatePointerCast(in_pSubroutine->createVoidPtrConstant(a_pInput->getVariable()->getAddress()).value,
                                    in_pSubroutine->toJitType(a_pInput->getValueType()));
    out_value.setType(a_pInput->getValueType());
}

void CodeGeneratorPrivate::visit(VirtualMethodTableSetupStatement* a_pInput, VisitorData a_Data)
{
    Subroutine* in_pSubroutine = (Subroutine*)a_Data.in[0];
    PHANTOM_ASSERT(a_pInput->getTable());
    Value         table_ptr = in_pSubroutine->createPtrConstant(a_pInput->getTable());
    Value         this_ = in_pSubroutine->getThis();
    FunctionType* pFuncType = a_pInput->getSource()->functionType(PHANTOM_TYPEOF(void), PHANTOM_TYPEOF(void*));
    Value         args[2] = {table_ptr, in_pSubroutine->convert(this_, PHANTOM_TYPEOF(void*))};
    in_pSubroutine->callNativePtr(in_pSubroutine->getMangledName(CppManglerFlag::Thunk, 0).c_str(),
                                  phantom::Closure(phantom::MethodClosure(&VirtualMethodTable::construct)).address,
                                  ABI::MethodCall, pFuncType, PHANTOM_CLASSOF(VirtualMethodTable), args, 2);
}

void CodeGeneratorPrivate::pushPropertyStack()
{
    m_PropertyExpressionPairs.push_back(phantom::new_<PropertyAccessPairs>());
}

void CodeGeneratorPrivate::pushProperty(PropertyExpression* a_pInput, Value a_LHS, Value a_Value,
                                        bool a_requiresSetCall)
{
    static_cast<PropertyAccessPairs*>(m_PropertyExpressionPairs.back())
    ->push_back(PropertyAccessPair{a_pInput, a_LHS, a_Value, a_requiresSetCall});
}

void CodeGeneratorPrivate::flushProperties(Subroutine* in_pSubroutine)
{
    PropertyAccessPairs& pap = *static_cast<PropertyAccessPairs*>(m_PropertyExpressionPairs.back());
    size_t               i = pap.size();
    while (i--)
    {
        PropertyAccessPair& pair = pap[i];
        PHANTOM_ASSERT(!pair.lhsAddress.isNull());
        PropertyExpression* propertyExpression = pair.propertyExpression;
        Type*               pPropertyType = propertyExpression->getValueType()->removeReference()->removeQualifiers();
        Value               valueAddress = pair.valueAddress;
        if (pair.requiresSetCall)
        {
            Value set_caller = in_pSubroutine->load(pair.lhsAddress);
            Value arg_value;
            if (GetArgumentPassingMode(propertyExpression->getProperty()->getSet()->getParameterType(0)) !=
                ArgumentPassingMode::ByPointer)
            {
                valueAddress = in_pSubroutine->loadRelative(valueAddress, 0, valueAddress.type->removeAddress());
            }
            Value args[2] = {set_caller, valueAddress};
            in_pSubroutine->callSubroutine(propertyExpression->getProperty()->getSet(), args, 2, 0);
        }
        auto pPropertyClass =
        propertyExpression->getProperty()->getValueType()->removeReference()->removeQualifiers()->asClass();
        if (pPropertyClass)
        {
            PHANTOM_ASSERT(pair.valueAddress.type->asAddressType());
            in_pSubroutine->callSubroutine(pPropertyClass->getDestructor(), &valueAddress, 1, 0);
        }
    }
    pap.clear();
}

void CodeGeneratorPrivate::popPropertyStack()
{
    m_PropertyExpressionPairs.pop_back();
}

void CodeGeneratorPrivate::finalizeDebugInfo()
{
    if (m_Context.m_pDebugContext)
        m_Context.m_pDebugContext->finalize();

#define PHANTOM_JIT_MODULE_DUMP 0

#if PHANTOM_JIT_MODULE_DUMP
    m_Module->print(llvm::errs(), nullptr);
#endif
}

phantom::lang::Type* CodeGeneratorPrivate::getArgumentType(phantom::lang::Type* a_pParamType) const
{
    switch (GetArgumentPassingMode(a_pParamType))
    {
    case ArgumentPassingMode::ByInt:
        // a class with size < uint64 => argument passed by value, not by address
        switch (a_pParamType->getSize())
        {
        case 1:
            return PHANTOM_TYPEOF(uint8_t);
        case 2:
            return PHANTOM_TYPEOF(uint16_t);
        case 4:
            return PHANTOM_TYPEOF(uint32_t);
        case 8:
            return PHANTOM_TYPEOF(uint64_t);
        default:
            return PHANTOM_TYPEOF(uint64_t);
        }

    case ArgumentPassingMode::ByPointer:
        return a_pParamType->addPointer();

    default:
        return a_pParamType;
    }
}

Value CodeGeneratorPrivate::compileArgument(Subroutine* a_pSubroutine, Type* a_pParamType, Expression* a_pExpression)
{
    // Win64 calling convention has a particular way of passing arguments
    // - every type with size 8 16 32 or 64 are passed as integers except if they have non trivial copy
    // constructor
    Value argumentValue;
    switch (GetArgumentPassingMode(a_pParamType))
    {
    case ArgumentPassingMode::ByInt:
        // a class with size < uint64 => argument passed by value, not by address
        argumentValue = compileExpression(a_pSubroutine, a_pExpression, e_Expression_Address);
        argumentValue = a_pSubroutine->convert(argumentValue, getArgumentType(a_pParamType)->addPointer());
        argumentValue = a_pSubroutine->load(argumentValue);
        return argumentValue;

    case ArgumentPassingMode::ByPointer:
        argumentValue = compileExpression(a_pSubroutine, a_pExpression, e_Expression_Address);
        return a_pSubroutine->convert(argumentValue, a_pParamType->addPointer());

    default:
        argumentValue = compileExpression(a_pSubroutine, a_pExpression,
                                          a_pParamType->asReference() ? e_Expression_Address : e_Expression_RValue);
        if (auto arr = a_pParamType->asArray())
        {
            a_pParamType = arr->getUnderlyingType()->removeAllExtents()->makePointer();
        }
        return a_pSubroutine->convert(argumentValue, a_pParamType);
    }
}

Value CodeGeneratorPrivate::compileExpression(Subroutine* a_pSubroutine, Expression* a_pExpression,
                                              ExpressionFlags a_Flags)
{
    pushDebugPos(a_pSubroutine, a_pExpression);
    Value       value;
    VisitorData data;
    const void* in[1] = {a_pSubroutine};
    void*       out[1] = {&value};
    data.flags = a_Flags;
    data.in = in;
    data.out = out;
    if (a_Flags & e_Expression_RValue)
    {
        pushPropertyStack();
    }
    a_pExpression->visit(this, data);
    if (a_Flags & e_Expression_RValue)
    {
        popPropertyStack();
    }
    for (auto pScopedDestruction : a_pExpression->getScopedDestructions())
    {
        compileExpression(a_pSubroutine, pScopedDestruction);
    }
    popDebugPos(a_pSubroutine, a_pExpression);

    return value;
}

Value CodeGeneratorPrivate::compileExpression(Subroutine* a_pSubroutine, Expression* a_pExpression, Value pointer,
                                              ExpressionFlags a_Flags)
{
    PHANTOM_ASSERT(!a_pExpression->isTemplateDependant());
    pushDebugPos(a_pSubroutine, a_pExpression);
    VisitorData data;
    const void* in[1] = {a_pSubroutine};
    void*       out[1] = {&pointer};
    data.flags = a_Flags | e_Expression_InPlace;
    data.in = in;
    data.out = out;
    if (a_Flags & e_Expression_RValue)
    {
        pushPropertyStack();
    }
    a_pExpression->visit(this, data);
    if (a_Flags & e_Expression_RValue)
    {
        popPropertyStack();
    }

    for (auto pScopedDestruction : a_pExpression->getScopedDestructions())
    {
        compileExpression(a_pSubroutine, pScopedDestruction);
    }
    popDebugPos(a_pSubroutine, a_pExpression);
    return pointer;
}

CaseLabel* CodeGeneratorPrivate::caseLabel(lang::Label* a_pLabel)
{
    CaseLabel* pCaseLabel = static_cast<CaseLabel*>(getData(a_pLabel));
    if (pCaseLabel == nullptr)
    {
        pCaseLabel = phantom::new_<CaseLabel>(a_pLabel);
        pCaseLabel->setCodeGenerator(this);
    }
    return pCaseLabel;
}

void CodeGeneratorPrivate::registerJitFunction(FunctionEntry func, class Subroutine* a_pJitSubroutine)
{
    m_JitSubroutineMap[func] = a_pJitSubroutine;
    m_LLVMFunctionMap[a_pJitSubroutine->getSubroutine()] = func;
}

void CodeGeneratorPrivate::unregisterJitFunction(FunctionEntry func)
{
    auto found = m_JitSubroutineMap.find(func);
    m_LLVMFunctionMap.erase(found->second->getSubroutine());
    m_JitSubroutineMap.erase(found);
}

void CodeGeneratorPrivate::offsetFunctions(ptrdiff_t a_Offset)
{
    m_FunctionOffset = a_Offset;
}

} // namespace jit
} // namespace phantom

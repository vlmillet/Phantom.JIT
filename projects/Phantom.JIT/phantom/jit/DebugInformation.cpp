// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

#include "DebugInformation.h"

#include "CodeGenerator.h"
#pragma warning(push, 0)
#include "llvm/Support/LEB128.h"

#include <llvm/MC/MCAsmInfo.h>
#include <llvm/MC/MCContext.h>
#include <llvm/MC/MCDisassembler/MCDisassembler.h>
#include <llvm/MC/MCInstrAnalysis.h>
#include <llvm/MC/MCInstrInfo.h>
#include <llvm/Object/SymbolSize.h>
#include <llvm/Support/TargetRegistry.h>
#pragma warning(pop)
#include "CodeGeneratorPrivate.h"
#include "phantom/lang/Application.h"
#include "phantom/lang/Debugger.h"
#include "phantom/lang/LocalVariable.h"
#include "phantom/lang/SourceFile.h"
#include "phantom/utils/Path.h"

#include <phantom/lang/Block.h>
#include <phantom/lang/CodeLocation.h>
#include <phantom/lang/Module.h>
#include <phantom/lang/Source.h>
#include <phantom/lang/Subroutine.h>
#if PHANTOM_OPERATING_SYSTEM == PHANTOM_OPERATING_SYSTEM_WINDOWS
#    include <windows.h>
#endif
#include "phantom/lang/Compiler.h"

namespace phantom
{
namespace jit
{
using namespace phantom::lang;
class DebugInformationPrivate : public llvm::JITEventListener
{
public:
    ~DebugInformationPrivate()
    {
        delete disassembler;
        delete instrInfo;
        delete instrAnalysis;
    }

    virtual void notifyObjectLoaded(ObjectKey K, const llvm::object::ObjectFile& Obj,
                                    const llvm::RuntimeDyld::LoadedObjectInfo& L) override
    {
        if (m_public->getCodeGenerator()->m_private->m_Context.m_pDebugContext)
        {
            auto temp = llvm::object::computeSymbolSizes(Obj);
            symbolSizes.swap(temp);
            dwarfContext = m_public->getCodeGenerator()->m_private->m_pDWARFContext.get();
        }
    }
    DebugInformation*                                         m_public = nullptr;
    llvm::DWARFContext*                                       dwarfContext = nullptr;
    llvm::MCSubtargetInfo*                                    targetInfo = nullptr;
    llvm::MCDisassembler*                                     disassembler = nullptr;
    llvm::MCInstrInfo*                                        instrInfo = nullptr;
    llvm::MCInstrAnalysis*                                    instrAnalysis = nullptr;
    std::vector<std::pair<llvm::object::SymbolRef, uint64_t>> symbolSizes;
    //         llvm::DWARFCompileUnit* compileUnit = nullptr;
    //         const llvm::DWARFDebugLine::LineTable* lineTable = nullptr;
    //
    //         llvm::DWARFCompileUnit* getCompileUnit()
    //         {
    //             if (!compileUnit)
    //                 _initCompileUnit();
    //             return compileUnit;
    //         }
    //         const llvm::DWARFDebugLine::LineTable* getLineTable()
    //         {
    //             if (!lineTable)
    //                 _initCompileUnit();
    //             return lineTable;
    //         }
    //
    //         void _initCompileUnit()
    //         {
    //             PHANTOM_ASSERT(dwarfContext);
    //             for (auto& CU : dwarfContext->compile_units())
    //             {
    //                 compileUnit = CU.get();
    //                 lineTable = pDwarf->getLineTableForUnit(cu);
    //                 auto& die = cu->getUnitDIE(false);
    //                 for (const llvm::DWARFAttribute& att : die.attributes())
    //                 {
    //                     if (att.Attr == llvm::dwarf::DW_AT_decl_file)
    //                     {
    //                         uint64_t fileIndex = *att.Value.getAsUnsignedConstant();
    //                         fileName.clear();
    //                         lineTable->getFileNameByIndex(fileIndex, "",
    //                         llvm::DILineInfoSpecifier::FileLineInfoKind::AbsoluteFilePath, fileName); if
    //                         (compareFileNames(fileName, getSource()->getSourceFile()->getPath())) // the compile unit
    //                         is the one
    //                         {
    //                             return ;
    //                         }
    //                     }
    //                 }
    //             }
    //             PHANTOM_UNREACHABLE();
    //             compileUnit = nullptr;
    //             lineTable = nullptr;
    //         }
};

DebugInformation::DebugInformation()
{
    m_private = phantom::new_<DebugInformationPrivate>();
}

DebugInformation::~DebugInformation()
{
    phantom::delete_<DebugInformationPrivate>(m_private);
}

template<class T>
static bool compareFileNames(const std::string& path0, const T& path1)
{
    size_t count = path0.size();
    if (path0.size() != count)
        return false;
    for (size_t i = 0; i < count; ++i)
    {
        char c0 = path0[i];
        char c1 = path1[i];
        if ((c0 == '\\' && c1 == '/') || (c0 == '/' && c1 == '\\'))
            continue;
        if (::tolower(c0) != ::tolower(c1))
            return false;
    }
    return true;
}

CodeLocation DebugInformation::getCodeLocationForAddress(void* a_pAddress) const
{
    //         if (m_private->dwarfContext)
    //         {
    //             llvm::DILineInfo info = m_private->dwarfContext->getLineInfoForAddress(uint64_t(a_pAddress));
    //             if (info.FileName.size())
    //             {
    //                 Sources sources;
    //                 getModule()->getSources(sources);
    //                 for (auto pSource : sources)
    //                 {
    //                     if (SourceFile* pFile = pSource->getSourceFile())
    //                     {
    //                         if (compareFileNames(pFile->getPath(), info.FileName))
    //                         {
    //                             return CodeLocation(pSource, CodePosition( info.Line, info.Column ));
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //         return CodeLocation();

    if (m_private->dwarfContext)
    {
        std::string fileName;
        for (const auto& CU : m_private->dwarfContext->compile_units())
        {
            const llvm::DWARFDebugLine::LineTable* lineTable = m_private->dwarfContext->getLineTableForUnit(CU.get());
            bool                                   found = false;
            CodePosition                           pos;
            auto it = std::lower_bound(lineTable->Rows.begin(), lineTable->Rows.end(), uint64_t(a_pAddress),
                                       [](const llvm::DWARFDebugLine::Row& row, uint64_t a_pAddress) -> bool {
                                           return row.Address.Address < a_pAddress;
                                       });
            if (it == lineTable->Rows.end())
                return CodeLocation();
            auto& row = *it;
            int   col = row.Column;
            if (col == 0)
                col = 1;
            int line = row.Line;
            if (row.Address.Address == uint64_t(a_pAddress))
            {
                while (line == 0)
                    line = (--it)->Line;
                Sources sources;
                getModule()->getSources(sources);
                for (auto pSource : sources)
                {
                    if (SourceFile* pFile = pSource->getSourceStream()->asFile())
                    {
                        fileName.clear();
                        lineTable->getFileNameByIndex(
                        row.File, "", llvm::DILineInfoSpecifier::FileLineInfoKind::AbsoluteFilePath, fileName);
                        if (compareFileNames(fileName, pFile->getPath()))
                        {
                            return CodeLocation(pSource, CodePosition(line, col));
                        }
                    }
                }
                return CodeLocation();
            }
            else
            {
                PHANTOM_ASSERT((row.Address.Address > uint64_t(a_pAddress)));
                if (row.Line == 0)
                {
                    PHANTOM_ASSERT(it != lineTable->Rows.begin());
                    while (line == 0)
                        line = (--it)->Line;
                    Sources sources;
                    getModule()->getSources(sources);
                    for (auto pSource : sources)
                    {
                        if (SourceFile* pFile = pSource->getSourceStream()->asFile())
                        {
                            fileName.clear();
                            lineTable->getFileNameByIndex(
                            row.File, "", llvm::DILineInfoSpecifier::FileLineInfoKind::AbsoluteFilePath, fileName);
                            if (compareFileNames(fileName, pFile->getPath()))
                            {
                                return CodeLocation(pSource, CodePosition(line, col));
                            }
                        }
                    }
                }
                return CodeLocation();
            }
        }
    }
    return CodeLocation();
}
//             std::string fileName;
//             PHANTOM_ASSERT(m_private->getCompileUnit());
//             PHANTOM_ASSERT(m_private->getLineTable());
//             bool found = false;
//             CodeLocation loc;
//             int prevLine = 0;
//             for (auto& row : m_private->getLineTable()->Rows)
//             {
//                 fileName.clear();
//                 lineTable->getFileNameByIndex(row.File, "",
//                 llvm::DILineInfoSpecifier::FileLineInfoKind::AbsoluteFilePath, fileName); if
//                 (compareFileNames(fileName, getSource()->getSourceFile()->getPath()))
//                 {
//                     for (auto address : a_Addresses)
//                     {
//                         int col = row.Column;
//                         if (col == 0) col = 1;
//                         if (row.Address == uint64_t(address))
//                         {
//                             if (row.Line == 0 && col)
//                             {
//                                 // don't know why dwarf has some buggy line|col entries like (line = 0x00 | col =
//                                 0x16) sometimes, we fallback on previous non zero line to fix this
//                                 PHANTOM_ASSERT(prevLine);
//                                 return CodeLocation(prevLine, col);
//                             }
//                             return CodeLocation(row.Line, col);
//                         }
//                         else if (row.Address < uint64_t(a_pAddress))
//                         {
//                             pos = CodePosition(row.Line, col);
//                         }
//                         else if (pos.isValid())
//                         {
//                             return pos;
//                         }
//                         else break;
//                     }
//                 }
//                 if (row.Line)
//                     prevLine = row.Line;
//             }
//         }
//         return CodePosition();

void* DebugInformation::getAddressForCodeLocation(CodeLocation a_CodeLocation) const
{
    if (getCodeGenerator()->m_private->m_pDWARFContext.get() == nullptr)
        return nullptr;
    LanguageElement* pElement = a_CodeLocation.source->getElementAtLine(a_CodeLocation.position.line);
    if (pElement)
    {
        if (Block* pBlock = pElement->getEnclosingBlock())
        {
            lang::Subroutine* pSubroutine = pBlock->getSubroutine();
            llvm::Function*   pFunction = getCodeGenerator()->m_private->getFunction(pSubroutine);
            if (pFunction)
            {
                for (auto& pair : m_private->symbolSizes)
                {
                    if (pair.first.getType() && pair.first.getType().get() == llvm::object::SymbolRef::ST_Function)
                    {
                        if (pair.first.getName())
                        {
                            auto name = pair.first.getName().get();
                            if (name == pFunction->getName())
                            {
                                uint64_t startAddress =
                                getCodeGenerator()->m_private->m_ExecutionEngine->getFunctionAddress(
                                pFunction->getName().str());
                                uint64_t endAddress(startAddress + pair.second);
                                auto     pDwarf = m_private->dwarfContext;
                                for (const auto& CU : pDwarf->compile_units())
                                {
                                    const llvm::DWARFDebugLine::LineTable* lineTable =
                                    pDwarf->getLineTableForUnit(CU.get());
                                    std::vector<uint32_t> lookupRows;
                                    lineTable->lookupAddressRange({startAddress, 0}, pair.second, lookupRows);
                                    bool found = false;
                                    for (auto& rowIndex : lookupRows)
                                    {
                                        auto& row = lineTable->Rows[rowIndex];
                                        row.dump(llvm::outs());
                                        if (row.Line ==
                                            a_CodeLocation.position.line /*&& row.Column == a_CodePosition.column*/)
                                        {
                                            return (void*)row.Address.Address;
                                        }
                                    }
                                }
                                break;
                            }
                        }
                    }
                }
            }
        }
    }
    return nullptr;
}

void DebugInformation::begin()
{
    const llvm::Target& target = getCodeGenerator()->m_private->m_ExecutionEngine->getTargetMachine()->getTarget();

    m_private->m_public = this;

    getCodeGenerator()->m_private->m_ExecutionEngine->RegisterJITEventListener(m_private);

    auto triple = llvm::sys::getDefaultTargetTriple();

    std::unique_ptr<const llvm::MCRegisterInfo> MRI(target.createMCRegInfo(triple));
    if (!MRI)
    {
        return;
    }

    llvm::MCTargetOptions opts;

    std::unique_ptr<const llvm::MCAsmInfo> MAI(target.createMCAsmInfo(*MRI, triple, opts));
    if (!MAI)
    {
        return;
    }

    // Set up the MCContext for creating symbols and_ MCExpr's.
    llvm::MCContext Ctx(MAI.get(), MRI.get(), nullptr);

    m_private->targetInfo = target.createMCSubtargetInfo(triple, llvm::sys::getHostCPUName(), "");
    m_private->disassembler = target.createMCDisassembler(*m_private->targetInfo, Ctx);
    m_private->instrInfo = target.createMCInstrInfo();
    m_private->instrAnalysis = target.createMCInstrAnalysis(m_private->instrInfo);
}

void DebugInformation::abort() {}

void DebugInformation::end()
{
    getCodeGenerator()->m_private->m_ExecutionEngine->UnregisterJITEventListener(m_private);
}

bool DebugInformation::findStepOverInstructions(byte* a_pReturnAddress, byte* a_pAddress, byte*& a_pNext,
                                                byte*& a_pJumpTarget) const
{
    if (m_private->disassembler == nullptr || m_private->instrInfo == nullptr || m_private->instrAnalysis == nullptr)
    {
        return false;
    }
    llvm::MCInst           instr;
    uint64_t               Size;
    llvm::raw_null_ostream null_out;
    uint8_t                bytes[16];
    memcpy(bytes, a_pAddress, sizeof(bytes));
    llvm::MCDisassembler::DecodeStatus status = m_private->disassembler->getInstruction(
    instr, Size, llvm::ArrayRef<uint8_t>(bytes, 16), (uint64_t)a_pAddress, null_out);
    if (status == llvm::MCDisassembler::Success)
    {
        PHANTOM_LOG(Information, "disassembly success opcode 0x%x\n", instr.getOpcode());
        if (m_private->instrAnalysis->isBranch(instr))
        {
            PHANTOM_LOG(Information, "found branch 0x%x\n", instr.getOpcode());
            uint64_t targetInst;
            if (m_private->instrAnalysis->evaluateBranch(instr, (uint64_t)a_pAddress, Size, targetInst))
            {
                a_pNext = (byte*)a_pAddress + Size;
                a_pJumpTarget = (byte*)targetInst;
                if (!a_pJumpTarget)
                {
                    PHANTOM_LOG(Information, "branch has no target 0x%x\n", instr.getOpcode());
                }
            }
            else
            {
                PHANTOM_LOG(Information, "failed to evaluate branch 0x%x\n", instr.getOpcode());
                return false;
            }
        }
        else if (m_private->instrAnalysis->isReturn(instr))
        {
            PHANTOM_LOG(Information, "opcode is return (%d operand) 0x%x\n", instr.getNumOperands(), instr.getOpcode());
            a_pNext = a_pReturnAddress;
            PHANTOM_LOG(Information, "return to %x from %x\n", a_pNext, a_pAddress);
        }
        else
        {
            a_pNext = (byte*)a_pAddress + Size;
        }
        return a_pNext != nullptr;
    }
    return false;
}

bool DebugInformation::findStepIntoInstruction(byte* a_pAddress, byte*& a_pCallAddress,
                                               void* a_pGenericThreadContext) const
{
    if (m_private->disassembler == nullptr || m_private->instrInfo == nullptr || m_private->instrAnalysis == nullptr)
    {
        return false;
    }

    uint8_t bytes[16];
    memcpy(bytes, a_pAddress, sizeof(bytes));
    llvm::MCInst                       instr;
    uint64_t                           Size;
    llvm::raw_null_ostream             null_out;
    llvm::MCDisassembler::DecodeStatus status = m_private->disassembler->getInstruction(
    instr, Size, llvm::ArrayRef<uint8_t>(bytes, 16), (uint64_t)a_pAddress, null_out);
    if (status == llvm::MCDisassembler::Success)
    {
        PHANTOM_LOG(Information, "disassembly success opcode 0x%x\n", instr.getOpcode());
        if (m_private->instrAnalysis->isCall(instr))
        {
            PHANTOM_LOG(Information, "opcode is call (%d operand) 0x%x\n", instr.getNumOperands(), instr.getOpcode());
            if (instr.getOperand(0).isImm())
                a_pCallAddress = (byte*)a_pAddress + Size + instr.getOperand(0).getImm();
            else if (instr.getOperand(0).isReg()) // in RAX or_ EAX
            {
#if PHANTOM_OPERATING_SYSTEM == PHANTOM_OPERATING_SYSTEM_WINDOWS
#    if PHANTOM_ARCHITECTURE == PHANTOM_ARCHITECTURE_X64
                a_pCallAddress = (byte*)((LPCONTEXT)a_pGenericThreadContext)->Rax;
#    elif PHANTOM_ARCHITECTURE == PHANTOM_ARCHITECTURE_X86
                a_pCallAddress = (byte*)((LPCONTEXT)a_pGenericThreadContext)->Eax;
#    endif
#endif
                PHANTOM_LOG(Information, "call to %x from %x\n", a_pCallAddress, a_pAddress);
                return true;
            }
        }
    }
    return false;
}

CodeGenerator* DebugInformation::getCodeGenerator() const
{
    return static_cast<CodeGenerator*>(Compiler::Get()->getCodeGenerator(getModule()));
}

static byte* AddressOfLocal(const std::string& a_Local, const std::string& a_FilePath, int a_LocalLine,
                            int a_SubroutineLine, llvm::DWARFDie& die, int a_FrameBaseReg, size_t a_iDebuggerFrameIndex,
                            const llvm::DWARFDebugLine::LineTable* lineTable)
{
    if (die.isSubroutineDIE())
    {
        if (die.getDeclLine() != a_SubroutineLine)
        {
            return nullptr;
        }
        std::string fileName;
        for (const llvm::DWARFAttribute& att : die.attributes())
        {
            if (att.Attr == llvm::dwarf::DW_AT_decl_file)
            {
                uint64_t fileIndex = *att.Value.getAsUnsignedConstant();
                if (fileIndex == 0)
                    return nullptr;
                fileName.clear();
                if (!lineTable->getFileNameByIndex(
                    fileIndex, "", llvm::DILineInfoSpecifier::FileLineInfoKind::AbsoluteFilePath, fileName))
                    return nullptr;
                if (!compareFileNames(fileName, a_FilePath))
                    return nullptr;
            }
            if (att.Attr == llvm::dwarf::DW_AT_frame_base)
            {
                if (att.Value.getAsBlock().hasValue())
                {
                    auto const& arr = *att.Value.getAsBlock();
                    a_FrameBaseReg = arr[0];
                }
            }
        }
    }
    else if (die.getTag() == llvm::dwarf::DW_TAG_variable || die.getTag() == llvm::dwarf::DW_TAG_formal_parameter)
    {
        if (a_LocalLine != -1 && die.getDeclLine() != a_LocalLine)
        {
            return nullptr;
        }
        if (a_Local == die.getName(llvm::DINameKind::ShortName))
        {
            for (const llvm::DWARFAttribute& att : die.attributes())
            {
                if (att.Attr == llvm::dwarf::DW_AT_location)
                {
                    if (llvm::dwarf::DW_FORM_exprloc == att.Value.getForm())
                    {
                        auto arr = *att.Value.getAsBlock();
                        if (arr[0] == llvm::dwarf::DW_OP_fbreg)
                        {
                            int64_t offset = llvm::decodeSLEB128(arr.data() + 1);
                            switch (a_FrameBaseReg)
                            {
#if PHANTOM_ARCHITECTURE == PHANTOM_ARCHITECTURE_X64
                            case llvm::dwarf::DW_OP_reg7: // sp
#else
                            case llvm::dwarf::DW_OP_reg4: // sp
#endif
                                PHANTOM_LOG(Error, "%s : RSP(%p) + %lld", a_Local.c_str(),
                                            Compiler::Get()->getDebugger()->getFrameStackPointer(a_iDebuggerFrameIndex),
                                            offset);
                                return Compiler::Get()->getDebugger()->getFrameStackPointer(a_iDebuggerFrameIndex) +
                                offset;
#if PHANTOM_ARCHITECTURE == PHANTOM_ARCHITECTURE_X64
                            case llvm::dwarf::DW_OP_reg6: // bp
#else
                            case llvm::dwarf::DW_OP_reg5: // bp
#endif
                                PHANTOM_LOG(Error, "%s : RBP(%p) + %lld", a_Local.c_str(),
                                            Compiler::Get()->getDebugger()->getFrameBasePointer(a_iDebuggerFrameIndex),
                                            offset);
                                return Compiler::Get()->getDebugger()->getFrameBasePointer(a_iDebuggerFrameIndex) +
                                offset;
                            default:
                                continue;
                            }
                        }
                    }
                }
            }
        }
    }
    llvm::DWARFDie child = die.getFirstChild();
    if (child)
    {
        while (child)
        {
            if (byte* pAddr = AddressOfLocal(a_Local, a_FilePath, a_LocalLine, a_SubroutineLine, child, a_FrameBaseReg,
                                             a_iDebuggerFrameIndex, lineTable))
            {
                return pAddr;
            }
            child = child.getSibling();
        }
    }
    return nullptr;
}

byte* DebugInformation::getLocalVariableAddress(lang::LocalVariable* a_pLocalVariable,
                                                size_t               a_iDebuggerFrameIndex) const
{
    auto pDwarf = getCodeGenerator()->m_private->m_pDWARFContext.get();
    if (pDwarf)
    {
        if (SourceFile* pSourceFile = a_pLocalVariable->getSource()->getSourceStream()->asFile())
        {
            auto&                                  cu = *pDwarf->compile_units().begin();
            auto                                   udie = cu->getUnitDIE(false);
            const llvm::DWARFDebugLine::LineTable* lineTable = pDwarf->getLineTableForUnit(cu.get());
            byte*                                  pAddress = AddressOfLocal(
            std::string(a_pLocalVariable->getName().begin(), a_pLocalVariable->getName().end()),
            std::string(pSourceFile->getPath().data(), pSourceFile->getPath().size()),
            a_pLocalVariable->getCodePosition().line, a_pLocalVariable->getSubroutine()->getCodePosition().line, udie,
            -1, a_iDebuggerFrameIndex, lineTable);
            PHANTOM_LOG(Error, "Local Variable '%.*s' found at %p",
                        PHANTOM_STRING_AS_PRINTF_ARG(a_pLocalVariable->getName()), pAddress);
            return pAddress;
        }
    }
    return nullptr;
}
} // namespace jit
} // namespace phantom

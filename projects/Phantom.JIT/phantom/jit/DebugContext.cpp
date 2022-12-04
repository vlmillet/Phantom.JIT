#include "DebugContext.h"

#include "CodeGeneratorPrivate.h"
#include "Context.h"
#include "phantom/utils/Path.h"

#include <fstream>
#include <phantom/lang/Block.h>
#include <phantom/lang/Class.h>
#include <phantom/lang/Compiler.h>
#include <phantom/lang/Field.h>
#include <phantom/lang/Function.h>
#include <phantom/lang/FunctionPointer.h>
#include <phantom/lang/FunctionType.h>
#include <phantom/lang/LValueReference.h>
#include <phantom/lang/LocalVariable.h>
#include <phantom/lang/Method.h>
#include <phantom/lang/MethodPointer.h>
#include <phantom/lang/Module.h>
#include <phantom/lang/Pointer.h>
#include <phantom/lang/Signature.h>
#include <phantom/lang/SourceFile.h>
#include <phantom/lang/Structure.h>
#include <phantom/lang/Template.h>
#include <phantom/lang/TemplateSpecialization.h>
#include <phantom/lang/Union.h>
#include <phantom/utils/StringBuffer.h>
#include <phantom/utils/StringUtil.h>
#include <sstream>
#if PHANTOM_OPERATING_SYSTEM == PHANTOM_OPERATING_SYSTEM_WINDOWS
#    include <llvm/JITPDB/JITPDBMemoryManager.h>
#    include <windows.h>
#endif
#include "phantom/lang/Namespace.h"

namespace phantom
{
namespace jit
{
using namespace phantom::lang;

namespace
{
Class* getFirstVtableHolder(Class* a_pClass)
{
    if (a_pClass->getVirtualMethodTables().size())
    {
        return a_pClass;
    }
    return nullptr; /*
     if (a_pClass->getBaseClasses().size() == 0)
         return a_pClass->getVirtualMethodTables().size() ? a_pClass : nullptr;
     Class* pBaseHolder = getFirstVtableHolder(a_pClass->getBaseClass(0));
     if (pBaseHolder == nullptr) // base has no vtable holder
     {
         return a_pClass->getVirtualMethodTables().size() ? a_pClass : nullptr;
     }
     return pBaseHolder;*/
}

String NativeNamePrefix(Symbol* _symbol)
{
    return {};
    //     String moduleName = _symbol->getModule()->getName();
    //     phantom::StringUtil::ReplaceAll(moduleName, ".", "__");
    //     return moduleName + "::";
}
const char* NativeExtendedNamePrefix = ""; //"extendedcpp::";
} // namespace

llvm::DIDerivedType* DebugContext::_toDIDerivedType(Field* a_pField)
{
    Type* pType = a_pField->getValueType();
    return m_DIBuilder.createMemberType(m_DICompilationUnit, toStringRef(a_pField->getName()),
                                        getOrCreateDIFile(a_pField), a_pField->getCodeRange().begin.line,
                                        pType->getSize() * 8, pType->getAlignment() * 8, a_pField->getOffset() * 8,
                                        llvm::DINode::DIFlags(0), toDIType(pType));
}

llvm::DISubprogram* DebugContext::_toDISubprogram(lang::Function* a_pFunction)
{
    return m_DIBuilder.createFunction(getOrCreateDIScope(a_pFunction->getNamingScope()),
                                      toStringRef(getDebugName(a_pFunction)), toStringRef(getMangledName(a_pFunction)),
                                      getOrCreateDIFile(a_pFunction), a_pFunction->getCodePosition().line,
                                      toDIFunctionType(a_pFunction->getSignature(), a_pFunction->getABI(), nullptr),
                                      a_pFunction->getBlock() ? a_pFunction->getBlock()->getCodePosition().line : 0,
                                      llvm::DINode::FlagZero, llvm::DISubprogram::SPFlagDefinition);
}

llvm::DISubprogram* DebugContext::_toDISubprogram(lang::Method* a_pMethod)
{
    Type* pThisType = a_pMethod->getOwnerClassType();
    if (a_pMethod->getSignature()->isConst())
        pThisType = pThisType->addConst();
    if (a_pMethod->getSignature()->testModifiers(Modifier::Volatile))
        pThisType = pThisType->addVolatile();
    auto pVTableHolder = toFwdDIType(a_pMethod->getOwnerClass());
    return m_DIBuilder.createMethod(
    toFwdDIType(a_pMethod->getOwnerClassType()), toStringRef(getDebugName(a_pMethod)),
    toStringRef(getMangledName(a_pMethod)), getOrCreateDIFile(a_pMethod), a_pMethod->getCodePosition().line,
    toDIFunctionType(a_pMethod->getSignature(), a_pMethod->getABI(), pThisType), a_pMethod->getVirtualTableIndex(0), 0,
    pVTableHolder, llvm::DINode::FlagZero,
    llvm::DISubprogram::DISPFlags(llvm::DISubprogram::SPFlagDefinition |
                                  (a_pMethod->isVirtual() * llvm::DISubprogram::SPFlagVirtual)));
}

void DebugContext::_writeNatvis(std::ostream& _out)
{
    String moduleName = m_pContext->m_pCodeGeneratorPrivate->m_pCodeGenerator->getModule()->getName();

    _out << "<?xml version=\"1.0\" encoding=\"utf-8\"?>" << std::endl;
    _out << "<AutoVisualizer xmlns=\"http://schemas.microsoft.com/vstudio/debugger/natvis/2010\">" << std::endl;

    SmallSet<StringBuffer> alreadyWritten;

    phantom::StringViews ignoredSymbols;
    StringView           ignoredSymbolsList;
    if (m_pContext->m_pCodeGeneratorPrivate->m_pCodeGenerator->getOption("ignore-natvis", ignoredSymbolsList))
    {
        phantom::StringUtil::Split(ignoredSymbols, ignoredSymbolsList, ",");
    }

    for (Symbol* pSym : m_NativeSymbols)
    {
        if (pSym->asClassType() || pSym->asEnum())
        {
            if (std::find(ignoredSymbols.begin(), ignoredSymbols.end(), pSym->getQualifiedDecoratedName()) !=
                ignoredSymbols.end())
            {
                continue;
            }
            String       qualName = pSym->getQualifiedName();
            String       nativeModuleName = phantom::Path(pSym->getModule()->getLibraryFullName()).filename();
            StringBuffer runtimeName;
            StringBuffer nativeName = StringView(nativeModuleName + '!' + qualName);
            if (TemplateSpecialization* pSpec = pSym->getTemplateSpecialization())
            {
                runtimeName = NativeNamePrefix(pSym) + qualName;
                runtimeName += "&lt;";
                nativeName += "&lt;";
                size_t count = pSpec->getArguments().size();
                for (size_t i = 0; i < count; ++i)
                {
                    if (i)
                    {
                        runtimeName += ',';
                        nativeName += ',';
                    }
                    String arg = pSpec->getArguments()[i]->getQualifiedDecoratedName();
                    phantom::StringUtil::ReplaceAll(arg, "&&", " µ");
                    phantom::StringUtil::ReplaceAll(arg, "&", " &amp;");
                    phantom::StringUtil::ReplaceAll(arg, "µ", "&amp;&amp;");
                    phantom::StringUtil::ReplaceAll(arg, " *", " µ");
                    phantom::StringUtil::ReplaceAll(arg, "*", " *");
                    phantom::StringUtil::ReplaceAll(arg, "µ", "*");
                    phantom::StringUtil::ReplaceAll(arg, "<", "&lt;");
                    phantom::StringUtil::ReplaceAll(arg, ">", "&gt;");
                    phantom::StringUtil::ReplaceAll(arg, ">>", "&gt; &gt;");
                    phantom::StringUtil::ReplaceAll(arg, " ,", ",");
                    if (arg.back() == ' ')
                        arg.pop_back();
                    runtimeName += arg; // "*";
                    nativeName += arg;  // "$T" + phantom::StringUtil::ToString(i + 1);
                }
                if (runtimeName.back() == ';' && runtimeName[runtimeName.size() - 2] == 't' &&
                    runtimeName[runtimeName.size() - 3] == 'g')
                {
                    runtimeName += " &gt;";
                    nativeName += " &gt;";
                }
                else
                {
                    runtimeName += "&gt;";
                    nativeName += "&gt;";
                }
            }
            else
            {
                runtimeName = moduleName + ".dll!" + NativeNamePrefix(pSym) + qualName;
            }
            if (alreadyWritten.insert(runtimeName).second)
            {
                _out << "<Type Name=\"" << runtimeName << "\"  Inheritable=\"0\">" << std::endl;
                _out << "<SmartPointer Usage=\"Minimal\">" << std::endl
                     << "(" << nativeName << "*)this" << std::endl
                     << "</SmartPointer>" << std::endl;
                _out << "<DisplayString>{(" << nativeName << "*)this,na}</DisplayString>" << std::endl;
                _out << "</Type>" << std::endl;
            }
        }
    }
    _out << "</AutoVisualizer>" << std::endl;
}

void DebugContext::finalize()
{
    replaceTemporaries();
    m_DIBuilder.finalize();

#if PHANTOM_OPERATING_SYSTEM == PHANTOM_OPERATING_SYSTEM_WINDOWS
    char buf[256];
    GetModuleFileNameA(NULL, buf, 256);
#else
#    error "provide current shared library path here"
#endif

    phantom::Path p(buf);
    p.popBack();
    p.append(m_pContext->m_pCodeGeneratorPrivate->m_pCodeGenerator->getModule()->getName());
    p.back() += ".natvis";

    // #define PHANTOM_JIT_OUTPUT_NATVIS_FILE

#if defined(PHANTOM_JIT_OUTPUT_NATVIS_FILE)
    std::ofstream ofstream(p.genericString().c_str());
    _writeNatvis(ofstream);
#endif
    std::ostringstream ostrstr;
    _writeNatvis(ostrstr);
    m_pDebugDllMemMgr->getPDBFileBuilder().addNatvisBuffer(p.genericString().c_str(),
                                                           llvm::MemoryBuffer::getMemBufferCopy(ostrstr.str()));
}

llvm::DIFile* DebugContext::getOrCreateDIFile(phantom::lang::Source* a_pSource)
{
    auto& pFile = m_DIFiles[a_pSource];
    if (pFile == nullptr)
    {
        if (auto pSS = a_pSource->getSourceStream())
        {
            SourceFile*   pSrcFile = pSS->asFile();
            phantom::Path p(pSrcFile ? pSrcFile->getPath() : "" /*a_pSource->iliUrl()*/);

            auto                                               FileNameStr = p.finalPath().absolute().genericString();
            llvm::StringRef                                    FileName = toStringRef(FileNameStr);
            llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> MemBuf = llvm::MemoryBuffer::getFileAsStream(FileName);
            if (MemBuf)
            {
                llvm::MD5            Hash;
                llvm::MD5::MD5Result Result;

                Hash.update((*MemBuf)->getBuffer());
                Hash.final(Result);

                llvm::SmallString<32> Checksum;

                Hash.stringifyResult(Result, Checksum);

                llvm::DIFile::ChecksumInfo<llvm::StringRef> ChecksumInfo(llvm::DIFile::CSK_MD5, Checksum);
                pFile = m_DIBuilder.createFile(FileName, "", ChecksumInfo);
            }
            else
            {
                PHANTOM_LOG(Error, "unable to open file '%*.s' for debug info checksum generation",
                            PHANTOM_STRING_AS_PRINTF_ARG(FileName));
                pFile = m_DIBuilder.createFile(FileName, "");
            }
        }
        else
        {
            pFile = m_DIBuilder.createFile("~nosource", "");
        }
    }
    return pFile;
}

llvm::DIFile* DebugContext::getOrCreateDIFile(phantom::lang::LanguageElement* a_pElement)
{
    // for template instantiations, we must forward to the template source
    if (!a_pElement->isNative())
    {
        if (auto pSpec = a_pElement->getEnclosingTemplateSpecialization())
        {
            if (auto pInstSpec = pSpec->getInstantiationSpecialization())
                pSpec = pInstSpec;
            if (pSpec->getOwner() == nullptr) // function template instance inside template class
                return getOrCreateDIFile(pSpec->getTemplate());
            return getOrCreateDIFile(pSpec);
        }
    }
    return getOrCreateDIFile(a_pElement->getCodeLocationSource());
}

llvm::DIScope* DebugContext::getOrCreateDIScope(phantom::lang::LanguageElement* a_pScope)
{
    if (a_pScope == phantom::lang::Namespace::Global())
        return m_DICompilationUnit;
    if (ClassType* pClassType = a_pScope->asClassType())
    {
        return toFwdDIType(pClassType);
    }
    else if (Namespace* pNS = a_pScope->asNamespace())
    {
        llvm::DINamespace*& ns = m_DINamespaces[pNS];
        if (ns == nullptr)
        {
            ns = m_DIBuilder.createNameSpace(getOrCreateDIScope(pNS->getParentNamespace()), toStringRef(pNS->getName()),
                                             true);
        }
        return ns;
    }
    else if (lang::Subroutine* pSR = a_pScope->asSubroutine())
    {
        return toDISubprogram(pSR);
    }
    else if (Block* pBlock = a_pScope->asBlock())
    {
        llvm::DIScope*& blk = m_DIBlocks[pBlock];
        if (blk == nullptr)
        {
            auto& pos = pBlock->getCodePosition();
            blk = m_DIBuilder.createLexicalBlock(getOrCreateDIScope(pBlock->getOwner()), getOrCreateDIFile(pBlock),
                                                 pos.line, pos.column);
        }
        return blk;
    }
    else if (Module* pMod = a_pScope->asModule())
    {
        llvm::DIScope*& mod = m_DIModules[pMod];
        if (mod == nullptr)
        {
            String name = pMod->getName();
            String path;

            if (pMod->isNative())
            {
                phantom::Path p(pMod->getLibraryFullName());
                name = p.filename();
                path = p.parentPath().genericString();
            }
            else if (pMod == m_pContext->m_pCodeGeneratorPrivate->m_pCodeGenerator->getModule())
            {
                return m_DICompilationUnit; // not found
            }
            else
            {
                CodeGenerator* codeGen = phantom::Object::Cast<CodeGenerator>(Compiler::Get()->getCodeGenerator(pMod));
                if (codeGen)
                {
                    if (auto debugCtx = codeGen->m_private->m_Context.m_pDebugContext)
                    {
                        name = phantom::Path(debugCtx->m_pDebugDllMemMgr->getDllPath().c_str()).filename();
                        path = StringView(debugCtx->m_pDebugDllMemMgr->getOutputPath().data(),
                                          debugCtx->m_pDebugDllMemMgr->getOutputPath().size());
                    }
                    else
                    {
                        return m_DICompilationUnit; // not found
                    }
                }
                else
                {
                    return m_DICompilationUnit; // not found
                }
            }

            llvm::DIBuilder DIB(*m_pContext->m_pCodeGeneratorPrivate->m_Module);
            mod = DIB.createCompileUnit(m_DICompilationUnit->getSourceLanguage(),
                                        // TODO: Support "Source" from external AST providers?
                                        DIB.createFile(toStringRef(name), toStringRef(path)),
                                        m_DICompilationUnit->getProducer(), true, llvm::StringRef(), 0,
                                        llvm::StringRef(), llvm::DICompileUnit::FullDebug, ~1ULL);
            DIB.finalize();
        }
        return mod;
    }
    else if (auto pSpec = a_pScope->asTemplateSpecialization())
    {
        return getOrCreateDIScope(pSpec->getTemplate()->getNamingScope());
    }

    PHANTOM_ASSERT(false);
    return nullptr;
}

String DebugContext::getMangledName(Symbol* a_pSymbol, CppManglerFlags a_Flags /*= 0*/, int a_Discriminator /*= 0*/)
{
    String prefix;
    if (a_Flags & CppManglerFlag::Thunk)
        prefix += "[thunk]";
    if (a_Flags & CppManglerFlag::Virtual)
        prefix += "[virtual]";
    if (a_Flags & CppManglerFlag::Apply)
        prefix += "[apply]";
    if (a_Flags & CppManglerFlag::Final)
        prefix += "[final]";
    auto sn = _getSymbolName(a_pSymbol);
    return prefix + StringView(sn.data(), sn.size());
    //         StringBuffer buf;
    //         CppMangler mangler(buf, a_Flags, 0);
    //         mangler.mangle(a_pSymbol);
    //         PHANTOM_ASSERT(buf.find("::") == String::npos);
    //         return String(buf.data(), buf.size());
}

void DebugContext::_fetchDIMembers(Union* a_pUnion, SmallVector<llvm::Metadata*>& a_Members)
{
    _pushDIFwd(a_pUnion);
    // if (!a_pUnion->isNative())
    _fetchDIMembers(static_cast<ClassType*>(a_pUnion), a_Members);
    _popDIFwd();
}

void DebugContext::_createVTableInfo(Class* a_pClass, SmallVector<llvm::Metadata*>& a_Members)
{
    // If this class is not dynamic then there is not any vtable info to collect.
    if (a_pClass->getVirtualMethodTables().empty())
        return;

    // Don't emit any vtable shape or vptr info if this class doesn't have an
    // extendable vfptr. This can happen if the class doesn't have virtual
    // methods, or in the MS ABI if those virtual methods only come from virtually
    // inherited bases.
    // 	const ASTRecordLayout& RL = CGM.getContext().getASTRecordLayout(RD);
    // 	if (!RL.hasExtendableVFPtr())
    // 		return;

    // CodeView needs to know how large the vtable of every dynamic class is, so
    // emit a special named pointer type into the element list. The vptr type
    // points to this type as well.
    llvm::DIType* VPtrTy = nullptr;
    bool          NeedVTableShape = true /*CGM.getCodeGenOpts().EmitCodeView &&
                      CGM.getTarget().getCXXABI().isMicrosoft()*/
    ;
    if (NeedVTableShape)
    {
        uint64_t PtrWidth = sizeof(void*) * 8; // in bits
        unsigned VSlotCount = a_pClass->getVirtualMethodTableSize(0);
        unsigned VTableWidth = PtrWidth * VSlotCount;
        // 		unsigned VtblPtrAddressSpace = CGM.getTarget().getVtblPtrAddressSpace();
        // 		Optional<unsigned> DWARFAddressSpace =
        // 			CGM.getTarget().getDWARFAddressSpace(VtblPtrAddressSpace);

        // Create a very wide void* type and insert it directly in the element list.
        llvm::DIType* VTableType =
        m_DIBuilder.createPointerType(nullptr, VTableWidth, 0, llvm::Optional<unsigned>(), "__vtbl_ptr_type");
        a_Members.push_back(VTableType);

        // The vptr is a pointer to this special vtable type.
        VPtrTy = m_DIBuilder.createPointerType(VTableType, PtrWidth);
    }

    // If there is a primary base then the artificial vptr member lives there.
#define PHANTOM_JIT_FORCE_ALWAYS_VTABLE 0
#if !PHANTOM_JIT_FORCE_ALWAYS_VTABLE
    if (!a_pClass->getBaseClasses().empty() && a_pClass->getBaseClass(0)->getVirtualMethodTables().size())
        return;
#endif

    unsigned        Size = sizeof(void*) * 8;
    phantom::String name = StringView("_vptr$") + a_pClass->getName();
    llvm::DIType*   VPtrMember =
    m_DIBuilder.createMemberType(m_DICompilationUnit, toStringRef(name), getOrCreateDIFile(a_pClass), 0, Size, 0, 0,
                                 llvm::DINode::FlagArtificial, VPtrTy);
    a_Members.push_back(VPtrMember);
}

void DebugContext::_fetchDIMembers(Class* a_pClass, SmallVector<llvm::Metadata*>& a_Members)
{
    _pushDIFwd(a_pClass);
    // if (!a_pClass->isNative())
    {
        for (auto base : a_pClass->getBaseClasses())
        {
            a_Members.push_back(m_DIBuilder.createInheritance(toFwdDIType(a_pClass), toDIType(base.baseClass),
                                                              base.offset * 8, 0,
                                                              llvm::DINode::DIFlags::FlagSingleInheritance));
        }

        _createVTableInfo(a_pClass, a_Members);

        _fetchDIMembers(static_cast<ClassType*>(a_pClass), a_Members);
    }
    _popDIFwd();
}

void DebugContext::_fetchDIMembers(ClassType* a_pClass, SmallVector<llvm::Metadata*>& a_Members)
{
    for (auto pField : a_pClass->getFields())
    {
        Type* pType = pField->getValueType();
        a_Members.push_back(toDIDerivedType(pField));
    }
    for (auto pMethod : a_pClass->getMethods())
    {
        a_Members.push_back(toDISubprogram(pMethod));
    }
    for (auto pFunc : a_pClass->getFunctions())
    {
        a_Members.push_back(toDISubprogram(pFunc));
    }
}

llvm::DIType* DebugContext::_toDIClass(Class* a_pClass)
{
    SmallVector<llvm::Metadata*> members;

    _fetchDIMembers(a_pClass, members);

    TemplateSpecialization* pSpec = a_pClass->getTemplateSpecialization();

    SmallVector<llvm::Metadata*> params;
    if (pSpec)
    {
        for (auto pArg : pSpec->getArguments())
        {
            if (Type* pType = pArg->asType())
            {
                params.push_back(m_DIBuilder.createTemplateTypeParameter(
                m_DICompilationUnit, toStringRef(pArg->getName()), toFwdDIType(pType), false));
            }
            else
            {
                Constant* pConst = pArg->asConstant();
                pType = pConst->getValueType();
                int64_t val;
                pConst->getValue(&val);
                params.push_back(m_DIBuilder.createTemplateValueParameter(
                m_DICompilationUnit, toStringRef(pArg->getName()), toFwdDIType(pType),
                llvm::Constant::getIntegerValue(m_pContext->toJitType(pType), llvm::APInt(32, val)), nullptr));
            }
        }
    }

    llvm::StringRef symname = _getSymbolName(a_pClass);

    llvm::DICompositeType* pDIType = m_DIBuilder.createClassType(

    getOrCreateDIScope(a_pClass->getModule()), symname, getOrCreateDIFile(a_pClass),
    a_pClass->getCodeRange().begin.line, a_pClass->getSize() * 8, a_pClass->getAlignment() * 8, 0,
    llvm::DINode::DIFlags(0), a_pClass->getBaseClasses().size() ? toDIType(a_pClass->getBaseClasses()[0]) : nullptr,
    m_DIBuilder.getOrCreateArray(toArrayRef(members)),
    /*VTableHolder=*/nullptr,
    /*pSpec ? llvm::MDTuple::get(m_pContext->m_LLVMContext / *, toArrayRef(params)* /) : */ nullptr,
    toStringRef(getMangledName(a_pClass)));

    if (pSpec)
    {
        m_DIBuilder.replaceArrays(
        pDIType, llvm::DINodeArray(),
        m_DIBuilder.getOrCreateArray(llvm::ArrayRef<llvm::Metadata*>(params.data(), params.size())));
    }

    return pDIType;
}

llvm::DIType* DebugContext::_toDIUnion(Union* a_pUnion)
{
    SmallVector<llvm::Metadata*> members;
    _fetchDIMembers(a_pUnion, members);
    return m_DIBuilder.createUnionType(getOrCreateDIScope(a_pUnion->getModule()), _getSymbolName(a_pUnion),
                                       getOrCreateDIFile(a_pUnion), a_pUnion->getCodeRange().begin.line,
                                       a_pUnion->getSize() * 8, a_pUnion->getAlignment() * 8, llvm::DINode::DIFlags(0),
                                       m_DIBuilder.getOrCreateArray(toArrayRef(members)), 0U,
                                       toStringRef(getMangledName(a_pUnion)));
}

llvm::DIType* DebugContext::_toDIStruct(Structure* a_pStruct)
{
    SmallVector<llvm::Metadata*> members;
    _fetchDIMembers(a_pStruct, members);
    return m_DIBuilder.createStructType(
    getOrCreateDIScope(a_pStruct->getModule()), _getSymbolName(a_pStruct), getOrCreateDIFile(a_pStruct),
    a_pStruct->getCodeRange().begin.line, a_pStruct->getSize() * 8, a_pStruct->getAlignment() * 8,
    llvm::DINode::DIFlags(0), nullptr, m_DIBuilder.getOrCreateArray(toArrayRef(members)), 0, nullptr,
    toStringRef(getMangledName(a_pStruct)));
    ;
}

llvm::DIType* DebugContext::_toDIEnum(Enum* a_pEnum)
{
    // Create elements for each enumerator.
    llvm::SmallVector<llvm::Metadata*, 16> csts;
    bool                                   IsSigned = a_pEnum->getUnderlyingIntType()->isSignedInteger();
    for (const auto* pCst : a_pEnum->getConstants())
    {
        int64_t Value;
        pCst->getValue(&Value);
        csts.push_back(m_DIBuilder.createEnumerator(toStringRef(pCst->getName()), Value, !IsSigned));
    }

    // Return a CompositeType for the enum itself.
    llvm::DINodeArray elms = m_DIBuilder.getOrCreateArray(csts);
    return m_DIBuilder.createEnumerationType(
    getOrCreateDIScope(a_pEnum->getModule()), _getSymbolName(a_pEnum), getOrCreateDIFile(a_pEnum),
    a_pEnum->getCodeRange().begin.line, a_pEnum->getSize() * 8, a_pEnum->getAlignment() * 8, elms,
    toDIType(a_pEnum->getUnderlyingIntType()), toStringRef(getMangledName(a_pEnum)), a_pEnum->isScoped());
    ;
}

llvm::StringRef DebugContext::_getSymbolName(Symbol* a_pSym)
{
    auto found = m_SymbolNames.find(a_pSym);

    if (found == m_SymbolNames.end())
    {
        String& str = m_SymbolNames[a_pSym];
        //         if (a_pSym->isNative())
        //         {
        //             str = "native";
        //             str += '!';
        //         }
        if (a_pSym->isNative())
        {
            m_NativeSymbols.insert(a_pSym);
            str += NativeNamePrefix(a_pSym);
        }
        else
        {
            if (TemplateSpecialization* pSpec = a_pSym->getTemplateSpecialization())
            {
                if (pSpec->getTemplate()->isNative() && !pSpec->isNative())
                {
                    str += NativeExtendedNamePrefix;
                }
            }
        }
        str += a_pSym->getQualifiedDecoratedName();
        //         if (a_pSym->isNative())
        //         {
        //             str = String(phantom::Path(a_pSym->getModule()->getDynamicLibraryPath()).filename()) + "!" +
        //             a_pSym->getQualifiedDecoratedName();
        //         }
        //         else if (a_pSym->getModule() == m_pContext->m_pCodeGeneratorPrivate->m_pCodeGenerator->getModule())
        //         {
        //             str = a_pSym->getQualifiedDecoratedName();
        //         }
        //         else
        //         {
        //             CodeGenerator* codeGen =
        //             phantom::Object::Cast<CodeGenerator>(Compiler::Get()->getCodeGenerator(a_pSym->getModule()));
        //             if (auto debugCtx = codeGen->m_private->m_Context.m_pDebugContext)
        //             {
        //                 str = String(debugCtx->m_pDebugDllMemMgr->getDllPath().filename()) + "!" +
        //                 a_pSym->getQualifiedDecoratedName();
        //             }
        //         }

        return llvm::StringRef(str.data(), str.size());
    }
    return llvm::StringRef(found->second.data(), found->second.size());
}

llvm::DIType* DebugContext::_toDIType(Type* a_pType)
{
    if (_hasDIFwd(a_pType))
        return toFwdDIType(a_pType);
    if (a_pType == PHANTOM_TYPEOF(void))
        return nullptr;
    if (a_pType->asConstType())
        return m_DIBuilder.createQualifiedType(llvm::dwarf::DW_TAG_const_type, toDIType(a_pType->removeConst()));
    if (a_pType->asVolatileType())
        return m_DIBuilder.createQualifiedType(llvm::dwarf::DW_TAG_volatile_type, toDIType(a_pType->removeVolatile()));
    if (a_pType->asConstVolatileType())
        return m_DIBuilder.createQualifiedType(
        llvm::dwarf::DW_TAG_const_type,
        m_DIBuilder.createQualifiedType(llvm::dwarf::DW_TAG_volatile_type, toDIType(a_pType->removeConstVolatile())));

    if (FunctionType* pFType = a_pType->asFunctionType())
    {
        return toDIFunctionType(pFType, ABI::CDecl, nullptr);
    }

    size_t size = a_pType->getSize() * 8;
    size_t align = a_pType->getAlignment() * 8;
    if (FunctionPointer* pFPType = a_pType->asFunctionPointer())
    {
        return m_DIBuilder.createPointerType(toDIFunctionType(pFPType->getFunctionType(), pFPType->getABI(), nullptr),
                                             size, align);
    }

    switch (a_pType->getTypeKind())
    {
    case TypeKind::Bool:
        return m_DIBuilder.createBasicType("bool", size, llvm::dwarf::DW_ATE_boolean);
    case TypeKind::Char:
        return m_DIBuilder.createBasicType("char", size, llvm::dwarf::DW_ATE_signed_char);
    case TypeKind::SChar:
        return m_DIBuilder.createBasicType("signed char", size, llvm::dwarf::DW_ATE_signed_char);
    case TypeKind::UChar:
        return m_DIBuilder.createBasicType("unsigned char", size, llvm::dwarf::DW_ATE_unsigned_char);
        PHANTOM_IF_WCHAR_T(case TypeKind::WChar:)
        PHANTOM_IF_CHAR16_T(case TypeKind::Char16:)
    case TypeKind::Short:
        return m_DIBuilder.createBasicType("short", size, llvm::dwarf::DW_ATE_signed);
    case TypeKind::UShort:
        return m_DIBuilder.createBasicType("unsigned short", size, llvm::dwarf::DW_ATE_unsigned);
        PHANTOM_IF_CHAR32_T(case TypeKind::Char32:)
    case TypeKind::Int:
        return m_DIBuilder.createBasicType("int", size, llvm::dwarf::DW_ATE_signed);
    case TypeKind::UInt:
        return m_DIBuilder.createBasicType("unsigned int", size, llvm::dwarf::DW_ATE_unsigned);
    case TypeKind::Long:
        return m_DIBuilder.createBasicType("long", size, llvm::dwarf::DW_ATE_signed);
    case TypeKind::ULong:
        return m_DIBuilder.createBasicType("unsigned long", size, llvm::dwarf::DW_ATE_unsigned);
    case TypeKind::LongLong:
        return m_DIBuilder.createBasicType("long long", size, llvm::dwarf::DW_ATE_signed);
    case TypeKind::ULongLong:
        return m_DIBuilder.createBasicType("unsigned long long", size, llvm::dwarf::DW_ATE_unsigned);
    case TypeKind::Float:
        return m_DIBuilder.createBasicType("float", size, llvm::dwarf::DW_ATE_float);
    case TypeKind::Double:
        return m_DIBuilder.createBasicType("double", size, llvm::dwarf::DW_ATE_float);
    case TypeKind::LongDouble:
        return m_DIBuilder.createBasicType("long double", size, llvm::dwarf::DW_ATE_float);
    case TypeKind::LValueReference:
        return m_DIBuilder.createReferenceType(llvm::dwarf::DW_TAG_reference_type,
                                               toFwdDIType(a_pType->removeReference()));
    case TypeKind::RValueReference:
        return m_DIBuilder.createReferenceType(llvm::dwarf::DW_TAG_rvalue_reference_type,
                                               toFwdDIType(a_pType->removeReference()));
    case TypeKind::Class:
    case TypeKind::VectorClass:
    case TypeKind::SetClass:
    case TypeKind::MapClass:
    case TypeKind::StringClass:
    case TypeKind::ArrayClass:
        return _toDIClass(static_cast<Class*>(a_pType));
    case TypeKind::Structure:
        return _toDIStruct(static_cast<Structure*>(a_pType));
    case TypeKind::Union:
        return _toDIUnion(static_cast<Union*>(a_pType));
    case TypeKind::Enum:
        return _toDIEnum(static_cast<Enum*>(a_pType));
    case TypeKind::Array:
    {
        llvm::SmallVector<llvm::Metadata*, 8> Subscripts;

        Array* pArray = static_cast<Array*>(a_pType);
        do
        {
            size_t itemCount = pArray->getItemCount();
            Type*  pItemType = pArray->getItemType();
            if (itemCount) // fixed size [N]
            {
                Subscripts.push_back(m_DIBuilder.getOrCreateSubrange(0, itemCount));
            }
            else // pointer-array []
            {
                Subscripts.push_back(m_DIBuilder.getOrCreateSubrange(0, -1));
            }
            a_pType = pItemType;
            pArray = pItemType->asArray();
        } while (pArray);

        llvm::DINodeArray SubscriptArray = m_DIBuilder.getOrCreateArray(Subscripts);
        return m_DIBuilder.createArrayType(size, align, toDIType(a_pType), SubscriptArray);
    }
    case TypeKind::Pointer:
        return m_DIBuilder.createPointerType(toFwdDIType(a_pType->removePointer()), size, align);
    case TypeKind::NullPtr:
        return m_DIBuilder.createNullPtrType();
    }
    if (a_pType->asMemberPointer())
        return m_DIBuilder.createPointerType(toDIType(PHANTOM_TYPEOF(void)), size, align);
    PHANTOM_ASSERT(false, "Given phantom type cannot be converted to DI type");
    return nullptr;
}

DebugContext::DebugContext(Context* a_pContext, llvm::Module* a_pLLVMModule)
    : m_pContext(a_pContext), m_DIBuilder(*a_pLLVMModule)
{
}

llvm::DISubprogram* DebugContext::toDISubprogram(lang::Subroutine* a_pSubroutine)
{
    if (lang::Method* m = a_pSubroutine->asMethod())
        return toDISubprogram(m);
    if (lang::Function* f = a_pSubroutine->asFunction())
        return toDISubprogram(f);
    return nullptr;
}

llvm::DIType* DebugContext::toFwdDIType(Type* a_pType)
{
    if (a_pType->asConstType())
        return m_DIBuilder.createQualifiedType(llvm::dwarf::DW_TAG_const_type, toFwdDIType(a_pType->removeConst()));
    if (a_pType->asVolatileType())
        return m_DIBuilder.createQualifiedType(llvm::dwarf::DW_TAG_volatile_type,
                                               toFwdDIType(a_pType->removeVolatile()));
    if (a_pType->asConstVolatileType())
        return m_DIBuilder.createQualifiedType(llvm::dwarf::DW_TAG_const_type, toFwdDIType(a_pType->removeConst()));

    if (ClassType* pClassType = a_pType->asClassType())
    {
        static int Tags = 1;
        auto&      pTemp = m_Temps[pClassType];
        if (pTemp == nullptr)
            pTemp = m_DIBuilder.createReplaceableCompositeType(
            Tags++, _getSymbolName(pClassType), getOrCreateDIScope(pClassType->getModule()),
            getOrCreateDIFile(pClassType), pClassType->getCodeRange().begin.line, 0, pClassType->getSize() * 8,
            pClassType->getSize() * 8, llvm::DINode::FlagFwdDecl, toStringRef(getMangledName(pClassType)));
        return pTemp;
    }
    return toDIType(a_pType);
}

static unsigned getDwarfCC(ABI a_ABI)
{
    switch (a_ABI)
    {
    case ABI::CDecl:
        // Avoid emitting DW_AT_calling_convention if the C convention was used.
        return 0;
    case ABI::StdCall:
        return llvm::dwarf::DW_CC_BORLAND_stdcall;
    case ABI::FastCall:
        return llvm::dwarf::DW_CC_BORLAND_msfastcall;
    case ABI::ThisCall:
        return llvm::dwarf::DW_CC_BORLAND_thiscall;
    case ABI::Win64:
        return llvm::dwarf::DW_CC_LLVM_Win64;
    }
    return 0;
}

llvm::DISubroutineType* DebugContext::toDIFunctionType(Signature* a_pSignature, ABI a_ABI, Type* a_pThisType)
{
    bool                            isRVOCandidate = a_pSignature->isRVOCandidate();
    bool                            isThisCall = a_pThisType != nullptr; // (a_pThisType != nullptr);
    size_t                          count = a_pSignature->getParameterCount() + isThisCall + isRVOCandidate;
    SmallVector<llvm::Metadata*, 6> params;
    params.push_back(toDIType(isRVOCandidate ? PHANTOM_TYPEOF(void) : a_pSignature->getReturnType()));
    params.resize(count + 1);
    {
        size_t i = (size_t)isThisCall + (size_t)isRVOCandidate;
        for (; i < count; ++i)
        {
            params[i + 1] = toDIType(a_pSignature->getParameterType(i - (size_t)isThisCall - (size_t)isRVOCandidate));
        }
    }
    if (isThisCall)
    {
        params[1] = m_DIBuilder.createObjectPointerType(toDIType(a_pThisType->addPointer()));
    }
    if (isRVOCandidate)
    {
        params[isThisCall + 1] = toDIType(a_pSignature->getReturnType()->addLValueReference());
    }
    return m_DIBuilder.createSubroutineType(m_DIBuilder.getOrCreateTypeArray(toArrayRef(params)),
                                            llvm::DINode::FlagZero, getDwarfCC(a_ABI));
}

llvm::DISubroutineType* DebugContext::toDIFunctionType(FunctionType* a_pFunctionType, ABI a_ABI, Type* a_pThisType)
{
    bool                         isRVOCandidate = a_pFunctionType->isRVOCandidate();
    bool                         isThisCall = a_pThisType != nullptr;
    size_t                       count = a_pFunctionType->getParameterTypeCount() + isThisCall + isRVOCandidate;
    SmallVector<llvm::Metadata*> params;
    params.push_back(toDIType(isRVOCandidate ? PHANTOM_TYPEOF(void) : a_pFunctionType->getReturnType()));
    params.resize(count + 1);
    {
        size_t i = (size_t)isThisCall + (size_t)isRVOCandidate;
        for (; i < count; ++i)
        {
            params[i + 1] =
            toDIType(a_pFunctionType->getParameterType(i - (size_t)isThisCall - (size_t)isRVOCandidate));
        }
    }
    if (a_pThisType)
    {
        params[1] = m_DIBuilder.createObjectPointerType(toDIType(a_pThisType->addPointer()));
    }
    if (isRVOCandidate)
    {
        params[isThisCall + 1] = toDIType(a_pFunctionType->getReturnType()->addLValueReference());
    }
    return m_DIBuilder.createSubroutineType(m_DIBuilder.getOrCreateTypeArray(toArrayRef(params)));
}

void DebugContext::replaceTemporaries()
{
    SmallVector<phantom::Pair<Type*, llvm::DICompositeType*>, 1024> temp;
    temp.reserve(m_Temps.size());
    while (true)
    {
        size_t sizeBefore = m_Temps.size();
        for (auto& pair : m_Temps)
        {
            if (pair.second)
            {
                temp.emplace_back(pair);
                pair.second = nullptr;
            }
        }
        for (auto& pair : temp)
            m_DIBuilder.replaceTemporary(llvm::TempDICompositeType(pair.second), toDIType(pair.first));
        if (sizeBefore == m_Temps.size())
            break;
        temp.clear();
    }
}

void DebugContext::createCompileUnit(StringView _name)
{
    m_DICompilationUnit = m_DIBuilder.createCompileUnit(
    llvm::dwarf::DW_LANG_C_plus_plus_14,
    m_DIBuilder.createFile(toStringRef(_name), "") /* no file for compilation unit, use the plugin file */, "", true,
    "", 0);
}

} // namespace jit
} // namespace phantom

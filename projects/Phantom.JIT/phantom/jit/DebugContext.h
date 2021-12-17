#pragma once

#include "prerequisites.h"

#if defined(_MSC_VER)
#    pragma warning(push, 0)
#elif defined(__clang__)
#    pragma clang diagnostic push
#    pragma clang diagnostic ignored "-Wall"
#    pragma clang diagnostic ignored "-Wextra"
#endif
#include "llvm/IR/DIBuilder.h"

#include <unordered_map>
#include <unordered_set>
#if defined(_MSC_VER)
#    pragma warning(pop)
#elif defined(__clang__)
#    pragma clang diagnostic pop
#endif

namespace llvm
{
class DICompileUnit;
class DICompositeType;
class DINode;
class DIScope;
class DINamespace;
class DIDerivedType;
class DIFile;
class DIType;
class DISubroutineType;
class Metadata;
class Module;
} // namespace llvm

namespace phantom
{
namespace jit
{
class DebugContext
{
public:
    DebugContext(Context* a_pContext, llvm::Module* a_pLLVMModule);

    lang::Type* fromDIType(llvm::DIType* type);

#define Context_to(DI, LangElem)                                                                                       \
    llvm::DI* to##DI(lang::LangElem* a_pElement)                                                                       \
    {                                                                                                                  \
        auto found = m_toDINodeMap.find(a_pElement);                                                                   \
        if (found != m_toDINodeMap.end())                                                                              \
            return static_cast<llvm::DI*>(found->second);                                                              \
        llvm::DI* di = _to##DI(a_pElement);                                                                            \
        m_fromDINodeMap[di] = a_pElement;                                                                              \
        return static_cast<llvm::DI*>(m_toDINodeMap[a_pElement] = di);                                                 \
    }

    Context_to(DIType, Type);
    Context_to(DIDerivedType, Field);
    Context_to(DISubprogram, Method);
    Context_to(DISubprogram, Function);

    llvm::DISubprogram* toDISubprogram(lang::Subroutine* a_pSubroutine);

    llvm::DIType* toFwdDIType(lang::Type* a_pType);

    llvm::DISubroutineType* toDIFunctionType(lang::Signature* a_pSignature, lang::ABI a_ABI,
                                             lang::Type* a_pThisType = nullptr);
    llvm::DISubroutineType* toDIFunctionType(lang::FunctionType* a_pFunctionType, lang::ABI a_ABI,
                                             lang::Type* a_pThisType = nullptr);
    llvm::DIFile*           getOrCreateDIFile(phantom::lang::LanguageElement* a_pElement);
    llvm::DIFile*           getOrCreateDIFile(phantom::lang::Source* a_pSource);

    llvm::DIScope* getOrCreateDIScope(phantom::lang::LanguageElement* a_pScope);

    String getMangledName(lang::Symbol* a_pSymbol, CppManglerFlags a_Flags = 0, int a_Discriminator = 0);

    void replaceTemporaries();

    llvm::DIType*        _toDIStruct(lang::Structure* a_pClassType);
    llvm::DIType*        _toDIClass(lang::Class* a_pClass);
    llvm::DIType*        _toDIUnion(lang::Union* a_pUnion);
    llvm::DIType*        _toDIType(lang::Type* a_pType);
    llvm::DIDerivedType* _toDIDerivedType(lang::Field* a_pField);
    llvm::DISubprogram*  _toDISubprogram(lang::Method* a_pMethod);
    llvm::DIType*        _toDIEnum(lang::Enum* param1);
    llvm::StringRef      _getSymbolName(lang::Symbol* a_pSym);
    void                 _fetchDIMembers(lang::ClassType* a_pClass, SmallVector<llvm::Metadata*>& members);
    void                 _fetchDIMembers(lang::Class* a_pClass, SmallVector<llvm::Metadata*>& members);
    void                 _fetchDIMembers(lang::Union* a_pUnion, SmallVector<llvm::Metadata*>& a_Members);
    void                 _createVTableInfo(lang::Class* a_pClass, SmallVector<llvm::Metadata*>& a_Members);
    void                 _pushDIFwd(lang::ClassType* a_pClass) { m_Fwds.push_back(a_pClass); }
    void                 _popDIFwd() { m_Fwds.pop_back(); }
    bool                 _hasDIFwd(lang::Type* a_pType) const
    {
        return std::find(m_Fwds.begin(), m_Fwds.end(), a_pType) != m_Fwds.end();
    }
    llvm::DISubprogram* _toDISubprogram(lang::Function* a_pFunction);

    void _writeNatvis(std::ostream& _out);

    std::unordered_map<phantom::lang::Source*, llvm::DIFile*> m_DIFiles;
    llvm::DICompileUnit*                                      m_DICompilationUnit = nullptr;

    std::unordered_map<lang::Namespace*, llvm::DINamespace*>    m_DINamespaces;
    std::unordered_map<lang::Block*, llvm::DIScope*>            m_DIBlocks;
    std::unordered_map<lang::Module*, llvm::DIScope*>           m_DIModules;
    std::unordered_map<lang::LanguageElement*, llvm::DINode*>   m_toDINodeMap;
    std::unordered_map<llvm::DINode*, lang::LanguageElement*>   m_fromDINodeMap;
    phantom::SmallMap<lang::Type*, llvm::DICompositeType*, 256> m_Temps;
    llvm::SmallVector<lang::ClassType*, 2048>                   m_Fwds;
    llvm::DIBuilder                                             m_DIBuilder;
    Context*                                                    m_pContext;
    llvm::JITPDBMemoryManager*                                  m_pDebugDllMemMgr = nullptr;
    std::unordered_map<lang::Symbol*, String>                   m_SymbolNames;
    std::unordered_set<lang::Symbol*>                           m_NativeSymbols;
    void                                                        finalize();
    void                                                        createCompileUnit(StringView _name);
};

} // namespace jit
} // namespace phantom

// license [
// This file is part of the Phantom project. Copyright 2011-2020 Vivien Millet.
// Distributed under the MIT license. Text available here at
// https://github.com/vlmillet/phantom
// ]

#pragma once

/* ****************** Includes ******************* */
#include "prerequisites.h"

#pragma warning(push, 0)
#include <llvm/IR/BasicBlock.h>
#pragma warning(pop)
#include <phantom/lang/CodeGenerator.h>
#include <phantom/lang/Subroutine.h>
#include <phantom/utils/Functor.h>
/* **************** Declarations ***************** */
/* *********************************************** */

namespace llvm
{
class FunctionType;
struct GenericValue;
class DISubprogram;
class DISubroutineType;
} // namespace llvm

namespace phantom
{
namespace jit
{
class CodeGenerator;
class Context;
class DebugContext;
struct CodeGeneratorPrivate;

class PHANTOM_EXPORT_PHANTOM_JIT Subroutine : public CodeGeneratorData
{
public:
    friend struct CodeGeneratorPrivate;

    Subroutine(lang::Subroutine* a_pSubroutine);
    ~Subroutine(void);

    virtual void finish();
    virtual void codeGen();
    virtual void setupFunctionPointers();
    virtual void release();

    lang::Subroutine* getSubroutine() const;

    lang::Signature*    getSignature() const;
    lang::FunctionType* getFunctionType() const;

    int               label(Label* l);
    llvm::BasicBlock* new_block();

    Value load(Value value) const;
    Value store(Value dest, Value value);
    Value loadRelative(Value value, ptrdiff_t offset, lang::Type* type);
    Value storeRelative(Value dest, ptrdiff_t offset, Value value);
    Value adjustPointer(Value value, ptrdiff_t offset, lang::Type* a_pType);
    Value adjustPointer(Value value, Value offset, lang::Type* a_pType);
    Value loadElem(Value base_addr, Value index, lang::Type* elem_type);
    Value loadElemAddress(Value base_addr, size_t index, lang::Type* elem_type);
    Value loadElemAddress(Value base_addr, Value index, lang::Type* elem_type);
    Value storeElem(Value base_addr, Value index, Value value);
    Value checkNull(Value value);
    Value add(Value value1, Value value2);
    Value sub(Value value1, Value value2);
    Value mul(Value value1, Value value2);
    Value div(Value value1, Value value2);
    Value rem(Value value1, Value value2);
    Value neg(Value value1);
    Value and_(Value value1, Value value2);
    Value or_(Value value1, Value value2);
    Value xor_(Value value1, Value value2);
    Value not_(Value value1);
    Value shl(Value value1, Value value2);
    Value shr(Value value1, Value value2);
    Value ushr(Value value1, Value value2);
    Value sshr(Value value1, Value value2);
    Value eq(Value value1, Value value2);
    Value ne(Value value1, Value value2);
    Value lt(Value value1, Value value2);
    Value le(Value value1, Value value2);
    Value gt(Value value1, Value value2);
    Value ge(Value value1, Value value2);
    Value cmpl(Value value1, Value value2);
    Value cmpg(Value value1, Value value2);
    Value toBool(Value value1);
    Value toNotBool(Value value1);
    Value branch(Label* label);
    Value branchIf(Value value, Label* label);
    Value branchIfNot(Value value, Label* label);
    Value jumpTable(Value value, Label* labels, uint num_labels);
    //     Value   addressOf (Value value1);
    //     Value   referenceOf (Value value1);
    //     Value   addressOfLabel (Label *label);
    Value convert(Value value, lang::Type* dest);
    Value callSubroutine(lang::Subroutine* a_pSubroutine, Value* args, uint a_uiArgCount, int flags,
                         bool a_bForceNonVirtual = false);
    Value callNative(lang::Subroutine* a_pSubroutine, Value* args, uint a_uiArgCount, bool a_bForceNonVirtual);
    Value callNativePtr(const char* name, void* native_func, lang::ABI abi, lang::FunctionType* a_pSignature,
                        lang::ClassType* a_pThisType, Value* args, unsigned int a_uiArgCount);
    //     Value   callIntrinsic( StringView a_strInstrinsicName, Value* a_pValues, size_t a_uiValueCount
    //     );
    //     Value   callIntrinsic( StringView a_strInstrinsicName, Value v0 );
    //     Value   callIntrinsic( StringView a_strInstrinsicName, Value v0, Value v1 );
    //     Value   callIntrinsic( StringView a_strInstrinsicName, Value v0, Value v1, Value
    //     v2 ); Value   callIntrinsic( StringView a_strInstrinsicName, Value v0, Value v1,
    //     Value v2, Value v3 ); int         incomingReg (Value value, int reg); int
    //     incomingFramePosn (Value value, int frame_offset); int         outgoingReg (Value value, int
    //     reg); int         outgoingFramePosn (Value value, int frame_offset); int         returnReg
    //     (Value value, int reg); int         setupForNested (int nested_level, int reg); int flushStruct
    //     (Value value); Value   import (Value value); int         push (Value value); int
    //     pushPtr (Value value, Type* type); int         setParam (Value value, int offset); int
    //     setParamPtr (Value value, Type* type, int offset); int         pushReturnAreaPtr (); int popStack (int
    //     num_items); int         deferPopStack (int num_items); int         flushDeferPop (int num_items);
    Value returnValue(Value value);
    Value returnVoid();
    //    int       returnPtr (Value value, Type* type);
    Value fallbackReturn();
    //     int         throwValue(Value value);
    //     Value   getCallStack ();
    //     Value   thrownException ();
    //     int         usesCatcher ();
    //     Value   startCatcher ();
    //     int         branchIfPCNotInRange (Label start_label, Label end_label, Label *label);
    //     int         rethrowUnhandled ();
    //     int         startFinally (Label *finally_label);
    //     int         returnFromFinally ();
    //     int         callFinally (Label *finally_label);
    //     Value   startFilter (Label *label, Type* type);
    //     int         returnFromFilter (Value value);
    //     Value   callFilter (Label *label, Value value, Type* type);
    Value memcpy PHANTOM_PREVENT_MACRO_SUBSTITUTION(Value dest, Value src, Value size);
    Value memmove PHANTOM_PREVENT_MACRO_SUBSTITUTION(Value dest, Value src, Value size);
    Value memset PHANTOM_PREVENT_MACRO_SUBSTITUTION(Value dest, Value value, Value size);
    Value alloca PHANTOM_PREVENT_MACRO_SUBSTITUTION(Value size);
    //     int         moveBlocksToEnd (Label from_label, Label to_label);
    //     int         moveBlocksToStart (Label from_label, Label to_label);
    //     int         markOffset (int offset);

    //    void        ref(Value value);

    //    Value   getStructPointer () const;
    void  createLocalVariableDebugInfo(lang::LocalVariable* a_pLocal, Value a_Address);
    void  createBlockVariableDebugInfo(lang::LocalVariable* a_pLocal, Value a_Address);
    void  createArgumentDebugInfo(lang::Parameter* a_pParameter, Value a_Address);
    Value getOrCreateAlloca(lang::LanguageElement* a_pRelatedBlockElement, lang::Type* type, size_t a_Idx = 0,
                            phantom::Functor<void(Value)> _InsertDeclare = phantom::Functor<void(Value)>());
    Value createCharConstant(char const_value);
    Value createUCharConstant(uchar const_value);
    Value createShortConstant(short const_value);
    Value createUShortConstant(ushort const_value);
    Value createIntConstant(int const_value);
    Value createUIntConstant(uint const_value);
    Value createSizeTConstant(size_t const_value);
    Value createPtrDiffTConstant(ptrdiff_t const_value);
    Value createVoidPtrConstant(void* const_value);
    template<typename t_Ty>
    Value createPtrConstant(t_Ty* const_value)
    {
        return createPtrConstant(const_value, static_cast<Pointer*>(PHANTOM_TYPEOF(t_Ty)->makePointer()));
    }
    Value createPtrConstant(void* const_value, lang::Pointer* a_pType);
    Value createLongLongConstant(longlong const_value);
    Value createULongLongConstant(ulonglong const_value);
    Value createFloatConstant(float const_value);
    Value createDoubleConstant(double const_value);
    Value getParameter(uint param) const;
    Value getDebugInfoParameterValue(uint param) const;
    Value getThis() const;
    Value getRVOParameter() const;
    Value returnAddress(lang::Type* a_pReturnType);

    lang::ABI getABI() const;

    void compileDebugPrint(StringView to_print);
    /*void        iterInit (jit_insn_iter_t*iter, jit_block block);
    void        iterInitLast (jit_insn_iter_t*iter, jit_block block);
    jit_insn    iterNext (jit_insn_iter_t*iter);
    jit_insn    iterPrevious (jit_insn_iter_t *iter);*/

    bool isCompiled() const;

    llvm::Type*         getJitIntType(size_t a_Size) const;
    llvm::Type*         toJitType(lang::Type* a_pType) const;
    llvm::FunctionType* toJitFunctionCallType(lang::FunctionType* a_pFunctionType, lang::ClassType* a_pThisType) const;
    unsigned            toJitABI(lang::ABI abi) const;

    Context*      getContext() const;
    DebugContext* getDebugContext() const;

    FunctionEntry         getJitFunction() const { return m_jit_function; }
    virtual FunctionEntry getJitCallFunction() const { return m_jit_function; }

    static void instructionCompilationCallback(void* insn, byte* start, byte* end);
    static void compilationSuccessCallback(void* func);

    String convertIntrinsicName(StringView a_strInstrinsicName);

    void toGV(void* a_pSrc, lang::Type* a_pType, llvm::GenericValue& gv);

    void fromGV(void* a_pDest, lang::Type* a_pType, const llvm::GenericValue& gv);

    Value takeTemporary(lang::Expression* a_pExpression);

    void registerTemporary(lang::Expression* a_pExpression, Value a_Temp);

    bool hasTemporaries() const { return m_Temporaries.size() != 0; }

    void apply(lang::ExecutionContext& context, void** args, size_t count);

    String getMangledName(CppManglerFlags a_Flags = 0, int a_Discriminator = 0) const;

protected:
    Value        autoValue(llvm::Value* v);
    lang::Type*  prepareArithmeticBinOp(Value& value1, Value& value2);
    virtual void compilerAboutToBeChanged(CodeGeneratorPrivate* a_pCodeGenerator) override;
    virtual void compilerChanged(CodeGeneratorPrivate* a_pCodeGenerator) override;

    void  createApplyFunction();
    void  _compileTraceCallBegin(StringView func);
    void  _compileTraceCallEnd();
    void  _createAllocaHeader();
    void  _destroyAllocaHeader();
    Value _createAlloca(lang::Type* type, phantom::Functor<void(Value)> _InsertDeclare);
    Value _callNativePtr(const char* a_pName, void* a_pNativePtr, lang::ABI abi, lang::FunctionType* a_pFunctionType,
                         lang::ClassType* a_pThisType, Value* args, unsigned int a_uiArgCount);
    static void ApplyFunctionAttributes(llvm::Function* pFunc, bool a_UnwindTable = true, bool a_Naked = false,
                                        bool a_Align = true);

protected:
    lang::CodeRangeLocations                                sm_CodeRangeLocationStack;
    FunctionEntry                                           m_jit_function;
    FunctionEntry                                           m_jit_call_function;
    FunctionEntry                                           m_jit_apply_function;
    lang::ABI                                               m_eAbi;
    SmallMap<lang::Expression*, Value>                      m_Temporaries;
    SmallSet<lang::Expression*>                             m_NonCompiledExpressions;
    SmallMap<lang::LanguageElement*, SmallVector<Value, 2>> m_Allocas;
    llvm::DISubprogram*                                     m_debugInfo = nullptr;
    size_t                                                  m_AllocaInsertingPoint = 0;
    SmallSet<Label*>                                        m_OrphanBlocks;
};

} // namespace jit
} // namespace phantom

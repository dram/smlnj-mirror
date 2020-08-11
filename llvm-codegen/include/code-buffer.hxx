/// \file code-buffer.hxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief The code_buffer class wraps up the LLVM code generator state.
///
/// \author John Reppy
///

#ifndef __CODE_BUFFER_HXX__
#define __CODE_BUFFER_HXX__

#include <memory>
#include <string>
#include <vector>
#include <unordered_map>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"

/*DEBUG*/#include "llvm/Support/raw_os_ostream.h"
/*DEBUG*/#include "llvm/Support/Debug.h"

#include "lambda-var.hxx"
#include "sml-registers.hxx"

using Value = llvm::Value;
using Type = llvm::Type;
using Args_t = std::vector<llvm::Value *>;

namespace llvm {
    class TargetMachine;
    namespace legacy {
	class FunctionPassManager;
    }
}

namespace CFG {
    class param;
    class frag;
    class attrs;
    class cluster;
    enum class frag_kind;
}

// map from lvars to values of type T*
template <typename T>
using lvar_map_t = std::unordered_map<LambdaVar::lvar, T *>;

// the different kinds of fragments.  The first two are restricted
// to entry fragments for clusters; all others are `INTERNAL`
//
using frag_kind = CFG::frag_kind;

// The code_buffer class encapsulates the current state of code generation, as well
// as information about the target architecture.  It is passed as an argument to
// all of the `codegen` methods in the CFG representation.
//
class code_buffer {
  public:

  // create the code buffer for the given target
    static code_buffer *create (std::string const & target);

    void optimize ();

  // initialize the code buffer for a new module
    void beginModule (std::string const & src, int nClusters);
    void endModule ();

  // mark the beginning/end of a cluster
    void beginCluster (CFG::cluster *cluster, llvm::Function *fn);
    void endCluster ();

  // initialize the code buffer for a new fragment
    void beginFrag ();

  // get the IR builder
    llvm::IRBuilder<> build () { return this->_builder; }

  // define a new LLVM function for a cluster with the given type; the `isFirst` flag
  // should be true for the entry function of the module.
    llvm::Function *newFunction (llvm::FunctionType *fnTy, std::string const &name, bool isFirst);

  // create a function type from a vector of parameter types.  This function adds
  // the extra types corresponding to the SML registers and for the unused
  // argument registers for continuations
    llvm::FunctionType *createFnTy (frag_kind kind, std::vector<Type *> const & tys) const;

  // create a vector to hold the types of function paramaters (including for fragments),
  // where `n` is the number of arguments to the call.  This method initialize the
  // prefix of the vector with the types of the SML registers
    std::vector<Type *> createParamTys (frag_kind kind, int n) const;

  // create a vector to hold the arguments of a call (APPLY/THROW/GOTO), where
  // `n` is the number of arguments to the call.  This method initialize the
  // prefix of the vector with the values of the SML registers
    Args_t createArgs (frag_kind kind, int n);

    void setupStdEntry (CFG::attrs *attrs, CFG::frag *frag);

  // setup the parameter lists for a fragment
    void setupFragEntry (CFG::frag *frag, std::vector<llvm::PHINode *> &phiNodes);

  // get the LLVM value that represents the specified SML register
    Value *mlReg (sml_reg_id r)
    {
	Value *reg = this->_regState.get(r);
	if (reg == nullptr) {
	    return this->_loadMemReg(r);
	} else {
	    return reg;
	}
    }

  // assign a value to an SML register
    void setMLReg (sml_reg_id r, Value *v)
    {
	Value *reg = this->_regState.get(r);
	if (reg == nullptr) {
	    return this->_storeMemReg(r, v);
	} else {
	    this->_regState.set(r, v);
	}
    }

  // save and restore the SML register state to a cache object
    void saveSMLRegState (reg_state & cache) { cache.copyFrom (this->_regState); }
    void restoreSMLRegState (reg_state const & cache) { this->_regState.copyFrom (cache); }

  // target parameters
    int wordSzInBytes () const { return this->_wordSzB; }
    bool is64Bit () const { return (this->_wordSzB == 8); }
    target_info const *targetInfo () const { return this->_target; }

  // align the allocation pointer for 64 bits on 32-bit machines.  The resulting
  // alloc pointer points to the location of the object descriptor, so adding
  // wordSzInBytes() should produce an 8-byte aligned address
    Value *alignedAllocPtr ()
    {
	if (this->is64Bit()) {
	    return this->mlReg (sml_reg_id::ALLOC_PTR);
	} else {
	    return this->createIntToPtr(
		this->createOr(
                    this->createPtrToInt (this->mlReg (sml_reg_id::ALLOC_PTR)),
		    this->uConst (4)),
		this->objPtrTy);
	}
    }

  // cached types
    llvm::IntegerType *i8Ty;
    llvm::IntegerType *i16Ty;
    llvm::IntegerType *i32Ty;
    llvm::IntegerType *i64Ty;
    Type *f32Ty;
    Type *f64Ty;
    llvm::IntegerType *intTy;	// native integer type
    Type *mlValueTy;		// the uniform ML value type, which is a pointer to the intTy
    Type *objPtrTy;		// pointer into the heap (i.e., a pointer to an ML value)
    Type *bytePtrTy;		// "char *" type
    Type *voidTy;		// "void"
    llvm::StructType *gcRetTy;	// return struct type for GC calls
    llvm::FunctionType *gcFnTy; // type of call-gc function

    llvm::IntegerType *iType (int sz) const
    {
	if (sz == 64) return this->i64Ty;
	else if (sz == 32) return this->i32Ty;
	else if (sz == 16) return this->i16Ty;
	else return this->i8Ty;
    }
    Type *fType (int sz) const
    {
	if (sz == 64) return this->f64Ty;
	else return this->f32Ty;
    }

  // ensure that a value has the `mlValue` type
    Value *asMLValue (Value *v)
    {
	auto ty = v->getType();
	if (ty == this->mlValueTy) {
	    return v;
	} else if (ty->isPointerTy()) {
	    return this->_builder.CreateBitCast (v, this->mlValueTy);
	} else {
	    return this->_builder.CreateIntToPtr(v, this->mlValueTy);
	}
    }

  // ensure that a value has the `mlValue` type
    Value *asObjPtr (Value *v)
    {
	if (! v->getType()->isPointerTy()) {
	    return this->_builder.CreateIntToPtr(v, this->objPtrTy);
	} else {
	    return this->_builder.CreateBitCast(v, this->objPtrTy);
	}
    }

  // ensure that a value is a machine-sized int type (assume that it is either intTy or mlValueTy
    Value *asInt (Value *v)
    {
	if (v->getType()->isPointerTy()) {
	    return this->_builder.CreatePtrToInt(v, this->intTy);
	} else {
	    return v;
	}
    }

  // cast an argument type to match the expected target type.  We assume that the
  // types are _not_ equal!
    Value *castTy (Type *srcTy, Type *tgtTy, Value *v);

/** NOTE: we may be able to avoid needing the signed constants */
  // signed integer constant of specified bit size
    llvm::ConstantInt *iConst (int sz, int64_t c) const
    {
	return llvm::ConstantInt::getSigned (this->iType (sz), c);
    }
  // signed constant of native size
    llvm::ConstantInt *iConst (int64_t c) const
    {
	return llvm::ConstantInt::getSigned (this->intTy, c);
    }
    llvm::ConstantInt *i32Const (int32_t n) const
    {
	return llvm::ConstantInt::getSigned (this->i32Ty, n);
    }

  // unsigned integer constant of specified bit size
    llvm::ConstantInt *uConst (int sz, uint64_t c) const
    {
	return llvm::ConstantInt::get (this->iType (sz), c);
    }
  // unsigned constant of native size
    llvm::ConstantInt *uConst (uint64_t c) const
    {
	return llvm::ConstantInt::get (this->intTy, c);
    }
    llvm::ConstantInt *u32Const (uint32_t n) const
    {
	return llvm::ConstantInt::get (this->i32Ty, n);
    }

  // insert a binding into the label-to-cluster map
    void insertCluster (LambdaVar::lvar lab, CFG::cluster *cluster)
    {
	std::pair<LambdaVar::lvar,CFG::cluster *> pair(lab, cluster);
	this->_clusterMap.insert (pair);
    }

  // lookup a binding in the label-to-cluster map
    CFG::cluster *lookupCluster (LambdaVar::lvar lab)
    {
	lvar_map_t<CFG::cluster>::const_iterator got = this->_clusterMap.find(lab);
	if (got == this->_clusterMap.end()) {
	    return nullptr;
	} else {
	    return got->second;
	}
    }

  // evaluate a LABEL (which maps to the given function) to an absolute address
    Value *evalLabel (llvm::Function *fn);

  // insert a binding into the label-to-fragment map
    void insertFrag (LambdaVar::lvar lab, CFG::frag *frag)
    {
	std::pair<LambdaVar::lvar,CFG::frag *> pair(lab, frag);
	this->_fragMap.insert (pair);
    }

  // lookup a binding in the label-to-fragment map
    CFG::frag *lookupFrag (LambdaVar::lvar lab)
    {
	lvar_map_t<CFG::frag>::const_iterator got = this->_fragMap.find(lab);
	if (got == this->_fragMap.end()) {
	    return nullptr;
	} else {
	    return got->second;
	}
    }

  // insert a binding into the lvar-to-value map
    void insertVal (LambdaVar::lvar lv, Value *v)
    {
	std::pair<LambdaVar::lvar,Value *> pair(lv, v);
	this->_vMap.insert (pair);
    }

  // lookup a binding in the lvar-to-value map
    Value *lookupVal (LambdaVar::lvar lv)
    {
	lvar_map_t<Value>::const_iterator got = this->_vMap.find(lv);
	if (got == this->_vMap.end()) {
	    return nullptr;
	} else {
	    return got->second;
	}
    }

  // create a fresh basic block in the current function
    llvm::BasicBlock *newBB ()
    {
	return llvm::BasicBlock::Create (this->_context, "", this->_curFn);
    }

  // return the block address for a basic block in the current function
    llvm::BlockAddress *blockAddr (llvm::BasicBlock *bb)
    {
	return llvm::BlockAddress::get (this->_curFn, bb);
    }

  // set the current block to insert instructions in
    void setInsertPoint (llvm::BasicBlock *bb)
    {
	this->_builder.SetInsertPoint (bb);
    }

  // get the current basic block
    llvm::BasicBlock *getCurBB ()
    {
	return this->_builder.GetInsertBlock ();
    }

  // utility function for allocating a record of ML values (pointers or
  // tagged ints).
    Value *allocRecord (uint64_t desc, Args_t const & args);

  // Create a GC invocation where `params` are the live variables and `frag` is nullptr
  // for GC checks at standard entries and is the fragment for GC checks at known functions.
    llvm::BasicBlock *invokeGC (CFG::frag const *frag, frag_kind kind);

  // return the basic-block that contains the Overflow trap generator
    llvm::BasicBlock *getOverflowBB ();

  // return branch-weight meta data, where `prob` represents the probability of
  // the true branch and is in the range 1..999.
    llvm::MDNode *branchProb (int prob);

  // get the branch-weight meta data for overflow-trap branches
    llvm::MDNode *overflowWeights ();

  // return an address in the stack with the given `ptrTy`.
    Value *stkAddr (Type *ptrTy, int offset)
    {
	if (this->_readReg == nullptr) {
	    this->_initSPAccess();
	}

	return this->createIntToPtr (
	    this->createAdd(
		this->_builder.CreateCall(
		    this->_readReg->getFunctionType(),
		    this->_readReg,
		    { llvm::MetadataAsValue::get(this->_context, this->_spRegMD) }),
		this->iConst(offset)),
	    ptrTy);

    }

  // get intinsics; these are cached for the current module
    llvm::Function *sadd32WOvflw () const
    {
	if (this->_sadd32WO == nullptr) {
	    this->_sadd32WO =
		_getIntrinsic (llvm::Intrinsic::sadd_with_overflow, this->i32Ty);
	}
	return this->_sadd32WO;
    }
    llvm::Function *ssub32WOvflw () const
    {
	if (this->_ssub32WO == nullptr) {
	    this->_ssub32WO =
		_getIntrinsic (llvm::Intrinsic::ssub_with_overflow, this->i32Ty);
	}
	return this->_ssub32WO;
    }
    llvm::Function *smul32WOvflw () const
    {
	if (this->_smul32WO == nullptr) {
	    this->_smul32WO =
		_getIntrinsic (llvm::Intrinsic::smul_with_overflow, this->i32Ty);
	}
	return this->_smul32WO;
    }
    llvm::Function *sadd64WOvflw () const
    {
	if (this->_sadd64WO == nullptr) {
	    this->_sadd64WO =
		_getIntrinsic (llvm::Intrinsic::sadd_with_overflow, this->i64Ty);
	}
	return this->_sadd64WO;
    }
    llvm::Function *ssub64WOvflw () const
    {
	if (this->_ssub64WO == nullptr) {
	    this->_ssub64WO =
		_getIntrinsic (llvm::Intrinsic::ssub_with_overflow, this->i64Ty);
	}
	return this->_ssub64WO;
    }
    llvm::Function *smul64WOvflw () const
    {
	if (this->_smul64WO == nullptr) {
	    this->_smul64WO =
		_getIntrinsic (llvm::Intrinsic::smul_with_overflow, this->i64Ty);
	}
	return this->_smul64WO;
    }
    llvm::Function *fabs32 () const
    {
	if (this->_fabs32 == nullptr) {
	    this->_fabs32 = _getIntrinsic (llvm::Intrinsic::fabs, this->i32Ty);
	}
	return this->_fabs32;
    }
    llvm::Function *fabs64 () const
    {
	if (this->_fabs64 == nullptr) {
	    this->_fabs64 = _getIntrinsic (llvm::Intrinsic::fabs, this->i64Ty);
	}
	return this->_fabs64;
    }
    llvm::Function *sqrt32 () const
    {
	if (this->_sqrt32 == nullptr) {
	    this->_sqrt32 = _getIntrinsic (llvm::Intrinsic::sqrt, this->i32Ty);
	}
	return this->_sqrt32;
    }
    llvm::Function *sqrt64 () const
    {
	if (this->_sqrt64 == nullptr) {
	    this->_sqrt64 = _getIntrinsic (llvm::Intrinsic::sqrt, this->i64Ty);
	}
	return this->_sqrt64;
    }

  /***** shorthand for LLVM integer instructions (with argument coercions) *****/
/* FIXME: Note that for now, we assume that all arithmetic is in the native integer size! */
    Value *createAdd (Value *a, Value *b)
    {
	return this->_builder.CreateAdd (this->asInt(a), this->asInt(b));
    }
    Value *createAnd (Value *a, Value *b)
    {
	return this->_builder.CreateAnd (this->asInt(a), this->asInt(b));
    }
    Value *createAShr (Value *a, Value *b)
    {
	return this->_builder.CreateAShr (this->asInt(a), this->asInt(b));
    }
    Value *createICmp (llvm::CmpInst::Predicate cmp, Value *a, Value *b)
    {
	return this->_builder.CreateICmp (cmp, this->asInt(a), this->asInt(b));
    }
    Value *createICmpEQ (Value *a, Value *b)
    {
	return this->_builder.CreateICmpEQ (this->asInt(a), this->asInt(b));
    }
    Value *createICmpNE (Value *a, Value *b)
    {
	return this->_builder.CreateICmpNE (this->asInt(a), this->asInt(b));
    }
    Value *createICmpSLT (Value *a, Value *b)
    {
	return this->_builder.CreateICmpSLT (this->asInt(a), this->asInt(b));
    }
    Value *createLShr (Value *a, Value *b)
    {
	return this->_builder.CreateLShr (this->asInt(a), this->asInt(b));
    }
    Value *createMul (Value *a, Value *b)
    {
	return this->_builder.CreateMul (this->asInt(a), this->asInt(b));
    }
    Value *createOr (Value *a, Value *b)
    {
	return this->_builder.CreateOr (this->asInt(a), this->asInt(b));
    }
    Value *createSDiv (Value *a, Value *b)
    {
	return this->_builder.CreateSDiv (this->asInt(a), this->asInt(b));
    }
    Value *createShl (Value *a, Value *b)
    {
	return this->_builder.CreateShl (this->asInt(a), this->asInt(b));
    }
    Value *createSIToFP (Value *v, Type *ty)
    {
	return this->_builder.CreateSIToFP (v, ty);
    }
    Value *createSRem (Value *a, Value *b)
    {
	return this->_builder.CreateSRem (this->asInt(a), this->asInt(b));
    }
    Value *createSub (Value *a, Value *b)
    {
	return this->_builder.CreateSub (this->asInt(a), this->asInt(b));
    }
    Value *createUDiv (Value *a, Value *b)
    {
	return this->_builder.CreateUDiv (this->asInt(a), this->asInt(b));
    }
    Value *createURem (Value *a, Value *b)
    {
	return this->_builder.CreateURem (this->asInt(a), this->asInt(b));
    }
    Value *createXor (Value *a, Value *b)
    {
	return this->_builder.CreateXor (this->asInt(a), this->asInt(b));
    }

    Value *createSExt (Value *v, Type *ty)
    {
	return this->_builder.CreateSExt (v, ty);
    }
    Value *createZExt (Value *v, Type *ty)
    {
	return this->_builder.CreateZExt (v, ty);
    }
    Value *createTrunc (Value *v, Type *ty)
    {
	return this->_builder.CreateTrunc(v, ty);
    }

  /***** shorthand for LLVM floating-point instructions *****/
    Value *createFAdd (Value *a, Value *b) { return this->_builder.CreateFAdd (a, b); }
    Value *createFCmp (llvm::CmpInst::Predicate cmp, Value *a, Value *b)
    {
	return this->_builder.CreateFCmp (cmp, a, b);
    }
    Value *createFDiv (Value *a, Value *b) { return this->_builder.CreateFDiv (a, b); }
    Value *createFMul (Value *a, Value *b) { return this->_builder.CreateFMul (a, b); }
    Value *createFNeg (Value *v) { return this->_builder.CreateFNeg (v); }
    Value *createFPToSI (Value *v, Type *ty) { return this->_builder.CreateFPToSI (v, ty); }
    Value *createFSub (Value *a, Value *b) { return this->_builder.CreateFSub (a, b); }

  /***** shorthand for load/store instructions *****/
    Value *createLoad (Type *ty, Value *adr)
    {
      // NOTE: our loads are always aligned to the ABI alignment requirement
	return this->_builder.CreateAlignedLoad (ty, adr, 0);
    }

  /***** shorthand for type cast instructions *****/
    Value *createIntToPtr (Value *v, Type *ty)
    {
	return this->_builder.CreateIntToPtr (v, ty);
    }
    Value *createPtrToInt (Value *v)
    {
	return this->_builder.CreatePtrToInt(v, this->intTy);
    }
    Value *createBitCast (Value *v, Type *ty)
    {
	return this->_builder.CreateBitCast (v, ty);
    }
    Value *createPointerCast (Value *v, Type *ty)
    {
	return this->_builder.CreatePointerCast (v, ty);
    }

  /***** shorthand for other instructions *****/
    Value *createExtractValue (Value *v, int i)
    {
	return this->_builder.CreateExtractValue (v, i);
    }
    llvm::BranchInst *createBr (llvm::BasicBlock *bb)
    {
	return this->_builder.CreateBr (bb);
    }
    Value *createGEP (Value *base, Value *idx)
    {
	return this->_builder.CreateInBoundsGEP (base, { idx });
    }
    Value *createGEP (Type *ty, Value *base, Value *idx)
    {
	return this->_builder.CreateInBoundsGEP (
	    this->createBitCast (base, ty),
	    { idx });
    }
    Value *createGEP (Value *base, int32_t idx)
    {
	return this->_builder.CreateInBoundsGEP (base, { this->i32Const(idx) });
    }
    Value *createGEP (Type *ty, Value *base, int32_t idx)
    {
	return this->_builder.CreateInBoundsGEP (
	    this->createBitCast (base, ty),
	    { this->i32Const(idx) });
    }

  /***** Code generation *****/

  // compile to an in-memory code object
    void compile () const;

  // dump assembly code to stdout
    void dumpAsm () const;

  // dump assembly code to a file
    void dumpAsm (std::string const &stem) const;

  // dump machine code to an object file
    void dumpObj (std::string const &stem) const;

  /***** Debugging support *****/

  // dump the current module to stderr
    void dump () const;

  // run the LLVM verifier on the module
    bool verify () const;

  private:
    struct target_info const	*_target;
    llvm::LLVMContext		_context;
    llvm::IRBuilder<>		_builder;
    class mc_gen		*_gen;
    llvm::Module		*_module;	// current module
    llvm::Function		*_curFn;	// current LLVM function
    CFG::cluster		*_curCluster;	// current CFG cluster
    lvar_map_t<CFG::cluster>	_clusterMap;	// per-module mapping from labels to clusters
    lvar_map_t<CFG::frag>	_fragMap;	// pre-cluster map from labels to fragments
    lvar_map_t<Value>		_vMap;		// per-fragment map from lvars to values

  // a basic block for the current cluster that will force an Overflow trap
    llvm::BasicBlock		*_overflowBB;

  // tracking the state of the SML registers
    sml_registers		_regInfo;	// target-specific register info
    reg_state			_regState;	// current register values

  // target-machine properties
    int64_t _wordSzB;

  // cached intrinsic functions
    mutable llvm::Function *_sadd32WO;		// @llvm.sadd.with.overflow.i32
    mutable llvm::Function *_ssub32WO;		// @llvm.ssub.with.overflow.i32
    mutable llvm::Function *_smul32WO;		// @llvm.smul.with.overflow.i32
    mutable llvm::Function *_sadd64WO;		// @llvm.sadd.with.overflow.i64
    mutable llvm::Function *_ssub64WO;		// @llvm.ssub.with.overflow.i64
    mutable llvm::Function *_smul64WO;		// @llvm.smul.with.overflow.i64
    mutable llvm::Function *_fabs32;		// @llvm.fabs.f32
    mutable llvm::Function *_fabs64;		// @llvm.fabs.f64
    mutable llvm::Function *_sqrt32;		// @llvm.sqrt.f32
    mutable llvm::Function *_sqrt64;		// @llvm.sqrt.f64

  // cached @llvm.read_register + meta data to access stack
    llvm::Function *_readReg;
    llvm::MDNode *_spRegMD;

  // helper function for getting an intrinsic when it has not yet
  // been loaded for the current module.
  //
    llvm::Function *_getIntrinsic (llvm::Intrinsic::ID id, Type *ty) const;

    void _initSPAccess ();

  // function for loading a special register from memory
    Value *_loadMemReg (sml_reg_id r);

  // function for setting a special memory register
    void _storeMemReg (sml_reg_id r, Value *v);

  // constructor
    code_buffer (struct target_info const *target);

};

#endif // !__CODE_BUFFER_HXX__

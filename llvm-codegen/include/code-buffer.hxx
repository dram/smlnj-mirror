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
#include "llvm/IR/Module.h"
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
    namespace legacy {
	class FunctionPassManager;
    }
}

namespace CFG {
    class frag;
    class cluster;
}

// map from lvars to values of type T*
template <typename T>
using lvar_map_t = std::unordered_map<LambdaVar::lvar, T *>;


// The code_buffer class encapsulates the current state of code generation, as well
// as information about the target architecture.  It is passed as an argument to
// all of the `codegen` methods in the CFG representation.
//
class code_buffer {
  public:

  // create the code buffer for the given target
    static code_buffer *create (std::string const & target);

  // initialize the code buffer for a new module
    void beginModule (std::string const & src, int nClusters);
    void endModule ();

  // mark the beginning/end of a cluster
    void beginCluster (llvm::Function *fn);
    void endCluster ();

  // initialize the code buffer for a new fragment
    void beginFrag ();

  // get the IR builder
    llvm::IRBuilder<> build () { return this->_builder; }

  // define a new LLVM function for a cluster with the given type; the `isFirst` flag
  // should be true for the entry function of the module.
    llvm::Function *newFunction (llvm::FunctionType *fnTy, std::string const &name, bool isFirst);

  // create a function type from a vector of parameter types.  This function adds
  // the extra types corresponding to the SML registers
    llvm::FunctionType *createFnTy (std::vector<Type *> const & tys) const;

  // create a vector to hold the types of function paramaters (including for fragments),
  // where `n` is the number of arguments to the call.  This method initialize the
  // prefix of the vector with the types of the SML registers
    std::vector<Type *> createParamTys (int n) const;

  // create a vector to hold the arguments of a call (APPLY/THROW/GOTO), where
  // `n` is the number of arguments to the call.  This method initialize the
  // prefix of the vector with the values of the SML registers
    Args_t createArgs (int n);

    void setupStdEntry (CFG::frag *frag);

  // setup the parameter lists for a fragment
    void setupFragEntry (CFG::frag *frag, std::vector<llvm::PHINode *> &phiNodes);

  // get the LLVM value that represents the specified SML register
    Value *mlReg (sml_reg_id r) const { return this->_regState.get(r); }

  // assign a value to an SML register
    void setMLReg (sml_reg_id r, Value *v) { this->_regState.set(r, v); }

  // save and restore the SML register state to a cache object
    void saveSMLRegState (reg_state & cache) { cache.copyFrom (this->_regState); }
    void restoreSMLRegState (reg_state const & cache) { this->_regState.copyFrom (cache); }

  // target parameters
    int wordSzInBytes () { return this->_wordSzB; }
    bool is64Bit () { return (this->_wordSzB == 8); }

  // cached types
    llvm::IntegerType *i8Ty;
    llvm::IntegerType *i16Ty;
    llvm::IntegerType *i32Ty;
    llvm::IntegerType *i64Ty;
    Type *f32Ty;
    Type *f64Ty;
    llvm::IntegerType *intTy;	// native integer type
    Type *mlValueTy;	// the uniform ML value type, which is a pointer to the intTy
    Type *objPtrTy;	// pointer into the heap (i.e., a pointer to an ML value)
    Type *bytePtrTy;	// "char *" type
    Type *voidTy;		// "void"

    llvm::IntegerType *iType (int sz)
    {
	if (sz == 64) return this->i64Ty;
	else if (sz == 32) return this->i32Ty;
	else if (sz == 16) return this->i16Ty;
	else return this->i8Ty;
    }
    Type *fType (int sz)
    {
	if (sz == 64) return this->f64Ty;
	else return this->f32Ty;
    }

  // ensure that a value has the `mlValue` type
    Value *asMLValue (Value *v)
    {
	if (! v->getType()->isPointerTy()) {
	    return this->_builder.CreateIntToPtr(v, this->mlValueTy);
	} else {
	    return v;
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
    llvm::ConstantInt *iConst (int sz, int64_t c)
    {
	return llvm::ConstantInt::getSigned (this->iType (sz), c);
    }
  // signed constant of native size
    llvm::ConstantInt *iConst (int64_t c)
    {
	return llvm::ConstantInt::getSigned (this->intTy, c);
    }
    llvm::ConstantInt *i32Const (int32_t n)
    {
	return llvm::ConstantInt::getSigned (this->i32Ty, n);
    }

  // unsigned integer constant of specified bit size
    llvm::ConstantInt *uConst (int sz, uint64_t c)
    {
	return llvm::ConstantInt::get (this->iType (sz), c);
    }
  // unsigned constant of native size
    llvm::ConstantInt *uConst (uint64_t c)
    {
	return llvm::ConstantInt::get (this->intTy, c);
    }
    llvm::ConstantInt *u32Const (uint32_t n)
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

  // return the basic-block that contains the Overflow trap generator
    llvm::BasicBlock *getOverflowBB ();

  // return branch-weight meta data, where `prob` represents the probability of
  // the true branch and is in the range 1..999.
    llvm::MDNode *branchProb (int prob);

  // get the branch-weight meta data for overflow-trap branches
    llvm::MDNode *overflowWeights ();

  // get intinsics; these are cached for the current module
    llvm::Function *sadd32WOvflw ()
    {
	if (this->_sadd32WO == nullptr) {
	    this->_sadd32WO =
		_getIntrinsic (llvm::Intrinsic::sadd_with_overflow, this->i32Ty);
	}
	return this->_sadd32WO;
    }
    llvm::Function *ssub32WOvflw ()
    {
	if (this->_ssub32WO == nullptr) {
	    this->_ssub32WO =
		_getIntrinsic (llvm::Intrinsic::ssub_with_overflow, this->i32Ty);
	}
	return this->_ssub32WO;
    }
    llvm::Function *smul32WOvflw ()
    {
	if (this->_smul32WO == nullptr) {
	    this->_smul32WO =
		_getIntrinsic (llvm::Intrinsic::smul_with_overflow, this->i32Ty);
	}
	return this->_smul32WO;
    }
    llvm::Function *sadd64WOvflw ()
    {
	if (this->_sadd64WO == nullptr) {
	    this->_sadd64WO =
		_getIntrinsic (llvm::Intrinsic::sadd_with_overflow, this->i64Ty);
	}
	return this->_sadd64WO;
    }
    llvm::Function *ssub64WOvflw ()
    {
	if (this->_ssub64WO == nullptr) {
	    this->_ssub64WO =
		_getIntrinsic (llvm::Intrinsic::ssub_with_overflow, this->i64Ty);
	}
	return this->_ssub64WO;
    }
    llvm::Function *smul64WOvflw ()
    {
	if (this->_smul64WO == nullptr) {
	    this->_smul64WO =
		_getIntrinsic (llvm::Intrinsic::smul_with_overflow, this->i64Ty);
	}
	return this->_smul64WO;
    }
    llvm::Function *fabs32 ()
    {
	if (this->_fabs32 == nullptr) {
	    this->_fabs32 = _getIntrinsic (llvm::Intrinsic::fabs, this->i32Ty);
	}
	return this->_fabs32;
    }
    llvm::Function *fabs64 ()
    {
	if (this->_fabs64 == nullptr) {
	    this->_fabs64 = _getIntrinsic (llvm::Intrinsic::fabs, this->i64Ty);
	}
	return this->_fabs64;
    }
    llvm::Function *sqrt32 ()
    {
	if (this->_sqrt32 == nullptr) {
	    this->_sqrt32 = _getIntrinsic (llvm::Intrinsic::sqrt, this->i32Ty);
	}
	return this->_sqrt32;
    }
    llvm::Function *sqrt64 ()
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

  /***** Debugging support *****/

  // dump the current module to stderr
    void dump () const;

  // run the LLVM verifier on the module
    bool verify () const;

  private:
    struct target_info const	*_target;
    llvm::LLVMContext		_context;
    llvm::IRBuilder<>		_builder;
    llvm::Module		*_module;
    llvm::legacy::FunctionPassManager *_passMngr;
    llvm::Function		*_curFn;	// current LLVM function
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
    llvm::Function *_sadd32WO;		// @llvm.sadd.with.overflow.i32
    llvm::Function *_ssub32WO;		// @llvm.ssub.with.overflow.i32
    llvm::Function *_smul32WO;		// @llvm.smul.with.overflow.i32
    llvm::Function *_sadd64WO;		// @llvm.sadd.with.overflow.i64
    llvm::Function *_ssub64WO;		// @llvm.ssub.with.overflow.i64
    llvm::Function *_smul64WO;		// @llvm.smul.with.overflow.i64
    llvm::Function *_fabs32;		// @llvm.fabs.f32
    llvm::Function *_fabs64;		// @llvm.fabs.f64
    llvm::Function *_sqrt32;		// @llvm.sqrt.f32
    llvm::Function *_sqrt64;		// @llvm.sqrt.f64

  // helper function for getting an intrinsic when it has not yet
  // been loaded for the current module.
  //
    llvm::Function *_getIntrinsic (llvm::Intrinsic::ID id, Type *ty)
    {
	return llvm::Intrinsic::getDeclaration (
	    this->_module, id, llvm::ArrayRef<Type *>(ty));
    }

  // constructor
    code_buffer (struct target_info const *target);

};

#endif // !__CODE_BUFFER_HXX__

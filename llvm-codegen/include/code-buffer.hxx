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

#include "lambda-var.hxx"

template <typename T>
using lvar_map_t = std::unordered_map<LambdaVar::lvar,T>;

struct target_info;

// the SML Machine "registers"
enum class ml_regs {
    ALLOC_PTR = 0,
    LIMIT_PTR,
    STD_ARG,
    EXN_HNDLR,			// exception handler
    VAR_PTR,			// var_ptr register
};

// cached state of SML registers; this is used to save/restore the state when
// processing the arms of a `BRANCH` or `SELECT`.
struct reg_state;

class code_buffer {
  public:
    code_buffer (target_info const &info);

  // initialize the code buffer for a new module
    void initModule (std::string &src);

    llvm::IRBuilder<> build () { return this->_builder; }

  // get the LLVM value that represents the specified SML register
    llvm::Value *mlReg (ml_regs r) const;

  // assign a value to an SML register
    void setMLReg (ml_regs r, llvm::Value *v);

  // save the current state of the SML registers
    reg_state *saveMLRegState ();

  // restore the state of the SML registers
    void restoreMLRegState (reg_state *);

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

  // target parameters
    int wordSzInBytes () { return this->_wordSzB; }
    bool is64Bit () { return (this->_wordSzB == 8); }

  // cached types
    llvm::IntegerType *i8Ty;
    llvm::IntegerType *i16Ty;
    llvm::IntegerType *i32Ty;
    llvm::IntegerType *i64Ty;
    llvm::Type *f32Ty;
    llvm::Type *f64Ty;
    llvm::IntegerType *intTy;	// native integer type
    llvm::Type *mlRefTy;	// type of pointers to mutable data (i.e., refs and arrays)
    llvm::Type *mlPtrTy;	// type of pointers to immutable data

    llvm::IntegerType *iType (int sz)
    {
	if (sz == 64) return this->i64Ty;
	else if (sz == 32) return this->i32Ty;
	else if (sz == 16) return this->i16Ty;
	else return this->i8Ty;
    }
    llvm::Type *fType (int sz)
    {
	if (sz == 64) return this->f64Ty;
	else return this->f32Ty;
    }

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

  // lvar to value map operations
    llvm::Value *lookupVal (LambdaVar::lvar lv)
    {
	lvar_map_t<llvm::Value *>::const_iterator got = this->_vMap.find(lv);
	if (got == this->_vMap.end()) {
	    return nullptr;
	} else {
	    return got->second;
	}
    }

  // insert a binding into the lvar-to-value map
    void insertVal (LambdaVar::lvar lv, llvm::Value *v)
    {
	std::pair<LambdaVar::lvar,llvm::Value *> pair(lv, v);
	this->_vMap.insert (pair);
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

  // return the basic-block that contains the Overflow trap generator
    llvm::BasicBlock *getOverflowBB ();

  // return branch-weight meta data, where `prob` represents the probability of
  // the true branch and is in the range 1..999.
    llvm::MDNode *branchProb (int prob);

  // get the branch-weight meta data for overflow-trap branches
    llvm::MDNode *overflowWeights ();

  private:
    llvm::LLVMContext		_context;
    llvm::IRBuilder<>		_builder;
    llvm::Module		*_module;
    llvm::Function		*_curFn;	// current LLVM function
    lvar_map_t<llvm::BasicBlock *> _bbMap;	// map from labels to basic blocks
    lvar_map_t<llvm::Value *>	_vMap;		// map from lvars to values

  // target-machine properties
    int64_t _wordSzB;

  // cached intrinsic functions for arithmetic with overflow detection
    llvm::Function *_sadd32WO;
    llvm::Function *_ssub32WO;
    llvm::Function *_smul32WO;
    llvm::Function *_sadd64WO;
    llvm::Function *_ssub64WO;
    llvm::Function *_smul64WO;

  // helper function for getting an intrinsic when it has not yet
  // been loaded for the current module.
  //
    llvm::Function *_getIntrinsic (llvm::Intrinsic::ID id, llvm::Type *ty)
    {
	return llvm::Intrinsic::getDeclaration (
	    this->_module, id, llvm::ArrayRef<llvm::Type *>(ty));
    }

};

#endif // !__CODE_BUFFER_HXX__

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

class code_buffer {
  public:
    code_buffer (target_info const &info);

  // initialize the code buffer for a new module
    void initModule (std::string &src);

    llvm::IRBuilder<> build () { return this->_builder; }

  // LLVM values that represent the standard registers; note that these get
  // redefined for each fragment
    llvm::Value *allocPtr () const;	// FIXME: should be inline
    llvm::Value *limitPtr () const;	// FIXME: should be inline

/** NOTE: we may be able to avoid needing the signed constants */
  // signed integer constant of specified bit size
    llvm::Value *iConst (int sz, int64_t c)
    {
	if (sz == 8) return llvm::ConstantInt::getSigned(this->i8Ty, c);
	else if (sz == 16) return llvm::ConstantInt::getSigned(this->i16Ty, c);
	else if (sz == 32) return llvm::ConstantInt::getSigned(this->i32Ty, c);
	else return llvm::ConstantInt::getSigned(this->i64Ty, c);
    }
  // signed constant of native size
    llvm::Value *iConst (int64_t c)
    {
	return llvm::ConstantInt::getSigned (this->intTy, c);
    }

  // unsigned integer constant of specified bit size
    llvm::Value *uConst (int sz, uint64_t c)
    {
	if (sz == 8) return llvm::ConstantInt::get(this->i8Ty, c);
	else if (sz == 16) return llvm::ConstantInt::get(this->i16Ty, c);
	else if (sz == 32) return llvm::ConstantInt::get(this->i32Ty, c);
	else return llvm::ConstantInt::get(this->i64Ty, c);
    }
  // unsigned constant of native size
    llvm::Value *uConst (uint64_t c)
    {
	return llvm::ConstantInt::get (this->intTy, c);
    }

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

    llvm::Value *wordSzInBytes ()
    {
	return this->uConst (this->_wordSzB);
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

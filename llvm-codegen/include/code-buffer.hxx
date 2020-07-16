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

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"

class code_buffer {
  public:
    code_buffer ();

  // initialize the code buffer for a new module
    void initModule (std::string &src);

    llvm::IRBuilder<> build () { return this->_builder; }

    llvm::Value *IConst (int sz, int64_t c)
    {
	if (sz == 8) return llvm::ConstantInt::getSigned(this->i8Ty, c);
	else if (sz == 16) return llvm::ConstantInt::getSigned(this->i16Ty, c);
	else if (sz == 32) return llvm::ConstantInt::getSigned(this->i32Ty, c);
	else return llvm::ConstantInt::getSigned(this->i64Ty, c);
    }
    llvm::Value *UConst (int sz, uint64_t c)
    {
	if (sz == 8) return llvm::ConstantInt::get(this->i8Ty, c);
	else if (sz == 16) return llvm::ConstantInt::get(this->i16Ty, c);
	else if (sz == 32) return llvm::ConstantInt::get(this->i32Ty, c);
	else return llvm::ConstantInt::get(this->i64Ty, c);
    }

  // get intinsics; these are cached for the current module
    llvm::Function *sadd32WOvflw ()
    {
	if (this->_sadd32WO == nilptr) {
	    this->_sadd32WO =
		_getIntrinsic (llvm::Intrinsic::sadd_with_overflow, this->i32Ty);
	}
	return this->_sadd32WO;
    }
    llvm::Function *ssub32WOvflw ()
    {
	if (this->_ssub32WO == nilptr) {
	    this->_ssub32WO =
		_getIntrinsic (llvm::Intrinsic::ssub_with_overflow, this->i32Ty);
	}
	return this->_ssub32WO;
    }
    llvm::Function *smul32WOvflw ()
    {
	if (this->_smul32WO == nilptr) {
	    this->_smul32WO =
		_getIntrinsic (llvm::Intrinsic::smul_with_overflow, this->i32Ty);
	}
	return this->_smul32WO;
    }
    llvm::Function *sadd64WOvflw ()
    {
	if (this->_sadd64WO == nilptr) {
	    this->_sadd64WO =
		_getIntrinsic (llvm::Intrinsic::sadd_with_overflow, this->i64Ty);
	}
	return this->_sadd64WO;
    }
    llvm::Function *ssub64WOvflw ()
    {
	if (this->_ssub64WO == nilptr) {
	    this->_ssub64WO =
		_getIntrinsic (llvm::Intrinsic::ssub_with_overflow, this->i64Ty);
	}
	return this->_ssub64WO;
    }
    llvm::Function *smul64WOvflw ()
    {
	if (this->_smul64WO == nilptr) {
	    this->_smul64WO =
		_getIntrinsic (llvm::Intrinsic::smul_with_overflow, this->i64Ty);
	}
	return this->_smul64WO;
    }

  // cached types
    const llvm::IntegerType *i8Ty;
    const llvm::IntegerType *i16Ty;
    const llvm::IntegerType *i32Ty;
    const llvm::IntegerType *i64Ty;
    const llvm::Type *f32Ty;
    const llvm::Type *f64Ty;

  private:
    llvm::LLVMContext		_context;
    llvm::IRBuilder<>		_builder;
    std::unique_ptr<llvm::Module> *_module;

  // cached intrinsic functions for arithmetic with overflow detection
    llvm::Function *_sadd32WO;
    llvm::Function *_ssub32WO;
    llvm::Function *_smul32WO;
    llvm::Function *_sad64dWO;
    llvm::Function *_ssub64WO;
    llvm::Function *_smul64WO;

    void llvm::Function *_getIntrinsic (llvm::Intrinsic::ID id, llvm::Type *ty);
};

#endif // !__CODE_BUFFER_HXX__

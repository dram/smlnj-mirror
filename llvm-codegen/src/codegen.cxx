/// \file codegen.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief This file holds the main code generator code.
///
/// \author John Reppy
///

#include "cfg.hxx"

/***** class code_buffer member functions *****/

code_buffer::code_buffer ()
  : _context(), _builder(this->_context), _module(nullptr)
{

  // initialize the standard types that we use
    this->i8Ty = llvm::IntegerType::get (this->_context, 8);
    this->i16Ty = llvm::IntegerType::get (this->_context, 16);
    this->i32Ty = llvm::IntegerType::get (this->_context, 32);
    this->i64Ty = llvm::IntegerType::get (this->_context, 64);
    this->f32Ty = llvm::Type::getPrimitiveType (this->_context, llvm::Type::FloatTyID);
    this->f64Ty = llvm::Type::getPrimitiveType (this->_context, llvm::Type::DoubleTyID);

} // constructor

void code_buffer::initModule (std::string &src)
{
    this->_module = new Module(src, this->_context);

  // clear the cached intrinsic functions
    this->_sadd32WO = nullptr;
    this->_ssub32WO = nullptr;
    this->_smul32WO = nullptr;
    this->_sad64dWO = nullptr;
    this->_ssub64WO = nullptr;
    this->_smul64WO = nullptr;

} // initModule

// helper function for getting an intrinsic when it has not yet
// been loaded for this module.
//
void llvm::Function *code_buffer::_getIntrinsic (llvm::Intrinsic::ID id, llvm::Type *ty)
{
    return llvm::Intrinsic::getDeclaration (this->_module, id, llvm::ArrayRef(ty));
}

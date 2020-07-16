/*! test-metadata.cxx
 *
 * Test LLVM metadata construction.
 *
 * To compile:
 *
 *	clang++ -std=c++17 -o test-metadata -I../llvm/include -L../llvm/lib test-metadata.cxx -lLLVMCore
 */

#include <string>
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Metadata.h"


int main ()
{
    llvm::LLVMContext cxt;
    llvm::Type *i32Ty = llvm::IntegerType::get (cxt, 32);

    auto name = llvm::MDString::get(cxt, "branch_weights");
    auto trueProb = llvm::ValueAsMetadata::get(llvm::ConstantInt::get(i32Ty, 42, false));
    auto falseProb = llvm::ValueAsMetadata::get(llvm::ConstantInt::get(i32Ty, 17, false));
    auto tpl = llvm::MDTuple::get(cxt, {name, trueProb, falseProb});

    tpl->dump();

    return 0;
}

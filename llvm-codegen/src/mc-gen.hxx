/// \file mc-gen.hxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Wrapper class for the low-level machine-specific parts of the code generator
///
/// \author John Reppy
///

#ifdef _MC_GEN_HXX_
#define _MC_GEN_HXX_

#include <memory>

#include "llvm/Target/TargetMachine.h"
#include "llvm/IR/LegacyPassManager.h"

class mc_gen {
  public:

    mc_gen (llvm::Context &context, target_info const *target);

  // dump the current module to stderr
    void dump () const
    {
#ifndef RELEASE_BUILD
	this->_module->dump();
#endif
    }

  // run the LLVM verifier on the module
    bool verify () const { return llvm::verifyModule (*this->_module, &llvm::dbgs()); }

  // per-module initialization and finalization
    void beginModule (std::string const & src);
    void endModule ();

  // run the per-function optimizations over the functions of the module
    void optimize ();

  // dump the code to an output file
    void dumpCode (std::string const & stem, bool asmCode = true) const;

  private:
    std::unique_ptr<llvm::Module> _module;
    std::unique_ptr<llvm::TargetMachine> _tgtMachine;
    std::unique_ptr<llvm::legacy::FunctionPassManager> _passMngr;

};

#endif // !_MC_GEN_HXX_

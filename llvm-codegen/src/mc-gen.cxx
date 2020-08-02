/// \file mc-gen.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Wrapper for the low-level machine-specific parts of the code generator
///
/// \author John Reppy
///

#include "mc-gen.hxx"

#include "llvm/Support/TargetRegistry.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Support/FileSystem.h"

#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"

mc_gen::mc_gen (llvm::Context &context, target_info const *target)
{
  // set up the target machine builder
    llvm::Triple triple("x86_64-apple-macosx10.15.0");
//    triple.setArch (target->arch);
    llvm::orc::JITTargetMachineBuilder tgtBuilder(triple);
    tgtBuilder.setRelocationModel (llvm::Reloc::PIC_);
//    tgtBuilder.setCodeGenOptLevel (llvm::CodeGenOpt::Less);
    llvm::TargetOptions tgtOpts;
    tgtOpts.GuaranteedTailCallOpt = true;
    tgtBuilder.setOptions (tgtOpts);

    auto tgtMachine = tgtBuilder.createTargetMachine();
    if (!tgtMachine) {
	assert(false);
    }
    this->_tgtMachine = std::move(*tgtMachine);

} // mc_gen constructor

void mc_gen::beginModule (std::string const & src)
{
    this->_module = std::make_unique<llvm::Module> (src, this->_context);

  // tell the module about the target machine
    this->_module->setTargetTriple(this->_tgtMachine->getTargetTriple().getTriple());
    this->_module->setDataLayout(this->_tgtMachine->createDataLayout());

  // setup the pass manager
    this->_passMngr = new llvm::legacy::FunctionPassManager (this->_module);

  // Do simple "peephole" optimizations and bit-twiddling optzns.
    this->_passMngr->add(llvm::createInstructionCombiningPass());
  // Reassociate expressions.
    this->_passMngr->add(llvm::createReassociatePass());
  // Eliminate Common SubExpressions.
    this->_passMngr->add(llvm::createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
    this->_passMngr->add(llvm::createCFGSimplificationPass());

    this->_passMngr->doInitialization();

} // mc_gen::beginModule

void mc_gen::endModule ()
{
    delete this->_passMngr->reset();
    delete this->_module->reset();
}

void mc_gen::optimize ()
{
  // run the function optimizations over every function
    for (auto it = this->_module->begin();  it != this->_module->end();  ++it) {
	this->_passMngr->run (*it);
    }

}

void dumpCode (std::string const & stem, bool asmCode) const
{
    std::string outFile;
    if (stem != "-") {
	outFile = stem + (asmCode ? ".s" : ".o");
    }
    else if (! asmCode) {
	outFile = "out.o";
    }

    std::error_code EC;
    llvm::raw_fd_ostream outStrm(outFile, EC, llvm::sys::fs::OF_None);
    if (EC) {
	llvm::errs() << "unable to open output file '" << outFile << "'\n";
	return;
    }

    llvm::legacy::PassManager pass;
    auto outKind = (asmCode ? llvm::CGFT_AssemblyFile : llvm::CGFT_ObjectFile);
    if (this->_tgtMachine->addPassesToEmitFile(pass, outStrm, nullptr, outKind)) {
	llvm::errs() << "unable to add pass to generate '" << outFile << "'\n";
	return;
    }

    pass.run(*this->_module);

    outStrm.flush();

}

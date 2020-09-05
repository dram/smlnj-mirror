/// \file mc-gen.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Wrapper for the low-level machine-specific parts of the code generator
///
/// \author John Reppy
///

#include "target-info.hxx"
#include "mc-gen.hxx"

#include "llvm/Support/TargetRegistry.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/SmallVectorMemoryBuffer.h"
#include "llvm/Object/ObjectFile.h"

#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"

/* define DEBUG_MC to enable printing of the IR after every internal LLVM step.
 * Warning: this produces 1000's of lines of output for even the smallest
 * example!!!
 */
//#define DEBUG_MC
#ifdef DEBUG_MC
#include "llvm/InitializePasses.h"
#include "llvm/Support/CommandLine.h"
#endif

#include <iostream>


mc_gen::mc_gen (llvm::LLVMContext &context, target_info const *info)
{
    std::string triple = "x86_64-apple-macosx10.15.0";

  // lookup the target in the registry
    std::string errMsg;
    auto *target = llvm::TargetRegistry::lookupTarget(triple, errMsg);
    if (target == nullptr) {
	std::cerr << "**** Fatal error: unable to find target\n";
        assert(false);
    }

llvm::dbgs() << "host CPU = " << llvm::sys::getHostCPUName() << "\n";

    llvm::TargetOptions tgtOptions;

  // make sure that tail calls are optimized
  /* It turns out that setting the GuaranteedTailCallOpt flag to true causes
   * a bug with non-tail JWA calls (the bug is a bogus stack adjustment after
   * the call).  Fortunately, our tail calls get properly optimized even
   * without that flag being set.
   */
//    tgtOpts.GuaranteedTailCallOpt = true;

// see include/llvm/Support/*Parser.def for the various CPS and feature names
// that are recognized
    std::unique_ptr<llvm::TargetMachine> tgtMachine(target->createTargetMachine(
	triple,
	"generic",		/* CPU name */
	"",			/* features string */
	tgtOptions,
	llvm::Reloc::PIC_,
	llvm::None,
	llvm::CodeGenOpt::Less));

    if (!tgtMachine) {
	std::cerr << "**** Fatal error: unable to create target machine\n";
        assert(false);
    }

    this->_tgtMachine = std::move(tgtMachine);

} // mc_gen constructor

void mc_gen::beginModule (llvm::Module *module)
{
  // tell the module about the target machine
    module->setTargetTriple(this->_tgtMachine->getTargetTriple().getTriple());
    module->setDataLayout(this->_tgtMachine->createDataLayout());

  // setup the pass manager
    this->_passMngr = std::make_unique<llvm::legacy::FunctionPassManager> (module);

  // setup analysis passes
    this->_passMngr->add(
	llvm::createTargetTransformInfoWrapperPass(
	    this->_tgtMachine->getTargetIRAnalysis()));
/* FIXME: are there other analysis passes that we need? */

  // set up a optimization pipeline following the pattern used in the Manticore
  // compiler.
    this->_passMngr->add(llvm::createLowerExpectIntrinsicPass());       /* -lower-expect */
    this->_passMngr->add(llvm::createCFGSimplificationPass());          /* -simplifycfg */
    this->_passMngr->add(llvm::createInstructionCombiningPass());       /* -instcombine */
    this->_passMngr->add(llvm::createReassociatePass());                /* -reassociate */
    this->_passMngr->add(llvm::createConstantPropagationPass());        /* -constprop */
    this->_passMngr->add(llvm::createEarlyCSEPass());                   /* -early-cse */
    this->_passMngr->add(llvm::createGVNPass());                        /* -gvn */
//    this->_passMngr->add(llvm::createSCCPPass());			/* -sccp */
    this->_passMngr->add(llvm::createDeadCodeEliminationPass());        /* -dce */
    this->_passMngr->add(llvm::createCFGSimplificationPass());          /* -simplifycfg */
    this->_passMngr->add(llvm::createInstructionCombiningPass());       /* -instcombine */
    this->_passMngr->add(llvm::createCFGSimplificationPass());          /* -simplifycfg */

    this->_passMngr->doInitialization();

} // mc_gen::beginModule

void mc_gen::endModule ()
{
    this->_passMngr.reset();
}

void mc_gen::optimize (llvm::Module *module)
{
  // run the function optimizations over every function
    for (auto it = module->begin();  it != module->end();  ++it) {
        this->_passMngr->run (*it);
    }

}

// adopted from SimpleCompiler::operator() (CompileUtils.cpp)
//
//llvm::Expected<std::unique_ptr<llvm::ObjectFile>>
bool mc_gen::compile (llvm::Module *module)
{
    llvm::SmallVector<char, 0> objBufferSV;
    {
	llvm::raw_svector_ostream objStrm(objBufferSV);
	llvm::legacy::PassManager pass;
	llvm::MCContext *ctx;
	if (this->_tgtMachine->addPassesToEmitMC(pass, ctx, objStrm)) {
	    llvm::errs() << "unable to add pass to generate code\n";
	    return true;
	}
	pass.run (*module);
    }

    auto objBuffer = std::make_unique<llvm::SmallVectorMemoryBuffer>(
	std::move(objBufferSV), module->getModuleIdentifier() + "-objectbuffer");

  // convert the memory buffer to an object file
    auto obj = llvm::object::ObjectFile::createObjectFile(objBuffer->getMemBufferRef());

    if (! obj) {
	llvm::errs() << "unable to get object file\n";
	return true;
    }

/* TODO: eventually, we should change the return type of this function to
 *
 *	llvm::Expected<llvm::StringRef>
 *
 * and just return the contents of the text segment.  We might want to verify
 * that the entry function is at the beginning of the section.
 */

  // print info about the sections
    llvm::dbgs() << "=== Sections ===\n";
    for (auto sect : (*obj)->sections()) {
	auto name = sect.getName();
	auto addr = sect.getAddress();
	auto sz = sect.getSize();
	if (name) {
	    llvm::dbgs() << "  " << *name;
	} else {
	    llvm::dbgs() << "  <section>";
	}
	if (sect.isText()) llvm::dbgs() << " [TEXT] ";
	else if (sect.isData()) llvm::dbgs() << " [DATA] ";
	llvm::dbgs() << " " << (void *)addr << ".." << (void *)(addr+sz) << "\n";
    }

  // print the symbols
    llvm::dbgs() << "=== Symbols ===\n";
    for (auto sym : (*obj)->symbols()) {
	auto name = sym.getName();
	auto addr = sym.getAddress();
	if (name && addr) {
	    llvm::dbgs() << "  " << *name << " @ " << (void *)*addr << "\n";
	}
    }

    return false;

}

void mc_gen::dumpCode (llvm::Module *module, std::string const & stem, bool asmCode) const
{
    std::string outFile;
    if (stem != "-") {
        outFile = stem + (asmCode ? ".s" : ".o");
    }
    else if (! asmCode) {
        outFile = "out.o";
    }
    else {
        outFile = stem;
    }

#ifdef DEBUG_MC
    llvm::PassRegistry *Registry = llvm::PassRegistry::getPassRegistry();
    llvm::initializeCore(*Registry);
    llvm::initializeCodeGen(*Registry);
    llvm::initializeLoopStrengthReducePass(*Registry);
    llvm::initializeLowerIntrinsicsPass(*Registry);
    llvm::initializeEntryExitInstrumenterPass(*Registry);
    llvm::initializePostInlineEntryExitInstrumenterPass(*Registry);
    llvm::initializeUnreachableBlockElimLegacyPassPass(*Registry);
    llvm::initializeConstantHoistingLegacyPassPass(*Registry);
    llvm::initializeScalarOpts(*Registry);
    llvm::initializeVectorization(*Registry);
    llvm::initializeScalarizeMaskedMemIntrinPass(*Registry);
    llvm::initializeExpandReductionsPass(*Registry);
    llvm::initializeHardwareLoopsPass(*Registry);

    // Initialize debugging passes.
    llvm::initializeScavengerTestPass(*Registry);

    // Register the target printer for --version.
    llvm::cl::AddExtraVersionPrinter(
	llvm::TargetRegistry::printRegisteredTargetsForVersion);

    const char *argv[] = {
	"codegen",
	"--print-after-all"
      };
    llvm::cl::ParseCommandLineOptions(2, argv, "codegen\n");
#endif

//    LLVMTargetMachine &LLVMTM = static_cast<LLVMTargetMachine &>(*Target);
//    MachineModuleInfoWrapperPass *MMIWP = new MachineModuleInfoWrapperPass(&LLVMTM);

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

    pass.run(*module);

    outStrm.flush();

}

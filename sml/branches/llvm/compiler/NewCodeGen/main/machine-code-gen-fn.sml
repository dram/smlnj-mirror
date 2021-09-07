(* machine-code-gen-fn.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Final machine code generation.  This module wraps up the post-FLINT phases
 * as a pass that takes the program in FLINT IR and produces the machine
 * code and literal data objects.
 *)

functor MachineCodeGenFn (

    structure MachSpec : MACH_SPEC

  (* used by CM to distinguish between different ABIs on Unix systems *)
    val abi_variant : string option

  ) : CODE_GENERATOR = struct

    structure CPSGen = CPSCompFn (MachSpec)

    fun phase x = Stats.doPhase (Stats.makePhase x)

    fun compile {source, prog} = let
	(* CPS compilation *)
	  val {clusters, maxAlloc, data} = CPSGen.compile {source=source, prog=prog}
        (* convert to CFG IR *)
          val cfg = CPSGen.toCFG {
                  source = source, clusters = clusters, maxAlloc = maxAlloc
                }
        (* pickle the IR into a vector *)
          val pkl = CFGPickler.toBytes cfg
        (* invoke the LLVM code generator to generate machine code *)
          val code = CodeObj.generate {
                  target = MachSpec.llvmTargetName,
                  src = source,
                  pkl = pkl
                }
	  in
	    {code = code, data = data}
	  end

  (* the following are used by CM *)
    val architecture = MachSpec.architecture
    val abi_variant = abi_variant

  end

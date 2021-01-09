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

    structure Gen : MACHINE_GEN

  (* `collect getEP` returns a code object constructed from the current contents
   * of the code string (see `compler/CodeGen/main/code-string.sml`).  The function
   * `getEP` returns the offset of the entry point in the code.
   *)
    val collect : (unit -> int) -> CodeObj.code_object

  ) : CODE_GENERATOR = struct

    structure MachSpec = Gen.MachSpec
    structure CPSGen = CPSCompFn (MachSpec)

    structure Machine = Gen

    val architecture = Gen.MachSpec.architecture
    val abi_variant = Gen.abi_variant

    fun phase x = Stats.doPhase (Stats.makePhase x)

    val codegen   = phase "CPS 120 cpsgen" Gen.codegen

    fun compile {source, prog} = let
	(* CPS compilation *)
	  val {clusters, maxAlloc, data} = CPSGen.compile {source=source, prog=prog}
	  val code = if !Control.CG.useLLVM
		then let
		(* convert to CFG IR *)
		  val cfg = CPSGen.toCFG {
			  source = source, clusters = clusters, maxAlloc = maxAlloc
			}
		(* pickle the IR into a vector *)
		  val pkl = CFGPickler.toBytes cfg
		  in
		  (* invoke the LLVM code generator *)
		    CodeObj.generate (source, pkl)
		  end
		else let
		(* MLRISC code generation *)
		  val getEP = codegen {
			  clusters = clusters, maxAlloc = maxAlloc, source = source
			}
		  in
		    collect getEP
		  end
	  in
	    {code = code, data = data}
	  end

  end

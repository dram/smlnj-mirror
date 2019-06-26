(* cps-comp.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor CPSCompFn (

    structure Gen : MACHINE_GEN
    val collect : (unit -> int) -> CodeObj.code_object

  ) : CODE_GENERATOR = struct

    structure MachSpec = Gen.MachSpec
    structure Convert = Convert(MachSpec)
    structure CPStrans = CPStrans(MachSpec)
    structure CPSopt = CPSopt(MachSpec)
    structure Closure = Closure(MachSpec)
    structure Spill = SpillFn(MachSpec)
    structure CpsSplit = CpsSplitFun (MachSpec)

    structure Machine = Gen

    val architecture = Gen.MachSpec.architecture
    val abi_variant = Gen.abi_variant

    fun bug s = ErrorMsg.impossible ("CPSComp:" ^ s)
    val say = Control_Print.say

    fun phase x = Stats.doPhase (Stats.makePhase x)

    val convert   = phase "CPS 060 convert" Convert.convert
    val cpstrans  = phase "CPS 065 cpstrans" CPStrans.cpstrans
    val cpsopt    = phase "CPS 070 cpsopt" CPSopt.reduce
    val litsplit  = phase "CPS 075 litsplit" Literals.split
    val closure   = phase "CPS 080 closure"  Closure.closeCPS
    val globalfix = phase "CPS 090 globalfix" GlobalFix.globalfix
    val spill     = phase "CPS 100 spill" Spill.spill
    val limit     = phase "CPS 110 limit" Limit.nolimit
    val codegen   = phase "CPS 120 cpsgen" Gen.codegen

  (** pretty printing for the CPS code *)
    fun prC s e = if !Control.CG.printit
	  then (
	    say (concat["\n[After ", s, " ...]\n\n"]);
	    PPCps.printcps0 e;
	    say "\n"; e)
	  else e

    fun compile {source, prog} = let
	(* finish up with CPS *)
	  val (nc0, ncn, dseg) = let
		val function = convert prog
		val _ = prC "convert" function
		val function = (prC "cpstrans" o cpstrans) function
		val function = cpsopt (function, NONE, false)
		val _ = prC "cpsopt" function
	      (* split out heap-allocated literals; litProg is the bytecode *)
		val (function, litProg) = litsplit function
		val _ = prC "cpsopt-code" function
		fun gen fx = let
		      val fx = (prC "closure" o closure) fx
		      val carg = globalfix fx
		      val carg = spill carg
		      val (carg, limit) = limit carg
		      val epthunk = codegen {
			      funcs = carg, limits = limit, source = source
			    }
		      in
			collect epthunk
		      end
	        in
		  case CpsSplit.cpsSplit function
		   of (fun0 :: funn) => (gen fun0, List.map gen funn, litProg)
		    | [] => bug "unexpected case on gen in CPSComp.compile"
		  (* end case *)
	        end
        in
	  {c0=nc0, cn=ncn, data=dseg}
        end (* function compile *)

  end (* CPSComp *)

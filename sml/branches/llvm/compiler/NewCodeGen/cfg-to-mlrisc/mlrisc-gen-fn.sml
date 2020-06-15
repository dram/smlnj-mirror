(* mlrisc-gen-fn.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Translate CPS to MLRISC.  This version removes various optimizations
 * that were not enabled.
 *)

signature MLRISC_GEN =
  sig

  (* The result is a thunk around the address of the resulting code
   * object's entry point.  The client must promise to first call
   * "finish" before forcing it.
   *)
    val codegen : string * CFG.cluster list -> (unit -> int)

  end

functor MLRiscGenFn (

    structure MachineSpec: MACH_SPEC
    structure Ext : SMLNJ_MLTREE_EXT
    structure Regs : CPSREGS
	where T.Region = CPSRegions
	  and T.Constant = SMLNJConstant
	  and T.Extension = Ext
    structure ClientPseudoOps : SMLNJ_PSEUDO_OPS
    structure PseudoOp   : PSEUDO_OPS
	where T = Regs.T
	  and Client = ClientPseudoOps
    structure MLTreeComp : MLTREECOMP
	where TS.T = Regs.T
	  and TS.S.P = PseudoOp
    structure MLTreeUtils : MLTREE_UTILS
	where T = Regs.T
    structure Flowgen : CONTROL_FLOWGRAPH_GEN
	where S = MLTreeComp.TS.S
	  and I = MLTreeComp.I
	  and CFG = MLTreeComp.CFG

  ) : MLRISC_GEN = struct

    structure M  = Regs.T		(* MLTree *)
    structure E  = Ext			(* Extensions *)
    structure C  = CFG
    structure P  = CFG_Prim		(* CFG primitive operators *)
    structure R  = CFGRegions		(* Regions *)
    structure CG = Control.CG		(* Compiler Control *)
    structure MS = MachineSpec		(* Machine Specification *)
    structure D  = MS.ObjDesc		(* ML Object Descriptors *)
    structure TS = MLTreeComp.TS	(* MLTREE streams *)
    structure CPs = ClientPseudoOps
    structure PB = PseudoOpsBasisTyp
    structure An = MLRiscAnnotations
    structure CB = CellsBasis
    structure Tbl = LambdaVar.Tbl

  (* Argument passing *)
    structure ArgP = ArgPassing(
	structure Cells = Cells
	structure C = Regs
	structure MS = MachineSpec)

  (* C-Calls handling *)
    structure CPSCCalls = CPSCCalls(
	structure MS = MachineSpec
	structure C = Regs
	structure MLTreeComp = MLTreeComp
	structure Cells = Cells
	structure CCalls = CCalls)

  (* lift selected CPS datatypes *)
    datatype cty = datatype C.cty
    datatype value = datatype C.value
    datatype accesspath = datatype C.accesspath

    fun error msg = MLRiscErrorMsg.error("MLRiscGen", msg)

    fun sameRegAs x y = CB.sameCell (x, y)

  (*
   * The main codegen function.
   *)
    fun codegen (source, clusters) = let
	(*
	 * Function for generating code for one cluster.
	 *)
	fun genCluster cluster = let
	      val _ = if !Control.debugging then app PPCps.printcps0 cluster else ()
	    (* The mltree stream *)
	      val stream as TS.S.STREAM{
		      beginCluster,  (* start a cluster *)
		      endCluster,    (* end a cluster *)
		      emit,          (* emit MLTREE stm *)
		      defineLabel,   (* define a local label *)
		      entryLabel,    (* define an external entry *)
		      exitBlock,     (* mark the end of a procedure *)
		      pseudoOp,      (* emit a pseudo op *)
		      annotation,    (* add an annotation *)
		      ...
		    } = MLTreeComp.selectInstructions (Flowgen.build ())
	   (* if the annotation flag is enabled, then wrap statements with their
	    * string representation.
	    *)
	      val emit = if !annoteFlg
		    then fn stm => emit (M.ANNOTATION(
		      stm,
		      #create MLRiscAnnotations.COMMENT (MLTreeUtils.stmToString stm)))
		    else emit
	   (* If there are raw C Calls (i.e., RCC is present), then we need to
	    * use the virtual frame pointer
	    *)
	      local
		fun hasRCC [] = false
		  | hasRCC ((_,_,_,_,cexp)::rest) =
		      CPSUtil.hasRCC cexp orelse hasRCC rest
	      in
	      val vfp = not MS.framePtrNeverVirtual andalso hasRCC cluster
	      val _ = ClusterAnnotation.useVfp := vfp
	      end
	    (* This is the GC comparison test used.  We have a choice of signed
	     * and unsigned comparisons.  This usually doesn't matter, but some
	     * architectures work better in one way or the other, so we are given
	     * a choice here.   For example, the Alpha has to do extra for unsigned
	     * tests, so on the Alpha we use signed tests.
	     *)
	      val gcTest = if Regs.signedGCTest
		    then M.CMP(pty, M.GT, Regs.allocptr, Regs.limitptr vfp)
		    else M.CMP(pty, M.GTU, Regs.allocptr, Regs.limitptr vfp)
	    (* memDisambiguation uses the new register counters,
	     * so this must be reset here.
	     *)
	      val _ = Cells.reset()
	    (* This keeps track of all the advanced offset on the hp
	     * since the beginning of the CPS function.
	     * This is important for generating the correct address offset
	     * for newly allocated records.
	     *)
	      val advancedHP = ref 0
	    (* Function grabty lookups the CPS type of a value expression in CPS.
	     *)
	      fun grabty (C.VAR v) = typmap v
		| grabty (C.LABEL v) = typmap v
		| grabty (C.NUM{ty, ...}) = C.NUMt ty
		| grabty (C.VOID) = C.FLTt 64 (* why? *)
		| grabty _ = CPSUtil.BOGt
	    (* The baseptr contains the start address of the entire
	     * compilation unit.  This function generates the address of
	     * a label that is embedded in the same compilation unit.  The
	     * generated address is relative to the baseptr.
	     *
	     * Note: For GC safety, we considered this to be an object reference
	     *)
	      fun laddr (lab, k) =
		    M.ADD(addrTy, Regs.baseptr vfp,
		      M.LABEXP(M.ADD(addrTy, M.LABEL lab,
			LI'(k - MachineSpec.constBaseRegOffset))))
	   (* Generate code for a statement *)
	      fun gen (C.LET(e, x, k), hp) =
		| gen (C.ALLOC(alloc, args, k), hp) =
		| gen (C.APP(f, args), hp) =
		| gen (C.SWITCH(arg, ks), hp) =
		| gen (C.BRANCH(test, args, k1, k2), hp) =
		| gen (C.ARITH(rator, args, x, k), hp) =
		| gen (C.SETTER(rator, args, k), hp) =
		| gen (C.RCC(kind, f, proto, args, params, k), hp) =
	      and genExp (C.VAR x) =
		| genExp (C.LABEL l) =
		| genExp (C.NUMP{iv, signed, sz}) =
		| genExp (C.LOOKER(rator, args)) =
		| genExp (C.PURE(rator, args)) =
		| genExp (C.SELECT(i, arg)) =
		| genExp (C.OFFSET(i, arg)) =
	      in
	      end

  end (* functor MLRiscGenFn *)

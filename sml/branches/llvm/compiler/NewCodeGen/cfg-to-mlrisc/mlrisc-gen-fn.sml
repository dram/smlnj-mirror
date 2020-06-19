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

functor NewMLRiscGenFn (

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
    structure InvokeGC : INVOKE_GC
	where TS = MLTreeComp.TS
	  and CFG = Flowgen.CFG
    structure Cells : CELLS
    structure CCalls : C_CALLS
	where T = Regs.T

    val compile : Flowgen.CFG.cfg -> unit

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

  (*
   * If this flag is on then split the entry block.
   * This should be on for SSA optimizations.
   *)
    val splitEntry = Control.MLRISC.mkFlag ("split-entry-block", "whether to split entry block")

  (* if this flag is on, then annotate instructions with their source MLTree *)
    val annoteFlg = Control.MLRISC.mkFlag ("annotate-instrs", "whether to annotate instructions")

  (* convert bit size to byte size; bit sizes should always be a power of 2 *)
    fun byteSz sz = (Word.toIntX(Word.>>(Word.fromInt sz, 0w3)))

  (* convert bit size to log2 byte size *)
    fun logByteSz 8 = 0
      | logByteSz 16 = 1
      | logByteSz 32 = 2
      | logByteSz 64 = 3
      | logByteSz _ = raise Match

  (* Various bit-width specifications, which serve as MLRisc types *)
(* QUESTION: do we care about the redundancy between Target.mlValueSz and MS.wordBitWidth? *)
    val ity = MS.wordBitWidth		(* word size in bits *)
    val ws = MS.wordByteWidth		(* word size in bytes *)
    val logWS = logByteSz ws		(* log2 of word size in bytes *)
    val addrTy = MS.addressBitWidth	(* naturalsize of address arithmetic *)
    val wordsPerDbl = 8 div ws		(* number of words for a Real.real *)

    val zero = M.LI 0
    val one  = M.LI 1
    fun LI' i = M.LI (M.I.fromInt(ity, i))

    fun error msg = MLRiscErrorMsg.error("MLRiscGen", String.concat msg)

  (* `alignOffset (offset, align)` returns `offset` rounded up to the next
   * multiple of `align` (which must be a power of 2).
   *)
    fun alignOffset (offset, align) = let
	  val w = Word.fromInt align - 0w1
	  in
	    Word.toIntX(Word.andb(Word.fromInt offset + w, Word.notb w))
	  end

  (* store a at the offset from the allocation pointer *)
    fun allocWord (hp, v) = M.STORE(ity, M.ADD(ity, Regs.allocptr, LI' hp), v, R.memory)

  (* Assign `v` to the CPS-Machine register, which may be implemented as a stack
   * location (e.g., the store pointer, exception handler, etc.).
   *)
    fun assign (M.REG(ty,r), v) = M.MV(ty, r, v)
      | assign (M.LOAD(ty, ea, mem), v) = M.STORE(ty, ea, v, mem)
      | assign _ = error ["assign"]

  (* expression to compute an base+offset address, where offset is a constant *)
    fun ea (adr, 0) = adr
      | ea (adr, offset) = M.ADD(ity, adr, LI' offset)

    fun addrAdd (adr, M.LI 0) = adr
      | addrAdd (adr, offset) = M.ADD(ity, adr, offset)

  (* `indexAddr (adr, sz, ix)` returns an expression to compute
   * the address adr + scale*ix, where scale is the sz/8.
   *)
    fun indexAddr (adr, _, M.LI 0) = adr
      | indexAddr (adr, 8, ix) = M.ADD(ity, adr, ix)
      | indexAddr (adr, sz, ix) = M.ADD(ity, adr, M.SLL(ity, ix, LI'(logByteSz sz)))

    fun scaleOffset (adr, _, 0) = adr
      | scaleOffset (adr, scale, offset) = M.ADD(ity, adr, LI'(scale * offset))

  (* return code to compute `adr + ws * idx` *)
    fun scaleWord (adr, M.LI 0) = adr
      | scaleWord (adr, idx) = M.ADD(ity, adr, M.SLL(ity, idx, LI' logWS))

  (* convert unsigned CPS comparison to MLRISC *)
    fun unsignedCmp oper = (case oper
	   of P.GT => M.GTU
	    | P.GTE => M.GEU
	    | P.LT => M.LTU
	    | P.LTE => M.LEU
	    | P.EQL => M.EQ
	    | P.NEQ => M.NE
	  (* end case *))

  (* convert signed CPS comparison to MLRISC *)
    fun signedCmp oper = (case oper
	   of P.GT => M.GT
	    | P.GTE  => M.GE
	    | P.LT => M.LT
	    | P.LTE  => M.LE
	    | P.NEQ => M.NE
	    | P.EQL => M.EQ
	  (* end case *))

  (* convert a floating-point comparison to MLRISC *)
    fun floatCmp oper = (case oper
	   of P.F_EQ => M.==
	    | P.F_ULG => M.?<>
	    | P.F_UN => M.?
	    | P.F_LEG => M.<=>
	    | P.F_GT => M.>
	    | P.F_GE  => M.>=
	    | P.F_UGT => M.?>
	    | P.F_UGE => M.?>=
	    | P.F_LT => M.<
	    | P.F_LE  => M.<=
	    | P.F_ULT => M.?<
	    | P.F_ULE => M.?<=
	    | P.F_LG => M.<>
	    | P.F_UE  => M.?=
	  (* end case *))

    val newLabel = Label.anon

  (* branch annotated with probability *)
    fun branchWithProb (br, 0) = br
      | branchWithProb (br, p) =
	  M.ANNOTATION(br,
	    #create MLRiscAnnotations.BRANCH_PROB(Probability.prob(p, 100)))
  (*
   * The allocation pointer.  This must be a register
   *)
    val M.REG(_, allocptrR) = Regs.allocptr

  (* The GC comparison test.  We have a choice of signed and unsigned
   * comparisons.  This usually doesn't matter, but some architectures
   * work better in one way or the other, so we are given a choice here.
   * For example, the Alpha has to do extra for unsigned tests, so on
   * the Alpha we use signed tests.
   *)
    fun gcTest vfp = if Regs.signedGCTest
	  then M.CMP(ity, M.GT, Regs.allocptr, Regs.limitptr vfp)
	  else M.CMP(ity, M.GTU, Regs.allocptr, Regs.limitptr vfp)

  (*
   * The main codegen function.
   *)
    fun codegen (source, clusters : CFG.cluster list) = let
	  val ((_, clusterEntry, _, _), _) = hd clusters
	  val splitEntry = !splitEntry
	(* A mapping of function names (CPS.lvars) to labels.
	 * If the flag splitEntry is on, we also distinguish between external and
	 * internal labels, make sure that no directly branches go to the
	 * external labels.
	 *)
	  exception LabelBind
	  val externLabelTbl : Label.label Tbl.hash_table = Tbl.mkTable(32, LabelBind)
	  val externLabel = Tbl.lookup externLabelTbl
	  val addExternLabel = Tbl.insert externLabelTbl
	  val localLabelTbl : Label.label Tbl.hash_table = Tbl.mkTable(32, LabelBind)
	  val localLabel = Tbl.lookup localLabelTbl
	  val addLocalLabel = Tbl.insert localLabelTbl
	(*
	 * Function for generating code for one cluster.
	 *)
	fun genCluster (cluster as (entry, frags)) = let
	      val _ = if !Control.debugging then PPCfg.prCluster cluster else ()
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
	    (* A mapping of lvars to MLRisc cells (aka, pseudo registers). *)
	      val gpRegTbl : M.rexp Tbl.hash_table = Tbl.mkTable(32, Fail "GP Tbl")
	      val addBinding = Tbl.insert gpRegTbl
	      fun bind (lv, ty, rexp) = let
		    val reg = Cells.newReg()
		    val reg' = M.REG(ty, reg)
		    in
		      emit (M.MV(ty, reg, rexp));
		      addBinding (lv, reg');
		      reg'
		    end
	      val lookup = Tbl.lookup gpRegTbl
	      val fpRegTbl : M.fexp Tbl.hash_table = Tbl.mkTable(16, Fail "FP Tbl")
	      val addFBinding = Tbl.insert fpRegTbl
	      fun fbind (lv, ty, fexp) = let
		    val reg = Cells.newReg()
		    in
		      emit (M.FMV(ty, reg, fexp));
		      addFBinding (lv, M.FREG(ty, reg))
		    end
	      val flookup = Tbl.lookup fpRegTbl
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
	      val vfp = not MS.framePtrNeverVirtual andalso CFGUtil.hasRCC cluster
	      val _ = ClusterAnnotation.useVfp := vfp
	    (* specialize for the vfp *)
	      val gcTest = gcTest vfp
	      val storePtr = Regs.storeptr vfp
	      val exnPtr = Regs.exnptr vfp
	      val varPtr = Regs.varptr vfp
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
	    (* `updateAllocPtr hp` emits code to advance the allocation pointer to
	     * by `hp`.  It also ensures that the
	     *)
	    (* Keep allocation pointer aligned on odd boundary.  This function must be
	     * called before any instruction that might raise an exception, since we want
	     * the allocation pointer to be correct.
	     * Note: We have accounted for the extra space this eats up in
	     *    limit.sml
	     *)
	      fun updateAllocPtr 0 = ()
		| updateAllocPtr hp = let
		    fun advBy hp = (
			  advancedHP := !advancedHP + hp;
			  emit(M.MV(ity, allocptrR, M.ADD(addrTy, Regs.allocptr, LI' hp))))
		    in
		      if Word.andb(Word.fromInt hp, Word.fromInt ws) <> 0w0
			then advBy(hp+ws)
			else advBy hp
		    end
	    (* `recordStore (adr, hp)` adds a store record to the store list, where
	     * `adr` is the address where a (possibly) boxed update has occurred.
	     *)
	      fun recordStore (adr, hp) = (
		    emit (M.STORE(ity, ea(Regs.allocptr, hp), adr, R.storelist));
		    emit (M.STORE(ity, ea(Regs.allocptr, hp+ws), storePtr, R.storelist));
		    emit (assign(storePtr, ea(Regs.allocptr, hp))))
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
	    (* generate code for a statement while preserving the value of advancedHP *)
	      fun genCont (stm, hp) = let
		    val save = !advancedHP
		    in
		      gen (stm, hp);
		      advancedHP := save
		    end
	    (* Generate code for a statement *)
	      and gen (C.LET(e, (x, CFG_Type.FLTt sz), k), hp) = (
		    fbind (x, sz, genFExp e);
		    gen (k, hp))
		| gen (C.LET(e, (x, CFG_Type.NUMt sz), k), hp) = (
		    bind (x, sz, genExp e);
		    gen (k, hp))
		| gen (C.LET(e, (x, _), k), hp) = (
		    bind (x, ity, genExp e);
		    gen (k, hp))
	      (** Allocation **)
		| gen (C.ALLOC(P.RECORD{desc, mut}, args, x, k), hp) = let
		  (* write the descriptor *)
		    val _ = emit (allocWord (hp, M.LI desc))
		  (* the object's offset from the allocation pointer *)
		    val obj = hp + ws
		  (* fill in the object's fields *)
		    val hp = List.foldl
			  (fn (arg, hp) => (emit (allocWord (hp, genExp arg)); hp + ws))
			    obj args
		    in
		      bind (x, ity, M.ADD(ity, Regs.allocptr, LI' obj));
		      gen (k, hp)
		    end
		| gen (C.ALLOC(P.RAW_RECORD{desc, kind, sz}, args, x, k), hp) = let
		    val nb = byteSz sz
		  (* align the heap offset (if necessary) *)
		    val hp = alignOffset(hp + ws, nb) - ws
		  (* write the descriptor *)
		    val _ = emit (allocWord (hp, M.LI desc))
		  (* the object's offset from the allocation pointer *)
		    val obj = hp + ws
		  (* fill in the object's fields *)
		    val store = if (kind = P.INT)
			  then fn (arg, hp) => (
			    M.STORE(sz,
			      M.ADD(ity, Regs.allocptr, LI' hp),
			      genExp arg,
			      R.memory);
			    hp + nb)
			  else fn (arg, hp) => (
			    M.FSTORE(sz,
			      M.ADD(ity, Regs.allocptr, LI' hp),
			      genFExp arg,
			      R.memory);
			    hp + nb)
		    val hp = List.foldl store obj args
		    in
		      bind (x, ity, M.ADD(ity, Regs.allocptr, LI' obj));
		      emit (allocWord (hp, M.LI desc));
		      gen (k, hp)
		    end
		| gen (C.ALLOC(P.RAW_ALLOC{desc=SOME d, align, len}, args, x, k), hp) = let
		    val hp = alignOffset(hp + ws, align) - ws
		    in
		      emit (allocWord (hp, M.LI d));
		      bind (x, ity, M.ADD(ity, Regs.allocptr, LI'(hp + ws)));
		      gen (k, hp + ws + len)
		    end
		| gen (C.ALLOC(P.RAW_ALLOC{desc, align, len}, args, x, k), hp) = let
		    val  hp = alignOffset(hp, align)
		    in
		      bind (x, ity, M.ADD(ity, Regs.allocptr, LI' hp));
		      gen (k, hp + len)
		    end
	      (** Function/Continuation Application **)
		| gen (C.APP(cc, f, args), hp) = ??
	      (** Control Flow **)
		| gen (C.SWITCH(arg, ks), hp) = let
		    val lab = newLabel ()
		    val labs = map (fn _ => newLabel()) ks
		    val tmpR = Cells.newReg()
		    val tmp = M.REG(ity, tmpR)
		    fun genCase (lab, stm) = (
			  defineLabel lab;
			  genCont (stm, hp))
		    in
		      emit (M.MV(ity, tmpR, laddr(lab, 0)));
		      emit (M.JMP(
			M.ADD(addrTy, tmp, M.LOAD(ity, scaleWord(tmp, genExp arg), R.readonly)),
			labs));
		      pseudoOp (PB.DATA_READ_ONLY);
		      pseudoOp (PB.EXT(CPs.JUMPTABLE{base=lab, targets=labs}));
		      pseudoOp PB.TEXT;
		      ListPair.app genCase (labs, ks)
		    end
		| gen (C.BRANCH(P.FSGN sz, [a], prob, kTrue, kFalse), hp) = let
		  (* testing the sign of a float is implemented by storing the float
		   * in the heap and then loading the word that contains the sign
		   * into an integer register and doing a bit test on the sign bit.
		   * This approach is portable, but there may be better ways on
		   * some hardware.
		   *)
		    val trueLab = newLabel ()
		  (* address of the word that contains the sign bit *)
		    val addr = if MachineSpec.bigEndian orelse (sz <= ity)
			    then M.ADD(addrTy, Regs.allocptr, LI' hp)
			    else M.ADD(ity, Regs.allocptr, LI'(hp + byteSz(sz - ity)))
		  (* the size of the data that we will load to do the bit test *)
		    val sz' = Int.min(sz, ity)
		    val cmp = M.CMP(sz', M.LT, M.LOAD(sz', addr, R.memory), zero)
		    in
		    (* store the float into the heap *)
		      emit (M.FSTORE(sz, ea(Regs.allocptr, hp), genFExp a, R.memory));
		      branchWithProb (M.BCC(cmp, trueLab), prob);
		      genCont (kFalse, hp);
		      defineLabel trueLab;
		      gen (kTrue, hp)
		    end
		| gen (C.BRANCH(test, args, prob, kTrue, kFalse), hp) = let
		    val trueLab = newLabel ()
		    in
		      branchWithProb (M.BCC(genCCExp (test, args), trueLab), prob);
		      genCont (kFalse, hp);
		      defineLabel trueLab;
		      gen (kTrue, hp)
		    end
	      (** Arithmetic (w/ Side Effects) **)
		| gen (C.ARITH(rator, args, x, k), hp) = ??
	      (** Updates **)
		| gen (C.SETTER(rator, args, k), hp) = let
		    fun store (sz, adr, v) =
			  emit (M.STORE(sz, adr, genExp v, R.memory))
		    fun fstore (sz, adr, v) =
			  emit (M.FSTORE(sz, adr, genFExp v, R.memory))
		    in
		      case (rator, args)
		       of (P.UNBOXED_UPDATE, [a, idx, v]) => (
			    store (ity, scaleWord(genExp a, genExp idx), v);
			    gen (k, hp))
			| (P.UPDATE, [a, idx, v]) => let
			    val adrReg = Cells.newReg()
			    val adr = M.REG(ity, adrReg)
			    in
			      emit (M.MV(ity, adrReg, scaleWord(genExp a, genExp idx)));
			      recordStore (adr, hp);
			      store (ity, adr, v);
			      gen (k, hp + 2*ws)
			    end
			| (P.UNBOXED_ASSIGN, [r, v]) => (
			    store(ity, genExp r, v);
			    gen(k, hp))
			| (P.ASSIGN, [r, v]) => let
			    val r' = regBind r
			    in
			      recordStore (r', hp);
			      store (ity, r', v);
			      gen (k, hp + 2*ws)
			    end
			| (P.RAW_UPDATE{kind=P.INT, sz}, [a, idx, v]) => (
			    store(sz, indexAddr(genExp a, sz, genExp idx), v);
			    gen(k, hp))
			| (P.RAW_UPDATE{kind=P.FLT, sz}, [a, idx, v]) => (
			    fstore(sz, indexAddr(genExp a, sz, genExp idx), v);
			    gen(k, hp))
			| (P.RAW_STORE{kind=P.INT, sz}, [a, idx, v]) => (
			    store(sz, addrAdd(genExp a, genExp idx), v);
			    gen(k, hp))
			| (P.RAW_STORE{kind=P.FLT, sz}, [a, idx, v]) => (
			    fstore(sz, addrAdd(genExp a, genExp idx), v);
			    gen(k, hp))
			| (P.SET_HDLR, [a]) => (assign (exnPtr, genExp a); gen(k, hp))
			| (P.SET_VAR, [a]) => (assign (varPtr, genExp a); gen(k, hp))
		      (* end case *)
		    end
	      (** C Calls **)
		| gen (C.RCC(kind, f, proto, args, params, k), hp) = ??
	    (* translate a CFG expression to a MLRisc register *)
	      and regBind (C.VAR x) = lookup x
		| regBind e = let
		    val reg = Cells.newReg()
		    in
		      emit (M.MV(ity, reg, genExp e));
		      M.REG(ity, reg)
		    end
	    (* translate a CFG expression to an MLRisc rexp value *)
	      and genExp (C.VAR x) = lookup x
		| genExp (C.LABEL l) = if splitEntry
		    then laddr(localLabel l, 0)
		    else laddr(externLabel l, 0)
		| genExp (C.NUM{iv, ...}) = M.LI iv
		| genExp (C.LOOKER(rator, args)) = (case (rator, List.map genExp args)
		     of (P.DEREF, [a]) =>
			  M.LOAD(ity, a, R.memory)
		      | (P.SUBSCRIPT, [adr, idx]) =>
			  M.LOAD(ity, scaleWord(adr, idx), R.memory)
		      | (P.RAW_SUBSCRIPT{kind=INT, sz}, [adr, idx]) =>
			  M.LOAD(sz, indexAddr(adr, sz, idx), R.memory)
		      | (P.GET_HDLR, _) => exnPtr
		      | (P.GET_VAR, _) => varPtr
		      | _ => error [
			    "unexpected ", PPCfg.lookerToString rator,
			    " in int expression"
			  ]
		    (* end case *))
		| genExp (C.PURE(rator, args)) = (case (rator, List.map genExp args)
		     of (P.PURE_ARITH{oper, sz}, [a, b]) => (case oper
			   of P.ADD => M.ADD(sz, a, b)
			    | P.SUB => M.SUB(sz, a, b)
			    | P.SMUL => M.MULU(sz, a, b)
			    | P.UMUL => M.MULS(sz, a, b)
			    | P.SDIV => M.DIVS(M.DIV_TO_ZERO, sz, a, b)
			    | P.SREM => M.REMS(M.DIV_TO_ZERO, sz, a, b)
			    | P.UDIV => M.DIVU(sz, a, b)
			    | P.UREM => M.REMU(sz, a, b)
			    | P.LSHIFT => M.SLL(sz, a, b)
			    | P.RSHIFT => M.SRA(sz, a, b)
			    | P.RSHIFTL => M.SRL(sz, a, b)
			    | P.ORB => M.ORB(sz, a, b)
			    | P.XORB => M.XORB(sz, a, b)
			    | P.ANDB => M.ANDB(sz, a, b)
			    | _ => error [
				  "unexpected ", PPCfg.pureopToString oper,
				  " in int expression"
				]
			  (* end case *))
		      | (P.EXTEND{signed=true, from, to}, [a]) => M.SX(to, from, a)
		      | (P.EXTEND{from, to, ...}, [a]) => M.ZX(to, from, a)
		      | (P.LOAD_RAW{offset, kind=INT, sz}, [adr]) =>
			  M.LOAD(sz, ea(adr, offset), R.memory)
		      | (P.PURE_SUBSCRIPT, [adr, idx]) =>
			  M.LOAD(ity, scaleWord(adr, idx), R.memory)
		      | (P.PURE_RAW_SUBSCRIPT{kind=INT, sz}, [adr, idx]) =>
			  M.LOAD(sz, indexAddr(adr, sz, idx), R.memory)
		      | _ => error [
			    "unexpected ", PPCfg.pureToString rator,
			    " in float expression"
			  ]
		    (* end case *))
		| genExp (C.SELECT(i, arg)) =
		    M.LOAD(ity, scaleOffset(genExp arg, ws, i), R.memory)
		| genExp (C.OFFSET(i, arg)) = scaleOffset(genExp arg, ws, i)
	    (* translate a CFG expression to an MLRisc fexp value *)
	      and genFExp (C.VAR x) = flookup x
		| genFExp (C.LABEL l) = error ["unexpected LABEL in float expression"]
		| genFExp (C.NUM{iv, signed, sz}) =
		    error ["unexpected NUM in float expression"]
		| genFExp (C.LOOKER(rator, args)) = (case (rator, List.map genExp args)
		     of (P.RAW_SUBSCRIPT{kind=FLT, sz}, [adr, idx]) =>
			  M.FLOAD(sz, indexAddr(adr, sz, idx), R.memory)
		      | _ => error [
			    "unexpected ", PPCfg.lookerToString rator,
			    " in int expression"
			  ]
		    (* end case *))
		| genFExp (C.PURE(rator, args)) = (case (rator, args)
		     of (P.PURE_ARITH{oper, sz}, [a, b]) => (case oper
			   of P.FADD => M.FADD(sz, genFExp a, genFExp b)
			    | P.FSUB => M.FSUB(sz, genFExp a, genFExp b)
			    | P.FMUL => M.FMUL(sz, genFExp a, genFExp b)
			    | P.FDIV => M.FDIV(sz, genFExp a, genFExp b)
			    | _ => error [
				  "unexpected ", PPCfg.pureopToString oper,
				  " in binary float expression"
				]
			  (* end case *))
		      | (P.PURE_ARITH{oper=P.FNEG, sz}, [a]) => M.FNEG(sz, genFExp a)
		      | (P.PURE_ARITH{oper=P.FABS, sz}, [a]) => M.FABS(sz, genFExp a)
		      | (P.PURE_ARITH{oper=P.FSQRT, sz}, [a]) => M.FSQRT(sz, genFExp a)
		      | (P.INT_TO_REAL{from, to}, [a]) => M.CVTI2F(to, from, genExp a)
		      | (P.LOAD_RAW{offset, kind=FLT, sz}, [adr]) =>
			  M.FLOAD(sz, ea(genExp adr, offset), R.real)
		      | (P.PURE_RAW_SUBSCRIPT{kind=FLT, sz}, [adr, idx]) =>
			  M.FLOAD(sz, indexAddr(genExp adr, sz, genExp idx), R.memory)
		      | _ => error [
			    "unexpected ", PPCfg.pureToString rator,
			    " in float expression"
			  ]
		    (* end case *))
		| genFExp (C.OFFSET(i, arg)) =
		    error ["unexpected OFFSET in float expression"]
	    (* translate a CFG branch primop to an MLRisc ccexp *)
	      and genCCExp (P.CMP{oper, signed=true, sz}, [a, b]) =
		    M.CMP(sz, signedCmp oper, genExp a, genExp b)
		| genCCExp (P.CMP{oper, sz, ...}, [a, b]) =
		    M.CMP(sz, unsignedCmp oper, genExp a, genExp b)
		| genCCExp (P.FCMP{oper, sz}, [a, b]) =
		    M.FCMP(sz, floatCmp oper, genFExp a, genFExp b)
		| genCCExp (P.BOXED, [a]) =
		    M.CMP(ity, M.EQ, M.ANDB(ity, genExp a, one), zero)
		| genCCExp (P.UNBOXED, [a]) =
		    M.CMP(ity, M.NE, M.ANDB(ity, genExp a, one), zero)
		| genCCExp (P.PEQL, [a, b]) = M.CMP(ity, M.EQ, genExp a, genExp b)
		| genCCExp (P.PNEQ, [a, b]) = M.CMP(ity, M.NE, genExp a, genExp b)
		| genCCExp (tst, _) = error [
		      "unexpected ", PPCfg.branchToString tst, " in condition"
		    ]
	      in
??
	      end (* genCluster *)
	  fun finishCompilationUnit file = let
		val stream = MLTreeComp.selectInstructions (Flowgen.build ())
		val TS.S.STREAM{beginCluster, pseudoOp, endCluster, ...} = stream
		in
		  Cells.reset();
		  ClusterAnnotation.useVfp := false;
		  beginCluster 0;
		  pseudoOp PB.TEXT;
		  InvokeGC.emitModuleGC stream;
		  pseudoOp PB.DATA_READ_ONLY;
		  pseudoOp (PB.EXT(CPs.FILENAME file));
		  compile (endCluster [#create An.NO_OPTIMIZATION ()])
		end (* finishCompilationUnit *)
	  in
	    List.app genCluster clusters;
	    finishCompilationUnit source;
	    fn () => Label.addrOf (externLabel clusterEntry)
	  end (* codegen *)

  end (* functor MLRiscGenFn *)

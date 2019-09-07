(* amd64CG.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * AMD64 specific backend.
 *)

functor AMD64CG (

    structure CCallParams : sig
	val frameAlign : int
	val returnSmallStructsInRegs : bool
      end

    val abi_variant: string option

  ) = MachineGen (
    structure I          = AMD64Instr
    structure C          = I.C
    structure F          = AMD64CFG
    structure R          = AMD64CpsRegs
    structure CG         = Control.CG

    structure MachSpec   = AMD64Spec
    val abi_variant      = abi_variant
    structure ClientPseudoOps = AMD64ClientPseudoOps
    structure PseudoOps  = AMD64PseudoOps
    structure Ext        = AMD64_SMLNJMLTreeExt(* amd64-specific *)
    structure CpsRegs    = AMD64CpsRegs
    structure InsnProps  = AMD64Props
    structure Asm        = AMD64AsmEmitter
    structure Shuffle    = AMD64Shuffle

    structure CCalls     = AMD64SVID_CCalls (structure T = AMD64MLTree)

    structure OmitFramePtr = AMD64OmitFramePointer(
	structure I = AMD64Instr
	structure CFG = AMD64CFG)

    val spill = CPSRegions.spill
    val stack = CPSRegions.stack

    fun error msg = MLRiscErrorMsg.error("AMD64CG",msg)

    val floats16ByteAligned = false

    structure MLTreeComp = AMD64Gen(
	structure I=AMD64Instr
	structure MLTreeUtils = MLTreeUtils (
	    structure T = AMD64MLTree
	    fun hashSext  _ _ = 0w0
	    fun hashRext  _ _ = 0w0
	    fun hashFext  _ _ = 0w0
	    fun hashCCext _ _ = 0w0

	    (* Equality extensions *)
	    fun eqSext  _ _ = false
	    fun eqRext  _ _ = false
	    fun eqFext  _ _ = false
	    fun eqCCext _ _ = false

	    (* Pretty printing extensions *)
	    fun showSext  _ _ = ""
	    fun showRext  _ _ = ""
	    fun showFext  _ _ = ""
	    fun showCCext _ _ = ""
	  )
	structure ExtensionComp = AMD64MLTreeExtComp (
	    structure I = AMD64Instr
	    structure T = AMD64MLTree
	    structure CFG = AMD64CFG
	    structure TS = AMD64MLTreeStream
	  )
	structure MLTreeStream = AMD64MLTreeStream
	val floats16ByteAligned = floats16ByteAligned
      (* the signBit and negateSignBit constants are stored in the stack; see
       * base/runtime/mach-dep/AMD64.prim.asm.
       *)
	fun signBit ty = AMD64MLTree.ADD(64,
		AMD64MLTree.LI 16,
		AMD64MLTree.REG(64, AMD64Cells.rsp))
	fun negateSignBit ty = AMD64MLTree.ADD(64,
		AMD64MLTree.LI 24,
		AMD64MLTree.REG(64, AMD64Cells.rsp))
      )

    structure Jumps = AMD64Jumps(
	structure Instr=AMD64Instr
	structure AsmEmitter=AMD64AsmEmitter
	structure Eval=AMD64MLTreeEval
	structure Shuffle=AMD64Shuffle
	structure MCEmitter=AMD64MCEmitter)

    structure BackPatch = BackPatch(
	structure Jumps=Jumps
	structure Emitter=AMD64MCEmitter
	structure Props=InsnProps
	structure CFG = AMD64CFG
	structure Asm=AMD64AsmEmitter
	structure CodeString=CodeString)

    structure RA = RISC_RA (
        structure I         = AMD64Instr
	structure CFG       = AMD64CFG
	structure InsnProps = InsnProps
	structure Rewrite   = AMD64Rewrite(AMD64Instr)
	structure SpillInstr= AMD64SpillInstr(
	    structure I = AMD64Instr
	    structure Props = InsnProps
	    val floats16ByteAligned = floats16ByteAligned)
	structure Asm       = AMD64AsmEmitter
	structure SpillHeur = ChaitinSpillHeur	(* ChowHennessySpillHeur? *)
	structure Spill     = RASpill(
	    structure InsnProps = InsnProps
	    structure Asm = AMD64AsmEmitter)

	val sp    = I.C.stackptrR
	val spill = CPSRegions.spill

	structure SpillTable = SpillTable(AMD64Spec)

	val architecture = AMD64Spec.architecture

	datatype spillOperandKind = SPILL_LOC | CONST_VAL
	type spill_info = unit

	fun beforeRA _ = SpillTable.spillInit()

	fun pure _ = false

      (* make copies *)
	structure Int =
          struct
	    val avail     = AMD64CpsRegs.availR
	    val dedicated = AMD64CpsRegs.dedicatedR

	    fun mkDisp loc = I.Immed(Int32.fromInt(SpillTable.getRegLoc loc))

	    fun spillLoc{info, an, cell, id} = {
		    opnd=I.Displace{base=sp, disp=mkDisp(RAGraph.FRAME id), mem=spill},
		    kind=SPILL_LOC
		  }

	    val mode = RACore.NO_OPTIMIZATION
          end

	structure Float =
          struct
	    val avail     = AMD64CpsRegs.availF
	    val dedicated = AMD64CpsRegs.dedicatedF

	    fun mkDisp loc = I.Immed(Int32.fromInt(SpillTable.getFregLoc loc))

	    fun spillLoc(S, an, loc) =
		  I.Displace{base=sp, disp=mkDisp(RAGraph.FRAME loc), mem=spill}

	    val mode = RACore.NO_OPTIMIZATION
          end
      ) (* RA *)

(* OLD RA

    fun base () = (* XXXX *)
          if !ClusterAnnotation.useVfp then AMD64CpsRegs.vfp else I.C.rsp

    structure RA = AMD64RegAlloc(
        structure I         = AMD64Instr
	structure CB	   = CellsBasis
	structure Props = InsnProps
	structure Asm       = AMD64AsmEmitter
	structure CFG       = AMD64CFG
	structure SpillHeur = ChowHennessySpillHeur
	structure Spill     = RASpill (
	    structure Asm = AMD64AsmEmitter
	    structure InsnProps = InsnProps)

	type spill_info = unit

	val toInt32 = Int32.fromInt
	fun cacheOffset r = I.Immed(toInt32(AMD64Runtime.vregStart +
				 Word.toIntX(Word.<<(Word.fromInt(r-8),0w2))))
	fun cacheFPOffset f = I.Immed(toInt32(AMD64Runtime.vFpStart +
				 Word.toIntX(Word.<<(Word.fromInt(f-40),0w3))))

	datatype ra_phase = SPILL_PROPAGATION | SPILL_COLORING
	datatype spill_operand_kind = SPILL_LOC | CONST_VAL

	structure Int =
	  struct
	    val avail     = R.availR
	    val dedicated = R.dedicatedR
	    val phases    = [SPILL_PROPAGATION,SPILL_COLORING]

	   (* We try to make unused memregs available for spilling
	    * This is necessary because of the stupid SML code generator
	    * doesn't keep track of which are being used.
	    *)
	    fun spillInit(RAGraph.GRAPH{nodes, ...}) = let
		  val lookup = IntHashTable.lookup nodes
		  fun find(r, free) = if r >= 10
		        then let (* note, %8 and %9 are reserved! *)
		          val free = (case lookup r
				 of RAGraph.NODE{uses=ref [], defs=ref [], ...} =>
				      cacheOffset r::free
			          | _ => free
				(* end case *))
			  in
			    find(r-1, free)
			  end
		        else free
 (* FIXME: this code doesn't seem correct
	       val free = find(31 (* AMD64Runtime.numVregs+8-1 *), [])
 *)
		  in  (*AMD64StackSpills.setAvailableOffsets free*) ()
		  end

	    val getRegLoc' = AMD64StackSpills.getRegLoc

	    fun spillLoc{info, an, cell, id} = {
	            opnd=I.Displace{base=base(), disp=getRegLoc' id, mem=spill},
		    kind=SPILL_LOC
		  }

	end

	structure Float =
       struct
          val avail     = R.availF
          val dedicated = R.dedicatedF
          val phases    = [SPILL_PROPAGATION]

          fun spillInit(RAGraph.GRAPH{nodes, ...}) = let
                val lookup = IntHashTable.lookup nodes
		fun find (r, free) = if r >= 32+8
		      then let
                        val free = (case lookup r
			       of RAGraph.NODE{uses=ref [], defs=ref [],...} =>
				    cacheFPOffset r::free
                                | _ => free
			      (* end case *))
                        in
			  find(r-1, free)
			end
                     else free
		val free = find(63, [])
		in
		  AMD64StackSpills.setAvailableFPOffsets free
                end

          fun spillLoc(S, an, loc) = I.Displace{
		  base=base(),
		  disp=AMD64StackSpills.getFregLoc loc, mem=spill
		}

      end

      val floats16ByteAligned = floats16ByteAligned
    ) (* AMD64RA *)
*)
  ) (* AMD64CG *)

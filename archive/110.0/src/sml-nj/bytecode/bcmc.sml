(* bcmc.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories
 *
 * Bytecode machine code emitter for SML/NJ bytecode interpreter.
 *
 *)

structure BCMC : BC_MCODER =
  struct
	
    val itow = Word.fromInt
    val wtoi = Word.toInt


  (* Bytecode machine opcodes.
   * NOTE: these must track the opcodes in src/runtime/bc.h
   *)
    val op_NOP		    = 0
    val op_LIMITCHK	    = 1			(* check the heap limit *)
    val op_LIMITCHK2	    = 2			(* check the heap limit (big alloc) *)
    val op_BEGIN	    = 3			(* start an allocation *)
    val op_ALLOC	    = (op_BEGIN+1)	  (* next-word := GPR1 *)
    val op_ALLOCimmed	    = (op_BEGIN+2)	  (* next-word := im24 *)
    val op_ALLOCaddr	    = (op_BEGIN+3)	  (* next-word := (pc + im24) *)
    val op_ALLOCpath	    = (op_BEGIN+4)
    val op_ALLOCpath0	    = (op_BEGIN+5)
    val op_END		    = (op_BEGIN+6)	(* complete an allocation *)
    val op_PAIR		    = (op_BEGIN+7)	(* pair allocation *)
    val op_PAIRimmedl	    = (op_BEGIN+8)
    val op_PAIRimmedr	    = (op_BEGIN+9)
    val op_ALLOCFRAME	    = (op_BEGIN+10)	(* allocate a quasi-stack frame *)
    val op_FREEFRAME	    = (op_BEGIN+11)	(* free a quasi-stack frame *)
    val op_MARKSTORE	    = 15		(* record a store into MEM[GPR1] *)
    val op_MARKSTOREindex   = (op_MARKSTORE+1)	(* record a store into *)
						(* MEM[GPR1+2*(GPR2-1)] *)
    val op_MARKSTOREoffset  = (op_MARKSTORE+2)	(* record a store into MEM[GPR1+im16] *)
    val op_IMMED32	    = 18		(* GPR1 := im32 *)
    val op_IMMED20	    = 19		(* GPR1 := im20 *)
    val op_CODEADDR	    = 20		(* GPR1 := (PC + im20) *)
    val op_MOVE		    = 21		(* GPR1 := GPR2 *)
    val op_LOAD		    = 22		(* GPR1 := MEM[GPR2] *)
    val op_LOADindex	    = (op_LOAD+1)	(* GPR1 := MEM[GPR2+2*(GPR3-1)] *)
    val op_LOADoffset	    = (op_LOAD+2)	(* GPR1 := MEM[GPR2+im16] *)
    val op_STORE	    = (op_LOAD+3)	(* MEM[GPR2] := GPR1 *)
    val op_STOREindex	    = (op_LOAD+4)	(* MEM[GPR2+2*(GPR3-1)] := GPR1 *)
    val op_STOREoffset	    = (op_LOAD+5)	(* MEM[GPR2+im16] := GPR1 *)
    val op_STOREimmed	    = (op_LOAD+6)	(* MEM[GPR1] := im20 *)
    val op_BLOAD	    = 29
    val op_BLOADindex	    = (op_BLOAD+1)
    val op_BLOADoffset	    = (op_BLOAD+2)
    val op_BSTORE	    = (op_BLOAD+3)
    val op_BSTOREindex	    = (op_BLOAD+4)
    val op_BSTOREoffset	    = (op_BLOAD+5)
    val op_BSTOREimmed	    = (op_BLOAD+6)	(* BMEM[GPR1] := im20 *)
    val op_ADD		    = 36		(* GPR1	:= (GPR2 + GPR3) *)
    val op_ADDimmed	    = (op_ADD+1)	(* GPR1 := (GPR2 + im16) *)
    val op_INCR		    = (op_ADD+2)	(* GPR1 := (GPR1 + im20) *)
    val op_ADDT		    = (op_ADD+3)	(* GPR1 := check(GPR2 + GPR3) *)
    val op_ADDTimmed	    = (op_ADD+4)	(* GPR1 := check(GPR2 + im16) *)
    val op_SUB		    = 41		(* GPR1 := (GPR2 - GPR3) *)
    val op_SUBimmed	    = (op_SUB+1)	(* GPR1 := (im16 - GPR2) *)
    val op_SUBT		    = (op_SUB+2)	(* GPR1 := check(GPR2 - GPR3) *)
    val op_SUBTimmed	    = (op_SUB+3)	(* GPR1 := check(im16 - GPR2) *)
    val op_MULT		    = 45		(* GPR1 := check(GPR1 * GPR2) *)
    val op_MULTimmed	    = (op_MULT+1)	(* GPR1 := check(GPR1 * im20) *)
    val op_DIVT		    = 47		(* GPR1 := check(GPR1 / GPR2) *)
    val op_DIVTimmed	    = (op_DIVT+1)	(* GPR1 := check(GPR1 / im20) *)
    val op_ORB		    = 49		(* GPR1 := (GPR2 | GPR3) *)
    val op_ORB1		    = (op_ORB+1)	(* GPR1 := (GPR2 | 1) *)
    val op_ORBimmed	    = (op_ORB+2)	(* GPR1 := (GPR2 | im16) *)
    val op_XORB		    = (op_ORB+3)	(* GPR1 := (GPR2 ^ GPR3) *)
    val op_XORBimmed	    = (op_ORB+4)	(* GPR1 := (GPR2 ^ im16) *)
    val op_ANDB		    = (op_ORB+5)	(* GPR1 := (GPR2 & GPR3) *)
    val op_ANDBimmed	    = (op_ORB+6)	(* GPR1 := (GPR2 & im16) *)
    val op_NOTB		    = (op_ORB+7)	(* GPR1 := ~GPR2 *)
    val op_ASHL		    = 57		(* GPR1 := (GPR2 << GPR3) *)
    val op_ASHL1	    = (op_ASHL+1)	(* GPR1 := (GPR2 << 1) *)
    val op_ASHLimmed	    = (op_ASHL+2)	(* GPR1 := (GPR2 << im16) *)
    val op_ASHR		    = 60		(* GPR1 := (GPR2 >> GPR3) *)
    val op_ASHR1	    = (op_ASHR+1)	(* GPR1 := (GPR2 >> 1) *)
    val op_ASHRimmed	    = (op_ASHR+2)	(* GPR1 := (GPR2 << im16) *)
    val op_FMOVE	    = 63		(* FPR1 := FPR2 *)
    val op_FLOAD	    = 64		(* FPR1 := MEM[GPR2] *)
    val op_FLOADindex	    = (op_FLOAD+1)	(* FPR1 := MEM[GPR2+4*(GPR3-1)] *)
    val op_FLOADoffset	    = (op_FLOAD+2)	(* FPR1 := MEM[GPR2+im16] *)
    val op_FCONST	    = (op_FLOAD+3)	(* FPR1 := MEM[PC+im20] *)
    val op_FSTORE	    = 68		(* MEM[GPR2] := FPR1 *)
    val op_FSTOREindex	    = (op_FSTORE+1)	(* MEM[GPR2+4*(GPR3-1)] := FPR1 *)
    val op_FSTOREoffset	    = (op_FSTORE+2)	(* MEM[GPR2+im16] := FPR1 *)
    val op_FALLOC	    = (op_FSTORE+3)	(* GPR2 := ALLOC(Real); *)
						(*   MEM[GPR2] := FPR1 *)
    val op_FADD		    = 72		(* FPR1 := (FPR2 + FPR3) *)
    val op_FSUB		    = (op_FADD+1)	(* FPR1 := (FPR2 - FPR3) *)
    val op_FMUL		    = (op_FADD+2)	(* FPR1 := (FPR2 * FPR3) *)
    val op_FDIV		    = (op_FADD+3)	(* FPR1 := (FPR2 / FPR3) *)
    val op_FNEG		    = (op_FADD+4)	(* FPR1 := ~FPR2 *)
    val op_FABS		    = (op_FADD+5)	(* FPR1 := abs(FPR2) *)
    val op_FLOAT	    = (op_FADD+6)	(* FPR1 := real(GPR2) *)
    val op_GETEXN	    = 79		(* GPR1 := exnPtr *)
    val op_PUTEXN	    = (op_GETEXN+1)	(* exnPtr := GPR1 *)
    val op_GETVAR	    = 81		(* GPR1 := varPtr *)
    val op_PUTVAR	    = (op_GETVAR+1)	(* varPtr := GPR1 *)
    val op_JMP		    = 83		(* PC := PC + im24 *)
    val op_JMPind	    = (op_JMP+1)	(* PC := GPR1 *)
    val op_JMPindex	    = (op_JMP+2)	(* PC := PC+GPR1+im20 *)
    val op_JMPBOXED	    = (op_JMP+3)	(* if boxed(GPR1) *)
						(*   then PC := PC + im20 *)
    val op_JMPUNBOXED	    = (op_JMP+4)	(* if !boxed(GPR1) *)
						(*   then PC := PC + im20 *)
    val op_JMPINRNG	    = (op_JMP+5)	(* if (0 <= GPR1 < GPR2) *)
						(*   then PC := PC + im16 *)
    val op_JMPOUTRNG	    = (op_JMP+6)	(* if !(0 <= GPR1 < GPR2) *)
						(*   then PC := PC + im16 *)
    val op_JMPLT	    = (op_JMP+7)	(* if (GPR1 < GPR2) *)
						(*   then PC := PC + im16 *)
    val op_JMPLE	    = (op_JMP+8)	(* if (GPR1 <= GPR2) *)
						(*   then PC := PC + im16 *)
    val op_JMPEQ	    = (op_JMP+9)	(* if (GPR1 == GPR2) *)
						(*   then PC := PC + im16 *)
    val op_JMPNE	    = (op_JMP+10)	(* if (GPR1 != GPR2) *)
						(*   then PC := PC + im16 *)
    val op_JMPGT	    = (op_JMP+11)	(* if (GPR1 > GPR2) *)
						(*   then PC := PC + im16 *)
    val op_JMPGE	    = (op_JMP+12)	(* if (GPR1 >= GPR2) *)
						(*   then PC := PC + im16 *)
    val op_FJMPLT	    = (op_JMP+13)	(* if (FPR1 < FPR2) *)
						(*   then PC := PC + im16 *)
    val op_FJMPLE	    = (op_JMP+14)	(* if (FPR1 <= FPR2) *)
						(*   then PC := PC + im16 *)
    val op_FJMPEQ	    = (op_JMP+15)	(* if (FPR1 == FPR2) *)
						(*   then PC := PC + im16 *)
    val op_FJMPNE	    = (op_JMP+16)	(* if (FPR1 != FPR2) *)
						(*   then PC := PC + im16 *)
    val op_FJMPGT	    = (op_JMP+17)	(* if (FPR1 > FPR2) *)
						(*   then PC := PC + im16 *)
    val op_FJMPGE	    = (op_JMP+18)	(* if (FPR1 >= FPR2) *)
						(*   then PC := PC + im16 *)

  (* Span dependent instructions *)
    structure Jumps =
      struct

	val chr = Char.chr

	local 
	  val f = chr o Word.toInt o Word.andb
	in
	fun byte0 i = f (itow i, 0wxff)
	fun byte1 i = f (Word.~>>(itow i, 0w8), 0wxff)
	fun byte2 i = f (Word.~>>(itow i, 0w16), 0wxff)
	fun byte3 i = f (Word.~>>(itow i, 0w24), 0wxff)
	end

	fun emitlong i = implode[byte3 i, byte2 i, byte1 i, byte0 i]

	datatype reg_t = GPR of int
	datatype fp_reg_t = FPR of int

      (*temporary register. This cannot be used for RangeChk or CondJmp. *)
	val tmpR = GPR 15

	datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR
	  		   | GEU | GTU | LTU | LEU
      (* these emitter functions return strings *)
	local
	  fun regPair (r1, r2) = 
	    chr (wtoi (Word.orb(Word.<<(itow r2, 0w4), itow r1)))
	in
	fun emit_RRR opcode (GPR r1, GPR r2, GPR r3) =
	      implode[#"\000", chr(r3), regPair(r1, r2), chr opcode]
	fun emit_RRI opcode (GPR r1, GPR r2, im) =
	      implode[byte1 im, byte0 im, regPair(r1, r2), chr opcode]
	fun emit_RIR opcode (r1, im, r2) = emit_RRI opcode (r1, r2, im)
	fun emit_RR opcode (GPR r1, GPR r2) =
	      implode[#"\000", #"\000", regPair(r1, r2), chr opcode]
	fun emit_RI opcode (GPR r, im) = let
	      val im = wtoi (Word.orb(Word.<<(itow im, 0w4), itow r))
	      in
		implode[byte2 im, byte1 im, byte0 im, chr opcode]
	      end
	fun emit_R opcode (GPR r) = implode[#"\000", #"\000", (chr r), chr opcode]
	fun emit_I opcode im = implode[byte2 im, byte1 im, byte0 im, chr opcode]
	fun emit_FFF opcode (FPR r1, FPR r2, FPR r3) =
	      implode[#"\000", chr(r3), regPair(r1, r2), chr opcode]
	fun emit_FRR opcode (FPR r1, GPR r2, GPR r3) =
	      implode[#"\000", chr(r3), regPair(r1, r2), chr opcode]
	fun emit_RRF opcode (GPR r1, GPR r2, FPR r3) =
	      implode[#"\000", chr(r3), regPair(r1, r2), chr opcode]
	fun emit_FFI opcode (FPR r1, FPR r2, im) =
	      implode[byte1 im, byte0 im, regPair(r1, r2), chr opcode]
	fun emit_FRI opcode (FPR r1, GPR r2, im) =
	      implode[byte1 im, byte0 im, regPair(r1, r2), chr opcode]
	fun emit_RIF opcode (GPR r1, im, FPR r2) =
	      implode[byte1 im, byte0 im, regPair(r1, r2), chr opcode]
	fun emit_FF opcode (FPR r1, FPR r2) =
	      implode[#"\000", #"\000", regPair(r1, r2), chr opcode]
	fun emit_FR opcode (FPR r1, GPR r2) =
	      implode[#"\000", #"\000", regPair(r1, r2), chr opcode]
	fun emit_FI opcode (FPR r, im) = let
	      val im = wtoi (Word.orb(Word.<<(itow im, 0w4), itow r))
	      in
		implode[byte2 im, byte1 im, byte0 im, chr opcode]
	      end
	fun emit_RF opcode (GPR r1, FPR r2) =
	      implode[#"\000", #"\000", regPair(r1, r2), chr opcode]
	end (* local *)

	datatype JumpKind
	  = Label of int
	  | Jump
	  | JumpIndex of reg_t
	  | JumpUnboxed of reg_t
	  | RangeChk of (reg_t * reg_t)
	  | CondJmp of (condition * reg_t * reg_t)
	  | FCondJmp of (condition * fp_reg_t * fp_reg_t)
	  | FConst of fp_reg_t
	  | CodeAddr of (reg_t * int)
	  | AllocAddr of int

	fun sizeCondJmp offset =
	      if ((~32768 <= offset) andalso (offset < 32768))
		then 4
		else 8  (* JMPcc +8; JMP dst *)
	fun isImmed20 offset = ((~524288 <= offset) andalso (offset < 524288))
	fun isImmed24 offset = ((~8388608 <= offset) andalso (offset < 8388608))

	exception TooBig
	fun sizejump (instr, _, src, dst) = let
	      val offset = src - dst
	      in
		case instr
		 of (Label _) => 4
		  | Jump => if isImmed24 offset then 4 else raise TooBig
		  | (JumpIndex _) =>
		      if isImmed20 offset then 4 else raise TooBig
		  | (JumpUnboxed _) =>
		      if isImmed20 offset
			then 4
		      else if isImmed24 offset
			then 8 (* JMPBOXED +8; JMP dst *)
			else raise TooBig
		  | (RangeChk _) => sizeCondJmp  offset
		  | (CondJmp _) => sizeCondJmp offset
		  | (FCondJmp _) => sizeCondJmp offset
		  | (FConst _) => if isImmed20 offset then 4 else raise TooBig
		  | (CodeAddr(_, im)) =>
		      if isImmed20(offset+im) then 4 else raise TooBig
		  | (AllocAddr im) =>
		      if isImmed24(offset+im) then 4 else raise TooBig
		(* end case *)
	      end

	fun emitjump (instr, sz, src, dst) : string = let
	      val offset = (dst - src)
	    (* Opcode offset from op_*LT opcode. *)
	      fun cond LSS = 0 | cond LEQ = 1
		| cond EQL = 2 | cond NEQ = 3
		| cond GTR = 4 | cond GEQ = 5
	      fun notCond LSS = GEQ | notCond LEQ = GTR
		| notCond EQL = NEQ | notCond NEQ = EQL
		| notCond GTR = LEQ | notCond GEQ = LSS
	      fun condJmp (cc, r1, r2, off) =
		    emit_RRI (op_JMPLT + (cond cc)) (r1, r2, off)
	      fun fcondJmp (cc, r1, r2, off) =
		    emit_FFI (op_FJMPLT + (cond cc)) (r1, r2, off)
	      val emit_JMP = emit_I op_JMP
	      in
		case (instr, sz)
		 of (Label im, 4) => emitlong (offset + im)
		  | (Jump, 4) => emit_JMP offset
		  | (JumpIndex r, 4) => emit_RI op_JMPindex (r, offset)
		  | (JumpUnboxed r, 4) => emit_RI op_JMPUNBOXED (r, offset)
		  | (JumpUnboxed r, 8) => (
		      (emit_RI op_JMPBOXED (r, 8)) ^ (emit_JMP (offset-4)))
		  | (RangeChk(r1, r2), 4) => emit_RRI op_JMPOUTRNG (r1, r2, offset)
		  | (RangeChk(r1, r2), 8) => (
		      (emit_RRI op_JMPINRNG (r1, r2, 8)) ^ (emit_JMP (offset-4)))
		  | (CondJmp(cc, r1, r2), 4) => condJmp (cc, r1, r2, offset) 
		  | (CondJmp(cc, r1, r2), 8) => (
		      (condJmp (notCond cc, r1, r2, 8)) ^ (emit_JMP (offset-4)))
		  | (FCondJmp(cc, fpr1, fpr2), 4) => fcondJmp (cc, fpr1, fpr2, offset)
		  | (FCondJmp(cc, fpr1, fpr2), 8) =>
		      ((fcondJmp (notCond cc, fpr1, fpr2, 8)) ^ (emit_JMP (offset-4)))
		  | (FConst fpr, 4) => emit_FI op_FCONST (fpr, offset)
		  | (CodeAddr(r, im), 4) => emit_RI op_CODEADDR (r, offset+im)
		  | (AllocAddr im, 4) => emit_I op_ALLOCaddr (offset+im)
		  | _ => ErrorMsg.impossible "[BCMC.emitjump]"
		(* end case *)
	      end

      end (* Jumps *)

    structure Emitter : BACKPATCH = Backpatch(Jumps)

    structure Coder : BC_CODER =
      struct

	type label_t = Emitter.Label

	open Jumps

      (* these emitter functions actually emit the code, returning unit *)
	local
	  fun emitFn f opcode arg = Emitter.emitstring(f opcode arg)
	in
	val emitlong = fn x => Emitter.emitstring (emitlong x)
	fun emitOpcode opcode =
	      Emitter.emitstring("\000\000\000" ^ String.str(Char.chr opcode))
	val emit_RRR = emitFn emit_RRR
	val emit_RRI = emitFn emit_RRI
	val emit_RIR = emitFn emit_RIR
	val emit_RR = emitFn emit_RR
	val emit_RI = emitFn emit_RI
	val emit_R = emitFn emit_R
	val emit_I = emitFn emit_I
	val emit_FFF = emitFn emit_FFF
	val emit_FRR = emitFn emit_FRR
	val emit_RRF = emitFn emit_RRF
	val emit_FRI = emitFn emit_FRI
	val emit_RIF = emitFn emit_RIF
	val emit_FF = emitFn emit_FF
	val emit_FR = emitFn emit_FR
	val emit_FI = emitFn emit_FI
	val emit_RF = emitFn emit_RF
	end (* local *)

	val align = Emitter.align
	val mark = Emitter.mark

	exception BadReal = IEEEReal.BadReal
	fun realconst s = Emitter.emitstring(IEEEReal.realconst s)
	val emitstring = Emitter.emitstring

	val newLabel = Emitter.newlabel
	val defineLabel	= Emitter.define
	fun emitLabel (n, lab) = Emitter.jump(Label n, lab)

      (* Psuedo instructions.  These are span dependent. *)
	fun emitJump lab = Emitter.jump(Jump, lab)
	fun emitJumpIndex (lab, r) = Emitter.jump(JumpIndex r, lab)
	fun emitJumpUnboxed (r, lab) = Emitter.jump(JumpUnboxed r, lab)
	fun emitRangeChk (r1, r2, lab) = Emitter.jump(RangeChk(r1, r2), lab)
	fun emitCondJmp (cc, r1, r2, lab) = Emitter.jump(CondJmp(cc, r1, r2), lab)
	fun emitFCondJmp (cc, fpr1, fpr2, lab) =
	      Emitter.jump(FCondJmp(cc, fpr1, fpr2), lab)
	fun emitFConst (fpr, lab) = Emitter.jump(FConst fpr, lab)
	fun emitCodeAddr (r, lab, im) = Emitter.jump(CodeAddr(r, im), lab)
	fun emitAllocAddr (lab, im) = Emitter.jump(AllocAddr im, lab)

	fun emit_IMMED32 (r, im) = (emit_R op_IMMED32 r; emitlong im)
	val emit_IMMED20 = emit_RI op_IMMED20
	val emit_MOVE = emit_RR op_MOVE
	val emit_FMOVE = emit_FF op_FMOVE
	val emit_GETEXN = emit_R op_GETEXN
	val emit_PUTEXN = emit_R op_PUTEXN
	val emit_GETVAR = emit_R op_GETVAR
	val emit_PUTVAR = emit_R op_PUTVAR
	fun emit_LIMITCHK (nWords, mask) =
	      emit_I op_LIMITCHK (
	        wtoi (Word.orb(Word.<<(itow nWords, 0w16), 
			       itow mask)))
	fun emit_LIMITCHK2 (nWords, mask) = (emit_I op_LIMITCHK2 mask; emitlong nWords)
	val emit_JMPind = emit_R op_JMPind		
	val emit_BEGIN = emit_I op_BEGIN
	val emit_ALLOC = emit_R op_ALLOC
	local
	  fun emitPath [] = ()
	    | emitPath (p::r) = (emitlong p; emitPath r)
	in
	fun emit_ALLOCpath (base, path) = (
	      emit_RI op_ALLOCpath (base, length path);
	      emitPath path)
	fun emit_ALLOCpath0 (base, path) = (
	      emit_RI op_ALLOCpath0 (base, length path);
	      emitPath path)
	end
	val emit_ALLOCimmed = emit_I op_ALLOCimmed
	val emit_END = emit_R op_END
	val emit_PAIR = emit_RRR op_PAIR
	val emit_PAIRimmedl = emit_RIR op_PAIRimmedl
	val emit_PAIRimmedr = emit_RRI op_PAIRimmedr
	val emit_ALLOCFRAME = emit_RI op_ALLOCFRAME
	val emit_FREEFRAME = emit_RI op_FREEFRAME
      (* NOTE: the GPR2 (GPR3 for MARKSTOREindex) field is used to store the
       * alwaysBoxed flag in a MARKSTORE* instruction.
       *)
	fun emit_MARKSTORE (r, alwaysBoxed) =
	      emit_RR op_MARKSTORE (r, GPR(if alwaysBoxed then 1 else 0))
	fun emit_MARKSTOREindex (r1, r2, alwaysBoxed) =
	      emit_RRR op_MARKSTOREindex (r1, r2, GPR(if alwaysBoxed then 1 else 0))
	fun emit_MARKSTOREoffset (r, offset, alwaysBoxed) =
	      emit_RRI op_MARKSTOREoffset (r, GPR(if alwaysBoxed then 1 else 0), offset)
	val emit_LOAD = emit_RR op_LOAD
	val emit_LOADindex = emit_RRR op_LOADindex
	val emit_LOADoffset = emit_RRI op_LOADoffset
	val emit_STORE = emit_RR op_STORE
	val emit_STOREindex = emit_RRR op_STOREindex
	val emit_STOREoffset = emit_RIR op_STOREoffset
	val emit_STOREimmed = emit_RI op_STOREimmed
	val emit_BLOAD = emit_RR op_BLOAD
	val emit_BLOADindex = emit_RRR op_BLOADindex
	val emit_BLOADoffset = emit_RRI op_BLOADoffset
	val emit_BSTORE = emit_RR op_BSTORE
	val emit_BSTOREimmed = emit_RI op_BSTOREimmed
	val emit_BSTOREindex = emit_RRR op_BSTOREindex
	val emit_BSTOREoffset = emit_RIR op_BSTOREoffset
	val emit_ASHL = emit_RRR op_ASHL
	val emit_ASHL1 = emit_RR op_ASHL1
	val emit_ASHLimmed = emit_RRI op_ASHLimmed
	val emit_ASHR = emit_RRR op_ASHR
	val emit_ASHR1 = emit_RR op_ASHR1
	val emit_ASHRimmed = emit_RRI op_ASHRimmed
	val emit_ORB = emit_RRR op_ORB
	val emit_ORB1 = emit_RR op_ORB1
	val emit_ORBimmed = emit_RRI op_ORBimmed
	val emit_XORB = emit_RRR op_XORB
	val emit_XORBimmed = emit_RRI op_XORBimmed
	val emit_ANDB = emit_RRR op_ANDB
	val emit_ANDBimmed = emit_RRI op_ANDBimmed
	val emit_NOTB = emit_RR op_NOTB
	val emit_ADD = emit_RRR op_ADD
	val emit_ADDimmed = emit_RRI op_ADDimmed
	val emit_INCR = emit_RI op_INCR
	val emit_ADDT = emit_RRR op_ADDT
	val emit_ADDTimmed = emit_RRI op_ADDTimmed
	val emit_SUB = emit_RRR op_SUB
	val emit_SUBimmed = emit_RRI op_SUBimmed
	val emit_SUBT = emit_RRR op_SUBT
	val emit_SUBTimmed = emit_RRI op_SUBTimmed
	val emit_MULT = emit_RR op_MULT
	val emit_MULTimmed = emit_RI op_MULTimmed
	val emit_DIVT = emit_RR op_DIVT
	val emit_DIVTimmed = emit_RI op_DIVTimmed

	val emit_FLOAD = emit_FR op_FLOAD
	val emit_FLOADindex = emit_FRR op_FLOADindex
	val emit_FLOADoffset = emit_FRI op_FLOADoffset
	val emit_FSTORE = emit_RF op_FSTORE
	val emit_FSTOREindex = emit_RRF op_FSTOREindex
	val emit_FSTOREoffset = emit_RIF op_FSTOREoffset
	val emit_FADD = emit_FFF op_FADD
	val emit_FSUB = emit_FFF op_FSUB
	val emit_FMUL = emit_FFF op_FMUL
	val emit_FDIV = emit_FFF op_FDIV
	val emit_FNEG = emit_FF op_FNEG
	val emit_FABS = emit_FF op_FABS
	val emit_FLOAT = emit_FR op_FLOAT
	val emit_FALLOC = emit_RF op_FALLOC

      (* allocate a two-word "special" cell *)
	val op_SPECIAL = 255
	val emit_SPECIAL = emit_RRR op_SPECIAL

	fun comment _ = ()

      end (* structure Coder *)

    val finish = Emitter.finish

  end (* BCMC *)

(*
 * $Log: bcmc.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:28  george
 *   Version 109.24
 *
 *)

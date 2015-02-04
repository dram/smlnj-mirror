(* sparcmcode.sml
 *
 * J.H. Reppy (3/15/89)
 * Cornell University
 * Ithaca, NY  14853
 * jhr@cs.cornell.edu
 *)

structure SparcMCode : SPARCMCODER =
struct
 structure Coder : BASICSPARC = struct
    open SparcMCBase

    datatype register = REG of int

    datatype fregister = FREG of int

    datatype reg_or_immed
	=   REGrand of register
	|   IMrand of int

    structure IEEERealConst =
	RealConst(IEEEReal(SparcMCBase (* really only emitWord *)))

  (* emit constants into the code stream *)
    val emitReal = IEEERealConst.realconst

  (* emit a string constant into the code stream. *)
    fun emitString s = emitstring s

  (* label operations *)
    fun emitLab (i, lab) = jump(LABELptr i, lab)

  (* Span dependent psuedo-instructions for handling relative addresses. *)
    fun loadAdr (lab, offset, REG baseReg, REG dstReg) =
	    jump(LOADadr(offset, baseReg, dstReg), lab)
    fun adjustAdr (lab, REG baseReg) = jump(ADJUSTadr baseReg, lab)

  (* span dependent conditional jumps (psuedo instructions) *)
    local
	fun emitBcc opcode = let
		val opcode = (opcode << 22)
		in
		    fn lab => jump(Bcc opcode, lab)
		end
    in

    val emit_ba = emitBcc 66	    (* 1000010 *)
    val emit_be = emitBcc 10	    (* 0001010 *)
    val emit_bne = emitBcc 74	    (* 1001010 *)
    val emit_ble = emitBcc 18	    (* 0010010 *)
    val emit_bge = emitBcc 90	    (* 1011010 *)
    val emit_bl = emitBcc 26	    (* 0011010 *)
    val emit_bg = emitBcc 82	    (* 1010010 *)
    val emit_fbe = emitBcc 78	    (* 1001110 *)
    val emit_fbne = emitBcc 14	    (* 0001110 *)
    val emit_fble = emitBcc 110	    (* 1101110 *)
    val emit_fbge = emitBcc 94	    (* 1011110 *)
    val emit_fbl = emitBcc 38	    (* 0100110 *)
    val emit_fbg = emitBcc 54	    (* 0110110 *)

    end (* local *)

  (* nop - no operation (really a sethi 0,%g0) *)
    fun emit_nop () = emitLong 16777216

  (* sethi - set high 22 bits *)
    fun emit_sethi (immed, REG rd) = let
	val im = if immed < 0 then (immed + 4194304) else immed
	in
	    emitWord(256 ++ (rd << 9) ++ (im >> 16));
	    emitWord(im & 65535)
	end

    local
      (* emit a 3 operand instruction with "11" in bits 31-30 *)
	fun emitOp11 opcode (REG a, REGrand(REG b), REG c) = (
		emitWord(49152 ++ (c << 9) ++ (opcode << 3) ++ (a >> 2));
		emitWord(((a & 3) << 14) ++ b))
	|   emitOp11 opcode (REG a, IMrand b, REG c) = let
		val im = if b < 0 then (b + 8192) else b
		in
		    emitWord(49152 ++ (c << 9) ++ (opcode << 3) ++ (a >> 2));
		    emitWord(((a & 3) << 14) ++ 8192 ++ im)
		end
    in

  (* ld - load a register from memory (3rd arg is dst) *)
    val emit_ld = emitOp11 0
  (* st - store a register into memory (3rd arg is src) *)
    val emit_st = emitOp11 4

  (* ldf - load a floating-point register from memory (3rd arg is dst) *)
    fun emit_ldf (r, ri, FREG fr) = emitOp11 32 (r, ri, REG fr)
  (* stf - store a floating-point register into memory (3rd arg is src) *)
    fun emit_stf (r, ri, FREG fr) = emitOp11 36 (r, ri, REG fr)

  (* load (unsigned)/store byte instructions (3rd arg is dst/src) *)
    val emit_ldb = emitOp11 1
    val emit_stb = emitOp11 5

    end (* local *)

    local
      (* emit a 3 operand instructions with "10" in bits 31-30. *)
	fun emitOp10 opcode (REG a, REGrand(REG b), REG c) = (
		emitWord(32768 ++ (c << 9) ++ (opcode << 3) ++ (a >> 2));
		emitWord(((a & 3) << 14) ++ b))
	|   emitOp10 opcode (REG a, IMrand b, REG c) = let
		val im = if b < 0 then (b + 8192) else b
		in
		    emitWord(32768 ++ (c << 9) ++ (opcode << 3) ++ (a >> 2));
		    emitWord(((a & 3) << 14) ++ 8192 ++ im)
		end
    in

  (* jmpl - jump and link *)
    val emit_jmpl = emitOp10 56	    (* 111000 *)

  (* integer operations *)
    val emit_add = emitOp10 0	    (* 000000 *)
    val emit_sub = emitOp10 4	    (* 000100 *)
    val emit_subcc = emitOp10 20    (* 010100 *)
    val emit_sll = emitOp10 37	    (* 100101 *)
    val emit_sra = emitOp10 39	    (* 100111 *)
    val emit_and = emitOp10 1	    (* 000001 *)
    val emit_andcc = emitOp10 17    (* 010001 *)
    val emit_or = emitOp10 2	    (* 000010 *)
    val emit_xor = emitOp10 3	    (* 000011 *)

  (* not - really "xnor r1,%g0,r2" *) 
    fun emit_not (r1, r2) = (emitOp10 7 (r1, REGrand(REG 0), r2))

    end (* local *)

  (* floating-point operations *)
    local
      (* emit a floating-point instruction of three args; this has "10" in
       * bits 31-30 and "110100" in bits 24-19.
       *)
	fun emitFOp3 opcode (FREG a, FREG b, FREG c) = (
		emitWord (33184 ++ (c << 9) ++ (a >> 2));
		emitWord (((a & 3) << 14) ++ (opcode << 5) ++ b))
       (* emit a 2 operand floating-point instruction (same bits as above) *)
	fun emitFOp2 opcode (FREG a, FREG b) = (
		emitWord (33184 ++ (b << 9));
		emitWord ((opcode << 5) ++ a))
       (* emit a 2 operand floating-point instruction with "110101" in bits 24-19. *)
	fun emitFOp2' opcode (FREG a, FREG b) = (
		emitWord (33192 ++ (a >> 2));
		emitWord (((a & 3) << 14) ++ (opcode << 5) ++ b))
    in

    val emit_fadd = emitFOp3 66	    (* 001000010 *)
    val emit_fsub = emitFOp3 70	    (* 001000110 *)
    val emit_fmul = emitFOp3 74	    (* 001001010 *)
    val emit_fdiv = emitFOp3 78	    (* 001001110 *)
    val emit_fneg = emitFOp2 5	    (* 000000101 *)

    val emit_fcmp = emitFOp2' 82    (* 001010010 *)

    end (* local *)

  (* trap on integer overflow *)
    fun emit_tvs () = (emitWord 36816; emitWord 0)  (* "10__0111111010000 ..." *)

    fun comment _ = ()

 end
 val finish = SparcMCBase.finish

end (* structure SparcMCode *)

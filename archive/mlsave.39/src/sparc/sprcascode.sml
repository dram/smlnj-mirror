(* sparcascode.sml
 *
 * J.H. Reppy (3/15/89)
 * Cornell University
 * Ithaca, NY  14853
 * jhr@cs.cornell.edu
 *)

structure SparcAssem =
struct
    val outfile = ref std_out
end (* SparcAssem *)


structure SparcAsCode : BASICSPARC =
struct

    open SparcAssem

    type Label = string

    datatype register = REG of int

    datatype fregister = FREG of int

    datatype reg_or_immed
	=   REGrand of register
	|   IMrand of int

  (* We use %o4 as a temporary register for intermediate address computations.
   * Note: %o4 is also used as SparcCM.localTmp, but there should be no conflict.
   *)
    val tmpReg = REG 12

  (** utility routines **)
    local
	val hexDigits = "0123456789abcdef"
	fun f (0, l) = l
	|   f (n, l) = (f (Bits.rshift(n, 4),
			    chr(ordof(hexDigits, Bits.andb(n, 15))) :: l))
	fun cvt 0 = ["0", "x", "0"]
	|   cvt n = ("0" :: "x" :: f(n, nil))
    in
	fun atoi i = implode(if (i < 0) then "-" :: cvt(~i) else cvt i)
    end

    fun emit s = output (!outfile) s

    fun newLine () = emit "\n"
    fun comma () = emit ","

    fun emitReg (REG 0) = emit "%g0"
    |	emitReg (REG 1) = emit "%g1"
    |	emitReg (REG 2) = emit "%g2"
    |	emitReg (REG 3) = emit "%g3"
    |	emitReg (REG 4) = emit "%g4"
    |	emitReg (REG 5) = emit "%g5"
    |	emitReg (REG 6) = emit "%g6"
    |	emitReg (REG 7) = emit "%g7"
    |	emitReg (REG 8) = emit "%o0"
    |	emitReg (REG 9) = emit "%o1"
    |	emitReg (REG 10) = emit "%o2"
    |	emitReg (REG 11) = emit "%o3"
    |	emitReg (REG 12) = emit "%o4"
    |	emitReg (REG 13) = emit "%o5"
    |	emitReg (REG 14) = emit "%sp"
    |	emitReg (REG 15) = emit "%o7"
    |	emitReg (REG 16) = emit "%l0"
    |	emitReg (REG 17) = emit "%l1"
    |	emitReg (REG 18) = emit "%l2"
    |	emitReg (REG 19) = emit "%l3"
    |	emitReg (REG 20) = emit "%l4"
    |	emitReg (REG 21) = emit "%l5"
    |	emitReg (REG 22) = emit "%l6"
    |	emitReg (REG 23) = emit "%l7"
    |	emitReg (REG 24) = emit "%i0"
    |	emitReg (REG 25) = emit "%i1"
    |	emitReg (REG 26) = emit "%i2"
    |	emitReg (REG 27) = emit "%i3"
    |	emitReg (REG 28) = emit "%i4"
    |	emitReg (REG 29) = emit "%i5"
    |	emitReg (REG 30) = emit "%fp"
    |	emitReg (REG 31) = emit "%i7"

    fun emitFReg (FREG 0) = emit "%f0"
    |	emitFReg (FREG 1) = emit "%f1"
    |	emitFReg (FREG 2) = emit "%f2"
    |	emitFReg (FREG 3) = emit "%f3"

    fun emitRand (REGrand r) = emitReg r
    |	emitRand (IMrand i) = emit(atoi i)

    fun emitOffset 0 = ()
    |	emitOffset i = (if (i < 0)
	    then (emit "-"; emit(atoi(~i)))
	    else (emit "+"; emit(atoi i)))

    fun emitAddr (base, REGrand(REG 0)) = emitReg base
    |	emitAddr (base, IMrand 0) = emitReg base
    |	emitAddr (base, REGrand offset) = (emitReg base; emit "+"; emitReg offset)
    |	emitAddr (base, IMrand offset) = (emitReg base; emitOffset offset)

    fun emitMemAddr args = (emit "["; emitAddr args; emit "]")


    local
	val cnt = ref 0
    in
    fun newlabel () = (inc cnt; "L" ^ makestring(!cnt))
    end (* local *)

  (* misc operations *)
    fun align () = emit "\t.align\t4\n"

    fun mark () = let
	    val lab = newlabel()
	    in
		emit lab; emit ":\t.long\t(((";
		emit lab; emit "-base)/4+1)*power_tags)+tag_backptr\n"
	    end

  (* emit constants into the code stream *)
    fun emitLong i = (emit "\t.long\t"; emit(atoi i); newLine())
    fun emitReal r = (emit "\t.double\t"; emit r; newLine())

    local
	fun oct i = let
		val m = Integer.makestring
		in
		    m(i div 64)^m((i div 8)mod 8)^m(i mod 8)
		end
	fun c_char "\n" = "\\n"
	|   c_char "\t" = "\\t"
	|   c_char "\\" = "\\\\"
	|   c_char "\"" = "\\\""
	|   c_char c = if ord c < 32 then "\\"^oct(ord c) else c
	fun a_str s = implode(map c_char (explode s))
    in

    fun emitString s = (
	    emit "\t.ascii \""; emit(a_str s); emit "\"\n"; emit "\t.align\t4\n")

    end (* local *)

  (* label operations *)
    fun emitLab (k, lab) = (
	    emit "5:\t.long\t("; emit lab; emit "-5b)"; emitOffset k; newLine())

    fun define lab = (emit lab; emit ":\n")

  (* Span dependent psuedo-instructions for handling relative addresses. *)
    fun loadAdr (lab, offset, baseReg, reg) = (
	    emit "\tset\t"; emit lab; emitOffset offset;
	    comma(); emitReg reg; newLine())
    fun adjustAdr (lab, reg) = (
	    emit "\tset\t("; emit lab; emit "-base),"; emitReg tmpReg; newLine();
	    emit "\tsub\t"; emitReg reg; comma(); emitReg tmpReg; comma();
		emitReg reg; newLine())

  (* span dependent conditional jumps (psuedo instructions) *)
    fun emit_ba lab = (emit "\tba\t"; emit lab; newLine())
    fun emit_be lab = (emit "\tbe\t"; emit lab; newLine())
    fun emit_bne lab = (emit "\tbne\t"; emit lab; newLine())
    fun emit_ble lab = (emit "\tble\t"; emit lab; newLine())
    fun emit_bge lab = (emit "\tbge\t"; emit lab; newLine())
    fun emit_bl lab = (emit "\tbl\t"; emit lab; newLine())
    fun emit_bg lab = (emit "\tbg\t"; emit lab; newLine())
    fun emit_fbe lab = (emit "\tfbe\t"; emit lab; newLine())
    fun emit_fbne lab = (emit "\tfbne\t"; emit lab; newLine())
    fun emit_fble lab = (emit "\tfble\t"; emit lab; newLine())
    fun emit_fbge lab = (emit "\tfbge\t"; emit lab; newLine())
    fun emit_fbl lab = (emit "\tfbl\t"; emit lab; newLine())
    fun emit_fbg lab = (emit "\tfbg\t"; emit lab; newLine())

  (* nop *)
    fun emit_nop () = emit "\tnop\n"

  (* sethi - set the high 22 bits of a register *)
    fun emit_sethi (i, reg) = (
	    emit "\tsethi\t"; emit(atoi i); comma(); emitReg reg; newLine())

  (* ld - load a register from memory (3rd arg is dst) *)
    fun emit_ld (a, b, c) =
	    (emit "\tld\t"; emitMemAddr(a, b); comma(); emitReg c; newLine())
  (* st - store a register into memory (3rd arg is src) *)
    fun emit_st (a, b, c) =
	    (emit "\tst\t"; emitReg c; comma(); emitMemAddr(a, b); newLine())

  (* ldf - load a floating-point register from memory (3rd arg is dst) *)
    fun emit_ldf (a, b, c) =
	    (emit "\tldf\t"; emitMemAddr(a, b); comma(); emitFReg c; newLine())
  (* stf - store a floating-point register into memory (3rd arg is src) *)
    fun emit_stf (a, b, c) =
	    (emit "\tstf\t"; emitFReg c; comma(); emitMemAddr(a, b); newLine())

  (* load/store (unsigned) byte instructions (3rd arg is dst/src) *)
    fun emit_ldb (a, b, c) =
	    (emit "\tldub\t"; emitMemAddr(a, b); comma(); emitReg c; newLine())
    fun emit_stb (a, b, c) =
	    (emit "\tstb\t"; emitReg c; comma(); emitMemAddr(a, b); newLine())

  (* jmpl - jump and link *)
    fun emit_jmpl (a, b, REG 0) = (
	    emit "\tjmp\t"; emitAddr(a, b); newLine())
    |	emit_jmpl (a, b, REG 15) = (
	    emit "\tcall\t"; emitAddr(a, b); newLine())

  (* integer operations *)
    local
	fun emit3Op op_name (a, b, c) = (
		emit "\t"; emit op_name; emit "\t";
		emitReg a; comma(); emitRand b; comma(); emitReg c; newLine())
    in

    fun emit_add args = emit3Op "add" args
    fun emit_addcc args = emit3Op "addcc" args
    fun emit_sub args = emit3Op "sub" args
    fun emit_subcc (a, b, REG 0) = (
	    emit "\tcmp\t"; emitReg a; comma(); emitRand b; newLine())
    |	emit_subcc args = emit3Op "subcc" args
    fun emit_sll args = emit3Op "sll" args
    fun emit_sra args = emit3Op "sra" args
    fun emit_and args = emit3Op "and" args
    fun emit_andcc args = emit3Op "andcc" args
    fun emit_or (a, REGrand(REG 0), b) = (
	    emit "\tmov\t"; emitReg a; comma(); emitReg b; newLine())
    |	emit_or (REG 0, IMrand im, b) = (
	    emit "\tset\t"; emit(atoi im); comma(); emitReg b; newLine())
    |	emit_or args = emit3Op "or" args
    fun emit_xor args = emit3Op "xor" args

    end (* local *)

    fun emit_not (a, b) = (emit "\tnot\t"; emitReg a; comma(); emitReg b; newLine())

  (* floating-point operations *)
    local
	fun emit2FOp op_name (a, b) = (
		emit "\t"; emit op_name; emit "\t";
		emitFReg a; comma(); emitFReg b; newLine())
	fun emit3FOp op_name (a, b, c) = (
		emit "\t"; emit op_name; emit "\t";
		emitFReg a; comma(); emitFReg b; comma(); emitFReg c; newLine())
    in

    fun emit_fadd args = emit3FOp "faddd" args
    fun emit_fsub args = emit3FOp "fsubd" args
    fun emit_fmul args = emit3FOp "fmuld" args
    fun emit_fdiv args = emit3FOp "fdivd" args
    fun emit_fcmp args = emit2FOp "fcmpd" args
    fun emit_fneg args = emit2FOp "fnegs" args

    end (* local *)

  (* trap on integer overflow *)
    fun emit_tvs () = (emit "\ttvs\tST_INT_OVERFLOW\n")

    local
	fun emitLine s = (emit "| "; emit s; newLine())
	fun split s = let
		val len = size s
		fun nextNL i = if ordof(s, i) = ord("\n")
			then i
			else nextNL(i+1)
		fun split' i = let
			val j = (nextNL i) handle Ord => len
			in
			    if (i < len)
			    then substring(s, i, j-i) :: split'(j+1)
			    else nil
			end
		in
		    split' 0
		end
    in
    fun comment s = app emitLine (split s)
    (* fun comment s = (emit "| "; emit s) *)
    end (* local *)

end (* structure SparcAsCode *)

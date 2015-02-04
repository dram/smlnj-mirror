(* sparccoder.sml
 *
 * J.H. Reppy (3/15/89)
 * Cornell University
 * Ithaca, NY  14853
 * jhr@cs.cornell.edu
 *)

functor SparcCoder (BS : BASICSPARC) : SPARCCODER =
struct

(* +DEBUG
    fun diag (s : string) f x = (f x) handle e => (
	    print "?exception "; print (System.exn_name e);
	    print " in SparcCoder."; print s; print "\n";
	    raise e)
   -DEBUG *)

    structure BS' :
    sig
	eqtype Label
	sharing type Label = BS.Label

	datatype register = REG of int

	datatype fregister = FREG of int

	datatype reg_or_immed
	    =	REGrand of register
	    |	IMrand of int

      (* misc operations *)
	val align : unit -> unit
	val mark : unit -> unit

      (* emit constants into the code stream *)
	val emitLong : int -> unit
	val emitReal : string -> unit
	val emitString : string -> unit

      (* label operations *)
	val emitLab : (int * Label) -> unit
	val newlabel : unit -> Label
	val define : Label -> unit

      (* Span dependent psuedo-instructions for handling relative addresses. *)
	val loadAdr : (Label * int * register * register) -> unit
	val adjustAdr : (Label * register) -> unit

      (* floating-point operations *)
	val emit_fadd : (fregister * fregister * fregister) -> unit
	val emit_fsub : (fregister * fregister * fregister) -> unit
	val emit_fmul : (fregister * fregister * fregister) -> unit
	val emit_fdiv : (fregister * fregister * fregister) -> unit
	val emit_fneg : (fregister * fregister) -> unit

      (* trap on integer overflow *)
	val emit_tvs : unit -> unit

	val comment : string -> unit

    end (* sig *) = BS

    open BS'

(* DEBUG  val emitLab = diag "BS.emitLab" emitLab *)
(* DEBUG  val loadAdr = diag "BS.loadAdr" loadAdr *)
(* DEBUG  val emit_fadd = diag "BS.emit_fadd" emit_fadd *)
(* DEBUG  val emit_fsub = diag "BS.emit_fsub" emit_fsub *)
(* DEBUG  val emit_fmul = diag "BS.emit_fmul" emit_fmul *)
(* DEBUG  val emit_fdiv = diag "BS.emit_fdiv" emit_fdiv *)
(* DEBUG  val emit_fneg = diag "BS.emit_fneg" emit_fneg *)
(* DEBUG  val emit_tvs = diag "BS.emit_tvs" emit_tvs *)

    datatype EA
	= Direct of register		(* %ri *)
	| Immed of int			(* immediate value, may be > 13-bits *)
	| ImmedLab of Label
      (* the following modes are for internal use only *)
	| Displace of (register * int)
	| Index of (register * register)
	| FloatReg of fregister

    datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR

  (* the sizes of immediate operands *)
    datatype immed_sz = Immed13 | Immed32

  (* return the size of an integer *)
    fun sizeImmed n = if (~4096 <= n) andalso (n < 4096) then Immed13 else Immed32

  (* global registers *)
    val g0 = REG 0 and g1 = REG 1 and g2 = REG 2 and g3 = REG 3
    val g4 = REG 4 and g5 = REG 5 and g6 = REG 6 and g7 = REG 7
  (* output registers *)
    val o0 = REG 8 and o1 = REG 9 and o2 = REG 10 and o3 = REG 11
    val o4 = REG 12 and o5 = REG 13 and sp = REG 14 and o7 = REG 15
  (* local registers *)
    val l0 = REG 16 and l1 = REG 17 and l2 = REG 18 and l3 = REG 19
    val l4 = REG 20 and l5 = REG 21 and l6 = REG 22 and l7 = REG 23
  (* input registers *)
    val i0 = REG 24 and i1 = REG 25 and i2 = REG 26 and i3 = REG 27
    val i4 = REG 28 and i5 = REG 29 and fp = REG 30 and i7 = REG 31

  (* floating-point registers *)
    val f0 = FREG 0
    val f1 = FREG 1
    val f2 = FREG 2
    val f3 = FREG 3

  (* %g0 always contains zero *)
    val zeroRand = REGrand g0

  (* use %o2 as a local temporary.  Note that mul and div also use %o2 and %o3. *)
    val tmpReg = o2
    val tmpRand = REGrand tmpReg
    val tmpReg2 = o3

  (* load an immediate value into a register *)
    fun setReg (n, dst) = (
	    case sizeImmed n
	    of	Immed13 => BS.emit_or(g0, IMrand n, dst)
	    |	Immed32 => let
		    val lo10 = Bits.andb(n, 1023)
		    in
			BS.emit_sethi(Bits.rshift(n, 10), dst);
			if (lo10 <> 0)
			then BS.emit_or(dst, IMrand lo10, dst)
			else ()
		    end)
(* DEBUG  val setReg = diag "setReg" setReg *)

    local
      (* emit code for load/store operations where the memory address
       * is [%r+immed].
       *)
	fun emitMemOp f (baseReg, offset, reg) = (
		case sizeImmed offset
		of  Immed13 => f(baseReg, IMrand offset, reg)
		|   Immed32 => (setReg(offset, tmpReg); f(baseReg, tmpRand, reg)))
    in

  (* generic data movement instruction (derived) *)
    fun move (Direct src, Direct dst) = BS.emit_or(src, zeroRand, dst)
    |	move (Immed n, Direct dst) = setReg (n, dst)
    |	move (Immed n, Displace(base, offset)) =
	    (setReg (n, tmpReg2); emitMemOp BS.emit_st (base, offset, tmpReg2))
    |	move (Immed n, Index(base, offset)) =
	    (setReg (n, tmpReg2); BS.emit_st(base, REGrand offset, tmpReg2))
    |	move (Displace(base, offset), Direct dst) =
	    (emitMemOp BS.emit_ld (base, offset, dst))
    |	move (Index(base, offset), Direct dst) =
	    BS.emit_ld(base, REGrand offset, dst)
    |	move (Direct src, Displace(base, offset)) =
	    (emitMemOp BS.emit_st (base, offset, src))
    |	move (Direct src, Index(base, offset)) =
	    BS.emit_st(base, REGrand offset, src)
    |	move (Displace(base, offset), FloatReg dst) =
	    (emitMemOp BS.emit_ldf (base, offset, dst))
    |	move (FloatReg src, Displace(base, offset)) =
	    (emitMemOp BS.emit_stf (base, offset, src))
    |	move (ImmedLab lab, Direct dst) = loadAdr(lab, 0, i3 (* baseCodePtr *), dst)
    |	move _ = ErrorMsg.impossible "[SparcCoder.move]"

    end (* local *)
(* DEBUG val move = diag "move" move *)

  (* load/store byte instructions *)
    fun emit_ldb (Index(base, offset), Direct dst) =
	    BS.emit_ldb(base, REGrand offset, dst)
    fun emit_stb (Direct src, Index(base, offset)) =
	    BS.emit_stb(base, REGrand offset, src)
(* DEBUG val emit_stb = diag "emit_stb" emit_stb *)
(* DEBUG val emit_ldb = diag "emit_ldb" emit_ldb *)

  (* control flow operations *)
    fun emit_jmp (Direct r) = (BS.emit_jmpl(r, zeroRand, g0); BS.emit_nop())
    |	emit_jmp (Displace(base, offset)) = (
	    case sizeImmed offset
	    of	Immed13 => BS.emit_jmpl(base, IMrand offset, g0)
	    |	Immed32 => (setReg(offset, tmpReg); BS.emit_jmpl(base, tmpRand, g0));
	    BS.emit_nop())
    |	emit_jmp (Index(base, offset)) = (
	    BS.emit_jmpl(base, REGrand offset, g0);  BS.emit_nop())
    |	emit_jmp (ImmedLab lab) = (BS.emit_ba(lab); BS.emit_nop())

    fun emit_jmpl (Direct r) = (BS.emit_jmpl(r, zeroRand, o7); BS.emit_nop())
(* DEBUG  val emit_jmp = diag "emit_jmp" emit_jmp *)
(* DEBUG  val emit_jmpl = diag "emit_jmpl" emit_jmpl *)

  (* conditional jumps *)
    local
	fun bcc EQL = BS.emit_be
	|   bcc NEQ = BS.emit_bne
	|   bcc LEQ = BS.emit_ble
	|   bcc GEQ = BS.emit_bge
	|   bcc LSS = BS.emit_bl
	|   bcc GTR = BS.emit_bg
	fun fbcc EQL = BS.emit_fbe
	|   fbcc NEQ = BS.emit_fbne
	|   fbcc LEQ = BS.emit_fble
	|   fbcc GEQ = BS.emit_fbge
	|   fbcc LSS = BS.emit_fbl
	|   fbcc GTR = BS.emit_fbg
    in
	fun emitCondJmp (cond, lab) = (bcc cond lab; BS.emit_nop())
	fun emitFCondJmp (cond, lab) = (fbcc cond lab; BS.emit_nop())
(* DEBUG  val emitCondJmp = diag "emitCondJmp" emitCondJmp *)
(* DEBUG  val emitFCondJmp = diag "emitFCondJmp" emitFCondJmp *)
    end

    local
(* +DEBUG
	fun prEA (Direct(REG r)) = (print "%r"; print r; ())
	|   prEA (Immed n) = (print n; ())
	|   prEA (ImmedLab _) = (print "<label>"; ())
	|   prEA (Displace _) = (print "Displace"; ())
	|   prEA (Index _) = (print "Index"; ())
	|   prEA (FloatReg(FREG r)) = (print "%f"; print r; ())
	fun prArgs (a, b, c) = (
		print "("; prEA a; print ", "; prEA b; print ", ";
		prEA c; print ")"; ())
	exception ThreeOp
-DEBUG *)
	fun threeOp f (Direct a, Direct b, Direct c) = f (a, REGrand b, c)
	|   threeOp f (Direct a, Immed b, Direct c) = (
		case sizeImmed b
		of  Immed13 => f(a, IMrand b, c)
		|   Immed32 => (setReg(b, tmpReg); f(a, tmpRand, c)))
(* +DEBUG
	|   threeOp _ args = (
		print "\n** threeOp "; prArgs args; print "\n"; ()(*raise ThreeOp*))
-DEBUG *)
    in

(*********)
  (* integer operations *)
    val emit_add = threeOp BS.emit_add
    val emit_sub = threeOp BS.emit_sub
    val emit_sll = threeOp BS.emit_sll
    val emit_sra = threeOp BS.emit_sra
    val emit_and = threeOp BS.emit_and
    val emit_or = threeOp BS.emit_or
    val emit_xor = threeOp BS.emit_xor
(**********)
(*** UNTIL CPS BUGS ARE FIXED ***
    local
	fun warning (s : string, a : int, b : int) = (
		print "WARNING: missed constant folding: SparcCoder."; print s;
		print "("; print a; print ", "; print b; print ")\n"; ())
	val emit_add' = threeOp BS.emit_add
	val emit_sub' = threeOp BS.emit_sub
	val emit_sll' = threeOp BS.emit_sll
	val emit_sra' = threeOp BS.emit_sra
	val emit_and' = threeOp BS.emit_and
	val emit_or' = threeOp BS.emit_or
	val emit_xor' = threeOp BS.emit_xor
    in
    fun emit_add (Immed a, Immed b, c) = (
	    warning("emit_add", a, b); move(Immed(a+b), c))
    |	emit_add args = emit_add' args
    fun emit_sub (Immed a, Immed b, c) = (
	    warning("emit_sub", a, b); move(Immed(a-b), c))
    |	emit_sub args = emit_sub' args
    fun emit_sll (Immed a, Immed b, c) = (
	    warning("emit_sll", a, b); move(Immed(Bits.lshift(a, b)), c))
    |	emit_sll args = emit_sll' args
    fun emit_sra (Immed a, Immed b, c) = (
	    warning("emit_sra", a, b); move(Immed(Bits.rshift(a, b)), c))
    |	emit_sra args = emit_sra' args
    fun emit_and (Immed a, Immed b, c) = (
	    warning("emit_and", a, b); move(Immed(Bits.andb(a, b)), c))
    |	emit_and args = emit_and' args
    fun emit_or (Immed a, Immed b, c) = (
	    warning("emit_or", a, b); move(Immed(Bits.orb(a, b)), c))
    |	emit_or args = emit_or' args
    fun emit_xor (Immed a, Immed b, c) = (
	    warning("emit_xor", a, b); move(Immed(Bits.xorb(a, b)), c))
    |	emit_xor args = emit_xor' args
    end 
*** UNTIL CPS BUGS ARE FIXED ***)

(* DEBUG  val emit_add = diag "emit_add" emit_add *)
(* DEBUG  val emit_sub = diag "emit_sub" emit_sub *)
(* DEBUG  val emit_sll = diag "emit_sll" emit_sll *)
(* DEBUG  val emit_sra = diag "emit_sra" emit_sra *)
(* DEBUG  val emit_and = diag "emit_and" emit_and *)
(* DEBUG  val emit_or = diag "emit_or" emit_or *)
(* DEBUG  val emit_xor = diag "emit_xor" emit_xor *)

    end

    fun emit_not (Direct a, Direct b) = BS.emit_not(a, b)
(* DEBUG  val emit_not = diag "emit_not" emit_not *)

    fun emit_cmp (Direct a, Direct b) = BS.emit_subcc(a, REGrand b, g0)
    |	emit_cmp (Direct a, Immed b) = (
	    case sizeImmed b
	    of	Immed13 => BS.emit_subcc(a, IMrand b, g0)
	    |	Immed32 => (setReg(b, tmpReg); BS.emit_subcc(a, tmpRand, g0)))
(* DEBUG  val emit_cmp = diag "emit_cmp" emit_cmp *)

    fun emit_btst (Immed a, Direct b) = (
	    case sizeImmed a
	    of	Immed13 => BS.emit_andcc(b, IMrand a, g0)
	    |	Immed32 => (setReg(a, tmpReg); BS.emit_andcc(b, tmpRand, g0)))
(* DEBUG  val emit_btst = diag "emit_btst" emit_btst *)

    fun emitIncr (r, incr) = (
	    case sizeImmed incr
	    of	Immed13 => BS.emit_add(r, IMrand incr, r)
	    |	Immed32 => (setReg(incr, tmpReg); BS.emit_add(r, tmpRand, r)))
(* DEBUG  val emitIncr = diag "emitIncr" emitIncr *)

    fun emitDecr (r, incr) = (
	    case sizeImmed incr
	    of	Immed13 => BS.emit_sub(r, IMrand incr, r)
	    |	Immed32 => (setReg(incr, tmpReg); BS.emit_sub(r, tmpRand, r)))
(* DEBUG  val emitDecr = diag "emitDecr" emitDecr *)

  (* fcmp - floating-point compare.  Note, an integer instruction must come
   * between the fcmp and the fbcc.
   *)
    fun emit_fcmp args = (BS.emit_fcmp args; BS.emit_nop())
(* DEBUG  val emit_fcmp = diag "emit_fcmp" emit_fcmp *)

end (* functor SparcCoder *)

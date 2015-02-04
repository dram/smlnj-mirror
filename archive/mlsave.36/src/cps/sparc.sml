(* sparc.sml
 *
 * J.H. Reppy (3/15/89)
 * Cornell University
 * Ithaca, NY  14853
 * jhr@cs.cornell.edu
 *)

functor SparcCM (S : SPARCCODER) : CMACHINE =
struct

(* +DEBUG *)
    fun diag (s : string) f x = (f x) handle e => (
	    print "?exception "; print (System.exn_name e);
	    print " in SparcCM."; print s; print "\n";
	    raise e)
(* -DEBUG *)

    structure S' :
    sig
	eqtype Label
	sharing type Label = S.Label

	datatype register = REG of int

	datatype fregister = FREG of int

	datatype EA
	    = Direct of register	    (* %ri *)
	    | Immed of int		    (* immediate value, may be > 13-bits *)
	    | ImmedLab of Label
	  (* the following modes are for internal use only *)
	    | Displace of (register * int)
	    | Index of (register * register)
	    | FloatReg of fregister

	datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR

	val move : (EA * EA) -> unit

    end = S

    open S'

  (* makes the immediate integer mode *)
    val immed = Immed

  (* if a is an immediate integer mode, extracts the integer *)
    fun isimmed (Immed i) = SOME i
    |	isimmed _ = NONE

    fun isreg (Direct(REG r)) = SOME r
    |	isreg _ = NONE
    fun eqreg (a: EA) b = (a = b)


  (** DEDICATED REGISTERS **)

  (* The following registers can hold pointers or properly tagged integers *)
    val exnptr			    = Direct S.g7
    val dataptr as Direct dataptr'  = Direct S.g6
    val storeptr		    = Direct S.g5

  (* The following registers may not hold pointers, and may hold untagged ints *)
    val arithtemp as Direct arithtemp'	= Direct S.o0
    val arithtemp2			= Direct S.o1

  (* The following registers are not dedicated, and must be all disjoint *)
    val standardclosure = Direct S.i2
    val standardarg	= Direct S.i0
    val standardcont	= Direct S.i1
    val miscregs	= map Direct [S.l0, S.l1, S.l2, S.l3, S.l4, S.l5, S.l6,
			    S.l7, S.i4, S.i5, S.g1, S.g2, S.g3, S.g4]

  (* we use %i3 to hold the base address of the current code block. *)
    val baseCodePtr as Direct baseCodePtr'  = Direct S.i3

  (* we use %o2 and %o3 as arguments to the mul and div code.  Note: these are
   * also used by SparcCoder as temporaries, but there isn't any conflict.
   *)
    val argReg1	as Direct argReg1'	= Direct S.o2
    val argReg2				= Direct S.o3

  (* we use %o4 as a local temporary *)
    val localTmp as Direct localTmp'	= Direct S.o4
    val ptrTmp as Direct ptrTmp'	= Direct S.o5	(** is this GC'ed? **)


  (* We treat align as a nop, since all instructions are 4-bytes and we always
   * realign after a string.
   *)
    fun align () = ()

  (* insert a gc-tag in the code so that next address may be moved into a record *)
    val mark = S.mark

  (* put an 4-byte integer literal into the code *)
    val emitlong = S.emitLong 

  (* put a floating literal into the code *)
    val realconst = S.emitReal	

  (* put a literal string into the code (just the chars, no descriptor or length) *)
    val emitstring = S.emitString 


  (* L3: emitlab(k,L2) is equivalent to L3: emitlong(k+L2-L3) *)
    fun emitlab (k, ImmedLab lab) = S.emitLab(k, lab)

  (* create a new label (but don't define it) *)
    val newlabel = (ImmedLab o S.newlabel)

  (* Associate a label with a point in the code *)
    fun define (ImmedLab lab) = S.define lab

  (* beginStdFn(cl, lab):
   * Note the beginning of a standard function with entry label lab, and
   * register cl containing its closure.  This requires generating code to
   * load the base code block address into baseCodePtr.
   *)
    fun beginStdFn (Direct closureReg, ImmedLab lab) = (
	    S.comment "begin standard fn\n";
	    move (Displace(closureReg, 0), baseCodePtr);
	    S.adjustAdr (lab, baseCodePtr'))
    |	beginStdFn _ = ErrorMsg.impossible("[SparcCM.beginStdFn]")
(* DEBUG *) val beginStdFn = diag "beginStdFn" beginStdFn

  (* unconditional jump to the address specified *)
    val jmp = S.emit_jmp
(* DEBUG *) val jmp = diag "jmp" jmp

    local
	val minBlockSize = 5
      (* generate code to move one or more adjacent fields from one record into
       * adjacent fields in the new record.  If the block is big enough, then
       * use a block copy loop.
       *)
	fun blockMove (srcreg, startindx, path, offset) = let
	      (* check a CPS path to see how large the block is *)
		fun chkpath
		    (cnt, i, path as (Direct r, CPS.SELp(j, CPS.OFFp 0)) :: rest) =
			if (r = srcreg) andalso (i+offset = j)
			then chkpath(cnt+1, i-1, rest)
			else (cnt, path)
		  | chkpath (cnt, _, rest) = (cnt, rest)
	      (* generate code to move fields individually *)
		fun moveFields (0, _) = ()
		  | moveFields (n, indx) = (
			move (Displace(srcreg, (indx+offset)*4), ptrTmp);
			move (ptrTmp, Displace(dataptr', indx*4));
			moveFields(n-1, indx-1))
		val (blksz, rest) = chkpath(1, startindx-1, path)
		in
		    if (blksz >= minBlockSize)
		    then let
			val lab = S.newlabel()
			in
			  (* generate code for a block copy loop *)
			    if (offset = 0) then (
				move(Immed((startindx*4)+4), localTmp);
				S.define lab;
				S.emitDecr(localTmp', 4);
				move (Index(srcreg, localTmp'), ptrTmp);
				move (ptrTmp, Index(dataptr', localTmp'));
				S.emit_cmp(localTmp, Immed(((startindx-blksz)+1)*4));
				S.emitCondJmp (GTR, lab))
			    else (
				move(Immed((startindx+offset+1)*4), argReg1);
				move(Immed((startindx*4)+4), localTmp);
				S.define lab;
				S.emitDecr(argReg1', 4);
				move (Index(srcreg, argReg1'), ptrTmp);
				S.emitDecr(localTmp', 4);
				move (ptrTmp, Index(dataptr', localTmp'));
				if (offset < 0)
				then S.emit_cmp(argReg1,
					Immed(((startindx-blksz)+offset+1)*4))
				else S.emit_cmp(localTmp,
					Immed(((startindx-blksz)+1)*4));
				S.emitCondJmp (GTR, lab))
			end
		    else moveFields(blksz, startindx);
		    (startindx-blksz, rest)
		end
    in

  (* record:
   * makes a new record, puts address of it into the destination specified
   * by the second arg. The contents are numbered from ~1 and up.
   *)
    fun record (vl : (EA * CPS.accesspath) list, dst : EA) = let
	    val len = length vl
	  (* For each field in the record generate the necessary moves to initialize
	   * it in the new record.  Note, since the initialization of a given field
	   * may trigger GC, the source of that move must be in a register that is
	   * saved across GCs (eg., ptrTmp).
	   *)
	    fun fields (_, nil) = ()
	      | fields (i, (Direct r, CPS.SELp(j, CPS.OFFp 0)) :: rest) =
		    if (i <> (len-2))
		    then fields (blockMove (r, i, rest, j-i))
		    else (
		      (* Block moves are not safe for the top word of a record
		       * since it can trigger garbage collection.
		       *)
			move (Displace(r, j*4), ptrTmp);
			move (ptrTmp, Displace(dataptr', i*4));
			fields (i-1, rest))
	      | fields (i, (Direct r, CPS.SELp(j, p)) :: rest) = (
		    move (Displace(r, j*4), ptrTmp);
		    fields (i, (ptrTmp, p) :: rest))
	      | fields (i, (ImmedLab l, p) :: rest) = (
		    S.loadAdr(l, 0, baseCodePtr', ptrTmp');
		    fields (i, (ptrTmp, p) :: rest))
	      | fields (i, (x as Direct _, CPS.OFFp 0) :: rest) = (
		    move (x, Displace(dataptr', i*4));
		    fields (i-1, rest))
	      | fields (i, (Direct r, CPS.OFFp j) :: rest) = (
		    S.emit_add (Direct r, Immed(j*4), ptrTmp);
		    move (ptrTmp, Displace(dataptr', i*4));
		    fields (i-1, rest))
	      | fields (i, (x, p) :: rest) = (
		    move (x, ptrTmp);
		    fields (i, (ptrTmp, p)::rest))
	    in
		fields (len-2, rev vl);
		move (dataptr, dst);
		S.emitIncr (dataptr', len*4)
	    end
(* DEBUG *) val record = diag "record" record
    end (* local *)

  (* select(i,x,y) = y <- mem[x+4*i] *)
    fun select (i, Direct r, dst) = move(Displace(r, i*4), dst)
    |	select (i, ImmedLab lab, dst) = (
(** NOTE: it would be more efficient to use  "ld [baseCodePtr+offset],dst" **)
	    S.loadAdr(lab, i*4, baseCodePtr', localTmp');
	    move(Displace(localTmp', 0), dst))
(* DEBUG *) val select = diag "select" select

  (* offset(i,x,y) = y <- x+4*i *)
    fun offset (i, r as Direct _, dst) = S.emit_add(r, Immed(i*4), dst)
    |	offset (i, ImmedLab lab, Direct dst') = (
	    S.loadAdr(lab, i*4, baseCodePtr', dst'))
(* DEBUG *) val offset = diag "offset" offset

  (* fetchindexb(x,y) fetches a byte:  y <- mem[x+arithtemp]
   * Note: y cannot be arithtemp
   *)
    fun fetchindexb (Direct src', dst as Direct dst') = (
	    if (dst' = arithtemp')
	    then raise ErrorMsg.impossible "[SparcCM.fetchindexb]" else ();
	    S.emit_ldb (Index(src', arithtemp'), dst))
(* DEBUG *) val fetchindexb = diag "fetchindexb" fetchindexb

  (* storeindexb(x,y) stores a byte:  mem[y+arithtemp] <- x; *)
    fun storeindexb (src as Direct _, Direct dst') =
	    S.emit_stb (src, Index(dst', arithtemp'))
(* DEBUG *) val storeindexb = diag "storeindexb" storeindexb

  (* jmpindexb(x):  pc <- (x+arithtemp) *)
    fun jmpindexb (ImmedLab lab) = (
	    S.loadAdr(lab, 0, baseCodePtr', localTmp');
	    S.emit_jmp(Index(localTmp', arithtemp')))
(* DEBUG *) val jmpindexb = diag "jmpindexb" jmpindexb

  (* fetchindexl(x,y,z) fetches a word:	 y <- mem[x+2*(z-1)] *)
    fun fetchindexl (Direct src', dst, Immed n) =
	    move (Displace(src', 2*n-2), dst)
    |	fetchindexl (Direct src', dst, indx as Direct _) = (
	    S.emit_sub(indx, Immed 1, localTmp);
	    S.emit_add(localTmp, localTmp, localTmp);
	    move (Index(src', localTmp'), dst))
    |	fetchindexl (ImmedLab lab, dst, indx as Direct indx') = (
	    S.loadAdr (lab, ~2, baseCodePtr', localTmp');
	    S.emit_add(indx, localTmp, localTmp);
	    move (Index(localTmp', indx'), dst))
(* DEBUG *) val fetchindexl = diag "fetchindexl" fetchindexl

  (* storeindexl(x,y,z) stores a word:	mem[y+2*(z-1)] <- x *)
    fun storeindexl (src, Direct dst', Immed n) =
	    move (src, Displace(dst', 2*n-2))
    |	storeindexl (src, Direct dst', indx as Direct _) = (
	    S.emit_sub(indx, Immed 1, localTmp);
	    S.emit_add(localTmp, localTmp, localTmp);
	    move (src, Index(dst', localTmp')))
(* DEBUG *) val storeindexl = diag "storeindexl" storeindexl

    local
      (* adjust the order of arguments for commutative operations *)
	fun adjustArgs (a as Immed _, b, c) = (b, a, c)
	|   adjustArgs args = args
    in

    val orb = (S.emit_or o adjustArgs)
    val andb = (S.emit_and o adjustArgs)
    val xorb = (S.emit_xor o adjustArgs)
    val addl3 = (S.emit_add o adjustArgs)
    fun addl3t args = (addl3 args; S.emit_tvs())
(* DEBUG *) val orb = diag "orb" orb
(* DEBUG *) val andb = diag "andb" andb
(* DEBUG *) val xorb = diag "xorb" xorb
(* DEBUG *) val addl3 = diag "addl3" addl3
(* DEBUG *) val addl3t = diag "addl3t" addl3t

    end (* local *)

  (* shift left: count, src, dest; shift count is non-negative *)
    fun ashl (cnt, src as Direct _, dst as Direct _) =
	    S.emit_sll (src, cnt, dst)
    |	ashl (cnt, src as Immed _, dst as Direct _) = (
	    move (src, localTmp);
	    S.emit_sll (localTmp, cnt, dst))
(* DEBUG *) val ashl = diag "ashl" ashl

  (* shift left (with overflow check) *)
(*** NOTE: this doesn't really do overflow checks unless it is a
**** constant shift of 1.
***)
    fun ashlt (cnt as Immed 1, src, dst as Direct _) =
	    addl3t(src, src, dst)
    |	ashlt args = ashl args
(* DEBUG *) val ashlt = diag "ashlt" ashlt

  (* shift right: count, src, dest; shift count is non-negative *)
    fun ashr (cnt, src as Direct _, dst as Direct _) = 
	    S.emit_sra (src, cnt, dst)
    |	ashr (cnt, src as Immed _, dst as Direct _) = (
	    move (src, localTmp);
	    S.emit_sra (localTmp, cnt, dst))
(* DEBUG *) val ashr = diag "ashr" ashr

  (* notb: bitwise complement. *)
    val notb = S.emit_not

  (* subl3(a,b,c):  c <- (b - a) *)
    fun subl3 (a, b as Immed _, c) = (
	    move(b, localTmp);
	    S.emit_sub(localTmp, a, c))
    |	subl3 (a, b, c) = S.emit_sub (b, a, c)
(* DEBUG *) val subl3 = diag "subl3" subl3

  (* subl3t(a,b,c):  c <- (b - a) (with overflow checking) *)
    fun subl3t args = (subl3 args; S.emit_tvs())
(* DEBUG *) val subl3t = diag "subl3t" subl3t

  (* mull2t/divl2t:
   * mull2t (a, b):  b <- (a * b) (with overflow checking done by ml_mul)
   * divl2t (a, b):  b <- (b div a)
   *)
    local
	fun intOp opAdr (a, b as Direct _) = (
		move (opAdr, localTmp);
		move (a, argReg2);
		move (b, argReg1);
		S.emit_jmpl (localTmp);
		move (argReg1, b))
(* DEBUG *) val intOp = diag "intOp" intOp
	val mulAdr = Displace(S.sp, 72)
	val divAdr = Displace(S.sp, 76)
    in
    val mull2t = intOp mulAdr
    val divl2 = intOp divAdr
(* DEBUG *) val mull2t = diag "mull2t" mull2t
(* DEBUG *) val divl2 = diag "divl2" divl2
    end

  (* bbs(i, dst, lab): test the i'th bit of dst and jump to lab if its zero *)
    fun bbs (Immed i, dst as Direct _, ImmedLab lab) = (
	    S.emit_btst(Immed(Bits.lshift(1, i)), dst);
	    S.emitCondJmp(NEQ, lab))
(* DEBUG *) val bbs = diag "bbs" bbs

    local
      (* reverse a condition (eg., (a <= b) <==> (b >= a) *)
	fun revCond NEQ = NEQ
	|   revCond EQL = EQL
	|   revCond LEQ = GEQ
	|   revCond GEQ = LEQ
	|   revCond LSS = GTR
	|   revCond GTR = LSS
    in

  (* ibranch(cond, a, b, lab):	if (a <cond> b) then pc <- lab *)
    fun ibranch (cond, a as Immed x, b, ImmedLab lab) = (
	    S.emit_cmp (b, a);
	    S.emitCondJmp (revCond cond, lab))
    |	ibranch (cond, a, b, ImmedLab lab) = (
	    S.emit_cmp (a, b);
	    S.emitCondJmp (cond, lab))
(* DEBUG *) val ibranch = diag "ibranch" ibranch

    end (* local *)

  (** Floating point instructions **
   * These instructions take ML real values as arguments (ie., addresses of
   * heap objects) and store their results in the heap.
   *)

    local
	val floatReg0 = FloatReg S.f0
	val floatReg1 = FloatReg S.f1

      (* the tag code for real values. *)
	val realTag = Immed(8*System.Tags.power_tags + System.Tags.tag_string)

      (* finishReal(r): emit code to store the result of a real operation
       * (in %f0, %f1) in the heap, and put the result address in r.
       *)
	fun finishReal (r) = (
		move (realTag, localTmp);
		move (floatReg1, Displace(dataptr', 4));
		move (floatReg0, Displace(dataptr', 0));
		move (localTmp, Displace(dataptr', ~4));
		move (dataptr, r);
		S.emitIncr(dataptr', 4*3))
(* DEBUG *) val finishReal = diag "finishReal" finishReal

	fun fetchReal (Direct src', dst as (FREG dst')) = (
		move (Displace(src', 0), FloatReg dst);
		move (Displace(src', 4), FloatReg(FREG(dst'+1))))
	|   fetchReal (ImmedLab lab, dst) = (
		S.loadAdr(lab, 0, baseCodePtr', ptrTmp');
		fetchReal (ptrTmp, dst))
(* DEBUG *) val fetchReal = diag "fetchReal" fetchReal

	fun floatOp fOp (a, b, result as Direct _) = (
		fetchReal (a, S.f0);
		fetchReal (b, S.f2);
		fOp (S.f0, S.f2, S.f0);
		finishReal result)
(* DEBUG *) val floatOp = diag "floatOp" floatOp

    in

  (* Negate the first arg and return a pointer to the result in the second *)
    fun mnegg (src, dst as Direct _) = (
	    fetchReal (src, S.f0);
	    S.emit_fneg (S.f0, S.f0);
	    finishReal dst)
(* DEBUG *) val mnegg = diag "mnegg" mnegg

  (* Add the first two arguments and store the result in the third *)
    fun addg3 args = (floatOp S.emit_fadd args)
(* DEBUG *) val addg3 = diag "addg3" addg3

  (* Subtract the second argument from the first and store the result in the third *)
    fun subg3 args = (floatOp S.emit_fsub args)
(* DEBUG *) val subg3 = diag "subg3" subg3

  (* Multiply the first two arguments and store the result in the third *)
    fun mulg3 args = (floatOp S.emit_fmul args)
(* DEBUG *) val mulg3 = diag "mulg3" mulg3

  (* Divide the first argument by the second and store the result in the third *)
    fun divg3 args = (floatOp S.emit_fdiv args)
(* DEBUG *) val divg3 = diag "divg3" divg3

  (* Conditionally branch on the float values of two arguments. *)
    fun gbranch (cond, a, b, ImmedLab lab) = (
	    fetchReal (a, S.f0);
	    fetchReal (b, S.f2);
	    S.emit_fcmp (S.f0, S.f2);
	    S.emitFCondJmp (cond, lab))
(* DEBUG *) val gbranch = diag "gbranch" gbranch

    end (* local *)

    fun profile _ = ()

    val comment = S.comment

end (* functor SparcCM *)

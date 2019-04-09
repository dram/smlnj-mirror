(* bytecode.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories
 *
 * The CMachine for the SML/NJ bytecode interpreter.
 *
 *)

functor ByteCode (C : BC_CODER) : CMACHINE =
  struct

    fun impossible s = ErrorMsg.impossible("ByteCode." ^ s)
    fun badarg s = ErrorMsg.impossible(concat ["ByteCode.", s, ": bad argument"])

    val itow = Word.fromInt
    val wtoi = Word.toInt

(* +DEBUG
fun diag (s : string) f x =
	f x handle e =>
		(print "?exception "; print (General.exnName e);
		 print " in m68."; print s; print "\n";
		 raise e)
-DEBUG *)

    structure C' : sig
	type label_t
	datatype reg_t = GPR of int
	datatype fp_reg_t = FPR of int

	datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR
  		           | GEU | GTU | LTU | LEU

	val emitlong : int -> unit
	exception BadReal of string
	val realconst : string -> unit
	val emitstring : string -> unit

	val mark : unit -> unit
	val comment : string -> unit
      end = C
    open C'

    datatype EA
      = Direct of reg_t
      | FDirect of fp_reg_t
      | ExnPtr
      | VarPtr
      | Immed of int
      | ImmedLab of label_t

    val immed = Immed

  (* Dedicated registers *)
    val exnptr			= ExnPtr
    val varptr			= VarPtr
    val varptr_indexable	= false

    val arithtemps = []

  (* Other registers *)
    val standardarg     = Direct(GPR 0)
    val standardcont	= Direct(GPR 1)
    val standardclosure = Direct(GPR 2)
    val standardlink	= Direct(GPR 3)
    val miscregs	= map (fn i => Direct(GPR i)) [4,5,6,7,8,9,10,11,12,13,14]
    val floatregs	= map (fn i => FDirect(FPR i)) [0,1,2,3,4,5,6,7]
    val savedfpregs	= map (fn i => FDirect(FPR i)) [8,9,10,11,12,13,14,15]

  (* temporary register *)
    val tmpR = GPR 15
    val tmpEA = Direct tmpR

    val align = C.align

    fun newlabel () = ImmedLab(C.newLabel())
    fun define (ImmedLab l) = C.defineLabel l
      | define _ = badarg "define"
    fun emitlab (i, ImmedLab l) = C.emitLabel (i, l)
      | emitlab _ = badarg "emitlab"

    datatype immed_size = Immed16 | Immed20 | Immed32

    fun sizeOfImmed n = if (n < 0)
	  then if (n < ~32768)
	    then if (n < ~524288)
	      then Immed32
	      else Immed20
	    else Immed16
	  else if (32768 <= n)
	    then if (524288 <= n)
	      then Immed32
	      else Immed20
	    else Immed16

    fun loadImmed (dstR, im) = (case (sizeOfImmed im)
	   of Immed32 => C.emit_IMMED32(dstR, im)
	    | _ => C.emit_IMMED20(dstR, im)
	  (* end case *))

    fun addImmed (dstR, srcR, im) = (
	  case (dstR = srcR, sizeOfImmed im)
	   of (false, Immed16) => C.emit_ADDimmed(dstR, srcR, im)
	    | (false, _) => (loadImmed(tmpR, im); C.emit_ADD(dstR, srcR, tmpR))
	    | (true, Immed32) => (C.emit_IMMED32(tmpR, im); C.emit_ADD(dstR, srcR, tmpR))
	    | (true, _) => C.emit_INCR(dstR, im)
	  (* end case *))

    fun addImmedT (dstR, srcR, im) = (
	  case (sizeOfImmed im)
	   of Immed16 => C.emit_ADDTimmed(dstR, srcR, im)
	    | _ => (loadImmed(tmpR, im); C.emit_ADDT(dstR, srcR, tmpR))
	  (* end case *))

    fun move (Direct r1, Direct r2) =
	  if (r1 = r2) then () else C.emit_MOVE(r2, r1)
      | move (Immed im, Direct r) = loadImmed(r, im)
      | move (ImmedLab l, Direct r) = C.emitCodeAddr (r, l, 0)
      | move (FDirect r1, FDirect r2) =
	  if (r1 = r2) then () else C.emit_FMOVE(r2, r1)
      | move (ExnPtr, Direct r) = C.emit_GETEXN r
      | move (Direct r, ExnPtr) = C.emit_PUTEXN r
      | move (VarPtr, Direct r) = C.emit_GETVAR r
      | move (Direct r, VarPtr) = C.emit_PUTVAR r
      | move _ = badarg "move"

    fun testLimit () = ()
    fun decLimit n = () (* for polling *)
    fun checkLimit (nbytes, resume, Immed mask, _, _) = let
	  val nwords = (nbytes+3) div 4
	  in
	    if (nwords < 256)
	      then C.emit_LIMITCHK (nwords, mask)
	      else C.emit_LIMITCHK2 (nwords, mask)
	  end
      | checkLimit _ = badarg "checkLimit"

    fun beginStdFn _ = ()

    fun jmp (Direct r) = C.emit_JMPind r
      | jmp (ImmedLab l) = C.emitJump l
      | jmp _ = badarg "jmp"

    fun allocField (Direct r, CPS.OFFp 0) = C.emit_ALLOC r
      | allocField (Direct r, path) = let
	    fun projPath (CPS.OFFp 0, pl) = C.emit_ALLOCpath0 (r, rev pl)
	      | projPath (CPS.OFFp n, pl) = C.emit_ALLOCpath (r, rev(n::pl))
	      | projPath (CPS.SELp(i, p), pl) = projPath (p, i::pl)
	    in
	      projPath (path, [])
	    end
      | allocField (Immed n, CPS.OFFp 0) = C.emit_ALLOCimmed n
      | allocField (ImmedLab l, CPS.OFFp n) = C.emitAllocAddr(l, n)
      | allocField _ = impossible "allocField"
    val allocRecord = app allocField

    fun record ([(Immed desc, CPS.OFFp 0), a, b], Direct dstR) = let
	  fun alloc () = (
		C.emit_BEGIN desc; allocField a; allocField b; C.emit_END dstR)
	  in
	    if (desc <> System.Tags.desc_pair)
	      then alloc()
	      else (case (a, b)
		 of ((Direct r1, CPS.OFFp 0), (Direct r2, CPS.OFFp 0)) =>
		      C.emit_PAIR(dstR, r1, r2)
		  | ((Immed n, CPS.OFFp 0), (Direct r, CPS.OFFp 0)) => (
		      case (sizeOfImmed n)
		       of Immed16 => C.emit_PAIRimmedl(dstR, n, r)
			| _ => (loadImmed(tmpR, n); C.emit_PAIR(dstR, tmpR, r))
		      (* end of case *))
		  | ((Direct r, CPS.OFFp 0), (Immed n, CPS.OFFp 0)) => (
		      case (sizeOfImmed n)
		       of Immed16 => C.emit_PAIRimmedr(dstR, r, n)
			| _ => (loadImmed(tmpR, n); C.emit_PAIR(dstR, r, tmpR))
		      (* end of case *))
		  | ((Immed n1, CPS.OFFp 0), (Immed n2, CPS.OFFp 0)) => (
		      case (sizeOfImmed n1)
		       of Immed16 => (
			    loadImmed(tmpR, n2);
			    C.emit_PAIRimmedl(dstR, n1, tmpR))
			| _ => (case (sizeOfImmed n2)
			     of Immed16 => (
				  loadImmed(tmpR, n1);
				  C.emit_PAIRimmedr(dstR, tmpR, n2))
			      | _ => alloc()
			    (* end case *))
		      (* end of case *))
		  | _ => alloc()
		(* end case *))
	  end
      | record ((Immed desc, CPS.OFFp 0)::vl, Direct dstR) = (
	  C.emit_BEGIN desc;
	  allocRecord vl;
	  C.emit_END dstR)
      | record ([(Direct tagR, CPS.OFFp 0), (Direct argR, CPS.OFFp 0)], Direct dstR) =
	  C.emit_SPECIAL(dstR, tagR, argR)
      | record ([(Direct tagR, CPS.OFFp 0), (Immed arg, CPS.OFFp 0)], Direct dstR) = (
	  loadImmed (tmpR, arg);
	  C.emit_SPECIAL(dstR, tagR, tmpR))
      | record _ = badarg "record"

    fun recordcont _ = badarg "recordcont"

 (* makes a new record that contains only floating-point values. Same
  * convention as record. The first arg is the descriptor
  *)
    fun fprecord _ = badarg "fprecord"

  (* recordStore(x, y, alwaysBoxed) records a store operation into mem[x+2*(y-1)].
   * The flag alwaysBoxed is true if the value stored is guaranteed to be boxed.
   *)
(* NOTE: eventually we can optimize the case where alwaysBoxed = false *)
    fun recordStore (Direct r, Immed 1, alwaysBoxed) = C.emit_MARKSTORE (r, alwaysBoxed)
      | recordStore (Direct r, Immed indx, alwaysBoxed) = let
	  val scaledIndex = 2*(indx-1)
	  in
	    case (sizeOfImmed scaledIndex)
	     of Immed16 => C.emit_MARKSTOREoffset (r, scaledIndex, alwaysBoxed)
	      | _ => (
		loadImmed (tmpR, indx);
		C.emit_MARKSTOREindex (r, tmpR, alwaysBoxed))
	    (* end case *)
	  end
      | recordStore (Direct r1, Direct r2, alwaysBoxed) =
	  C.emit_MARKSTOREindex (r1, r2, alwaysBoxed)
      | recordStore _ = badarg "recordStore"
(*** STOREPTR
    fun recordStore (x, y, alwaysBoxed) = (
	  C.emit_BEGINSP (if alwaysBoxed then 1 else 0);
	  allocRecord [(x, CPS.OFFp 0), (y, CPS.OFFp 0)];
	  C.emit_ENDSP())
***)

    fun select (0, Direct srcR, Direct dstR) = C.emit_LOAD(dstR, srcR)
      | select (i, Direct srcR, Direct dstR) = C.emit_LOADoffset(dstR, srcR, 4*i)
      | select (i, ImmedLab lab, Direct dstR) = (
	  C.emitCodeAddr (tmpR, lab, 4*i); C.emit_LOAD(dstR, tmpR))
      | select _ = badarg "select"

    fun loadpseudo _ = badarg "loadpseudo"
    fun storepseudo _ = badarg "storepseudo"

    fun offset (i, Direct(srcR), Direct(dstR)) = addImmed (dstR, srcR, 4*i)
      | offset (i, ImmedLab lab, Direct dstR) = C.emitCodeAddr (dstR, lab, 4*i)
      | offset _ = badarg "offset"

    fun fetchindexb (Direct srcR, Direct dstR, Direct indxR) =
	  C.emit_BLOADindex (dstR, srcR, indxR)
      | fetchindexb (Direct srcR, Direct dstR, Immed 0) =
	  C.emit_BLOAD (dstR, srcR)
      | fetchindexb (Direct srcR, Direct dstR, Immed indx) = (
	  case (sizeOfImmed indx)
	   of Immed16 => C.emit_BLOADoffset (dstR, srcR, indx)
	    | _ => (loadImmed(tmpR, indx); C.emit_BLOADindex (dstR, srcR, tmpR))
	  (* end case *))
      | fetchindexb _ = badarg "fetchindexb"

    fun storeindexb (Immed src, Direct dstR, indx) = let
	  val reg = (case indx
		 of (Immed 0) => dstR
		  | (Immed i) => (addImmed (tmpR, dstR, i); tmpR)
		  | (Direct indxR) => (C.emit_ADD(tmpR, dstR, indxR); tmpR)
		  | _ => impossible "storeindexb: bad index"
		(* end case *))
	  in
	    C.emit_BSTOREimmed(reg, src)
	  end
      | storeindexb (Direct srcR, Direct dstR, Direct indxR) =
	  C.emit_BSTOREindex (dstR, indxR, srcR)
      | storeindexb (Direct srcR, Direct dstR, Immed 0) =
	  C.emit_BSTORE (dstR, srcR)
      | storeindexb (Direct srcR, Direct dstR, Immed indx) = (
	  case (sizeOfImmed indx)
	   of Immed16 => C.emit_BSTOREoffset (dstR, indx, srcR)
	    | _ => (loadImmed(tmpR, indx); C.emit_BSTOREindex (dstR, tmpR, srcR))
	  (* end case *))
      | storeindexb _ = badarg "storeindexb"

    fun jmpindexb (ImmedLab lab, Direct r) = C.emitJumpIndex(lab, r)
      | jmpindexb _ = badarg "jmpindexb"

  (* fetchindexl(x,y,z) fetches a word:  y <- mem[x+2*(z-1)] *)
    fun fetchindexl (Direct srcR, Direct dstR, Direct indxR) =
	  C.emit_LOADindex (dstR, srcR, indxR)
      | fetchindexl (Direct srcR, Direct dstR, Immed 1) =
	  C.emit_LOAD (dstR, srcR)
      | fetchindexl (Direct srcR, Direct dstR, Immed indx) = let
	  val scaledIndex = 2*(indx-1)
	  in
	    case (sizeOfImmed scaledIndex)
	     of Immed16 => C.emit_LOADoffset (dstR, srcR, scaledIndex)
	      | _ => (loadImmed(tmpR, indx); C.emit_LOADindex (dstR, srcR, tmpR))
	    (* end case *)
	  end
      | fetchindexl (ImmedLab lab, Direct dstR, Direct indxR) = (
	  C.emitCodeAddr (tmpR, lab, 0);
	  C.emit_LOADindex (dstR, tmpR, indxR))
      | fetchindexl _ = badarg "fetchindexl"

  (* storeindexl(x,y,z) stores a word:  mem[y+2*(z-1)] <- x *)
    fun storeindexl (Immed src, Direct dstR, indx) = (
	  case (sizeOfImmed src)
	   of Immed32 => (loadImmed(tmpR, src); storeindexl(tmpEA, Direct dstR, indx))
	    | _ => let
		val reg = (case indx
		       of (Immed 1) => dstR
			| (Immed i) => (addImmed (tmpR, dstR, 2*(i-1)); tmpR)
			| (Direct indxR) => (
			    C.emit_ADDimmed(tmpR, indxR, ~1);
			    C.emit_ADD(tmpR, tmpR, tmpR);
			    C.emit_ADD(tmpR, dstR, tmpR);
			    tmpR)
			| _ => impossible "storeindex: bad index"
		      (* end case *))
		in
		  C.emit_STOREimmed(reg, src)
		end)
      | storeindexl (ImmedLab lab, dstEA, indxEA) = (
	  C.emitCodeAddr(tmpR, lab, 0);
	  storeindexl (tmpEA, dstEA, indxEA))
      | storeindexl (Direct srcR, Direct dstR, Direct indxR) =
	  C.emit_STOREindex (dstR, indxR, srcR)
      | storeindexl (Direct srcR, Direct dstR, Immed 1) = C.emit_STORE (dstR, srcR)
      | storeindexl (Direct srcR, Direct dstR, Immed indx) = let
	  val scaledIndex = 2*(indx-1)
	  in
	    case (sizeOfImmed scaledIndex)
	     of Immed16 => C.emit_STOREoffset (dstR, scaledIndex, srcR)
	      | _ => (loadImmed(tmpR, indx); C.emit_STOREindex (dstR, tmpR, srcR))
	    (* end case *)
	  end
      | storeindexl _ = badarg "storeindexl"

  (* fetchindexd(x,y,z):  y <- mem[x+4*(z-1)] *)
    fun fetchindexd (Direct srcR, FDirect dstR, Direct indxR) =
	  C.emit_FLOADindex (dstR, srcR, indxR)
      | fetchindexd (Direct srcR, FDirect dstR, Immed 1) =
	  C.emit_FLOAD (dstR, srcR)
      | fetchindexd (Direct srcR, FDirect dstR, Immed indx) = let
	  val scaledIndex = 4*(indx-1)
	  in
	    case (sizeOfImmed scaledIndex)
	     of Immed16 => C.emit_FLOADoffset (dstR, srcR, scaledIndex)
	      | _ => (loadImmed(tmpR, indx); C.emit_FLOADindex (dstR, srcR, tmpR))
	    (* end case *)
	  end
      | fetchindexd _ = badarg "fetchindexd"

  (* storeindexd(x,y,z):  mem[y+4*(z-1)] <- x *)
    fun storeindexd (FDirect srcR, Direct dstR, Direct indxR) =
	  C.emit_FSTOREindex (dstR, indxR, srcR)
      | storeindexd (FDirect srcR, Direct dstR, Immed 1) = C.emit_FSTORE (dstR, srcR)
      | storeindexd (FDirect srcR, Direct dstR, Immed indx) =  let
	  val scaledIndex = 4*(indx-1)
	  in
	    case (sizeOfImmed scaledIndex)
	     of Immed16 => C.emit_FSTOREoffset (dstR, scaledIndex, srcR)
	      | _ => (loadImmed(tmpR, indx); C.emit_FSTOREindex (dstR, tmpR, srcR))
	    (* end case *)
	  end
      | storeindexd _ = badarg "storeindexd"

    fun arithImmed im = (case (sizeOfImmed im)
	   of Immed16 => Immed im
	    | _ => (loadImmed(tmpR, im); tmpEA)
	  (* end case *))
    fun adjustArgs (Immed im, srcR as (Direct _), dst) = (srcR, arithImmed im, dst)
      | adjustArgs (srcR as (Direct _), Immed im, dst) = (srcR, arithImmed im, dst)
      | adjustArgs args = args

    fun ashl (Direct cntR, Direct srcR, Direct dstR) = C.emit_ASHL (dstR, srcR, cntR)
      | ashl (Immed 1, Direct srcR, Direct dstR) = C.emit_ASHL1 (dstR, srcR)
      | ashl (Immed cnt, Direct srcR, Direct dstR) = C.emit_ASHLimmed (dstR, srcR, cnt)
      | ashl (Direct cntR, Immed im, Direct dstR) = (
	  loadImmed (dstR, im);
	  C.emit_ASHL (dstR, dstR, cntR))
      | ashl (Immed cnt, Immed src, Direct dstR) =
	  loadImmed (dstR, wtoi (Word.<<(itow src, itow cnt)))
      | ashl _ = badarg "ashl"

    fun ashr (Direct cntR, Direct srcR, Direct dstR) = C.emit_ASHR (dstR, srcR, cntR)
      | ashr (Immed 1, Direct srcR, Direct dstR) = C.emit_ASHR1(dstR, srcR)
      | ashr (Immed cnt, Direct srcR, Direct dstR) = C.emit_ASHRimmed (dstR, srcR, cnt)
      | ashr (Direct cntR, Immed im, Direct dstR) = (
	  loadImmed (dstR, im);
	  C.emit_ASHR (dstR, dstR, cntR))
      | ashr (Immed cnt, Immed src, Direct dstR) =
	  loadImmed (dstR, wtoi (Word.~>>(itow src, itow cnt)))
      | ashr _ = badarg "ashl"

    fun orb args = (case (adjustArgs args)
	   of (Direct srcR1, Direct srcR2, Direct dstR) => C.emit_ORB(dstR, srcR1, srcR2)
	    | (Direct srcR, Immed 1, Direct dstR) => C.emit_ORB1(dstR, srcR)
	    | (Direct srcR, Immed i, Direct dstR) => C.emit_ORBimmed(dstR, srcR, i)
	    | _ => badarg "orb"
	  (* end case *))

    fun xorb args = (case (adjustArgs args)
	   of (Direct srcR1, Direct srcR2, Direct dstR) =>
		C.emit_XORB(dstR, srcR1, srcR2)
	    | (Direct srcR, Immed i, Direct dstR) => C.emit_XORBimmed(dstR, srcR, i)
	    | _ => badarg "xorb"
	  (* end case *))

    fun andb args = (case (adjustArgs args)
	   of (Direct srcR1, Direct srcR2, Direct dstR) =>
		C.emit_ANDB(dstR, srcR1, srcR2)
	    | (Direct srcR, Immed i, Direct dstR) => C.emit_ANDBimmed(dstR, srcR, i)
	    | _ => badarg "andb"
	  (* end case *))

    fun notb (Direct srcR, Direct dstR) = C.emit_NOTB(dstR, srcR)
      | notb _ = badarg "notb"

    fun add (Direct srcR1, Direct srcR2, Direct dstR) = C.emit_ADD(dstR, srcR1, srcR2)
      | add (Direct srcR, Immed i, Direct dstR) = addImmed(dstR, srcR, i)
      | add (Immed i, Direct srcR, Direct dstR) = addImmed(dstR, srcR, i)
      | add _ = badarg "add"

  (* sub(a, b, c) produces code for c := b-a.
   * NOTE: SUB and SUBimmed treat the order of their arguments differently.
   *)
    fun sub (Direct srcR1, Direct srcR2, Direct dstR) = C.emit_SUB(dstR, srcR2, srcR1)
      | sub (Direct srcR, Immed i, Direct dstR) = (
	  case (sizeOfImmed i)
	   of Immed16 => C.emit_SUBimmed(dstR, srcR, i)
	    | _ => (loadImmed(tmpR, i); C.emit_SUB(dstR, tmpR, srcR))
	  (* end case *))
      | sub (Immed i, Direct srcR, Direct dstR) = addImmed(dstR, srcR, ~i)
      | sub _ = badarg "sub"

    fun addt (Direct srcR1, Direct srcR2, Direct dstR) = C.emit_ADDT(dstR, srcR1, srcR2)
      | addt (Direct srcR, Immed i, Direct dstR) = addImmedT(dstR, srcR, i)
      | addt (Immed i, Direct srcR, Direct dstR) = addImmedT(dstR, srcR, i)
      | addt (Immed i1, Immed i2, Direct dstR) = (
	(* This should only occur when we need to build a constant larger than
	 * 2^29.  Note, we assume that "b" is tagged (see "cps/generic.sml").
	 *)
	  case (sizeOfImmed i1, sizeOfImmed i2)
	   of (Immed16, _) => (loadImmed(tmpR, i2); C.emit_ADDimmed(dstR, tmpR, i1))
	    | (_, Immed16) => (loadImmed(tmpR, i1); C.emit_ADDimmed(dstR, tmpR, i2))
	    | _ => (
		loadImmed(tmpR, i1); loadImmed(dstR, i2);
		C.emit_ADD(dstR, tmpR, dstR))
	  (* end case *))
      | addt _ = badarg "addt"

    fun subt (Direct srcR1, Direct srcR2, Direct dstR) = C.emit_SUBT(dstR, srcR2, srcR1)
      | subt (Direct srcR, Immed i, Direct dstR) = (
	  case (sizeOfImmed i)
	   of Immed16 => C.emit_SUBTimmed(dstR, srcR, i)
	    | _ => (loadImmed(tmpR, i); C.emit_SUBT(dstR, tmpR, srcR))
	  (* end case *))
      | subt (Immed i, Direct srcR, Direct dstR) = addImmedT(dstR, srcR, ~i)
      | subt _ = badarg "subt"

    fun mult (Direct src, Direct dstR) = C.emit_MULT(dstR, src)
      | mult (Immed src, Direct dstR) = (case (sizeOfImmed src)
	   of Immed32 => (loadImmed(tmpR, src); C.emit_MULT(dstR, tmpR))
	    | _ => C.emit_MULTimmed(dstR, src)
	  (* end case *))
      | mult _ = badarg "mult"

    fun divt (Direct src, Direct dstR) = C.emit_DIVT(dstR, src)
      | divt (Immed src, Direct dstR) = (case (sizeOfImmed src)
	   of Immed32 => (loadImmed(tmpR, src); C.emit_DIVT(dstR, tmpR))
	    | _ => C.emit_DIVTimmed(dstR, src)
	  (* end case *))
      | divt _ = badarg "divt"

    (* word32 support *)
    val immed32 = Immed o Word32.toInt
    fun addu _ = ()
    fun subu _ = ()
    fun mulu _ = ()
    fun divtu _ = ()
    fun lshr _ = ()

    fun bbs (Immed 0, Direct r, ImmedLab lab) = C.emitJumpUnboxed(r, lab)
      | bbs _ = badarg "bbs"

    fun ibranch (cond, Direct r1, Direct r2, ImmedLab lab) =
	  C.emitCondJmp (cond, r1, r2, lab)
      | ibranch (cond, Immed a, Direct r2, ImmedLab lab) = (
	  loadImmed(tmpR, a); C.emitCondJmp (cond, tmpR, r2, lab))
      | ibranch (cond, Direct r1, Immed b, ImmedLab lab) = (
	  loadImmed(tmpR, b); C.emitCondJmp (cond, r1, tmpR, lab))
      | ibranch _ = ()			(* word32 support *)

    fun rangeChk (Direct a, Direct hi, ImmedLab lab) = C.emitRangeChk(a, hi, lab)
      | rangeChk (Immed a, Direct hi, ImmedLab lab) = (
	  loadImmed(tmpR, a); C.emitRangeChk(tmpR, hi, lab))
      | rangeChk (Direct a, Immed hi, ImmedLab lab) = (
	  loadImmed(tmpR, hi); C.emitRangeChk(a, tmpR, lab))
      | rangeChk _ = badarg "rangeChk"

    fun faddd (FDirect R1, FDirect R2, FDirect dstR) = C.emit_FADD(dstR, R1, R2)
      | faddd _ = badarg "faddd"
    fun fsubd (FDirect R1, FDirect R2, FDirect dstR) = C.emit_FSUB(dstR, R1, R2)
      | fsubd _ = badarg "fsubd"
    fun fmuld (FDirect R1, FDirect R2, FDirect dstR) = C.emit_FMUL(dstR, R1, R2)
      | fmuld _ = badarg "fmuld"
    fun fdivd (FDirect R1, FDirect R2, FDirect dstR) = C.emit_FDIV(dstR, R1, R2)
      | fdivd _ = badarg "fdivd"
    fun fnegd (FDirect srcR, FDirect dstR) = C.emit_FNEG(dstR, srcR)
      | fnegd _ = badarg "fnegd"
    fun fabsd (FDirect srcR, FDirect dstR) = C.emit_FABS(dstR, srcR)
      | fabsd _ = badarg "fabsd"
    fun cvti2d (Direct srcR, FDirect dstR) = C.emit_FLOAT(dstR, srcR)
      | cvti2d _ = badarg "cvti2d"

    fun fbranchd (cond, FDirect r1, FDirect r2, ImmedLab lab) =
	  badarg "fbranchd -- 1" (* C.emitFCondJmp (cond, r1, r2, lab) *)
      | fbranchd _ = badarg "fbranchd"

    fun storefloat (FDirect srcR, Direct dstR) = C.emit_FALLOC(dstR, srcR)
      | storefloat _ = badarg "storefloat"

    fun loadfloat (Direct srcR, FDirect dstR) = C.emit_FLOAD(dstR, srcR)
      | loadfloat (ImmedLab lab, FDirect dstR) = C.emitFConst(dstR, lab)
      | loadfloat _ = badarg "loadfloat"

  end (* ByteCode *)

(*
 * $Log: bytecode.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:28  george
 *   Version 109.24
 *
 *)

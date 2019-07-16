(* bcas.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories
 *
 * Bytecode assembly code emitter for SML/NJ bytecode interpreter.
 *
 *)


structure BCAssem =
  struct
    val outfile = ref TextIO.stdOut
  end

structure BCAs : BC_CODER =
  struct

    type label_t = string

    datatype reg_t = GPR of int
    datatype fp_reg_t = FPR of int

    datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR
      		       | GEU | GTU | LTU | LEU
    fun pr s = TextIO.output(!BCAssem.outfile, s)

    fun emitGPR (GPR r) = pr("r" ^ Int.toString r)
    fun emitFPR (FPR r) = pr("f" ^ Int.toString r)
    fun emitImmed (im : int) = pr("#" ^ Int.toString im)
    fun emitOffset 0 = ()
      | emitOffset i = if (i < 0)
	  then pr("-" ^ Int.toString(~i)) else pr("+" ^ Int.toString i)
    fun comma () = pr ","
    fun nl () = pr "\n"
    fun emitOp instr = (pr "\t"; pr instr; pr "\t")
    fun emitAddr_RR (r1, r2) = (pr "["; emitGPR r1; pr " + "; emitGPR r2; pr "]")
    fun emitAddr_RI (r, off) = (pr "["; emitGPR r; emitOffset off; pr "]")
    fun emitAddr_R r = (pr "["; emitGPR r; pr "]")
    local
      fun emitFn f instr arg = (emitOp instr; f arg; nl())
      fun emitArg_RRR (r1, r2, r3) = (
	    emitGPR r1; comma(); emitGPR r2; comma(); emitGPR r3)
      fun emitArg_RRI (r1, r2, im) = (
	    emitGPR r1; comma(); emitGPR r2; comma(); emitImmed im)
      fun emitArg_RIR (r1, im, r2) = (
	    emitGPR r1; comma(); emitImmed im; comma(); emitGPR r2)
      fun emitArg_RR (r1, r2) = (emitGPR r1; comma(); emitGPR r2)
      fun emitArg_RI (r1, im) = (emitGPR r1; comma(); emitImmed im)
      val emitArg_R = emitGPR
      val emitArg_I = emitImmed
      fun emitArg_FFF (r1, r2, r3) = (
	    emitFPR r1; comma(); emitFPR r2; comma(); emitFPR r3)
      fun emitArg_FF (r1, r2) = (emitFPR r1; comma(); emitFPR r2)
      fun emitArg_FR (r1, r2) = (emitFPR r1; comma(); emitGPR r2)
      fun emitArg_RF (r1, r2) = (emitGPR r1; comma(); emitFPR r2)
      fun emitMask m = let
	    fun lp (0w0, _, _) = ()
	      | lp (i, r, flg) = if (Word.andb(i, 0w1) <> 0w0)
		  then (
		    if flg then comma() else ();
		    emitGPR(GPR r);
		    lp (Word.>>(i, 0w1), r+1, true))
		  else lp (Word.>>(i, 0w1), r+1, flg)
	    in
	      pr "{"; lp(Word.fromInt m, 0, false); pr "}"
	    end
    in
    val emit_RRR = emitFn emitArg_RRR
    val emit_RRI = emitFn emitArg_RRI
    val emit_RIR = emitFn emitArg_RIR
    val emit_RR = emitFn emitArg_RR
    val emit_RI = emitFn emitArg_RI
    val emit_R = emitFn emitArg_R
    val emit_I = emitFn emitArg_I
    fun emit_IM instr (im, mask) = (
	  emitOp instr; emitImmed im; comma(); emitMask mask; nl())
    val emit_FFF = emitFn emitArg_FFF
    val emit_FF = emitFn emitArg_FF
    val emit_FR = emitFn emitArg_FR
    val emit_RF = emitFn emitArg_RF
    fun emitLoad_RR instr (r1, r2) = (
	  emitOp instr; emitGPR r1; comma(); emitAddr_R r2; nl())
    fun emitLoad_RRR instr (r1, r2, r3) = (
	  emitOp instr; emitGPR r1; comma(); emitAddr_RR(r2, r3); nl())
    fun emitLoad_RRI instr (r1, r2, im) = (
	  emitOp instr; emitGPR r1; comma(); emitAddr_RI(r2, im); nl())
    fun emitStore_RR instr (r1, r2) = (
	  emitOp instr; emitAddr_R r1; comma(); emitGPR r2; nl())
    fun emitStore_RRR instr (r1, r2, r3) = (
	  emitOp instr; emitAddr_RR(r1, r2); comma(); emitGPR r3; nl())
    fun emitStore_RIR instr (r1, im, r2) = (
	  emitOp instr; emitAddr_RI(r1, im); comma(); emitGPR r2; nl())
    fun emitStore_RI instr (r, im) = (
	  emitOp instr; emitAddr_R r; comma(); emitImmed im; nl())
    end

    fun align () = pr "\t.align\n"

    local
      val cnt = ref 0
    in
    fun newLabel () = (cnt := !cnt + 1; "L" ^ Int.toString (!cnt))
    end

    fun defineLabel lab = pr(lab ^ ":\n")

    fun emitLabel (0, lab) = (emitOp ".long"; pr lab; pr "-.\n")
      | emitLabel (n, lab) = (emitOp ".long"; pr "("; pr lab; emitOffset n; pr ")-.\n")

    local
      fun strEscape 0 = "\\000"
	| strEscape i = if (i < 10)
	      then "\\00" ^ Int.toString(i)
	    else if (i < 100)
	      then "\\0" ^ Int.toString(i)
	      else "\\" ^ Int.toString(i)
      fun c_char #"\n" = "\\n"
	| c_char #"\t" = "\\t"
	| c_char #"\\" = "\\\\"
	| c_char #"\"" = "\\\""
	| c_char c = if (c < #" ") then strEscape(Char.ord c) else String.str c
      fun asciiStr s = concat (map c_char (explode s))
    in
    fun emitstring s = (pr "\t.string \""; pr(asciiStr s); pr "\"\n")
    end

    exception BadReal of string

    fun realconst s = (emitOp "\t.double"; pr s; nl())

    fun emitlong (i : int) = (emitOp ".long"; pr(Int.toString i); nl())

    fun mark () = let
	  val lab = newLabel()
	  in
	    pr lab; pr ":\t.long\tMAKE_DESC((";
	    pr lab; pr "-base)/4+1,tag_backptr)\n"
	  end

    val comment = pr

  (* Psuedo instructions.  These are span dependent. *)
    fun emitJump lab = (emitOp "JMP"; pr lab; nl())
    fun emitJumpIndex (lab, r) = (emitOp "JMPindex"; pr lab; pr "+"; emitGPR r; nl())
    fun emitJumpUnboxed (r, lab) = (
	  emitOp "JMPUNBOXED"; emitGPR r; comma(); pr lab; nl())
    fun emitRangeChk (r1, r2, lab) = (
	  emitOp "JMPINRNG"; emitGPR r1; comma(); emitGPR r2; comma(); pr lab; nl())

    local
      fun emitCC NEQ = "JMPNE"
	| emitCC EQL = "JMPEQ"
	| emitCC LEQ = "JMPLE"
	| emitCC GEQ = "JMPGE"
	| emitCC LSS = "JMPLT"
	| emitCC GTR = "JMPGT"
    in
    fun emitCondJmp (cc, r1, r2, lab) = (
	  emitOp (emitCC cc); emitGPR r1; comma();
	  emitGPR r2; comma(); pr lab; nl())
    fun emitFCondJmp (cc, r1, r2, lab) = (
	  emitOp ("F" ^ emitCC cc); emitFPR r1; comma();
	  emitFPR r2; comma(); pr lab; nl())
    end

    fun emitFConst (r, lab) = (emitOp "FCONST"; emitFPR r; comma(); pr lab; nl())

    fun emitCodeAddr (r, lab, 0) = (
	  emitOp "CODEADDR"; emitGPR r; comma(); pr lab; nl())
      | emitCodeAddr (r, lab, n) = (
	  emitOp "CODEADDR"; emitGPR r; comma(); pr lab; emitOffset n; nl())

    fun emitAllocAddr (lab, n) = (emitOp "  ALLOCaddr"; pr lab; emitOffset n; nl())

    val emit_IMMED32 = emit_RI "IMMED32"
    val emit_IMMED20 = emit_RI "IMMED20"
    val emit_MOVE = emit_RR "MOVE"
    val emit_FMOVE = emit_FF "FMOVE"
    val emit_GETEXN = emit_R "GETEXN"
    val emit_PUTEXN = emit_R "PUTEXN"
    val emit_GETVAR = emit_R "GETVAR"
    val emit_PUTVAR = emit_R "PUTVAR"
    val emit_LIMITCHK = emit_IM "LIMITCHK"
    val emit_LIMITCHK2 = emit_IM "LIMITCHK2"
    val emit_JMPind = emit_R "JMPind"		
    val emit_BEGIN = emit_I "BEGIN"
    val emit_ALLOC = emit_R "  ALLOC"
    local
      fun emitPath [] = ()
	| emitPath [p : int] = pr(Int.toString p)
	| emitPath (p::r) = (pr(Int.toString p); comma(); emitPath r)
    in
    fun emit_ALLOCpath (base, path) = (
	  emitOp "  ALLOCpath"; emitGPR base; comma();
	  pr "["; emitPath path; pr "]\n")
    fun emit_ALLOCpath0 (base, path) = (
	  emitOp "  ALLOCpath0"; emitGPR base; comma();
	  pr "["; emitPath path; pr "]\n")
    end
    val emit_ALLOCimmed = emit_I "  ALLOCimmed"
    val emit_END = emit_R "END"
    val emit_PAIR = emit_RRR "PAIR"
    val emit_PAIRimmedl = emit_RIR "PAIRimmedl"
    val emit_PAIRimmedr = emit_RRI "PAIRimmedr"
    val emit_ALLOCFRAME = emit_RI "ALLOCFRAME"
    val emit_FREEFRAME = emit_RI "FREEFRAME"
    fun emit_MARKSTORE (r, alwaysBoxed) = (
	  emitOp "MARKSTORE"; emitAddr_R r;
	  if alwaysBoxed then pr ",boxed\n" else nl())
    fun emit_MARKSTOREindex (r1, r2, alwaysBoxed) = (
	  emitOp "MARKSTOREindex"; emitAddr_RR(r1, r2);
	  if alwaysBoxed then pr ",boxed\n" else nl())
    fun emit_MARKSTOREoffset (r, i, alwaysBoxed) = (
	  emitOp "MARKSTOREoffse"; emitAddr_RI(r, i);
	  if alwaysBoxed then pr ",boxed\n" else nl())
    val emit_LOAD = emitLoad_RR "LOAD"
    val emit_LOADindex = emitLoad_RRR "LOADindex"
    val emit_LOADoffset = emitLoad_RRI "LOADoffset"
    val emit_STORE = emitStore_RR "STORE"
    val emit_STOREindex = emitStore_RRR "STOREindex"
    val emit_STOREoffset = emitStore_RIR "STOREoffset"
    val emit_STOREimmed = emitStore_RI "STOREimmed"
    val emit_BLOAD = emitLoad_RR "BLOAD"
    val emit_BLOADindex = emitLoad_RRR "BLOADindex"
    val emit_BLOADoffset = emitLoad_RRI "BLOADoffset"
    val emit_BSTORE = emitStore_RR "BSTORE"
    val emit_BSTOREindex = emitStore_RRR "BSTOREindex"
    val emit_BSTOREoffset = emitStore_RIR "BSTOREoffset"
    val emit_BSTOREimmed = emitStore_RI "BSTOREimmed"
    val emit_ASHL = emit_RRR "ASHL"
    val emit_ASHL1 = emit_RR "ASHL1"
    val emit_ASHLimmed = emit_RRI "ASHLimmed"
    val emit_ASHR = emit_RRR "ASHR"
    val emit_ASHR1 = emit_RR "ASHR1"
    val emit_ASHRimmed = emit_RRI "ASHRimmed"
    val emit_ORB = emit_RRR "ORB"
    val emit_ORB1 = emit_RR "ORB1"
    val emit_ORBimmed = emit_RRI "ORBimmed"
    val emit_XORB = emit_RRR "XORB"
    val emit_XORBimmed = emit_RRI "XORBimmed"
    val emit_ANDB = emit_RRR "ANDB"
    val emit_ANDBimmed = emit_RRI "ANDBimmed"
    val emit_NOTB = emit_RR "NOTB"
    val emit_ADD = emit_RRR "ADD"
    val emit_ADDimmed = emit_RRI "ADDimmed"
    val emit_INCR = emit_RI "INCR"
    val emit_ADDT = emit_RRR "ADDT"
    val emit_ADDTimmed = emit_RRI "ADDTimmed"
    val emit_SUB = emit_RRR "SUB"
    val emit_SUBimmed = emit_RRI "SUBimmed"
    val emit_SUBT = emit_RRR "SUBT"
    val emit_SUBTimmed = emit_RRI "SUBTimmed"
    val emit_MULT = emit_RR "MULT"
    val emit_MULTimmed = emit_RI "MULTimmed"
    val emit_DIVT = emit_RR "DIVT"
    val emit_DIVTimmed = emit_RI "DIVTimmed"

    fun emit_FLOAD (r1, r2) = (emitOp "FLOAD"; emitFPR r1; comma(); emitAddr_R r2; nl())
    fun emit_FLOADindex (r1, r2, r3) = (
	  emitOp "FLOADindex"; emitFPR r1; comma(); emitAddr_RR(r2, r3); nl())
    fun emit_FLOADoffset (r1, r2, im)= (
	  emitOp "FLOADoffset"; emitFPR r1; comma(); emitAddr_RI(r2, im); nl())
    fun emit_FSTORE (r1, r2) = (
	  emitOp "FSTORE"; emitAddr_R r1; comma(); emitFPR r2; nl())
    fun emit_FSTOREindex (r1, r2, r3) = (
	  emitOp "FSTOREindex"; emitAddr_RR(r1, r2); comma(); emitFPR r3; nl())
    fun emit_FSTOREoffset (r1, im, r2) = (
	  emitOp "FSTOREoffset"; emitAddr_RI(r1, im); comma(); emitFPR r2; nl())
    val emit_FADD = emit_FFF "FADD"
    val emit_FSUB = emit_FFF "FSUB"
    val emit_FMUL = emit_FFF "FMUL"
    val emit_FDIV = emit_FFF "FDIV"
    val emit_FNEG = emit_FF "FNEG"
    val emit_FABS = emit_FF "FABS"
    val emit_FLOAT = emit_FR "FLOAT"
    val emit_FALLOC = emit_RF "FALLOC"

  (* allocate a two-word "special" cell *)
    val emit_SPECIAL = emit_RRR "SPECIAL"

  end (* BCAs *)

(*
 * $Log: bcas.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:28  george
 *   Version 109.24
 *
 *)

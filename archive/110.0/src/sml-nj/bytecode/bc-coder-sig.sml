(* bc-coder.sig
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories
 *
 *)

signature BC_CODER =
  sig

    type label_t
    datatype reg_t = GPR of int
    datatype fp_reg_t = FPR of int

    datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR
	   	       | GEU | GTU | LTU | LEU
    val align	: unit -> unit

    val newLabel	: unit -> label_t
    val defineLabel	: label_t -> unit
    val emitLabel	: (int * label_t) -> unit

    exception BadReal of string
    val emitlong	: int -> unit
    val realconst	: string -> unit
    val emitstring	: string -> unit

    val mark	: unit -> unit
    val comment : string -> unit

  (* Psuedo instructions.  These are span dependent. *)
    val emitJump	: label_t -> unit
    val emitJumpIndex	: (label_t * reg_t) -> unit
    val emitJumpUnboxed	: (reg_t * label_t) -> unit
    val emitRangeChk	: (reg_t * reg_t * label_t) -> unit
    val emitCondJmp	: (condition * reg_t * reg_t * label_t) -> unit
    val emitFCondJmp	: (condition * fp_reg_t * fp_reg_t * label_t) -> unit
    val emitFConst	: (fp_reg_t * label_t) -> unit
    val emitCodeAddr	: (reg_t * label_t * int) -> unit
    val emitAllocAddr	: (label_t * int) -> unit

  (* Real bytecode instructions *)
    val emit_IMMED32		: (reg_t * int) -> unit
    val emit_IMMED20		: (reg_t * int) -> unit
    val emit_MOVE		: (reg_t * reg_t) -> unit
    val emit_FMOVE		: (fp_reg_t * fp_reg_t) -> unit
    val emit_GETEXN		: reg_t -> unit
    val emit_PUTEXN		: reg_t -> unit
    val emit_GETVAR		: reg_t -> unit
    val emit_PUTVAR		: reg_t -> unit
    val emit_LIMITCHK		: (int * int) -> unit
    val emit_LIMITCHK2		: (int * int) -> unit
    val emit_JMPind		: reg_t -> unit		
    val emit_BEGIN		: int -> unit
    val emit_ALLOC		: reg_t -> unit
    val emit_ALLOCpath		: (reg_t * int list) -> unit
    val emit_ALLOCpath0		: (reg_t * int list) -> unit
    val emit_ALLOCimmed		: int -> unit
    val emit_END		: reg_t -> unit
    val emit_PAIR		: (reg_t * reg_t * reg_t) -> unit
    val emit_PAIRimmedl		: (reg_t * int * reg_t) -> unit
    val emit_PAIRimmedr		: (reg_t * reg_t * int) -> unit
    val emit_ALLOCFRAME		: (reg_t * int) -> unit
    val emit_FREEFRAME		: (reg_t * int) -> unit
    val emit_MARKSTORE		: (reg_t * bool) -> unit
    val emit_MARKSTOREindex	: (reg_t * reg_t * bool) -> unit
    val emit_MARKSTOREoffset	: (reg_t * int * bool) -> unit
    val emit_LOAD		: (reg_t * reg_t) -> unit
    val emit_LOADindex		: (reg_t * reg_t * reg_t) -> unit
    val emit_LOADoffset		: (reg_t * reg_t * int) -> unit
    val emit_STORE		: (reg_t * reg_t) -> unit
    val emit_STOREindex		: (reg_t * reg_t * reg_t) -> unit
    val emit_STOREoffset	: (reg_t * int * reg_t) -> unit
    val emit_STOREimmed		: (reg_t * int) -> unit
    val emit_BLOAD		: (reg_t * reg_t) -> unit
    val emit_BLOADindex		: (reg_t * reg_t * reg_t) -> unit
    val emit_BLOADoffset	: (reg_t * reg_t * int) -> unit
    val emit_BSTORE		: (reg_t * reg_t) -> unit
    val emit_BSTOREindex	: (reg_t * reg_t * reg_t) -> unit
    val emit_BSTOREoffset	: (reg_t * int * reg_t) -> unit
    val emit_BSTOREimmed	: (reg_t * int) -> unit
    val emit_ASHL		: (reg_t * reg_t * reg_t) -> unit
    val emit_ASHL1		: (reg_t * reg_t) -> unit
    val emit_ASHLimmed		: (reg_t * reg_t * int) -> unit
    val emit_ASHR		: (reg_t * reg_t * reg_t) -> unit
    val emit_ASHR1		: (reg_t * reg_t) -> unit
    val emit_ASHRimmed		: (reg_t * reg_t * int) -> unit
    val emit_ORB		: (reg_t * reg_t * reg_t) -> unit
    val emit_ORB1		: (reg_t * reg_t) -> unit
    val emit_ORBimmed		: (reg_t * reg_t * int) -> unit
    val emit_XORB		: (reg_t * reg_t * reg_t) -> unit
    val emit_XORBimmed		: (reg_t * reg_t * int) -> unit
    val emit_ANDB		: (reg_t * reg_t * reg_t) -> unit
    val emit_ANDBimmed		: (reg_t * reg_t * int) -> unit
    val emit_NOTB		: (reg_t * reg_t) -> unit
    val emit_ADD		: (reg_t * reg_t * reg_t) -> unit
    val emit_ADDimmed		: (reg_t * reg_t * int) -> unit
    val emit_INCR		: (reg_t * int) -> unit
    val emit_ADDT		: (reg_t * reg_t * reg_t) -> unit
    val emit_ADDTimmed		: (reg_t * reg_t * int) -> unit
    val emit_SUB		: (reg_t * reg_t * reg_t) -> unit
    val emit_SUBimmed		: (reg_t * reg_t * int) -> unit
    val emit_SUBT		: (reg_t * reg_t * reg_t) -> unit
    val emit_SUBTimmed		: (reg_t * reg_t * int) -> unit
    val emit_MULT		: (reg_t * reg_t) -> unit
    val emit_MULTimmed		: (reg_t * int) -> unit
    val emit_DIVT		: (reg_t * reg_t) -> unit
    val emit_DIVTimmed		: (reg_t * int) -> unit

    val emit_FLOAD		: (fp_reg_t * reg_t) -> unit
    val emit_FLOADindex		: (fp_reg_t * reg_t * reg_t) -> unit
    val emit_FLOADoffset	: (fp_reg_t * reg_t * int) -> unit
    val emit_FSTORE		: (reg_t * fp_reg_t) -> unit
    val emit_FSTOREindex	: (reg_t * reg_t * fp_reg_t) -> unit
    val emit_FSTOREoffset	: (reg_t * int * fp_reg_t) -> unit
    val emit_FADD		: (fp_reg_t * fp_reg_t * fp_reg_t) -> unit
    val emit_FSUB		: (fp_reg_t * fp_reg_t * fp_reg_t) -> unit
    val emit_FMUL		: (fp_reg_t * fp_reg_t * fp_reg_t) -> unit
    val emit_FDIV		: (fp_reg_t * fp_reg_t * fp_reg_t) -> unit
    val emit_FNEG		: (fp_reg_t * fp_reg_t) -> unit
    val emit_FABS		: (fp_reg_t * fp_reg_t) -> unit
    val emit_FLOAT		: (fp_reg_t * reg_t) -> unit
    val emit_FALLOC		: (reg_t * fp_reg_t) -> unit

  (* allocate a two-word "special" cell *)
    val emit_SPECIAL		: (reg_t * reg_t * reg_t) -> unit

  end (* BC_CODER *)

signature BC_MCODER = 
  sig 
    structure Coder : BC_CODER
    val finish : unit -> Word8Vector.vector
  end

(*
 * $Log: bc-coder-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:28  george
 *   Version 109.24
 *
 *)

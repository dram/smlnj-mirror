(* mltree.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

functor MLTreeF(structure Const : CONSTANT
		structure R : REGION
                structure S : INSTRUCTION_STREAM
                type rextension 
                type fextension
               ) : MLTREE =
struct
  structure Constant = Const
  structure PseudoOp = S.P
  structure Stream = S
  structure Region = R
  structure Basis  = MLTreeBasis
  structure Util   = MLTreeUtil

  type rextension = rextension
  type fextension = rextension

  type ty  = Basis.ty
  type fty = Basis.fty
  type var = int (* variable *)
  type src = var (* source variable *)
  type dst = var (* destination variable *)
  type reg = var (* physical register *)

  datatype cond = datatype Basis.cond
  datatype fcond = datatype Basis.fcond
  datatype ext = datatype Basis.ext
  datatype rounding_mode = datatype Basis.rounding_mode

  (* phi-functions for SSA form *)
  datatype phi =
      PHI  of ty * dst * src list 
    | FPHI of fty * dst * src list 
    | CCPHI of dst * src list 

  (* aliasing declarations 
   * These are used to define physical register bindings for SSA names 
   *)
  type alias = var * reg  

  (* statements *)
  datatype stm =
      MV      of ty * dst * rexp	
    | CCMV    of dst * ccexp
    | FMV     of fty * dst * fexp	
    | COPY    of ty * dst list * src list
    | FCOPY   of fty * dst list * src list
    | JMP     of rexp * Label.label list
    | CALL    of rexp * mlrisc list * mlrisc list * Region.region
    | RET

    | STORE  of ty * rexp * rexp * Region.region	(* address, data *)
    | STORE_UNALIGNED of ty * rexp * rexp * Region.region
    | FSTORE of fty * rexp * fexp * Region.region	(* address, data *)
    | FSTORE_UNALIGNED of fty * rexp * fexp * Region.region
    | BCC    of Basis.cond * ccexp * Label.label 
    | FBCC   of Basis.fcond * ccexp * Label.label
    | ANNOTATION of stm * Annotations.annotation

      (* The following are used internally by SSA optimizations; 
       * The frontend should not generate these.
       *)
    | RTL of word ref * word * stm (* an RTL *) 
    | RTLPHI of int (* a phi-function at block id *)
    | RTLPINNED of stm (* pinned statement *)
    | RTLPAR of stm list (* parallel execution *)
   
  and rexp = 
      REG    of ty * src

      (* sizes of constants are inferred by context *)
    | LI     of int   
    | LI32   of Word32.word
    | LI64   of Word64.word
    | LABEL  of LabelExp.labexp
    | CONST  of Constant.const

    | ADD    of ty * rexp * rexp
    | SUB    of ty * rexp * rexp 

      (* signed multiplication etc. *)
    | MULS   of ty * rexp * rexp
    | DIVS   of ty * rexp * rexp
    | REMS   of ty * rexp * rexp

      (* unsigned multiplication etc. *)
    | MULU   of ty * rexp * rexp
    | DIVU   of ty * rexp * rexp 
    | REMU   of ty * rexp * rexp

      (* trapping versions of above. These are all signed *)
    | ADDT   of ty * rexp * rexp 
    | SUBT   of ty * rexp * rexp 
    | MULT   of ty * rexp * rexp
    | DIVT   of ty * rexp * rexp
    | REMT   of ty * rexp * rexp 

    | ANDB   of ty * rexp * rexp
    | ORB    of ty * rexp * rexp
    | XORB   of ty * rexp * rexp
    | NOTB   of ty * rexp

    | SRA   of ty * rexp * rexp		(* value, shift *)
    | SRL   of ty * rexp * rexp
    | SLL   of ty * rexp * rexp

      (* type promotion *)
    | CVTI2I of ty * Basis.ext * ty * rexp
    | CVTF2I of ty * Basis.rounding_mode * fty * fexp

      (* conditional (eager) evaluation *)
    | COND of ty * ccexp * rexp * rexp

      (* integer load *)
    | LOAD of ty * rexp * Region.region
    | LOAD_UNALIGNED of ty * rexp * Region.region

    | SEQ of stm * rexp

    | EXT of ty * rextension * rexp list

    | MARK of rexp * Annotations.annotation

      (* Used in RTL *)
    | RTLPC (* the program counter; used for describing relative addressing *)
    | RTLMISC of Basis.misc_op ref * rexp list

  and fexp =
      FREG   of fty * src
    | FLOAD  of fty * rexp * Region.region
    | FLOAD_UNALIGNED  of fty * rexp * Region.region

    | FADD   of fty * fexp * fexp
    | FMUL   of fty * fexp * fexp
    | FSUB   of fty * fexp * fexp 
    | FDIV   of fty * fexp * fexp
    | FABS   of fty * fexp 
    | FNEG   of fty * fexp
    | FSQRT  of fty * fexp

    | CVTI2F of fty * Basis.ext * ty * rexp
    | CVTF2F of fty * Basis.rounding_mode * fty * fexp
    | FSEQ   of stm * fexp

    | FEXT of fty * fextension * fexp list

    | FMARK of fexp * Annotations.annotation

      (* used in RTL *)
    | RTLFMISC of Basis.misc_op ref * fexp list

  and ccexp =
      CC     of src
    | CMP    of ty * Basis.cond * rexp * rexp 
    | FCMP   of fty * Basis.fcond * fexp * fexp
    | CCMARK of ccexp * Annotations.annotation
    | RTLCCMISC of Basis.misc_op ref * ccexp list

  and mlrisc = CCR of ccexp | GPR of rexp | FPR of fexp

  exception Unsupported of string * rexp

  type ('i,'regmap) stream =
       ('i -> unit,'regmap,Annotations.annotations,
        mlrisc list, alias, phi) Stream.stream

end (* MLTREE *)

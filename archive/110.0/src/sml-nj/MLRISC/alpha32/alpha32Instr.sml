(* alpha32Instr.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor Alpha32Instr
  (structure Const : CONSTANT
   structure Region: REGION): ALPHA32INSTR =  
struct
  structure C = Alpha32Cells
  structure Constant = Const
  structure Region = Region

(*  type csets = int list * int list *)
				      (* Instruction formats *)
  datatype operand =
      REGop of int
    | IMMop of int
    | HILABop of LabelExp.labexp
    | LOLABop of LabelExp.labexp
    | LABop of LabelExp.labexp
    | CONSTop of Const.const

  datatype cond_code =
      CC_EQ | CC_NEQ
    | CC_LT | CC_LE | CC_GT | CC_GE
    | CC_LTU | CC_LEU | CC_GTU | CC_GEU

  datatype branch = BR | BEQ | BGE | BGT | BLE | BLT | BNE | BLBC | BLBS

  datatype load = LDL | LDQ | LDQ_U
  datatype store = STL | STQ | STQ_U
  datatype fload = LDT | LDS
  datatype fstore = STT		

  datatype operate =
      ZAP | ADDL | ADDQ | SUBL | SUBQ | MULL
    | S4ADDL | S8ADDL
    | CMPULE | CMPULT | CMPEQ | CMPLE | CMPLT | SGNXL 
    | AND | BIS | XOR | SRA | SRL | SLL
    | INSBL | EXTBL | EXTQH | MSKBL | MSKLH

  datatype pseudo_op = DIVL | DIVLU

  datatype operateV = ADDLV | SUBLV | MULLV

  datatype foperate = 
      CPYS | CPYSN 
    | CVTQT | CVTLQ
    | CMPTEQ | CMPTLT | CMPTLE | CMPTUN
  datatype foperateV = CVTTQ | ADDT | SUBT | MULT | DIVT

  datatype osf_user_palcode = 
    BPT | BUGCHK | CALLSYS | GENTRAP | IMB | RDUNIQUE | WRUNIQUE

  datatype instruction =
    DEFFREG of int			(* define a floating point register *)

  (* Load/Store *)
  | LDA of {r:int, b:int, d:operand}	(* use of REGop is illegal as operand *)
  | LDAH of {r:int, b:int, d:operand} (* use of REGop is illegal as operand *)

  | LOAD of {ldOp:load, r:int, b:int, d:operand, mem:Region.region}
  | STORE of {stOp:store, r:int, b:int, d:operand, mem:Region.region}
  | FLOAD of {ldOp:fload, r:int, b:int, d:operand, mem:Region.region}
  | FSTORE of {stOp:fstore, r:int, b:int, d:operand, mem:Region.region}

  (* Control Instructions *)
  | JMPL of {r:int, b:int, d:int} * Label.label list
  | JSR of {r:int, b:int, d:int} * C.cellset * C.cellset
  | BRANCH of branch * int * Label.label   
  | FBRANCH of branch * int * Label.label  

  (* Integer Operate *)
  | OPERATE of {oper:operate, ra:int, rb:operand, rc:int}
  | OPERATEV of {oper:operateV, ra:int, rb:operand, rc:int}
  | PSEUDOARITH of {oper: pseudo_op, ra:int, rb:operand, rc:int, tmps: C.cellset}

  (* Copy instructions *)
  | COPY of int list * int list * instruction list option ref
  | FCOPY of int list * int list * instruction list option ref

  (* Floating Point Operate *)
  | FOPERATE of {oper:foperate, fa:int, fb:int, fc:int}
  | FOPERATEV of {oper:foperateV, fa:int, fb:int, fc:int}

  (* Misc *)
  | TRAPB				(* Trap barrier *)

  | CALL_PAL of {code:osf_user_palcode, def:int list, use:int list}

end




(*
 * $Log: alpha32Instr.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:34  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.4  1997/09/17 20:01:07  george
 *    Added support for S4ADDL and S8ADDL
 *
# Revision 1.3  1997/08/29  10:59:16  george
#   Added LDS, CVTLQ, DIVL and DIVLU instructions.
#
# Revision 1.2  1997/07/28  20:03:59  george
#   Added a region component to all load and stores
#
# Revision 1.1.1.1  1997/04/19  18:14:22  george
#   Version 109.27
#
 *)

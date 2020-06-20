(* cfg.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * CFG IR for SML/NJ code generation.
 *)

structure CFG_Prim =
  struct
    datatype numkind = INT | FLT

  (* rounding modes for float conversions *)
      datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO

  (* allocation operations *)
    datatype alloc
      = RECORD of {desc : IntInf.int, mut : bool}
      | RAW_RECORD of {desc : IntInf.int, kind : numkind, sz : int}
      | RAW_ALLOC of {desc : IntInf.int option, align : int, len : int}

  (* arithmetic operations that may overflow; for the division operators,
   * we assume that the second argument is never zero (i.e., an explicit
   * test for zero is done before the operation).
   * NOTE: this type is defined in the ArithOps structure (ElabData/prim/arithops.sml)
   *)
    datatype arithop = datatype CPS.P.arithop

  (* arithmetic operations that do not overflow; for the division operators,
   * we distinguish between signed and unsigned operations, and assume that
   * the second argument is never zero (i.e., an explicit test for zero is
   * done before the operation).
   *)
    datatype pureop
      = ADD | SUB
      | SMUL | SDIV | SREM
      | UMUL | UDIV | UREM
      | LSHIFT | RSHIFT | RSHIFTL
      | ORB | XORB | ANDB
      | FADD | FSUB | FMUL | FDIV
      | FNEG | FABS | FSQRT

    datatype pure
      = PURE_ARITH of {oper : pureop, sz : int}
      | EXTEND of {signed : bool, from : int, to : int}
      | INT_TO_REAL of {from : int, to : int}
      | LOAD_RAW of {offset : int, kind : numkind, sz : int}
      | PURE_SUBSCRIPT
      | PURE_RAW_SUBSCRIPT of {kind : numkind, sz : int}

    datatype arith
      = ARITH of {oper : arithop, sz : int}
      | TEST of {from : int, to : int}
      | TESTU of {from : int, to : int}
      | REAL_TO_INT of {mode : rounding_mode, from : int, to : int}

    datatype looker
      = DEREF
      | SUBSCRIPT
      | RAW_SUBSCRIPT of {kind : numkind, sz : int}
      | GET_HDLR | GET_VAR

    datatype setter
      = UNBOXED_UPDATE | UPDATE				(* array update *)
      | UNBOXED_ASSIGN | ASSIGN				(* reference assignment *)
      | RAW_UPDATE of {kind : numkind, sz : int}	(* raw array update *)
      | RAW_STORE of {kind : numkind, sz : int}		(* raw store to base+offset *)
      | SET_HDLR | SET_VAR

  (* fcmpop conforms to the IEEE std 754 predicates. *)
    datatype fcmpop = datatype CPS.P.fcmpop

  (* comparison operators
   * NOTE: this type is defined in the ArithOps structure (ElabData/prim/arithops.sml)
   *)
    datatype cmpop = datatype CPS.P.cmpop

  (* These are two-way branches dependent on pure inputs *)
    datatype branch
      = CMP of {oper: cmpop, signed: bool, sz : int}
      | FCMP of {oper: fcmpop, sz: int}
      | FSGN of int
      | BOXED | UNBOXED
      | PEQL | PNEQ

  end

structure CFG =
  struct

    datatype ty
      = NUMt of int
      | FLTt of int
      | PTRt
      | FUNt
      | CNTt

    datatype frag_kind
      = GC_CHECK
      | INTERNAL

    datatype calling_conv
      = STD_FUN
      | STD_CONT
      | KNOWN of {gcChk : bool}

    datatype rcc_kind
      = FAST_RCC
      | REENTRANT_RCC

  (* fragment/function parameters *)
    type param = LambdaVar.lvar * ty

  (* branch probabilities are measured in percent (1..99).  We use 0 to
   * represent the absence of probability information.
   *)
    type probability = int

    datatype exp
      = VAR of LambdaVar.lvar
      | LABEL of LambdaVar.lvar
      | NUM of {iv : IntInf.int, signed : bool, sz : int}
      | LOOKER of CFG_Prim.looker * exp list
      | PURE of CFG_Prim.pure * exp list
      | SELECT of int * exp
      | OFFSET of int * exp

    datatype stm
      = LET of exp * param * stm
      | ALLOC of CFG_Prim.alloc * exp list * LambdaVar.lvar * stm
      | APP of calling_conv * exp * exp list
      | GOTO of LambdaVar.lvar * exp list
      | SWITCH of exp * stm list
      | BRANCH of CFG_Prim.branch * exp list * probability * stm * stm
      | ARITH of CFG_Prim.arith * exp list * param * stm
      | SETTER of CFG_Prim.setter * exp list * stm
      | RCC of rcc_kind * string * PrimCTypes.c_proto * exp list * param list * stm

  (* an extended basic block *)
    type frag = frag_kind * LambdaVar.lvar * param list * stm

  (* the entry fragment of a cluster *)
    type function = calling_conv * LambdaVar.lvar * param list * stm

    type cluster = function * frag list

  end

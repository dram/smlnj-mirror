(* cfg.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * CFG IR for SML/NJ code generation.
 *)

structure CPS_Type =
  struct
    type intty = {sz : int, tag : bool}
    datatype pkind = datatype CPS.pkind
    datatype cty = datatype CPS.cty
    datatype record_kind = datatype CPS.record_kind
  end

structure CFG_Prim =
  struct
  (* numkind includes kind and size *)
    datatype numkind = INT | UINT | FLT

  (* allocation operations *)
    datatype alloc
      = RECORD of {tag : IntInf.int, mut : bool}
      | RAW_RECORD of {tag : IntInf.int, align : int}
      | RAW_ALLOC of {tag : IntInf.int, align : int, len : int}

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
      = ADD | SUB | MUL
      | SDIV | SREM
      | UDIV | UREM
      | LSHIFT | RSHIFT | RSHIFTL
      | ORB | XORB | ANDB
      | FADD | FSUB | FMUL | FDIV
      | FABS
      | FSQRT
      | FSIN | FCOS | FTAN

    datatype pure
      = PURE_ARITH of {oper : arithop, size : int}
      | COPY of {from : int, to : int}
      | EXTEND of {from : int, to : int}
      | TRUNC of {from : int, to : int}
      | COPY_INF of int
      | EXTEND_INF of int
      | TRUNC_INF of int
      | INT_TO_REAL of {from : int, to : int}
      | LOAD_WORD of {offset : int, ty : CPS_Type.cty}
      | LOAD_RAW of {offset : int, kind : numkind, sz : int}
      | PURE_SUBSCRIPT
      | PURE_RAW_SUBSCRIPT of {kind : numkind, sz : int}

    datatype arith
      = ARITH of {oper : arithop, size : int}
      | TEST of {from : int, to : int}
      | TESTU of {from : int, to : int}
      | TEST_INF of int
      | REAL_TO_INT of {floor : bool, from : int, to : int}

    datatype looker
      = DEREF
      | SUBSCRIPT
      | RAW_SUBSCRIPT of {kind : numkind, size : int}
      | GETHDLR | GETVAR

    datatype setter
      = UNBOXED_UPDATE | UPDATE
      | UNBOXED_ASSIGN | ASSIGN
      | RAW_UPDATE of {kind : numkind, size : int}
      | SETHDLR | SETVAR

  (* fcmpop conforms to the IEEE std 754 predicates. *)
    datatype fcmpop = datatype CPS.P.fcmpop

  (* comparison operators
   * NOTE: this type is defined in the ArithOps structure (ElabData/prim/arithops.sml)
   *)
    datatype cmpop = datatype CPS.P.cmpop

  (* These are two-way branches dependent on pure inputs *)
    datatype branch = datatype CPS.P.branch

  end

structure CFG =
  struct

    datatype fun_kind
      = STANDARD
      | GC_CHECK
      | INTERNAL

    datatype rcc_kind
      = FAST_RCC
      | REENTRANT_RCC

    type rcc_param = LambdaVar.lvar * CPS_Type.cty

    datatype exp
      = VAR of LambdaVar.lvar
      | LABEL of LambdaVar.lvar
      | NUM of {iv : IntInf.int, signed : bool, sz : int}
      | LOOKER of CFG_Prim.looker * exp list
      | PURE of CFG_Prim.pure * exp list
      | SELECT of int * exp
      | OFFSET of int * exp

    type accesspath = int list * int

    type field = exp * accesspath

    datatype stm
      = LET of exp * LambdaVar.lvar * stm
      | ALLOC of CFG_Prim.alloc * exp list * LambdaVar.lvar * stm
      | RECORD of CPS_Type.record_kind * field list * LambdaVar.lvar * stm
      | APP of exp * exp list
      | SWITCH of exp * stm list
      | BRANCH of CFG_Prim.branch * exp list * stm * stm
      | ARITH of CFG_Prim.arith * exp list * LambdaVar.lvar * stm
      | SETTER of CFG_Prim.setter * exp list * stm
      | RCC of rcc_kind * string * CTypes.c_proto * exp list * rcc_param list * stm

    type function = fun_kind * LambdaVar.lvar * LambdaVar.lvar list * CPS_Type.cty list * stm

  end

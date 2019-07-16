(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* ltyenv.sig *)

signature LTY_ENV = sig 

type tkind = LtyKernel.tkind
type tyc = LtyKernel.tyc
type lty = LtyKernel.lty
type tkindEnv = LtyKernel.tkindEnv

type ltyEnv 
exception ltUnbound
val initLtyEnv : ltyEnv
val ltLookup : ltyEnv * LambdaVar.lvar * DebIndex.depth -> lty
val ltInsert : ltyEnv * LambdaVar.lvar * lty * DebIndex.depth -> ltyEnv

(** printing arbitrary tycs and ltys *)
val tcPrint : tyc -> string
val ltPrint : lty -> string

(** testing the equivalence on arbitrary tycs and ltys *)
val tcEquiv : tyc * tyc -> bool
val ltEquiv : lty * lty -> bool
val ltEqvGen : int -> (lty * lty -> bool)

(** lazily applying a lty to a list of tycs *)
exception TkTycChk
exception LtyAppChk
val ltApply : lty * tyc list -> lty
val ltAppSt : lty * tyc list -> lty (* always returns normal form *)
val tcAppSt : tyc * tyc list -> tyc (* always returns normal form *)
val ltAppChk : lty * tyc list * tkindEnv -> lty

(** lazily finding out the arg and res of an lty *)
exception LtyArrow
val ltArrow : lty -> lty * lty

(** lazily selecting a field from a record/structure type *)
exception LtySelect
val ltSelect : lty * int -> lty

(** build a function or functor type from a pair of arbitrary ltys *)
val ltFun : lty * lty -> lty

(** build a tuple type from a list of arbitrary ltys *)
val ltTup : lty list -> lty

(** adjusting an lty or a tyc based on its depth information **)
val adjLty : lty * DebIndex.depth * DebIndex.depth -> lty
val adjTyc : tyc * DebIndex.depth * DebIndex.depth -> tyc
val adjTycSp : tyc * DebIndex.depth * DebIndex.depth -> tyc

(* special adjustment functions used in specializations *)
val ltAdjSt : tkind list * lty * tyc list * int * int -> lty
val tcAdjSt : tkind list * tyc * tyc list * int * int -> tyc
val ltSinkSt : tkind list * lty * DebIndex.depth * DebIndex.depth -> lty
val tcSinkSt : tkind list * tyc * DebIndex.depth * DebIndex.depth -> tyc

(** wrapping a lambda tyc *)
val tcAbs : tyc * tkind -> tyc
val tcWrap : tyc -> tyc option
val ltWrap : lty -> lty option
val tcsWrap : tyc list -> tyc list option

(** based on the given tyc, return its appropriate Update operator *)
val tcUpd : tyc -> PrimOp.primop 

(** type convertion; used in the ltNarrow phase *)
val tkLty : tkind -> lty
val tcNarrow : tyc -> tyc
val ltNarrow : lty -> lty
val ltNarrowSt : lty -> lty

end (* signature LTY_ENV *)


(*
 * $Log: ltyenv.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.4  1997/05/05 19:59:39  george
 *   Change the term language into the quasi-A-normal form. Added a new round
 *   of lambda contraction before and after type specialization and
 *   representation analysis. Type specialization including minimum type
 *   derivation is now turned on all the time. Real array is now implemented
 *   as realArray. A more sophisticated partial boxing scheme is added and
 *   used as the default.
 *
 * Revision 1.3  1997/04/18  15:48:15  george
 *   Cosmetic changes on some constructor names. Changed the shape for
 *   FIX type to potentially support shared dtsig. -- zsh
 *
 * Revision 1.2  1997/02/26  21:46:29  george
 *   Added ltWrap (from zsh)
 *
 *)

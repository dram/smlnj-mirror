(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* ltykernel.sig *)

signature LTYKERNEL = sig 

(* definitions of kind and kind-environment *)
type tkind
type tkindEnv 

datatype tkindI
  = TK_TYC                                (* ground mono tycon *)
  | TK_TBX				  (* boxed/tagged tycon *)
  | TK_SEQ of tkind list                  (* sequence of tycons *)
  | TK_FUN of tkind * tkind               (* tycon function *)

(* definitions of tyc and tyc-environment *)
type tyc
type tycEnv 

datatype tycI
  = TC_VAR of DebIndex.index * int        (* tyc variable *)
  | TC_PRIM of PrimTyc.primtyc            (* primitive tyc *)
  | TC_FN of tkind list * tyc             (* tyc abstraction *)
  | TC_APP of tyc * tyc list              (* tyc application *)
  | TC_SEQ of tyc list                    (* tyc sequence *)
  | TC_PROJ of tyc * int                  (* tyc projection *)

  | TC_SUM of tyc list                    (* sum tyc *)
  | TC_FIX of tyc * int                   (* recursive tyc *) 
  | TC_ABS of tyc                         (* abstract tyc *)

  | TC_TUPLE of tyc list                  (* std record tyc *)
  | TC_ARROW of tyc * tyc                 (* std function tyc *)

  | TC_BOX of tyc                         (* boxed tyc *)
  | TC_CONT of tyc                        (* std continuation tyc *)
  | TC_IND of tyc * tycI                  (* indirect tyc thunk *)
  | TC_ENV of tyc * int * int * tycEnv    (* tyc closure *)

(* definition of lty *)
type lty
datatype ltyI          
  = LT_TYC of tyc                         (* monomorphic type *)  
  | LT_STR of lty list                    (* structure type *)
  | LT_PST of (int * lty) list            (* partial-str type *)
  | LT_FCT of lty * lty                   (* functor type *)
  | LT_POLY of tkind list * lty           (* polymorphic type *)
  | LT_CNT of lty                         (* std continuation type *)
  | LT_IND of lty * ltyI                  (* indirect type thunk *)
  | LT_ENV of lty * int * int * tycEnv    (* type closure *)

(** injections and projections on tkind, tyc, and lty *)
val tk_inj : tkindI -> tkind 
val tc_inj : tycI -> tyc
val lt_inj : ltyI -> lty

val tk_out : tkind -> tkindI
val tc_out : tyc -> tycI
val lt_out : lty -> ltyI

(** key comparison for tkind, tyc, and lty; used in pickling *)
val tk_cmp : tkind * tkind -> order
val tc_cmp : tyc * tyc -> order
val lt_cmp : lty * lty -> order

(** get the hash key of each lty, used in translate/coerce.sml; a hack! *)
val lt_key : lty -> int

(** testing the "pointer" equality for tkind, tyc, and lty *)
val tk_eq : tkind * tkind -> bool
val tc_eq : tyc * tyc -> bool
val lt_eq : lty * lty -> bool

(** testing the equivalence for "normalized" tkind, tyc and lty *)
val tk_eqv : tkind * tkind -> bool
val tc_eqv : tyc * tyc -> bool
val lt_eqv : lty * lty -> bool

(** utility functions on tkindEnv *)
exception tkUnbound
val initTkEnv : tkindEnv
val tkLookup : tkindEnv * int * int -> tkind
val tkInsert : tkindEnv * tkind list -> tkindEnv

(** utility functions on tycEnv *)
exception tcUnbound
val initTycEnv : tycEnv
val tcLookup : int * tycEnv -> (tyc list option * int)
val tcInsert : tycEnv * (tyc list option * int) -> tycEnv
val tcSplit : tycEnv -> ((tyc list option * int) * tycEnv) option

(** testing if a tyc (or lty) is in the normal form *)
val tcp_norm : tyc -> bool
val ltp_norm : lty -> bool

(** finding out the for a tyc's innermost-bound free variables *)
val tc_depth : tyc * DebIndex.depth -> DebIndex.depth
val tcs_depth : tyc list * DebIndex.depth -> DebIndex.depth

(** utility functions for TC_ENV and LT_ENV types *)
val tcc_env : tyc * int * int * tycEnv -> tyc
val ltc_env : lty * int * int * tycEnv -> lty

(** utility functions for updating tycs and ltys *)
val tyc_upd : tyc * tyc -> unit
val lty_upd : lty * lty -> unit

end (* signature LTYKERNEL *)


(*
 * $Log: ltykernel.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/04/18 15:42:27  george
 *   Type specialization is now added in (though it is still turned off).
 *   A pretty-fancy kind of minimum type derivation is also done. -- zsh
 *
 * Revision 1.1.1.1  1997/01/14  01:38:10  george
 *   Version 109.24
 *
 *)

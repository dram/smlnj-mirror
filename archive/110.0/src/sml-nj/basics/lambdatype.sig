(* Copyright 1996 by AT&T Bell Laboratories *)
(* lambdatype.sig *)

signature LAMBDATYPE = 
sig

type tkind = LtyKernel.tkind
type tyc = LtyKernel.tyc
type lty = LtyKernel.lty

(** constructing a tkind value *)
val tkc_mono : tkind
val tkc_mobx  : tkind
val tkc_funs : int -> tkind
val tkc_args : int -> tkind list
val tkc_seqs : tkind list -> tkind
val tkc_fcts : tkind * tkind -> tkind

(** constructing a tyc value *)
val tcc_var  : DebIndex.index * int -> tyc
val tcc_prim : PrimTyc.primtyc -> tyc
val tcc_fn   : tkind list * tyc -> tyc
val tcc_app  : tyc * tyc list -> tyc
val tcc_seq  : tyc list -> tyc
val tcc_proj : tyc * int -> tyc
val tcc_sum  : tyc list -> tyc
val tcc_fix  : tyc * int -> tyc 
val tcc_abs  : tyc -> tyc
val tcc_box  : tyc -> tyc
val tcc_tup  : tyc list -> tyc
val tcc_arw  : tyc * tyc -> tyc

(** primitive lambda tycs *)
val tcc_int   : tyc
val tcc_int32 : tyc
val tcc_real  : tyc
val tcc_string: tyc
val tcc_exn   : tyc
val tcc_void  : tyc
val tcc_unit  : tyc
val tcc_bool  : tyc

val tcc_null  : tyc
val tcc_ref   : tyc
val tcc_array : tyc
val tcc_vector: tyc
val tcc_tv    : int -> tyc
val tcc_iexn   : tyc

val tcc_cont  : tyc -> tyc

(** decomposing a tyc value *)
val tcd_arw    : tyc -> tyc * tyc
val tcd_tup    : tyc -> tyc list
val tcd_abs    : tyc -> tyc

(** constructing an lty value *)
val ltc_tyc  : tyc -> lty
val ltc_str  : lty list -> lty
val ltc_pst  : (int * lty) list -> lty
val ltc_fct  : lty * lty -> lty
val ltc_poly : tkind list * lty -> lty

(** primtive lambda types *)
val ltc_int    : lty
val ltc_int32  : lty
val ltc_real   : lty
val ltc_string : lty
val ltc_exn    : lty
val ltc_void   : lty
val ltc_bool   : lty
val ltc_unit   : lty

val ltc_top    : lty
val ltc_tv     : int -> lty
val ltc_cont   : lty -> lty
val ltc_tup    : lty list -> lty
val ltc_arw    : lty * lty -> lty
val ltc_fun    : lty * lty -> lty
val ltc_pexn   : lty
val ltc_iexn   : lty

(** decomposing a lty value *)
val ltd_tyc    : lty -> tyc
val ltd_tup    : lty -> lty list
val ltd_arw    : lty -> lty * lty

(** printing values of tkind, tyc, lty *)
val tk_print : tkind -> string
val tc_print : tyc -> string
val lt_print : lty -> string

(** other utility functions *)
val lt_select : lty * int -> lty
val lt_arrow : lty -> lty * lty
val lt_swap : lty -> lty
val lt_merge : lty * lty -> lty
val lt_size : lty -> int
val lt_length : lty -> int

end (* signature LAMBDATYPE *) 



(*
 * $Log: lambdatype.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/04/18 15:48:14  george
 *   Cosmetic changes on some constructor names. Changed the shape for
 *   FIX type to potentially support shared dtsig. -- zsh
 *
 * Revision 1.2  1997/02/26  21:44:59  george
 *   added tcd_tup. (from zsh)
 *
 *)

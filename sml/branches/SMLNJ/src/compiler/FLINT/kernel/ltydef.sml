(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* ltydef.sml *)

structure LtyDef : LTYDEF = 
struct

local structure PT = PrimTyc
      structure DI = DebIndex
      structure LK = LtyKernel

      fun bug msg = ErrorMsg.impossible("LtyDef: "^msg)
      val say = Control.Print.say

      (** common utility functions *)
      val tk_inj = LK.tk_inj
      val tk_out = LK.tk_out
      val tk_eqv = LK.tk_eqv

      val tc_inj = LK.tc_inj
      val tc_out = LK.tc_out 
      val tc_eqv = LK.tc_eqv
 
      val lt_inj = LK.lt_inj
      val lt_out = LK.lt_out 
      val lt_eqv = LK.lt_eqv

in

(** basic entities *)
type index = DI.index
type depth = DI.depth
type primtyc = PT.primtyc
type tvar = LK.tvar

type fflag = LK.fflag
type rflag = LK.rflag

type tkind = LK.tkind
type tyc = LK.tyc
type lty = LK.lty

(* 
 * FLINT tkind is roughly equivalent to the following ML datatype 
 *
 *    datatype tkind 
 *      = TK_MONO 
 *      | TK_BOX
 *      | TK_SEQ of tkind list
 *      | TK_FUN of tkind list * tkind
 *
 * We treat tkind as an abstract type so we can no longer use 
 * pattern matching. 
 *)

(** tkind constructors *)
val tkc_mono   : tkind = tk_inj (LK.TK_MONO)
val tkc_box    : tkind = tk_inj (LK.TK_BOX)
val tkc_seq    : tkind list -> tkind = tk_inj o LK.TK_SEQ
val tkc_fun    : tkind list * tkind -> tkind = tk_inj o LK.TK_FUN

(** tkind deconstructors *)
val tkd_mono   : tkind -> unit = fn _ => ()
val tkd_box    : tkind -> unit = fn _ => ()
val tkd_seq    : tkind -> tkind list = fn tk => 
      (case tk_out tk of LK.TK_SEQ x => x
                       | _ => bug "unexpected tkind in tkd_seq")  
val tkd_fun    : tkind -> tkind list * tkind = fn tk => 
      (case tk_out tk of LK.TK_FUN x => x
                       | _ => bug "unexpected tkind in tkd_fun")  

(** tkind predicates *)
val tkp_mono   : tkind -> bool = fn tk => tk_eqv(tk, tkc_mono)
val tkp_box    : tkind -> bool = fn tk => tk_eqv(tk, tkc_box)
val tkp_seq    : tkind -> bool = fn tk => 
      (case tk_out tk of LK.TK_SEQ _ => true | _ => false)
val tkp_fun    : tkind -> bool = fn tk => 
      (case tk_out tk of LK.TK_FUN _ => true | _ => false)

(** tkind one-arm switches *)
fun tkw_mono (tk, f, g) = if tk_eqv(tk, tkc_mono) then f () else g tk
fun tkw_box (tk, f, g) = if tk_eqv(tk, tkc_box) then f () else g tk
fun tkw_seq (tk, f, g) = 
      (case tk_out tk of LK.TK_SEQ x => f x | _ => g tk)
fun tkw_fun (tk, f, g) = 
      (case tk_out tk of LK.TK_FUN x => f x | _ => g tk)


(* 
 * FLINT fflag and rflag are used to classify different kinds of monomorphic 
 * functions and records. As of now, they are roughly equivalent to:
 *
 *    datatype fflag
 *      = FF_VAR of bool * bool
 *      | FF_FIXED
 *
 *    datatype rflag = RF_TMP
 *
 * We treat both as abstract types so pattern matching no longer applies.
 * NOTE: FF_VAR flags are used by FLINTs before we perform representation
 * analysis while FF_FIXED is used by FLINTs after we perform representation
 * analysis. 
 *)

(** fflag and rflag constructors *)
val ffc_var    : bool * bool -> fflag = fn x => LK.FF_VAR x
val ffc_fixed  : fflag = LK.FF_FIXED
val rfc_tmp    : rflag = LK.RF_TMP

(** fflag and rflag deconstructors *)
val ffd_var    : fflag -> bool * bool = fn x =>
      (case x of LK.FF_VAR x => x | _ => bug "unexpected fflag in ffd_var")
val ffd_fixed  : fflag -> unit = fn x =>
      (case x of LK.FF_FIXED => () | _ => bug "unexpected fflag in ffd_fixed")
val rfd_tmp    : rflag -> unit = fn (LK.RF_TMP) => ()

(** fflag and rflag predicates *)
val ffp_var    : fflag -> bool = fn x => 
      (case x of LK.FF_VAR _ => true | _ => false)
val ffp_fixed  : fflag -> bool = fn x => 
      (case x of LK.FF_FIXED => true | _ => false)
val rfp_tmp    : rflag -> bool = fn (LK.RF_TMP) => true

(** fflag and rflag one-arm switch *)
fun ffw_var (ff, f, g) = 
      (case ff of LK.FF_VAR x => f x | _ => g ff)
fun ffw_fixed (ff, f, g) = 
      (case ff of LK.FF_FIXED => f () | _ => g ff)
fun rfw_tmp (rf, f, g) = f()


(* 
 * FLINT tyc is roughly equivalent to the following ML datatype 
 *
 *    datatype tyc
 *      = TC_VAR of index * int
 *      | TC_NVAR of tvar
 *      | TC_PRIM of primtyc
 *      | TC_FN of tkind list * tyc
 *      | TC_APP of tyc * tyc list
 *      | TC_SEQ of tyc list
 *      | TC_PROJ of tyc * int
 *      | TC_SUM of tyc list
 *      | TC_FIX of tyc * int
 *      | TC_WRAP of tyc                   (* used after rep. analysis only *)
 *      | TC_ABS of tyc                    (* NOT USED *)
 *      | TC_BOX of tyc                    (* NOT USED *)
 *      | TC_TUPLE of tyc list             (* rflag hidden *)
 *      | TC_ARROW of fflag * tyc list * tyc list 
 *
 * We treat tyc as an abstract type so we can no longer use 
 * pattern matching. Type applications (TC_APP) and projections 
 * (TC_PROJ) are automatically reduced as needed, that is, the
 * client does not need to worry about whether a tyc is in the
 * normal form or not, all functions defined here automatically 
 * take care of this.
 *)

(** tyc constructors *)
val tcc_var    : index * int -> tyc = tc_inj o LK.TC_VAR
val tcc_nvar   : tvar -> tyc = tc_inj o LK.TC_NVAR
val tcc_prim   : primtyc -> tyc = tc_inj o LK.TC_PRIM
val tcc_fn     : tkind list * tyc -> tyc = tc_inj o LK.TC_FN
val tcc_app    : tyc * tyc list -> tyc = tc_inj o LK.TC_APP
val tcc_seq    : tyc list -> tyc = tc_inj o LK.TC_SEQ
val tcc_proj   : tyc * int -> tyc = tc_inj o LK.TC_PROJ
val tcc_sum    : tyc list -> tyc = tc_inj o LK.TC_SUM
val tcc_fix    : (int * tyc * tyc list) * int -> tyc = tc_inj o LK.TC_FIX
val tcc_wrap   : tyc -> tyc = fn tc => tc_inj (LK.TC_TOKEN(LK.wrap_token, tc))
val tcc_abs    : tyc -> tyc = tc_inj o LK.TC_ABS
val tcc_box    : tyc -> tyc = tc_inj o LK.TC_BOX 
val tcc_tuple  : tyc list -> tyc = fn ts => tc_inj (LK.TC_TUPLE (rfc_tmp, ts))
val tcc_arrow  : fflag * tyc list * tyc list -> tyc = LK.tcc_arw

(** tyc deconstructors *)
val tcd_var    : tyc -> index * int = fn tc =>
      (case tc_out tc of LK.TC_VAR x => x
                       | _ => bug "unexpected tyc in tcd_var")  
val tcd_nvar   : tyc -> tvar = fn tc =>
      (case tc_out tc of LK.TC_NVAR x => x
                       | _ => bug "unexpected tyc in tcd_nvar")  
val tcd_prim   : tyc -> primtyc = fn tc =>
      (case tc_out tc of LK.TC_PRIM x => x
                       | _ => bug "unexpected tyc in tcd_prim")  
val tcd_fn     : tyc -> tkind list * tyc = fn tc =>
      (case tc_out tc of LK.TC_FN x => x
                       | _ => bug "unexpected tyc in tcd_fn")  
val tcd_app    : tyc -> tyc * tyc list = fn tc =>
      (case tc_out tc of LK.TC_APP x => x
                       | _ => bug "unexpected tyc in tcd_app")  
val tcd_seq    : tyc -> tyc list = fn tc =>
      (case tc_out tc of LK.TC_SEQ x => x
                       | _ => bug "unexpected tyc in tcd_seq")  
val tcd_proj   : tyc -> tyc * int = fn tc =>
      (case tc_out tc of LK.TC_PROJ x => x
                       | _ => bug "unexpected tyc in tcd_proj")  
val tcd_sum    : tyc -> tyc list = fn tc =>
      (case tc_out tc of LK.TC_SUM x => x
                       | _ => bug "unexpected tyc in tcd_sum")  
val tcd_fix    : tyc -> (int * tyc * tyc list) * int = fn tc =>
      (case tc_out tc of LK.TC_FIX x => x
                       | _ => bug "unexpected tyc in tcd_fix")  
val tcd_wrap   : tyc -> tyc = fn tc => 
      (case tc_out tc 
        of LK.TC_TOKEN(tk, x) => 
             if LK.token_eq(tk, LK.wrap_token) then x
             else bug "unexpected token tyc in tcd_wrap"
         | _ => bug "unexpected regular tyc in tcd_wrap")
val tcd_abs    : tyc -> tyc = fn tc =>
      (case tc_out tc of LK.TC_ABS x => x
                       | _ => bug "unexpected tyc in tcd_abs")  
val tcd_box    : tyc -> tyc = fn tc =>
      (case tc_out tc of LK.TC_BOX x => x
                       | _ => bug "unexpected tyc in tcd_box")  
val tcd_tuple  : tyc -> tyc list = fn tc =>
      (case tc_out tc of LK.TC_TUPLE (_,x) => x
                       | _ => bug "unexpected tyc in tcd_tuple")  
val tcd_arrow  : tyc -> fflag * tyc list * tyc list = fn tc => 
      (case tc_out tc of LK.TC_ARROW x => x
                       | _ => bug "unexpected tyc in tcd_arrow")  

(** tyc predicates *)
val tcp_var    : tyc -> bool = fn tc => 
      (case tc_out tc of LK.TC_VAR _ => true | _ => false)
val tcp_nvar   : tyc -> bool = fn tc => 
      (case tc_out tc of LK.TC_NVAR _ => true | _ => false)
val tcp_prim   : tyc -> bool = fn tc => 
      (case tc_out tc of LK.TC_PRIM _ => true | _ => false)
val tcp_fn     : tyc -> bool = fn tc => 
      (case tc_out tc of LK.TC_FN _ => true | _ => false)
val tcp_app    : tyc -> bool = fn tc => 
      (case tc_out tc of LK.TC_APP _ => true | _ => false)
val tcp_seq    : tyc -> bool = fn tc => 
      (case tc_out tc of LK.TC_SEQ _ => true | _ => false)
val tcp_proj   : tyc -> bool = fn tc => 
      (case tc_out tc of LK.TC_PROJ _ => true | _ => false)
val tcp_sum    : tyc -> bool = fn tc => 
      (case tc_out tc of LK.TC_SUM _ => true | _ => false)
val tcp_fix    : tyc -> bool = fn tc => 
      (case tc_out tc of LK.TC_FIX _ => true | _ => false)
val tcp_wrap   : tyc -> bool = fn tc =>
      (case tc_out tc of LK.TC_TOKEN (tk, _) => LK.token_eq(tk, LK.wrap_token)
                       | _ => false)
val tcp_abs    : tyc -> bool = fn tc => 
      (case tc_out tc of LK.TC_ABS _ => true | _ => false)
val tcp_box    : tyc -> bool = fn tc => 
      (case tc_out tc of LK.TC_BOX _ => true | _ => false)
val tcp_tuple  : tyc -> bool = fn tc => 
      (case tc_out tc of LK.TC_TUPLE _ => true | _ => false)
val tcp_arrow  : tyc -> bool = fn tc => 
      (case tc_out tc of LK.TC_ARROW _ => true | _ => false)

(** tyc one-arm switches *)
fun tcw_var (tc, f, g) = 
      (case tc_out tc of LK.TC_VAR x => f x | _ => g tc)
fun tcw_nvar (tc, f, g) = 
      (case tc_out tc of LK.TC_NVAR x => f x | _ => g tc)
fun tcw_prim (tc, f, g) = 
      (case tc_out tc of LK.TC_PRIM x => f x | _ => g tc) 
fun tcw_fn (tc, f, g) = 
      (case tc_out tc of LK.TC_FN x => f x | _ => g tc)   
fun tcw_app (tc, f, g) = 
      (case tc_out tc of LK.TC_APP x => f x | _ => g tc)  
fun tcw_seq (tc, f, g) = 
      (case tc_out tc of LK.TC_SEQ x => f x | _ => g tc)  
fun tcw_proj (tc, f, g) = 
      (case tc_out tc of LK.TC_PROJ x => f x | _ => g tc) 
fun tcw_sum (tc, f, g) = 
      (case tc_out tc of LK.TC_SUM x => f x | _ => g tc)  
fun tcw_fix (tc, f, g) = 
      (case tc_out tc of LK.TC_FIX x => f x | _ => g tc)  
fun tcw_wrap (tc, f, g) = 
      (case tc_out tc 
        of LK.TC_TOKEN(rk, x) => 
             if LK.token_eq(rk, LK.wrap_token) then f x else g tc
         | _ => g tc)  
fun tcw_abs (tc, f, g) = 
      (case tc_out tc of LK.TC_ABS x => f x | _ => g tc)  
fun tcw_box (tc, f, g) = 
      (case tc_out tc of LK.TC_BOX x => f x | _ => g tc)  
fun tcw_tuple (tc, f, g) = 
      (case tc_out tc of LK.TC_TUPLE (_,x) => f x | _ => g tc)
fun tcw_arrow (tc, f, g) = 
      (case tc_out tc of LK.TC_ARROW x => f x | _ => g tc)


(* 
 * FLINT lty is roughly equivalent to the following ML datatype 
 *
 *    datatype lty
 *      = LT_TYC of tyc
 *      | LT_STR of lty list
 *      | LT_FCT of lty list * lty list
 *      | LT_POLY of tkind list * lty list
 *
 * We treat lty as an abstract type so we can no longer use pattern
 * matching. The client does not need to worry about whether an lty
 * is in normal form or not. 
 *)

(** lty constructors *)
val ltc_tyc    : tyc -> lty = lt_inj o LK.LT_TYC
val ltc_str    : lty list -> lty = lt_inj o LK.LT_STR
val ltc_fct    : lty list * lty list -> lty = lt_inj o LK.LT_FCT
val ltc_poly   : tkind list * lty list -> lty = lt_inj o LK.LT_POLY

(** lty deconstructors *)
val ltd_tyc    : lty -> tyc = fn lt => 
      (case lt_out lt of LK.LT_TYC x => x
                       | _ => bug "unexpected lty in ltd_tyc")
val ltd_str    : lty -> lty list = fn lt => 
      (case lt_out lt of LK.LT_STR x => x
                       | _ => bug "unexpected lty in ltd_str")
val ltd_fct    : lty -> lty list * lty list = fn lt => 
      (case lt_out lt of LK.LT_FCT x => x
                       | _ => bug "unexpected lty in ltd_fct")
val ltd_poly   : lty -> tkind list * lty list = fn lt => 
      (case lt_out lt of LK.LT_POLY x => x
                       | _ => bug "unexpected lty in ltd_poly")

(** lty predicates *)
val ltp_tyc    : lty -> bool = fn lt =>
      (case lt_out lt of LK.LT_TYC _ => true | _ => false)
val ltp_str    : lty -> bool = fn lt =>
      (case lt_out lt of LK.LT_STR _ => true | _ => false)
val ltp_fct    : lty -> bool = fn lt =>
      (case lt_out lt of LK.LT_FCT _ => true | _ => false)
val ltp_poly   : lty -> bool = fn lt =>
      (case lt_out lt of LK.LT_POLY _ => true | _ => false)

(** lty one-arm switches *)
fun ltw_tyc (lt, f, g) = 
      (case lt_out lt of LK.LT_TYC x => f x | _ => g lt)
fun ltw_str (lt, f, g) = 
      (case lt_out lt of LK.LT_STR x => f x | _ => g lt)
fun ltw_fct (lt, f, g) = 
      (case lt_out lt of LK.LT_FCT x => f x | _ => g lt)
fun ltw_poly (lt, f, g) = 
      (case lt_out lt of LK.LT_POLY x => f x | _ => g lt)


(* 
 * Because FLINT tyc is embedded inside FLINT lty, we supply the
 * the following utility functions on building ltys that are based
 * on simple mono tycs.
 *)

(** tyc-lty constructors *)
val ltc_var    : index * int -> lty = ltc_tyc o tcc_var
val ltc_prim   : primtyc -> lty = ltc_tyc o tcc_prim
val ltc_tuple  : lty list -> lty = ltc_tyc o (tcc_tuple o (map ltd_tyc))
val ltc_arrow  : fflag * lty list * lty list -> lty = fn (r, t1, t2) => 
  let val ts1 = map ltd_tyc t1
      val ts2 = map ltd_tyc t2
   in ltc_tyc (tcc_arrow(r, ts1, ts2))
  end

(** tyc-lty deconstructors *)
val ltd_var    : lty -> index * int = tcd_var o ltd_tyc
val ltd_prim   : lty -> primtyc = tcd_prim o ltd_tyc
val ltd_tuple  : lty -> lty list = (map ltc_tyc) o (tcd_tuple o ltd_tyc)
val ltd_arrow  : lty -> fflag * lty list * lty list = fn t =>
  let val (r, ts1, ts2) = tcd_arrow (ltd_tyc t)
   in (r, map ltc_tyc ts1, map ltc_tyc ts2)
  end

(** tyc-lty predicates *)
val ltp_var    : lty -> bool = fn t =>
  (case lt_out t of LK.LT_TYC x => tcp_var x | _ => false)
val ltp_prim   : lty -> bool = fn t =>
  (case lt_out t of LK.LT_TYC x => tcp_prim x | _ => false)
val ltp_tuple  : lty -> bool = fn t =>
  (case lt_out t of LK.LT_TYC x => tcp_tuple x | _ => false)
val ltp_arrow  : lty -> bool = fn t =>
  (case lt_out t of LK.LT_TYC x => tcp_arrow x | _ => false)

(** tyc-lty one-arm switches *)
fun ltw_var (lt, f, g) = 
  (case lt_out lt 
    of LK.LT_TYC tc => 
         (case tc_out tc of LK.TC_VAR x => f x | _ => g lt)
     | _ => g lt)

fun ltw_prim (lt, f, g) = 
  (case lt_out lt 
    of LK.LT_TYC tc => 
         (case tc_out tc of LK.TC_PRIM x => f x | _ => g lt)
     | _ => g lt)

fun ltw_tuple (lt, f, g) = 
  (case lt_out lt 
    of LK.LT_TYC tc => 
         (case tc_out tc of LK.TC_TUPLE (_, x) => f x | _ => g lt)
     | _ => g lt)

fun ltw_arrow (lt, f, g) = 
  (case lt_out lt 
    of LK.LT_TYC tc => 
         (case tc_out tc of LK.TC_ARROW x => f x | _ => g lt)
     | _ => g lt)


(* 
 * The following functions are written for CPS only. If you are writing
 * writing code for FLINT, you should not use any of these functions. 
 * The continuation referred here is the internal continuation introduced
 * via CPS conversion; it is different from the source-level continuation 
 * ('a cont) or control continuation ('a control-cont) where are represented 
 * as PT.ptc_cont and PT.ptc_ccont respectively. 
 *
 *)

(** cont-tyc-lty constructors *)
val tcc_cont   : tyc list -> tyc = tc_inj o LK.TC_CONT
val ltc_cont   : lty list -> lty = lt_inj o LK.LT_CONT

(** cont-tyc-lty deconstructors *)
val tcd_cont   : tyc -> tyc list = fn tc => 
      (case tc_out tc of LK.TC_CONT x => x
                       | _ => bug "unexpected tyc in tcd_cont")  
val ltd_cont   : lty -> lty list = fn lt => 
      (case lt_out lt of LK.LT_CONT x => x 
                       | _ => bug "unexpected lty in ltd_cont")

(** cont-tyc-lty predicates *)
val tcp_cont   : tyc -> bool = fn tc => 
      (case tc_out tc of LK.TC_CONT _ => true | _ => false)
val ltp_cont   : lty -> bool = fn lt => 
      (case lt_out lt of LK.LT_CONT _ => true | _ => false)

(** cont-tyc-lty one-arm switches *)
fun tcw_cont (tc, f, g) = 
      (case tc_out tc of LK.TC_CONT x => f x | _ => g tc) 
fun ltw_cont (lt, f, g) = 
      (case lt_out lt of LK.LT_CONT x => f x | _ => g lt)



(* 
 * The following functions are written for PLambdaType only. If you
 * are writing code for FLINT only, don't use any of these functions. 
 * The idea is that in PLambda, all (value or type) functions have single
 * argument and single return-result. Ideally, we should define 
 * another sets of datatypes for tycs and ltys. But we want to avoid
 * the translation from PLambdaType to FLINT types, so we let them
 * share the same representations as much as possible. 
 *
 * Ultimately, LtyDef should be separated into two files: one for 
 * FLINT, another for PLambda, but we will see if this is necessary.
 *
 *)

(*  
 * The implementation here is TEMPORARY; Stefan needs to take a look at 
 * this. Note parrow could share the representation with arrow if there 
 * is one-to-one mapping between parrow and arrow.
 *)

(** plambda tyc-lty constructors *)
val tcc_parrow : tyc * tyc -> tyc =    
  fn (x, y) => tcc_arrow(ffc_var (false, false), [x], [y])
val ltc_parrow : lty * lty -> lty =
  fn (x, y) => ltc_tyc (tcc_parrow (ltd_tyc x, ltd_tyc y))
val ltc_ppoly  : tkind list * lty -> lty = fn (ks, t) => ltc_poly(ks, [t]) 
val ltc_pfct   : lty * lty -> lty = fn (x, y) => ltc_fct ([x], [y])

(** plambda tyc-lty deconstructors *)
val tcd_parrow : tyc -> tyc * tyc = fn tc =>   
  (case tc_out tc 
    of LK.TC_ARROW (_, xs, ys) => (LK.tc_autotuple xs, LK.tc_autotuple ys)
     | _ => bug "unexpected tyc in tcd_parrow")
val ltd_parrow : lty -> lty * lty = fn t =>
  let val (t1, t2) = tcd_parrow (ltd_tyc t)
   in (ltc_tyc t1, ltc_tyc t2)
  end
val ltd_ppoly  : lty -> tkind list * lty = fn t =>
  let val (ks, ts) = ltd_poly t
   in case ts of [x] => (ks, x)
               | _ => bug "unexpected case in ltd_ppoly"
  end
val ltd_pfct   : lty -> lty * lty = fn t =>
  let val (ts1, ts2) = ltd_fct t
   in case (ts1, ts2) of ([x], [y]) => (x, y)
                       | _ => bug "unexpected case in ltd_pfct"
  end

(** plambda tyc-lty predicates *)
val tcp_parrow : tyc -> bool = fn tc =>  
  (case tc_out tc of LK.TC_ARROW (_, [x], [y]) => true | _ => false)
val ltp_parrow : lty -> bool = fn t => 
  (case lt_out t of LK.LT_TYC x => tcp_parrow x | _ => false)
val ltp_ppoly  : lty -> bool = fn t =>
  (case lt_out t of LK.LT_POLY (_, [x]) => true | _ => false)
val ltp_pfct   : lty -> bool = fn t =>
  (case lt_out t of LK.LT_FCT ([x], [y]) => true | _ => false)

(** plambda tyc-lty one-arm switches *)
fun tcw_parrow (tc, f, g) =     
  (case tc_out tc of LK.TC_ARROW (_,[x],[y]) => f (x,y) | _ => g tc)
fun ltw_parrow (lt, f, g) = 
  (case lt_out lt
    of LK.LT_TYC tc => 
         (case tc_out tc of LK.TC_ARROW (_,[x],[y]) => f(x,y) | _ => g lt)
     | _ => g lt)
fun ltw_ppoly (lt, f, g) =
  (case lt_out lt of LK.LT_POLY(ks,[x]) => f(ks,x) | _ => g lt)
fun ltw_pfct (lt, f, g) =
  (case lt_out lt of LK.LT_FCT([x],[y]) => f(x,y) | _ => g lt)

end (* top-level local *)
end (* structure LtyDef *)


(*
 * $Log: ltydef.sml,v $
 * Revision 1.4  1998/12/22 17:01:45  jhr
 *   Merged in 110.10 changes from Yale.
 *
 *)

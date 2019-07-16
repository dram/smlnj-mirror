(* Copyright 1996 by AT&T Bell Laboratories *)
(* lambdatype.sml *)

structure LambdaType : LAMBDATYPE = 
struct

local structure PT = PrimTyc
      structure DI = DebIndex
      open LtyKernel
in

fun bug msg = ErrorMsg.impossible("LambdaType: "^msg)
val say = Control.Print.say

type tkind = tkind
type tyc = tyc
type lty = lty

(** constructing a tkind value *)
val tkc_mono = tk_inj (TK_TYC)
val tkc_mobx = tk_inj (TK_TBX)
fun tkc_args n = 
  let fun h (n, r) = if n < 1 then r else h(n-1, tkc_mono::r)
   in h(n, [])
  end
val tkc_seqs = tk_inj o TK_SEQ
val tkc_fcts = tk_inj o TK_FUN

val tkc_fn1 = tkc_fcts(tkc_seqs(tkc_args 1), tkc_mono)
val tkc_fn2 = tkc_fcts(tkc_seqs(tkc_args 2), tkc_mono)
val tkc_fn3 = tkc_fcts(tkc_seqs(tkc_args 3), tkc_mono)

fun tkc_funs 0 = tkc_mono
  | tkc_funs 1 = tkc_fn1
  | tkc_funs 2 = tkc_fn2
  | tkc_funs 3 = tkc_fn3
  | tkc_funs i = tkc_fcts(tkc_seqs(tkc_args i), tkc_mono)

(*

(** decomposing a tkind value *)
fun tkd_seqs x = 
  (case (tk_out x) 
    of TK_SEQ s => s
     | _ => bug "unexpected case in tkd_seqs")

fun tkd_fcts x = 
  (case (tk_out x) 
    of TK_FUN s => s
     | _ => bug "unexpected case in tkd_fcts")

(** testing a tkind value *)
fun tkp_mono x = (case (tk_out x) of (TK_TYC | TK_TBX) => true | _ => false)
fun tkp_seqs x = (case (tk_out x) of TK_SEQ _ => true | _ => false)
fun tkp_fcts x = (case (tk_out x) of TK_FUN _ => true | _ => false)

*)

(** constructing a tyc value *)
val tcc_var  = tc_inj o TC_VAR
val tcc_prim = tc_inj o TC_PRIM
val tcc_fn   = tc_inj o TC_FN
val tcc_app  = tc_inj o TC_APP
val tcc_seq  = tc_inj o TC_SEQ
val tcc_proj = tc_inj o TC_PROJ
val tcc_sum  = tc_inj o TC_SUM
val tcc_fix  = tc_inj o TC_FIX
val tcc_abs  = tc_inj o TC_ABS
val tcc_box  = tc_inj o TC_BOX
val tcc_tup  = tc_inj o TC_TUPLE
val tcc_arw  = tc_inj o TC_ARROW

val tcc_int    = tcc_prim PT.ptc_int31
val tcc_int32  = tcc_prim PT.ptc_int32
val tcc_real   = tcc_prim PT.ptc_real
val tcc_string = tcc_prim PT.ptc_string
val tcc_exn    = tcc_prim PT.ptc_exn
val tcc_void   = tcc_prim PT.ptc_void
val tcc_unit   = tcc_int
val tcc_null   = tcc_tup []

val tcc_bool   = 
  let val tsig_bool = tcc_fn ([tkc_mono], tcc_sum [tcc_null, tcc_null])
   in tcc_fix(tsig_bool, 0)
  end

val tcc_list   =  (* not exported, used for the printing purpose *)
  let val alpha = tcc_var (DI.innermost, 0)
      val tlist = tcc_var (DI.innersnd, 0)
      val alist = tcc_app (tlist, [alpha])
      val tcc_cons = tcc_tup [alpha, alist]
      val tlist = tcc_fn([tkc_mono], tcc_sum [tcc_cons, tcc_null])
                            (** the order here should be consistent with
                                that in basics/basictypes.sml **)
      val tsig_list = tcc_fn([tkc_funs 1], tlist)
   in tcc_fix(tsig_list, 0)
  end

val tcc_ref    = tcc_prim PT.ptc_ref
val tcc_array  = tcc_prim PT.ptc_array
val tcc_vector = tcc_prim PT.ptc_vector
fun tcc_tv i   = tcc_var(DI.innermost, i)
val tcc_cont = tc_inj o TC_CONT

(** printing values of tkind, tyc, lty *)
local val itos = Int.toString
      fun plist(p, []) = ""
        | plist(p, x::xs) = 
            (p x) ^ (String.concat (map (fn z => ("," ^ (p z))) xs))

      fun parw(p, (t1, t2)) = (p t1) ^ " -> " ^ (p t2)
in

fun tk_print x = 
  let fun g (TK_TYC) = "K0"
        | g (TK_TBX) = "KB0"
        | g (TK_FUN z) =  (parw(tk_print, z))
        | g (TK_SEQ zs) = "KS(" ^ (plist(tk_print, zs)) ^ ")"
   in g (tk_out x)
  end

fun tc_print x =
  let fun g (TC_VAR(i,j)) = "TV(" ^ (DI.di_print i) ^ "," ^ (itos j) ^ ")"
        | g (TC_PRIM pt) = PT.pt_print pt
        | g (TC_FN([], t)) = "TF(0," ^ (tc_print t) ^ ")"
        | g (TC_FN(ks, t)) = 
              "\\" ^ (itos (length ks)) ^ ".(" ^ (tc_print t) ^ ")"
        | g (TC_APP(t, [])) = tc_print t ^ "[]"
        | g (TC_APP(t, zs)) =
              (tc_print t) ^ "[" ^ (plist(tc_print, zs)) ^ "]"
        | g (TC_SEQ zs) = "TS(" ^ (plist(tc_print,zs)) ^ ")"
        | g (TC_PROJ (t, i)) = 
              "TP(" ^ (tc_print t) ^ "," ^ (itos i) ^ ")"
        | g (TC_SUM tcs) =  
              "TSUM(" ^ (plist(tc_print, tcs)) ^ ")"
        | g (TC_FIX (tyc, i)) = 
              if tc_eq(x,tcc_bool) then "B" 
              else if tc_eq(x,tcc_list) then "LST" 
                   else ("DT{" ^ (tc_print tyc) ^ "===" ^ (itos i) ^ "}")
        | g (TC_ABS t) = "Ax(" ^ (tc_print t) ^ ")"
        | g (TC_BOX t) = "Bx(" ^ (tc_print t) ^ ")"
        | g (TC_TUPLE zs) = "TT<" ^ (plist(tc_print, zs)) ^ ">"
        | g (TC_ARROW z) = parw(tc_print, z)
        | g (TC_CONT t) = "Cnt(" ^ (tc_print t) ^ ")"
        | g (TC_IND _) = bug "unexpected TC_IND in tc_print"
        | g (TC_ENV _) = bug "unexpected TC_ENV in tc_print"
   in g (tc_out x)
  end

fun lt_print x =
  let fun h (i, t) = "(" ^ (itos i) ^ "," ^ (lt_print t) ^ ")"

      fun g (LT_TYC t) = tc_print t
        | g (LT_STR zs) = "S{" ^ (plist(lt_print, zs)) ^ "}"
        | g (LT_PST zs) = "PS{" ^ (plist(h, zs)) ^ "}"
        | g (LT_FCT (t1,t2)) = (lt_print t1) ^ "=>" ^ (lt_print t2)
        | g (LT_POLY(ks, t)) = 
             "Q" ^ (itos (length ks)) ^ ".(" ^ (lt_print t) ^ ")"
        | g (LT_CNT t) = "CNT(" ^ (lt_print t) ^ ")"
        | g (LT_IND _) = bug "unexpected LT_IND in lt_print"
        | g (LT_ENV _) = bug "unexpected LT_ENV in lt_print"
   in g (lt_out x)
  end

end (* print local *)

(** decomposing a tyc value *)
fun tcd_abs x = 
  (case tc_out x 
    of TC_ABS tc => tc
     | _ => bug "unexpected tyc in tcd_abs")

(** constructing a lty value *)
val ltc_tyc  = lt_inj o LT_TYC
val ltc_pst  = lt_inj o LT_PST
val ltc_fct  = lt_inj o LT_FCT
val ltc_poly = lt_inj o LT_POLY

val ltc_int    = ltc_tyc tcc_int
val ltc_int32  = ltc_tyc tcc_int32
val ltc_real   = ltc_tyc tcc_real
val ltc_string = ltc_tyc tcc_string
val ltc_exn    = ltc_tyc tcc_exn
val ltc_void   = ltc_tyc tcc_void
val ltc_bool   = ltc_tyc tcc_bool
val ltc_unit   = ltc_tyc tcc_unit

fun ltc_str (* [] = ltc_unit
  | ltc_str *) x = lt_inj(LT_STR x)

val ltc_tv  = ltc_tyc o tcc_tv
val ltc_top = ltc_poly([tkc_mono], ltc_tv 0)

(** a hack for value-carrying exceptions *)
val tcc_pexn = tcc_arw(tcc_tv 0, tcc_exn) 
val ltc_pexn = ltc_poly([tkc_mono], ltc_tyc tcc_pexn)
val tcc_iexn = tcc_app(tcc_ref, [tcc_string])
val ltc_iexn = ltc_tyc tcc_iexn

fun ltd_tyc t = 
  (case (lt_out t) 
    of LT_TYC x => x 
     | _ => bug "unexpected type in ltd_tyc")

fun ltc_tup [] = ltc_unit
  | ltc_tup xs = ltc_tyc (tcc_tup (map ltd_tyc xs))

fun ltc_arw (x, y) = ltc_tyc (tcc_arw (ltd_tyc x, ltd_tyc y))
fun ltc_fun (x, y) = 
  (case (lt_out x, lt_out y) 
    of (LT_TYC _, LT_TYC _) => ltc_arw(x, y)
     | _ => ltc_fct(x, y))

fun tcd_tup x = 
  (case (tc_out x)
    of TC_TUPLE s => s
     | _ => bug "unexpected case in tcd_tup")

(** decomposing a lty value; the current implementation is not efficient *)
fun ltd_tup x = 
  (case (lt_out x)
    of LT_TYC t => map ltc_tyc (tcd_tup t)
     | _ => bug "unexpected case 2 in ltd_tup")

fun tcd_arw x = 
  (case (tc_out x)
    of TC_ARROW (s1, s2) => (s1, s2)
     | TC_CONT s => (s, tcc_void)
     | _ => (say "for the type:\n"; say (tc_print x); say "\n";
             bug "unexpected case in tcd_arw"))

fun ltd_arw x = 
  (case (lt_out x)
    of LT_TYC t => let val (s1, s2) = tcd_arw t
                    in (ltc_tyc s1, ltc_tyc s2)
                   end
     | _ => bug "unexpected case 2 in ltd_arw")

fun ltc_cont x = 
  (case (lt_out x)
    of LT_TYC t => ltc_tyc(tcc_cont t)
     | LT_CNT _ => bug "unexpected LT_CNT in ltc_cont"
     | LT_POLY _ => bug "unexpected LT_POLY in ltc_cont"
     | LT_ENV _ => bug "unexpected LT_ENV in ltc_cont"
     | _ => (lt_inj o LT_CNT) x)

(** other misc utility functions *)
fun tc_select(tc, i) = 
  let fun g (TC_TUPLE zs) = 
             ((List.nth(zs, i)) 
              handle _ => bug "wrong TC_TUPLE in tc_select")  
        | g (TC_IND _) = bug "wrong IND-TCs in tc_select"
        | g (TC_ENV _) = bug "wrong ENV-TCs in tc_select"
        | g _ = bug "wrong TCs in tc_select"

   in g(tc_out tc)
  end

fun lt_select(t, i) = 
  let fun g (LT_STR ts) = 
             ((List.nth(ts, i)) 
              handle _ => bug "incorrect LT_STR in lt_select")
        | g (LT_PST ts) = 
             let fun h [] = bug "incorrect LT_PST in lt_select"
                   | h ((j,a)::r) = if i=j then a else h r
              in h ts
             end
        | g (LT_TYC tc) = ltc_tyc(tc_select(tc, i))
        | g _ = bug "incorrect lambda types in lt_select"

   in g(lt_out t)
  end

fun lt_arrow t = 
  (case (lt_out t) 
    of (LT_FCT(t1, t2)) => (t1, t2)
     | (LT_CNT t) => (t, ltc_void)
     | _ => ltd_arw t)

fun tc_swap t = 
  (case (tc_out t)
    of TC_ARROW (s1, s2) => tcc_arw(s2, s1)
     | _ => bug "unexpected tycs in tc_swap")

fun lt_swap t = 
  (case (lt_out t)
    of (LT_POLY (ks, x)) => ltc_poly(ks, lt_swap x)
     | (LT_TYC x) => ltc_tyc(tc_swap x)
     | _ => bug "unexpected type in lt_swap")

fun lt_merge(t1, t2) = 
  let fun g (LT_PST ts1, LT_PST ts2) = LT_PST(h(ts1, ts2))
        | g (LT_PST _, x2) = x2
             (* 
             if lt_eqv(t1, t2) then x2 
             else bug "incompatible PST and STR types in lt_merge"
             *)
        | g (x1, _) = x1
             (*
             if lt_eqv(t1, t2) then x1 
             else bug "incompatible STR and PST types in lt_merge"
              *)

      and h ([], []) = []
        | h (x, []) = x
        | h ([], y) = y
        | h (x as ((i,t)::l), y as ((j,s)::r)) = 
             if i < j then ((i,t)::(h(l,y)))
             else if i > j then ((j,s)::(h(x,r)))
                  else ((i, lt_merge(t,s))::(h(l,r)))

   in lt_inj(g(lt_out t1, lt_out t2))
  end

fun tc_size tc = 
  let fun g (TC_TUPLE []) = 1
        | g (TC_TUPLE xs) = foldr (op +) 0 (map tc_size xs)
        | g _ = 1

   in g(tc_out tc)
  end

fun lt_size t = 
  let fun g (LT_STR []) = 1
        | g (LT_STR ts) = foldr (op +) 0 (map lt_size ts)
        | g (LT_TYC tc) = tc_size tc
        | g _ = 1

   in g(lt_out t)
  end

fun tc_length tc = 
  let fun g (TC_TUPLE []) = 1
        | g (TC_TUPLE xs) = length xs
        | g _ = 1

   in g(tc_out tc)
  end

(* new version from Zhong, 10/25/97.  LT_STR [] type can occur now *)
fun lt_length t = 
  let fun g (LT_STR ts) = length ts
	| g (LT_TYC tc) = tc_length tc
	| g _ = 1
   in g(lt_out t)
  end
(* old
fun lt_length t = 
  let fun g (LT_STR []) = bug "empty STR in lt_length"
        | g (LT_STR ts) = length ts
        | g (LT_TYC tc) = tc_length tc
        | g _ = 1

   in g(lt_out t)
  end
*)
(* val tc_apply : tyc * tyc list * env -> tyc *)
fun tc_apply _ = bug "tc_apply has not been implemented yet."

(* val lt_apply : lty * tyc list * env -> lty *)
fun lt_apply (t, ts) = bug "lt_apply has not been implemented yet."

end (* top-level local *)
end (* structure LambdaType *)

(*
 * $Log: lambdatype.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.6  1997/10/26 23:10:06  dbm
 *   Fix from Zhong for lt_length nil case.
 *
 * Revision 1.5  1997/09/23  03:45:45  dbm
 *   Fix for function ltc_str (Zhong).
 *
 * Revision 1.4  1997/05/05  19:59:38  george
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
 * Revision 1.2  1997/02/26  21:45:26  george
 *    Cosmetic changes on priting TC_APP tyc
 *
 *)

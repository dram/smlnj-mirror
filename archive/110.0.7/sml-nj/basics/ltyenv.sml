(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* ltyenv.sml *)

structure LtyEnv : LTY_ENV = 
struct 

local structure DA = Access
      structure DI = DebIndex
      structure LT = LambdaType
      structure PO = PrimOp
      structure PT = PrimTyc
      open LtyKernel
in 

type tkind = tkind
type tkindEnv = tkindEnv
type tyc = tyc
type lty = lty

fun bug s = ErrorMsg.impossible ("LtyEnv: " ^ s)
val say = Control.Print.say
fun fromto(i,j) = if i < j then (i::fromto(i+1,j)) else []

fun option(NONE) = false
  | option(SOME _) = true

fun exists(p, a::r) = if p a then true else exists(p, r)
  | exists(p, []) = false

fun opList l = exists(option, l)

(***************************************************************************
 *                 UTILITY FUNCTIONS ON TC_ENV and LT_ENV                  *
 ***************************************************************************)

(** normalizing an arbitrary tyc into a tyc with simple-top-level *)
fun tcLazyRd t = 
let fun g x = 
      (case tc_out x
        of TC_IND (tc, _) => g tc
         | TC_ENV(tc, i, j, te) => 
             let val ntc = g(h(tc, i, j, te))
              in tyc_upd(x, ntc); ntc
             end
         | _ => x)

    and h(x, 0, 0, _) = g x
      | h(x, ol, nl, tenv) = 
         let fun prop z = tcc_env(z, ol, nl, tenv)
          in (case tc_out x
               of TC_VAR (i,j) => 
                   if (i <= ol) then
                     (let val et = tcLookup(i, tenv)
                       in case et 
                           of (NONE, n) => LT.tcc_var(nl - n, j)
                            | (SOME ts, n) => 
                                (let val y = List.nth(ts, j) 
                                             handle _ => raise tcUnbound
                                  in h(y, 0, nl - n, initTycEnv)
                                 end)
                      end)
                   else LT.tcc_var(i-ol+nl, j)
                | TC_PRIM p => x
                | TC_FN (ks, tc) => 
                   let val tenv' = tcInsert(tenv, (NONE, nl))
                    in LT.tcc_fn(ks, tcc_env(tc, ol+1, nl+1, tenv'))
                   end
                | TC_APP (tc, tcs) => LT.tcc_app(prop tc, map prop tcs)
                | TC_SEQ tcs => LT.tcc_seq (map prop tcs)
                | TC_PROJ (tc, i) => LT.tcc_proj(prop tc, i)
                | TC_SUM tcs => LT.tcc_sum (map prop tcs)
                | TC_FIX (tc, i) => LT.tcc_fix(prop tc, i)
                | TC_ABS tc => LT.tcc_abs (prop tc)
                | TC_BOX tc => LT.tcc_box (prop tc)
                | TC_TUPLE tcs => LT.tcc_tup (map prop tcs)
                | TC_ARROW (tc1, tc2) => LT.tcc_arw (prop tc1, prop tc2)
                | TC_CONT tc => bug "unexpected TC_CONT in tcLazyRd"
                | TC_IND (tc, _) => h(tc, ol, nl, tenv)
                | TC_ENV(tc, ol', nl', tenv') => 
                   if ol = 0 then h(tc, ol', nl+nl', tenv')
                   else h(g x, ol, nl, tenv))
         end (* function h *)

in if tcp_norm(t) then t else g t
end (* function tcLazyRd *)

(** normalizing an arbitrary lty into an lty with simple-top-level *)
fun ltLazyRd t = 
let fun g x = 
      (case lt_out x
        of LT_IND (lt, _) => g lt
         | LT_ENV(lt, i, j, te) => 
             let val nlt = g(h(lt, i, j, te))
              in lty_upd(x, nlt); nlt
             end
         | _ => x)

    and h(x, 0, 0, _) = g x
      | h(x, ol, nl, tenv) = 
         let fun prop z = ltc_env(z, ol, nl, tenv)
          in (case lt_out x
               of LT_TYC tc => LT.ltc_tyc (tcc_env(tc, ol, nl, tenv))
                | LT_STR ts => LT.ltc_str (map prop ts)
                | LT_PST its => LT.ltc_pst (map (fn (i, t) => (i, prop t)) its)
                | LT_FCT (t1, t2) => LT.ltc_fct(prop t1, prop t2)
                | LT_POLY (ks, t) => 
                   let val tenv' = tcInsert(tenv, (NONE, nl))
                    in LT.ltc_poly(ks, ltc_env(t, ol+1, nl+1, tenv'))
                   end
                | LT_CNT t => bug "unexpected LT_CNT in ltLazyRd"
                | LT_IND (t, _) => h(t, ol, nl, tenv)
                | LT_ENV (lt, ol', nl', tenv') => 
                   if ol = 0 then h(lt, ol', nl+nl', tenv')
                   else h(g x, ol, nl, tenv))
         end (* function h *)
in if ltp_norm(t) then t else g t
end (* function ltLazyRd *)

fun stripInd t = (case tc_out t of TC_IND (x,_) => stripInd x | _ => t)
 
(** normalizing an arbitrary tyc into a simple weak-head-normal-form *)
fun tcWhNorm t = if tcp_norm(t) then t else 
  let val nt = tcLazyRd t
   in case (tc_out nt)
       of TC_APP(tc, tcs) =>
            (let val tc' = tcWhNorm tc
              in case (tc_out tc')
                  of TC_FN(ks, b) =>  
                       let fun base () = 
                             (b, 1, 0, tcInsert(initTycEnv,(SOME tcs, 0)))
                           val sp = 
                             (case tc_out b
                               of TC_ENV(b', ol', nl', te') => 
                                    (case tcSplit te'
                                      of SOME((NONE, n), te) =>
                                           if (n = nl'-1) andalso (ol' > 0)
                                           then (b', ol', n, 
                                                 tcInsert(te, (SOME tcs, n)))
                                           else base()
                                       | _ => base())
                                | _ => base())
                           val res = tcWhNorm(tcc_env sp)
                        in tyc_upd(nt, res); res
                       end
                   | ((TC_SEQ _) | (TC_TUPLE _) | (TC_ARROW _) | (TC_IND _)) =>
                       bug "unexpected tycs in tcWhNorm-TC_APP"
                   | _ => let val xx = LT.tcc_app(tc', tcs) 
                           in stripInd xx
                          end
(*
                       let fun h x = 
                             let val nx = tcWhNorm x
                              in (case tc_out nx
                                   of TC_BOX z => h z
                                    | TC_ABS z => h z
                                    | _ => nx)
                             end
                        in LT.tcc_app(tc', map h tcs)
                       end
*)
             end)
        | TC_PROJ(tc, i) =>
            (let val tc' = tcWhNorm tc
              in (case (tc_out tc')
                   of (TC_SEQ tcs) => 
                        let val res = List.nth(tcs, i)
                                      handle _ => bug "TC_SEQ in tcWhNorm"
                            val nres = tcWhNorm res
                         in tyc_upd(nt, nres); nres
                        end
                    | ((TC_PRIM _) | (TC_FIX _) | (TC_FN _) | (TC_SUM _) |
                       (TC_ARROW _) | (TC_ABS _) | (TC_BOX _) | (TC_IND _) |
                       (TC_TUPLE _)) =>
                         bug "unexpected tycs in tcWhNorm-TC_PROJ"
                    | _ => let val xx = LT.tcc_proj(tc', i)
                            in stripInd xx
                           end)

             end)
        | TC_IND (tc, _) => tcWhNorm tc
        | TC_ENV _ => bug "unexpected TC_ENV in tcWhNorm"
        | _ => nt
  end (* function tcWhNorm *)

(** normalizing an arbitrary lty into the simple weak-head-normal-form *)
fun ltWhNorm t = if ltp_norm(t) then t else 
  let val nt = ltLazyRd t
   in case (lt_out nt)
       of LT_TYC tc => LT.ltc_tyc(tcWhNorm tc)
        | _ => nt
  end (* function ltWhNorm *)

(** normalizing an arbitrary tyc into the standard normal form *)
fun tcNormRd t = if (tcp_norm t) then t else
  let val nt = tcWhNorm t
   in if (tcp_norm nt) then nt
      else
        (let val res = 
              (case (tc_out nt)
                of TC_FN (ks, tc) => LT.tcc_fn(ks, tcNormRd tc)
                 | TC_APP (tc, tcs) => 
                     LT.tcc_app(tcNormRd tc, map tcNormRd tcs)
                 | TC_SEQ tcs => LT.tcc_seq (map tcNormRd tcs)
                 | TC_PROJ (tc, i) => LT.tcc_proj(tcNormRd tc, i)
                 | TC_SUM tcs => LT.tcc_sum (map tcNormRd tcs)
                 | TC_FIX (tc, i) => LT.tcc_fix(tcNormRd tc, i)
                 | TC_ABS tc => LT.tcc_abs (tcNormRd tc)
                 | TC_BOX tc => LT.tcc_box (tcNormRd tc)
                 | TC_TUPLE tcs => LT.tcc_tup (map tcNormRd tcs)
                 | TC_ARROW (tc1, tc2) => 
                     LT.tcc_arw (tcNormRd tc1, tcNormRd tc2)
                 | TC_IND (tc, _) => tcNormRd tc
                 | TC_ENV _ => bug "unexpected tycs in tcNormRd"
                 | _ => nt)
          in tyc_upd(nt, res); res
         end)
  end (* function tcNormRd *)

(** normalizing an arbitrary lty into the standard normal form *)
fun ltNormRd t = if (ltp_norm t) then t else 
  let val nt = ltLazyRd t
   in if (ltp_norm nt) then nt
      else 
        (let val res = 
              (case lt_out nt
                of LT_TYC tc => LT.ltc_tyc (tcNormRd tc)
                 | LT_STR ts => LT.ltc_str (map ltNormRd ts)
                 | LT_PST its => 
                     LT.ltc_pst (map (fn (i, t) => (i, ltNormRd t)) its)
                 | LT_FCT (t1, t2) => LT.ltc_fct(ltNormRd t1, ltNormRd t2)
                 | LT_POLY (ks, t) => LT.ltc_poly(ks, ltNormRd t)
                 | LT_IND (lt, _) => ltNormRd lt
                 | _ => bug "unexpected ltys in ltNormRd")
          in lty_upd(nt, res); res
         end)
  end (* function ltNormRd *)

(***************************************************************************
 *                 UTILITY FUNCTIONS ON ltyEnv                             *
 ***************************************************************************)
(** utility values and functions on ltyEnv *)
type ltyEnv = (lty * DebIndex.depth) IntmapF.intmap

exception ltUnbound
val initLtyEnv : ltyEnv = IntmapF.empty

fun ltLookup (venv, lv, nd) = 
  let val (lt, d) = (IntmapF.lookup venv lv) handle _ => 
                        (say "**** hmmm, I didn't find the variable ";
                         say (Int.toString lv); say "\n";
                         raise ltUnbound)
   in if d=nd then lt
      else if d > nd then bug "unexpected depth info in ltLookup"
           else ltc_env(lt, 0, nd - d, initTycEnv)
  end

fun ltInsert (venv, lv, lt, d) = IntmapF.add(venv, lv, (lt, d))

(***************************************************************************
 *                 UTILITY FUNCTIONS ON TKIND CHECKING                     *
 ***************************************************************************)
fun app2(f, [], []) = ()
  | app2(f, a::r, b::z) = (f(a, b); app2(f, r, z))
  | app2(f, _, _) = bug "unexpected list arguments in function app2"

exception TkTycChk
fun tkSel (tk, i) = 
  (case (tk_out tk)
    of (TK_SEQ ks) => (List.nth(ks, i) handle _ => raise TkTycChk)
     | _ => raise TkTycChk)

fun tkApp (tk1, tk2) = 
  (case (tk_out tk1)
    of TK_FUN(a, b) => if tk_eq(a, tk2) then b else raise TkTycChk
     | _ => raise TkTycChk)

fun tkTyc (t, kenv) = 
  let fun g x = 
        (case tc_out x
          of (TC_VAR (i, j)) => tkLookup(kenv, i, j)
           | (TC_PRIM pt) => LT.tkc_funs (PrimTyc.pt_arity pt)
           | (TC_FN(ks, tc)) => 
               LT.tkc_fcts(LT.tkc_seqs ks, tkTyc(tc, tkInsert(kenv, ks)))
           | (TC_APP (tc, tcs)) => tkApp(g tc, LT.tkc_seqs(map g tcs))
           | (TC_SEQ tcs) => LT.tkc_seqs (map g tcs)
           | (TC_PROJ(tc, i)) => tkSel(g tc, i)
           | (TC_SUM tcs) => 
               let val _ = map (fn x => tk_eq(g x, LT.tkc_mono)) tcs
                in LT.tkc_mono
               end
           | (TC_FIX (tc, i)) =>
               let val k = g tc
                in (case (tk_out k)
                     of TK_FUN(a, b) => 
                          if tk_eq(a, b) then tkSel(a, i)
                          else raise TkTycChk
                      | _ => raise TkTycChk)
               end
           | (TC_ABS tc) => (tk_eq(g tc, LT.tkc_mono); LT.tkc_mono)
           | (TC_BOX tc) => (tk_eq(g tc, LT.tkc_mono); LT.tkc_mono)
           | (TC_TUPLE tcs) => 
               let val _ = map (fn x => tk_eq(g x, LT.tkc_mono)) tcs
                in LT.tkc_mono
               end
           | (TC_ARROW (tc1, tc2)) =>
               let val _ = tk_eq(g tc1, LT.tkc_mono)
                   val _ = tk_eq(g tc2, LT.tkc_mono) 
                in LT.tkc_mono
               end
           | _ => bug "unexpected TC_ENV or TC_CONT in tkTyc")
   in g t 
  end

and tkChk kenv (k, tc) = 
  if tk_eq(k, tkTyc(tcNormRd tc, kenv)) then () else raise TkTycChk


(***************************************************************************
 *                 UTILITY FUNCTIONS ON TYPE CHECKING                      *
 ***************************************************************************)

(** printing arbitrary tycs and ltys *)
val tcPrint : tyc -> string = LT.tc_print o tcNormRd
val ltPrint : lty -> string = LT.lt_print o ltNormRd

(** testing the equality of values of tkind, tyc, lty *)
fun eqlist (p, x::xs, y::ys) = (p(x,y)) andalso (eqlist(p, xs, ys))
  | eqlist (p, [], []) = true
  | eqlist _ = false

(** testing the equality on arbitrary tycs and ltys *)
fun tcEquiv (t1 : tyc, t2) = 
  let val t1' = tcWhNorm t1 
      val t2' = tcWhNorm t2
   in (if ((tcp_norm t1') andalso (tcp_norm t2')) then tc_eq(t1', t2') 
       else false) orelse (* a temporary hack *)
           (case (tc_out t1', tc_out t2')
             of (TC_FN(ks1, b1), TC_FN(ks2, b2)) =>
                  (eqlist(tk_eq, ks1, ks2)) andalso (tcEquiv(b1, b2))
              | (TC_APP(a1, b1), TC_APP(a2, b2)) =>
                  (tcEquiv(a1, a2)) andalso (eqlist(tcEqvBx, b1, b2))
              | (TC_SEQ ts1, TC_SEQ ts2) => eqlist(tcEquiv, ts1, ts2)
              | (TC_SUM ts1, TC_SUM ts2) => eqlist(tcEquiv, ts1, ts2)
              | (TC_TUPLE ts1, TC_TUPLE ts2) => eqlist(tcEquiv, ts1, ts2)
              | (TC_ABS a, TC_ABS b) => tcEqvBx(a, b)
              | (TC_BOX a, TC_BOX b) => tcEquiv(a, b)
              | (TC_PROJ(a1, i1), TC_PROJ(a2, i2)) =>
                  (i1 = i2) andalso (tcEquiv(a1, a2))
              | (TC_ARROW(a1, b1), TC_ARROW(a2, b2)) => 
                  (tcEquiv(a1, a2)) andalso (tcEquiv(b1, b2))
              | (TC_FIX(tc1, i1), TC_FIX(tc2, i2)) => true 
(*
                   let val (k1, c1) = List.nth(tc1, i1)
                       val (k2, c2) = List.nth(tc2, i2) 
                    in (tk_eq(k1, k2)) andalso true
     (c1 = c2) orelse (let fun mkfs (xs, n, res) = 
                              if n = 0 then res 
                              else mkfs(xs, n-1, (LT.tcc_fix(xs, n-1))::res)
                            val ts1 = mkfs(kcs1, length kcs1, [])
                            val ts2 = mkfs(kcs2, length kcs2, [])
                            val t1 = tcc_env (c1, 1, 0, [(SOME ts1, 0)])
                            val t2 = tcc_env (c2, 1, 0, [(SOME ts2, 0)])
                         in if tc_eq(tcNormRd t1, tcNormRd t2) then true
                            else (say "************************************\n";
                                  say "WARN: we can't prove these two ltys \n";
                                  say "equivalent, but have to let it go. \n";
                                  say "************************************\n";
                                  true)
                        end)
                   end
*)
              | _ => false)

  end (* function tcEquiv *)

(** testing the equality of arbitrary tycs, with relaxed constraints *)
(** used after the wrapping phase *)
and tcEqvBx (t1 : tyc, t2) = 
  let val t1' = tcWhNorm t1 
      val t2' = tcWhNorm t2
   in (if ((tcp_norm t1') andalso (tcp_norm t2')) then tc_eq(t1', t2')
       else false) orelse
      (case (tc_out t1', tc_out t2')
        of (TC_FN(ks1, b1), TC_FN(ks2, b2)) =>
             (eqlist(tk_eq, ks1, ks2)) andalso (tcEqvBx(b1, b2))
         | (TC_APP(a1, b1), TC_APP(a2, b2)) =>
             (tcEquiv(a1, a2)) andalso (eqlist(tcEqvBx, b1, b2))
         | (TC_SEQ ts1, TC_SEQ ts2) => eqlist(tcEqvBx, ts1, ts2)
         | (TC_SUM ts1, TC_SUM ts2) => eqlist(tcEqvBx, ts1, ts2)
         | (TC_TUPLE ts1, TC_TUPLE ts2) => eqlist(tcEqvBx, ts1, ts2)
         | (TC_ABS a, TC_ABS b) => tcEqvBx(a, b)
         | (TC_ABS a, _) => tcEqvBx(a, t2')
         | (_, TC_ABS b) => tcEqvBx(t1', b)
         | (TC_BOX a, TC_BOX b) => tcEqvBx(a, b)
         | (TC_BOX a, _) => tcEqvBx(a, t2')
         | (_, TC_BOX b) => tcEqvBx(t1', b)
         | (TC_PROJ(a1, i1), TC_PROJ(a2, i2)) =>
             (i1 = i2) andalso (tcEqvBx(a1, a2))
         | (TC_ARROW(a1, b1), TC_ARROW(a2, b2)) => 
             (tcEqvBx(a1, a2)) andalso (tcEqvBx(b1, b2))
         | (TC_FIX(tc1, i1), TC_FIX(tc2, i2)) => true
(*
              let val (k1, c1) = List.nth(kcs1, i1)
                  val (k2, c2) = List.nth(kcs2, i2)
               in (tk_eq(k1, k2)) andalso true (* (c1 = c2) (* a hack *) *)
              end
*)
         | _ => false)
  end (* function tcEqvBx *)

     
fun ltEquiv (t1 : lty, t2) = 
  let val t1' = ltWhNorm t1
      val t2' = ltWhNorm t2
      fun sp (r, []) = true
        | sp (r, (i,t)::s) = 
            (if (ltEquiv(List.nth(r,i),t)) 
             then sp(r,s) else false) handle _ => false

      fun pp ([], _) = true
        | pp (_, []) = true
        | pp (a as ((i,t)::l), b as ((j,s)::r)) = 
            if i > j then pp(a,r) 
            else if i < j then pp(l,b) 
                 else if (ltEquiv(t,s)) then pp(l,r) else false 

   in (if ((ltp_norm t1') andalso (ltp_norm t2')) then lt_eqv(t1', t2') 
       else false) orelse
           (case (lt_out t1', lt_out t2')
             of (LT_POLY(ks1, b1), LT_POLY(ks2, b2)) =>
                  (eqlist(tk_eq, ks1, ks2)) andalso (ltEquiv(b1, b2))
              | (LT_FCT(a1, b1), LT_FCT(a2, b2)) => 
                  (ltEquiv(a1, a2)) andalso (ltEquiv(b1, b2))
              | (LT_TYC a, LT_TYC b) => tcEquiv(a, b)
              | (LT_STR s1, LT_STR s2) => eqlist(ltEquiv, s1, s2)
              | (LT_PST s1, LT_PST s2) => pp(s1, s2)
              | (LT_PST s1, LT_STR s2) => sp(s2, s1)
              | (LT_STR s1, LT_PST s2) => sp(s1, s2)
              | _ => false)

  end (* function ltEquiv *)

(** generating the proper type-equality functions; the whole thing is
    a big hack and of course, the int phase # is a hack also *)
fun ltEqvGen i = 
  if i < 10 then ltEquiv
  else if i < 20 then ltEquiv
       else ltEquiv
     
(** lazily applying an lty to a list of tycs, without kind verifications *) 
fun ltApply (lt, ts) = 
  let val nt = ltWhNorm lt
   in (case (lt_out nt, ts)
        of (LT_POLY(ks, b), _) => 
             ltc_env(b, 1, 0, tcInsert(initTycEnv, (SOME ts, 0)))
         | (_, []) => nt  (* ? *)
         | _ => bug "incorrect lty application in ltApply")
  end

(** strictly applying an lty to a list of tycs, without kind verifications *)
fun ltAppSt (lt, ts) = ltNormRd(ltApply(lt, ts))

(** strictly applying a tyc to a list of tycs, without kind verifications *)
fun tcAppSt (tc, ts) = tcNormRd(LT.tcc_app(tc, ts))

(** lazily applying an lty to a list of tycs, with kind verifications *) 
exception LtyAppChk 
fun ltAppChk (lt, ts, kenv) = 
  let val nt = ltWhNorm lt
   in (case (lt_out nt, ts)
        of (LT_POLY(ks, b), _) => 
              (app2(tkChk kenv, ks, ts);
               ltc_env(b, 1, 0, tcInsert(initTycEnv, (SOME ts, 0))))
         | (_, []) => nt 
         | _ => raise LtyAppChk)
  end

(** lazily finding out the arg and res of an lty *)
exception LtyArrow 
fun ltArrow lt = 
  (LT.lt_arrow(ltWhNorm lt)) handle _ => raise LtyArrow

(** lazily selecting a field from a record/structure type *)
exception LtySelect
fun ltSelect (lt, i) = 
  (LT.lt_select(ltWhNorm lt, i)) handle _ => raise LtySelect

(** build a function or functor type from a pair of arbitrary ltys *)
fun ltFun (t1, t2) = LT.ltc_fun(ltWhNorm t1, ltWhNorm t2)
fun ltTup ts = LT.ltc_tup(map ltWhNorm ts)

(** adjusting an lty or a tyc based on its depth information **)
fun adjLty (lt, d, nd) = 
  if d = nd then lt else ltNormRd(ltc_env(lt, 0, nd - d, initTycEnv))

fun adjTyc (tc, d, nd) = 
  if d = nd then tc else tcNormRd(tcc_env(tc, 0, nd - d, initTycEnv))

(** the following function is called only inside transtype.sml *)
fun adjTycSp (tc, d, nd) = 
  if d = nd then tc 
  else (let val dd = nd - d
         in tcNormRd(tcc_env(tc, 1, dd + 1, 
                             tcInsert(initTycEnv, (NONE, dd))))
        end)

(** a special lty application --- used inside the translate/specialize.sml *)
fun ltAdjSt (ks, lt, ts, dist, bnl) = 
  let fun h(abslevel, ol, nl, tenv) =
        if abslevel = 0 then ltc_env(lt, ol, nl, tenv)
        else if abslevel > 0 then 
               h(abslevel-1, ol+1, nl+1, tcInsert(tenv, (NONE, nl)))
             else bug "unexpected cases in ltAdjSt"

      val btenv = tcInsert(initTycEnv, (SOME ts, 0))
      val nt = h(dist, 1, bnl, btenv)
   in ltNormRd nt
  end

(** a special tyc application --- used inside the translate/specialize.sml *)
fun tcAdjSt(ks, tc, ts, dist, bnl) =
  let fun h(abslevel, ol, nl, tenv) =
        if abslevel = 0 then tcc_env(tc, ol, nl, tenv)
        else if abslevel > 0 then 
               h(abslevel-1, ol+1, nl+1, tcInsert(tenv, (NONE, nl)))
             else bug "unexpected cases in tcAdjSt"

      val btenv = tcInsert(initTycEnv, (SOME ts, 0))
      val nt = h(dist, 1, bnl, btenv)
   in tcNormRd nt
  end

(** sinking the lty one-level down --- used inside the specialize.sml *)
fun ltSinkSt (ks, lt, d, nd) = 
  let fun h(abslevel, ol, nl, tenv) =
        if abslevel = 0 then ltc_env(lt, ol, nl, tenv)
        else if abslevel > 0 then
               h(abslevel-1, ol+1, nl+1, tcInsert(tenv, (NONE, nl)))
             else bug "unexpected cases in ltSinkSt"
      val nt = h(nd-d, 0, 1, initTycEnv)
   in ltNormRd nt
  end

(** sinking the tyc one-level down --- used inside the specialize.sml *)
fun tcSinkSt (ks, tc, d, nd) = 
  let fun h(abslevel, ol, nl, tenv) =
        if abslevel = 0 then tcc_env(tc, ol, nl, tenv)
        else if abslevel > 0 then
               h(abslevel-1, ol+1, nl+1, tcInsert(tenv, (NONE, nl)))
             else bug "unexpected cases in ltSinkSt"
      val nt = h(nd-d, 0, 1, initTycEnv)
   in tcNormRd nt
  end


(** utility functions on figuring out the arity of a tkind *)
fun tkd_arity tk = 
  case (tk_out tk) 
   of TK_TYC => 0
    | TK_TBX => 0
    | TK_FUN(tk1, tk2) =>
        (case (tk_out tk1)
          of TK_SEQ ks => length ks
           | _ => bug "unexpected case #1 tkd_arity")
    | _ => bug "unexpected case #2 in tkd_arity"

(** abstracting over a lambda tyc *)
fun tcAbs (tc, tk) = 
  let val ar = tkd_arity tk
   in if ar = 0 then LT.tcc_abs tc
      else 
       (let val ks = LT.tkc_args ar
            val ts = map (fn x => LT.tcc_var(DI.innermost, x)) (fromto(0, ar))
            val ntc = tcc_env(tc, 0, 1, initTycEnv)
         in LT.tcc_fn(ks, LT.tcc_abs(tcAppSt(ntc, ts)))
        end)
  end

(** wrapping over a lambdatyc; assumption: arg is in normal form already *)
(** warning: this does not handle tycons of non-zero arity *)
datatype ucvinfo = SOMEB of tyc
                 | SOMEU of tyc
                 | NOTHING

fun uinfoList l = exists(fn NOTHING => false | _ => true, l)

val tcBox = LT.tcc_box 

fun tcWrap x = 
  (case (tc_out x)
    of (TC_PRIM pt) =>  
         if PT.isvoid pt then NONE else SOME (tcBox x) 
         (* if PT.unboxed pt then SOME (tcBox x) else NONE *)
         (* warning: this does not handle tycons of non-zero arity *)
     | (TC_TUPLE _ | TC_ARROW _) => SOME(ucvInfo x)
     | (TC_FN(ks, tc)) => 
         (case (tc_out tc, tcWrap tc)
           of (TC_SEQ _, NONE) => NONE
            | (TC_PRIM _, NONE) => NONE
            | (TC_FN _, _) => bug "unexpected case in tcWrap"
            | (_, NONE) => SOME(LT.tcc_fn(ks, tcBox tc))
                (** invariants: any TC_FN whose body is not TC_SEQ
                 must have a body of kind Omega; temporary hack **)
            | (_, SOME z) => SOME(LT.tcc_fn(ks, z)))
     | (TC_APP(tc, ts)) => 
         (case tcWrap tc of NONE => NONE
                          | SOME z => SOME(LT.tcc_app(z, ts)))
     | (TC_SEQ ts) => 
         (case tcsWrap ts of NONE => NONE
                           | SOME z => SOME(LT.tcc_seq z))
     | _ => NONE)

and ucvInfo x =
  (case tcUncover x
    of NOTHING => tcBox x
     | SOMEB y => y
     | SOMEU z => tcBox z)

and tcsWrap xs = 
  let fun p([], flag, bs) = if flag then SOME(rev bs) else NONE
        | p(a::r, flag, bs) = 
            (case (tcWrap a) of NONE => p(r, flag, a::bs)
                              | SOME z => p(r, true, z::bs))
   in p(xs, false, [])
  end

and ltWrap x = 
  (case lt_out x
    of LT_TYC t => (case tcWrap t
                     of NONE => NONE
                      | SOME z => SOME(LT.ltc_tyc z))
     | _ => bug "unexpected case in ltWrap")

(*** wrapping for partially-boxed representations ***)
and tcUncover x = 
  (case (tc_out x)
    of (TC_PRIM pt) => NOTHING
     | (TC_VAR _ | TC_PROJ _ | TC_ABS _) => SOMEU x
     | (TC_TUPLE ts) => 
         let val nts = map tcUncover ts
          in if (uinfoList nts) then 
               (let fun h(z, NOTHING) = z
                      | h(_, SOMEB z) = z
                      | h(_, SOMEU z) = z
                    val nt = LT.tcc_tup (ListPair.map h (ts, nts))
                 in SOMEB(tcBox nt)
                end)
             else NOTHING
         end
     | (TC_ARROW (tc1,tc2)) => 
         let val ntc1 = 
               (case tc_out tc1
                 of TC_TUPLE (ts as [_, _]) =>
                      let val nts = map tcWrap ts
                       in if (opList nts) then 
                            (let fun h(z, NONE) = z
                                   | h(_, SOME z) = z
                                 val nt = LT.tcc_tup(ListPair.map h (ts, nts))
                              in SOMEU nt
                             end)
                          else NOTHING
                      end
                  | (TC_VAR _ | TC_PROJ _ | TC_APP _) => SOMEB tc1
                  | _ => (case (tcWrap tc1) 
                           of SOME x => SOMEU x
                            | _ => NOTHING))

             val ntc2 = tcWrap tc2
          in (case (ntc1, ntc2)
               of (NOTHING, NONE) => NOTHING
                | (SOMEU z1, NONE) => SOMEU (LT.tcc_arw(z1, tc2))
                | (SOMEB z1, NONE) => SOMEB (tcBox(LT.tcc_arw(z1, tc2)))
                | (NOTHING, SOME z2) => SOMEU (LT.tcc_arw(tc1, z2))
                | (SOMEU z1, SOME z2) => SOMEU (LT.tcc_arw(z1, z2))
                | (SOMEB z1, SOME z2) => SOMEB (tcBox(LT.tcc_arw(z1, z2))))
         end
     | (TC_APP(tc, ts)) => 
         (case tcUncover tc of NOTHING => NOTHING
                             | _ => SOMEU x)
     | _ => NOTHING)

(** based on the given tyc, return its appropriate Update operator *)
fun tcUpd (tc) =  (* tc is in normal form *)
  let fun h(TC_PRIM pt) = 
            if PT.ubxupd pt then PO.UNBOXEDUPDATE
            else if PT.bxupd pt then PO.BOXEDUPDATE 
                 else PO.UPDATE
        | h(TC_TUPLE _ | TC_ARROW _) = PO.BOXEDUPDATE
        | h(TC_FIX (tc, 0)) = 
            (case (tc_out tc)
              of TC_FN([k],b) => h (tc_out b)
               | _ => PO.UPDATE)
        | h(TC_SUM tcs) = 
            let fun g (a::r) = if tc_eq(a, LT.tcc_null) then g r else false
                  | g [] = true
             in if (g tcs) then PO.UNBOXEDUPDATE else PO.UPDATE
            end
        | h _ = PO.UPDATE
   in h(tc_out tc)
  end

(** val tkLty : tkind -> lty *)
fun tkLty tk = 
  (case tk_out tk
    of TK_TYC => LT.ltc_int
     | TK_TBX => LT.ltc_int
     | TK_SEQ ks => LT.ltc_tup (map tkLty ks)
     | TK_FUN (k1, k2) => LT.ltc_arw(tkLty k1, tkLty k2))

fun tcNarrow t = 
  (case (tc_out t)
    of TC_PRIM pt => 
         if PT.isvoid pt then LT.tcc_void else t
     | TC_TUPLE tcs => LT.tcc_tup (map tcNarrow tcs)
     | TC_ARROW (tc1, tc2) => LT.tcc_arw(tcNarrow tc1, tcNarrow tc2)
     | _ => LT.tcc_void)

fun ltNarrow t = 
  (case lt_out t
    of LT_TYC tc => LT.ltc_tyc (tcNarrow tc)
     | LT_STR ts => LT.ltc_str (map ltNarrow ts)
     | LT_PST its => LT.ltc_pst (map (fn (i, t) => (i, ltNarrow t)) its)
     | LT_FCT (t1, t2) => LT.ltc_fct(ltNarrow t1, ltNarrow t2)
     | LT_POLY (ks, t) => LT.ltc_fct(LT.ltc_str (map tkLty ks), ltNarrow t)
     | LT_CNT _ => bug "unexpected CNTs in ltNarrow"
     | LT_IND _ => bug "unexpected INDs in ltNarrow"
     | LT_ENV _ => bug "unexpected ENVs in ltNarrow")

fun tcNarrowSt t = 
  let val nt = tcWhNorm t
   in (case tc_out nt
        of TC_PRIM pt => 
             if PT.isvoid pt then LT.tcc_void else nt
         | TC_TUPLE tcs => LT.tcc_tup (map tcNarrowSt tcs)
         | TC_ARROW (tc1, tc2) => LT.tcc_arw(tcNarrowSt tc1, tcNarrowSt tc2)
         | _ => LT.tcc_void)
  end

fun ltNarrowSt t = 
  (case lt_out (ltWhNorm t)
    of LT_TYC tc => LT.ltc_tyc (tcNarrowSt tc)
     | LT_STR ts => LT.ltc_str (map ltNarrowSt ts)
     | LT_PST its => LT.ltc_pst (map (fn (i, t) => (i, ltNarrowSt t)) its)
     | LT_FCT (t1, t2) => LT.ltc_fct(ltNarrowSt t1, ltNarrowSt t2)
     | LT_POLY (ks, t) => LT.ltc_fct(LT.ltc_str (map tkLty ks), ltNarrowSt t)
     | LT_CNT _ => bug "unexpected CNTs in ltNarrowSt"
     | LT_IND _ => bug "unexpected INDs in ltNarrowSt"
     | LT_ENV _ => bug "unexpected ENVs in ltNarrowSt")

(*
val tcNarrow =
  Stats.doPhase (Stats.makePhase "Compiler 053 1-tcNarw") tcNarrow

val ltNarrow =
  Stats.doPhase (Stats.makePhase "Compiler 053 2-ltNarw") ltNarrow
*)

end (* toplevel local *)
end (* structure LtyEnv *)

(*
 * $Log: ltyenv.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.7  1998/01/07 15:10:14  dbm
 *   Fixing bug 1322. The tcWrap function in ltyenv.sml was written in a
 *   way to avoid unnecessary wrappers; unfortunately, by doing this,
 *   we forgot to put in the wrapper in the case of TC_FN.
 *
 * Revision 1.6  1997/10/03 15:08:27  dbm
 *   Fix for bug 1285.
 *
 * Revision 1.5  1997/07/15  15:48:24  dbm
 *   Fix for representation bug (#1209).
 *
 * Revision 1.4  1997/05/05  19:59:40  george
 *   Change the term language into the quasi-A-normal form. Added a new round
 *   of lambda contraction before and after type specialization and
 *   representation analysis. Type specialization including minimum type
 *   derivation is now turned on all the time. Real array is now implemented
 *   as realArray. A more sophisticated partial boxing scheme is added and
 *   used as the default.
 *
 * Revision 1.3  1997/04/18  15:48:16  george
 *   Cosmetic changes on some constructor names. Changed the shape for
 *   FIX type to potentially support shared dtsig. -- zsh
 *
 * Revision 1.2  1997/02/26  21:46:56  george
 *   Added ltWrap (from zsh)
 *
 *)

(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* ltyextern.sml *)

structure LtyExtern : LTYEXTERN = 
struct

local structure PT = PrimTyc
      structure DI = DebIndex
      structure LK = LtyKernel
      structure PO = PrimOp     (* really should not refer to this *)
      structure FL = FLINT

      fun bug msg = ErrorMsg.impossible("LtyExtern: "^msg)
      val say = Control.Print.say

      (** common utility functions *)
      val tk_inj = LK.tk_inj
      val tk_out = LK.tk_out

      val tc_inj = LK.tc_inj
      val tc_out = LK.tc_out 
 
      val lt_inj = LK.lt_inj
      val lt_out = LK.lt_out 

      val tcc_env = LK.tcc_env
      val ltc_env = LK.ltc_env
      val tc_whnm = LK.tc_whnm
      val lt_whnm = LK.lt_whnm
      val tc_norm = LK.tc_norm
      val lt_norm = LK.lt_norm

in

open LtyBasic

(** instantiating a polymorphic type or an higher-order constructor *)
fun lt_inst (lt : lty, ts : tyc list) = 
  let val nt = lt_whnm lt
   in (case ((* lt_outX *) lt_out nt, ts)
        of (LK.LT_POLY(ks, b), ts) => 
             let val nenv = LK.tcInsert(LK.initTycEnv, (SOME ts, 0))
              in map (fn x => ltc_env(x, 1, 0, nenv)) b
             end
         | (_, []) => [nt]   (* this requires further clarifications !!! *)
         | _ => bug "incorrect lty instantiation in lt_inst")
  end 

fun lt_pinst (lt : lty, ts : tyc list) = 
  (case lt_inst (lt, ts) of [y] => y | _ => bug "unexpected lt_pinst")

(********************************************************************
 *                      KIND-CHECKING ROUTINES                      *
 ********************************************************************)
exception TkTycChk
exception LtyAppChk

(* tkSubkind returns true if k1 is a subkind of k2, or if they are 
 * equivalent kinds.  it is NOT commutative.  tksSubkind is the same
 * thing, component-wise on lists of kinds.
 *)
fun tksSubkind (ks1, ks2) =
    ListPair.all tkSubkind (ks1, ks2)   (* component-wise *)
and tkSubkind (k1, k2) = 
    tk_eqv (k1, k2) orelse              (* reflexive *)
    case (tk_out k1, tk_out k2) of
        (LK.TK_BOX, LK.TK_MONO) => true (* ground kinds (base case) *)
      (* this next case is WRONG, but necessary until the
       * infrastructure is there to give proper boxed kinds to
       * certain tycons (e.g., ref : Omega -> Omega_b)
       *)
      | (LK.TK_MONO, LK.TK_BOX) => true
      | (LK.TK_SEQ ks1, LK.TK_SEQ ks2) =>     
          tksSubkind (ks1, ks2)
      | (LK.TK_FUN (ks1, k1'), LK.TK_FUN (ks2, k2')) => 
          tksSubkind (ks1, ks2) andalso (* contravariant *)
          tkSubkind (k1', k2')
      | _ => false

(* is a kind monomorphic? *)
fun tkIsMono k = tkSubkind (k, tkc_mono)

(* assert that k1 is a subkind of k2 *)
fun tkAssertSubkind (k1, k2) =
    if tkSubkind (k1, k2) then ()
    else raise TkTycChk

(* assert that a kind is monomorphic *)
fun tkAssertIsMono k =
    if tkIsMono k then ()
    else raise TkTycChk

(* select the ith element from a kind sequence *)
fun tkSel (tk, i) = 
  (case (tk_out tk)
    of (LK.TK_SEQ ks) => (List.nth(ks, i) handle _ => raise TkTycChk)
     | _ => raise TkTycChk)

fun tks_eqv (ks1, ks2) = tk_eqv(tkc_seq ks1, tkc_seq ks2)

fun tkApp (tk, tks) = 
  (case (tk_out tk)
    of LK.TK_FUN(a, b) => if tks_eqv(a, tks) then b else raise TkTycChk
     | _ => raise TkTycChk)

(* check the application of tycs of kinds `tks' to a type function of
 * kind `tk'.
 *)
fun tkApp (tk, tks) = 
  (case (tk_out tk)
    of LK.TK_FUN(a, b) =>
       if tksSubkind(tks, a) then b else raise TkTycChk
     | _ => raise TkTycChk)

(* Kind-checking naturally requires traversing type graphs.  to avoid
 * re-traversing bits of the dag, we use a dictionary to memoize the
 * kind of each tyc we process.
 *
 * The problem is that a tyc can have different kinds, depending on
 * the valuations of its free variables.  So this dictionary maps a
 * tyc to an association list that maps the kinds of the free
 * variables in the tyc (represented as a TK_SEQ) to the tyc's kind.
 *)
structure Memo :> sig
    type dict 
    val newDict         : unit -> dict
    val recallOrCompute : dict * tkindEnv * tyc * (unit -> tkind) -> tkind
end =
struct
    structure TcDict = BinaryMapFn
                           (struct
                               type ord_key = tyc
                               val compare = LK.tc_cmp
                           end)

    type dict = (tkind * tkind) list TcDict.map ref
    val newDict : unit -> dict = ref o (fn () => TcDict.empty)

    fun recallOrCompute (dict, kenv, tyc, doit) =
        (* what are the valuations of tyc's free variables
         * in kenv? *)
        (* (might not be available for some tycs) *)
        case LK.tkLookupFreeVars (kenv, tyc) of
            SOME ks_fvs => let
                (* encode those as a kind sequence *)
                val k_fvs = tkc_seq ks_fvs
                (* query the dictionary *)
                val kci = case TcDict.find(!dict, tyc) of
                    SOME kci => kci
                  | NONE => []
                (* look for an equivalent environment *)
                fun sameEnv (k_fvs',_) = tk_eqv(k_fvs, k_fvs')
            in
                case List.find sameEnv kci of
                    SOME (_,k) => k     (* HIT! *)
                  | NONE => let
                        (* not in the list.  we will compute
                         * the answer and cache it
                         *)
                        val k = doit()
                        val kci' = (k_fvs, k) :: kci
                    in
                        dict := TcDict.insert(!dict, tyc, kci');
                        k
                    end
            end
          | NONE =>
            (* freevars were not available.  we'll have to
             * recompute and cannot cache the result.
             *)
            doit()

end (* Memo *)

(* return the kind of a given tyc in the given kind environment *)
fun tkTycGen() = let
    val dict = Memo.newDict()

    fun tkTyc kenv t = let
        (* default recursive invocation *)    
        val g = tkTyc kenv
        (* how to compute the kind of a tyc *)
        fun mk() =
            case tc_out t of
                LK.TC_VAR (i, j) =>
                tkLookup (kenv, i, j)
              | LK.TC_NVAR _ => 
                bug "TC_NVAR not supported yet in tkTyc"
              | LK.TC_PRIM pt =>
                tkc_int (PrimTyc.pt_arity pt)
              | LK.TC_FN(ks, tc) =>
                tkc_fun(ks, tkTyc (tkInsert (kenv,ks)) tc)
              | LK.TC_APP (tc, tcs) =>
                tkApp (g tc, map g tcs)
              | LK.TC_SEQ tcs =>
                tkc_seq (map g tcs)
              | LK.TC_PROJ(tc, i) =>
                tkSel(g tc, i)
              | LK.TC_SUM tcs =>
                (List.app (tkAssertIsMono o g) tcs;
                 tkc_mono)
              | LK.TC_FIX ((n, tc, ts), i) =>
                let val k = g tc
                    val nk =
                        case ts of
                            [] => k 
                          | _ => tkApp(k, map g ts)
                in
                    case (tk_out nk) of
                        LK.TK_FUN(a, b) => 
                        let val arg =
                                case a of
                                    [x] => x
                                  | _ => tkc_seq a
                        in
                            if tkSubkind(arg, b) then (* order? *)
                                (if n = 1 then b else tkSel(arg, i))
                            else raise TkTycChk
                        end
                      | _ => raise TkTycChk
                end
              | LK.TC_ABS tc =>
                (tkAssertIsMono (g tc);
                 tkc_mono)
              | LK.TC_BOX tc =>
                (tkAssertIsMono (g tc);
                 tkc_mono)
              | LK.TC_TUPLE (_,tcs) =>
                (List.app (tkAssertIsMono o g) tcs;
                 tkc_mono)
              | LK.TC_ARROW (_, ts1, ts2) =>
                (List.app (tkAssertIsMono o g) ts1;
                 List.app (tkAssertIsMono o g) ts2;
                 tkc_mono)
              | LK.TC_TOKEN(_, tc) =>
                (tkAssertIsMono (g tc);
                 tkc_mono)
              | LK.TC_PARROW _ => bug "unexpected TC_PARROW in tkTyc"
              | LK.TC_ENV _ => bug "unexpected TC_ENV in tkTyc"
              | LK.TC_IND _ => bug "unexpected TC_IND in tkTyc"
              | LK.TC_CONT _ => bug "unexpected TC_CONT in tkTyc"
    in
        Memo.recallOrCompute (dict, kenv, t, mk)
    end
in
    tkTyc
end

(* assert that the kind of `tc' is a subkind of `k' in `kenv' *)
fun tkChkGen() = let
    val tkTyc = tkTycGen()
    fun tkChk kenv (k, tc) =
        tkAssertSubkind (tkTyc kenv tc, k)
in
    tkChk
end
    
(* lty application with kind-checking (exported) *)
fun lt_inst_chk_gen() = let
    val tkChk = tkChkGen()
    fun lt_inst_chk (lt : lty, ts : tyc list, kenv : tkindEnv) = 
        let val nt = lt_whnm lt
        in (case ((* lt_outX *) lt_out nt, ts)
              of (LK.LT_POLY(ks, b), ts) => 
                 let val _ = ListPair.app (tkChk kenv) (ks, ts)
                     fun h x = ltc_env(x, 1, 0,
                                       tcInsert(initTycEnv, (SOME ts, 0)))
                 in map h b
                 end
               | (_, []) => [nt]    (* ? problematic *)
               | _ => raise LtyAppChk)
        end
in
    lt_inst_chk
end

(** a special lty application --- used inside the translate/specialize.sml *)
fun lt_sp_adj(ks, lt, ts, dist, bnl) = 
  let fun h(abslevel, ol, nl, tenv) =
        if abslevel = 0 then ltc_env(lt, ol, nl, tenv)
        else if abslevel > 0 then 
               h(abslevel-1, ol+1, nl+1, tcInsert(tenv, (NONE, nl)))
             else bug "unexpected cases in ltAdjSt"

      val btenv = tcInsert(initTycEnv, (SOME ts, 0))
      val nt = h(dist, 1, bnl, btenv)
   in nt (* was lt_norm nt *)
  end

(** a special tyc application --- used inside the translate/specialize.sml *)
fun tc_sp_adj(ks, tc, ts, dist, bnl) =
  let fun h(abslevel, ol, nl, tenv) =
        if abslevel = 0 then tcc_env(tc, ol, nl, tenv)
        else if abslevel > 0 then 
               h(abslevel-1, ol+1, nl+1, tcInsert(tenv, (NONE, nl)))
             else bug "unexpected cases in tcAdjSt"

      val btenv = tcInsert(initTycEnv, (SOME ts, 0))
      val nt = h(dist, 1, bnl, btenv)
   in nt (* was tc_norm nt *)
  end

(** sinking the lty one-level down --- used inside the specialize.sml *)
fun lt_sp_sink (ks, lt, d, nd) = 
  let fun h(abslevel, ol, nl, tenv) =
        if abslevel = 0 then ltc_env(lt, ol, nl, tenv)
        else if abslevel > 0 then
               h(abslevel-1, ol+1, nl+1, tcInsert(tenv, (NONE, nl)))
             else bug "unexpected cases in ltSinkSt"
      val nt = h(nd-d, 0, 1, initTycEnv)
   in nt (* was lt_norm nt *)
  end

(** sinking the tyc one-level down --- used inside the specialize.sml *)
fun tc_sp_sink (ks, tc, d, nd) = 
  let fun h(abslevel, ol, nl, tenv) =
        if abslevel = 0 then tcc_env(tc, ol, nl, tenv)
        else if abslevel > 0 then
               h(abslevel-1, ol+1, nl+1, tcInsert(tenv, (NONE, nl)))
             else bug "unexpected cases in ltSinkSt"
      val nt = h(nd-d, 0, 1, initTycEnv)
   in nt (* was tc_norm nt *)
  end

(** utility functions used in CPS *)
fun lt_iscont lt = 
      (case lt_out lt
        of LK.LT_CONT _ => true
         | LK.LT_TYC tc => 
             (case tc_out tc of LK.TC_CONT _ => true | _ => false)
         | _ => false)

fun ltw_iscont (lt, f, g, h) = 
      (case lt_out lt
        of LK.LT_CONT t => f t
         | LK.LT_TYC tc => 
             (case tc_out tc of LK.TC_CONT x => g x | _ => h lt)
         | _ => h lt)


fun tc_bug tc s = bug (s ^ "\n\n" ^ (tc_print tc) ^ "\n\n")
fun lt_bug lt s = bug (s ^ "\n\n" ^ (lt_print lt) ^ "\n\n")

(** other misc utility functions *)
fun tc_select(tc, i) = 
  (case tc_out tc
    of LK.TC_TUPLE (_,zs) =>
         ((List.nth(zs, i)) handle _ => bug "wrong TC_TUPLE in tc_select")
     | _ => tc_bug tc "wrong TCs in tc_select")

fun lt_select(t, i) = 
  (case lt_out t
    of LK.LT_STR ts => 
         ((List.nth(ts, i)) handle _ => bug "incorrect LT_STR in lt_select")
     | LK.LT_TYC tc => ltc_tyc(tc_select(tc, i))
     | _ => bug "incorrect lambda types in lt_select")

fun tc_swap t = 
  (case (tc_out t)
    of LK.TC_ARROW (LK.FF_VAR (r1,r2), [s1], [s2]) => 
         tcc_arrow(LK.FF_VAR (r2,r1), [s2], [s1])
     | LK.TC_ARROW (LK.FF_FIXED, [s1], [s2]) =>
         tcc_arrow(LK.FF_FIXED, [s2], [s1])
     | _ => bug "unexpected tycs in tc_swap")

fun lt_swap t = 
  (case (lt_out t)
    of (LK.LT_POLY (ks, [x])) => ltc_poly(ks, [lt_swap x])
     | (LK.LT_TYC x) => ltc_tyc(tc_swap x)
     | _ => bug "unexpected type in lt_swap")

(** functions that manipulate the FLINT function and record types *)
fun ltc_fkfun (FL.FK_FCT, atys, rtys) = 
      ltc_fct (atys, rtys)
  | ltc_fkfun (FL.FK_FUN {fixed, ...}, atys, rtys) = 
      ltc_arrow(fixed, atys, rtys)

fun ltd_fkfun lty = 
  if ltp_fct lty then ltd_fct lty
  else let val (_, atys, rtys) = ltd_arrow lty
        in (atys, rtys)
       end

fun ltc_rkind (FL.RK_TUPLE _, lts) = ltc_tuple lts
  | ltc_rkind (FL.RK_STRUCT, lts) = ltc_str lts
  | ltc_rkind (FL.RK_VECTOR t, _) = ltc_vector (ltc_tyc t)

fun ltd_rkind (lt, i) = lt_select (lt, i)

(****************************************************************************
 *             UTILITY FUNCTIONS USED BY POST-REPRESENTATION ANALYSIS       *
 ****************************************************************************)
(** find out what is the appropriate primop given a tyc *)
fun tc_upd_prim tc = 
  let fun h(LK.TC_PRIM pt) = 
            if PT.ubxupd pt then PO.UNBOXEDUPDATE
            else if PT.bxupd pt then PO.BOXEDUPDATE 
                 else PO.UPDATE
        | h(LK.TC_TUPLE _ | LK.TC_ARROW _) = PO.BOXEDUPDATE
        | h(LK.TC_FIX ((1,tc,ts), 0)) = 
            let val ntc = case ts of [] => tc
                                   | _ => tcc_app(tc, ts)
             in (case (tc_out ntc)
                  of LK.TC_FN([k],b) => h (tc_out b)
                   | _ => PO.UPDATE)
            end
        | h(LK.TC_SUM tcs) = 
            let fun g (a::r) = if tc_eqv(a, tcc_unit) then g r else false
                  | g [] = true
             in if (g tcs) then PO.UNBOXEDUPDATE else PO.UPDATE
            end
        | h _ = PO.UPDATE
   in h(tc_out tc)
  end

(** tk_lty : tkind -> lty --- finds out the corresponding type for a tkind *)
fun tk_lty tk = 
  (case tk_out tk
    of LK.TK_MONO => ltc_int
     | LK.TK_BOX => ltc_int
     | LK.TK_SEQ ks => ltc_tuple (map tk_lty ks)
     | LK.TK_FUN (ks, k) => 
         ltc_arrow(ffc_fixed, [ltc_tuple(map tk_lty ks)], [tk_lty k]))


(* tnarrow_gen : unit -> ((tyc -> tyc) * (lty -> lty) * (unit->unit)) *)
fun tnarrow_gen () = 
  let fun tcNarrow tcf t = 
        (case (tc_out t)
          of LK.TC_PRIM pt => 
               if PT.isvoid pt then tcc_void else t
           | LK.TC_TUPLE (_, tcs) => tcc_tuple (map tcf tcs)
           | LK.TC_ARROW (r, ts1, ts2) => 
               tcc_arrow(ffc_fixed, map tcf ts1, map tcf ts2)
           | _ => tcc_void)

      fun ltNarrow (tcf, ltf) t = 
        (case lt_out t
          of LK.LT_TYC tc => ltc_tyc (tcf tc)
           | LK.LT_STR ts => ltc_str (map ltf ts)
           | LK.LT_FCT (ts1, ts2) => ltc_fct(map ltf ts1, map ltf ts2)
           | LK.LT_POLY (ks, xs) => 
               ltc_fct([ltc_str (map tk_lty ks)], map ltf xs)
           | LK.LT_CONT _ => bug "unexpected CNTs in ltNarrow"
           | LK.LT_IND _ => bug "unexpected INDs in ltNarrow"
           | LK.LT_ENV _ => bug "unexpected ENVs in ltNarrow")

      val {tc_map, lt_map} = LtyDict.tmemo_gen {tcf=tcNarrow, ltf=ltNarrow}
   in (tc_map o tc_norm, lt_map o lt_norm, fn ()=>())
  end (* function tnarrow_gen *)

(* twrap_gen   : bool -> ((tyc -> tyc) * (lty -> lty) *
 *                        (tyc -> tyc) * (lty -> lty) * (unit -> unit)) 
 *)
fun twrap_gen bbb = 
  let fun tc_wmap (w, u) t =
        (case (tc_out t)
          of (LK.TC_VAR _ | LK.TC_NVAR _) => t
           | LK.TC_PRIM pt => if PT.unboxed pt then tcc_wrap t else t
           | LK.TC_FN (ks, tc) => tcc_fn(ks, w tc) (* impossible case *)
           | LK.TC_APP (tc, tcs) => tcc_app(w tc, map w tcs)
           | LK.TC_SEQ tcs => tcc_seq(map w tcs)
           | LK.TC_PROJ (tc, i) => tcc_proj(w tc, i)
           | LK.TC_SUM tcs => tcc_sum (map w tcs)
           | LK.TC_FIX ((n,tc,ts), i) => 
               tcc_fix((n, tc_norm (u tc), map w ts), i) 

           | LK.TC_TUPLE (_, ts) => tcc_wrap(tcc_tuple (map w ts)) (* ? *)
           | LK.TC_ARROW (LK.FF_VAR(b1,b2), ts1, ts2) =>  
               let val nts1 =    (* too specific ! *)                       
                     (case ts1 of [t11,t12] => [w t11, w t12] 
                                | _ => [w (LK.tc_autotuple ts1)])
                   val nts2 = [w (LK.tc_autotuple ts2)]
                   val nt = tcc_arrow(ffc_fixed, nts1, nts2)
                in if b1 then nt else tcc_wrap nt
               end
           | LK.TC_ARROW (LK.FF_FIXED, _, _) =>  
                bug "unexpected TC_FIXED_ARROW in tc_umap"
           | LK.TC_TOKEN (k, t) => bug "unexpected token tyc in tc_wmap"
           | LK.TC_BOX _ => bug "unexpected TC_BOX in tc_wmap"
           | LK.TC_ABS _ => bug "unexpected TC_ABS in tc_wmap"
           | _ => bug "unexpected other tycs in tc_wmap")

      fun tc_umap (u, w) t =
        (case (tc_out t)
          of (LK.TC_VAR _ | LK.TC_NVAR _ | LK.TC_PRIM _) => t
           | LK.TC_FN (ks, tc) => tcc_fn(ks, u tc) (* impossible case *) 
           | LK.TC_APP (tc, tcs) => tcc_app(u tc, map w tcs)
           | LK.TC_SEQ tcs => tcc_seq(map u tcs)
           | LK.TC_PROJ (tc, i) => tcc_proj(u tc, i)
           | LK.TC_SUM tcs => tcc_sum (map u tcs)
           | LK.TC_FIX ((n,tc,ts), i) => 
               tcc_fix((n, tc_norm (u tc), map w ts), i) 

           | LK.TC_TUPLE (rk, tcs) => tcc_tuple(map u tcs)
           | LK.TC_ARROW (LK.FF_VAR(b1,b2), ts1, ts2) =>  
               tcc_arrow(ffc_fixed, map u ts1, map u ts2)
           | LK.TC_ARROW (LK.FF_FIXED, _, _) =>  
               bug "unexpected TC_FIXED_ARROW in tc_umap"
           | LK.TC_PARROW _ => bug "unexpected TC_PARROW in tc_umap"

           | LK.TC_BOX _ => bug "unexpected TC_BOX in tc_umap"
           | LK.TC_ABS _ => bug "unexpected TC_ABS in tc_umap"
           | LK.TC_TOKEN (k, t) => 
               if LK.token_eq(k, LK.wrap_token) then 
                 bug "unexpected TC_WRAP in tc_umap"
               else tc_inj (LK.TC_TOKEN (k, u t))

           | _ => bug "unexpected other tycs in tc_umap")

      fun lt_umap (tcf, ltf) t = 
        (case (lt_out t)
          of LK.LT_TYC tc => ltc_tyc (tcf tc)
           | LK.LT_STR ts => ltc_str (map ltf ts)
           | LK.LT_FCT (ts1, ts2) => ltc_fct(map ltf ts1, map ltf ts2)
           | LK.LT_POLY (ks, xs) => ltc_poly(ks, map ltf xs)
           | LK.LT_CONT _ => bug "unexpected CNTs in lt_umap"
           | LK.LT_IND _ => bug "unexpected INDs in lt_umap"
           | LK.LT_ENV _ => bug "unexpected ENVs in lt_umap")

      val {tc_wmap=tcWrap, tc_umap=tcMap, lt_umap=ltMap, cleanup} =
        LtyDict.wmemo_gen{tc_wmap=tc_wmap, tc_umap=tc_umap, lt_umap=lt_umap}

      fun ltWrap x = 
        ltw_tyc (x, (fn tc => ltc_tyc (tcWrap tc)),
                     fn _ => bug "unexpected case in ltWrap")

   in (tcWrap o tc_norm, ltWrap o lt_norm, 
       tcMap o tc_norm, ltMap o lt_norm, cleanup)
  end


end (* top-level local *)
end (* structure LtyExtern *)

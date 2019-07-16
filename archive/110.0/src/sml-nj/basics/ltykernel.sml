(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* ltykernel.sml *)

structure LtyKernel :> LTYKERNEL = 
struct 

(***************************************************************************
 *                UTILITY FUNCTIONS FOR HASHCONSING BASICS                 *
 ***************************************************************************)

(** hashconsing implementation basics *)
local open SortedList
      val MVAL = 10000
      val BVAL = MVAL * 2 (* all index i start from 0 *)
in 

type enc_tvar = int
fun tvToInt (d, i) = d * MVAL + i
fun tvFromInt x = ((x div MVAL), (x mod MVAL))

fun exitLevel xs = 
  let fun h ([], x) = rev x
        | h (a::r, x) = if a < BVAL then h(r, x) else h(r, (a-MVAL)::x)
   in h(xs, [])
  end
  
datatype aux_info = AX_REG of bool * enc_tvar list
                  | AX_NO

val mergeTvs = merge
val fmergeTvs = foldmerge

type 'a hash_cell = (int * 'a * aux_info) ref

end (* local of hashconsing implementation basics *)


(***************************************************************************
 *                 DATATYPE DEFINITIONS                                    *
 ***************************************************************************)

(** definition of kinds for all the lambda tycs *)
datatype tkindI
  = TK_TYC                                     (* ground typ-constructors *)
  | TK_TBX                                     (* ground parametric types *)
  | TK_SEQ of tkind list                       (* kind for a seq. of tycs *)
  | TK_FUN of tkind * tkind                    (* high-order tyc functors *)

withtype tkind = tkindI hash_cell              (* hash-consing-impl of tkind *)

(** tkind environment is implementated as list *)
type tkindEnv = tkind list list

(** definitions of lambda tycons and lambda types *)
datatype tycI
  = TC_VAR of DebIndex.index * int             (* tyc variables *)
  | TC_PRIM of PrimTyc.primtyc                 (* primitive tycons *)

  | TC_FN of tkind list * tyc                  (* constructor abstraction *)
  | TC_APP of tyc * tyc list                   (* constructor application *)

  | TC_SEQ of tyc list                         (* a sequence of tycons *)
  | TC_PROJ of tyc * int                       (* projection on sequence *)

  | TC_SUM of tyc list                         (* sum of monotypes *)
  | TC_FIX of tyc * int                        (* recursive types *)
  | TC_ABS of tyc                              (* abs tyc, not stamped *)

  | TC_TUPLE of tyc list                       (* record tycon *)
  | TC_ARROW of tyc * tyc                      (* default arrow tycon *)
  | TC_BOX of tyc                              (* wrapped tyc *)
  | TC_CONT of tyc                             (* intern continuation tycon *)

  | TC_IND of tyc * tycI                       (* a tyc thunk and its sig *)
  | TC_ENV of tyc * int * int * tycEnv         (* tyc closure *)

and ltyI          
  = LT_TYC of tyc                              (* constructor type *)
  | LT_STR of lty list                         (* structure record type *)
  | LT_PST of (int * lty) list                 (* partial-structure type *)
  | LT_FCT of lty * lty                        (* functor arrow type *)
  | LT_POLY of tkind list * lty                (* polymorphic type *)
  | LT_CNT of lty                              (* intern cont type *)

  | LT_IND of lty * ltyI                       (* a lty thunk and its sig *)
  | LT_ENV of lty * int * int * tycEnv         (* lty closure *)

withtype tyc = tycI hash_cell                  (* hash-consed tyc cell *)

     and lty = ltyI hash_cell                  (* hash-consed lty cell *)

     and tycEnv = tyc     (* 
                           * This really is (tyc list option * int) list,
                           * it is encoded using SEQ[(PROJ(SEQ tcs),i)]
                           * and SEQ[(PROJ(VOID, i))]. (ZHONG)
                           *)

(***************************************************************************
 *                   HASHCONSING IMPLEMENTATIONS                           *
 ***************************************************************************)

(** Hash-consing implementations of tyc, tkind, lty *)

local structure Weak = SMLofNJ.Weak
      structure PT = PrimTyc
      structure DI = DebIndex

      fun bug msg = ErrorMsg.impossible("LtyKernel: "^msg)
 
      val itow = Word.fromInt
      val wtoi = Word.toIntX
      val andb = Word.andb

      val N = 2048 (* 1024 *)
      val NN = itow (N*N)
      val P = 0w509 (* was 0w1019, a prime < 1024 so that N*N*P < maxint *)

      val tk_table : tkind Weak.weak list Array.array = Array.array(N,nil)
      val tc_table : tyc Weak.weak list Array.array = Array.array(N,nil)
      val lt_table : lty Weak.weak list Array.array = Array.array(N,nil)

      fun vector2list v = Vector.foldr (op ::) [] v

      fun revcat(a::rest,b) = revcat(rest,a::b)
        | revcat(nil,b) = b

      fun combine [x] = itow x
        | combine (a::rest) = 
            andb(itow a +(combine rest)*P, NN - 0w1)
        | combine _ = bug "unexpected case in combine"

      (* 
       * Because of the "cmp" function below, it's necessary to keep
       * each bucket-list in a consistent order, and not reverse
       * or move-to-front or whatever. 
       *)
      fun look(table, h, t, eq, mk) =
        let val i = wtoi(andb(itow h, itow(N-1)))

            fun g(l, z as (w::rest)) = 
                  (case Weak.strong w
                    of SOME (r as ref(h',t',_)) =>
                        if (h=h') andalso (eq {new=t, old=t'})
                        then (Array.update(table, i, revcat(l,z)); r)
                        else g(w::l, rest)
                     | NONE => g(l, rest))
              | g(l, []) = 
                  let val r = mk(h, t)
                   in Array.update(table, i, (Weak.weak r) :: rev l); r
                  end

         in g([], Array.sub(table, i))
        end

      fun cmp(table, a as ref(ai,_,_), b as ref (bi,_,_)) =
        if ai < bi then LESS 
        else if ai > bi then GREATER
           else if a = b then EQUAL
                else let val index = wtoi (andb(itow ai,itow(N-1)))
                         fun g [] = bug "unexpected case in cmp"
                           | g (w::rest) =
                                 (case Weak.strong w
                                   of SOME r => 
                                        if a=r then LESS 
                                        else if b=r then GREATER
                                                    else g rest
                                    | NONE => g rest)
                      in g(Array.sub(table,index))
                     end


      fun getnum (ref(i,_,_)) = i
      fun tagnums nil = nil
        | tagnums ((i,t)::rest) = i::getnum t::tagnums rest

      fun tk_hash tk =
        let fun g (TK_TYC) = 0w1
              | g (TK_TBX) = 0w2
              | g (TK_SEQ ks) = combine (3::map getnum ks)
              | g (TK_FUN(k1, k2)) = combine [4, getnum k1, getnum k2]
         in g tk
        end

      fun tc_hash tc = 
        let fun g (TC_VAR(d, i)) = combine [1, (DI.di_key d)*10, i]
              | g (TC_PRIM pt) = combine [2, PT.pt_toint pt]
              | g (TC_FN(ks, t)) = combine (3::(getnum t)::(map getnum ks))
              | g (TC_APP(t, ts)) = combine (4::(getnum t)::(map getnum ts))
              | g (TC_SEQ ts) = combine (5::(map getnum ts))
              | g (TC_PROJ(t, i)) = combine [6, (getnum t), i]
              | g (TC_SUM ts) = combine (7::(map getnum ts))
              | g (TC_FIX(t, i)) = 
                     combine [8, (getnum t), i]
              | g (TC_ABS t) = combine [9, getnum t]
              | g (TC_BOX t) = combine [10, getnum t]
              | g (TC_TUPLE ts) = combine (11::(map getnum ts))
              | g (TC_ARROW(t1, t2)) = combine [12, getnum t1, getnum t2]
              | g (TC_CONT t) = combine [13, getnum t]
              | g (TC_ENV(t,i,j,env)) = 
                     combine[14, getnum t, i, j, getnum env]
              | g (TC_IND _) = bug "unexpected TC_IND in tc_hash"

         in g tc
        end 

      fun lt_hash lt = 
        let fun g (LT_TYC t) = combine [1, getnum t]
              | g (LT_STR ts) = combine (2::(map getnum ts))
              | g (LT_PST ts) = combine (3::(tagnums ts))
              | g (LT_FCT(t1, t2)) = combine [4, getnum t1, getnum t2]
              | g (LT_POLY(ks, t)) = combine (5::(getnum t)::(map getnum ks))
              | g (LT_CNT t) = combine [6, getnum t]
              | g (LT_ENV(t,i,j,env)) = 
                     combine [7, getnum t, i, j, getnum env]
              | g (LT_IND _) = bug "unexpected LT_IND in tc_hash"
         in g lt
        end

      fun tkI_eq {new: tkindI, old} = (new = old)
      
      (* the 1st is the one being mapped; the 2nd is the one in hash table *)
      fun tcI_eq {new : tycI, old=TC_IND(_,s)} = tcI_eq {new=new, old=s}
        | tcI_eq {new, old} = (new=old)

      fun ltI_eq {new : ltyI, old=LT_IND(_,s)} = ltI_eq {new=new, old=s}
        | ltI_eq {new, old} = (new=old)

      val baseAux = AX_REG (true, [])

      fun getAux (ref(i : int, _, x)) = x

      fun mergeAux(AX_NO, _) = AX_NO
        | mergeAux(_, AX_NO) = AX_NO
        | mergeAux(AX_REG(b1,vs1), AX_REG(b2,vs2)) =
            AX_REG(b2 andalso b1, mergeTvs(vs1, vs2))

      fun fsmerge [] = baseAux
        | fsmerge [x] = getAux x
        | fsmerge xs = 
            let fun loop([], z) = z
                  | loop(_, AX_NO) = AX_NO
                  | loop(a::r, z) = loop(r, mergeAux(getAux a, z))
             in loop(xs, baseAux)
            end

      fun exitAux(AX_REG(b, vs)) = AX_REG(b, exitLevel vs)
        | exitAux x = x

      fun tc_aux tc = 
        let fun g (TC_VAR(d, i)) = AX_REG(true, [tvToInt(d, i)])
              | g (TC_PRIM pt) = baseAux
              | g (TC_APP(ref(_, TC_FN _, AX_NO), _)) = AX_NO
              | g (TC_PROJ(ref(_, TC_SEQ _, AX_NO), _)) = AX_NO
              | g (TC_APP(ref(_, TC_FN _, AX_REG(_,vs)), ts)) = 
                     mergeAux(AX_REG(false, vs), fsmerge ts)       (* ? *)
              | g (TC_PROJ(ref(_, TC_SEQ _, AX_REG(_,vs)), _)) = 
                     AX_REG(false, vs)                             (* ? *)
              | g (TC_FN(ks, t)) = exitAux(getAux t)
              | g (TC_APP(t, ts)) = fsmerge (t::ts)
              | g (TC_SEQ ts) = fsmerge ts
              | g (TC_PROJ(t, _)) = getAux t
              | g (TC_SUM ts) = fsmerge ts
              | g (TC_FIX(t, _)) = getAux t
              | g (TC_ABS t) = getAux t
              | g (TC_BOX t) = getAux t
              | g (TC_TUPLE ts) = fsmerge ts
              | g (TC_ARROW(t1, t2)) = fsmerge[t1, t2]
              | g (TC_CONT t) = getAux t
              | g (TC_IND _) = bug "unexpected TC_IND in tc_aux"
              | g (TC_ENV _) = AX_NO
         in g tc
        end 
        
      fun lt_aux lt = 
        let fun g (LT_TYC t) = getAux t
              | g (LT_STR ts) = fsmerge ts
              | g (LT_PST ts) = fsmerge (map #2 ts)
              | g (LT_FCT(t1, t2)) = fsmerge [t1, t2]
              | g (LT_POLY(ks, t)) = exitAux(getAux t)
              | g (LT_CNT t) = getAux t
              | g (LT_IND _) = bug "unexpected LT_IND in lt_aux"
              | g (LT_ENV _) = AX_NO
         in g lt
        end

      fun tk_mk (i : int, k: tkindI) = ref (i, k, AX_NO)
      fun tc_mk (i : int, tc : tycI) = ref (i, tc, tc_aux tc)
      fun lt_mk (i : int, lt : ltyI) = ref (i, lt, lt_aux lt)

in 

(** a temporary hack on getting the list of free tyvars *)
fun tc_vs (r as ref(_ : int, _ : tycI, AX_NO)) = NONE
  | tc_vs (r as ref(_ : int, _ : tycI, AX_REG (_,x))) = SOME x

fun lt_vs (r as ref(_ : int, _ : ltyI, AX_NO)) = NONE
  | lt_vs (r as ref(_ : int, _ : ltyI, AX_REG (_,x))) = SOME x


(** converting from the hash-consing reps to the standard reps *)
fun tk_out (r as ref(_ : int, t : tkindI, _ : aux_info)) = t
fun tc_out (r as ref(_ : int, t : tycI, _ : aux_info)) = t
fun lt_out (r as ref(_ : int, t : ltyI, _ : aux_info)) = t


(** converting from the standard reps to the hash-consing reps *)
fun tk_inj t = look(tk_table, wtoi(tk_hash t), t, tkI_eq, tk_mk)
fun tc_inj t = look(tc_table, wtoi(tc_hash t), t, tcI_eq, tc_mk)
fun lt_inj t = look(lt_table, wtoi(lt_hash t), t, ltI_eq, lt_mk)


(** key-comparison on tkind, tyc, lty *)
fun tk_cmp (k1, k2) = cmp(tk_table, k1, k2)
fun tc_cmp (t1, t2) = cmp(tc_table, t1, t2)
fun lt_cmp (t1, t2) = cmp(lt_table, t1, t2)


(** get the hash key of each lty, only used in translate/coerce.sml; a hack *)
fun lt_key (ref (h : int, _ : ltyI, _ : aux_info)) = h


(** testing the equality of values of tkind, tyc, lty *)
fun tk_eq (x: tkind, y) = (x = y)
fun tc_eq (x: tyc, y) = (x = y)
fun lt_eq (x: lty, y) = (x = y)

(** testing the equivalence for "normalized" tkind, tyc and lty *)
val tk_eqv = tk_eq     (* all tkinds are normalized *)

fun tc_eqv (x as ref (_ : int, _ : tycI, AX_REG(true,_)), 
            y as ref (_, _, AX_REG(true, _))) = (x = y)
  | tc_eqv _ = bug "unexpected arguments in tc_eqv"

fun eqlist (p, x::xs, y::ys) = (p(x,y)) andalso (eqlist(p, xs, ys))
  | eqlist (p, [], []) = true
  | eqlist _ = false

(* 
 * All the complexity of lt_eqv comes from the partial-structure (or
 * partial record) type (the LT_PST type). If we can remove LT_PST
 * type, then the following can be considerabily simplified. (ZHONG)
 *)
fun lt_eqv ((x as ref (i, ti, AX_REG(true,_))) : lty, 
             y as ref (j, tj, AX_REG(true,_))) = 
      let fun sp (r, []) = true
            | sp (r, (i,t)::s) = 
                 (if (lt_eqv(List.nth(r,i),t)) 
                  then sp(r,s) else false) handle _ => false

          fun pp ([], _) = true
            | pp (_, []) = true
            | pp (a as ((i,t)::l), b as ((j,s)::r)) = 
                if i > j then pp(a,r) 
                else if i < j then pp(l,b) 
                     else if lt_eqv(t,s) then pp(l,r) else false 

          fun g(LT_PST s1, LT_PST s2) = pp(s1, s2)
            | g(LT_PST s1, LT_STR s2) = sp(s2, s1)
            | g(LT_STR s1, LT_PST s2) = sp(s1, s2)
            | g(LT_STR s1, LT_STR s2) = eqlist(lt_eqv, s1, s2)
            | g(LT_FCT (s11,s12), LT_FCT(s21, s22)) = 
                 (lt_eqv(s11,s21)) andalso (lt_eqv(s12, s22))
            | g(LT_POLY (ks1, t1), LT_POLY(ks2, t2)) = 
                 (eqlist(tk_eq, ks1, ks2)) andalso (lt_eqv(t1, t2))
            | g(LT_TYC t1, LT_TYC t2) = tc_eqv(t1, t2)
            | g(LT_CNT t1, LT_CNT t2) = lt_eqv(t1, t2)
            | g _ = false

       in (lt_eq(x,y)) orelse (g(ti, tj))
      end
  | lt_eqv _ = bug "unexpected complicated lty in lt_eqv"


(***************************************************************************
 *                     OTHER UTILITY FUNCTIONS                             *
 ***************************************************************************)

(** utility functions for manipulating the tkindEnv *)
exception tkUnbound
val initTkEnv : tkindEnv = []

fun tkLookup (kenv, i, j) = 
  let val ks = List.nth(kenv, i-1) handle _ => raise tkUnbound
   in List.nth(ks, j) handle _ => raise tkUnbound
  end

fun tkInsert (kenv, ks) = ks::kenv


(** utility functions for manipulating the tycEnv *)
local val tc_void = tc_inj(TC_PRIM(PT.ptc_void))
      fun tc_cons (t, b) = tc_inj(TC_ARROW(t, b))
      fun tc_interp x = 
        (case tc_out x
          of TC_PROJ(y, i) =>
               (case tc_out y of TC_SEQ ts => (SOME ts, i)
                               | TC_PRIM _ => (NONE, i)
                               | _ => bug "unexpected tycEnv1 in tc_interp")
           | _ => bug "unexpected tycEnv2 in tc_interp")

      fun tc_encode(NONE, i) = tc_inj(TC_PROJ(tc_void,i))
        | tc_encode(SOME ts, i) = 
            tc_inj(TC_PROJ(tc_inj(TC_SEQ(ts)), i))

in

exception tcUnbound
val initTycEnv : tycEnv = tc_void

fun tcLookup(i, tenv : tycEnv) = 
      if i > 1 then
        (case tc_out tenv of TC_ARROW(_,x) => tcLookup(i-1, x)
                           | _ => bug "unexpected tycEnv in tcLookup")
      else if i = 1 then
             (case tc_out tenv of TC_ARROW(x,_) => tc_interp x 
                                | _ => raise tcUnbound)
           else bug "unexpected argument in tcLookup"

fun tcInsert(tenv : tycEnv, et) = tc_cons(tc_encode et, tenv)

fun tcSplit(tenv : tycEnv) =
  (case tc_out tenv of TC_ARROW(x,y) => SOME (tc_interp x, y)
                     | _ => NONE)
  

end (* utililty function for tycEnv *)


(** checking if a tyc or an lty is in the normal form *)
fun tcp_norm ((t as ref (i, _, AX_REG(b,_))) : tyc) =  b
  | tcp_norm _ = false

fun ltp_norm ((t as ref (i, _, AX_REG(b,_))) : lty) =  b
  | ltp_norm _ = false


(** finding out the innermost binding depth for a tyc's free variables *)
fun tc_depth (x, d) =
  let val tvs = tc_vs x
   in case tvs
       of NONE => bug "unexpected case in tc_depth"
        | SOME [] => DI.top
        | SOME (a::_) => d + 1 - (#1(tvFromInt a))
  end

fun tcs_depth ([], d) = DI.top
  | tcs_depth (x::r, d) = Int.max(tc_depth(x, d), tcs_depth(r, d))

(** utility functions for tc_env and lt_env *)
local fun tcc_env_int(x, 0, 0, te) = x
        | tcc_env_int(x, i, j, te) = tc_inj(TC_ENV(x, i, j, te))

      fun ltc_env_int(x, 0, 0, te) = x
        | ltc_env_int(x, i, j, te) = lt_inj(LT_ENV(x, i, j, te))
 
      fun withEff ([], ol, nl, tenv) = false
        | withEff (a::r, ol, nl, tenv) = 
            let val (i, j) = tvFromInt a
                val neweff = 
                  if i > ol then (ol <> nl)
                  else (* case tcLookup(i, tenv)
                           of (NONE, n) => (nl - n) <> i
                            | (SOME ts, n) =>
                                 (let val y = List.nth(ts, j)
                                   in (case tc_out y
                                        of TC_VAR(ni, nj) =>
                                            ((nj <> j) orelse ((ni+nl-n) <> i))
                                         | _ => true)
                                  end) *) true
             in neweff orelse (withEff(r, ol, nl, tenv))
            end

in 

fun tcc_env(x, ol, nl, tenv) =
  let val tvs = tc_vs x
   in case tvs
       of NONE => tcc_env_int(x, ol, nl, tenv)
        | SOME [] => x
        | SOME nvs => if withEff(nvs, ol, nl, tenv) 
                      then tcc_env_int(x, ol, nl, tenv)
                      else x 
  end

fun ltc_env(x, ol, nl, tenv) = 
  let val tvs = lt_vs x
   in case tvs
       of NONE => ltc_env_int(x, ol, nl, tenv)
        | SOME [] => x
        | SOME nvs => if withEff (nvs, ol, nl, tenv) 
                      then ltc_env_int(x, ol, nl, tenv)
                      else x 
  end

end (* utility functions for lt_env and tc_env *)


(** utility functions for updating tycs and ltys *)
fun tyc_upd (tgt as ref(i : int, old : tycI, AX_NO), nt) = 
      (tgt := (i, TC_IND (nt, old), AX_NO))
  | tyc_upd (tgt as ref(i : int, old : tycI, x as (AX_REG(false, _))), nt) = 
      (tgt := (i, TC_IND (nt, old), x))
  | tyc_upd _ = bug "unexpected tyc_upd on already normalized tyc"

fun lty_upd (tgt as ref(i : int, old : ltyI, AX_NO), nt) = 
      (tgt := (i, LT_IND (nt, old), AX_NO))
  | lty_upd (tgt as ref(i : int, old : ltyI, x as (AX_REG(false, _))), nt) = 
      (tgt := (i, LT_IND (nt, old), x))
  | lty_upd _ = bug "unexpected lty_upd on already normalized lty"


end (* toplevel local *)
end (* abstraction LtyKernel *)

(*
 * $Log: ltykernel.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.4  1997/04/18 15:42:28  george
 *   Type specialization is now added in (though it is still turned off).
 *   A pretty-fancy kind of minimum type derivation is also done. -- zsh
 *
 * Revision 1.3  1997/02/11  15:15:57  george
 * moved stuff from System to SMLofNJ
 *
 * Revision 1.2  1997/01/31  20:39:32  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:10  george
 *   Version 109.24
 *
 *)

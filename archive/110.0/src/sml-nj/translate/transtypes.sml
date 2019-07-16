(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* transtypes.sml *)

signature TRANSTYPES = 
sig
  val tpsKnd : Types.tycpath -> LambdaType.tkind
  val tpsTyc : DebIndex.depth -> Types.tycpath -> LambdaType.tyc

  val toTyc  : DebIndex.depth -> Types.ty -> LambdaType.tyc
  val toLty  : DebIndex.depth -> Types.ty -> LambdaType.lty
end (* signature TRANSTYPES *)

structure TransTypes : TRANSTYPES = 
struct

local structure BT = BasicTypes
      structure DI = DebIndex
      structure LE = LtyEnv
      structure LT = LambdaType
      structure TU = TypesUtil
      open Types

in

exception STRANGE

fun bug msg = ErrorMsg.impossible ("TransTypes: " ^ msg)
val say = Control.Print.say 

local
structure PP = PrettyPrint
structure EM = ErrorMsg
in
val env = StaticEnv.empty
fun ppType x = 
 ((PP.with_pp (EM.defaultConsumer())
           (fn ppstrm => (PP.add_string ppstrm "find: ";
                          PPType.resetPPType();
                          PPType.ppType env ppstrm x)))
  handle _ => say "fail to print anything")

fun ppTycon x = 
 ((PP.with_pp (EM.defaultConsumer())
           (fn ppstrm => (PP.add_string ppstrm "find: ";
                          PPType.resetPPType();
                          PPType.ppTycon env ppstrm x)))
  handle _ => say "fail to print anything")
end



local val recTyContext = ref [~1]
in 
fun enterRecTy (a) = (recTyContext := (a::(!recTyContext)))
fun exitRecTy () = (recTyContext := tl (!recTyContext))
fun recTyc (i) = 
      let val x = hd(!recTyContext)
       in if x = 0 then LT.tcc_var(DI.innermost, i)
          else if x > 0 then LT.tcc_var(DI.innersnd, i)
               else bug "unexpected RECtyc"
      end
end (* end of recTyc hack *)

fun tpsKnd (TP_VAR {kind, ...}) = kind
  | tpsKnd _ = bug "unexpected tycpath parameters in tpsKnd"

fun tpsTyc d tp = 
  let fun h (TP_VAR {depth, num, ...}, cur) = 
              LT.tcc_var(DI.calc(cur, depth), num)
        | h (TP_TYC tc, cur) = tycTyc(tc, cur)
        | h (TP_SEL (tp, i), cur) = LT.tcc_proj(h(tp, cur), i)
        | h (TP_APP (tp, ps), cur) = 
              LT.tcc_app(h(tp, cur), map (fn x => h(x, cur)) ps)
        | h (TP_FCT (ps, ts), cur) = 
              let val ks = map tpsKnd ps
                  val cur' = DI.next cur
                  val ts' = map (fn x => h(x, cur')) ts
               in LT.tcc_fn(ks, LT.tcc_seq ts')
              end

   in h(tp, d)
  end

(*
and tycTyc x = 
  Stats.doPhase(Stats.makePhase "Compiler 043 1-tycTyc") tycTyc0 x
*)

and tycTyc(tc, d) = 
  let fun dtsTyc nd ({arity, lambdatyc=ref (SOME(tc, od)), ...} : dtmember) = 
            (LT.tkc_funs arity, LE.adjTycSp(tc, od, nd))

        | dtsTyc nd ({dcons: dconDesc list, arity, lambdatyc=x as ref _, ...}) = 
            let (* open problem: how to translate RECtyc i ? *)
                val nd' = if arity=0 then nd else DI.next nd

                fun f ({domain=NONE, rep, name}, r) = (LT.tcc_null)::r
                  | f ({domain=SOME t, rep, name}, r) = (toTyc nd' t)::r

                val _ = enterRecTy(arity)
                val core = LT.tcc_sum(foldr f [] dcons)
                val _ = exitRecTy()

                val resTyc = if arity=0 then core
                             else (let val ks = LT.tkc_args arity
                                    in LT.tcc_fn(ks, core)
                                   end)
             in x := SOME (resTyc, nd); (LT.tkc_funs arity, resTyc)
            end
      

      fun h (PRIMITIVE pt, _) = LT.tcc_prim(pt)
        | h (DATATYPE {index, members, lambdatyc=ref (SOME (tc,od))}, _) = 
              LE.adjTyc(tc, od, d)
        | h (DATATYPE {index, members as #[{arity, dcons,...}], 
                       lambdatyc=x as ref _}, _) = 
              if (length dcons) < 10
              then (let val mtcs =
                         Vector.foldr  (* should be Vector.map *)
			 (fn (x,y) => (dtsTyc (DI.next d) x) :: y) nil members 
                        val ks = map #1 mtcs
                        val ts = map #2 mtcs
                        val tb = case ts of [x] => x
                                          | _ => LT.tcc_seq ts
                        val tsig = LT.tcc_fn(ks, tb)
                        val tc = LT.tcc_fix(tsig, index) 
                     in x := (SOME (tc, d)); tc
                    end)
              (***>> To avoid overly long compilation time, I turned
                     off the general type translations on datatypes <<***)
              else (let val ks = LT.tkc_args arity
                        val tb = LT.tcc_fn(ks, LT.tcc_void)
                        val tsig = LT.tcc_fn([LT.tkc_funs arity], tb)
                        val tc = LT.tcc_fix(tsig, 0)
                     in x := (SOME (tc, d)); tc
                    end)

        | h (DATATYPE {index, members, lambdatyc=x as ref _}, _) = 
              let (* val mtcs = map (dtsTyc (DI.next d)) members *)
                  (* val tc = LT.tcc_fix(mtcs, index) *)
                  (***>> To avoid overly long compilation time, I turned
                         off the general type translations on datatypes <<***)
                  val {arity, ...} = Vector.sub(members, index)
                  val ks = LT.tkc_args arity
                  val tb = LT.tcc_fn(ks, LT.tcc_void)
                  val tsig = LT.tcc_fn([LT.tkc_funs arity],tb)
                  val tc = LT.tcc_fix(tsig, 0)
               in x := (SOME (tc, d)); tc
              end
        | h (ABSTRACT tc, 0) = (* LT.tcc_abs *) (g tc)
        | h (ABSTRACT tc, n) = 
              let val ks = LT.tkc_args n
                  fun fromto(i,j) = if i < j then (i::fromto(i+1,j)) else []
                  val fs = fromto(0, n)
                  val ts = map (fn i => LT.tcc_var(DI.innermost, i)) fs
                  val b = LE.tcAppSt(tycTyc(tc, DI.next d), ts)
               in LT.tcc_fn(ks, (* LT.tcc_abs *) b)
              end
        | h (FLEXTYC tp, _) = tpsTyc d tp
        | h (FORMAL, _) = bug "unexpected FORMAL kind in tycTyc-h"
        | h (TEMP, _) = bug "unexpected TEMP kind in tycTyc-h"

      and g (tycon as (GENtyc{kind as DATATYPE _, arity, ...})) = 
              if TU.eqTycon(tycon, BT.refTycon) then LT.tcc_ref 
              else h(kind, arity)
        | g (GENtyc{kind, arity, ...}) = h(kind, arity)
        | g (DEFtyc{tyfun, ...}) = tfTyc(tyfun, d)
        | g (RECtyc i) = recTyc i
        | g (RECORDtyc _) = bug "unexpected RECORDtyc in tycTyc-g"
        | g (PATHtyc{arity, path=InvPath.IPATH ss, entPath}) = 
              ((* say "*** Warning for compiler writers: PATHtyc ";
               app (fn x => (say (Symbol.name x); say ".")) ss;
               say " in translate: ";
               say (EntPath.entPathToString entPath);
               say "\n"; *)
               if arity > 0 then LT.tcc_fn(LT.tkc_args arity, LT.tcc_void)
               else LT.tcc_void
               (* raise STRANGE *))
                (* bug "unexpected PATHtyc in tycTyc-g" *)
        | g (ERRORtyc) = bug "unexpected tycon in tycTyc-g"

   in (g tc) 
(*    handle STRANGE => 
        (say "printing tycon **>>>: ";
         ppTycon tc; say "<<<**\n";
         raise STRANGE)
*)
  end

and tfTyc (TYFUN{arity=0, body}, d) = toTyc d body
  | tfTyc (TYFUN{arity, body}, d) = 
      let val ks = LT.tkc_args arity
       in LT.tcc_fn(ks, toTyc (DI.next d) body)
      end

and toTyc d t = 
  let fun h (INSTANTIATED t) = g t
        | h (LBOUND {depth, num}) = LT.tcc_var(DI.calc(d, depth), num)
        | h (OPEN _) = LT.tcc_void
        | h _ = LT.tcc_void  (* ZHONG? *)

      and g (VARty tv) = h(!tv)
        | g (CONty(RECORDtyc _, [])) = LT.tcc_unit
        | g (CONty(RECORDtyc _, ts)) = LT.tcc_tup (map g ts)
        | g (CONty(tyc, [])) = tycTyc(tyc, d)
        | g (CONty(DEFtyc{tyfun,...}, args)) = g(TU.applyTyfun(tyfun,args))
        | g (CONty(tc as GENtyc {kind=ABSTRACT _, ...}, ts)) = 
              LE.tcAppSt(tycTyc(tc, d), map g ts)
        | g (CONty(tc as GENtyc _, [t1, t2])) = 
              if TU.eqTycon(tc, BT.arrowTycon) then LT.tcc_arw(g t1, g t2)
              else LT.tcc_app(tycTyc(tc, d), [g t1, g t2])
        | g (CONty(tyc, ts)) = LT.tcc_app(tycTyc(tyc, d), map g ts)
        | g (IBOUND i) = LT.tcc_var(DI.innermost, i)
        | g (POLYty _) = bug "unexpected poly-type in toTyc"
        | g (UNDEFty) = bug "unexpected undef-type in toTyc"
        | g (WILDCARDty) = bug "unexpected wildcard-type in toTyc"

   in (g t) 
      (* handle STRANGE =>
        (say "printing type **-->: ";
         ppType t; say "<--**\n";
         raise STRANGE)
       *)

  end

and toLty d (POLYty {tyfun=TYFUN{arity=0, body}, ...}) = toLty d body
  | toLty d (POLYty {tyfun=TYFUN{arity, body},...}) = 
      let val ks = LT.tkc_args arity
       in LT.ltc_poly(ks, toLty (DI.next d) body)
      end

  | toLty d x = LT.ltc_tyc (toTyc d x)

(*
val toLty = (fn x => fn y =>
      (toLty x y) handle STRANGE => 
        (say "printing type **:"; 
         ppType y; say "** \n";
         bug "unexpected PATHtyc"))
*)

(*
val toTyc  = 
  (fn x => fn y => 
     (Stats.doPhase(Stats.makePhase "Compiler 043 2-toTyc") (toTyc x) y))

val toLty  = 
  (fn x => fn y => 
     (Stats.doPhase(Stats.makePhase "Compiler 043 3-toLty") (toLty x) y))
*)

end (* toplevel local *)
end (* structure TransTypes *)


(*
 * $Log: transtypes.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:48  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.8  1997/11/24 22:30:05  dbm
 *   Fix for bug 1315.
 *
 * Revision 1.7  1997/09/15  16:03:38  dbm
 *   Eliminated rest of "*** Warning for compiler writers..." message.
 *
 * Revision 1.6  1997/09/10  22:16:02  dbm
 *   Commented out warning message: "*** Warning for compiler writers: PATHtyc ".
 *
 * Revision 1.5  1997/04/18  15:49:04  george
 *   Cosmetic changes on some constructor names. Changed the shape for
 *   FIX type to potentially support shared dtsig. -- zsh
 *
 * Revision 1.4  1997/04/02  04:18:09  dbm
 *   Added general default case to local function h at line 192.
 *
 * Revision 1.3  1997/03/17  19:00:27  dbm
 * Changes in datatype representation to support datatype replication.
 *
 * Revision 1.2  1997/02/26  21:55:08  george
 *    A temporary fix for the exception STRANGE bug, BUG 1124. A better fix
 *    would require the faithful instantiation of datatypes in functor signatures.
 *
 *)

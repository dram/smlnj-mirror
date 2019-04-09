(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* pequal.sml *)

signature PEQUAL = 
sig

  (* 
   * Constructing generic equality functions; the current version will
   * use runtime polyequal function to deal with abstract types. (ZHONG)
   *)
  val equal : {getStrEq : unit -> PLambda.lexp, 
               getPolyEq : unit -> PLambda.lexp} * StaticEnv.staticEnv 
               -> (Types.ty * Types.ty * DebIndex.depth) -> PLambda.lexp

  val debugging : bool ref     

end (* signature PEQUAL *)


structure PEqual : PEQUAL = 
struct

local structure DA = Access
      structure EM = ErrorMsg
      structure T  = Types
      structure BT = BasicTypes
      structure LT = LambdaType
      structure TT = TransTypes
      structure TU = TypesUtil
      structure SE = StaticEnv
      structure PO = PrimOp
      structure PP = PrettyPrint
      open Types PLambda 

in

val debugging = ref false
fun bug msg = ErrorMsg.impossible("Equal: "^msg)
val say = Control.Print.say

val --> = BT.-->
infix -->

(*
 * MAJOR CLEANUP REQUIRED ! The function mkv is currently directly taken 
 * from the LambdaVar module; I think it should be taken from the 
 * "compInfo". Similarly, should we replace all mkLvar in the backend
 * with the mkv in "compInfo" ? (ZHONG)
 *)
val mkv = LambdaVar.mkLvar

(* 
 * Is TU.dconType necessary, or could a variant of transTyLty that 
 * just takes tyc and domain be used in transDcon??? 
 *)
fun transDcon(tyc, {name,rep,domain}, d) =
      (name, rep, TT.toLty d (TU.dconType(tyc,domain)))

val (trueDcon', falseDcon') = 
  let fun h (DATACON{name, rep, ...}) = (name, rep, LT.ltc_bool)
   in (h BT.trueDcon, h BT.falseDcon)
  end

val (trueLexp, falseLexp) =
  let val unitLexp = INT 0
   in (CON (trueDcon', [], unitLexp), CON (falseDcon', [], unitLexp))
  end

fun argType(domain, []) = domain
  | argType(domain, args) =
      TU.applyTyfun(TYFUN{arity=length args,body=domain},args)

fun reduceTy ty =
  (case TU.headReduceType ty
    of POLYty{tyfun=TYFUN{body,...},...} => reduceTy body
     | ty => ty)

fun expandREC (members: T.dtmember vector) =
  let fun g (RECtyc i) = 
           let val {tycname,stamp,dcons,arity,eq,sign,lambdatyc} =
	           Vector.sub(members,i)
            in GENtyc{stamp=stamp,arity=arity,eq=ref(YES), (* why not "eq=ref(!eq)"
							      or "eq=eq"? *)
		      kind=DATATYPE{index=i, members=members,
                                    lambdatyc=ref NONE}, (* why not =lambdatyc ? *)
		      path=InvPath.IPATH[tycname]}
	   end
        | g x = x
      fun f(CONty(tyc,tyl)) = CONty(g tyc, map f tyl)
        | f(x as IBOUND _) = x
        | f _ = bug "unexpected type in expandREC"
   in f
  end

exception Poly

fun equivType(ty,ty') =
  let fun eq(ty as CONty(tycon, args), ty' as CONty(tycon', args')) =
              (if TU.eqTycon(tycon, tycon')
	       then ListPair.all equivType (args,args') 
	       else (equivType(TU.reduceType ty, ty')
		     handle ReduceType =>
			 (equivType(ty,TU.reduceType ty')
 		    handle ReduceType => false)))
        | eq(VARty _, _) = raise Poly
        | eq(_, VARty _) = raise Poly
        | eq(POLYty _, _) = raise Poly
        | eq(_, POLYty _) = raise Poly
        | eq _ = false
   in eq(TU.prune ty, TU.prune ty')
  end

(****************************************************************************
 *                   Commonly-used Lambda Types                             *
 ****************************************************************************)

val boolty = LT.ltc_bool
fun eqLty lt = LT.ltc_arw(LT.ltc_tup [lt, lt], boolty)
val inteqty = eqLty (LT.ltc_int)
val int32eqty = eqLty (LT.ltc_int32)
val booleqty = eqLty (LT.ltc_bool)
val realeqty = eqLty (LT.ltc_real)

exception Notfound

(****************************************************************************
 *              equal --- the equality function generator                   *
 ****************************************************************************)
fun equal ({getStrEq, getPolyEq}, env) 
          (polyEqTy : ty, concreteType : ty, d) =
let 

val cache : (ty * lexp * lexp ref) list ref = ref nil

fun enter ty =
  let val v = VAR(mkv())
      val r = ref v
   in if !debugging 
      then PP.with_pp (EM.defaultConsumer())
            (fn ppstrm => (PP.add_string ppstrm "enter: ";
               PPType.resetPPType(); PPType.ppType env ppstrm ty))
      else ();
      cache := (ty, v, r) :: !cache; (v,r)
  end

fun find ty =
  let fun f ((t,v,e)::r) = if equivType(ty,t) then v else f r
        | f [] = (if !debugging
                  then say "equal.sml-find-notfound\n" else ();
                  raise Notfound)
   in if !debugging 
      then PP.with_pp (EM.defaultConsumer())
           (fn ppstrm => (PP.add_string ppstrm "find: ";
                          PPType.resetPPType();
                          PPType.ppType env ppstrm ty))
      else ();
      f (!cache)
  end

fun eqTy ty = eqLty(TT.toLty d ty)
fun ptrEq(p, ty) = PRIM(p, eqTy ty, [])
fun prim(p, lt) = PRIM(p, lt, [])

fun atomeq (tyc, ty) =
  if TU.equalTycon(tyc,BT.intTycon) then prim(PO.IEQL,inteqty)
  else if TU.equalTycon(tyc,BT.int32Tycon) then prim(PO.IEQL,int32eqty)
  else if TU.equalTycon(tyc,BT.wordTycon) then prim(PO.IEQL,inteqty)
  else if TU.equalTycon(tyc,BT.word8Tycon) then prim(PO.IEQL,inteqty)
  else if TU.equalTycon(tyc,BT.charTycon) then prim(PO.IEQL,inteqty)
  else if TU.equalTycon(tyc,BT.word32Tycon) then prim(PO.IEQL,int32eqty)
  else if TU.equalTycon(tyc,BT.boolTycon) then prim(PO.IEQL,booleqty) 
  else if TU.equalTycon(tyc,BT.realTycon) then prim(PO.FEQLd,realeqty)
  else if TU.equalTycon(tyc,BT.stringTycon) then getStrEq()
  else if TU.equalTycon(tyc,BT.refTycon) then ptrEq(PO.PTREQL, ty) 
  else if TU.equalTycon(tyc,BT.arrayTycon) then ptrEq(PO.PTREQL, ty)
  else if TU.equalTycon(tyc,BT.word8arrayTycon) then ptrEq(PO.PTREQL, ty)
  else if TU.equalTycon(tyc,BT.real64arrayTycon) then ptrEq(PO.PTREQL, ty)
  else raise Poly

fun test(ty, 0) = raise Poly
  | test(ty, depth) =
     (if !debugging
      then PP.with_pp (EM.defaultConsumer())
           (fn ppstrm => (PP.add_string ppstrm "test: ";
                          PPType.resetPPType();
                          PPType.ppType env ppstrm ty))
      else ();

      case ty
       of VARty(ref(INSTANTIATED t)) => test(t,depth)
        | CONty(DEFtyc _, _) => test(TU.reduceType ty,depth)
        | CONty(RECORDtyc _, tyl) =>
            (find ty handle Notfound =>
               let val v = mkv() and x=mkv() and y=mkv()
                   val (eqv, patch) = enter ty
                   fun loop(n, [ty]) = 
                         APP(test(ty,depth), 
                             RECORD[SELECT(n, VAR x),
                             SELECT(n, VAR y)])

                     | loop(n, ty::r) = 
                         SWITCH(loop(n,[ty]), BT.boolsign,
                                [(DATAcon(trueDcon'), loop(n+1,r)),
                                 (DATAcon(falseDcon'), falseLexp)],
                                NONE)

                     | loop(_,nil) = trueLexp

                   val lt = TT.toLty d ty
                in patch := FN(v, LT.ltc_tup [lt,lt],
                             LET(x, SELECT(0, VAR v),
                               LET(y, SELECT(1, VAR v), 
                                    loop(0, tyl))));
                   eqv
               end)

        | CONty(tyc as GENtyc{kind=PRIMITIVE _,eq=ref YES,...}, tyl) =>
            atomeq (tyc, ty)

        | CONty(GENtyc{eq=ref ABS,stamp,arity,kind,path}, tyl) =>
            test(TU.mkCONty(GENtyc{eq=ref YES,stamp=stamp,arity=arity,
                                   kind=kind,path=path}, tyl), depth)
            (* assume that an equality datatype has been converted
               to an abstract type in an abstype declaration *)

        | CONty(tyc as GENtyc{kind=DATATYPE{index,members,lambdatyc},
                              ...}, tyl) =>
            let val {dcons=dcons0,...} = Vector.sub(members,index)
                fun expandRECdcon{domain=SOME x, rep, name} = 
                     {domain=SOME(expandREC members x),rep=rep,name=name}
                  | expandRECdcon z = z

             in case map expandRECdcon dcons0
                 of [{rep=REF,...}] => atomeq(tyc, ty)
                  | dcons =>                          
                     (find ty handle Notfound =>
                       let val v = mkv() and x=mkv() and y=mkv()
                           val (eqv, patch) = enter ty
                           fun inside {domain=NONE,...} = trueLexp
                             | inside (c as {domain = SOME dom,...}) =
                               (case reduceTy dom
                                 of (CONty(RECORDtyc [],_)) => trueLexp
                                  | _ =>
                                    (let val b = TT.toLty d dom
                                         val argt = argType(dom, tyl)
                                         val tcs = map (TT.toTyc d) tyl
                                         val c' = transDcon(tyc, c, d)
                                         fun decon(c, e) = DECON(c, tcs, e)
                                      in APP(test(argt, depth-1),
                                             RECORD[decon(c',VAR x),
                                                    decon(c',VAR y)])
                                     end))
                           val lt = TT.toLty d ty
                           val argty = LT.ltc_tup [lt,lt]
                           val pty = LT.ltc_arw(argty, boolty)
 
                           val body = 
                             case dcons
                              of [] => bug "empty data types"
                               | [dcon] => inside dcon       
                               | _ =>
                                  let (** this is somewhat a hack !!!! *)
                                      (* val sign = map #rep dcons *)
                                      fun isConst(DA.CONSTANT _) = true
                                        | isConst(DA.LISTNIL) = true
                                        | isConst _ = false

                                      fun getCsig({rep=a,domain,name}::r,c,v)= 
                                           if isConst a then getCsig(r, c+1, v)
                                           else getCsig(r, c, v+1)
                                        | getCsig([], c, v) = DA.CSIG(v,c)

                                      val sign = getCsig(dcons,0,0)

                                      fun concase dcon =
                                        let val dcon' =
                                              DATAcon(transDcon(tyc,dcon,d))
                                         in (dcon',
                                             SWITCH(VAR y, sign, 
                                                  [(dcon', inside dcon)],
                                                  SOME(falseLexp)))
                                        end
                                   in SWITCH(VAR x, sign, 
                                        map concase dcons, NONE)
                                  end

                           val body = 
                             SWITCH(APP(PRIM(PO.PTREQL, pty, []), 
                                        RECORD[VAR x, VAR y]),
                                    BT.boolsign,
                                    [(DATAcon(trueDcon'), trueLexp),
                                     (DATAcon(falseDcon'), body)],
                                    NONE)
                        in patch := FN(v, argty,
                                     LET(x, SELECT(0, VAR v),
                                      LET(y, SELECT(1, VAR v), body)));
                           eqv
                       end)
            end

        | _ => raise Poly)

val body = test(concreteType, 10)
val fl = !cache

in 

(case fl 
  of [] => body
   | _ => let fun g ((ty, VAR v, e), (vs, ts, es)) = 
                        (v::vs, (eqTy ty)::ts, (!e)::es)
                | g _ = bug "unexpected equality cache value"

              val (vs, ts, es) = foldr g ([], [], []) fl
           in FIX(vs, ts, es, body)
          end)

end handle Poly => 
  (GENOP({default=getPolyEq(),
          table=[([LT.tcc_string], getStrEq())]}, 
         PO.POLYEQL, TT.toLty d polyEqTy, 
         [TT.toTyc d concreteType]))


end (* toplevel local *)                       
end (* structure Equal *)

(*
 * $Log: pequal.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:48  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/12/03 21:23:42  dbm
 *   Added pointer equality optimization (in function atomeq) for new
 *   primitive types word8array and real64array.
 *
# Revision 1.1  1997/05/06  01:15:36  george
#   Version 109.27+
#
 * Revision 1.3  1997/03/17  19:00:08  dbm
 * Changes in datatype representation to support datatype replication.
 *
 * Revision 1.2  1997/02/26  21:53:28  george
 *    Fix a derived bug from the STRANGE-exception bug.
 *
 *)

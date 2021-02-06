(* translate.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TRANSLATE =
sig

  (* Invariant: transDec always applies to a top-level absyn declaration *)
  val transDec : { rootdec: Absyn.dec,
		   exportLvars: Access.lvar list,
                   oldenv: StaticEnv.staticEnv,
                   env: StaticEnv.staticEnv,
		   cproto_conv: string,
		   compInfo: Absyn.dec CompInfo.compInfo }
                 -> {flint: FLINT.prog,
                     imports: (PersStamps.persstamp
                               * ImportTree.importTree) list}

end (* signature TRANSLATE *)

structure Translate : TRANSLATE =
struct

local structure B  = Bindings
      structure BT = BasicTypes
      structure DA = Access
      structure DI = DebIndex
      structure EM = ErrorMsg
      structure LV = LambdaVar
      structure AS = Absyn
      structure AU = AbsynUtil
      structure LT = PLambdaType   (* = LtyExtern *)
      structure M  = Modules
      structure MC = MatchComp
      structure PO = Primop
      structure PP = PrettyPrint
      structure PU = PPUtil
      structure S  = Symbol
      structure SP = SymPath
      structure LN = LiteralToNum
      structure TT = TransTypes
      structure T = Types
      structure TU = TypesUtil
      structure V  = VarCon
      structure EU = ElabUtil
      structure Tgt = Target

      structure IIMap = RedBlackMapFn (type ord_key = IntInf.int
				       val compare = IntInf.compare)

      open Absyn PLambda TransUtil
in

(****************************************************************************
 *                   DEBUGGING AND PRETTYPRINTING                           *
 ****************************************************************************)

val debugging = FLINT_Control.trdebugging
fun bug msg = EM.impossible("Translate: " ^ msg)
fun warn msg = EM.warn("Translate: " ^ msg)

val say = Control.Print.say
fun newline () = say "\n"
fun debugmsg (msg : string) =
    if !debugging then (say msg; say "\n") else ()

val ppDepth = Control.FLINT.printDepth

val with_pp = PP.with_default_pp

fun ppPat pat =
    PP.with_default_pp
      (fn ppstrm => PPAbsyn.ppPat StaticEnv.empty ppstrm (pat, (!ppDepth)))

fun ppExp exp =
    PP.with_default_pp
      (fn ppstrm => PPAbsyn.ppExp (StaticEnv.empty,NONE) ppstrm (exp, (!ppDepth)))

fun ppDec dec =
    PP.with_default_pp
      (fn ppstrm => PPAbsyn.ppDec (StaticEnv.empty,NONE) ppstrm (dec, (!ppDepth)))

fun ppType msg ty =
    PP.with_default_pp
	(fn ppstrm => (PP.string ppstrm (msg^": "); PPType.ppType StaticEnv.empty ppstrm ty))

fun ppLexp lexp =
    PP.with_default_pp
      (fn ppstrm => PPLexp.ppLexp (!ppDepth) ppstrm lexp)

fun ppTycArgs tycs =
    PP.with_default_pp
      (fn ppstrm =>
	  PPUtil.ppBracketedSequence ("[", "]", (PPLty.ppTyc 50)) ppstrm tycs)

	
(****************************************************************************
 *                   TRANSLATING NUMBER LITERALS                            *
 ****************************************************************************)

(* listUnion : T.tyvar list * T.tyvar list -> T.tyvar list *)
(* utility function used in transRVBs -- append without duplicates *)
fun listUnion (l : T.tyvar list, m : T.tyvar list) =
    let val l' = List.filter (fn tv => (not (List.exists (fn tv' => tv' = tv) m))) l
     in l' @ m
    end

(* mkTyargs : T.tyvar list * T.tyvar list * 'a * (int -> 'a) -> 'a list *)
(* map each tyvar, tv, in fromBbtvs to some form of "type" derived from the _position_, k,
 * of that tyvar in the toBtvs list (maker (k,tv)), or to default if tv is not in toBtvs.
 * The length of the result is therefore the same as the length of fromBtvs. *)
fun mkTyargs (fromBtvs, toBtvs, default, maker) =
    let fun search (tv: Types.tyvar, k, []) = default
	  | search (tv, k, tv'::r) = if TU.eqTyvar(tv, tv') then maker(k,tv) else search (tv, k+1, r)
	fun lookup tv = search (tv, 0, toBtvs)
     in map lookup fromBtvs
    end

(* transNum : Types.ty IntConst.t * T.ty -> int IntConst.t *)
(* Translates a front-end numeric literal (Types.ty IntConst.t) into a FLINT-style
 * numeric literal representation (int IntCons.t).
 * QUESTION: perhaps we should preserve the size, in the case of
 * word8, for better jump tables?  Also, chars are represented as default ints. *)
fun transNum ({ival, ty}: T.ty IntConst.t) : con =
    let fun mkWORD sz = WORDcon{ival = ival, ty = sz}  (* FLINT-style literal *)
	fun mkINT sz  = INTcon{ival = ival, ty = sz}   (* FLINT-style literal *)
	val defaultIntSz = (* 63 *) Target.defaultIntSz
     in if TU.equalType(ty, BT.intTy)
	  then mkINT defaultIntSz
	else if TU.equalType(ty, BT.int32Ty)
	  then mkINT 32
	else if TU.equalType(ty, BT.int64Ty)
	  then mkINT 64
	else if TU.equalType(ty, BT.intinfTy)
	  then mkINT 0
	else if TU.equalType(ty, BT.wordTy)
	  then mkWORD defaultIntSz
	else if TU.equalType(ty, BT.word8Ty)
	  then mkWORD defaultIntSz  (* or:  mkWORD 8 (if we want accurate char size) *)
	else if TU.equalType(ty, BT.word32Ty)
	  then mkWORD 32
        else if TU.equalType(ty, BT.word64Ty)
          then mkWORD 64
	else bug "transNum"
    end

(* selectTyArgs : tyvar list * tyvar list -> LT.tyc list  -- not currently used *)
(* pattvs are the generalized tyvars of the pattern, vartvs are those for a variable
 * in the pattern *)
fun selectTyArgs (pattvs, vartvs) =
    let val vartvsArity = length vartvs
	val indices = List.tabulate(vartvsArity, (fn x => x))
	(* 0-based indexes into btvs, the bound type variable
	 * sequence for _this_ bvar *)
	val tvToIndex = ListPair.zip(vartvs,indices)
	fun lookup (tv: Types.tyvar, nil) = NONE
	  | lookup (tv, (tv',k)::r) = if tv = tv' then SOME k else lookup (tv,r)
	val targs = map (fn tv => case lookup (tv, tvToIndex)
				   of NONE => LT.tcc_void
				    | SOME k => LT.tcc_var(1,k))
			pattvs
    in targs
    end

(****************************************************************************
 *                          MAIN FUNCTION                                   *
 *                                                                          *
 *  val transDec : Absyn.dec * Access.lvar list                             *
 *                 * StaticEnv.staticEnv * CompBasic.compInfo               *
 *                 -> {flint: FLINT.prog,                                   *
 *                     imports: (PersStamps.persstamp                       *
 *                               * ImportTree.importTree) list}             *
 ****************************************************************************)

fun transDec
	{ rootdec, exportLvars, oldenv, env, cproto_conv,
	 compInfo as {errorMatch,error,...}: Absyn.dec CompInfo.compInfo } =
let

(* lvar generator taken from compInfo *)
val mkvN = #mkLvar compInfo
fun mkv () = mkvN NONE

(** generate the set of ML-to-FLINT type translation functions *)
val {tpsKnd, tpsTyc, toTyc, toLty, strLty, fctLty} =
    TT.genTT()
fun toTcLt d = (toTyc d, toLty d)

(* toDconLty : DebIndex.depth -> Types.ty -> lty *)
(** translating the typ field in DATACON into lty; constant datacons
    will take ltc_unit as the argument *)
fun toDconLty d ty =
    (case ty
       of T.POLYty{sign, tyfun=T.TYFUN{arity, body}} =>
            if BT.isArrowType body then toLty d ty
            else toLty d (T.POLYty{sign=sign,
				    tyfun=T.TYFUN{arity=arity,
						   body=BT.-->(BT.unitTy, body)}})
	| _ => if BT.isArrowType ty then toLty d ty
               else toLty d (BT.-->(BT.unitTy, ty)))

(* CON' : Plambda.dataconstr * PlambdType.tyc list * lexp -> lexp *) 
(* version of CON with special translation of ref and susp pseudo datacons *)
fun CON' ((_, DA.REF, lt), ts, e) = APP (PRIM (PO.MAKEREF, lt, ts), e)
  | CON' ((_, DA.SUSP (SOME(DA.LVAR d, _)), lt), ts, e) =
      let val v   = mkv ()
          val fe = FN (v, LT.ltc_tuple [], e)
       in APP(TAPP (VAR d, ts), fe)
      end
  | CON' x = CON x

fun patToConsig (APPpat(dcon,_,_)) = TU.dataconSign dcon
  | patToConsig (CONpat(dcon,_)) = TU.dataconSign dcon
  | patToConsig _ = DA.CNIL

(*
 * The following code implements the exception tracking and
 * errormsg reporting.
 *)

local
  val region = ref(0,0)
  val markexn = PRIM(PO.MARKEXN,
		  LT.ltc_parrow(LT.ltc_tuple [LT.ltc_exn, LT.ltc_string],
				LT.ltc_exn), [])
in

fun withRegion loc f x =
  let val r = !region
   in (region := loc; f x before region:=r)
      handle e => (region := r; raise e)
  end

fun mkRaise(x, lt) =
  let val e = if !Control.trackExn
              then APP(markexn, RECORD[x, STRING(errorMatch(!region))])
              else x
   in RAISE(e, lt)
  end

fun complain msg = error (!region) msg
fun repErr msg = complain EM.COMPLAIN msg EM.nullErrorBody
fun repWarn msg = complain EM.WARN msg EM.nullErrorBody
fun repPolyEq () =
    if !Control.polyEqWarn then complain EM.WARN "calling polyEqual" EM.nullErrorBody
    else ()

end (* markexn-local *)

(***************************************************************************
 *          SHARING AND LIFTING OF STRUCTURE IMPORTS AND ACCESSES          *
 ***************************************************************************)

(* _Dependent_ or _secondary_ lvars are defined in terms of a base lvar via
 * an access path (int list). These will be bound to a nested SELECT expression
 * rooted at the base lvar. "Variables" with access of the form, e.g.,
 * PATH(PATH(LVAR lv, i1),i2) are translated to the lexp 
 * SELECT(i2, SELECT(i1, VAR lv)).  A given base lvar may have several
 * dependent lvars defined by paths based on that lvar. These are registered
 * in a "dependentLvarsTable" lvar hash table. Each dependent variable is
 * uniquely determined by the base lvar and the access path. *)

exception DEP_LVAR_TABLE
type key = int  (* hash of an accesspath : int list *)
type accesspath = int list
type depLvar = (key * accesspath * lvar)
(* "Dependent lvars and their accesspaths off the base lvar mapping to the depLvar,
 * where the key is the hash of the accesspath *)

(** dependentLvarsTable: lvar --> depLvar list 
    or lvar -> (<hashkey(accesspath)> (= int) * accesspath (= int list) * lvar) list *)
val dependentLvarsTable : depLvar list LambdaVar.Tbl.hash_table =
    LambdaVar.Tbl.mkTable (32, DEP_LVAR_TABLE)

(* hashkey : int list -> key *)
(* hash of an accesspath (an int list) *)
fun hashkey ints = foldr (fn (x,y) => ((x * 10 + y) mod 1019)) 0 ints

(* buildHeader : lvar -> (lexp -> lexp) *)
(* creates a wrapper function that wraps a nested sequence of let declarations
 * around a base lvar (the argument), where the definiens of each let is nested SELECTS for
 * the accesspath in each depLvar item starting from VAR lvar for the baseLvar. *)
fun buildHeader baseLvar =
  let val depLvars = LambdaVar.Tbl.lookup dependentLvarsTable baseLvar
      fun wrapHeader((_, accesspath, accessLvar), hdr) =
             let val accessExp = foldl (fn (k,e) => SELECT(k,e)) (VAR baseLvar) accesspath
	      in fn e => hdr (LET (accessLvar, accessExp, e))
	     end
   in foldr wrapHeader ident depLvars   (* ident = TransUtil.ident = identity fn *)
  end handle DEP_LVAR_TABLE => ident   (* if lvar not in dependentLvarsTable? *)

(* bindvar : lvar * accesspath * symbol option -> lvar *)
(* returns an dependent lvar to be bound to lvar via accesspath selection, and
 * if it is new, registers it in dependentLvarsTable *)
fun bindvar (lvar, [], _) = lvar  (* no access path, hence no new dependent lvar *)
  | bindvar (baseLvar, accesspath, nameOp) =
      let val depLvars = LambdaVar.Tbl.lookup dependentLvarsTable baseLvar
	  	         handle DEP_LVAR_TABLE => []
          val key = hashkey accesspath  (* hash of accesspath *)
          fun look [] =
                let val dependentLvar = mkvN nameOp
                in LambdaVar.Tbl.insert dependentLvarsTable
		     (baseLvar, (key, accesspath, dependentLvar) :: depLvars);
		   dependentLvar
                end
            | look ((key', accesspath', dependentLvar) :: rest) =
                if (key' = key) andalso (accesspath' = accesspath)
	        then dependentLvar
	        else look rest
       in look depLvars
      end

datatype pidInfo = ANON of (int * pidInfo) list
                 | NAMED of lvar * lty * (int * pidInfo) list

(* mkPidInfo : lty * int list * symbol option -> pidInfo * lvar *)
fun mkPidInfo (lty, l, nameOp) =
    let val lvar = mkvN nameOp
	fun mkpi [] = NAMED(lvar, lty, [])
          | mkpi (a::r) = ANON [(a, mkpi r)]
     in (mkpi l, lvar)
    end

(* mergePidInfo : pidInfo * lty * int list * symbol option -> pidInfo * lvar *)
fun mergePidInfo (pi, lty, l, nameOp) =
  let fun merge (pi as NAMED(v,_,_), []) = (pi, v)
        | merge (ANON xl, [])  =
            let val lvar = mkvN nameOp
             in (NAMED(lvar, lty, xl), lvar)
            end
        | merge (z, a::r) =
            let val (xl, mknode) =
                    case z of ANON c => (c, ANON)
                            | NAMED (v,tt,c) => (c, fn x => NAMED(v,tt,x))

                fun dump ((np, v), z, y) =
                      let val nz = (a, np) :: z
                       in (mknode((rev y) @ nz), v)
                      end

                fun look ([], y) = dump(mkPidInfo(lty, r, nameOp), [], y)
                  | look (u as ((x as (i,pi))::z), y) =
		      (case Int.compare (i, a)
		         of LESS => look(z, x::y)
                          | EQUAL => dump (merge (pi, r), z, y)
                          | GREATER => dump (mkPidInfo (lty, r, nameOp), u, y))

             in look(xl, [])
            end
   in merge(pi, l)
  end (* mergePidInfo *)

(** a map that stores information about external references *)
val persmap = ref (PersMap.empty : pidInfo PersMap.map)

(* mkPid : pid * lty * int list * symbol option -> lvar *)
fun mkPid (pid, lty, l, nameOp) =
    case PersMap.find (!persmap, pid)
      of NONE =>
	  let val (pinfo, var) = mkPidInfo (lty, l, nameOp)
	   in persmap := PersMap.insert(!persmap, pid, pinfo);
	      var
	  end
       | SOME pinfo =>
	  let val (newPinfo, var) = mergePidInfo (pinfo, lty, l, nameOp)
	      fun rmv (key, map) = (* clear the old pinfo for pid *)
		  let val (newMap, _) = PersMap.remove(map, key)
		  in newMap
		  end handle e => map
	   in persmap := PersMap.insert(rmv (pid, !persmap), pid, newPinfo);
	      var
	  end

val iimap = ref (IIMap.empty : lvar IIMap.map)

(* getII : IntInf.int -> lvar *)
(* uses iimap to map IntInf.int to lvars, creating new mappings is necessary *)
fun getII n =
    case IIMap.find (!iimap, n)
      of SOME lvar => lvar
       | NONE =>
	 let val lvar = mkv ()
	  in iimap := IIMap.insert (!iimap, n, lvar);
	     lvar
	 end

(* transAccessTyped : A.access * lty * S.symbol option -> lvar *)
(* translating an access with type into a VAR lexp, which is registered
 * in dependentLvarsTable if local (rooted at LVAR), or in persmap if external,
 * (rooted at EXTERN). This returns an lvar rather than a VAR because it
 * is used for both variables (including var, str, fct) and constructors. *)
fun transAccessTyped (access, lty, nameOp) =
  let fun register (DA.LVAR lvar, accesspath) = bindvar (lvar, accesspath, nameOp)
        | register (DA.EXTERN pid, accesspath) = mkPid (pid, lty, accesspath, nameOp)
        | register (DA.PATH(a,i), accesspath) = register (a, i::accesspath)
        | register _ = bug "unexpected access in transAccessTyped"
   in register (access, [])
  end

(* transAccessLocal : A.access * S.symbol option -> lexp *)
(* translating a "local" (LVAR or PATH rooted at an LVAR) access into a
 * VAR lexp, and registering it in dependentLvarsTable if accesspath is not null. *)
fun transAccessLocal (access, nameOp) =
  let fun register (DA.LVAR lvar, accesspath) = bindvar(lvar, accesspath, nameOp)
        | register (DA.PATH(a,i), accesspath) = register (a, i::accesspath)
        | register _ = bug "unexpected access in transAccess"
   in VAR (register (access, []))
  end

(* transAccess: DA.access * lty * S.symbol option -> lexp *)
(* translating an access into a VAR lexp, using transAccessTyped if
 * the access is external, or transAccessLocal if it is local *)
fun transAccess (access, lty, nameOp) =
    if extern access    (* TransUtil.extern, check if access is EXTERN-based *)
    then VAR (transAccessTyped (access, lty, nameOp))
    else transAccessLocal (access, nameOp)

(*
 * These two functions are major gross hacks. The NoCore exceptions would
 * be raised when compiling boot/dummy.sml, boot/assembly.sig, and
 * boot/core.sml; the assumption is that the result of coreExn and coreAcc
 * would never be used when compiling these three files. A good way to
 * clean up this is to put all the core constructors and primitives into
 * the primitive environment. (ZHONG)
 *
 * NOTE: the CoreAccess structure (ElabData/stateenv/coreacc.sml) also
 * defines a NoCore exception, but does not export it.  Does it make
 * sense to combine these things?
 *)
exception NoCore

(* coreExn : S.symbol list -> lexp option *)
(* Accessing _constructors_ via the Core structure.
 * Used in TansPrim.trans to access Subscript, Assembly.Div, and Char
 * exception constructors. *)
fun coreExn ids =
    (case CoreAccess.getCon' (fn () => raise NoCore) oldenv ids
      of T.DATACON { name, rep as DA.EXN _, typ, ... } =>
	   let val lty = toDconLty DI.top typ
	       val newrep = mkRep(rep, lty, name)
	       val _ = debugmsg ">>coreExn in translate.sml: "
              (* val _ = PPLexp.printLexp (CON'((name, nrep, nt), [], unitLexp))
	         val _ = print "\n" *)
            in SOME (CON'((name, newrep, lty), [], unitLexp))
           end
       | _ => bug "coreExn in translate"
      (* end case *))
    handle NoCore => NONE

(* coreAcc : symbol -> lexp *)
(* Accessing variables via the Core structure, and localizing access via
 * transAccessTyped. *)
and coreAcc id =
    (case CoreAccess.getVar' (fn () => raise NoCore) oldenv [id]
       of V.VALvar { access, typ, path, ... } =>
	    VAR (transAccessTyped (access, toLty DI.top (!typ), getNameOp path))
        | _ => bug "coreAcc in translate"
    (* end case *))
    handle NoCore =>
	(warn(concat["no Core access for '", id, "'\n"]);
	 INT{ival = 0, ty = Tgt.defaultIntSz})

(* mkRep : DA.conrep * lty * S.symbol -> DA.conrep *)
(* "localize" the conrep's access, for exception or SUSP constructors *)
and mkRep (rep, lty, name) =
    (case rep
       of (DA.EXN access) =>
          let (* val _ = print (concat ["mkRep:EXN:", " ", S.name name,
				     " ", DA.prAcc access, "\n"]) *)
		 val (argt, _) = LT.ltd_parrow lty
		 val lvar = transAccessTyped (access, LT.ltc_etag argt, SOME name)
              in DA.EXN (DA.LVAR lvar)
             end
        | (DA.SUSP NONE) =>  (* a hack to support "delay-force" primitives *)
             (case (coreAcc "delay", coreAcc "force")
               of (VAR x, VAR y) => DA.SUSP(SOME (DA.LVAR x, DA.LVAR y))
                | _ => bug "unexpected case on mkRep SUSP 1")
        | (DA.SUSP (SOME _)) => bug "unexpected case on mkRep SUSP 2"
        | _ => rep)

(** The runtime polymorphic equality and string equality dictionary. *)
val eqDict =
  let val strEqRef : lexp option ref = ref NONE
      val polyEqRef : lexp option ref = ref NONE
      val intInfEqRef : lexp option ref = ref NONE

      fun getStrEq () =
        (case (!strEqRef)
          of SOME e => e
           | NONE => (let val e = coreAcc "stringequal"
                       in strEqRef := (SOME e); e
                      end))

      fun getIntInfEq () =		(* same as polyeq, but silent *)
	  case !intInfEqRef of
	      SOME e => e
	    | NONE => let val e =
			      TAPP (coreAcc "polyequal",
				    [toTyc DI.top BT.intinfTy])
		      in
			  intInfEqRef := SOME e; e
		      end

      fun getPolyEq () =
        (repPolyEq();
	 case (!polyEqRef)
          of SOME e => e
           | NONE => (let val e = coreAcc "polyequal"
                       in polyEqRef := (SOME e); e
                      end))
   in {getStrEq=getStrEq, getIntInfEq=getIntInfEq, getPolyEq=getPolyEq}
  end

val eqGen = PEqual.equal (eqDict, env)

val boolsign = BT.boolsign
val (trueDcon', falseDcon') =
  let val lt = LT.ltc_parrow(LT.ltc_unit, LT.ltc_bool)
      fun h (T.DATACON{name,rep,typ,...}) = (name, rep, lt)
   in (h BT.trueDcon, h BT.falseDcon)
  end

val trueLexp = CON(trueDcon', [], unitLexp)
val falseLexp = CON(falseDcon', [], unitLexp)

fun COND(a,b,c) =
  SWITCH(a,boolsign, [(DATAcon(trueDcon', [], mkv()),b),
                      (DATAcon(falseDcon', [], mkv()),c)], NONE)

fun composeNOT (eq, t) =
  let val v = mkv()
      val argt = LT.ltc_tuple [t, t]
   in FN(v, argt, COND(APP(eq, VAR v), falseLexp, trueLexp))
  end

val lt_unit = LT.ltc_unit
val lt_u_u = LT.ltc_parrow (lt_unit, lt_unit)

(* translation of prim ops *)
val transPrim =
    TransPrim.trans {coreAcc = coreAcc, coreExn = coreExn, mkv = mkv,
		     mkRaise = mkRaise}

(* genintinfswitch : var * (con * exp) list * lexp -> lexp *)
(* generates PLambda.lexp code for a case over an IntInf.int value. *)
(* where does this belong?  At what level should it be coded?  To Absyn? *)
(* This was moved from the old trans/matchcomp.sml.
 * con is an IntCON of type IntInf.int (: Types.ty IntConst.t).
 * translate.sml has to recognize this special form of a shallow Case
 * and invoke this function to handle it. *)
fun genintinfswitch (sv: LambdaVar.lvar, cases, default) =
    let (* build a chain of equality tests for checking large pattern values *)
	fun build [] = default
	  | build ((n, e) :: r) =
	      COND (APP (#getIntInfEq eqDict (), RECORD [VAR sv, VAR (getII n)]),
		    e, build r)
	(* make a small int constant pattern *)
	fun mkSmall n = INTcon{ival = IntInf.fromInt n, ty = Tgt.defaultIntSz}
	(* split pattern values into small values and large values;
	 * small values can be handled directly using SWITCH *)
	fun split ([], s, l) = (rev s, rev l)
	  | split ((n, e) :: r, sm, lg) =
	      (case LN.lowVal n
		 of SOME l => split (r, (mkSmall l, e) :: sm, lg)
		  | NONE => split (r, sm, (n, e) :: lg)
	      (* end case *))
	fun gen () =
	      (case split (cases, [], [])
		 of ([], largeints) => build largeints
		  | (smallints, largeints) =>
		      let val iv = mkv ()
		       in LET (iv, APP (coreAcc "infLowValue", VAR sv),
			       SWITCH (VAR iv, DA.CNIL, smallints, SOME (build largeints)))
		      end
	      (* end case *))
       in gen ()
      end
(* similar special cases for REF and SUSP constructors in pattern.  See genswitch in
 * FLINT/trans/matchcomp.sml. *)


(***************************************************************************
 *                                                                         *
 * Translating various bindings into lambda expressions:                   *
 *                                                                         *
 *   val mkVar : V.var * DI.depth -> L.lexp                                *
 *   val mkVE : V.var * T.ty list * DI.depth -> L.lexp                     *
 *   val mkCE : T.datacon * T.ty list * L.lexp option * DI.depth -> L.lexp *
 *   val mkStr : M.Structure * DI.depth -> L.lexp                          *
 *   val mkFct : M.Functor * DI.depth -> L.lexp                            *
 *   val mkBnd : DI.depth -> B.binding -> L.lexp                           *
 *                                                                         *
 ***************************************************************************)
(* [KM???] mkVar is calling transAccess, which just drops the prim!!! *)
fun mkVar (V.VALvar{access, typ, path, ...}, d) =
      transAccess(access, toLty d (!typ), getNameOp path)
  | mkVar _ = bug "unexpected vars in mkVar"

(* mkVE : V.var * T.ty list * depth -> lexp
 * This translates a variable, which may be bound to a primop.
 * In the case of a primop variable, this function reconstructs the
 * type parameters of instantiation of the intrinsic primop type relative
 * to the variable occurrence type *)
fun mkVE (e as V.VALvar { typ, prim = PrimopId.Prim p, ... }, tys, d) =
      let val occurenceTy = TU.applyPoly(!typ, tys)
              (* compute the occurrence type of the variable *)
          val primop = PrimopBind.defnOf p
          val intrinsicType = PrimopBind.typeOf p
	  val _ = debugmsg ">>mkVE: before matchInstTypes"
	  val intrinsicParams =
              (* compute intrinsic instantiation params of intrinsicType *)
              case (TU.matchInstTypes(true, d, occurenceTy, intrinsicType)
                      : (T.tyvar list * T.tyvar list) option )
                of SOME(_, tvs) =>
		   (if !debugging then
                      complain EM.WARN
                        "mkVE ->matchInstTypes -> pruneTyvar"
                        (fn ppstrm =>
                          (PP.string ppstrm
                            ("tvs length: " ^ Int.toString (length tvs));
                           PP.newline ppstrm;
                           PPVal.ppDebugVar
                            (fn x => "") ppstrm env e;
                           if (length tvs) = 1
                           then PPType.ppType env ppstrm (T.VARty (hd tvs))
                           else ()))
                    else ();
                    map TU.pruneTyvar tvs)
                 | NONE =>
                   (ElabDebug.withInternals (fn () => (complain EM.COMPLAIN
                      "mkVE:primop intrinsic type doesn't match occurrence type"
                      (fn ppstrm =>
                          (PP.string ppstrm "VALvar: ";
                           PPVal.ppVar ppstrm e;
                           PP.newline ppstrm;
                           PP.string ppstrm "occtypes: ";
                           PPType.ppType env ppstrm occurenceTy;
                           PP.newline ppstrm;
                           PP.string ppstrm "intrinsicType: ";
                           PPType.ppType env ppstrm intrinsicType;
                           PP.newline ppstrm;
                           PP.string ppstrm "instpoly occ: ";
                           PPType.ppType env ppstrm
                             (#1 (TU.instantiatePoly occurenceTy));
                           PP.newline ppstrm;
                           PP.string ppstrm "instpoly intrinsicType: ";
                           PPType.ppType env ppstrm
                             (#1 (TU.instantiatePoly intrinsicType))));
                    bug "mkVE -- NONE")))
	  val _ = debugmsg "<<mkVE: after matchInstTypes"
       in case (primop, intrinsicParams)
            of (PO.POLYEQL, [t]) => eqGen(intrinsicType, t, toTcLt d)
             | (PO.POLYNEQ, [t]) =>
               composeNOT(eqGen(intrinsicType, t, toTcLt d), toLty d t)
             | (PO.RAW_CCALL NONE, [a, b, c]) =>
               let val i = SOME (CProto.decode cproto_conv
                                   { fun_ty = a, encoding = b })
                           handle CProto.BadEncoding => NONE
               in PRIM (PO.RAW_CCALL i, toLty d intrinsicType,
                        map (toTyc d) intrinsicParams)
               end
             | _ => (** where do these intrinsicType originate?
			A: PrimopBindings *)
		    transPrim(primop, (toLty d intrinsicType),
                              map (toTyc d) intrinsicParams)
      end
  | mkVE (var as V.VALvar{typ, prim = PrimopId.NonPrim, path, ...}, tys, d) =
    (* non primop variable *)
      (if !debugging
       then (print "### mkVE nonprimop\n";
             print (SymPath.toString path); print "\n";
             ppType "mkVE1: " (!typ);
             print "|tys| = "; print (Int.toString(length tys)); print "\n";
             app (ppType "mkVE2: ") tys; print "\n")
       else ();
       case tys
        of [] => (if !debugging then print "mkVE3\n" else (); mkVar (var, d))
          | _ => TAPP(mkVar(var, d), map (toTyc d) tys))
                 (* dbm: when does this second case occur? *)
  | mkVE _ = bug "non VALvar passed to mkVE"

(* mkCE : T.datacon * T.tyvar list * lexp option * DB.depth -> lexp *)
(* Translation of constructor constant and contructor application expressions.
 * This should deal with rep translation for Match and Bind exceptions,
 * among others(?). *)
fun mkCE (T.DATACON{const, rep, name, typ, ...}, tyvars, argOp, d) =
  let val lty = toDconLty d typ
      val localRep = mkRep(rep, lty, name)
      val dataconstr = (name, localRep, lty)
      val tycs = map (toTyc d o T.VARty) tyvars
   in if const then CON' (dataconstr, tycs, unitLexp)
      else (case argOp
             of SOME le => CON'(dataconstr, tycs, le)
              | NONE =>
                 let val (argLty, _) = LT.ltd_parrow(LT.lt_pinst(lty, tycs))
                     val paramLvar = mkv()
                  in FN(paramLvar, argLty, CON'(dataconstr, tycs, VAR paramLvar))
                 end)
  end

fun mkStr (s as M.STR { access, prim, ... }, d) =
      transAccess(access, strLty(s, d, compInfo), NONE)
  | mkStr _ = bug "unexpected structures in mkStr"

fun mkFct (f as M.FCT { access, prim, ... }, d) =
      transAccess(access, fctLty(f, d, compInfo), NONE)
  | mkFct _ = bug "unexpected functors in mkFct"

fun mkBnd d =
  let fun transBind (B.VALbind v) = mkVar(v, d)
        | transBind (B.STRbind s) = mkStr(s, d)
        | transBind (B.FCTbind f) = mkFct(f, d)
        | transBind (B.CONbind (T.DATACON {rep=(DA.EXN access), name, typ, ...})) =
          let val nt = toDconLty d typ
              val (argt,_) = LT.ltd_parrow nt
          in VAR (transAccessTyped (access, LT.ltc_etag argt, SOME name))
          end
        | transBind _ = bug "unexpected bindings in transBind"
   in transBind
  end


(******************************************************************************
 *                                                                            *
 * Translating core absyn declarations into lambda expressions:               *
 *                                                                            *
 *    val transVBs  : Absyn.vb list * depth -> PLambda.lexp -> PLambda.lexp   *
 *    val transRVBs : Absyn.rvb list * depth -> PLambda.lexp -> PLambda.lexp  *
 *    val mkEBs  : Absyn.eb list * depth -> PLambda.lexp -> PLambda.lexp      *
 *                                                                            *
 * transVBs(vbs,d) produces a function taking a "body" or "scope" lexp.       *
 * Top-level variable binding have special handling specified at the end      *
 * of the main translate function, transDec.                                  *
 ******************************************************************************)

(* setBoundTyvars : depth * Types.tyvar list * (unit -> 'a) -> 'a *)
(* Temporarily set the tvkind of polymorphically bound tyvars to LBOUNDs with
 * the "current" TFN depth while performing a translation. *)
fun setBoundTyvars (TFNdepth, boundtvs, transfn) =
    let val savedtvkinds = map ! boundtvs
        (* save original contents of boundtvs (tvkinds)for restoration
         * after the invocation of transfn *)
	fun setbtvs (i, []) = ()
	  | setbtvs (i, tv::rest) =
	    (tv := T.LBOUND {depth=TFNdepth, eq=TU.tyvarIsEq tv, index=i};
	     setbtvs (i+1, rest))
	val _ = setbtvs(0, boundtvs)
        (* assign LBOUNDs to the boundtvs to mark them as type
         * parameter variables _locally_ during translation of exp *)		    
	val result = transfn ()
	(* perform and encapsulated translation with the new boundtvs context*)
	val _ = ListPair.app (op :=) (boundtvs, savedtvkinds)
        (* restore tyvar contents to state before the translation *)
    in result
    end

(* transPolyExp : Absyn.exp * depth * Types.tyvar list -> PLambda.lexp
 * Translate an expression with (potential) type parameters (boundtvs).
 * The boundtvs are temporarily set to LBOUNDs with appropriate depth and index
 * during a call of mkExp, then restored to their previous values. This is done
 * in case this call of transPolyExp occurs dynamically within an "outer" call
 * of transPolyExp that sets its own boundtvs which may overlap with
 * those of this call.
 * -- exp : Absyn.exp, the expression to be translated with tmv abstracted
 *      The expression is assumed to be the rhs of a simple var binding (val v = exp)
 * -- depth : int, the current depth of TFN abstractions (0-based)
 * -- boundtvs : tyvar list (3rd arg), the generalized tmvs of the expression
 * QUESTION: Can this happen?  If so, what is an example? 
 * CONJECTURE: a metatyvar can only be generalized at one declaration level, though
 * it may be generalized in parallel multiple times in a single declaration (e.g. RVB).
 * So scopes of a single metatyvar will not be "nested". If so, is the "restoration" of
 * the tvkinds of "bound tmvs" not necessary? Needs to be justified. *)
fun transPolyExp (exp, TFNdepth, []) = mkExp(exp, TFNdepth)
    (* exp is not polymorphic, hence no type abstraction around exp and TFNdepth
     * is not incremented *)
  | transPolyExp (exp, TFNdepth, boundtvs) =
    (* The LBOUND equality property probably does not matter at this point
     * because typechecking and signature matching are already completed [GK 2/24/08]. *)
      let val TFNdepth' = TFNdepth + 1  (* TFNdepth incremented for translation of exp *)
	  val bodyLexp = setBoundTyvars (TFNdepth, boundtvs, (fn () => mkExp (exp, TFNdepth')))
       in TFN(LT.tkc_arg(length(boundtvs)), bodyLexp)
      end

(* transVBs : Absyn.vb list * depth -> PLambda.lexp -> PLambda.lexp *)
(* implicit _body_: lexp parameter, representing the scope of the vb declarations.
 * Compound patterns will have been eliminated by match compilation in transMatch,
 * so the only cases here should be simple VBs with VARpat or WILDpat patterns. *)
and transVBs (vbs, d) =
  let fun transVB (VB{pat, exp, boundtvs,...}, body) =  (* tyvars field of VB not needed *)
	  let val pat = AU.stripPatMarks pat  (* strip out MARKpat's *)
	   in (case pat
		of (VARpat bvar | CONSTRAINTpat(VARpat bvar, _)) =>
                  (* Simple variable pattern. No special case needed for primops [dbm: 5/1/07] *)
		  (* ASSERT: VB.boundtvs = !bvar.btvs for the unique pattern variable bvar,
	           * So it should not matter whether we abstract over the boundtvs of the VB or
                   * the !btvs of the unique bvar (!btvs) *)
		   let val (bvarLvar, bvarBtvs) =  (* extract lvar (access) and btvs from bvar *)
			   (case bvar
			      of V.VALvar{access=DA.LVAR lvar, btvs, ...} => (lvar, !btvs)
			       | _ =>  bug "mkVB 1")
		    in (* checkBoundTvsEqual (boundtvs, bvarBtvs); *)(* check boundtvs = bvarBtvs (as tyvar sets) *)
		       case exp
		         of RSELECTexp(tupleVar, i) =>
			   (* This indicates the "special-purpose" variable (re)bindings generated
                            * by MatchComp.transMatchDec when translating compound binding pattern to a match. *)
			   (case tupleVar
			     of V.VALvar {access=DA.LVAR tupleLvar, btvs,...} =>
				let val tupleVarBtvs = !btvs  (* is tupleVarBtvs = boundtvs ? We copuld check this. *)
		                    (* These are the joint bound tyvars of the original compound
				     * pattern, which may be a proper superset of bvarBtvs.
				     * ASSERT: bvarBtvs subset tupleVarBtvs (checked below by checkBoundTvsSubset)
                                     * In fact: tupleVarBtvs = listUnion of all the bvarBtvs's. *)
				    val _ = if !debugging
					    then print (concat["transVB: tupleVar.name = ", S.name(V.varName tupleVar),
							       ", tupleLvar = " ^ LV.lvarName tupleLvar ^ "\n"])
					    else ()
				    val _ = checkBoundTvsSubset (bvarBtvs, tupleVarBtvs)
				       (* DBM: this check should pass if this VB is part of the match compilation of a
                                        * general VB, where the MC transform is performed post type checking. *)
				    val defnLexp =
					(case (tupleVarBtvs, bvarBtvs)
					  of (nil, _) => mkExp (exp, d)  (* (=> null bvarBtvs) no polymorphism *)
					   | (_, nil) =>  (* tupleVarBtvs non-null, bvarBtvs null *)
					     let val argTvs = List.tabulate (length tupleVarBtvs, (fn _ => LT.tcc_void))
					      in SELECT (i, TAPP (VAR tupleLvar, argTvs))
					     end
					   | _ => (* both tupleVarBtvs and bvarBtvs non null *)
					     let val bvarArity = length bvarBtvs
						 val argTvs = mkTyargs (tupleVarBtvs, bvarBtvs,
									LT.tcc_void, (fn (k,_) => LT.tcc_var(1,k)))
						 val _ = if !debugging
							 then (print (concat
							         ["transVB: bvar = ", S.name(V.varName bvar),
								  "; bvarLvar = ", LV.lvarName bvarLvar,
								  "\n |bvarBtvs| = ", Int.toString(length(bvarBtvs)),
								  ", |tupleVarBtvs| =", Int.toString(length(tupleVarBtvs)),
								  "\n argTvs = "]);
							       ppTycArgs argTvs)
							 else ()
					      in TFN(LT.tkc_arg (length bvarBtvs),  (* tuple of M (mono) kinds *)
						     SELECT(i, TAPP(VAR tupleLvar, argTvs)))
					     end)
				 in LET(bvarLvar, defnLexp, body)
				end
			      | _ => bug "transVB - unexpected tupleVar")
			 | _ =>  (* ordinary case -- use boundtvs from VB rather than bvarBtvs with transPolyExp.
                                  * boundtvs and bvarBtvs should be equal *)
			   LET(bvarLvar, transPolyExp(exp, d, boundtvs), body)
		   end
		 | (WILDpat | CONSTRAINTpat(WILDpat, _)) => (* wildcard pattern *)
		     LET(mkv(), transPolyExp (exp, d, boundtvs), body)
		     (* transPolyExp vs mkExp should not matter since the fresh let-bound lvar
                      * not referenced. But can boundtvs be non null for WILDpat? If always nil,
		      * then mkPolyExp is equivalent to mkExp. *)
		 | _ => (ppPat pat; bug "transVB -- unexpected compound pat"))
		     (* can't happen -- after match compilation, all VBs bind a simple variable, or wildcard *)
	  end (* fun transVB *)

   in fold transVB vbs
      (* missing fold(r) argument is the _body_; the return type is lexp -> lexp *)
  end (* transVBs *)

(* transRVBs : rvb list * int -> lexp -> lexp *)
(* mkRVB is rewritten to achieve the effect of the former ElabUtil.wrapRECdec, but post type
   checking. 

       rvbs ==>

       local 
         val fns = let rvbs in (f0, f1, ...)  
       in
         val f0' = fns.0      -- TFN((_), SELECT(0, TAPP(fns, f0_tyargs))), 
	 val f1' = fns.1
	 ...
       end

   1. extract btvs (btvsLists) from the defined variables (vars).
   2. Form their "union" to get list of bound tyvars (jointBtvs) for the main variable (fns).
      There may be overlaps between the btvs lists for the defined functions (f0, f1, ...), so
      this is not just the concatenation. Does order matter?
   3. translate the declaration of fns, abstracted wrt jointBtvs using transPolyExp, but the
      type abstraction is for the entire defn of fns. This produces an lexp of the
      form TFN(_, FIX rvb* IN fn-var-tuple).
   3. Construct type argument lists (TV & VOID) for the component functions f0', ,,, using tyargs
   4  Construct lexp for the wrapped rvb declaration.

  Special Case: single function val recs (recursive or not).  E.g.

   fun f x = x

   val rec f = fn x => x

  Translates to?

   val fr = let val rec f = fn x => x in f    (fr : All 'a . 'a -> 'a)
   val f' = fr
*)
and transRVBs (nil, _) = bug "transRVBs - no rbv"
  | transRVBs ([RVB{var as V.VALvar{access=DA.LVAR defLvar, typ, btvs,...}, exp,...}], d) =
    (* single function defn, binding defLvar *)
    let val ([newDefLvar], occurs) = aconvertLvars ([var], [exp])
     in if occurs
	then (* recursive case -- produce FIX *)
	    let val fixLvar = mkv ()  (* fresh lvar to be bound to the FIX lexp -- not used!!! *)
		val boundTvs = !btvs
		val numBoundTvs = length boundTvs
		val poly = numBoundTvs > 0
		fun mkTyArgs (nil, i, tyargs) = rev tyargs
		  | mkTyArgs (tv::tvs, i, tyargs) = 
		    mkTyArgs (tvs, i+1, T.VARty(ref(T.LBOUND{depth = d, eq = TU.tyvarIsEq tv, index = i}))::tyargs)
		val polyTyArgs = mkTyArgs (boundTvs, 0, nil)    (* null boundTvs => null polyTyArgs *)
		val monoType = TU.applyPoly (!typ, polyTyArgs)  (* de-polymorphize !typ, instantiating with LBOUNDS *)
		val lty = toLty (if poly then d+1 else d) monoType
		    (* was d+1; changed to d to fix utils/t1.sml bug. WRONG PLACE! *)
		val lexp =  (* translate exp without type abstraction, but with LBOUND instantiated boundTvs *)
		    if poly
		    then setBoundTyvars(d, boundTvs, (fn () => mkExp (exp, d+1)))
		    else mkExp (exp, d)
		val fixLexp = FIX([newDefLvar], [lty], [lexp], VAR newDefLvar)
		val rvbLexp =
		    if poly
		    then TFN(LT.tkc_arg numBoundTvs, fixLexp)
		    else fixLexp
(*
		val argLtycs = List.tabulate (numBoundTvs, (fn k => LT.tcc_var(1,k)))
		val defn = VAR fixLvar
		    case boundTvs
		      of nil => VAR fixLvar
		       | _ => TFN (LT.tkc_arg numBoundTvs, TAPP(VAR fixLvar, argLtycs))
*)
	     in (fn body => LET (defLvar, rvbLexp, body))
	    end
        else (* non-recursive case -- translate as though it were a VALdec *)
	    (fn body => LET(defLvar, transPolyExp(exp, d, !btvs), body))
    end
  | transRVBs (rvbs, d) = (* general case with multiple recursive function definitions *)
    let fun collect (RVB{var as V.VALvar{access=DA.LVAR lvar, typ, btvs, ...}, exp, ...}::rest,
		     vars, lvars, typs, btvss, exps) =
	      collect (rest, var::vars, lvar::lvars, !typ::typs, !btvs::btvss, exp::exps)
	  | collect (nil, vars, lvars, typs, btvss, exps) =
	      (rev vars, rev lvars, rev typs, rev btvss, rev exps)
	  | collect _ = bug "transRVB:collect -- bad RVB"
        val (vars, lvars, typs, btvss, exps) = collect (rvbs, nil, nil, nil, nil, nil)
	val (newLvars, _) = aconvertLvars (vars, exps)  (* not checking rhs occurrences of vars *)
        val boundTvs = foldr listUnion [] btvss         (* ordered "union" of btvs lists (duplicates merged) *)
	val numBoundTvs = length boundTvs
	val poly = numBoundTvs > 0
	val voidTycArgs = List.tabulate (numBoundTvs, (fn _ => LT.tcc_void))
	val fnsLvar = mkv ()  (* fresh lvar to be bound to the tuple of val rec defined functions *)
	val fntupleLexp = RECORD (map VAR newLvars)

        (* argTys : T.tyvar list -> T.type list *)
	fun argTys btvs =
	    mkTyargs (btvs, boundTvs, T.UNDEFty,
		      (fn (k,tv) => T.VARty(ref(T.LBOUND{depth=d, index=k, eq=TU.tyvarIsEq tv}))))
				  
        (* argLtcs : T.tyvar list -> LT.tyc list *)
        fun argLtcs btvs =
	    mkTyargs (boundTvs, btvs, LT.tcc_void, (fn (k,_) => LT.tcc_var(1,k)))

	val rvbLexp =
	    let val lexps =
		    if poly
		    then setBoundTyvars (d, boundTvs, (fn () => map (fn e => mkExp (e, d+1)) exps))
		    else map (fn e => mkExp (e, d)) exps
		      (* no polymorphism within FIX, so there should be no TFNs introduced *)
		val ltys = ListPair.map
			     (fn (ty,btvs) =>
				 toLty (if poly then d+1 else d)
				       (TU.applyPoly (ty, argTys btvs)))
			     (typs, btvss)
	        val fixLexp = FIX(newLvars, ltys, lexps, fntupleLexp)
	     in if poly
		then TFN (LT.tkc_arg numBoundTvs, fixLexp) 
		else fixLexp
	    end

	fun buildDec ([], _, _, body) = body
	  | buildDec (lvar::lvars, btvs::btvss, i, body) =
	      let val defn = (case (boundTvs, btvs)
			       of (nil,_) => SELECT (i, VAR fnsLvar)  (* "fnsLvar" not polymorphic *)
			        | (_,nil) => SELECT (i, TAPP(VAR fnsLvar, voidTycArgs))  (* lvar not polymorphic *)
				| _ => TFN (LT.tkc_arg (length btvs), (* fnsLvar and lvar both polymorphic *)
					    SELECT (i, TAPP(VAR fnsLvar, argLtcs btvs))))
	       in buildDec (lvars, btvss, i+1, LET (lvar, defn, body))
	      end
     in (fn body => LET (fnsLvar, rvbLexp, buildDec (lvars, btvss, 0, body)))
    end

and mkEBs (ebs, d) =
  let fun g (EBgen {exn=T.DATACON{rep=DA.EXN(DA.LVAR v), typ, ...},
                    ident, ...}, b) =
              let val nt = toDconLty d typ
                  val (argt, _) = LT.ltd_parrow nt
               in LET(v, ETAG(mkExp(ident, d), argt), b)
              end
        | g (EBdef {exn=T.DATACON{rep=DA.EXN(DA.LVAR v), typ, name, ...},
                    edef=T.DATACON{rep=DA.EXN(acc), ...}}, b) =
              let val nt = toDconLty d typ
                  val (argt, _) = LT.ltd_parrow nt
               in LET(v, VAR (transAccessTyped(acc, LT.ltc_etag argt, SOME name)), b)
              end
        | g _ = bug "unexpected exn bindings in mkEBs"

   in fold g ebs
  end


(***************************************************************************
 *                                                                         *
 * Translating module exprs and decls into lambda expressions:             *
 *                                                                         *
 *    val mkStrexp : Absyn.strexp * depth -> PLambda.lexp                   *
 *    val mkFctexp : Absyn.fctexp * depth -> PLambda.lexp                   *
 *    val mkStrbs  : Absyn.strb list * depth -> PLambda.lexp -> PLambda.lexp *
 *    val mkFctbs  : Absyn.fctb list * depth -> PLambda.lexp -> PLambda.lexp *
 *                                                                         *
 ***************************************************************************)
and mkStrexp (se, d) =
  let fun transStrexp (VARstr s) = mkStr(s, d)
        | transStrexp (STRstr bs) = SRECORD (map (mkBnd d) bs)
        | transStrexp (APPstr {oper, arg, argtycs}) =
              let val e1 = mkFct(oper, d)
                  val tycs = map (tpsTyc d) argtycs
                  val e2 = mkStr(arg, d)
               in APP(TAPP(e1, tycs), e2)
              end
        | transStrexp (LETstr (dec, b)) = mkDec (dec, d) (transStrexp b)
        | transStrexp (MARKstr (b, reg)) = withRegion reg transStrexp b
   in transStrexp se
  end

and mkFctexp (fe, d) =
  let fun transFctexp (VARfct f) = mkFct(f, d)
        | transFctexp (FCTfct {param as M.STR { access, ... }, argtycs, def }) =
	  (case access of
	       DA.LVAR v =>
               let val knds = map tpsKnd argtycs
                   val nd = DI.next d  (* reflecting type abstraction *)
                   val body = mkStrexp (def, nd)
                   val hdr = buildHeader v
               (* binding of all v's components *)
               in
		   TFN(knds, FN(v, strLty(param, nd, compInfo), hdr body))
               end
	     | _ => bug "mkFctexp: unexpected access")
        | transFctexp (LETfct (dec, b)) = mkDec (dec, d) (transFctexp b)
        | transFctexp (MARKfct (b, reg)) = withRegion reg transFctexp b
        | transFctexp _ = bug "unexpected functor expressions in mkFctexp"
   in transFctexp fe
  end

(* new mkStrbs
and mkStrbs (sbs, d) =
  let fun transSTRB (STRB{name, str=M.STR { access, sign, rlzn, prim }, def}, b) =
	  (case access
	     of DA.LVAR v =>  (* binding of all v's components *)
                  let val hdr = buildHeader v
                   in LET(v, mkStrexp(def, d), hdr b)
                  end
	      | _ => bug "mkStrbs: unexpected access")
        | transSTRB _ = bug "unexpected structure bindings in mkStrbs *"
  in fold transSTRB sbs
  end
*)

and mkStrbs (sbs, d) =
  let fun transSTRB (STRB {str, def, ... }, b) =
	  (case str
	    of M.STR {access, ...} =>
	       (case access
		 of DA.LVAR v =>  (* binding of all v's components *)
                    let val hdr = buildHeader v
                    in LET(v, mkStrexp(def, d), hdr b)
                    end
		  | _ => bug "mkStrbs: unexpected access")
	       | M.STRSIG _ => bug "mkStrbs: str=STRSIG"
	       | M.ERRORstr => bug "mkStrbs: str=ERRORstr")
  in fold transSTRB sbs
  end

(* old version saved --
and mkStrbs (sbs, d) =
  let fun transSTRB (STRB{str=M.STR { access, ... }, def, ... }, b) =
	  (case access
	     of DA.LVAR v =>  (* binding of all v's components *)
                  let val hdr = buildHeader v
                   in LET(v, mkStrexp(def, d), hdr b)
                  end
	      | _ => bug "mkStrbs: unexpected access")
        | transSTRB (STRB{str=M.STRSIG _, ...}, b) =
	    bug "mkStrbs: str=STRSIG"
        | transSTRB (STRB{str=M.ERRORstr, ...}, b) =
	    bug "mkStrbs: str=ERRORstr"
(*        | transSTRB _ = bug "unexpected structure bindings in mkStrbs" *)
  in fold transSTRB sbs
  end
*)
      
and mkFctbs (fbs, d) =
  let fun transFCTB (FCTB{fct=M.FCT { access, ... }, def, ... }, b) =
	  (case access
	     of DA.LVAR v =>
		let val hdr = buildHeader v
                 in LET(v, mkFctexp(def, d), hdr b)
		end
	      | _ => bug "mkFctbs: unexpected access")
        | transFCTB _ = bug "unexpected functor bindings in mkFctbs"
  in fold transFCTB fbs
  end


(***************************************************************************
 * Translating absyn decls and exprs into lambda expression:               *
 *                                                                         *
 *    val mkDec : A.dec * DI.depth -> PLambda.lexp -> PLambda.lexp         *
 *    val mkExp : A.exp * DI.depth -> PLambda.lexp                         *
 *                                                                         *
 ***************************************************************************)
and mkDec (dec, d) =
(* "let dec in body": mkDec produces a function to be applied to the translation of body,
 * to translate the entire let expression *)
  let (* mkDec0 : AS.dec -> (lexp -> lexp) *)
      fun mkDec0 (VALdec vbs) = transVBs(vbs, d)
        | mkDec0 (VALRECdec rvbs) = transRVBs(rvbs, d)
	| mkDec0 (DOdec exp) = (fn body => LET(mkv(), mkExp(exp, d), body))
        | mkDec0 (ABSTYPEdec{body,...}) =
	  ((* print "mkDec:ABSTYPEdec:body = ";
	   ppDec body; *)
	   mkDec0 body)
        | mkDec0 (EXCEPTIONdec ebs) = mkEBs(ebs, d)
        | mkDec0 (STRdec sbs) = mkStrbs(sbs, d)
        | mkDec0 (FCTdec fbs) = mkFctbs(fbs, d)
        | mkDec0 (LOCALdec(ld, vd)) = (mkDec0 ld) o (mkDec0 vd)
        | mkDec0 (SEQdec ds) =  foldr (op o) ident (map mkDec0 ds)
        | mkDec0 (MARKdec(x, reg)) =
              let val f = withRegion reg mkDec0 x
               in fn y => withRegion reg f y
              end
        | mkDec0 (OPENdec xs) =
              let (* special hack to make the import tree simpler *)
                  fun mkos (_, s as M.STR { access, ... }) =
                      if extern access then
                          (transAccessTyped (access, strLty(s, d, compInfo), NONE);
                          ())
                      else ()
                    | mkos _ = ()
               in app mkos xs; ident
              end
        | mkDec0 _ = ident
   in mkDec0 dec
  end

and mkExp (exp, d) =
  let val tTyc = toTyc d
      val tLty = toLty d

      (* mkDcon : Types.datacon -> Plambda.dataconstr *)
      fun mkDcon (T.DATACON {name, rep, typ, ...}) =
	  let val lty = toDconLty d typ
	   in (name, mkRep (rep, lty, name), lty)
	  end

(* patToCon : AS.pat * -> Plambda.con
 * maps a "shallow" pattern to a Plambda.con *)
(* ### REWRITE AND MERGE THESE TWO COMMENTS -- mostly obsolete ### *)
(* How does the fresh lvar introduced (old translate.sml) in the CONpat and APPpat
 * cases get connected to the rhs expression of rules? Is a lambda-abstraction over
 * the new lvar wrapped around the corresponding rhs?  Or is a Let-binding
 * of the lvar to the argument of the constuctor (unit for constant constructors)
 * introduced somewhere (or implicit in the semantics of SWITCH)?  We have already
 * introduced svars that represent the argument to non-constant constructors. *)
    (* The pattern argument is a "shallow" pattern, i.e. a constant or a
     * datacon applied to a variable.
     * How does the fresh lvar introduced in the CONpat case get
     * It doesn't matter since there is nothing for it to bind to in
     * a destructed CONpat.
     * In the old match compiler, fresh lvars created for CONpat and APPpat
     * are mapped by an "environment" from a path identifying the pattern point.
     * Here we get the lvar from the var (VALvar) in the shallow pattern for
     * APPpat and VECLENpat. This variable is already refered to in the corresponding
     * rule rhs. *)
(* patToCon has been moved inside function mkExp to give it access to the tTyc type
 * translation function. *)

      (* patToCon : AS.pat * -> Plambda.con *)
      (* this function was brought inside mkExp to give it access to tTyc above *)
      fun patToCon pat =
	  (case pat
	     of CONpat (datacon, tvs) =>
		  let val dummyLvar = mkv()
			(* fresh lvar to be bound to nonexistent datacon "argument". This
			 * var bindings should be a separate var option component in SWITCH
			 * cases. *)
		      val nts = map (tTyc o T.VARty) tvs
		   in DATAcon (mkDcon datacon, nts, dummyLvar)
		  end
	      | APPpat (datacon, tvs, VARpat(V.VALvar{access=DA.LVAR lvar,...})) =>
		  let val nts = map (tTyc o T.VARty) tvs
		   in DATAcon (mkDcon datacon, nts, lvar)
		  end
	      | NUMpat (_, lit) => transNum lit
	      | STRINGpat s => STRINGcon s
	      | CHARpat c => bug "patToCon: CHARpat found (should be NUMpat)" 
	      | pat => (ppPat pat;
			bug "patToCon: unexpected pattern")
	    (* end case *))

      and transRule (RULE(pat, rhs)) = (patToCon pat, mkExp0 rhs)

      and mkExp0 (VARexp (ref v, ts)) =
            (debugmsg ">>mkExp VARexp";
	     mkVE(v, map T.VARty ts, d))
        | mkExp0 (CONexp (dc, ts)) =
	  (let val _ = debugmsg ">>mkExp CONexp: "
	       val c = mkCE(dc, ts, NONE, d)
	       val _ = if !debugging then ppLexp c else ()
	   in c end)
        | mkExp0 (APPexp (CONexp(dc, ts), e2)) =
	  (let val _ = debugmsg ">>mkExp APPexp: "
	       val c = mkCE(dc, ts, SOME(mkExp0 e2), d)
	       val _ = if !debugging then ppLexp c else ()
	   in c end)
        | mkExp0 (NUMexp(src, {ival, ty})) = (
	    debugmsg ">>mkExp NUMexp";
	    if TU.equalType (ty, BT.intTy) then INT{ival = ival, ty = Tgt.defaultIntSz}
	    else if TU.equalType (ty, BT.int32Ty) then INT{ival = ival, ty = 32}
	    else if TU.equalType (ty, BT.int64Ty) then INT{ival = ival, ty = 64}
	    else if TU.equalType (ty, BT.intinfTy) then VAR (getII ival)
	    else if TU.equalType (ty, BT.wordTy) then WORD{ival = ival, ty = Tgt.defaultIntSz}
	  (* NOTE: 8-bit word is promoted to default tagged word representation *)
	    else if TU.equalType (ty, BT.word8Ty) then WORD{ival = ival, ty = Tgt.defaultIntSz}
	    else if TU.equalType (ty, BT.word32Ty) then WORD{ival = ival, ty = 32}
	    else if TU.equalType (ty, BT.word64Ty) then WORD{ival = ival, ty = 64}
	    else (ppType "NUMexp: " ty; bug "translate NUMexp"))
(* REAL32: handle 32-bit reals *)
        | mkExp0 (REALexp(_, {rval, ty})) = REAL{rval = rval, ty = Tgt.defaultRealSz}
        | mkExp0 (STRINGexp s) = STRING s
(* QUESTION: do we want to map characters to word8? *)
(** NOTE: the following won't work for cross compiling to multi-byte characters **)
        | mkExp0 (CHARexp c) = INT{ival = IntInf.fromInt (Char.ord c),
				   ty = Tgt.defaultIntSz}
        | mkExp0 (RECORDexp []) = unitLexp
        | mkExp0 (RECORDexp xs) =
            if sorted xs then RECORD (map (fn (_,e) => mkExp0 e) xs)
            else let val vars = map (fn (l,e) => (l,(mkExp0 e, mkv()))) xs
                     fun bind ((_,(e,v)),x) = LET(v,e,x)
                     val bexp = map (fn (_,(_,v)) => VAR v) (sortrec vars)
                  in foldr bind (RECORD bexp) vars
                 end

        | mkExp0 (RSELECTexp (var,i)) =  (* record selection, no polymorphism *)
	    (case V.varAccess var
	      of DA.LVAR lvar => SELECT(i, VAR lvar)
	       | _ => bug "mkExp0:RSELECTexp")

        | mkExp0 (VECTORexp ([], ty)) =
             TAPP(coreAcc "vector0", [tTyc ty])
        | mkExp0 (VECTORexp (xs, ty)) =
             let val tc = tTyc ty
                 val vars = map (fn e => (mkExp0 e, mkv())) xs
                 fun bind ((e,v),x) = LET(v, e, x)
                 val bexp = map (fn (_,v) => VAR v) vars
              in foldr bind (VECTOR (bexp, tc)) vars
             end

        | mkExp0 (VSELECTexp(var,index)) =
	    let val DA.LVAR lvar = V.varAccess var
		val varTy = V.varType var  (* should be vector type *)
	        val elemTy = TU.vectorElemTy varTy
			     handle exn =>
			            (ppType "mkExp0[VSELECTexp]" varTy; raise exn)
	        val tc = tTyc elemTy
		val lt_sub =
                    let val vecTyc = LT.ltc_vector (LT.ltc_tv 0)
                    in LT.ltc_poly([LT.tkc_mono],
				   [LT.ltc_parrow(LT.ltc_tuple [vecTyc, LT.ltc_int],
						  LT.ltc_tv 0)])
                    end
		val indexLexp = INT{ival = IntInf.fromInt index, ty = Target.defaultIntSz}
	     in APP(PRIM(PO.SUBSCRIPTV, lt_sub, [tc]),
		    RECORD[ VAR lvar, indexLexp ])
            end

        | mkExp0 (SEQexp [e]) = mkExp0 e
        | mkExp0 (SEQexp (e::r)) = LET(mkv(), mkExp0 e, mkExp0 (SEQexp r))

        | mkExp0 (APPexp (e1, e2)) = APP(mkExp0 e1, mkExp0 e2)
        | mkExp0 (MARKexp (e, reg)) = withRegion reg mkExp0 e
        | mkExp0 (CONSTRAINTexp (e,_)) = mkExp0 e

        | mkExp0 (RAISEexp (e, ty)) = mkRaise(mkExp0 e, tLty ty)

        | mkExp0 (HANDLEexp (baseExp, (rules, lhsTy, rhsTy))) =
	    (* ty will always be UNDEFty, because (rules, ty) produced by
             * ElabUtil.makeHANDLEexp. *)
	    (case rules
	       of [RULE(VARpat exnvar, handlerExp)] =>
		    (case V.varAccess exnvar
		       of DA.LVAR exnlvar =>
			    HANDLE (mkExp0 baseExp, FN(exnlvar, tLty lhsTy, mkExp0 handlerExp))
			| _ => bug "mkExp0:HANDLEexp:exnvar:access")
		| _ => bug "mkExp0:HANDLEexp")

        | mkExp0 (FNexp (rules, argty, resty)) =
	     (* ty is the type of the patterns (lhs) of the rules, produced
	      * by the type checker for FNexp *)
             (case rules
	        of [RULE(pat, rhs)] =>  (* expect single rule, produced by match compilation *)
		     (case pat
		       of VARpat matchVar =>
			  (case V.varAccess matchVar
		            of DA.LVAR paramLvar =>
			         FN (paramLvar, tLty argty, mkExp0 rhs)
			     | _ => bug "mkExp0:FNexp:matchVar.access not LVAR")
			| _ => (ppPat pat; bug "mkExp0:FNexp:non-variable pattern"))
		 | r1::r2::_ => bug "mkExp0:FNexp:multiple rules"
		 | _ => bug "mkExp0:FNexp:WTF")

        (* For SWITCHexp, we translate to SWITCH lexp except for REF and SUSP
         * scrutinees.  NOTE: translate should never see an Absyn.CASEexp.
	 * All CASEexp's are produced by match compilation, never directly
	 * by the elaborator-typechecker (source Absyn CASEexp have been
         * converted to LETexp in typecheck).
         * The pat of each rule will be "shallow", meaning it is a constant
         * (INTpat, ..., CONpat) or an APPpat of the form
         * APPpat(dcon, tvs, VARpat v), where v is an "internal" (svar)
         * variable. The rhs corresponding to an APPpat will use the unique
         * variable in its pattern (if any) to refer to the constructor argument
	 * The special single (pseudo-) constructor pattern cases involving
         * the "ref" and "susp" constructors, and the special case of switching on
         * intinf constants are treated as speical cases. For all other cases,
         * immediately builds a SWITCH.
	 * The defaultOp arg will need to be SOME when the rule pats are not
         * complete (exhaustive) -- we don't add a default rule. *)
	(* ASSERT: length rules > 0, and length rules = 1 in the case of SINGLE
         * dcons, in which case defaultOp = NONE.
         * ASSERT: scrutinee is VALvar with LVAR access. *)
        | mkExp0 (SWITCHexp (scrutinee: V.var, rules, defaultOp)) =
	     (* non-degenerate case, multiple rules *)
             let val scrutineeLvar = V.varToLvar scrutinee
		 val RULE(pat1, _) :: _ = rules
		 val consig = patToConsig pat1
		 val trules as ((con1, lexp1) :: _) = map transRule (rev rules)
		 val defaultOp' = Option.map mkExp0 defaultOp
             in case con1
		 of DATAcon((_, DA.REF, lt), ts, lvar) =>
		      (* ref pseudo-constructor, single, hence unique rule *)
		      LET(lvar,
			  APP (PRIM (Primop.DEREF, LT.lt_swap lt, ts), VAR scrutineeLvar),
			  lexp1)
		  | DATAcon((_, DA.SUSP(SOME(_, DA.LVAR f)), lt), ts, lvar) =>
		      (* susp pseudo-constructor, single, hence unique rule *)
		      let val localLvar = mkv()
		      in LET(lvar,
			     LET(localLvar, TAPP(VAR f, ts), APP(VAR localLvar, VAR scrutineeLvar)),
			     lexp1)
		      end
		  | INTcon{ty=0, ...} => (* IntInf.int constant *)
		      let fun strip (INTcon{ty=0, ival}, lexp) = (ival, lexp)
			    | strip _ = bug "genswitch - INTINFcon"
		      in genintinfswitch (scrutineeLvar, map strip trules,
					  Option.valOf defaultOp')
		      end
		  | _ => SWITCH(VAR scrutineeLvar, consig, trules, defaultOp')
             end

	| mkExp0 (VSWITCHexp (scrutinee: V.var, rules, default)) = 
	    let val scrutineeLvar = V.varToLvar scrutinee
		val lengthLvar = mkv()  (* fresh lvar to be bound to the length of the vector *)
                val scrutTy = V.varType scrutinee
		(* val _ = (ppType "Translate.mkExp0[VSWITCH]: scrutTy = " scrutTy; newline()) *)
		val elemType = TU.vectorElemTy scrutTy
                               handle exn =>
				      (ppType "Translate.mkExp0[VSWITCHexp]" scrutTy; newline();
				       raise exn)
		val elemtyc = tTyc elemType
		val lt_len = LT.ltc_poly([LT.tkc_mono],
					 [LT.ltc_parrow(LT.ltc_tv 0, LT.ltc_int)])
		val vectortyc = LT.tcc_vector elemtyc
	     in LET(lengthLvar,
		    APP(PRIM(PO.LENGTH, lt_len, [vectortyc]),
			VAR scrutineeLvar),
		    SWITCH(VAR lengthLvar, DA.CNIL, map transRule rules, SOME(mkExp0 default)))
	    end

	| mkExp0 (IFexp { test, thenCase, elseCase }) =
	    COND (mkExp0 test, mkExp0 thenCase, mkExp0 elseCase)

	| mkExp0 (ANDALSOexp (e1, e2)) =
	    COND (mkExp0 e1, mkExp0 e2, falseLexp)

	| mkExp0 (ORELSEexp (e1, e2)) =
	    COND (mkExp0 e1, trueLexp, mkExp0 e2)

	| mkExp0 (WHILEexp { test, expr }) =
	    let val fv = mkv ()
		val body =
		    FN (mkv (), lt_unit,
			COND (mkExp0 test,
			      LET (mkv (), mkExp0 expr, APP (VAR fv, unitLexp)),
			      unitLexp))
	    in
		FIX ([fv], [lt_u_u], [body], APP (VAR fv, unitLexp))
	    end

        | mkExp0 (LETexp (dec, body)) = mkDec (dec, d) (mkExp0 body)

        | mkExp0 e =
            EM.impossibleWithBody "untranslateable expression:\n  "
              (fn ppstrm => (PPAbsyn.ppExp (env,NONE) ppstrm (e, !ppDepth)))

   in mkExp0 exp
  end

(* DBM: are tranIntInf and wrapII still relevant? Relation with genintinfswitch? *)
and transIntInf d s =
    (* This is a temporary solution.  Since IntInf literals
     * are created using a core function call, there is
     * no indication within the program that we are really
     * dealing with a constant value that -- in principle --
     * could be subject to such things as constant folding. *)
    let val consexp = CONexp (BT.consDcon, [ref (T.INSTANTIATED BT.wordTy)])
	fun build [] = CONexp (BT.nilDcon, [ref (T.INSTANTIATED BT.wordTy)])
	  | build (d :: ds) = let
	      val i = Word.toIntX d
	      in
		APPexp (consexp, EU.TUPLEexp [
		    NUMexp("<lit>", {ival = IntInf.fromInt i, ty = BT.wordTy}),
		    build ds
		  ])
	      end
	fun mkSmallFn s =
	      coreAcc(if LN.isNegative s then "makeSmallNegInf" else "makeSmallPosInf")
	fun mkFn s =
	      coreAcc(if LN.isNegative s then "makeNegInf" else "makePosInf")
	fun small w =
	      APP (mkSmallFn s,
		mkExp (
		  NUMexp("<lit>", {ival = IntInf.fromInt (Word.toIntX w), ty = BT.wordTy}),
		  d))
     in case LN.repDigits s
          of [] => small 0w0
	   | [w] => small w
	   | ws => APP (mkFn s, mkExp (build ws, d))
    end

(* Wrap bindings for IntInf.int literals around body. *)
fun wrapII body = let
    fun one (n, v, b) = LET (v, transIntInf DI.top n, b)
in
    IIMap.foldli one body (!iimap)
end

(* wrapPidInfo: lexp * (pid * pidInfo) list -> lexp * importTree *)
fun wrapPidInfo (body, pidinfos) =
  let val imports =
        let fun p2itree (ANON xl) =
                  ImportTree.ITNODE (map (fn (i,z) => (i, p2itree z)) xl)
              | p2itree (NAMED _) = ImportTree.ITNODE []
         in map (fn (p, pi) => (p, p2itree pi)) pidinfos
        end

(*
      val _ = let val _ = say "\n ****************** \n"
                  val _ = say "\n the current import tree is :\n"
                  fun tree (ImportTree.ITNODE []) = ["\n"]
                    | tree (ImportTree.ITNODE xl) =
                        foldr (fn ((i, x), z) =>
                          let val ts = tree x
                              val u = (Int.toString i)  ^ "   "
                           in (map (fn y => (u ^ y)) ts) @ z
                          end) [] xl
                  fun pp (p, n) =
                    (say ("Pid " ^ (PersStamps.toHex p) ^ "\n");
                     app say (tree n))
               in app pp imports; say "\n ****************** \n"
              end
*)
      val plexp =
        let fun get ((_, ANON xl), z) = foldl get z xl
              | get ((_, u as NAMED (_,t,_)), (n,cs,ts)) =
                  (n+1, (n,u)::cs, t::ts)

            (* get the fringe information *)
            val getp = fn ((_, pi), z) => get((0, pi), z)
            val (finfos, lts) =
              let val (_, fx, lx) = foldl getp (0,[],[]) pidinfos
               in (rev fx, rev lx)
              end

            (* do the selection of all import variables *)
            fun mksel (u, xl, be) =
              let fun g ((i, pi), be) =
                    let val (v, xs) = case pi of ANON z => (mkv(), z)
                                               | NAMED(v,_,z) => (v, z)
                     in LET(v, SELECT(i, u), mksel(VAR v, xs, be))
                    end
               in foldr g be xl
              end
            val impvar = mkv()
            val implty = LT.ltc_str lts
            val nbody = mksel (VAR impvar, finfos, body)
         in FN(impvar, implty, nbody)
        end
   in (plexp, imports)
  end (* function wrapPidInfo *)

(** the list of things being exported from the current compilation unit *)
val exportLexp = SRECORD (map VAR exportLvars)

val _ = debugmsg ">>mkDec"
(** translating the ML absyn into the PLambda expression *)
val body = mkDec (rootdec, DI.top) exportLexp
val _ = debugmsg "<<mkDec"
val _ = if CompInfo.anyErrors compInfo
	then raise EM.Error
	else ()
(** add bindings for intinf constants *)
val body = wrapII body

(** wrapping up the body with the imported variables *)
val (plexp, imports) = wrapPidInfo (body, PersMap.listItemsi (!persmap))

(** type check body (including kind check) **)
val ltyerrors = if !FLINT_Control.checkPLambda
		then ChkPlexp.checkLtyTop(plexp,0)
		else false
val _ = if ltyerrors
        then (print "**** Translate: checkLty failed ****\n";
              with_pp(fn str =>
                (PU.pps str "absyn:"; PP.newline str;
                 ElabDebug.withInternals
                  (fn () => PPAbsyn.ppDec (env,NONE) str (rootdec,1000));
		 PP.newline str;
                 PU.pps str "lexp:"; PP.newline str;
                 PPLexp.ppLexp 25 str plexp));
              complain EM.WARN "checkLty" EM.nullErrorBody;
	     bug "PLambda type check error!")
        else ()


val _ = if !Control.FLINT.print
	  then (say ("\n\n[After Translate" ^ " ...]\n\n"); ppLexp plexp)
	  else ()

(** normalizing the plambda expression into FLINT *)
val flint = let val _ = debugmsg ">>norm"
		val _ = if !debugging
			then complain EM.WARN ">>flintnm" EM.nullErrorBody
			else ()
		val n = FlintNM.norm plexp
		val _ = debugmsg "<<postnorm"
	    in n end

in {flint = flint, imports = imports}
end (* function transDec *)

end (* top-level local *)
end (* structure Translate *)

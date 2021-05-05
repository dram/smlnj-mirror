(* Elaborator/matchcomp/matchcomp.sml *)

(* This is the "unified" match compiler, where match compilation is embeded in an absyn to
 * absyn translation, which drives the process (andor tree, decision tree, "code" gen).
 * The "code" (Absyn.exp) for a match is generated from the decision tree and information
 * from the original andor (used for record/vector destruction and variable bindings (and types).
 * This code performs pattern dispatching and destruction and then invokes the appropriate
 * RHS expressions with the necessary variable bindings.
 *
 * The "code" is in the form of an Absyn.exp.  Absyn.exp has been augmented with two new
 * expression forms: SWITCHexp and VSWITCHexp, which are created only by the match
 * compilation. Match compilation is invoked (on absyn decs) type checking, so it is
 * responsible for "maintaining" the correct type information in the translated absyn.
 *)

structure MatchComp =
struct

local
  structure S = Symbol
  structure SP = SymPath
  structure T = Types
  structure BT = BasicTypes
  structure TU = TypesUtil
  structure A = Access
  structure V = VarCon
  structure AS = Absyn
  structure AU = AbsynUtil
  structure EU = ElabUtil
  structure R = Rules
  structure SV = SVar
  structure SE = SVarEnv
  structure K = Key
  structure MT = MCTypes
  structure MU = MCUtil
  structure L = Layers
  structure DT = DecisionTree
  structure C = VMCexp
  structure PP = PrettyPrint
  structure PU = PPUtil
  structure ED = ElabDebug
  open Absyn MCTypes

  (* printing for matchComp *)

  fun bug msg = ErrorMsg.impossible ("MatchComp: " ^ msg)

  val printAndor = MCControl.printAndor
  val printDecisionTree = MCControl.printDecisionTree
  val printMatchAbsyn = MCControl.printMatchAbsyn
  val debugging = MCControl.mcdebugging
  val stats = MCControl.mcstats

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun says strings = saynl (concat strings)

  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)

  fun ppAndor andor =
      PP.with_default_pp
	  (fn ppstrm =>
	      (PP.string ppstrm "andor:\n";
	       MCPrint.ppAndor ppstrm andor;
	       PP.newline ppstrm))

  fun ppDecisionTree dectree =
      PP.with_default_pp
	  (fn ppstrm =>
	      (PP.string ppstrm "dectree:\n";
	       MCPrint.ppDecTree ppstrm dectree;
	       PP.newline ppstrm))

  fun ppExp (exp, msg) =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm msg;
	       PPAbsyn.ppExp (StaticEnv.empty, NONE) ppstrm (exp, 20);
	       PP.newline ppstrm))

  fun ppDec (dec, msg) =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm msg;
	       PPAbsyn.ppDec (StaticEnv.empty, NONE) ppstrm (dec, 20);
	       PP.newline ppstrm))

  fun ppPat pat =
      PP.with_default_pp(fn ppstrm => PPAbsyn.ppPat StaticEnv.empty ppstrm (pat, 20))

  fun ppVar var =
      PP.with_default_pp(fn ppstrm => PPVal.ppVar ppstrm var)

  fun ppType msg ty =
      PP.with_default_pp
	(fn ppstrm => (PP.string ppstrm (msg^": "); PPType.ppType StaticEnv.empty ppstrm ty))

  fun timeIt x = TimeIt.timeIt (!stats) x

in

(* How should we treat SINGLE constructor patterns, and in particular the
 * "special" ones like *ref* (and *susp* )?  We generate a special single
 * datacon "deconstructor" (expressed as a single-variant SWITCHexp) in these cases.
 * The special cases (ref,susp) are detected and handled in Translate
 * (FLINT/trans/translate.sml).
 * SWITCHexp translates almost directly to Plambda.SWITCH.
 *
 * Also need to deconstruct AND and SINGLE _below_ a terminal OR/Decision node,
 * since variables may occur below the node. This is done by the call of
 * genAndor within the body of genDecTree.
 *)

fun k_ident (x: AS.exp) = x

val choiceTotalThreshold = 10

fun reportStats (nodeCount: int, {rulesUsed, failures, choiceTotal, choiceDist}: DT.decTreeStats) =
    if !stats andalso choiceTotal > choiceTotalThreshold
    then (say "decTree Stats: \n";
	  says ["  nodeCount =   ", Int.toString nodeCount];
	  says ["  choiceTotal = ", Int.toString choiceTotal];
	  newline())
    else ()


(* ================================================================================ *)
(* matchComp: AS.rule list * T.ty * T.datacon -> AS.exp * VarCon.var *)
(* lhsTy is the type of the patterns (i.e. the domain or lhs) of the match
 * Called by transMatch.  fillPat will have been applied to rule patterns. *)
(*
and matchComp (rules: AS.rule list, lhsTy: T.ty, rhsTy: T.ty, failExnOp: T.datacon option) =
    let val numRules = length rules
	val _ = says [">> matchComp[", Int.toString numRules,"]"]
	val patterns = map (fn (AS.RULE(pat,_)) => pat) rules (* strip RULE constructor *)
	val (andor, nodeCount) = (* AndOr.makeAndor(patterns, lhsTy) *)
              timeIt ("makeAndor", "no location", AndOr.makeAndor, (patterns, lhsTy))
	val _ = saynl "<< makeAndor"
	val _ = if !printAndor
		then ppAndor andor
		else ()
	val (dectree,ruleCounts) = (* DecisionTree.decisionTree (andor, numRules) *)
	      timeIt ("decisionTree", "no location", DT.decisionTree, (andor, numRules))
	val decTreeStats = DT.decTreeStats dectree
	val _ = saynl "<< decisionTree"
	val _ = if !printDecisionTree
		then ppDecisionTree dectree
		else ()
	val _ = reportStats (nodeCount, decTreeStats)
        val (matchExp, matchRootVar) =
	    (* genMatch (rules, andor, dectree, ruleCounts, rhsTy, varenvAC, matchFailExn) *)
	    timeIt ("genMatch", "no location",
		    genMatch, (rules, andor, dectree, ruleCounts, rhsTy, varenvAC, matchFailExn))
	val _ = saynl "<< genMatch"
	val _ = if !printMatchAbsyn
		then ED.withInternals (fn () => ppExp (matchExp, "matchComp:matchExp: \n"))
		else ()
    in saynl "<< matchComp";  (* -- debugging *)
       (matchExp, SVar.svarToVar matchRootVar)
    end
*)
(* matchComp : (pat * lexp) list * failInfo * toLcLt * errTy * giisTy
                -> lexp * lvar * ruleno list * bool * bool *)
fun matchComp (rules, lhsTy: T.ty, rhsTy: T.ty, failExnOp: T.datacon option) =
let fun timeIt x = TimeIt.timeIt (!stats) x
    val location = "nolocation"
    val rules' = map (fn AS.RULE x => x) rules  (* strip RULE constructor *)
    val _ = MCPrint.debugPrint
              ("matchComp: match = \n",
	        (fn ppstrm => MCPrint.ppHMatch ppstrm hybridMatch))

    val (numExpandedRules, expandedPats, rhsFunBinders, ruleMap) =
	Preprocessing.expandPats (rules', lhsTy, rhsTy)

    (* ruleset containing all rulenos after or-expansion. If there are or-patterns
     * in the match, numRulesExpanded > length rules. *)
    val allRules: RS.set = RS.fromList (List.tabulate(numExpandedRules, fn x => x));

    val protoAndor: protoAndor = (* ProtoAndor.makeProtoAndor expandedPats *)
        timeIt ("makeProtoAndor", location, ProtoAndor.makeProtoAndor, expandedPats)

    val _ = MCPrint.debugPrint
	      ("** matchComp: protoAndor = ",
	       (fn ppstrm => MCPrint.ppProtoAndor ppstrm protoAndor))

    val (andor: andor, pvarmap: Andor.pvarmap, nodeCount) =
	(* Andor.makeAndor (protoAndor, allRules) *)
	timeIt ("Andor.makeAndor", location, Andor.makeAndor, (protoAndor, allRules))

    val _ = MCPrint.debugPrint
	      ("** matchComp: andor (nodeCount = " ^ Int.toString nodeCount ^ ") =",
	       (fn ppstrm => MCPrint.ppAndor ppstrm andor))

    val _ = MCPrint.debugPrint
	      ("** matchComp: pvarmap = ",
	       (fn ppstrm => ppPvarMap ppstrm pvarmap))

    val decTree = (* DecisionTree.genDecisionTree (andor, allRules) *)
	timeIt ("genDecisionTree", location, DT.genDecisionTree, (andor, allRules))

    val _ = MCPrint.debugPrint
	      ("** matchComp: decTree = ",
	       (fn ppstrm => MCPrint.ppDecTree ppstrm decTree))

    (* checking exhaustiveness and redundancy of rules *)

    val decTreeStats as {rulesUsed, failures, ...} : DT.decTreeStats = DT.decTreeStats decTree
    val unusedRules : ruleset = RS.difference (allRules, rulesUsed)  (* expanded rules *)
    val unusedOriginalRules : ruleset = RS.map (#3 o ruleMap) unusedRules
        (* unusedRules translated back to corresponding original rule numbers *)
    val redundant = not (RS.isEmpty unusedRules)
    val nonexhaustive = (failures > 0)  (* any FAIL nodes => nonexhaustive rules *)

    (* generating the "raw" lexp for the match *)

    val (matchLexp, rootLvar) = (* Generate.genMatch (andor, decTree, pvarmap, ruleMap,
                                                      fail, toTcLt, giis) *)
        timeIt ("genMatch", location, Generate.genMatch,
		(andor, decTree, pvarmap, ruleMap, failExnOp))

    (* wrapping let-bindings of abstracted right-hand-sides around match code,
     * (corresponds to newmc "genprelude") *)

    val code: AS.exp = foldl (fn (fbinder, body) => fbinder body) matchLexp
			     rhsFunBinders

    val _ = if !printMatchAbsyn
	    then ED.withInternals (fn () => ppExp (matchExp, "matchComp:matchExp: \n"))
	    else ()

    val _ = reportStats (nodeCount, decTreeStats)

    (* rudundant <=> not (null unusedOriginalRules) <=> not (null unusedExpandedRules) *)
 in (code, rootLvar, RS.toList unusedOriginalRules, redundant, nonexhaustive)
end

(* ================================================================================ *)

(* translation functions: translate AS.exp and AS.dec while compiling matches
 *   transExp : AS.exp -> AS.exp
 *   transDec : AS.dec -> AS.dec
 *)

(* simpleVALdec : V.var * AS.exp * T.tyvar list -> AS.dec *)
(* used in Translate.transDec/transVB *)
fun simpleVALdec (var, exp, boundtvs) =
    VALdec [VB{pat = VARpat var, exp = exp,
	       typ = V.varType var, boundtvs = boundtvs, tyvars = ref nil}]

(* transExp : AS.exp -> AS.exp *)
fun transExp (calledFrom: string, exp) =
    let val _ = says [">> transExp: from ", calledFrom]
	val transRules =  (* apply fillPat to rule patterns, translate rhss *)
	    map (fn RULE(pat,exp) => RULE(EU.fillPat pat, transExp exp))
        fun trans exp =
            (case exp
	       of RECORDexp fields =>
		    RECORDexp (map (fn (numberedLabel, exp) => (numberedLabel, trans exp))
				   fields)
		| VECTORexp (exps,ty) => VECTORexp(map trans exps, TU.prune ty)
		| APPexp (rator, rand) => APPexp (trans rator, trans rand)
		| FNexp (rules, argTy, resTy) =>
		    let val RULE(pat1,exp1)::_ = rules (* debugging *)
			val _ = (say "transExp:FNexp:pat1 = "; ppPat pat1; newline() (* debugging *))
			val (bodyExp, matchVar) =
			    matchComp (transRules rules, argTy, resTy, SOME (EU.getMatchExn()))
		     in FNexp ([RULE(VARpat matchVar, bodyExp)], TU.prune argTy, TU.prune resTy)
		    end
		| HANDLEexp (baseExp, (rules, argTy, resTy)) =>
		    let val (handlerBody, matchVar) =
			    matchComp (transRules rules, argTy, resTy, NONE)
			val matchRule = RULE(VARpat matchVar, handlerBody)
		     in HANDLEexp(trans baseExp, ([matchRule], TU.prune argTy, TU.prune resTy))
		    end
		| CASEexp (scrutinee, (rules, scrutTy, resTy)) =>
		    let val RULE(pat1,exp1)::_ = rules (* debugging *)
			val _ = (say "transExp:CASEexp:pat1 = "; ppPat pat1; newline() (* debugging *))
			val (caseBody, matchVar) =
			    matchComp(transRules rules, scrutTy, resTy, EU.getMatchExn())
		    in LETexp(VALdec[VB{pat = VARpat matchVar,
					exp = trans scrutinee,
					typ = scrutTy,
					boundtvs = nil,
					tyvars = ref nil}],
			      caseBody)
		    end
		| RAISEexp (exp, ty) => RAISEexp (trans exp, ty) (* original source raise *)
		| IFexp {test, thenCase, elseCase} =>
		    IFexp {test = trans test, thenCase = trans thenCase, elseCase = trans elseCase}
		| ANDALSOexp (exp1, exp2) => ANDALSOexp (trans exp1, trans exp2)
		| ORELSEexp (exp1, exp2) => ORELSEexp (trans exp1, trans exp2)
		| WHILEexp { test, expr } => WHILEexp{ test = trans test, expr = trans expr}
		| LETexp (dec, exp) => LETexp(transDec dec, trans exp)
		| SEQexp exps => SEQexp (map trans exps)
		| CONSTRAINTexp (exp, ty) => CONSTRAINTexp (trans exp, ty)
		| MARKexp (exp, region) => MARKexp (trans exp, region)
		| _ => exp)
		  (* (VARexp _ | CONexp _ | NUMexp _ | REALexp _ | STRINGexp _  | CHARexp _ |
		   *  RSELECTexp _ | VSELECTexp _ | SWITCHexp _ | VSWITCHexp) => exp *)
     in trans exp
    end (* transExp *)

(* transDec : AS.dec -> dec *)
and transDec (dec: AS.dec): AS.dec =
    let fun transDec0 (dec: AS.dec) : AS.dec =
            (case dec
	      of VALdec vbs => transVBs vbs
	       | VALRECdec rvbs => VALRECdec (map transRVB rvbs)
	       | DOdec exp => DOdec (transExp("transDec:DOdec", exp))
	       | LOCALdec (innerDec, outerDec) =>
		   LOCALdec (transDec0 innerDec, transDec0 outerDec)
	       | SEQdec decs => SEQdec (map transDec0 decs)
	       | ABSTYPEdec {abstycs, withtycs, body} =>
		   ABSTYPEdec {abstycs = abstycs, withtycs = withtycs, body = transDec0 body}
	       | MARKdec (dec, region) => MARKdec (transDec0 dec, region)
	       | _ => dec) (* transDec called locally for LETSTR, LETFCT, STRdec, FCTdec *)

	(* transvb : AS.vb -> AS.dec *)
	(* translation of vb to dec
	 * -- We can get away without a d (DB depth) parameter, leaving it to Translate.
	 * -- Looks like we can get away with never dealing with an internal svar in the match.
	 * -- we need to access or (re)construct the type of the pat.
	 *      Could store this as a field of VB.
	 * -- do we need an absyn equivalent to mkPE, say transPolyExp? We don't have an equivalent
	 *      to TFN in absyn -- yet! *)
	and transVB (VB{pat, exp, typ, boundtvs, tyvars}) =
	    (* match compile [(pat,exp)] if pat is nontrivial (not a var);
	     * -- check what the match compiler does with (single) irrefutable patterns
	     *    DONE -- it does the right thing. *)
	    (if !debugging
	     then (say "transVB:pat = "; ppPat pat; ppExp (exp, "transVB:exp = "))
	     else ();
	     case EU.fillPat(AU.stripPatMarks pat)   (* does fillPat strip MARKpats? *)
	       of (WILDpat | CONSTRAINTpat(WILDpat, _)) =>  (* WILDpat pattern *)
		  let val exp' = transExp ("transVB:WILDpat", exp)
		   (* val _ = (print "transVB:exp' = "; ppExp exp') *)
		   in VALdec([VB{pat = WILDpat, exp = exp', typ = typ,
				 boundtvs = boundtvs, tyvars = tyvars}])
		  end
		| (VARpat var | CONSTRAINTpat(VARpat var, _)) =>
		  (* simple single variable pattern *)
		  let val exp' = transExp ("transVB:VARpat", exp)
		  (* val _ = (print "transVB:exp' = "; ppExp exp') *)
		  in VALdec([VB{pat = VARpat var, exp = exp',
				typ = typ, boundtvs = boundtvs, tyvars = tyvars}])
		  end
		| pat =>  (* compound, possibly refutable, pattern. Does this work for constants? *)
		  let val patvars = AU.patternVars pat
		      val bindExn = EU.getBindExn()
		  in case patvars
		       of nil =>   (* "constant" pattern, no pattern variables *)
			  let val (matchExp, matchVar) =
				   matchComp ([RULE (pat, AU.unitExp)], typ, BT.unitTy, bindExn)
			      val resultDec =
				  LOCALdec(simpleVALdec(matchVar,
							transExp ("transVB: null patvars", exp), nil),
					    VALdec([VB{pat=WILDpat, exp = matchExp,
						       typ=typ, boundtvs=nil, tyvars = ref nil}]))
			   in if !debugging then ppDec (resultDec, "transVB (no var): \n") else ();
			      resultDec
			  end
			| [pvar] =>     (* single pattern variable *)
			  let val pvarTy = V.varType pvar
			      val (matchExp, matchVar) =
				   matchComp ([RULE (pat, VARexp(ref pvar, nil))], typ, pvarTy, bindExn)
			      val resultDec =
				   LOCALdec(simpleVALdec(matchVar, transExp ("transVB: single patvar", exp), nil),
					    simpleVALdec(pvar, matchExp, boundtvs))
			   in if !debugging then ppDec (resultDec, "transVB (single var): \n") else ();
			      resultDec
			  end
			| patvars =>
			  let val patvarstupleExp =
				  EU.TUPLEexp (map (fn var => VARexp(ref var, nil)) patvars)
			      val patvarstupleTy = Tuples.mkTUPLEtype (map V.varType patvars)
			      val (matchExp, matchVar) =
				  matchComp ([RULE (pat, patvarstupleExp)], typ, patvarstupleTy, bindExn)
				  (* matchVar will be bound to MC-translation of exp *)
			      val ptupleVar = V.VALvar{path = SP.SPATH [S.varSymbol "<ptupleVar>"],
						       typ = ref(patvarstupleTy),
						       btvs = ref(boundtvs),  (* possibly polymorphic *)
						       access = A.LVAR(LambdaVar.mkLvar()),
						       prim = PrimopId.NonPrim}
			      fun selectVBs([], _) = []
				| selectVBs (pvar::pvars, n) = (* non-polymorphic *)
				    simpleVALdec(pvar, RSELECTexp (ptupleVar,n), nil)
				      :: selectVBs(pvars, n+1)
				    (* defining a pattern var by (record) selection from a
				     * var (ptupleVar) bound to the tuple of all the pattern
				     * var values; the btvs of each pattern var is a subset
				     * of the btvs of ptupleVar. *)
			      val resultDec =
				  LOCALdec(SEQdec [simpleVALdec(matchVar,
								transExp ("transVB: general", exp), nil),
						   simpleVALdec(ptupleVar, matchExp, boundtvs)],
					   SEQdec (selectVBs(patvars, 0)))
					  (* rebinding orig pattern variables *)
			   in if !debugging then ppDec (resultDec, "transVB (multiple vars): \n") else ();
			      resultDec
			  end
		  end (* pat case *))

	    (* NOTE: Given the way Translate.transDec deals with LOCALdec (i.e. not
             * hiding the local decls), we use a single SEQ encompassing all the
             * declarations. *)

	(* transVBs : AS.vb list -> AS.dec *)
	and transVBs nil = bug "transVBs: nil"  (* expect at least one *)
	  | transVBs [vb] = transVB vb
	  | transVBs vbs = SEQdec (map transVB vbs)

	and transRVB (RVB{var: V.var, exp: exp, resultty: T.ty option, tyvars: T.tyvar list ref}) =
	    (says [">> transRVB:var = ", Symbol.name (V.varName var)];
	     RVB {var = var, exp = transExp ("transRVB", exp), resultty = resultty, tyvars = tyvars})

     in transDec0 dec
    end (* transDec *)

fun transMatchDec dec = transDec dec

end (* local *)
end (* structure MatchComp *)

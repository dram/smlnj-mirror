(* matchcomp.sml *)

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

  val printAndor = ElabControl.printAndor
  val printDecisionTree = ElabControl.printDecisionTree
  val printMatchAbsyn = ElabControl.printMatchAbsyn
  val debugging = ElabControl.mcdebugging

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)
  fun says strings = saynl (concat strings)

  fun bug msg = ErrorMsg.impossible ("MatchComp: " ^ msg)

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

(* simpleVALdec : V.var * AS.exp * T.tyvar list -> AS.dec *)
(* used in Translate.transDec/transVB *)
fun simpleVALdec (var, exp, boundtvs) =
    VALdec [VB{pat = VARpat var, exp = exp,
	       typ = V.varType var, boundtvs = boundtvs, tyvars = ref nil}]


(* genMatch : AS.rules * MT.andor * DT.decTree * int vector * T.ty * varenvAC * T.datacon -> AS.exp * V.var *)
(* top level code generating function for matches (formerly MCCode.genCode) *)
fun genMatch (rules: AS.rule list, andor, decTree, ruleCounts, rhsTy, varenvAC, matchExn) =
    let val _ = dbsay ">> genMatch"
	val rules = map (fn AS.RULE r => r) rules
	  (* rules : (AS.pat * AS.exp) list -- the RULE constructor is stripped *)

    val numberOfRules = length rules

    (* isMultiple : ruleno -> bool *)
    fun isMultiple ruleno = Vector.sub (ruleCounts, ruleno) > 1

    val multirules = List.filter isMultiple (List.tabulate (numberOfRules, (fn n => n)))
        (* list of rule numbers that have multiple uses, in ascending order *)

    val ruleCountsList = Vector.foldr (op ::) nil ruleCounts
    val _ = dbsays ["** genMatch: ruleCounts: ",
		    (PrintUtil.listToString ("[", ",", "]") Int.toString ruleCountsList),
		    ", length rules: ", Int.toString (length rules)]
	     
    val _ = dbsays ["multirules = ",
		    (PrintUtil.listToString ("[", ",", "]") Int.toString multirules)]

    (* rulePat : ruleno -> AS.pat *)
    fun rulePat n = #1 (List.nth (rules, n))

    (* rulePatVars : V.ar list list *)
    (* note! WILDpat will produce _no_ bound variables, but corresponding rhs should
     * be functionalized *) 
    val rulePatVars: V.var list list = map (AU.patternVars o #1) rules

    (* ruleBoundVars : ruleno -> V.var list *)
    fun ruleBoundVars n = List.nth (rulePatVars, n)

    (* ruleRHS : ruleno -> AS.exp *)
    fun ruleRHS n = #2 (List.nth (rules, n))
          (* : AS.exp -- rhs expression of rule n, already match translated *)

    fun sameVar (var : V.var, AS.VARexp(ref var',_)) = V.eqVar (var, var')
      | sameVar _ = false

    (* makeRHSfun : ruleno -> AS.exp option *)
    (* part of the need to alpha-convert vars is to ensure that the FLINT invariant of
     * lvars only being bound once is maintained (in particular, use of VarEnvAC.alphaEnv). *)
    fun makeRHSfun ruleno =
        if isMultiple ruleno
	then let val pat = rulePat ruleno
		 val boundVars = ruleBoundVars ruleno
		 val rhs = ruleRHS ruleno
	      in if (* (null boundVars andalso not (AU.isWild pat)) orelse *)
		    (length boundVars = 1 andalso sameVar (hd boundVars, rhs))
		 then NONE (* rhs fn <<would take unit>> or is identity, don't wrap <- WRONG! scc/t2.sml *)
		 else (* wrap rhs fun binding while alpha-converting boundVars in rhs *)
		     let val venv = VarEnvAC.alphaEnv boundVars
		         (* varenvAC for alpha-converting boundVars, empty if WILDpat *)
			 val fnbody = transExp (rhs, VarEnvAC.append (venv, varenvAC))
			 val fnvar = SV.newSvar("rhs"^(Int.toString ruleno), T.UNDEFty)
		     in SOME (fnvar, C.Sfun(VarEnvAC.range venv, fnbody, rhsTy))
		     end
	     end
	else NONE  (* rule rhs only used once *)

    (* rhsFuns : (svar * AS.exp) option array, of length numberOfRules  *)
    (* rhs functions constructed here, before generating the main match expression, so that
     * genRHS will be affected by the special cases where rhs functions are not needed *)
    val rhsFuns = Array.tabulate (numberOfRules, makeRHSfun)

    (* wrapFn : ruleno * AS.exp -> AS.exp *)
    (* wraps the (match) expression with a let binding of a rhs function, which
     * abstracts over the pattern variables for the corresponding rule. The
     * pattern variables used as parameters of the rhs function need to be alpha-
     * converted to ensure unique bindings of lvars after translation *)
    fun wrapFn (ruleno, baseExp) =
        case Array.sub (rhsFuns, ruleno)
	  of NONE => baseExp
	   | SOME (fnvar, fnexp) => C.Letf(fnvar, fnexp, baseExp)

    (* wrap the rhs fn bindings around the base match expression *)
    fun genPrelude exp = (* foldr wrapFn exp multirules *)
	let fun wrap (0, base) = wrapFn (0, base)
	      | wrap (n, base) = wrap (n-1, wrapFn (n, base))       
	 in wrap (numberOfRules - 1, exp)
	end

    (* bindSVars : var list * layer * varenvMC -> VarEnvAC.varenvAC *)
    (* pvars will be the source pattern variables for the layer. bindSVars will
     * determine the right svar (match administrative variable) to bind each pattern variable
     * to, producing an varenvAC environment. The varenvAC is computed from the varenvMC
     * based on the rule and (possibly) consistency with the decision tree trace. This
     * is because a pattern variable that is duplicated in OR patterns may be associated
     * with multiple svars. We choose the right one based on the layer argument. *)
    fun bindSVars (pvars, layer, varenvMC) =
	let val _ = dbsay ">> bindSVars";
	    fun findSvar (var: V.var) = (var, VarEnvMC.lookVar (varenvMC, var, layer))
	    val venv = map findSvar pvars
	 in if !debugging
            then (say "<< bindSvars: venv = "; VarEnvAC.printVarEnvAC venv;
		  say ", layer = "; say (L.layerToString layer); say "\n")
	    else ();
	    venv
	end

    (* genRHS : ruleno * varenvMC * path list * varenvAC -> AS.exp *)
    fun genRHS (layer, varenvMC: VarEnvMC.varenvMC, externVarenvAC) =
	let val ruleno = L.toRule layer
	    val patvars = ruleBoundVars ruleno
	    val localVarenvAC = bindSVars (patvars, layer, varenvMC)
	    val svars = VarEnvAC.range localVarenvAC
	    val venv = VarEnvAC.append (localVarenvAC, externVarenvAC)
	    (* val _ = (print "genRHS:venv = "; VarEnvAC.printVarEnvAC venv; print "\n") *)
	 in case Array.sub(rhsFuns, ruleno)
	      of SOME (fvar, _) => C.Sapp (fvar, svars)  (* applies the rhs fun *)
	       | NONE => transExp (ruleRHS ruleno, venv)
	          (* match svars substituted for source vars directly into a single-use rhs *)
	end

    (* savedTraces : trace list ref
     * This variable saves traces that are found at DMATCH nodes.
     * These can be used to generate counterexamples for non-exhaustive matches. *)
    val savedTraces = ref (nil : path list list)
    fun saveTrace dtrace = (savedTraces := dtrace :: !savedTraces)

    (* bindPatVars: svar * AOinfo * varenvMC -> varenvMC *)
    (* add bindings pvar |-> (layer, svar) to the environment varenvMC for all pvars bound at
     * an AndOr node (represented by info and svar). First the (primary) vars, and then the
     * (secondary) asvars are bound.
     * [Note: could use one foldl after appending vars to asvars.] *)
    fun bindPatVars (svar, {vars,asvars,...} : AOinfo, varenvMC) =
	(dbsays [">> bindPatVars: |vars| = ", Int.toString (length vars), ", |asvars| = ",
		 Int.toString (length asvars)];
	foldl (fn ((pvar,layer), env) => VarEnvMC.bindVar(pvar, layer, svar, env))
	   (foldl (fn ((pvar,layer), env) => VarEnvMC.bindVar(pvar, layer, svar, env)) varenvMC vars)
	   asvars)


    (* genAndor: andor * SE.svarenv * VarEnvMC.varenvMC * (exp -> exp)
                 -> SE.svarenv * VarEnvMC.varenvMC * (exp -> exp) *)
    (* Translates top non-OR structure, i.e. AND structure, (if any) into nested let
     * expressions to be wrapped around a body expression (generated by a decision tree.
     * The wrapped let expressions are accumulated in a "continuation", k: exp -> exp
     * that will be applied to the expression generated for the next chosen decision tree
     * It is applied to the root node of an andor tree, and also to the variant
     * nodes under an OR choice.
     * What happens in the case of a single, irrefutable rule, where there will
     * be no OR-nodes? Claim that it works out properly. (Explain, details?) *)
    (* ASSUME: andor.info.id is bound to an svar in the svarenv argument *)
    fun genAndor (andor, svarenv, varenvMC, k) =
	let fun genNodes (nodes, svarenv, varenvMC, k) =
		  let fun genf (node, (svarenv0, varenvMC0, k0)) =
			    genNode (node, svarenv0, varenvMC0, k0)
		   in foldr genf (svarenv, varenvMC, k) nodes
		  end
            (* genNode : andor * SE.svarenv * varenvMC * (exp -> exp) 
                         -> SE.svarenv * varenvMC * (exp -> exp)  *)
	    and genNode (AND {info,children,andKind,...}, svarenv, varenvMC, k) =
                (dbsays [">> genNode:AND: ", Int.toString (#id info), ", ",
			 Int.toString (length (#vars info))];
		 case SE.lookSvar (svarenv, #id info)
		    of SOME thisSvar =>
			 let fun collectChildSvars (childAndor, (svars, svarenv)) =
				 let val childInfo = getInfo childAndor
				     val svar = MU.infoToSvar (childInfo)
					 (* was: info -- fix for vecpat bug *)
				  in (svar :: svars, SE.bindSvar(#id childInfo, svar, svarenv))
				 end
			     val (childrenSvars, svarenv') =
				   foldr collectChildSvars ([], svarenv) children
			     val varenvMC' = bindPatVars (thisSvar, info, varenvMC)
			     val (svarenv'', varenvMC'', k0) =
				   genNodes(children, svarenv', varenvMC', k)
			     val k1 =
				 (case andKind
				   of RECORD =>
					(fn inner => C.Letr (childrenSvars, thisSvar, k0 inner))
				    | VECTOR =>
					(fn inner => C.Letv (childrenSvars, thisSvar, k0 inner)))
			  in (svarenv'', varenvMC'', k1)
			 end
		     | NONE => bug "genNode:AND: no svar")
	      | genNode (SINGLE {info, variant = (key, arg)}, svarenv, varenvMC, k) =
                (dbsays [">> genNode:SINGLE: ", Int.toString (#id info), ", ",
			 Int.toString (length (#vars info))];
		  case SE.lookSvar (svarenv, #id info)
		    of SOME thisSvar =>
			 let val varenvMC' = bindPatVars (thisSvar, info, varenvMC)
			  in case arg
			       of LEAF _ => (svarenv, varenvMC', k)
				| INITIAL => bug "genAndor: SINGLE: arg = INITIAL"
				| _ =>  (* neither LEAF nor INITIAL *)
				  let val argInfo = getInfo arg (* defined -- not LEAF, INITIAL *)
				      val argSvar = MU.infoToSvar argInfo
				      val svarenv' = SE.bindSvar(#id argInfo, argSvar, svarenv)
				      val (svarenv'', varenvMC'', k1) =
					    genNode (arg, svarenv', varenvMC', k)
				      val k2 =
					  fn (inner: exp) =>
					     C.Switch (thisSvar, [(key, SOME argSvar, k1 inner)],
						       NONE)
				  in (svarenv'', varenvMC'', k2)
				  end
			 end
		     | NONE => bug "genNode:SINGLE: no svar")
	      | genNode (OR {info as {id,vars,...},...}, svarenv, varenvMC, k) =
                  (* this OR node is already accounted for in the dectree (if not redundant) *)
                (dbsays [">> genNode:OR: ", Int.toString id, ", ", Int.toString (length vars)];
		  case SE.lookSvar (svarenv, id)
		     of SOME thisSvar =>
			    let val varenvMC' = bindPatVars (thisSvar, info, varenvMC)
			     in (svarenv, varenvMC', k)
			    end
		      | NONE => bug "genNode:OR: no svar")
	      | genNode (VARS {info as {id,vars,...}, ...}, svarenv, varenvMC, k) =
                (dbsays [">> genNode:VARS: ", Int.toString id, ", ", Int.toString (length vars)];
		   case SE.lookSvar (svarenv, id)
		     of SOME thisSvar =>
		         let val varenvMC' = bindPatVars (thisSvar, info, varenvMC)
			  in (svarenv, varenvMC', k)
			 end
		      | NONE => bug "genNode:VARS: no svar")
	      | genNode (LEAF _, _, _, _) = bug "genNode: LEAF"
	          (* should not happen! LEAF is found only in an OR variant; genNode stops at OR *)
	      | genNode (INITIAL, _, _, _) = bug "genNode: INITIAL"
	 in genNode (andor, svarenv, varenvMC, k)
	end (* genAndor *)

    (* genDecTree: decTree * svarenv * varenvMC -> varenvMC * AS.exp *)
    (* need to thread a varenvMC through or some varenvMC bindings will become inaccessible (t6.sml) *) 
    fun genDecTree (decTree, svarenv, varenvMC) =
	(case decTree
	   of MT.CHOICE{node, choices, default} =>
		(case node    (* ASSERT: node must be OR *)
		  of OR{info, variants,...} =>
		     (* choices and variants should be "congruent", since choices is
		      * created as a Variants.map of variants, but we insure coordination
		      * by defining aoVariants and dtVariants bellow, which will be
		      * guaranteed to have keys in the same order. *)
		     (case SE.lookSvar (svarenv, #id info)
		       of SOME svar =>
			  let (* val _ = (ppType "MatchComp.genDecTree: svar type: "
				          (SVar.svarType svar); newline()) *)
			      fun switchBody ((key,node0)::nrest, (key',decTree0)::drest, sbody) =
				    (* ASSERT: key = key' *)
				    let val _ = if not(K.eqKey(key,key'))  (* verifying ASSERT above *)
						then bug "genDecTree:CHOICE:keys disagree"
						else ()
					(* val _ = say (concat ["genDecTree.switchBody:key = ",
					                        keyToString key, "\n"]) *)
					val (svarOp, decExp) =  (* argument svar + subtree *)
					    (case node0
					      of LEAF _ =>
						   let val decExp0 = genDecTree(decTree0, svarenv, varenvMC)
						    in (NONE, decExp0)
						   end
						| _ =>
						  let val svar0 =
							  (case node0 (* special case for vector pats *)
							    of AND{andKind=VECTOR,...} => svar (* from parent *)
							     | _ => MU.andorToSvar node0)
						       val svarenv' = SE.bindSvar(getId node0, svar0, svarenv)
						       val (svarenv'', varenvMC', k_node0) =
							   genAndor(node0, svarenv', varenvMC, k_ident)
						       val decExp0 = genDecTree(decTree0, svarenv'', varenvMC')
						    in (SOME svar0, k_node0 decExp0)
						   end)
				     in switchBody(nrest, drest, (key, svarOp, decExp)::sbody)
				    end
				  | switchBody (nil,nil,sbody) = rev sbody
				  | switchBody _ = bug "code.genDecTree.switchBody"
			        val aoVariants = Variants.listItems' variants
				fun getDecTrees (k,_) =
				    (case Variants.find' (choices,k)
			               of SOME x => x
				        | NONE => bug "getDecTrees")
				val dtVariants = map getDecTrees aoVariants
				val sbody = switchBody(aoVariants, dtVariants, nil)
				val default = Option.map (fn t => genDecTree(t,svarenv,varenvMC)) default
			     in C.Switch (svar, sbody, default)
			    end
			  | NONE => bug "genDecTree:CHOICE: no svar")
		   | _ => bug "genDecTree: CHOICE node not an OR node")
            | MT.DMATCH dtrace => (saveTrace dtrace; C.Failure (matchExn, rhsTy))  (* match failure *)
	    | MT.DLEAF (layer, dtrace) => genRHS(layer, varenvMC, varenvAC)) (* dispatch to rhs *)

    val svarTop = MU.andorToSvar andor
    val svarenv0 = SE.bindSvar (getId andor, svarTop, SE.empty)
    val (svarenv', varenvMC, k_andor) = genAndor (andor, svarenv0, VarEnvMC.empty, k_ident)
    val decExp = genDecTree (decTree, svarenv', varenvMC)
    val mainExp = k_andor decExp

 in (genPrelude mainExp, svarTop)
end (* fun genMatch *)

(* ================================================================================ *)
(* matchComp: AS.rule list * T.ty * T.datacon -> AS.exp * VarCon.var *)
(* lhsTy is the type of the patterns (i.e. the domain or lhs) of the match
 * Called by transMatch.  fillPat will have been applied to rule patterns. *)
and matchComp (rules: AS.rule list, lhsTy: T.ty, rhsTy: T.ty, varenvAC,
	       matchFailExn: T.datacon) =
    let val _ = dbsay ">> matchComp";
	val numRules = length rules
	val patterns = map (fn (AS.RULE(pat,_)) => pat) rules (* strip RULE constructor *)
	val andor = AndOr.makeAndor(patterns, lhsTy)
	val _ = dbsay "<< makeAndor"
	val _ = if !printAndor
		then ppAndor andor
		else ()
	val (dectree,ruleCounts) = DecisionTree.decisionTree (andor, numRules)
	val _ = dbsay "<< decisionTree"
	val _ = if !printDecisionTree
		then ppDecisionTree dectree
		else ()
        val (matchExp, matchVar) =
	    genMatch (rules, andor, dectree, ruleCounts, rhsTy, varenvAC, matchFailExn)
	val _ = dbsay "<< genMatch"
	val _ = if !printMatchAbsyn
		then ED.withInternals (fn () => ppExp (matchExp, "matchComp:matchExp: \n"))
		else ()
    in dbsay "<< matchComp";  (* -- debugging *)
       (matchExp, SVar.svarToVar matchVar)
    end

(* ================================================================================ *)
(* translation functions: translate AS.exp and AS.dec while compiling matches and
 * alpha-converting source pattern variables *)

(* transExp : AS.exp * varenvAC -> AS.exp *)
and transExp (exp, venv: VarEnvAC.varenvAC) =
    let val transRules =  (* just do fillPat on pattern *)
	    map (fn RULE(pat,exp) => RULE(EU.fillPat pat, exp))
        fun trans exp =
            (case exp
	       of RECORDexp fields =>
		    RECORDexp (map (fn (numberedLabel, exp) => (numberedLabel, trans exp))
				   fields)
		| VECTORexp (exps,ty) => VECTORexp(map trans exps, TU.prune ty)
		| APPexp (rator, rand) => APPexp (trans rator, trans rand)
		| FNexp (rules, argTy, resTy) =>
		    let val RULE(pat1,exp1)::_ = rules (* debugging *)
			(* val _ = (print "transExp:FNexp:pat1 = "; ppPat pat1 (* debugging *)) *)
			val (bodyExp, matchVar) =
			    matchComp (transRules rules, argTy, resTy, venv, EU.getMatchExn())
		     in FNexp ([RULE(VARpat matchVar, bodyExp)], TU.prune argTy, TU.prune resTy)
		    end
		| HANDLEexp (baseExp, (rules, argTy, resTy)) =>
		    let val (handlerBody, matchVar) =
			    matchComp (transRules rules, argTy, resTy, venv, EU.getMatchExn())
			val matchRule = RULE(VARpat matchVar, handlerBody)
		     in HANDLEexp(trans baseExp, ([matchRule], TU.prune argTy, TU.prune resTy))
		    end
		| RAISEexp (exp, ty) => RAISEexp (trans exp, ty) (* original source raise *)
		| CASEexp (scrutinee, (rules, scrutTy, resTy)) =>
		    let val _ = dbsay ">> transExp: CASEexp"
			val (caseBody, matchVar) =
			    matchComp(transRules rules, scrutTy, resTy, venv, EU.getMatchExn())
		    in LETexp(VALdec[VB{pat = VARpat matchVar,
					exp = trans scrutinee,
					typ = scrutTy,
					boundtvs = nil,
					tyvars = ref nil}],
			      caseBody)
		    end
		| IFexp {test, thenCase, elseCase} =>
		  IFexp {test = trans test, thenCase = trans thenCase, elseCase = trans elseCase}
		| ANDALSOexp (exp1, exp2) => ANDALSOexp (trans exp1, trans exp2)
		| ORELSEexp (exp1, exp2) => ORELSEexp (trans exp1, trans exp2)
		| WHILEexp { test, expr } => WHILEexp{ test = trans test, expr = trans expr}
		| LETexp (dec, exp) => LETexp(transDec (dec, venv), trans exp)
		| SEQexp exps => SEQexp (map trans exps)
		| CONSTRAINTexp (exp, ty) => CONSTRAINTexp (trans exp, ty)
		| MARKexp (exp, region) => MARKexp (trans exp, region)
		| VARexp (vref, tvs) =>
		    (case VarEnvAC.look (venv, !vref)
		       of NONE => exp
			| SOME var => (VARexp (ref var, tvs)))
		| _ => exp)
		  (* (VARexp _ | CONexp _ | NUMexp _ | REALexp _ | STRINGexp _  | CHARexp _ |
		   *  RSELECTexp _ | VSELECTexp _ | SWITCHexp _ | VSWITCHexp) => exp *)
     in trans exp
    end (* transExp *)

(* transDec : AS.dec * VarEnvAC.varenvAC -> dec *)
and transDec (dec: AS.dec, venv: VarEnvAC.varenvAC) : AS.dec =
    let fun transDec0 (dec: AS.dec) : AS.dec =
            (case dec
	      of VALdec vbs => transVBs vbs
	       | VALRECdec rvbs => VALRECdec (map transRVB rvbs)
	       | DOdec exp => DOdec (transExp(exp, venv))
	       | LOCALdec (innerDec, outerDec) =>
		   LOCALdec (transDec0 innerDec, transDec0 outerDec)
	       | SEQdec decs => SEQdec (map transDec0 decs)
	       | ABSTYPEdec {abstycs, withtycs, body} =>
		   ABSTYPEdec {abstycs = abstycs, withtycs = withtycs, body = transDec0 body}
	       | MARKdec (dec, region) => MARKdec (transDec0 dec, region)
	       | _ => dec) (* transDec called locally for LETSTR, LETFCT, STRdec, FCTdec *)

	    (* transvb : A.vb -> A.dec *)
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
		      let val exp' = transExp (exp, venv)
	               (* val _ = (print "transVB:exp' = "; ppExp exp') *)
		       in VALdec([VB{pat = WILDpat, exp = exp', typ = typ,
				     boundtvs = boundtvs, tyvars = tyvars}])
		      end
		    | (VARpat var | CONSTRAINTpat(VARpat var, _)) =>
		      (* simple single variable pattern *)
		      let val exp' = transExp (exp, venv)
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
				       matchComp ([RULE (pat, AU.unitExp)], typ, BT.unitTy, venv, bindExn)
				  val resultDec =
				       LOCALdec(simpleVALdec(matchVar, transExp (exp, venv), nil),
						VALdec([VB{pat=WILDpat, exp = matchExp,
							   typ=typ, boundtvs=nil, tyvars = ref nil}]))
			       in if !debugging then ppDec (resultDec, "transVB (no var): \n") else ();
				  resultDec
			      end
			    | [pvar] =>     (* single pattern variable *)
			      let val pvarTy = V.varType pvar
				  val (matchExp, matchVar) =
				       matchComp ([RULE (pat, VARexp(ref pvar, nil))], typ, pvarTy, venv,
						   bindExn)
				  val resultDec =
				       LOCALdec(simpleVALdec(matchVar, transExp (exp, venv), nil),
						simpleVALdec(pvar, matchExp, boundtvs))
			       in if !debugging then ppDec (resultDec, "transVB (single var): \n") else ();
                                  resultDec
			      end
			    | patvars =>
			      let val patvarstupleExp =
			              EU.TUPLEexp (map (fn var => VARexp(ref var, nil)) patvars)
				  val patvarstupleTy = Tuples.mkTUPLEtype (map V.varType patvars)
				  val (matchExp, matchVar) =
				      matchComp ([RULE (pat, patvarstupleExp)], typ, patvarstupleTy, venv,
						 bindExn)
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
				      LOCALdec(SEQdec [simpleVALdec(matchVar, transExp (exp, venv), nil),
						       simpleVALdec(ptupleVar, matchExp, boundtvs)],
					       SEQdec (selectVBs(patvars, 0)))
					      (* rebinding orig pattern variables *)
			       in if !debugging then ppDec (resultDec, "transVB (multiple vars): \n") else ();
				  resultDec
			      end
		      end (* pat case *)
		)


	    (* NOTE: Given the way Translate.transDec deals with LOCALdec (i.e. not
             * hiding the local decls), we use a single SEQ encompassing all the
             * declarations. *)

	    (* transVBs : AS.vb list -> AS.dec *)
	    and transVBs nil = bug "transVBs: nil"  (* expect at least one *)
	      | transVBs [vb] = transVB vb
	      | transVBs vbs = SEQdec (map transVB vbs)

	    and transRVB (RVB{var: V.var, exp: exp, resultty: T.ty option, tyvars: T.tyvar list ref}) =
		((* print "transRVB:var = "; print (Symbol.name (V.varName var)); print "\n"; *)
		RVB {var = var, exp = transExp (exp,venv), resultty = resultty, tyvars = tyvars})

	in transDec0 dec
    end (* transDec *)

fun transMatchDec dec = transDec (dec, VarEnvAC.empty)

end (* local *)
end (* structure MatchComp *)

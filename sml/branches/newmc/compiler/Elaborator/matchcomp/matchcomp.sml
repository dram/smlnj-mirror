(* matchcomp.sml *)

(* The "code" (Absyn.exp) for a match is generated from the decision tree and information
 * from the original andor (used for record/vector destruction and variable bindings (and types).
 * This code performs pattern dispatching and destruction and then invokes the appropriate
 * RHS expressions with the necessary variable bindings.
 *
 * The "code" is in the form of an Absyn.exp.  Absyn.exp has been augmented with two new
 * expression forms: SWITCHexp and VSWITCHexp. These are created only by the match
 * compilation, which takes place after type checking.
 *)

structure MatchComp =
struct

local
  structure S = Symbol
  structure SP = SymPath
  structure T = Types
  structure TU = TypesUtil
  structure A = Access
  structure V = VarCon
  structure AS = Absyn
  structure AU = AbsynUtil
  structure EU = ElabUtil
  structure R = Rules
  structure SV = SVar
  structure SE = SVarEnv
  structure MT = MCTypes
  structure MU = MCUtil
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

  fun ppAndor andor =
      PP.with_default_pp
	  (fn ppstrm =>
	      (PP.string ppstrm "andor:\n";
	       MCPrint.ppAndor ppstrm andor))

  fun ppDecisionTree dectree =
      PP.with_default_pp
	  (fn ppstrm =>
	      (PP.string ppstrm "dectree:\n";
	       MCPrint.ppDecTree ppstrm dectree))

  fun ppCode exp =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm "Absyn.exp:\n";
	       PPAbsyn.ppExp (StaticEnv.empty, NONE) ppstrm (exp, 20)))

  fun ppPat pat =
      PP.with_default_pp(fn ppstrm => PPAbsyn.ppPat StaticEnv.empty ppstrm (pat, 20))

  (* duplicates ppCode *)
  fun ppExp exp =
      PP.with_default_pp(fn ppstrm => PPAbsyn.ppExp (StaticEnv.empty,NONE) ppstrm (exp, 20))

  fun bug msg = ErrorMsg.impossible ("MCCode: " ^ msg)

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
(* used in transDec.transVB *)
fun simpleVALdec (var, exp, boundtvs) = 
    VALdec [VB{pat = VARpat var, exp = exp,
	       typ = V.varType var, boundtvs = boundtvs, tyvars = ref nil}]


(* genMatch : R.rules * MT.andor * DT.decTree * int vector * T.ty * T.datacon -> AS.exp * V.var *)
(* top level code generating function (formerly MCCode.genCode) *)
fun genMatch (rules: AS.rule list, andor, decTree, ruleCounts, rhsTy, varenvAC, matchExn) =
let val rules = map (fn AS.RULE r => r) rules
	  (* : (AS.pat * AS.exp) list -- strip the RULE constructor *)

    val rulePatVars: V.var list list = map (AU.patternVars o #1) rules

    fun multiple ruleno = Vector.sub(ruleCounts, ruleno) > 1

    val multirules = List.filter multiple (List.tabulate (length rules, (fn n => n)))
        (* list of rule numbers that have multiple uses *)

    val funSvars = map (fn r => SV.newSvar("rhsfun"^(Int.toString r), T.UNDEFty)) multirules
        (* produce svars to name rhs functions for rules in multirules *)

    (* findFunVar : ruleno -> svar *)
    fun findFunVar n =  (* n in multirules *)
	let fun loop (rule::rules, svar::svars) =
	          if rule = n then svar
	          else loop (rules, svars)
	      | loop (nil, nil) = bug "findFunVar: n out of scope"
	      | loop _ = bug "findFunVar: lengths of rules, svars differ"
	 in loop (multirules, funSvars)
	end

    (* rulePat : ruleno -> AS.pat *)
    fun rulePat n = #1 (List.nth (rules, n))

    (* ruleBoundVars : ruleno -> V.var list *)
    fun ruleBoundVars n = List.nth (rulePatVars, n)

    (* ruleRHS : ruleno -> AS.exp *)
    fun ruleRHS n = #2 (List.nth (rules, n))
          (* : AS.exp -- rhs expression of rule n, already match translated *)

    (* wrapFn : ruleno * AS.exp -> AS.exp *)
    (* wraps the (match) expression with a let binding of a rhs function, which
     * abstracts over the pattern variables for the corresponding rule. These 
     * pattern variables are _private_ to the rule and thus don't need to be
     * alpha-converted. *)
    fun wrapFn (n, exp) =
	let val boundVars = ruleBoundVars n
	    val body = transExp (ruleRHS n, varenvAC)  (* varenvAC; no new local bindings *)
	 in C.Letf(findFunVar n, C.Sfun(boundVars, body), exp)
	end

    (* wrap the rhs fn bindings around the base match expression *)
    fun genPrelude inner = foldr wrapFn inner multirules

    (* allConsistent : path * path list -> bool *)
    fun allConsistent (varpath, dtrace: path list) =
	List.all (MU.consistentPath varpath) dtrace

    (* bindSVars : var list * ruleno * varenvMC * trace -> (var * var) list *)
    fun bindSVars (pvars, rule, varenvMC, rhsTrace) =
	let fun findSvar (var: V.var) = 
		(case VarEnvMC.lookVar (varenvMC, var)
		  of SOME bindings =>
		     let fun scan (rule', path, svar) =
			     if rule' = rule
			     then if allConsistent (path, rhsTrace) (* CONJ: always true? *)
				  then SOME (var, svar)
				  else (print "@@@@ bindSVars: inconsistent path\n"; NONE)
		             else NONE
		     in  case List.mapPartial scan bindings
			  of nil => bug "bindSVars: no binding"
			   | _::_::_ => bug "bindSVars: multiple bindings"
			   | [b] => b  (* should be a unique svar for this var, rule, path *)
		     end
		   | NONE => bug "bindSVars: unbound pattern var")
	in map findSvar pvars
	end

    (* genRHS : (AS.pat * AS.exp) * varenvMC * path list * varenvAC -> AS.exp *)
    fun genRHS (rule, varenvMC, rhsTrace, externVarenvAC) =
	let val patvars = ruleBoundVars rule
	    val localVarenvAC = bindSVars (patvars, rule, varenvMC, rhsTrace)
	    val svars = VarEnvAC.range localVarenvAC
	    val rhsExp = ruleRHS rule
	 in if multiple rule
	    then C.Sapp(findFunVar rule, svars)  (* applies a rhs fun *)
	    else transExp (rhsExp, VarEnvAC.append(externVarenvAC, localVarenvAC))
	      (* match svars substituted for source vars directly into a single-use rhs *)
	end

    (* savedTraces : trace list ref
     * This variable saves traces that are found at DMATCH nodes.
     * These can be used to generate counterexamples for non-exhaustive matches. *)
    val savedTraces = ref (nil : path list list)
    fun saveTrace dtrace = (savedTraces := dtrace :: !savedTraces)

(* "direct style" -- replaced by continuation style version below
    (* genAndor: andor * (svarenv * varenv) -> (exp -> exp) * (svarenv * varenv) *)
    (* translates top non-OR structure (if any) into nested let expressions
     * wrapped around a body expression "inner". Applies to root node
     * of an andor tree, and also applied to the child nodes under an OR choice.
     * What happens in the case of a single, irrefutable rule, where there will
     * be no OR-nodes? It works out properly. *)
    fun genAndor (andor, (svarenv, varenv)) =
	let fun genNodes (node::nodes, envs) =
		let val genf ((build0, envs),node) =
			let val (build1, envs') = genNode (node, envs)
			in ((build1 o build0), envs')
			end
		in foldr genf ((fn x=> x), envs) nodes
		end
	    and genNode (AND{info,children,andKind,...}, (svarenv, varenv)) =
		  let val thisSvar = lookup (svarenv, #id info)
		      fun collectSvars ((svars, svarenv), andor) =
			  let val svar0 = MU.infoToSvar andor
			  in (svar0 :: svars, SE.bindSvar(andorId andor, svar0, svarenv))
			  end
		      val (childrenSvars, svarenv') =
			  foldl collectSvars ([], svarenv) children
		      val (bodyfn, (svarenv'', varenv'))  = genNodes(children, (svarenv',varenv))
		      val build = 
		          (case andKind
		            of RECORD => fn inner => C.Letr (svars, svar, bodyfn inner)
		             | VECTOR => fn inner => C.Letv (svars, svar, bodyfn inner))
		  in (build, (svarenv', varenv'))
		  end
	      | genNode (SINGLE{info, key, arg, ...}, envs) =
		  (case arg
		    of LEAF _ => ((fn x => x), envs)
		     | _ =>
		       let val (build, envs') = genNode (arg, envs)
			   fun build' (inner: exp) =>
			       C.Switch (svar, [(key, SOME(getSvar arg), build inner)], NONE)
		       in (build', envs')
		       end)
	      | genNode (OR _, envs) = ((fn x => x), envs)
                  (* this OR node is already accounted for in the dectree (if not redundant) *)
	      | genNode (VARS{info, defaults,...}, (svarenv, varenv)) =
		  let val thisSvar = SE.lookSvar (svarenv, #id info)
		      val binder = bindVar (thisSvar, #path info)
		      val varenv' = foldr binder (foldr binder varenv (#vars info))
                                          (#asvars info)       
		  in ((fn x => x), (svarenv, varenv'))
		  end
	      | genNode (LEAF _, _) = bug "genNode: LEAF"
	          (* should not happen! LEAF is found only in an OR variant *)
	      | genNode (INITIAL, _) = bug "genNode: INITIAL"
	 in genNode(andor, dectree)
	end
 *)
			       
    (* bindPatVars: svar * info * varenvMC -> varenvMC *)
    fun bindPatVars (svar, {id,path,vars,asvars,...} : AOinfo, varenvMC) =
	foldl (fn ((v,r), env) => VarEnvMC.bindVar(v, (r, path, svar), env))
	   (foldl (fn ((v,r),env) => VarEnvMC.bindVar(v, (r, path, svar), env)) varenvMC vars)
	   asvars

(* "continuation style" *)
    (* genAndor: andor * SE.svarenv * VE.varenv * (exp -> exp)
                 -> (svarenv * varenv) * (exp -> exp) *)
    (* Translates top non-OR structure (if any) into nested let expressions
     * wrapped around a body expression "inner".  The wrapped let expressions are
     * collected in a "continuation", k: exp -> exp that will be applied to the
     * expression generated for the decision tree associated with this node.
     * It is applied to the root node of an andor tree, and also to the child
     * nodes under an OR choice.
     * What happens in the case of a single, irrefutable rule, where there will
     * be no OR-nodes? It works out properly. *)
    (* ASSUME: the andor.info.id is bound in the svarenv argument *)
    fun genAndor (andor, svarenv, varenvMC, k) =
	let fun genNodes (nodes, svarenv, varenvMC, k) =
		  let fun genf (node, (svarenv0, varenvMC0, k0)) =
			    genNode (node, svarenv0, varenvMC0, k0)
		   in foldr genf (svarenv, varenvMC, k) nodes
		  end
	    and genNode (AND {info,children,andKind,...}, svarenv, varenvMC, k) =
                  (case SE.lookSvar (svarenv, #id info)
		    of SOME thisSvar =>
			 let fun collectChildSvars (andor, (svars, svarenv)) =
				 let val svar = MU.infoToSvar info
				  in (svar :: svars, SE.bindSvar(getId andor, svar, svarenv))
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
		  (case SE.lookSvar (svarenv, #id info)
		    of SOME thisSvar =>
			 let val varenvMC' = bindPatVars (thisSvar, info, varenvMC)
			  in case arg
			       of LEAF _ => (svarenv, varenvMC', k)
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
	      | genNode (OR {info,...}, svarenv, varenvMC, k) =
                  (* this OR node is already accounted for in the dectree (if not redundant) *)
		  (case SE.lookSvar (svarenv, #id info)
		    of SOME thisSvar =>
			    let val varenvMC' = bindPatVars (thisSvar, info, varenvMC)
			     in (svarenv, varenvMC', k)
			    end
		    | NONE => bug "genNode:OR: no svar")
	      | genNode (VARS {info, defaults,...}, svarenv, varenvMC, k) =
		  (case SE.lookSvar (svarenv, #id info)
		    of SOME thisSvar =>
		         let val varenvMC' = bindPatVars (thisSvar, info, varenvMC)
			 in (svarenv, varenvMC', k)
			 end
		       | NONE => bug "genNode:VARS: no svar")
	      | genNode (LEAF _, _, _, _) = bug "genNode: LEAF"
	          (* should not happen! LEAF is found only in an OR variant *)
	      | genNode (INITIAL, _, _, _) = bug "genNode: INITIAL"
	 in genNode (andor, svarenv, varenvMC, k)
	end (* genAndor *)

    (* genDecTree: decTree * svarenv * varenvMC -> mcexp *)
    fun genDecTree (decTree, svarenv, varenvMC) =
	(case decTree
	   of MT.CHOICE{node, choices, default} =>
		(case node    (* ASSERT: node must be OR *)
		  of OR{info, variants,...} =>
		     (* ASSERT: choices and variants are "congruent" (same keys
		      * in same order), hence same length *)
		     (case SE.lookSvar (svarenv, #id info)
		       of SOME svar =>
			    let fun switchBody ((key,node0)::nrest, (key',decTree0)::drest, sbody) =
				    (* ASSERT: key = key'.  Verify? *)
				    (* ASSERT?: if decTree0 is CHOICE, then decTree0.node = node0 *)
				    let val (svarOp, decExp) =  (* argument svar + subtree *)
					    (case node0
					      of LEAF _ =>
						   let val decExp0 = genDecTree(decTree0, svarenv, varenvMC)
						    in (NONE, decExp0)
						   end
						| _ =>
						  let val svar0 =
							  (case node0
							    of AND{andKind=VECTOR,...} => svar (* parent *)
							     | _ => MU.andorToSvar node0)
						       val svarenv' = SE.bindSvar(getId node0, svar0, svarenv)
						       val (svarenv'', varenvMC', k_node0) =
							   genAndor(node0, svarenv', varenvMC, k_ident)
						       val decExp0 = genDecTree(decTree0, svarenv'', varenvMC')
						    in (SOME svar0, k_node0 decExp0)
						   end)
				     in switchBody(nrest,drest,(key, svarOp, decExp)::sbody)
				    end
				  | switchBody (nil,nil,sbody) = rev sbody
				  | switchBody _ = bug "code.genDecTree.switchBody"
				val sbody = switchBody(variants, choices, nil)
				val default = Option.map (fn t => genDecTree(t,svarenv,varenvMC)) default
			     in C.Switch (svar, sbody, default)
			    end
			  | NONE => bug "genDecTree:CHOICE: no svar")
		   | _ => bug "genDecTree")
            | MT.DMATCH dtrace => (saveTrace dtrace; C.Failure (matchExn, rhsTy))
	    | MT.DLEAF (rule, dtrace) => genRHS(rule, varenvMC, dtrace, varenvAC))

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
 * Called by transMatch.  fillPat has been applied to rule patterns. *)
and matchComp (rules: AS.rule list, lhsTy: T.ty, rhsTy: T.ty, varenvAC,
	       matchFailExn: T.datacon) =
    let val _ = print ">>> matchComp\n" (* -- debugging *)
	val patterns = map (fn (AS.RULE(pat,_)) => pat) rules (* strip RULE constructor *)
	val andor = AndOr.makeAndor(patterns, lhsTy)
	val _ = if !printAndor
		then ppAndor andor
		else ()
	val (dectree,ruleCounts) = DecisionTree.decisionTree andor
	val _ = if !printDecisionTree
		then ppDecisionTree dectree
		else ()
        val (matchExp, matchVar) =
	    genMatch (rules, andor, dectree, ruleCounts, rhsTy, varenvAC, matchFailExn)
	val _ = if !printMatchAbsyn
		then ED.withInternals (fn () => ppCode matchExp)
		else ()
    in print "<<< matchComp\n";  (* -- debugging *)
       (matchExp, SVar.svarToVar matchVar)
    end

(* ================================================================================ *)
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
		    let (* val _ = print ">>> transExp: CASEexp\n" *)
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
		| VARexp (vref, _) =>
		    (case VarEnvAC.look (venv, !vref)
		       of NONE => exp
			| SOME var => (vref := var; exp))
		| _ => exp)
		  (* (VARexp _ | CONexp _ | NUMexp _ | REALexp _ | STRINGexp _  | CHARexp _ |
		   *  SELECTexp _ | SWITCHexp _ | VSWITCHexp) => exp *)
     in trans exp
    end (* transExp *)

(* transDec : dec -> dec *)
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
	     * -- can we get away without a d (DB depth) parameter? Leaving it to Translate? Think so.
	     * -- looks like we can get away with never dealing with an internal svar in the match.
	     * -- we need to access or (re)construct the type of the pat.
	     *      Could store this as a field of VB.
	     * -- do we need an absyn equivalent to mkPE, say transPolyExp? We don't have an equivalent
	     *      to TFN in absyn -- yet! *)
	    and transVB (VB{pat, exp, typ, boundtvs, tyvars}) =
		(* match compile [(pat,exp)] if pat is nontrivial (not a var);
		 * -- check what the match compiler does with (single) irrefutable patterns
		 *    DONE -- it does the right thing. *)
		((* print "transVB:pat = "; ppPat pat;
		 print "transVB:exp = "; ppExp exp; *)
		 case AU.stripPatMarks pat
		  of (VARpat var | CONSTRAINTpat(VARpat var, _)) =>
		         (* simple variable pattern *)
		      let val exp' = transExp (exp, venv)
	              (* val _ = (print "transVB:exp' = "; ppExp exp') *)
		      in VALdec([VB{pat = VARpat var, exp = exp',
				    typ = typ, boundtvs = boundtvs, tyvars = tyvars}])
		      end
		    | pat =>  (* compound, possibly refutable, pattern *)
		      let val patvars = AU.patternVars pat
			  val patvarstuple =
			        EU.TUPLEexp (map (fn var => VARexp(ref var, nil)) patvars)
			  val patvarstuplety = Tuples.mkTUPLEtype (map V.varType patvars)
			  val (matchExp, matchVar) =
			      matchComp([RULE(pat,patvarstuple)], typ, patvarstuplety, venv,
					EU.getBindExn())
			      (* matchVar will be bound to trans of exp *)
			  val ptupleVar = V.VALvar{path = SP.SPATH [S.varSymbol "topVar"],
						   typ = ref(patvarstuplety),
						   btvs = ref(boundtvs),
						      (* possibly polymorphic *)
						   access = A.LVAR(LambdaVar.mkLvar()),
						   prim = PrimopId.NonPrim}
			  fun selectVBs([], _) = []
			    | selectVBs (pvar::pvars, n) = (* non-polymorphic *)
				simpleVALdec(pvar, SELECTexp(ptupleVar,n,true), nil)
				  :: selectVBs(pvars, n+1) 
				(* defining a pattern var by (record) selection from a
                                 * var (ptupleVar) bound to the tuple of all the pattern
                                 * var values; the btvs of each pattern var is a subset
                                 * of the btvs of ptupleVar. *)
		      in SEQdec (simpleVALdec(matchVar, transExp (exp, venv), nil) ::
				 simpleVALdec(ptupleVar, matchExp, boundtvs) ::
				 selectVBs(patvars, 0)) (* rebinding orig pattern variables *)
		      end)

	    (* NOTE: Given the way Translate.transDec deals with LOCALdec (i.e. not
             * hiding the local decls), we can just have a single SEQ encompassing all the
             * declarations. *)

	    (* transVBs : AS.vb list -> AS.dec *)
	    and transVBs nil = bug "transVBs: nil"  (* expect at least one *)
	      | transVBs [vb] = transVB vb
	      | transVBs vbs = SEQdec (map transVB vbs)

	    and transRVB (RVB{var: V.var, exp: exp, boundtvs: T.tyvar list,
			      resultty: T.ty option, tyvars: T.tyvar list ref}) =
		(print "transRVB:var = "; print (Symbol.name (V.varName var)); print "\n";
		RVB {var = var, exp = transExp (exp,venv), boundtvs = boundtvs,
		     resultty = resultty, tyvars = tyvars})

	in transDec0 dec
    end (* transDec *)

fun transMatchDec dec = transDec (dec, VarEnvAC.empty)

end (* local *)
end (* structure MatchComp *)

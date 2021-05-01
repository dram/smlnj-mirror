(* matchcomp/generate.sml *)

(* generating absyn "code" for a match *)
(* matchcomp.sml *)

(* This is the "code-generation" phase of match compiler as modelled on the revmc version.
 * The "code" (Absyn.exp) for a match is generated from the decision tree and its associated andor
 * (used for record/vector destruction and variable bindings).
 * This code performs pattern dispatching and destruction and then invokes the appropriate
 * RHS expressions with the necessary variable bindings. Types are needed for the "administrative"
 * mvars that are introduced.  These are computed by deconstructing the pattern type of the match.
 *
 * The "code" is in the form of an Absyn.exp.  Absyn.exp has been augmented with two new
 * expression forms: SWITCHexp and VSWITCHexp, which are created only by the match
 * compilation (i.e. never by direct elaboration of source code). Match compilation is
 * invoked (on "typed" absyn decs) after type checking, so it is responsible for "maintaining"
 * the correct type information in the translated absyn, which should remain well-typed.
 *)

structure Generate =
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

  fun bug msg = ErrorMsg.impossible ("Generate: " ^ msg)

  val printAndor = ElabControl.printAndor
  val printDecisionTree = ElabControl.printDecisionTree
  val printMatchAbsyn = ElabControl.printMatchAbsyn
  val debugging = ElabControl.mcdebugging
  val stats = ElabControl.stats

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

(* new absyn-generator based on revmc version of generate.sml
 *  1. new approach: assume rhs of rules are processed _before_ the match is translated,
 *  so we are only concerned with the patterns (as in revmc and oldmc).
 *  2. match information is passed in the form of pvarmap and ruleMap parameters.
 *     pvarmap maps bound pattern variables (pvar) to list of ruleno * nodeId pairs
 *        that determine the "locations" where the variable occurs (there can be
 *        multiple occurrences because of the ramification of or-patterns
 *        [it is assumed that pattern variables occurring in different (original) rules
 *        are distinct, even if they have the same "name" symbol.]
 *     ruleMap: maps rulenos (expanded) to 
 *        a) list of pvars bound in that rule's pattern (shared by all rules in a ramified family)
 *        b) variable to which the rhs function will be bound (also shared)
 *        c) original ruleno (before expansion)
 *  3. _all_ rhs expressions are abstracted to rhs functions, the defns of these functions
 *     are wrapped around the match expression itself. So no rhs expressions are "inlined".
 *     this should insure that alpha-conversion of pattern variables is not required -- they
 *     will only be bound once in the abstraction of the corresponding rhs exp.
 *)

(* Singleton constructors are not treated in any special way. They still need to be destructed,
 * and the general case handles them -- they just have a single variant case in the switch.
 * Special constructors like ref and susp are handled by Translate, so they don't need to be
 * treated specially here (check?).
 * SWITCHexp translates almost directly to Plambda.SWITCH.
 *
 * Also need to deconstruct AND-structure _below_ a terminal OR/Decision node,
 * since variables may occur below the node. This is done by the call of
 * genAndor within the body of genDecTree.
 *)

(* fun k_ident (x: AS.exp) = x  -- not using exp-continuations *)

val choiceTotalThreshold = 10

fun reportStats (nodeCount: int, {rulesUsed, failures, choiceTotal, choiceDist}: DT.decTreeStats) =
    if !stats andalso choiceTotal > choiceTotalThreshold  (* don't report stats for smallish matches *)
    then (say "decTree Stats: \n";
	  says ["  nodeCount =   ", Int.toString nodeCount];
	  says ["  choiceTotal = ", Int.toString choiceTotal];
	  newline())
    else ()


(* lvar analogue of mvarenv *)
structure M = IntBinaryMap

(* mvarenv: a finite mapping from AndOr node id numbers (andor.info.id) to
 * "mvars", which are lvars used as "administrative" variables in the match compiler
 * to represent values produced during value destruction. The do not derive from the pattern variables,
 * but in the end each pattern varialbe (for a given rule) will be associated with some mvar, which
 * in turn will be bound do the pattern variable's "matching value". *)
type mvarenv = LV.lvar M.map

val emptyMvarenv = M.empty

(* bindMvar : nodeId * LV.lvar * mvarenv -> mvarenv *)
fun bindMvar (id: int, mvar: LV.lvar, env: mvarenv) =
    M.insert(env, id, mvar)

(* lookMvar : mvarenv * nodeId -> LV.lvar option *)
fun lookMvar (env: mvarenv, id: nodeId) = M.find (env, id)

(* lookMvar' : mvarenv * nodeId -> LV.lvar *)
(* a version of lookMvar that is expecting to succeed *)
fun lookMvar' (env: mvarenv, id: nodeId) =
    case M.find (env, id)
      of NONE => bug "lookMvar'"
       | SOME mvar => mvar

fun andorMvar (mvarenv, andor) =
    lookMvar' (mvarenv, getId andor)


(* simpleVALdec : V.var * AS.exp * T.tyvar list -> AS.dec *)
(* used in Translate.transDec/transVB *)
fun simpleVALdec (var, exp, boundtvs) =
    VALdec [VB{pat = VARpat var, exp = exp,
	       typ = V.varType var, boundtvs = boundtvs, tyvars = ref nil}]


type failInfo = T.datacon * T.ty  (* exception datacon and "result" type *)

(* genMatch : MT.andor * DT.decTree * pvarmap * ruleMap * failInfo -> AS.exp * V.var *)
(* top level code generating function for matches (formerly MCCode.genCode) *)
fun genMatch (andor, decTree, pvarmap, ruleMap, fail) =
    let val _ = dbsay ">> genMatch"

    fun sameVar (var : V.var, AS.VARexp(ref var',_)) = V.eqVar (var, var')
      | sameVar _ = false

     (* findId : Andor.pvarmap * V.var * ruleno -> nodeId option *)
     fun findId (pvarmap: Andor.pvarmap, pvar, ruleno) =
	 (* pvar, ruleno --pvarmap--> nodeId --mvarenv--> mvar *)
	 (case Andor.pvarFind (pvarmap, V.varToLvar pvar)
	    of NONE => NONE
	     | SOME rule_ids => 
	       let fun look ((ruleno',id)::rest) =
		       if ruleno' = ruleno then SOME id
		       else look rest
		     | look nil = NONE
	       in look rule_ids
	       end)

     (* genRHS : ruleno * mvarenv * pvarmap -> AS.exp
      *  invoking the rule RHS function (abstracted rhs) on pattern variable mvars *)
     fun genRHS (ruleno, pvarmap, mvarenv) =
	   let val (pvars, fvar, _) = ruleMap ruleno
	       fun lookupPvar (pvar: V.var) : AS.lexp = 
		    (case findId (pvarmap, pvar, ruleno)
		       of NONE => bug "lookupPvar: (pvar, ruleno) not found in pvarmap"
			| SOME id => AS.VARexp (lookMvar' (mvarenv, id)))
	    in case pvars
	         of [pvar] => AS.APPexp (AS.VARexp fvar, lookupPvar pvar)
		  | pvars =>  (* multiple bound pvars, including none *)
		      PL.APP(PL.VAR fvar, AU.mkTupleExp (map lookupPvar pvars))
	   end

    (* makeRHSfun : ruleno -> AS.exp option
     *  part of the need to alpha-convert vars is to ensure that the FLINT invariant of
     *  lvars only being bound once is maintained (in particular, use of VarEnvAC.alphaEnv). *)
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
			 val fnbody = transExp (("makeRHSfun:"^Int.toString ruleno), rhs,
						VarEnvAC.append (venv, varenvAC))
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

    (* genRHS : layer * varenvMC * varenvAC -> AS.exp *)
    fun genRHS (layer, varenvMC: VarEnvMC.varenvMC, externVarenvAC) =
	let val ruleno = L.toRule layer
	    val patvars = ruleBoundVars ruleno
	    val localVarenvAC = bindSVars (patvars, layer, varenvMC)
	    val svars = VarEnvAC.range localVarenvAC
	    val venv = VarEnvAC.append (localVarenvAC, externVarenvAC)
	    (* val _ = (print "genRHS:venv = "; VarEnvAC.printVarEnvAC venv; print "\n") *)
	 in case Array.sub(rhsFuns, ruleno)
	      of SOME (fvar, _) => C.Sapp (fvar, svars)  (* applies the rhs fun *)
	       | NONE => transExp ("genRHS:"^Int.toString ruleno, ruleRHS ruleno, venv)
	          (* match svars substituted for source vars directly into a single-use rhs *)
	end

    (* savedTraces : trace list ref
     * This variable saves traces that are found at DMATCH nodes.
     * These can be used to generate counterexamples for non-exhaustive matches. *)
    val savedTraces = ref (nil : path list list)
    fun saveTrace dtrace = (savedTraces := dtrace :: !savedTraces)

(* this is done in makeAndor as it produces the pvarmap 
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
*)

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

end (* structure Generate *)

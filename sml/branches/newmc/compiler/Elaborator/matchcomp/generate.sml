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
  structure LV = LambdaVar
  structure A = Access
  structure V = VarCon
  structure AS = Absyn
  structure AU = AbsynUtil
  structure EU = ElabUtil
  structure MC = MCCommon  (* ~ MCTypes *)
  structure MU = MCUtil
  structure DT = DecisionTree
  structure PP = PrettyPrint
  structure PU = PPUtil
  structure ED = ElabDebug
  open Absyn MCCommon

  (* printing for matchComp *)

  fun bug msg = ErrorMsg.impossible ("Generate: " ^ msg)

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

  fun mkv () = LV.mkLvar ()

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


(* mvarenv: environment mapping node ids (nodeId == int) to variables (V.var) *)
structure M = IntBinaryMap

(* mvarenv: a finite mapping from AndOr node id numbers (andor.info.id) to
 *  "mvars", which are "administrative" variables (V.var) used in the match compiler
 *  to represent "intermediate" values produced during value destruction. The do not derive
 *  from the pattern variables, but in the end each pattern variable _occurrence_ 
 *  (for a given node and rule rule) will be associated with some mvar, which
 *  in turn will be bound (dynamically) to the pattern variable's "matching value". *)
type mvarenv = V.var M.map

val emptyMvarenv = M.empty

(* bindMvar : nodeId * V.var * mvarenv -> mvarenv *)
fun bindMvar (id: int, mvar: V.var, env: mvarenv) =
    M.insert(env, id, mvar)

(* lookMvar : mvarenv * nodeId -> V.var option *)
fun lookMvar (env: mvarenv, id: nodeId) = M.find (env, id)

(* lookMvar' : mvarenv * nodeId -> V.var *)
(* a version of lookMvar that is expecting to succeed *)
fun lookMvar' (env: mvarenv, id: nodeId) =
    case M.find (env, id)
      of NONE => bug "lookMvar'"
       | SOME mvar => mvar

(* andorVar : mvarenv * andor -> V.var *)
fun andorVar (mvarenv, andor) =
    lookMvar' (mvarenv, getId andor)


(* simpleVALdec : V.var * AS.exp * T.tyvar list -> AS.dec *)
(* used in Translate.transDec/transVB *)
fun simpleVALdec (var, exp, boundtvs) =
    VALdec [VB{pat = VARpat var, exp = exp,
	       typ = V.varType var, boundtvs = boundtvs, tyvars = ref nil}]


type failInfo = T.datacon option * T.ty  (* optional exception datacon and "result" type *)

(* genMatch : MC.andor * DT.decTree * pvarmap * ruleMap * failInfo -> AS.exp * V.var *)
(* top level code generating function for matches (formerly MCCode.genCode) *)
fun genMatch (andor, decTree, pvarmap, ruleMap, (failExnOp, resTy)) =
    let val _ = dbsay ">> genMatch"

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
      *  lexp invoking the rule RHS function (abstracted rhs) on the mvars corresponding to
      *  the pattern bound variable(s) *)
     fun genRHS (ruleno, pvarmap, varenv) =
	   let val (pvars, fvar, _) = ruleMap ruleno
	       val fvarExp = VARexp (ref fvar, nil)
	       fun lookupPvar (pvar: V.var) : AS.exp = 
		    (case findId (pvarmap, pvar, ruleno)
		       of NONE => bug "lookupPvar: (pvar, ruleno) not found in pvarmap"
			| SOME id => VARexp (ref(lookMvar' (varenv, id)), nil))
	    in case pvars
	         of [pvar] => AS.APPexp (fvarExp, lookupPvar pvar)
		  | pvars =>  (* multiple bound pvars, including none *)
		      AS.APPexp(fvarExp, AU.TUPLEexp (map lookupPvar pvars))
	   end


    (* bindAndorIds : andor * mvarenv -> mvarenv
     * wrapBindings : lexp * andor * mvarenv -> lexp
     * Translates top non-OR structure, i.e. AND structure, (if any) into nested let
     * expressions that are wrapped around a body expression (generated by a decision tree)
     * to "destruct" that structure and bind its components to fresh "mvars" (lvars used
     * as Match Compiler administrative variables. The association of mvars to andor tree
     * locations (represented by nodeIds) is represented by mvarenv (an id --> mvar mapping).
     * The wrapped let expressions are accumulated in a "continuation", k: exp -> exp
     * that will be applied to the expression generated for the next chosen decision tree
     * It is applied to the root node of an andor tree, and also to the variant
     * nodes under an OR choice.
     * What happens in the case of a single, irrefutable rule, where there will
     * be no OR-nodes? Claim that it works out properly. (Explain, details?) *)
    (* ASSUME: The andor nodeId is already bound to an mvar in the mvarenv argument. *)

    (* newVar : nodeId * T.ty -> V.var *)
     fun newVar (id, typ) =
	 V.newVALvar (Symbol.varSymbol ("n" ^ Int.toString id), typ)
    (* bindNodes : andor  list * mvarenv -> mvarenv *)
    fun bindNodes (andors, mvarenv) = foldr bindAndorIds mvarenv andors

    (* bindAndorIds : andor * mvarenv -> mvarenv
     *  binds fresh mvars to all the node ids in andor argument, stopping at OR and VAR nodes,
     *  i.e. the "upper subtree" consisting of all the "AND-structural subnodes" are bound
     *  to fresh mvars.
     * ASSERT: the andor ids are not bound in the argument mvarenv *)
    and bindAndorIds (AND {id, children, typ}, mvarenv) =
	  (dbsays [">> bindAndorIds:AND: ", Int.toString id];
	   bindNodes (children, bindMvar(id, newVar(id, typ), mvarenv)))
      | bindAndorIds (OR {id, typ, ...}, mvarenv) = 
          (dbsays [">> bindAndorIds:OR: ", Int.toString id];
	   bindMvar (id, newVar(id, typ), mvarenv))
      | bindAndorIds (VAR {id, typ}, mvarenv) =
          (dbsays [">> bindAndorIds:VAR: ", Int.toString id];
	   bindMvar (id, newVar(id, typ), mvarenv))

    (* wrapBindings : AS.exp * andor * mvarenv -> AS.exp *)
    fun wrapBindings (exp, andor, varenv) = 
	(case andor
	  of AND {id, children, ...} =>
	       let val thisVar = lookMvar' (varenv, id)
		   val childrenVars = map (fn node => andorVar (varenv, node)) children
		   fun wrap (andor, lexp) = wrapBindings (lexp, andor, varenv)
		   val exp' = foldr wrap exp children
	       in MU.mkLetr (childrenVars, thisVar, exp')
	       end
	   | _  => exp)

    (* wrapBindingsList : AS.exp * andor list * mvarenv -> AS.exp *)
    fun wrapBindingsList (lexp, andors, mvarenv) =
	foldr (fn (andor, le) => wrapBindings (le, andor, mvarenv)) lexp andors

    val varenv0 = bindAndorIds (andor, emptyMvarenv)
    val rootVar = andorVar (varenv0, andor)

    (* savedTraces : trace list ref
     * This variable saves traces that are found at DMATCH nodes.
     * These can be used to generate counterexamples for non-exhaustive matches.
    val savedTraces = ref (nil : path list list)
    fun saveTrace dtrace = (savedTraces := dtrace :: !savedTraces)
    *)

    (* genDecTree: decTree * varenv -> AS.exp *)
    (* test with ?/t6.sml *)
    fun genDecTree (decTree, varenv) =
	(case decTree
	   of MC.CHOICE{andor = OR {id, cases = cases_OR, ...}, sign, cases = cases_CHOICE, default} =>
		 (* cases and variants should be "congruent", since choices is
		  * created as a Variants.map over variants, but we insure coordination
		  * by defining aoVariants and dtVariants bellow, which will be
		  * guaranteed to have keys in the same order. *)
		let val var = lookMvar' (varenv, id)
		    (* genCase: variant * (MC.con * dectree) -> (MC.con * var option * AS.exp) *)
		    fun genCase ((con, _, subcase), (con', decTree0)) =
			 (* ASSERT: con = con' -- the variant lists should be coordinated *)
			(if not (conEq (con, con'))  (* verifying ASSERT above *)
			 then bug "genDecTree:CHOICE:keys disagree"
			 else ();
			 (* say (concat ["genDecTree.switchBody:con = ", conToString con, "\n"]); *)
			 (case subcase   (* variant node assoc. with con *)
			    of CONST =>  (* no destruct, no new values to be bound *)
				 (con, NONE, genDecTree(decTree0, varenv))
			     | VEC elements => (* => con = VLENcon(len,ty) *)
				 (* var is bound to the vector value *)
				 let val varenv1 = bindNodes (elements, varenv)
					 (* AND-destruct the vector elements, binding new vars *)
				     val baseExp = genDecTree (decTree0, varenv1)
				     val exp' = wrapBindingsList (baseExp, elements, varenv1)
				     val elementVars =
					 map (fn andor => andorVar (varenv1, andor))
					     elements
				     val exp'' =
					 (case con
					    of MC.VLENcon (_,elemty) => 
					       MU.mkLetv (elementVars, var, exp' (*, elemty ???*))
					     | _ => bug "genDecTree: expected VLENcon")
				  in (con, SOME var, exp'')
				     (* var is bound to the vector itself, inherited from OR *)
				 end
			     | DCON argNode =>  (* con is non-constant datacon *)
				 let (* destruct node0, binding new vars *)
				     val varenv1 = bindAndorIds (argNode, varenv)
					 (* add bindings for argNode AND-structure *)
				     val argVar = andorVar (varenv1, argNode)
					 (* bound to datacon argument *)
				     val baseLexp = genDecTree(decTree0, varenv1)
				     val caselexp = wrapBindings (baseLexp, argNode, varenv1)
				  in (con, SOME argVar, caselexp)
				 end))
		    val switchCases = ListPair.mapEq genCase (cases_OR, cases_CHOICE)
		    val defaultOp = Option.map (fn dt => genDecTree (dt, varenv)) default
		 in MU.mkSwitch (var, switchCases, defaultOp)
		     (* switch detects and handles the vector length case *)
		end
            | MC.FAIL =>
		let val failExp = 
			(case failExnOp
			  of NONE => AS.VARexp(ref rootVar, nil)
			   | SOME datacon => AS.CONexp(datacon, nil))
		 in AS.RAISEexp (failExp, resTy)
		end
	    | MC.RHS ruleno => genRHS (ruleno, pvarmap, varenv)
	        (* invoke (i.e. call) the appropriate rhs function for this ruleno *)
	    | _ => bug "genDecTree: CHOICE andor not an OR node"
	(* end case *))
        (* end genDecTree *)

    val dtLexp = genDecTree (decTree, varenv0)
    val lexp = wrapBindings (dtLexp, andor, varenv0)

 in (lexp, rootVar)
end (* fun genMatch *)

end (* top local *)
end (* structure Generate *)

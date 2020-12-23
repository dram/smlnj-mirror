(* mc-code.sml *)

(* The "code" (Absyn.exp) for the match is generated from the decision tree and information
 * from the original andor (about record deconstruction and variable bindings (and types).
 * This code performs pattern dispatching and deconstruction and then invokes the appropriate
 * RHS expressions with the necessary variable bindings.
 *
 * The "code" (at least initially) will be a neutral minimal abstract code (perhaps similar
 * to the Lambda IR in the early compiler (FN, APP, SELECT, SWITCH, etc.).  Types and type
 * variables are taken into account.  Goal is to see if the "code" language can be replaced by
 * (or translated into) Absyn (perhaps modified appropriately.
 *)

structure MCCode =
struct

local
   structure T = Types
   structure AS = Absyn
   structure R = Rules
   structure SV = SVar
   structure MT = MCTypes
   structure MU = MCUtil
   structure M = RuleVarMap
   structure DT = DecisionTree
   structure C = VMCexp
   open Absyn MCTypes
in

(* How should we treat SINGLE constructor patterns, and in particular the
 * "special" ones like *ref* (and *susp* )?  We generate a special single
 * datacon "deconstructor" (expressed as Case) in these cases. The special cases (ref,susp)
 * are detected and handled in Translate (FLINT/trans/translate.sml). It turns
 * out that Case translates almost directly to Plambda.SWITCH.
 *
 * Also need to deconstruct AND and SINGLE _below_ a terminal OR/Decision node,
 * since variables may occur below the node. This is done by the call of
 * genNode within the body of genDec.
 *)

(* code : R.rules * MT.andor * DT.decTree * int vector * T.ty * T.datacon -> AS.exp *)
(* top level code generating function *)
fun code (rules: AS.rule list, andor, decTree, ruleCounts, rhsTy, matchExn) =
let val rules = map (fn AS.RULE r => r) rules
	  (* : (AS.pat * AS.exp) list -- strip the RULE constructor *)

    fun multiple ruleno = Vector.sub(ruleCounts, ruleno) > 1

    val ruleVarMap = M.makeRuleMap andor  (* ruleno --> (V.var * SV.var) list *)

    val multirules = List.filter multiple (List.tabulate (length rules, (fn n => n)))
        (* list of rule numbers with multiple uses *)

    val funSvars = map (fn r => SV.newSvar("rhsfun"^(Int.toString r), T.UNDEFty)) multirules
        (* produce svars to name rhs functions for rules in multirules *)

    fun findFunVar n =  (* n in multirules *)
	let fun loop (rule::rules, svar::svars) =
	          if n = rule then svar
	          else loop (rules, svars)
	      | loop (nil, nil) = bug "findFunVar: n out of scope"
	      | loop _ = bug "findFunVar: lengths of rules, svars differ"
	 in loop (multirules, funSvars)
	end

    (* ruleRHS : ruleno -> AS.exp *)
    fun ruleRHS n = #2 (List.nth (rules, n))
          (* : AS.exp -- rhs expression of rule n, already match translated *)

    fun wrapFn (n, exp) =
	let val varmap = M.lookup (ruleVarMap, n)
	    val paramvars = MU.uniqueVars (map #1 varmap)
		 (* get the fn parameter svars from varmap *)
	 in C.Letf(findFunVar n, C.Sfun(paramvars, ruleRHS n), exp)
	end

    (* wrap the rhs fn bindings around the base match expression *)
    fun genPrelude inner = foldr wrapFn inner multirules


    (* allConsistent : path * path list -> bool *)
    fun allConsistent (varpath, dtrace: path list) =
	List.all (MU.consistentPath varpath) dtrace

    (* filterSVars : trace * varmap -> (var * var) list *)
    fun filterSVars (rhsTrace, varmap) =
	let fun f ((var, path, svar)::rest) = 
		if allConsistent (path, rhsTrace)
		then (var,svar) :: f rest
		else f rest
	      | f nil = nil
	in f varmap
	end

    (* genRHS : (AS.pat * AS.exp) * MT.path -> AS.exp *)
    fun genRHS (rule, rhsTrace)  =
	let val varmap0 = M.lookup(ruleVarMap, rule)  (* : (V.var * path * SV.svar) list *)
	    val varmap = filterSVars (rhsTrace, varmap0)
	    val rhsExp = ruleRHS rule
	 in if multiple rule
	    then C.Sapp(findFunVar rule, map #2 varmap)  (* uses a rhs fun *)
	    else (MCUtil.substVars (rhsExp, varmap); rhsExp)
	      (* match svars substituted directly into a unique rhs exp *)
	end

    (* savedTraces : trace list ref
     * This variable saves traces that are found at DMATCH nodes. To be used
     * to generate counterexamples for non-exhaustive matches. *)
    val savedTraces = ref (nil : path list list)
    fun saveTrace dtrace = (savedTraces := dtrace :: !savedTraces)

    (* genNode: andor -> mcexp -> mcexp *)
    (* translates top non-OR structure (if any) into nested let expressions
     * wrapped around a body expression "inner". Applies to root node
     * of an andor tree, and also applied to the child nodes under an OR choice.
     * What happens in the case of a single, irrefutable rule, where there will
     * be no OR-nodes? It works out properly. *)
    fun genNode (andor, inner) =
	let fun genNodes (node::nodes, inner) =
		(* genNodes: andor list * mcexp -> mcexp -- for AND children *)
		  genTop(node, genNodes(nodes, inner))
	      | genNodes (nil,inner) = inner
	    and genTop (AND{svar,children,andKind,...}, inner) =
		  let val svars = map getSvar children
		      val body = genNodes(children, inner)
		   in case andKind
		        of RECORD => C.Letr (svars, svar, body)
		         | VECTOR => C.Letv (svars, svar, body)
		  end
	      | genTop (SINGLE{svar, key, arg, ...}, inner) =
		  (case arg
		    of LEAF _ => inner (* constant dcon *)
		     | _ => C.Switch (svar, [(key, SOME(getSvar arg), genTop(arg,inner))],
				      NONE))
	      | genTop (OR _, inner) = inner
	      | genTop (VARS{defaults,...}, inner) = inner (* genRHS (R.minItem defaults) *)
	      | genTop (LEAF _, _) = bug "genNode: LEAF"
	          (* should not happen! LEAF is found only _below_ an OR *)
	      | genTop (INITIAL, _) = bug "genNode: INITIAL"
	 in genTop(andor, inner)
	end

    (* genDec: decTree -> mcexp *)
    fun genDec (decTree) =
	(case decTree
	   of MT.CHOICE{node, choices, default} =>
		(case node    (* ASSERT: node must be OR *)
		  of OR{svar, variants,...} =>
		     (* ASSERT: choices and variants are "congruent" (same keys
		      * in same order), hence same length *)
		     let fun switchBody ((key,node0)::nrest, (key',decTree0)::drest, sbody) =
			    (* ASSERT: key = key'.  Verify? *)
			     let val decCode0 = genDec decTree0
				 val (svarOp, decCode) =
				     (case node0
					of LEAF _ => (NONE, decCode0)
					 | _ => (SOME(getSvar node0),
						 genNode(node0, decCode0)))
			      in switchBody(nrest,drest,(key, svarOp, decCode)::sbody)
			     end
			   | switchBody (nil,nil,sbody) = rev sbody
			   | switchBody _ = bug "code.genDec.switchBody"
			 val sbody = switchBody(variants, choices, nil)
			 val default = Option.map genDec default
		      in C.Switch (svar, sbody, default)
		     end
		   | _ => bug "genDec")
            | MT.DMATCH dtrace => (saveTrace dtrace; C.Failure (matchExn, rhsTy))
	    | MT.DLEAF (rule, dtrace) => genRHS(rule, dtrace))

 in genPrelude(genNode(andor, genDec decTree))

end (* fun code *)

end (* local *)
end (* structure MCCode *)

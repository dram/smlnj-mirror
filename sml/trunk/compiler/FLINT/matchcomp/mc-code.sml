(* mc-code.sml *)

(* The "code" for the match is generated from the decision tree and information from
 * the original andor (about record deconstruction and variable bindings (and types).
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
   structure A = Absyn
   structure R = Rules
   structure SV = SVar
   structure M = RuleVarMap
   open MCTypes DecisionTree
in

(* code : rules * andor * decTree * int vector * typevar list -> mcexp *)
(* top level code generating function *)
fun code (rules: (A.pat * A.exp) list, andor, decTree, ruleCounts, tyvars) =
let fun multiple ruleno = Vector.sub(ruleCounts, ruleno) > 1
    fun ruleRHS n = #2(List.nth(rules,n))
    val ruleVarMap = M.makeRuleMap andor
    val multirules = List.filter multiple (List.tabulate (length rules, (fn n => n)))
    val funSvars = map (fn r => SV.newSvar("rhsfun"^(Int.toString r), T.UNDEFty)) multirules
    fun wrapFn (n, exp) =
	let val paramSvar = SV.newSvar("frule"^(Int.toString n), T.UNDEFty)
	 in Letf(List.nth(funSvars,n), Sfun(map #1 (M.lookup(ruleVarMap,n)), ruleRHS n), exp)
	end
    fun bindTyvars exp =
	(case tyvars
	  of nil => exp
	   | _ => Tfun(tyvars, exp))
    fun genPrelude inner =
	bindTyvars (foldr wrapFn inner multirules)
    fun genRHS rule =
	let val varmap = M.lookup(ruleVarMap, rule)
	 in if multiple rule
	    then Sapp(List.nth(funSvars,rule), map #2 varmap)
	    else Letm(map #1 varmap, map #2 varmap, ruleRHS rule)
	end
	     
    (* genNode: andor -> mcexp -> mcexp *)
    (* translates top AND structure (if any) into nested Letr expressions
     * wrapped around a body expression "inner". *)
    fun genNode (andor, inner) =
	let fun genAND (node::nodes, inner) =
		(* genAND: andor list * mcexp -> mcexp *)
		(case node
		  of AND{svar,children,...} =>
		     let val svars = map getSvar children
		     in Letr (svar, svars, genAND(children,(genAND(nodes, inner))))
		     end
		   | _ => genAND(nodes,inner))  (* skip OR, VARS, LEAF *)
	      | genAND (nil,inner) = inner
	in (case andor
	     of OR _ => inner  (* top node is OR, hence first OR-node choice *)
	      | VARS _ => genRHS(R.minItem(getLive andor))
	      | AND _ => genAND ([andor], inner)
	      | SINGLE{arg,...} => genAND([arg], inner)   (* BUG!!! -- need to strip dcon *)
	      | LEAF _ => bug "genNode: LEAF"
	          (* should not happen! LEAF if found only _below_ an OR *)
	      | _ => bug "genNode")
	end

    (* genDec: decTree * mcexp -> mcexp *)
    (* need to pass a var-type map and insert types for variables bindings *)
    fun genDec (decTree) =
	(case decTree
	   of CHOICE{node, choices, default} =>
	     (case node    (* ASSERT: node must be OR *)
	       of OR{svar, variants,...} =>
		  (* ASSERT: choices and variants are "congruent" (same keys
		   * in same order) *)
		  let fun switchBody ((key,node0)::rest, (key',decTree0)::rest', sbody) =
			 (* ASSERT: key = key'.  Verify? *)
			  let val svarOp =
				  (case node0
				    of LEAF _ => NONE
				     | _ => SOME(getSvar node0))
			      val decCode =
				  (key, svarOp, genNode(node0, genDec decTree0))
			  in switchBody(rest,rest',decCode::sbody)
			  end
			| switchBody (nil,_,sbody) = rev sbody
			| switchBody _ = bug "genDec.switchBody"
		      val sbody = switchBody(variants, choices, nil)
		      val default =
			  case default
			   of NONE => NONE
			    | SOME dt => SOME (genDec dt)
		  in Case(svar, sbody, default)
		  end
		| _ => bug "genDec")
	   | DMATCH => MATCH
	   | DLEAF rule => genRHS rule)
              
in
    genPrelude(genNode(andor, genDec decTree))
end (* fun code *)

end (* local *)
end (* structure MCCode *)

    
(* Notes (preliminary)
-----------------------
When we are generating code for a decTree D (at node N), the surrounding
structure for D has been "destructed" to provide a context that, in particular
has a binding for Var(Node(D)).  During dynamic matching, Var(D) will be bound to
the value component being matched to decTree.
 
When an AND node is destructed, we get a Letr binding of all the variables (lvars) of
the component nodes of the AND.  We need to remember that these have been bound
(and are still in scope?) when we need to use one of those variables for some
decision. Their scope os the body of the Letr exp.

So at each subexpression we can keep track of which variables are in scope
at that subexpression (a set of lvars).  Veriables for the nodes of variants
are bound in branches of Case0 expressions.  These are in scope only in the
exp of the corresponding switch branch.

How much of the top of the pattern space has been destructured before we
deal with the first decTree node (OR node), D?  I must be at least enough to
bind Var(Node(D)).  At the top level, the whole value being matched is bound
to vtop.
*)

(* (1) find root andor node of dtree and construct code to access that node
   (2) generate Case0 for root of dtree
       (2a) for each decVariant for that root, "find" arg component corresponding
            to the associated variant decTree (meaning construct access code).

 * Given a decTree node (and associated andor node and its path), need two bind a variable
   to the correspoinding value component (using a nesting of letv bindings).
 * Among the existing variable bindings, which is closest along a path from vtop (the
   root variable bound to the entire argument value) to the path to the target..

 * given a path (of the next decTree node), find the nearest (lowest) variable
   bound along that path and construct access to bind a variable to the given path.

   Example:  decTree D @ p0 where p0 = [k1, k2, k3, k4, k5, k6]
    The case0 code for D must be placed in a mcexp context that "unravels" the
    path p0 and binds a variable v_p0 to the arg value at p0 (val_p0).
    Maybe this is represented as a kind of "continuation", or "context" expression?

    Suppose a variable v is bound at k3 (has path [k1, k2, k3]) and it is the "closest"
    variable on the path, i.e. there are no variables bound at k4, k5, k6.
    Suppose k4, k5, k6 are R1.D(true).R2   (k4 = R1, k5 = D(true), k6 = R2)
    The D(true) key on this path means there has been a previous decTree at the
    path p1 = [k1, ..., k4], with true as one of its keys. The OR node at p1 "dominates"
    the OR node underlying the decTree at  

    letv (v1,v2) = v (the variable at k3)
       Case0 v1
         true =>   (bind a variable here? No, because of nullary key true.)
   
    If 

Example:
AND(a1,a2,a3)
  
let vtop = arg
letv (v1,v2,v3) = vtop
    (v1: R1, v2: R2, v3: R3)

get dtree
dtree.node.path = R1  --> v3 = vtop.R1

    Case0(v1, branches)
    variants = R1.variants = (k1, a11) :: (k2, a12) :: arest
       branch1 = (k1, dt1) :: dtrest1
       k1 => code(a11,dt1)
       branch2 = (k2, dt2):: dtrest2  (where dtrest1 = (k2,dt2)::dtrest2)
       k2 => code(a12,dt2)


what if dt_root (root node of decTree) has path R1.R3?

   letv (v1,...) = vtop
    letv (w1,w2,w3) = v1   (w3 = vtop.R1.R3)
      case0 w3
       etc. ....

auxiliary info

   path -> variable (for already bound variables)
   variable -> path

(for "visible top-and structure")

for each variable binding in the code, can record path for the variable.
 
given a path, can produce a path relative to some existing bound variable

*)

(* CONJECTURE: if N1 is an ancestor of N2, then Var(N1) is in scope at the
"(code) position" of N2. *)

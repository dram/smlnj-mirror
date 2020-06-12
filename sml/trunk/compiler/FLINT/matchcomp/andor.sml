(* makeandor.sml *)

(* andor.sml *)

structure AndOr =
struct

local
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure IC = IntConst
  open Absyn MCTypes
  (* also used: IntInf, Target, Char, String *)
in

fun bug msg = Errormsg.impossible msg

val newLvar = LambdaVar.mkLvar

(* numCon : IntInf.int * int -> constCon *)
fun numCon (v, ty) =
    let fun mkWORD sz = WORDconst(IC.WORDconst {ival = v, ty = sz})
	fun mkINT sz = INTconst(IC.INTconst {ival = v, ty = sz})
    in
	if TU.equalType(ty, BT.intTy)
	  then mkINT Target.defaultIntSz
	else if TU.equalType(ty, BT.int32Ty)
	  then mkINT 32
	else if TU.equalType(ty, BT.int64Ty)
	  then mkINT 64
	else if TU.equalType(ty, BT.intinfTy)
	  then mkINT 0
	else if TU.equalType(ty, BT.wordTy)
	  then mkWORD Target.defaultIntSz
	else if TU.equalType(ty, BT.word8Ty)
(* QUESTION: perhaps we should preserve the size (e.g., in the case of
 * word8) for better jump tables? *)
	  then mkWORD Target.defaultIntSz
	else if TU.equalType(ty, BT.word32Ty)
	  then mkWORD 32
        else if TU.equalType(ty, BT.word64Ty)
          then mkWORD 64
	else bug "numCon"
    end

(* charCon : string -> char; string assumed to be of size = 1 *)
fun charCon s = String.sub (s, 0)

(* addAsBinding : binding * andor -> andor *)
fun addAsBinding (b, AND{path,asvars,vars,live,defaults,children}) =
    AND{path=path,asvars = b::asvars,vars=vars,live=live,defaults=defaults,children=children}
  | addAsBinding (b, OR{path,asvars,vars,live,defaults,variants}) =
    OR{path=path,asvars = b::asvars,vars=vars,live=live,defaults=defaults,variants=variants}
  | addAsBinding (b, SINGLE{path,asvars,vars,dcon,arg}) = 
    SINGLE{path=path,asvars = b::asvars,vars=vars,dcon=dcon,arg=arg})
  | addAsBinding (b, VARS{path,asvars,vars,dcon,arg}) = 
    VARS{path=path,asvars = b::asvars,vars=vars,live=live}
  | addAsBinding (b, LEAF _) = bug "addAsBinding - LEAF"
  (* as-binding for a (say) constant will be attached to the parent OR node *)
				   
(* addVarBinding : binding * andor -> andor *)
fun addVarBinding (b, AND{path,asvars,vars,live,defaults,children}) =
    AND{path=path,asvars=asvars,vars=b::vars,live=live,defaults=defaults,children=children}
  | addVarBinding (b, OR{path,asvars,vars,live,defaults,variants}) =
    OR{path=path,asvars=asvars,vars=b::vars,live=live,defaults=defaults,variants=variants}
  | addVarBinding (b, SINGLE{path,asvars,vars,dcon,arg}) = 
    SINGLE{path=path,asvars=asvars,vars=b::vars,dcon=dcon,arg=arg})
  | addVarBinding (b, VARS{path,asvars,vars,dcon,arg}) = 
    VARS{path=path,asvars=asvars,vars=b::vars,live=live}
  | addVarBinding (b, LEAF _) = bug "addVarBinding - LEAF"
  (* var-binding for a (say) constant will be attached to the parent OR node *)

(* addConstChild: key * ruleno * path * (key * andor) list -> (key * andor) list *)
fun addConstChild (key, rule, path, nil) =  (* new constant variant *)
    [(const, LEAF{path = extendPath(path, key), live=R.singleton rule})]
  | addConstChild (constKey, path, rule, (child as (constKey', LEAF{live,...}))::rest) =
      if eqConstKey(constKey, constKey')
      then (constKey, LEAF{path=path,live=R.add(live,rule)})::rest
      else child::(addConstChild(const, path, rule, rest))

(* makeAndor : pat list -> andor *)
(* looks like makeAndor only depends on the first (path,pat) component of each triple
 * forming an element of the matchRep argument (which is a list of triples) *)
fun makeAndor pats =
let
    (* initialization based on the first pattern (rule = 0). No need to pass ruleno
     * since it will always be 0 for initialization *)

    (* pushDefaults : int list * matchTree -> matchTree *)
    (* Pushes down default rules introduced by variable patterns to all
     * the nodes in the matchTree to which the variable is attached by mergePat.
     * This is used as a post-processing phase after the initial matchTree is
     * constructued by calls of mergeAndor. *)
    fun pushDefaults (defaults : ruleset, andor: andor) : andor =
	case andor
	 of OR{path,asvars,vars,live,defaults,variants} =>
	 let val defaults' = R.union(defaults, map #2 vars)
	     fun push (key,andor') = (key, pushDefaults(defaults', andor'))
	 in OR{path=path,
	       asvars=asvars,
	       vars=vars,
	       live=live, (* doesn't affect live *)
	       defaults = defaults',
	       variants = map push variants}
       | SINGLE{path,asvars,vars,dcon,andor} =>
	 let val defaults' = R.union(defaults, map #2 vars)
	  in SINGLE{path=path,dcon=dcon,asvars=asvars,vars=vars,
		    arg=pushDefaults(defaults',tree)}
	 end
       | AND{path,asvars,vars,live,defaults,children} =>
	 let val defaults' = R.union(defaults, map #2 vars)
	 in AND{path=path,
		asvars=asvars,
		vars=vars,
		live=live,
		children = map (fn mt => pushDefaults(defaults',mt)) children}
	 end
       | VARS{path,asvars,vars,live} => 
	 let val defaults' = R.union(defaults, map #2 vars)
	  in VAR{path=path,asvars=asvars,vars=vars,live=live,defaults=defaults'}
       | LEAF{path,live,defaults=defaults0} =>  (* push defaults all the way down to LEAF nodes? *)
	 LEAF{path=path,live=live,defaults=R.union(defaults,defaults0)}  (* needs defaults? *)


    val initLive : ruleset = R.singleton 0

    (* initAnd : pat list -> andor list
     * Initializing an AND node; preserves order of pattern list.
     * ruleno = 0 and live = {0} (because of mergeAndor with INITIAL) *)
    fun initAnd (pats, path) =
	rev(#1 (foldl
		 (fn (pat,(andors,index)) =>
			(mergeAndor(pat, 0, extendPath(path, R index), INITIAL)::andors, index+1))
		 (nil,0) pats)

    (* mergeAndor : pat * ruleno * path * andor -> andor *)
    (* merge the next pat at rule ruleno into the partially constructed andor tree 
     *  -- don't need to pass a path argument because paths will be found in andor arg?
     *  -- add ruleno to live set at each merged node
     *  -- will typically create new branches (links) under OR nodes, unless keys agree
     *     with an existing variant
     *  -- merging a variable pattern introduces a default rule that has to be propaged
     *     through the entire andor tree being merged into.
     *  -- initialization of the andor tree is achieved by merging the first pattern
     *     into the special andor tree INITIAL, with the ROOTPATH and ruleno 0.
     *     All the nodes get the same live ruleset: R.singleton rule = {0}. (This was
     *     formerly a separate function, initAndor, but handling ORpats is cleaner
     *     with this single-pass method. *)
    and mergeAndor (VARpat var, rule, path, INITIAL) =
	VARS{lvar = newLvar(),
	     path = path,
	     vars = [(var,0)],
	     live = R.singleton rule}
      | mergeAndor (WILDpat, rule, path, INITIAL) =
          (* wildcard pat treated as a particular variable pattern *)
	VARS{lvar = newLvar(),
	     path = path,
	     vars = [(genWildVar(),0)],
	     live = R.singleton rule}
      | mergeAndor (LAYEREDpat(var,_,basepat), rule, path, INITIAL) =
	  (* ignoring type constraint option *)
	  addAsBinding ((var,rule), mergeAndor (basepat, path))  (* no link added for AS basepat *)
      | mergeAndor (NUMpat(_, {ival, ty}), rule, path, INITIAL) =
	  (* how are int and word distinguished? *)
	  let val key = I(ival,ty)  (* how about word? *)
	      val newpath = extendPath(path, key)
	   in OR{lvar = newLvar(),
		 path = path,
		 asvars = nil,
		 vars = nil,
		 live = R.singleton rule,
		 defaults = R.empty,
		 variants = [(key, LEAF{path=newpath,live=R.singleton rule})]
	  end
      | mergeAndor (STRINGpat s, rule, path, INITIAL) =
	  let val key = S s
	      val newpath = extendPath(path,key)
	   in OR{lvar = newLvar(),
		 path = path,
		 asvars = nil,
		 vars = nil,
		 live = R.singleton rule,
		 defaults = R.empty,
		 variants = [(key, LEAF{path=newpath,live=R.singleton rule})]}
	  end
      | mergeAndor (CHARpat s, rule, path, INITIAL) =
	  let val key = C (charCon s)
	      val newPath = extendPath(path, key)
	   in OR{lvar = newLvar(),
		 path = path,
		 asvars = nil,
		 vars = nil,
		 live = R.singleton rule,
		 defaults = R.empty,
		 variants = [(key, LEAF{path=newpath, live=R.singleton rule})]}
	  end
      | mergeAndor (RECORDpat{fields,...}, rule, path, INITIAL) =
	  AND{lvar = newLvar(),
	      path = path,
	      asvars = nil,
	      vars = nil,
	      live = R.singleton rule,
	      defaults = R.empty,
	      children = (initAnd(map #2 fields, path)}
      | mergeAndor (CONpat(dcon,tvs), rule, path, INITIAL) =  (* constant datacon *)
	  let val key = D(dcon,tvs)
	      val newpath = extendPath(path, key)
  	   in if singletonDatacon dcon
	      then SINGLE{lvar = newLvar(),
			  path = path,
			  dcon = dcon,
			  asvars = nil,
			  vars = nil,
			  defaults = R.empty,
			  live = R.singleton rule,  (* redundant ??? *)
			  arg = LEAF{path = newpath, live = R.singleton rule}}
	      else OR{path = path,
		      asvars = nil,
		      vars = nil,
		      live = R.singleton rule,
		      defaults = R.empty,
		      variants = [(key, LEAF{path=newpath, live=R.singleton rule})]}
	  end
      | mergeAndor (APPpat(dcon,tvs,pat), rule, path, INITIAL) =
	  let val key = D(dcon,tvs)
	      val newpath = extendPath(path, key)
	   in if singletonDatacon dcon
	      then SINGLE{path = path,  (* SINGLE's arg gets the new link *)
			  dcon = dcon,  (* there is only one *)
			  asvars = nil,
			  vars = nil,
			  live = R.singleton rule,  (* redundant -- same as child *)
			  defaults = R.empty, (* no variables on path, because 1 pattern *)
			  arg = mergeAndor(pat,rule,newpath)}
	      else OR{lvar = newLvar(),
		      path = path,
		      live = R.singleton rule,
		      defaults = R.empty,
		      asvars = nil,
		      vars = nil,
		      variants = [(key, mergeAndor(pat,rule,newpath))]}
	  end
      | mergeAndor (VECTORpat(pats,ty), rule, path, INITIAL) =
	  let val vlen = length pats
	      val newpath = insert(VL(vlen), path)
	  in OR{lvar = newLvar(),
		path = path,
		asvars = nil,
		vars = nil,
		live = R.singleton rule,
		defaults = R.empty,
		variants = ORvec (ty, [(vlen, AND(initAnd(pats, newpath)))])}
	  end
      | mergeAndor (VARpat v, rule, path, andor) =
	  addVarBinding ((v,rule), andor)
          (* rule should be propagated into defaults for all nodes in andor!
           * Since the andor tree has already been build, this will be done
           * in a second pass by the function pushDefaults. *)
      | mergeAndor (WILDpat, rule, path, andor) =
	  addVarBinding ((genWildVar(),rule), andor)
          (* same as for VARpat case *)
      | mergeAndor (CONSTRAINTpat(pat, _), rule, path, andor) =
	  (* disregard type constraints -- they have no role in pattern matching *)
	  mergeAndor(pat, rule, andor)
      | mergeAndor (LAYEREDpat(v,_,basepat), rule, path, andor) =
	  addAsBinding ((v,rule), mergeAndor (basepat, rule, path, andor))
      | mergeAndor (NUMpat(_, {ival, ty}), rule, _, 
		    OR{lvar,path,asvars,vars,live,defaults,variants}) =
	  OR{lvar = lvar,
	     path = path,
	     asvars = asvars,
	     vars = vars,
	     live = R.add(live,rule),
	     defaults = defaults,
	     variants = addConstChild(numCon(ival, ty), rule, path, variants)}
      | mergeAndor (STRINGpat s, rule, _,
		    OR{lvar,path,asvars,vars,live,defaults,variants}) =
	  OR{lvar = lvar,
	     path = path,
	     asvars = asvars,
	     vars = vars,
	     live = R.add(live,rule),
	     defaults = defaults,
	     variants = addConstChild(S s, rule, path, variants)}
      | mergeAndor (CHARpat s, rule, _,
		    OR{path,asvars,vars,live,defaults,variants}) =
	  OR{lvar = lvar,
	     path = path,
	     asvars = asvars,
	     vars = vars,
	     live = R.add(live,rule),
	     defaults = defaults,
	     variants = addConstChild(C(charCon s), rule, path, variants)}
      | mergeAndor (RECORDpat{fields,...}, rule, _, AND{path,children,...}) =
        (* mergeAnd : (ruleno * path) -> pat * andor -> andor *)
	  let fun mergeAnd (pat, andor) = mergeAndor(pat, rule, path, andor)
	    (* arity of record and AND andor node are equal because they have the same type *)
	   in AND (ListPair.map (mergeAnd (rule,path)) (map #2 fields, children))
	  end
      | mergeAndor (VECTORpat(pats,ty), rule, _,
		    OR{path,asvars,vars,live,defaults,variants})
	  (* ASSERT: ty and ty' are equal *)
	  OR{path = path,
	     asvars = asvars,
	     vars = vars,
	     live = R.add(live,rule),
	     defaults = defaults,
	     variants = mergeVector (V(length pats,ty),pats,rule,path,variants)}
      | mergeAndor (CONpat(dcon,tvs), rule, _,
		    OR{lvar,path,asvars,vars,live,defaults,variants)) =
	  OR{lvar = lvar,
	     path = path,
	     asvars = asvars,
	     vars = vars,
	     live = R.add(live,rule),
	     defaults = defaults,
	     variants = mergeData (D(dcon,tvs), NONE, rule, path, variants))}
      | mergeAndor (APPpat(dcon,tvs,pat), rule, _,
		    OR{lvar,path,asvars,vars,live,defaults,variants)) =
	  OR{lvar = lvar,
	     path = path,
	     asvars = asvars,
	     vars = vars,
	     live = R.add(live,rule),
	     defaults = defaults,
	     variants = mergeData (D(dcon,tvs), SOME pats, rule, path, variants)}
      | mergeAndor (ORpat(pat1,pat2), rule, path, andor) = 
	  mergeAndor(pat1, rule, path, mergeAndor(pat2, rule, path, andor))
      | mergeAndor (pat, rule, _, andor as VARS _) =
	  mergeVars (pat, rule, andor)
      | mergeAndor _ =  (* remaining cases impossible: incompatible types *)
	  bug "mergeAndor: incompatible pat and andor tree"

    (* mergeVector : pat list * rule * path * (int * andor) list -> (int * andor) list *)
    and mergeVector (pats,rule,path,children: (int * andor) list) : (int * andor) list =
	let val len = length pats (* could be 0 *)
	    fun merge (vbs as (vb as (len', AND andors)::rest), prefix) =
		if len < len'  (* a new vector length, hence new variant *)
		then List.revAppend(prefix,
		       (len, AND(initAnd(pats, path, rule)))::vbs)
		else if len = len'
		then List.revAppend(prefix,
		       (len, AND(mergeAnd(pats, andors, rule)))::rest)
		else merge (rest, vb::prefix)
	      | merge (nil,prefix) = rev((len, AND(initAnd(pats,rule,path)))::prefix)
	 in merge(children,nil)
	end

    (* mergeData : datacon * pat option * ruleno * variant list -> variant list *)
    (* can the tvs of two dataCons with the same datacon differ? If so, how to handle this? *)
    and mergeData (key, patOp, rule, path, variants) =
	let fun merge ((key',andor)::rest, revprefix) =
		  if eqKey(key,key')
		  then let val child =
			       case (patOp, andorOp)
				 of (NONE, LEAF{path,live}) =>  (* constant dcon *)
				    (key, LEAF{path=path,
					       live=R.add(ruleset,rule),
					       defaults=R.empty})
				  | (SOME pat, andor) => 
				     (key, mergeAndor(pat,rule,path,andor))
				  | _ => bug "mergeData 1"
			 in List.revAppend (revprefix, child::rest)
			end
		  else merge(rest, db::revprefix)
	      | merge (nil, revprefix) =  (* dcon is new to the OR node *)
		  let val child =
			  case patOp
			    of NONE =>  (* constant dcon *)
			       (key, LEAF{path=extendPath(path,key), live=R.singleton rule})
			     | SOME pat =>
			       (key, mergeAndor(pat,rule,extendPath(path,key),INITIAL))
		   in rev (child::revprefix)
		  end
	      | merge _ = bug "mergeData 2"
	 in merge(children`, nil)
	end

    and mergeVars (pat, rule, VARS{path, asvars, vars, live}) =
        (* does live ruleset from the VARS node play a part?  
         * Is live = R.empty and invariant? *)
	let val andor0 = mergeAndor(pat, rule, path, INITIAL)
	    val andor1 = addVarBindings(vars, andor0)
	    val andor2 = addAsBindings(asvars, andor1)
	 in andor2
	end

    (* makeAndor0 : pat list -> andor *)
    fun makeAndor0 (pat::pats) =
	let val andor0 = mergeAndor(pat, 0, rootPath, INITIAL)   (* rule 0 *)
	in #1 (foldl (fn (pat,(andor, rule)) =>
			 (mergeAndor(pat,rule,rootPath,andor), next rule)) (andor0,next 0)) pats
	end
      | makeAndor0 nil = bug "makeAndor0" (* INVARIANT: pats arg not null *)

in pushDefaults(R.empty, (makeAndor0 pats))

end (* fun makeAndor *)

end (* local open *)

end (* structure AndOr *)


    (* DBM: what are the invariants of merging?  E.g.
        - Should WILDCARD and variables "absorb" any futher patterns? NO! Example 2.
        - If there is a dominating discrimination, will two nodes ever be merged? NO.
        - There are no nested BIND or DEFAULT constructors.  They always merge into one.
        - Patterns that are merged into an andor tree have the same type, and that
          applies to subpatterns all the way down to the leaves. This implies that
          a record pattern will only be merged with a "record" (ALL) andor node,
          never with a ORdata or ORconst or ORvec andor node (which are varieties of OR nodes).
        - vector element patterns will only be merged with andor nodes (lists) of the
          same length (after discriminating on the length at a ORvec node).
        -- push defaults down paths and accumulte them
     *)

    (* DEFAULT RULE:
     * If we systematically add an extra default rule (e.g. "_ => raise MATCH") to close
     * a potentially nonexhaustive match, then the rule number of that default rule will
     * be n (if the original match had n rules, numbered starting with 0).
     * This means that rule n will be a default rule for the entire andor tree of the
     * match.
     *)

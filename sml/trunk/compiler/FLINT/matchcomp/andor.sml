(* makeandor.sml *)

(* andor.sml *)

structure AndOr =
struct

local
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure IC = IntConst
  structure LV = LambdaVar
  structure VC = VarCon
  structure R = Rules
  open Absyn MCTypes
  (* also used: IntInf, Target, Char, String *)
in

fun bug msg = ErrorMsg.impossible msg

val newLvar = LambdaVar.mkLvar

(* numCon : IntInf.int * T.ty -> key *)
(* Takes a front-end number literal (Types.ty IntConst.t) and produces a FLINT-style
 * literal representation (int IntCons.t).
 * For translation of matches to Absyn, we should stick withy front-end style
 * (Types.ty IntConst.t), with the translation to FLINT-style done later in translate. *)
fun numKey (v, ty) =
    let fun mkWORD sz = W({ival = v, ty = sz})  (* FLINT-style literal *)
	fun mkINT sz  = I({ival = v, ty = sz})  (* FLINT-style literal *)
	val defaultIntSz = 63 (* = Target.defaultIntSz *)
    in
	if TU.equalType(ty, BT.intTy)
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
(* QUESTION: perhaps we should preserve the size (e.g., in the case of
 * word8) for better jump tables? *)
	  then mkWORD defaultIntSz
	else if TU.equalType(ty, BT.word32Ty)
	  then mkWORD 32
        else if TU.equalType(ty, BT.word64Ty)
          then mkWORD 64
	else bug "numCon"
    end

(* charCon : string -> char; string assumed to be of size = 1 *)
fun charCon s = String.sub (s, 0)

(* addAsBinding : asBindings * andor -> andor *)
fun addAsBindings (b, AND{lvar,path,asvars,vars,live,defaults,children}) =
    AND{lvar=lvar,path=path,asvars = asvars@b,vars=vars,live=live,
	defaults=defaults,children=children}
  | addAsBindings (b, OR{lvar,path,asvars,vars,live,defaults,variants}) =
    OR{lvar=lvar,path=path,asvars = asvars@b,vars=vars,live=live,
       defaults=defaults,variants=variants}
  | addAsBindings (b, SINGLE{lvar,path,asvars,vars,dcon,arg}) = 
    SINGLE{lvar=lvar,path=path,asvars = asvars@b,vars=vars,dcon=dcon,arg=arg}
  | addAsBindings (b, VARS{lvar,path,asvars,vars,live,defaults}) = 
    VARS{lvar=lvar,path=path,asvars = asvars@b,vars=vars,live=live,defaults=defaults}
  | addAsBindings _ = bug "addAsBindings"
    (* as-binding for a (say) constant will be attached to the parent OR node *)
				   
(* addVarBindings : varBindings * andor -> andor *)
fun addVarBindings (b, AND{lvar,path,asvars,vars,live,defaults,children}) =
    AND{lvar=lvar,path=path,asvars=asvars,vars=vars@b,live=live,
	defaults=defaults,children=children}
  | addVarBindings (b, OR{lvar,path,asvars,vars,live,defaults,variants}) =
    OR{lvar=lvar,path=path,asvars=asvars,vars=vars@b,live=live,
       defaults=defaults,variants=variants}
  | addVarBindings (b, SINGLE{lvar,path,asvars,vars,dcon,arg}) = 
    SINGLE{lvar=lvar,path=path,asvars=asvars,vars=vars@b,dcon=dcon,arg=arg}
  | addVarBindings (b, VARS{lvar,path,asvars,vars,live,defaults}) = 
    VARS{lvar=lvar,path=path,asvars=asvars,vars=vars@b,live=live,defaults=defaults}
  | addVarBindings _ = bug "addVarBindings"
    (* var-binding for a (say) constant will be attached to the parent OR node *)

(* addConstVariant: key * ruleno * path * (key * andor) list -> (key * andor) list *)
fun addConstVariant (key, rule, path, nil) =  (* new constant variant *)
    [(key, LEAF{path = extendPath(path, key), live=R.singleton rule, defaults=R.empty})]
  | addConstVariant (key, rule, _, (variant as (key', LEAF{path,live,defaults}))::rest) =
      if eqKey(key, key')
      then (key, LEAF{path=path,live=R.add(live,rule),defaults=defaults})::rest
      else variant::(addConstVariant(key, rule, path, rest))
  | addConstVariant _ = bug "addConstVariant"
			  
(* makeAndor : pat list -> andor *)
(* construct the AND-OR tree for the sequence of patterns of a match *)
fun makeAndor (pats: pat list) =
let
    (* pushDefaults : int list * matchTree -> matchTree *)
    (* Pushes down default rules introduced by variable patterns to all
     * the nodes in the matchTree to which the variable is attached by mergePat.
     * This is used as a post-processing phase after the initial matchTree is
     * constructued by calls of mergeAndor. It is expected that all defaults
     * fields will be empty in the AND-OR tree constructed by the repeated
     * calls of mergeAndor for each rule. *)
    fun pushDefaults (outerDefaults : ruleset, andor: andor) : andor =
	case andor
	 of OR{lvar,path,asvars,vars,live,defaults,variants} =>
	     let val defaults' = R.union(outerDefaults, R.addList(defaults, map #2 vars))
		 fun push (key,andor') = (key, pushDefaults(defaults', andor'))
	     in OR{lvar=lvar, path=path, asvars=asvars, vars=vars, live=live,
		   defaults = defaults', variants = map push variants}
	     end
	  | SINGLE{lvar,path,asvars,vars,dcon,arg} =>
	      let val defaults' = R.addList(outerDefaults, map #2 vars)
	       in SINGLE{lvar=lvar,path=path,dcon=dcon,asvars=asvars,vars=vars,
			 arg=pushDefaults(defaults',arg)}
	      end
	  | AND{lvar=lvar,path,asvars,vars,live,defaults,children} =>
	      let val defaults' = R.union(outerDefaults, R.addList(defaults, map #2 vars))
	      in AND{lvar=lvar, path=path, asvars=asvars, vars=vars, live=live, defaults=defaults',
		     children = map (fn andor => pushDefaults(defaults',andor)) children}
	      end
	  | VARS{lvar,path,asvars,vars,live,defaults} => 
	      let val defaults' = R.union(outerDefaults, R.addList(defaults, map #2 vars))
	      in VARS{lvar=lvar,path=path,asvars=asvars,vars=vars,live=live,defaults=defaults'}
	      end
	  | LEAF{path,live,defaults} =>
	      (* push defaults all the way down to LEAF nodes? Are defaults used here? *)
	      LEAF{path=path,live=live,defaults=R.union(outerDefaults,defaults)}
	  | _ => bug "pushDefaults"

    (* initAnd : pat list * ruleno * path -> andor list
     * Initializing an AND node; preserves order of pattern list.
     *   pats: component patterns of a product
     *   path: path to the AND node
     *   ruleno = 0 and live = {0} (because of mergeAndor with INITIAL) *)
    fun initAnd (pats, rule, path) =
	rev(#1 (foldl
		 (fn (pat,(andors,index)) =>
		     (mergeAndor(pat, rule, extendPath(path, R index), INITIAL)::andors,
		      index+1))
		 (nil,0) pats))

    (* mergeAndor : pat * ruleno * path * andor -> andor *)
    (* merge the next pat at rule ruleno into the partially constructed andor tree 
     *  -- don't need to pass a path argument because paths will be found in andor arg?
     *  -- add ruleno to live set at each merged node
     *  -- will typically create new branches (links) under OR nodes, unless keys agree
     *     with an existing variant
     *  -- merging a variable pattern introduces a default rule that has to be propaged
     *     through the entire andor tree being merged into.
     *  -- initialization of the andor tree is achieved by merging the first pattern
     *     into the special andor tree INITIAL, with path = rootPath and rule = 0.
     *     All the nodes get the same live ruleset: R.singleton rule = {0}. (This was
     *     formerly a separate function, initAndor, but handling ORpats is cleaner
     *     with this single-pass method.
     *  -- path argument only relevant to initialization of new nodes *)
    and mergeAndor (VARpat var, rule, path, INITIAL) =
	VARS{lvar = newLvar(), path = path, asvars = [], vars = [(var,rule)],
	     live = R.singleton rule, defaults = R.empty}
      | mergeAndor (WILDpat, rule, path, INITIAL) =
          (* wildcard pat treated as a particular variable pattern *)
	  let val lvar = newLvar()
	  in VARS{lvar = lvar, path = path, asvars = nil,
		  vars = [(VarCon.mkVALvar("_",lvar), rule)],
		  live = R.singleton rule,
		  defaults = R.empty}
	  end
      | mergeAndor (LAYEREDpat(var,basepat), rule, path, INITIAL) =
	  (* ignoring type constraint option *)
	addAsBindings ([(var,rule)], mergeAndor (basepat, rule, path, INITIAL))
          (* no link added for the basepat *)
      | mergeAndor (NUMpat(_, {ival, ty}), rule, path, INITIAL) =
	  (* how are int and word distinguished? *)
	  let val key = numKey(ival,ty)  (* produces int or word based on ty *)
	      val newpath = extendPath(path, key)
	   in OR{lvar = newLvar(), path = path, asvars = nil, vars = nil,
		 live = R.singleton rule, defaults = R.empty,
		 variants = [(key, LEAF{path=newpath,live=R.singleton rule, defaults=R.empty})]}
	  end
      | mergeAndor (STRINGpat s, rule, path, INITIAL) =
	  let val key = S s
	      val newpath = extendPath(path,key)
	   in OR{lvar = newLvar(), path = path, asvars = nil, vars = nil,
		 live = R.singleton rule, defaults = R.empty,
		 variants = [(key, LEAF{path=newpath,live=R.singleton rule, defaults=R.empty})]}
	  end
      | mergeAndor (CHARpat s, rule, path, INITIAL) =
	  let val key = C (charCon s)
	      val newpath = extendPath(path, key)
	   in OR{lvar = newLvar(), path = path, asvars = nil, vars = nil,
		 live = R.singleton rule, defaults = R.empty,
		 variants = [(key, LEAF{path=newpath, live=R.singleton rule, defaults=R.empty})]}
	  end
      | mergeAndor (RECORDpat{fields,...}, rule, path, INITIAL) =
	  AND{lvar = newLvar(), path = path, asvars = nil, vars = nil,
	      live = R.singleton rule, defaults = R.empty,
	      children = initAnd(map #2 fields, rule, path)}
      | mergeAndor (CONpat(dcon,tvs), rule, path, INITIAL) =  (* constant datacon *)
	  let val key = D(dcon,tvs)
	      val newpath = extendPath(path, key)
  	   in if TU.dataconWidth dcon = 1
	      then SINGLE{lvar = newLvar(), path = path, asvars = nil, vars = nil, dcon = dcon,
			  arg = LEAF{path = newpath, live = R.singleton rule, defaults=R.empty}}
	      else OR{lvar=newLvar(), path = path, asvars = nil, vars = nil,
		      live = R.singleton rule, defaults = R.empty,
		      variants = [(key, LEAF{path=newpath, live=R.singleton rule, defaults=R.empty})]}
	  end
      | mergeAndor (APPpat(dcon,tvs,pat), rule, path, INITIAL) =
	  let val key = D(dcon,tvs)
	      val newpath = extendPath(path, key)
	   in if TU.dataconWidth dcon = 1
	      then SINGLE{lvar = newLvar(), path = path,  (* SINGLE's arg gets the new link *)
			  dcon = dcon,  (* there is only one *)
			  asvars = nil,
			  vars = nil,
			  arg = mergeAndor(pat,rule,newpath,INITIAL)}
	      else OR{lvar = newLvar(), path = path, asvars = nil, vars = nil,
		      live = R.singleton rule, defaults = R.empty,
		      variants = [(key, mergeAndor(pat,rule,newpath,INITIAL))]}
	  end
      | mergeAndor (VECTORpat(pats,ty), rule, path, INITIAL) =
	  let val vlen = length pats
	      val newpath = extendPath(path, V(vlen,ty))
	      val velements =
		  AND{lvar = newLvar(),
		      path = newpath,
		      asvars = nil, vars = nil,
		      live = R.singleton rule,
		      defaults = R.empty,
		      children = initAnd(pats, rule, newpath)}
	  in OR{lvar = newLvar(),
		path = path,
		asvars = nil,
		vars = nil,
		live = R.singleton rule,
		defaults = R.empty,
		variants = [(V(vlen,ty), velements)]}
	  end
      | mergeAndor (VARpat v, rule, path, andor) =
	  addVarBindings ([(v,rule)], andor)
          (* rule should be propagated into defaults for all nodes in andor!
           * Since the andor tree has already been build, this will be done
           * in a second pass by the function pushDefaults. *)
      | mergeAndor (WILDpat, rule, path, andor) =
	  addVarBindings ([(VC.mkVALvar("_",newLvar()),rule)], andor)
          (* same as for VARpat case *)
(*      | mergeAndor (CONSTRAINTpat(pat, _), rule, path, andor) =
	  (* disregard type constraints -- they have no role in pattern matching *)
	  mergeAndor(pat, rule, path, andor)
*)
      | mergeAndor (LAYEREDpat(v,basepat), rule, path, andor) =
	  addAsBindings ([(v,rule)], mergeAndor (basepat, rule, path, andor))
      | mergeAndor (NUMpat(_, {ival, ty}), rule, _, 
		    OR{lvar,path,asvars,vars,live,defaults,variants}) =
	  OR{lvar = lvar, path = path, asvars = asvars, vars = vars,
	     live = R.add(live,rule), defaults = defaults,
	     variants = addConstVariant(numKey(ival, ty), rule, path, variants)}
      | mergeAndor (STRINGpat s, rule, _,
		    OR{lvar,path,asvars,vars,live,defaults,variants}) =
	  OR{lvar = lvar, path = path, asvars = asvars, vars = vars,
	     live = R.add(live,rule), defaults = defaults,
	     variants = addConstVariant(S s, rule, path, variants)}
      | mergeAndor (CHARpat s, rule, _,
		    OR{lvar,path,asvars,vars,live,defaults,variants}) =
	  OR{lvar = lvar, path = path, asvars = asvars, vars = vars,
	     live = R.add(live,rule), defaults = defaults,
	     variants = addConstVariant(C(charCon s), rule, path, variants)}
      | mergeAndor (RECORDpat{fields,...}, rule, _,
		    AND{lvar,path,asvars,vars,live,defaults,children}) =
              (* mergeAnd : (ruleno * path) -> pat * andor -> andor *)
	  let fun mergeAnd (pat, andor) = mergeAndor(pat, rule, path, andor)
	    (* arity of record and AND andor node are equal because they have the same type *)
	  in AND{lvar = lvar, path = path, asvars = asvars, vars = vars,
		 live = R.add(live,rule), defaults = defaults,
		 children = ListPair.map mergeAnd (map #2 fields, children)}
	  end
      | mergeAndor (VECTORpat(pats,ty), rule, _,
		    OR{lvar,path,asvars,vars,live,defaults,variants}) =
	  (* ASSERT: ty and ty' are equal *)
	  OR{lvar = lvar, path = path, asvars = asvars, vars = vars,
	     live = R.add(live,rule), defaults = defaults,
	     variants = mergeVector (pats,ty,rule,path,variants)}
      | mergeAndor (CONpat(dcon,tvs), rule, _,
		    OR{lvar,path,asvars,vars,live,defaults,variants}) =
	  OR{lvar = lvar, path = path, asvars = asvars, vars = vars,
	     live = R.add(live,rule), defaults = defaults,
	     variants = mergeData (D(dcon,tvs), NONE, rule, path, variants)}
      | mergeAndor (APPpat(dcon,tvs,pat), rule, _,
		    OR{lvar,path,asvars,vars,live,defaults,variants}) =
	  OR{lvar = lvar, path = path, asvars = asvars, vars = vars,
	     live = R.add(live,rule), defaults = defaults,
	     variants = mergeData (D(dcon,tvs), SOME pat, rule, path, variants)}
      | mergeAndor (ORpat(pat1,pat2), rule, path, andor) = 
	  mergeAndor(pat1, rule, path, mergeAndor(pat2, rule, path, andor))
      | mergeAndor (pat, rule, _, andor as VARS _) =
	  mergeVars (pat, rule, andor)
      | mergeAndor _ =  (* remaining cases impossible: incompatible types *)
	  bug "mergeAndor: incompatible pat and andor tree"

   (* mergeVector : pat list * rule * path * variant list -> variant list *)
   (* maintains vector variants in ascending order by length *)
    and mergeVector (pats, ty, rule, path, variants: (key * andor) list) : variant list =
	let val len = length pats (* could be 0 *)
	    val newKey = V(len, ty)
	    val newPath = extendPath(path, newKey)
	    fun merge (variants as (vv as (V(len',_), andor))::rest, revprefix) =
		if len < len'  (* a new vector length, hence new vector variant *)
		then List.revAppend(revprefix,
				    (newKey,
				     AND{lvar = newLvar(),
					 path = extendPath(path, newKey),
					 asvars = nil, vars = nil,
					 live = R.singleton rule,
					 defaults = R.empty,
					 children = initAnd(pats, rule, path)})::variants)
		else if len = len' (* merge into existing variant *)
		then case andor
		      of AND{lvar,path,asvars,vars,live,defaults,children} =>
			 let fun mergeAnd (pat, andor) = mergeAndor(pat, rule, path, andor)
			  in List.revAppend(revprefix,
				(newKey,  (* same as the old key *)
				 AND{lvar=lvar, path=path, asvars=asvars, vars=vars,
				     live = R.add(live,rule), defaults = defaults,
				     children =
				      ListPair.map mergeAnd (pats, children)})
				:: rest)
			 end
		      | _ => bug "mergeVector"
		else merge (rest, vv::revprefix)
	      | merge (nil,revprefix) =  (* len is new and > existing lengths in variants *)
		let val newVariant = (newKey,
				      AND{lvar = newLvar(),
					  path = newPath,
					  asvars = nil, vars = nil,
					  live = R.singleton rule,
					  defaults = R.empty,
					  children = initAnd(pats,rule,newPath)})
		 in rev(newVariant::revprefix)
		end
	      | merge _ = bug "mergeVector.merge"
	 in merge(variants,nil)
	end

    (* mergeData : datacon * pat option * ruleno * variant list -> variant list *)
    (* could the tvs of two dataCons with the same datacon differ? If so, how to handle this? *)
    and mergeData (key, patOp: pat option, rule, path, variants) =
	let fun merge ((variant as (key',andor))::rest, revprefix) =
		  if eqKey(key,key')
		  then let val newVariant =
			       case (patOp, andor)
				 of (NONE, LEAF{path,live,defaults}) =>  (* constant dcon *)
				    (key, LEAF{path=path,
					       live=R.add(live,rule),
					       defaults=defaults})
				  | (SOME pat, andor) => 
				     (key, mergeAndor(pat,rule,path,andor))
				  | _ => bug "mergeData 1"
			 in List.revAppend (revprefix, newVariant::rest)
			end
		  else merge(rest, variant::revprefix)
	      | merge (nil, revprefix) =  (* dcon is new to the OR node *)
		  let val child =
			  case patOp
			    of NONE =>  (* constant dcon *)
			       (key, LEAF{path=extendPath(path,key), live=R.singleton rule,
					  defaults=R.empty})
			     | SOME pat =>
			       (key, mergeAndor(pat,rule,extendPath(path,key),INITIAL))
		   in rev (child::revprefix)
		  end
	 in merge(variants, nil)
	end

    and mergeVars (pat, rule, VARS{lvar, path, asvars, vars, live, defaults}) =
        (* does live ruleset from the VARS node play a part?  
         * Is live = R.empty and invariant? *)
	let val andor0 = mergeAndor(pat, rule, path, INITIAL) (* will generate new lvar *)
                (* should we be preserving the lvar of the VARS andor? *)
	    val andor1 = addVarBindings(vars, andor0)
	    val andor2 = addAsBindings(asvars, andor1)
	 in andor2
	end
      | mergeVars _ = bug "mergeVars"

    (* makeAndor0 : pat list -> andor *)
    (* REQUIREMENT: must be at least one pattern; length(pats) > 0 *)
    fun makeAndor0 pats =
        #1 (foldl (fn (pat,(andor, rule)) =>
		      (mergeAndor(pat,rule,rootPath,andor), R.increment rule))
		  (INITIAL, 0) pats)

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

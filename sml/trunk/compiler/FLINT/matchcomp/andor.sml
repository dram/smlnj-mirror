(* makeandor.sml *)

(* andor.sml *)

structure AndOr =
struct

local
  structure T = Types
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure LV = LambdaVar
  structure V = Var
  structure SV = SVar
  structure R = Rules
  open Absyn MCTypes
  (* also used: IntInf, Target, Char, String *)
in

fun bug msg = ErrorMsg.impossible msg

(* numToKey : T.ty IntConst.t -> key *)
fun numToKey ((num as {ival,ty}): T.ty IntConst.t) : key =
    if List.exists (fn ty' => TU.equalType(ty,ty'))
		   [BT.intTy, BT.int32Ty, BT.int64Ty, BT.intinfTy]
    then I num
    else W num

(* charCon : string -> char; string assumed to be of size = 1 *)
fun charCon s = String.sub (s, 0)

(* addAsBinding : asBindings * andor -> andor *)
fun addAsBindings (b, AND{svar,path,asvars,vars,direct,defaults,children}) =
    AND{svar=svar,path=path,asvars = asvars@b,vars=vars,direct=direct,
	defaults=defaults,children=children}
  | addAsBindings (b, OR{svar,path,asvars,vars,direct,defaults,variants}) =
    OR{svar=svar,path=path,asvars = asvars@b,vars=vars,direct=direct,
       defaults=defaults,variants=variants}
  | addAsBindings (b, SINGLE{svar,path,asvars,vars,dcon,arg}) = 
    SINGLE{svar=svar,path=path,asvars = asvars@b,vars=vars,dcon=dcon,arg=arg}
  | addAsBindings (b, VARS{svar,path,asvars,vars,defaults}) = 
    VARS{svar=svar,path=path,asvars = asvars@b,vars=vars,defaults=defaults}
  | addAsBindings _ = bug "addAsBindings"
    (* as-binding for a (say) constant will be attached to the parent OR node *)
				   
(* addVarBindings : varBindings * andor -> andor *)
fun addVarBindings (b, AND{svar,path,asvars,vars,direct,defaults,children}) =
    AND{svar=svar,path=path,asvars=asvars,vars=vars@b,direct=direct,
	defaults=defaults,children=children}
  | addVarBindings (b, OR{svar,path,asvars,vars,direct,defaults,variants}) =
    OR{svar=svar,path=path,asvars=asvars,vars=vars@b,direct=direct,
       defaults=defaults,variants=variants}
  | addVarBindings (b, SINGLE{svar,path,asvars,vars,dcon,arg}) = 
    SINGLE{svar=svar,path=path,asvars=asvars,vars=vars@b,dcon=dcon,arg=arg}
  | addVarBindings (b, VARS{svar,path,asvars,vars,defaults}) = 
    VARS{svar=svar,path=path,asvars=asvars,vars=vars@b,defaults=defaults}
  | addVarBindings _ = bug "addVarBindings"
    (* var-binding for a key LEAF will be attached to the parent OR node *)

(* mergeConst: key * ruleno * path * (key * andor) list -> (key * andor) list *)
fun mergeConst (key, rule, path, nil) =  (* new constant variant *)
    [(key, LEAF{path = extendPath(path, key), direct=R.singleton rule, defaults=R.empty})]
  | mergeConst (key, rule, _, (variant as (key', LEAF{path,direct,defaults}))::rest) =
      if eqKey(key, key')
      then (key, LEAF{path=path,direct=R.add(direct,rule),defaults=defaults})::rest
      else variant::(mergeConst(key, rule, path, rest))
  | mergeConst _ = bug "mergeConst"
			  
(* makeAndor : pat list * T.ty -> andor *)
(* construct the AND-OR tree for the sequence of patterns of a match *)
fun makeAndor (pats: pat list, topTy: T.ty) =
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
	 of OR{svar,path,asvars,vars,direct,defaults,variants} =>
	     (* ASSERT: defaults = R.empty *)
	     let val defaults' = R.addList(outerDefaults, map #2 vars)
		 fun push (key,andor') = (key, pushDefaults(defaults', andor'))
	     in OR{svar=svar, path=path, asvars=asvars, vars=vars, direct=direct,
		   defaults = defaults', variants = map push variants}
	     end
	  | SINGLE{svar,path,asvars,vars,dcon,arg} =>
	      let val defaults' = R.addList(outerDefaults, map #2 vars)
	       in SINGLE{svar=svar,path=path,dcon=dcon,asvars=asvars,vars=vars,
			 arg=pushDefaults(defaults',arg)}
	      end
	  | AND{svar=svar,path,asvars,vars,direct,defaults,children} =>
	      let val defaults' = R.addList(outerDefaults, map #2 vars)
	      in AND{svar=svar, path=path, asvars=asvars, vars=vars,
		     direct=direct, defaults=defaults',
		     children = map (fn andor => pushDefaults(defaults',andor)) children}
	      end
	  | VARS{svar,path,asvars,vars,defaults} =>
	      let val defaults' = R.addList(outerDefaults, map #2 vars)
	       in VARS{svar=svar,path=path,asvars=asvars,vars=vars,defaults=defaults'}
	      end
	  | LEAF{path,direct,defaults} =>
	      (* ASSERT: defaults = R.empty *)
	      (* push defaults all the way down to LEAF nodes? Are defaults used here? *)
	      LEAF{path=path,direct=direct,defaults=outerDefaults}
	  | _ => bug "pushDefaults(INITIAL)"

    (* initAnd : pat list * ruleno * rpath -> andor list
     * Initializing an AND node; preserves order of pattern list.
     *   pats: component patterns of a product
     *   path: path to the AND node
     *   ruleno = 0 and direct = {0} (because of mergeAndor with INITIAL)
     * Minor problem: this causes the svars for the children to be generated in
     *  right-to-left order which shows up in the code for Letr bindings.  *)
    fun initAnd (pats, tys, rule, rpath) =
	rev(#1 (foldl
		 (fn ((pat,ty), (andors,index)) =>
		     (mergeAndor(pat, ty, rule, extendRPath(rpath, R index), INITIAL)::andors,
		      index+1))
		 (nil,0) (ListPair.zip(pats,tys))))

    (* mergeAndor : pat * ty * ruleno * rpath * andor -> andor *)
    (* merge the next pat at rule ruleno into the partially constructed andor tree 
     *  -- don't need to pass a path argument because paths will be found in andor arg?
     *  -- We add ruleno to direct set at each merged node
     *  -- Will typically create new branches (links) under OR nodes, unless keys agree
     *     with an existing variant
     *  -- Merging a variable pattern introduces a default rule that has to be propaged
     *     through the entire andor tree being merged into.  defaults will all be 
     *     R.empty originally, but will be updated from vars fields by the pushDefaults pass.
     *  -- Initialization of the andor tree is achieved by merging the first pattern
     *     into the special andor tree INITIAL, with path = rootPath and rule = 0.
     *     All the nodes get the same direct ruleset: R.singleton rule = {0}. (This was
     *     formerly a separate function, initAndor, but handling ORpats is cleaner
     *     with this single-pass method. The per-pattern case structure could be
     *     simplified by unifying treatment of INITIAL under each pattern case instead
     *     of having separate pattern cases for INITIAL and non-INITIAL andors.
     *  -- Path and type arguments are only relevant to initialization of new nodes. Once
     *     a new node is initialized, its path and its type (in the svar) are fixed,
     *     and those fields do not change as additional patterns are merged at the node.
     *     An unfortunate effect of merging initAndor into mergeAndor is that the real
     *     "merge" cases for mergeAndor have to take two arguments (ty and path) that 
     *     are irrelevant (and ignored?) because they are only used in the initialization
     *     of a node. *)
    and mergeAndor (VARpat var, ty, rule, rpath, INITIAL) =
	let val path = reverseRPath rpath
	in VARS{svar = SV.newSvar(pathToString path, ty), path = path, asvars = [],
		vars = [(var,rule)], defaults = R.empty}
	end
      | mergeAndor (WILDpat, ty, rule, rpath, INITIAL) =
          (* wildcard pat treated as a particular variable pattern *)
	  let val path = reverseRPath rpath
	      val svar = SV.newSvar(pathToString path, ty)
	  in VARS{svar = svar, path = path, asvars = nil,
		  vars = [(V.newVALvar("_", ty), rule)],
		  defaults = R.empty}
	  end
      | mergeAndor (LAYEREDpat(var,basepat), ty, rule, rpath, INITIAL) =
	  (* ignoring type constraint option *)
	  addAsBindings ([(var,rule)], mergeAndor (basepat, ty, rule, rpath, INITIAL))
          (* no link added for the basepat *)
      | mergeAndor (NUMpat(_, numLiteral), ty, rule, rpath, INITIAL) =
	  (* ASSERT: ty should be an integer or word ty (as defined in BasicTypes)
           * ASSERT: ty should be equal to the ty component of numLiteral *)
	let val key = numToKey(numLiteral)
	              (* produces int (I) or word (W) key depending on ty *)
	      val path = reverseRPath rpath
	      val newRPath = extendRPath(rpath, key)
	      val newPath = reverseRPath newRPath
	  in OR{svar = SV.newSvar(pathToString path, ty), path = path,
	        asvars = nil, vars = nil,
	        direct = R.singleton rule, defaults = R.empty,
	        variants = [(key, LEAF{path=newPath,direct=R.singleton rule, defaults=R.empty})]}
	  end
      | mergeAndor (STRINGpat s, ty, rule, rpath, INITIAL) =
	  (* ASSERT: ty = BT.stringTy *)
	  let val key = S s
	      val path = reverseRPath rpath
	      val newRPath = extendRPath(rpath,key)
	  in OR{svar = SV.newSvar(pathToString path, ty), path = path,
		asvars = nil, vars = nil,
		direct = R.singleton rule, defaults = R.empty,
		variants = [(key,
			     LEAF{path=reverseRPath newRPath,
				  direct=R.singleton rule, defaults=R.empty})]}
	  end
      | mergeAndor (CHARpat c, ty, rule, rpath, INITIAL) =
	  (* ASSERT: ty = BT.charTy *)
	  let val key = C (charCon c)
	      val path = reverseRPath rpath
	      val newRPath = extendRPath(rpath, key)
	      val newPath = reverseRPath newRPath
	  in OR{svar = SV.newSvar(pathToString path, ty), path = path,
		asvars = nil, vars = nil, direct = R.singleton rule, defaults = R.empty,
		variants = [(key, LEAF{path=newPath, direct=R.singleton rule, defaults=R.empty})]}
	       (* QUESTION: adding the rule to _both_ the OR node and the descendent LEAF node? *)
	  end
      | mergeAndor (RECORDpat{fields,...}, ty, rule, rpath, INITIAL) =
	  let val path = reverseRPath rpath
	      val elemTys = TU.destructRecord(ty)
	   in AND{svar = SV.newSvar(pathToString path, ty), path = path, asvars = nil, vars = nil,
	          direct = R.singleton rule, defaults = R.empty,
	          children = initAnd(map #2 fields, elemTys, rule, rpath)}
	  end
      | mergeAndor (CONpat(dcon,tvs), ty, rule, rpath, INITIAL) =  (* constant datacon *)
          (* ty is the (instantiated) type of the dcon *)
	  let val key = D dcon
	      val path = reverseRPath rpath
	      val newRPath = extendRPath(rpath, key)
	      val newPath = reverseRPath newRPath
	      val svar = SV.newSvar(pathToString path, ty)
  	   in if TU.dataconWidth dcon = 1  (* single datacon *)
	      then SINGLE{svar = svar, path = path,
			  asvars = nil, vars = nil, dcon = dcon,
			  arg = LEAF{path = newPath, direct = R.singleton rule, defaults=R.empty}}
	      else OR{svar=svar, path = path, asvars = nil, vars = nil,
		      direct = R.singleton rule, defaults = R.empty,
		      variants = [(key, LEAF{path=newPath, direct=R.singleton rule,
					     defaults=R.empty})]}
	  end
      | mergeAndor (APPpat(dcon,tvs,pat), ty, rule, rpath, INITIAL) =
	  let val key = D dcon   (* tvs not used *)
	      val path = reverseRPath rpath
	      val newRPath : rpath = extendRPath(rpath, key)
	      val argty = TU.destructCon(ty,dcon)
	   in if TU.dataconWidth dcon = 1
	      then SINGLE{svar = SV.newSvar(pathToString path, ty),
			  path = path,  (* SINGLE's arg gets the new link *)
			  dcon = dcon,  (* unique for this datatype *)
			  asvars = nil,
			  vars = nil,
			  arg = mergeAndor(pat,argty,rule,newRPath,INITIAL)}
	      else OR{svar = SV.newSvar(pathToString path, ty),
		      path = path, asvars = nil, vars = nil,
		      direct = R.singleton rule, defaults = R.empty,
		      variants = [(key, mergeAndor(pat,argty,rule,newRPath,INITIAL))]}
	  end
      | mergeAndor (VECTORpat(pats,elemty), vecty, rule, rpath, INITIAL) =
	let val vlen = length pats
	    val path = reverseRPath rpath
	      val newRPath = extendRPath(rpath, V(vlen))
	      val newPath = reverseRPath newRPath
	      val elemTys = TU.replicateTy(elemty, vlen)  (* list of replicated elemty *)
	      val elemsTy = TU.mkTupleTy(elemTys)         (* tuple type for elements *)
	      val velements =
		  AND{svar = SV.newSvar(pathToString newPath, elemsTy),
		      path = reverseRPath(newRPath),
		      asvars = nil, vars = nil,
		      direct = R.singleton rule,
		      defaults = R.empty,
		      children = initAnd(pats, elemTys, rule, newRPath)}
	  in OR{svar = SV.newSvar(pathToString path, vecty),
		path = path,
		asvars = nil,
		vars = nil,
		direct = R.singleton rule,
		defaults = R.empty,
		variants = [(V vlen, velements)]}
	  end
      (* following rules merge a pattern into an existing andor node *)
      | mergeAndor (VARpat v, ty, rule, rpath, andor) =
	  addVarBindings ([(v,rule)], andor)
          (* rule should be propagated into defaults for all nodes in andor!
           * Since the andor tree has already been build, this will be done
           * in a second pass by the function pushDefaults. *)
      | mergeAndor (WILDpat, ty, rule, path, andor) =
	  addVarBindings ([(V.newVALvar("_",ty),rule)], andor)
          (* same as for VARpat case *)
(* This case will be restored for integrated match compiler.
      | mergeAndor (CONSTRAINTpat(pat, _), rule, path, andor) =
	  (* disregard type constraints -- they have no role in pattern matching *)
	  mergeAndor(pat, rule, path, andor)
*)
      | mergeAndor (LAYEREDpat(v,basepat), ty, rule, rpath, andor) =
	  addAsBindings ([(v,rule)], mergeAndor (basepat, ty, rule, rpath, andor))
      | mergeAndor (NUMpat(_, numLiteral), ty, rule, _, 
		    OR{svar,path,asvars,vars,direct,defaults,variants}) =
	  OR{svar = svar, path = path, asvars = asvars, vars = vars,
	     direct = R.add(direct,rule), defaults = defaults,
	     variants = mergeConst(numToKey(numLiteral), rule, path, variants)}
      | mergeAndor (STRINGpat s, _, rule, _,
		    OR{svar,path,asvars,vars,direct,defaults,variants}) =
	  OR{svar = svar, path = path, asvars = asvars, vars = vars,
	     direct = R.add(direct,rule), defaults = defaults,
	     variants = mergeConst(S s, rule, path, variants)}
      | mergeAndor (CHARpat s, _, rule, _,
		    OR{svar,path,asvars,vars,direct,defaults,variants}) =
	  OR{svar = svar, path = path, asvars = asvars, vars = vars,
	     direct = R.add(direct,rule), defaults = defaults,
	     variants = mergeConst(C(charCon s), rule, path, variants)}
      | mergeAndor (RECORDpat{fields,...}, _, rule, _,
		    AND{svar,path,asvars,vars,direct,defaults,children}) =
              (* mergeAnd : pat * andor -> andor *)
	  let fun mergeAnd (pat, andor) =
		  mergeAndor(pat, getType andor, rule, reversePath path, andor)
	    (* arity of record and AND andor node are equal because they have the same type *)
	   in AND{svar = svar, path = path, asvars = asvars, vars = vars,
		  direct = R.add(direct,rule), defaults = defaults,
		  children = ListPair.map mergeAnd (map #2 fields, children)}
	  end
      | mergeAndor (VECTORpat(pats,ty), ty', rule, _,
		    OR{svar,path,asvars,vars,direct,defaults,variants}) =
	  (* ASSERT: ty and ty' are equal, both being the element type of the vector *)
	  OR{svar = svar, path = path, asvars = asvars, vars = vars,
	     direct = R.add(direct,rule), defaults = defaults,
	     variants = mergeVector (pats,ty,rule,path,variants)}
      | mergeAndor (CONpat(dcon,tvs), _, rule, _,
		    OR{svar,path,asvars,vars,direct,defaults,variants}) =
	let val ty = SV.svarType svar  (* get the type from the existing OR node *)
	    val newVariants = mergeDataConst (D dcon, ty, rule, path, variants)
	 in OR{svar = svar, path = path, asvars = asvars, vars = vars,
	       direct = R.add(direct,rule), defaults = defaults, variants = newVariants}
	end
      | mergeAndor (APPpat(dcon,tvs,pat), _, rule, _,
		    OR{svar,path,asvars,vars,direct,defaults,variants}) =
	let val ty = SV.svarType svar
	    val newVariants = mergeData (D dcon, pat, ty, rule, path, variants)
	 in OR{svar = svar, path = path, asvars = asvars, vars = vars,
	       direct = R.add(direct,rule), defaults = defaults,
	       variants = newVariants}
	end
      | mergeAndor (ORpat(pat1,pat2), _, rule, rpath, andor) =
	  mergeAndor(pat1, T.UNDEFty, rule, rpath, mergeAndor(pat2, T.UNDEFty, rule, rpath, andor))
      | mergeAndor (pat, _, rule, _, VARS{svar, path, asvars, vars, defaults}) =
	  (* does direct ruleset from the VARS node play a part?  
	   * Is direct = R.empty an invariant? *)
	  let val andor0 = mergeAndor(pat, SV.svarType svar, rule, reversePath path, INITIAL)
	          (* This will generate new svar; should we be preserving the svar
                   * of the existing VARS node? *)
	      val andor1 = addVarBindings(vars, andor0)
	      val andor2 = addAsBindings(asvars, andor1)
	   in andor2
	  end
      | mergeAndor _ =  (* remaining cases impossible: incompatible types *)
	  bug "mergeAndor: incompatible pat and andor tree"

   (* mergeVector : pat list * rule * path * variant list -> variant list *)
   (* maintains vector variants in ascending order by length *)
    and mergeVector (pats, elemty, rule, path, variants: (key * andor) list) : variant list =
	let val vlen = length pats (* could be 0 *)
	    val newKey = V vlen
	    val newPath : path = extendPath(path, newKey)
	    val elemTys = TU.replicateTy(elemty, vlen)  (* list of replicated elem tys *)
	    val elemsTy = TU.mkTupleTy(elemTys)         (* tuple type for elements *)
					   
	    fun merge ((variants as ((vv as (V vlen', andor))::rest)), revprefix) =
		if vlen < vlen'  (* a new vector length, hence new vector variant *)
		then List.revAppend(revprefix,
				    (newKey,
				     AND{svar = SV.newSvar(pathToString newPath, elemsTy),
					 path = path,
					 asvars = nil, vars = nil,
					 direct = R.singleton rule,
					 defaults = R.empty,
					 children = initAnd(pats, elemTys, rule, path)})
				    ::variants)
		else if vlen = vlen' (* merge into existing variant *)
		then case andor
		      of AND{svar,path,asvars,vars,direct,defaults,children} =>
			 let fun mergeAnd (pat, andor) = mergeAndor(pat, elemty, rule, path, andor)
			  in List.revAppend(revprefix,
				(newKey,  (* same as the old key *)
				 AND{svar=svar, path=path, asvars=asvars, vars=vars,
				     direct = R.add(direct,rule), defaults = defaults,
				     children =
				      ListPair.map mergeAnd (pats, children)})
				:: rest)
			 end
		      | _ => bug "mergeVector"
		else merge (rest, vv::revprefix)
	      | merge (nil,revprefix) =  (* len does not match an existing lengths in variants *)
		let val newVariant = (newKey,
				      AND{svar = SV.newSvar(pathToString newPath, elemsTy),
					  path = newPath,
					  asvars = nil, vars = nil,
					  direct = R.singleton rule,
					  defaults = R.empty,
					  children = initAnd(pats,elemTys,rule,reversePath newPath)})
		 in rev(newVariant::revprefix)
		end
	      | merge _ = bug "mergeVector.merge"
	 in merge(variants,nil)
	end

    (* mergeDataConst : key * ty * ruleno * path * variant list -> variant list *)
    and mergeDataConst (key as D dcon, ty, rule, path, variants) =
	let fun merge ((variant as (key',andor))::rest, revprefix) =
		  if eqKey(key,key') (* same constant dcon, hence andor must be LEAF *)
		  then let val modifiedVariant =  (* merge with existing variant for this dcon *)
			       case andor
				 of LEAF{path,direct,defaults} =>  (* constant dcon *)
				    (key, LEAF{path=path,
					       direct=R.add(direct,rule),
					       defaults=defaults})
				    | _ => bug "mergeDataConst"
			 in List.revAppend (revprefix, modifiedVariant::rest)
			end
		  else merge(rest, variant::revprefix)
	      | merge (nil, revprefix) =  (* key dcon is new to the OR node *)
		  let val newpath = extendPath(path, key) (* path was the parent path *)
		      val newVariant =    (* new variant created *)
			    (key, LEAF{path=newpath, direct=R.singleton rule,
				       defaults=R.empty})
		   in rev (newVariant::revprefix)
		  end
	 in merge(variants, nil)
	end
      | mergeDataConst _ = bug "mergeDataConst: bad key"

    (* mergeData : key * pat * ty * ruleno * path * variant list -> variant list *)
    and mergeData (key as D dcon, pat, ty, rule, path, variants) =
	let val patTy = TU.destructCon(ty, dcon)
	    fun merge ((variant as (key',andor))::rest, revprefix) =
		  if eqKey(key,key')
		  then let val modifiedVariant =  (* merge with existing variant for this dcon *)
			       (key, mergeAndor(pat,patTy,rule,reversePath path,andor))
			 in List.revAppend (revprefix, modifiedVariant::rest)
			end
		  else merge(rest, variant::revprefix)
	      | merge (nil, revprefix) =  (* key dcon is new to the OR node *)
		  let val newpath = extendPath(path, key)
		      val newVariant =  (* new variant created *)
			    (key, mergeAndor(pat,patTy,rule,reversePath newpath,INITIAL))
		   in rev (newVariant::revprefix)
		  end
	 in merge(variants, nil)
	end
      | mergeData _ = bug "mergeData: bad key"

    (* makeAndor0 : pat list * T.ty -> andor *)
    (* ASSERT: length(pats) > 0 *)
    (* ASSERT: ty will the the type of all the patterns *)
    fun makeAndor0 (pats,ty) =
        #1 (foldl (fn (pat,(andor, rule)) =>
		      (mergeAndor(pat,ty,rule,rootRPath,andor), R.increment rule))
		  (INITIAL, 0) pats)

 in pushDefaults(R.empty, (makeAndor0 (pats, topTy)))

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

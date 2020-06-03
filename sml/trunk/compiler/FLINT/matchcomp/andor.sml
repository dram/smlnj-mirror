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

(* addConstChild: choiceKey * path * ruleno * (choiceKey * andor) list -> (choiceKey * andor) list *)
fun addConstChild (constKey, path, rule, nil) =  (* new constant variant *)
    [(const, LEAF{path = addPath(path, CL constKey), live=R.singleton rule})]
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
     * constructued by calls of mergePat. *)
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


    val initLive : ruleset = R.add(R.empty, 0)

    (* initAnd : pat list -> andor list
     * preserves order of pattern list *)
    fun initAnd (pats, path) =
	rev(#1 (foldl
		 (fn (pat,(andors,index)) => (initAndor(pat,addToPath(path, RL index))::andors, index+1))
		 (nil,0) pats)

    (* initAndor : pat * path -> andor *)
    (* build AND-OR tree from pattern of first rule
     * All the nodes get the same live ruleset: initLive = {0} *)
    and initAndor (VARpat var, path) =
	VARS{path = path,
	     vars = [(var,0)],
	     live = initLive}
      | initAndor (WILDpat, rule) =
          (* wildcard pat treated as a particular variable pattern *)
	VARS{path = path,
	     vars = [(genWildVar(),0)],
	     live = initLive}
      | initAndor (LAYEREDpat(var,_,basepat), path) = (* ignoring type constraint option *)
	  addAsBinding ((var,rule), initAndor (basepat, path))  (* no link added for AS basepat *)
      | initAndor (NUMpat(_, {ival, ty}), rule) =  (* how are int and word distinguished? *)
	  let val intKey = INTkey(ival,ty)  (* how about word? *)
	      val newpath = addToPath(path, CL(intKey))
	   in OR{path = path,
		 asvars = nil,
		 vars = nil,
		 live = initLive,
		 defaults = R.empty,
		 variants = [(intKey, LEAF{path=newpath,live=initLive})]
	  end
      | initAndor (STRINGpat s, path) =
	  let val newpath = addToPath(path, CL(constCon))
	   in OR{path = path,
		 asvars = nil,
		 vars = nil,
		 live = initLive,
		 defaults = R.empty,
		 variants = [(STRINGkey s, LEAF{path=newpath,live=initLive})]}
	  end
      | initAndor (CHARpat s, path) =
	  let val newPath = addToPath(path, CL(constCon))
	   in OR{path = path,
		 asvars = nil,
		 vars = nil,
		 live = 
		 defaults = R.empty,
		 variants = [(CHARkey(charCon s), LEAF{path=newpath, live=initLive})]}
	  end
      | initAndor (RECORDpat{fields,...}, path) =
	  AND{path = path,
	      asvars = nil,
	      vars = nil,
	      live = initLive,
	      defaults = R.empty,
	      children = (initAnd(map #2 fields, path)}
      | initAndor (CONpat(dcon,tvs), rule) =  (* constant datacon *)
	  let val newpath = addToPath(path, DL(dcon))
  	   in if singletonDatacon dcon
	      then SINGLE{path = path,
			  dcon = dcon,
			  asvars = nil,
			  vars = nil,
			  defaults = R.empty, ???
			  live = R.singleton rule,  (* redundant ??? *)
			  arg = LEAF{path = newpath, live = R.singleton rule}}
	      else OR{path = path,
		      asvars = nil,
		      vars = nil,
		      live = initLive,
		      defaults = R.empty,
		      variants = [(DATAkey(dcon, tvs), LEAF{path=newpath, live=initLive})]}
	  end
      | initAndor (APPpat(dcon,tvs,pat), rule) =
	let val newpath = insert(DL(dcon), path)
	 in if singletonDatacon dcon
	    then SINGLE{path = path,  (* SINGLE's arg gets the new link *)
			dcon = dcon,  (* there is only one *)
			asvars = nil,
			vars = nil,
			live = initLive,  (* redundant -- same as child *)
			defaults = R.empty, (* no variables on path, because 1 pattern *)
			arg = initAndor(pat,rule,newpath)}
	    else OR{path = path,
		    live = initLive,
		    defaults = R.empty,
		    asvars = nil,
		    vars = nil,
		    variants = [(DATAkey(dcon, tvs), initAndor(pat,rule,newpath))]}
	end
      | initAndor (VECTORpat(pats,ty), rule) =
	let val vlen = length pats
	    val newpath = insert(VL(vlen), path)
	in OR{path = path,
	      asvars = nil,
	      vars = nil,
	      live = initLive,
	      defaults = R.empty,
	      variants = ORvec (ty, [(vlen, AND(initAnd(pats, newpath)))])}
	end
      | initAndor _ =
	  bug "initAndor - unexpected pat arg"

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





    (* mergeAndor : pat * ruleno * andor -> andor *)
    (* merge the next pat at rule ruleno into the partially constructed andor tree 
     *  -- don't need to pass a path argument because paths will be found in andor arg?
     *  -- add ruleno to live set at each merged node
     *  -- will typically create new branches (links) under OR nodes, unless keys agree
     *     with an existing variant
     *  -- merging a variable pattern introduces a default rule that has to be propaged
     *     through the entire andor tree being merged into.
     *)
    and mergeAndor (VARpat v, rule, andor) =
	addVarBinding ((v,rule), andor)
          (* rule should be propagated into defaults for all nodes in andor!
           * Since the andor tree has already been build, this will have to be
           * done by altering it. Probably best to batch these default propagations
           * rather than doing it repeatedly for each variable pattern when encountered *)
      | mergeAndor (WILDpat, rule, andor) =
	  addVarBinding ((genWildVar(),rule), andor)
          (* same as for VARpat case *)
      | mergeAndor (CONSTRAINTpat(pat, _), rule, andor) =
	  (* disregard type constraints -- they have no role in pattern matching *)
	  mergeAndor(pat, rule, andor)
      | mergeAndor (LAYEREDpat(v,_,basepat), rule, andor) =
	  addAsBinding ((v,rule), mergeAndor (basepat, rule, andor))
      | mergeAndor (NUMpat(_, {ival, ty}), rule,
		    OR{path,asvars,vars,live,defaults,variants}) =
	  OR{path = path,
	     asvars = asvars,
	     vars = vars,
	     live = R.add(live,rule),
	     defaults = defaults,
	     variants = addConstChild(numCon(ival, ty), path, rule, variants)}
      | mergeAndor (STRINGpat s, rule,
		    OR{path,asvars,vars,live,defaults,variants}) =
	  OR{path = path,
	     asvars = asvars,
	     vars = vars,
	     live = R.add(live,rule),
	     defaults = defaults,
	     variants = addConstChild(STRINGkey s, path, rule, variants)}
      | mergeAndor (CHARpat s, rule,
		    OR{path,asvars,vars,live,defaults,variants}) =
	  OR{path = path,
	     asvars = asvars,
	     vars = vars,
	     live = R.add(live,rule),
	     defaults = defaults,
	     variants = ORconst (addConstChild(CHARkey(charCon s), path, rule, variants))}
      | mergeAndor (RECORDpat{fields,...}, rule, AND andors) =
	  (* arity of record and AND andor node are equal because they have the same type *)
	  AND (mergeAnd (map #2 fields, andors, rule))
      | mergeAndor (VECTORpat(pats,ty), rule,
		    OR{path,asvars,vars,live,defaults,variants})
	  (* ASSERT: ty and ty' are equal *)
	  OR{path = path,
	     asvars = asvars,
	     vars = vars,
	     live = R.add(live,rule),
	     defaults = defaults,
	     variants = ORvec (mergeVector (pats,rule,path,variants)))
      | mergeAndor (CONpat(dcon,tvs), rule,
		    OR{path,asvars,vars,live,defaults,variants)) =
	  OR{path = path,
	     asvars = asvars,
	     vars = vars,
	     live = R.add(live,rule),
	     defaults = defaults,
	     variants = mergeData (DATAkey(dcon,tvs), NONE, rule, path, variants))}
      | mergeAndor (APPpat(dcon,tvs,pat), rule,
		    OR{path,asvars,vars,live,defaults,children)) =
	  OR{path = path,
	     asvars = asvars,
	     vars = vars,
	     live = R.add(live,rule),
	     defaults = defaults,
	     variants = mergeData (DATAkey(dcon,tvs), SOME pats, rule, path,children))}
      | mergeAndor (pat, rule, VARS{path,asvars,vars,live}) =
	  mergeVars (pat, rule)
      | mergeAndor _ =  (* remaining cases impossible: incompatible types *)
	  bug "mergeAndor: incompatible pat and andor tree"

    (* mergeAnd : pat list * ruleno * andor list -> andor list *)
    and mergeAnd (nil, rule, nil) = nil
      | mergeAnd (pat::pats, rule, andor::andors) =
	 (mergeAndor(pat, rule, andor))::(mergeAnd(pats, rule, andors))
      | mergeAnd _ = bug "mergeAnd - list length mismatch" (* impossible *)

    (* mergeVector : pat list * rule * path * (int * andor) list -> (int * andor) list *)
    and mergeVector (pats,rule,path,children: (int * andor) list) : (int * andor) list =
	let val len = length pats (* could be 0 *)
	    fun merge (vbs as (vb as (len', AND andors)::rest), prefix) =
		if len < len'  (* new vector length *)
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
    and mergeData (dataKey, patOp, rule, path, variants) =
	let fun merge ((dataKey',andor)::rest, revprefix) =
		  if eqChoiceKey(dataKey,dataKey')
		  then let val child =
			       case (patOp, andorOp)
				 of (NONE, LEAF{path,live}) =>  (* constant dcon *)
				     (dataKey, LEAF{path=path, live=R.add(ruleset,rule)})
				  | (SOME pat, andor) => 
				     (dataKey, mergeAndor(pat,andor,rule))
				  | _ => bug "mergeData 1"
			 in List.revAppend (revprefix, child::rest)
			end
		  else merge(rest, db::revprefix)
	      | merge (nil, revprefix) =  (* dcon is new to the OR node *)
		  let val child =
			  case patOp
			    of NONE =>  (* constant dcon *)
			       (dataKey,
				LEAF{path=addToPath(path,DL(dcon)), live=R.singleton rule})
			     | SOME pat =>
			       (dataKey, initAndor(pat,rule,addToPath(path,DL(dcon))))
		   in rev (child::revprefix)
		  end
	      | merge _ = bug "mergeData 2"
	 in merge(children`, nil)
	end

    and mergeVars (pat, rule, VARS{path, asvars, vars, live}) =
        (* does live ruleset from the VARS node play a part?  
         * Is live = R.empty and invariant? *)
	let val andor0 = initAndor(pat, rule, path)
	    val andor1 = addVarBindings(vars, andor0)
	    val andor2 = addAsBindings(asvars, andor1)
	 in andor2
	end

    (* makeAndor0 : pat list -> andor *)
    fun makeAndor0 (pat::pats) =
	let val andor0 = initAndor(pat,rootPath)   (* rule 0 *)
	in #1 (foldl (fn (pat,(andor, rule)) =>
			 (mergeAndor(pat,rule,andor), next rule)) (andor0,next 0)) pats
	end
      | makeAndor0 nil = bug "makeAndor0" (* INVARIANT: pats arg not null *)

in pushDefaults(R.empty, (makeAndor0 pats))

end (* fun makeAndor *)

end (* local open *)

end (* structure AndOr *)

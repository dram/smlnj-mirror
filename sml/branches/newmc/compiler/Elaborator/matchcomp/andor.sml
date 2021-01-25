(* makeandor.sml *)

(* andor.sml *)

structure AndOr =
struct

local
  structure T = Types
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure LV = LambdaVar
  structure V = VarCon
  structure SV = SVar
  structure MU = MCUtil
  structure R = Rules
  structure PP = PrettyPrint
  open Absyn MCTypes
  (* also used: IntInf, Target, Char, String *)

  val say = Control_Print.say
  fun newline () = say "\n"
  fun bug msg = ErrorMsg.impossible msg

  fun ppType ty =
	PP.with_default_pp
	  (fn ppstrm => PPType.ppType StaticEnv.empty ppstrm ty)

  fun ppTypes tys =
        PP.with_default_pp
	  (fn ppstrm => PPUtil.ppBracketedSequence ("[", "]", PPType.ppType StaticEnv.empty) ppstrm tys)

  fun ppPat pat =
       PP.with_default_pp(fn ppstrm => PPAbsyn.ppPat StaticEnv.empty ppstrm (pat, 20))

in

(* numToKey : T.ty IntConst.t -> key *)
fun numToKey ((num as {ival,ty}): T.ty IntConst.t) : key =
    if List.exists (fn ty' => TU.equalType(ty,ty'))
		   [BT.intTy, BT.int32Ty, BT.int64Ty, BT.intinfTy]
    then I num
    else W num

(* node ids (: int) *)
val idcount = ref 0;
fun newId () = !idcount before (idcount := !idcount + 1)
fun resetId () = (idcount := 0)

fun addAsBind (b, {id,typ,path,vars,asvars}: AOinfo) =
    {id=id, typ=typ, path=path, vars=vars, asvars = asvars@b}

fun addBind (b, {id,typ,path,vars,asvars}: AOinfo) =
    {id=id, typ=typ, path=path, vars=vars@b, asvars = asvars}

(* addAsBinding : asBindings * andor -> andor *)
fun addAsBindings (b, AND{info,direct,defaults,children,andKind}) =
    AND{info=addAsBind(b, info), direct=direct, defaults=defaults,
	children=children, andKind=andKind}
  | addAsBindings (b, OR{info,direct,defaults,variants}) =
    OR{info=addAsBind(b,info), direct=direct, defaults=defaults, variants=variants}
  | addAsBindings (b, SINGLE{info,variant}) = 
    SINGLE{info=addAsBind(b,info), variant=variant}
  | addAsBindings (b, VARS{info,defaults}) = 
    VARS{info=addAsBind(b,info), defaults=defaults}
  | addAsBindings _ = bug "addAsBindings"
    (* as-binding for a (say) constant will be attached to the parent OR node *)
				   
(* addVarBindings : varBindings * andor -> andor *)
fun addVarBindings (b, AND{info,direct,defaults,children,andKind}) =
    AND{info=addBind(b,info), direct=direct, defaults=defaults,children=children,
	andKind=andKind}
  | addVarBindings (b, OR{info,direct,defaults,variants}) =
    OR{info=addBind(b,info), direct=direct, defaults=defaults, variants=variants}
  | addVarBindings (b, SINGLE{info,variant}) = 
    SINGLE{info=addBind(b,info), variant=variant}
  | addVarBindings (b, VARS{info,defaults}) = 
    VARS{info=addBind(b,info), defaults=defaults}
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
(* construct the AND-OR tree for the sequence of patterns of a match
 * patTy is the common type of all the patterns (i.e. the domain type of the match) *)
fun makeAndor (pats: pat list, patTy: T.ty) =
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
	 of OR{info,direct,defaults,variants} =>
	     (* ASSERT: defaults = R.empty *)
	     let val defaults' = R.addList(outerDefaults, map #2 (#vars info))
		 fun push (key,andor') = (key, pushDefaults(defaults', andor'))
	     in OR{info=info, direct=direct, defaults = defaults',
		   variants = map push variants}
	     end
	  | SINGLE{info,variant=(key,arg)} =>
	      let val defaults' = R.addList(outerDefaults, map #2 (#vars info))
	       in SINGLE{info=info, variant=(key, pushDefaults(defaults',arg))}
	      end
	  | AND{info,direct,defaults,children,andKind} =>
	      let val defaults' = R.addList(outerDefaults, map #2 (#vars info))
	      in AND{info=info, direct=direct, defaults=defaults', andKind=andKind,
		     children = map (fn andor => pushDefaults(defaults',andor)) children}
	      end
	  | VARS{info,defaults} =>
	      let val defaults' = R.addList(outerDefaults, map #2 (#vars info))
	       in VARS{info=info, defaults=defaults'}
	      end
	  | LEAF{path,direct,defaults} =>
	      (* ASSERT: defaults = R.empty *)
	      (* push defaults all the way down to LEAF nodes? Are defaults used here? *)
	      LEAF{path=path, direct=direct, defaults=outerDefaults}
	  | _ => bug "pushDefaults(INITIAL)"

    (* initAnd : pat list * T.ty list * ruleno * rpath -> andor list
     * Initializing an AND node, producing the children node list;
     * preserves the order of pattern list.
     *   pats: component patterns of a product;
     *   tys: types of product components
     *   path: path to the AND node
     *   ruleno = 0 and direct = {0} (because of mergeAndor with INITIAL) *)
    fun initAnd (pats, tys, rule, rpath) =
	if length pats <> length tys
	then bug (concat["length pats: ", Int.toString(length pats), " length tys: ",
			   Int.toString(length tys), "\n"])
	else
	rev(#1 (foldl
		 (fn ((pat,ty), (andors,index)) =>
		     (mergeAndor(pat, ty, rule, extendRPath(rpath, R index), INITIAL)::andors,
		      index+1))
		 (nil,0) (ListPair.zipEq(pats,tys))))

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
     *     of having separate pattern cases for INITIAL and non-INITIAL andors -- this
     *     version has duplicate cases for patterns depending on whether it is the
     *     initial pattern or not.
     *  -- Path and type arguments are only relevant to initialization of new nodes. Once
     *     a new node is initialized, its path and its type (in the svar) are fixed,
     *     and those fields do not change as additional patterns are merged at the node.
     *     An unfortunate effect of merging initAndor into mergeAndor is that the real
     *     "merge" cases for mergeAndor have to take two arguments (ty and path) that 
     *     are irrelevant (and ignored?) because they are only used in the initialization
     *     of a node.  Merging further patterns will not affect the path or type of a node. *)
    and mergeAndor (VARpat var, ty, rule, rpath, INITIAL) =
	let val path = reverseRPath rpath
	in VARS{info = {id = newId(), typ = ty, path = path, asvars = [], vars = [(var,rule)]},
		defaults = R.empty}
	end
      | mergeAndor (WILDpat, ty, rule, rpath, INITIAL) =
          (* wildcard pat treated as a particular variable pattern *)
	  let (* val _ = (say "mergeAndor:WILDpat\n") *)
	      val path = reverseRPath rpath
	   in VARS{info = {id = newId(), typ = ty, path = path, asvars = nil,
			   vars = [(V.wildVar,rule)]},
		   defaults = R.empty}
	  end
      | mergeAndor (LAYEREDpat(VARpat(var),basepat), ty, rule, rpath, INITIAL) =
	  (* ignoring type constraint option *)
	  addAsBindings ([(var,rule)], mergeAndor (basepat, ty, rule, rpath, INITIAL))
          (* no link added for the basepat *)
      | mergeAndor (NUMpat(_, numLiteral), ty, rule, rpath, INITIAL) =
	  (* ASSERT: ty should be an integer or word ty (as defined in BasicTypes; not checked)
           * ASSERT: ty should be equal to the ty component of numLiteral; not checked *)
	let val key = numToKey(numLiteral)
	              (* produces int (I) or word (W) key depending on ty *)
	      val path = reverseRPath rpath
	      val newRPath = extendRPath(rpath, key)
	      val newPath = reverseRPath newRPath
	  in OR{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil},
	        direct = R.singleton rule, defaults = R.empty,
	        variants = [(key, LEAF{path=newPath, direct=R.singleton rule,
				       defaults=R.empty})]}
	  end
      | mergeAndor (STRINGpat s, ty, rule, rpath, INITIAL) =
	  (* ASSERT: ty = BT.stringTy *)
	  let val key = S s
	      val path = reverseRPath rpath
	      val newRPath = extendRPath(rpath,key)
	  in OR{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil},
		direct = R.singleton rule, defaults = R.empty,
		variants = [(key,
			     LEAF{path=reverseRPath newRPath,
				  direct=R.singleton rule, defaults=R.empty})]}
	  end
      | mergeAndor (CHARpat c, ty, rule, rpath, INITIAL) =
	  (* ASSERT: ty = BT.charTy *)
	  let val key = C c
	      val path = reverseRPath rpath
	      val newRPath = extendRPath(rpath, key)
	      val newPath = reverseRPath newRPath
	  in OR{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil},
			direct = R.singleton rule, defaults = R.empty,
			variants = [(key, LEAF{path=newPath, direct=R.singleton rule,
					       defaults=R.empty})]}
	       (* QUESTION: adding the rule to _both_ the OR node and the descendent LEAF node? *)
	  end
      | mergeAndor (pat as RECORDpat{fields,...}, ty, rule, rpath, INITIAL) =
	  let val path = reverseRPath rpath
	      val ty = TU.headReduceType ty
(*
	      val _ = (say "mergeAndor:RECORDpat(INITIAL):ty: "; ppType ty; newline())
	      val _ = (say "mergeAndor:RECORDpat(INITIAL):pat: "; ppPat pat; newline())
*)
	      val elemTys = TU.destructRecordTy ty
			    handle e => (ppPat pat; ppType ty; raise e)
(*	      val _ = (say "mergeAndor:RECORD: field types: "; ppTypes elemTys; newline()) *)
	      val children = initAnd(map #2 fields, elemTys, rule, rpath)
(*	      val _ = say ("mergeAndor:RECORDpat:|children| = " ^ 
                             Int.toString(length children) ^ "\n")
*)
	   in AND{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil},
	          direct = R.singleton rule, defaults = R.empty,
	          children = children,
		  andKind = RECORD}
	  end
      | mergeAndor (CONpat(dcon,tvs), ty, rule, rpath, INITIAL) =  (* constant datacon *)
          (* ty is the (instantiated) type of the dcon *)
	  let val key = D (dcon, tvs)
	      val path = reverseRPath rpath
	      val newRPath = extendRPath(rpath, key)
	      val newPath = reverseRPath newRPath
	      val svar = SV.newSvar(pathToString path, ty)
  	   in if TU.dataconWidth dcon = 1  (* single datacon *)
	      then SINGLE{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil},
			  variant = (key, LEAF{path = newPath, direct = R.singleton rule,
					       defaults=R.empty})}
	      else OR{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil},
		      direct = R.singleton rule, defaults = R.empty,
		      variants = [(key, LEAF{path=newPath, direct=R.singleton rule,
					     defaults=R.empty})]}
	  end
      | mergeAndor (APPpat(dcon,tvs,pat), ty, rule, rpath, INITIAL) =
	  let (* val _ = print ">>>mergeAndor:APPpat\n" *)
	      val key = D (dcon,tvs)
	      val path = reverseRPath rpath
	      val newRPath : rpath = extendRPath(rpath, key)
	      (* val _ = print ">>>destructDataconTy\n" *)
	      (* val dconTy = TU.dataconType dcon
	         val _ = (print "mergeAndor:APPpat:dataconty = "; ppType dconTy) *)
	      val argty = TU.destructDataconTy(ty,dcon)
	      (* val _ = (print "mergeAndor:APPpat:ty: "; ppType ty;
		          print "<<<destructDataconTy:argTy: "; ppType argty) *)
	   in if TU.dataconWidth dcon = 1
	      then SINGLE{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil},
			  variant = (key, mergeAndor (pat,argty,rule,newRPath,INITIAL))}
	      else ((* print "mergeAndor:APPpat:OR\n"; *)
		    OR{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil},
		       direct = R.singleton rule, defaults = R.empty,
		       variants = [(key, mergeAndor (pat,argty,rule,newRPath,INITIAL))]})
	  end
      | mergeAndor (VECTORpat(pats,elemty), vecty, rule, rpath, INITIAL) =
	  (* Note: the vector node and its descendent AND(VECTOR) node share the
           * same svar, which is bound to the vector value. The vector OR node and
	   * the AND(VECTOR) node for its elements have the same vector type.
           * ASSERT: vecty = elemty vector *)
  	  let (* val _ = (say "mergeAndor[VECTORpat(INITIAL)]: vecty = "; ppType vecty;
		         say "; elemty = "; ppType elemty; newline()) *)
	      val vlen = length pats  (* vector length *)
	      val path = reverseRPath rpath  (* path for vector node *)
	      val newRPath = extendRPath(rpath, V(vlen))
	      val newPath = reverseRPath newRPath
	      val elemTys = TU.replicateTy(elemty, vlen)  (* list of replicated elemty *)
	      val variantNode =  (* FIX: how to ensure that OR and AND have same svar? *)
		  AND{info = {id = newId(), typ = vecty, path = newPath, asvars = nil, vars = nil},
		      direct = R.singleton rule, defaults = R.empty,
		      children = initAnd(pats, elemTys, rule, newRPath),
		      andKind = VECTOR}
	  in OR{info = {id = newId(), typ = vecty, path = path, asvars = nil, vars = nil},
		direct = R.singleton rule, defaults = R.empty,
		variants = [(V vlen, variantNode)]}
	  end

      (* ====== following rules merge a pattern into an existing andor node ====== *)

      | mergeAndor (VARpat v, ty, rule, rpath, andor) =
	  addVarBindings ([(v,rule)], andor)
          (* rule should be propagated into defaults for all nodes in andor!
           * Since the andor tree has already been build, this will be done
           * in a second pass by the function pushDefaults. *)
      | mergeAndor (WILDpat, ty, rule, path, andor) =
	  addVarBindings ([(V.newVALvar(Symbol.varSymbol "_", ty), rule)], andor)
          (* same as for VARpat case *)
      | mergeAndor (CONSTRAINTpat(pat, ty), _, rule, path, andor) =
	  (* disregard type constraints -- they have no role in pattern matching
           * and they have been checked by the type checker *)
	  mergeAndor(pat, ty, rule, path, andor)
      | mergeAndor (LAYEREDpat(VARpat v, basepat), ty, rule, rpath, andor) =
	  addAsBindings ([(v,rule)], mergeAndor (basepat, ty, rule, rpath, andor))
      | mergeAndor (NUMpat(_, numLiteral), ty, rule, _, 
		    OR{info,direct,defaults,variants}) =
	  OR{info = info,
	     direct = R.add(direct,rule), defaults = defaults,
	     variants = mergeConst(numToKey(numLiteral), rule, (#path info), variants)}
      | mergeAndor (STRINGpat s, _, rule, _,
		    OR{info,direct,defaults,variants}) =
	  OR{info = info,
	     direct = R.add(direct,rule), defaults = defaults,
	     variants = mergeConst(S s, rule, (#path info), variants)}
      | mergeAndor (CHARpat c, _, rule, _,
		    OR{info,direct,defaults,variants}) =
	  OR{info = info,
	     direct = R.add(direct,rule), defaults = defaults,
	     variants = mergeConst(C c, rule, (#path info), variants)}
      | mergeAndor (RECORDpat{fields,...}, _, rule, _,
		    AND{info,direct,defaults,children,andKind}) =
              (* mergeAnd : pat * andor -> andor *)
	  let fun mergeAnd (pat, andor) =
		  mergeAndor(pat, getType andor, rule, reversePath (#path info), andor)
	    (* arity of record and AND andor node are equal because they have the same type *)
	   in AND{info = info,
		  direct = R.add(direct,rule), defaults = defaults, andKind = andKind,
		  children = ListPair.map mergeAnd (map #2 fields, children)}
	  end
      | mergeAndor (VECTORpat(pats,elemty), vecty, rule, _,
		    OR{info,direct,defaults,variants}) =
	  (* ASSERT: vecty = elemty vector = varType svar
           * pattern and OR-node both have type vecty
           * svar will be bound to the vector value *)
	  OR{info = info,
	     direct = R.add(direct,rule), defaults = defaults,
	     variants = mergeVector (pats, vecty, elemty, rule, (#path info), variants)}
      | mergeAndor (CONpat(dcon,tvs), ty, rule, _,
		    OR{info,direct,defaults,variants}) =
	let val newVariants = mergeDataConst (D (dcon,tvs), ty, rule, (#path info), variants)
	 in OR{info = info,
	       direct = R.add(direct,rule), defaults = defaults, variants = newVariants}
	end
      | mergeAndor (CONpat (patDcon, tvs), _, rule, _,
		    SINGLE {info, variant=(key as D(dcon,_),arg)}) =
          if TU.eqDatacon (patDcon, dcon) then
	     (* this must be true, because dcon is singleton *)
	     (case arg
	       of LEAF {path=leafPath, direct, defaults} =>
		    let val newArg =
			    LEAF{path = leafPath, direct = R.add (direct, rule), defaults = defaults}
		     in SINGLE{info = info, variant = (key, newArg)}
		    end
		| _ => bug "mergeAndor:CONpat:SINGLE:arg")
	  else bug "mergeAndor:CONpat:SINGLE: dcons don't agree"
      | mergeAndor (APPpat(dcon,tvs,pat), _, rule, _,
		    OR{info,direct,defaults,variants}) =
	let val ty = #typ info
	    val newVariants = mergeData (D (dcon,tvs), pat, ty, rule, (#path info), variants)
	 in OR{info = info,
	       direct = R.add(direct,rule), defaults = defaults,
	       variants = newVariants}
	end
      | mergeAndor (APPpat (patDcon, tvs, argPat), ty, rule, rpath,
		    SINGLE{info, variant = (key as D(dcon,_), arg)}) =
          if TU.eqDatacon (patDcon, dcon) then  (* must be true, because dcon is singleton *)
	     let val argTy = TU.destructDataconTy (#typ info, patDcon)
		 val argRpath = extendRPath (rpath, D(patDcon,tvs)) (* == reverse(rpath of arg)? *)
		 val mergedArg = mergeAndor(argPat, argTy, rule, argRpath, arg) 
	      in SINGLE{info = info, variant = (key, mergedArg)}
	     end
	  else bug "mergeAndor:APPpat:SINGLE: dcons don't agree"
      | mergeAndor (ORpat(pat1,pat2), ty, rule, rpath, andor) =
	  mergeAndor(pat1, ty, rule, rpath,
		     mergeAndor(pat2, ty, rule, rpath, andor))
      | mergeAndor (pat, _, rule, _, VARS{info = {typ,path,vars,asvars,...}, defaults}) =
	  (* does direct ruleset from the VARS node play a part?  
	   * Is direct = R.empty an invariant? *)
	  let val andor0 = mergeAndor(pat, typ, rule, reversePath path, INITIAL)
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
    and mergeVector (pats, vecty, elemty, rule, path, variants: (key * andor) list)
	  : variant list =
	let val vlen = length pats (* could be 0 *)
	    val newKey = V vlen
	    val newPath : path = extendPath(path, newKey)
	    val elemTys = TU.replicateTy(elemty, vlen)  (* list of replicated elem tys *)
	    fun merge ((variants as ((vv as (V vlen', andor))::rest)), revprefix) =
		if vlen < vlen'  (* a new vector length, hence new vector variant *)
		then List.revAppend(revprefix,
				    (newKey,
				     AND{info = {id = newId(), typ = vecty, path = newPath,
						 asvars = nil, vars = nil},
					 direct = R.singleton rule,
					 defaults = R.empty,
					 children = initAnd(pats, elemTys, rule, path),
					 andKind = VECTOR})
				    ::variants)
		else if vlen = vlen' (* merge into existing variant *)
		then case andor
		      of AND{info,direct,defaults,children,andKind} =>
			 let fun mergeNode (pat, andor) =
				 mergeAndor(pat, elemty, rule, path, andor)
			  in List.revAppend(revprefix,
				(newKey,  (* same as the old key *)
				 AND{info = info,
				     direct = R.add(direct,rule), defaults = defaults,
				     children = ListPair.map mergeNode (pats, children),
				     andKind = VECTOR})
				:: rest)
			 end
		      | _ => bug "mergeVector"
		else merge (rest, vv::revprefix)
	      | merge (nil,revprefix) =
		  (* vlen does not match an existing length in variants *)
		let val newVariant =
			(newKey,
			 AND{info = {id = newId(), typ = vecty, path = newPath,
				     asvars = nil, vars = nil},
			     direct = R.singleton rule,
			     defaults = R.empty, andKind = VECTOR,
			     children = initAnd(pats,elemTys,rule,reversePath newPath)})
		 in rev(newVariant::revprefix)
		end
	      | merge _ = bug "mergeVector.merge"

	 in merge(variants,nil)
	end

    (* mergeDataConst : key * ty * ruleno * path * variant list -> variant list *)
    and mergeDataConst (key as D _, ty, rule, path, variants) =
	let fun merge ((variant as (key',andor))::rest, revprefix) =
		  if eqKey(key,key') (* same constant dcon, hence andor must be LEAF *)
		  then let val modifiedVariant =
			       (* merge with existing variant for this dcon *)
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
    (* the new key and an existing matching key would have different tvs (instantiation
     * metatyvars), but this doesn't matter since the type (ty) of the pattern node is
     * the same for all variants, so instantiations will be equivalent for the same 
     * dataconstructor. *)
    and mergeData (key as D (dcon,tvs), pat, ty, rule, path, variants) =
	let val patTy = TU.destructDataconTy(ty, dcon)
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
    fun makeAndor0 (pats, ty) =
        #1 (foldl (fn (pat, (andor, ruleno)) =>
		      (mergeAndor (pat,ty,ruleno,rootRPath,andor), R.increment ruleno))
		  (INITIAL, 0) pats)

    val andOr1 = makeAndor0 (pats, patTy)
    val andOr2 = pushDefaults (R.empty, andOr1)
 in andOr2
end (* fun makeAndor *)

end (* local open *)

end (* structure AndOr *)


(* TODO: vectors of length 0 and 1? *)

(* INVARIANT: variants for a vector OR-node are ordered by vector pattern length *)

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

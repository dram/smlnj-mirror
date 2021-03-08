(* nnew-andor.sml *)
(* see comments at bottom of file *)

structure AndOr =
struct

local
  structure T = Types
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure LV = LambdaVar
  structure V = VarCon
  structure SV = SVar
  structure K = Key
  structure MU = MCUtil
  structure LS = Layers.Set
  structure LL = LiveLayers
  structure R = Rules
  structure PP = PrettyPrint
  open Absyn MCTypes
  (* also used directly: IntInf, Target, Char, String *)

  val debugging = ElabControl.mcdebugging

  val say = Control_Print.say
  fun newline () = say "\n"
  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)

  fun bug msg = ErrorMsg.impossible ("MatchComp: " ^ msg)

  fun ppType ty =
	PP.with_default_pp
	  (fn ppstrm => PPType.ppType StaticEnv.empty ppstrm ty)

  fun ppTypes tys =
        PP.with_default_pp
	  (fn ppstrm =>
		PPUtil.ppBracketedSequence ("[", "]", PPType.ppType StaticEnv.empty)
					   ppstrm tys)

  fun ppPat pat =
       PP.with_default_pp(fn ppstrm => PPAbsyn.ppPat StaticEnv.empty ppstrm (pat, 20))

in

(* numToKey : T.ty IntConst.t -> key *)
fun numToKey ((num as {ival,ty}): T.ty IntConst.t) : K.key =
    if List.exists (fn ty' => TU.equalType(ty,ty'))
		   [BT.intTy, BT.int32Ty, BT.int64Ty, BT.intinfTy]
    then K.I num
    else K.W num

(* node ids (: int) *)
val idcount = ref 0;
fun newId () = !idcount before (idcount := !idcount + 1)
fun resetId () = (idcount := 0)

(* addBind : varBinding list * AOinfo -> AOinfo *)
fun addBind (b, {id,typ,path,vars,asvars}: AOinfo) =
    (dbsays [ ">> addBind: id = ", Int.toString id, "; |bind| = ", Int.toString(length b)];
     {id=id, typ=typ, path=path, vars=vars@b, asvars = asvars})

(* addAsBind : varBinding list * AOinfo -> AOinfo *)
fun addAsBind (b, {id,typ,path,vars,asvars}: AOinfo) =
    {id=id, typ=typ, path=path, vars=vars, asvars = asvars@b}

(* addVarBindings : varBindings * andor -> andor *)
fun addVarBindings (b, AND{info,andKind,children,live}) =
    AND{info=addBind(b,info), andKind=andKind, children=children, live = live}
  | addVarBindings (b, OR{info,variants,live}) =
    OR{info=addBind(b,info), variants=variants, live=live}
  | addVarBindings (b, SINGLE{info,variant}) = 
    SINGLE{info=addBind(b,info), variant=variant}
  | addVarBindings (b, VARS{info,layers}) =
    VARS{info=addBind(b,info), layers=layers}
  | addVarBindings _ = bug "addVarBindings"
    (* var-binding for a key LEAF will be attached to the parent OR node *)

(* addAsBinding : varBindings * andor -> andor *)
fun addAsBindings (b, AND{info,andKind,children,live}) =
    AND{info=addAsBind(b, info), andKind=andKind, children=children, live=live}
  | addAsBindings (b, OR{info,variants,live}) =
    OR{info=addAsBind(b,info), variants=variants, live=live}
  | addAsBindings (b, SINGLE{info,variant}) = 
    SINGLE{info=addAsBind(b,info), variant=variant}
  | addAsBindings (b, VARS{info,layers}) = 
    VARS{info=addAsBind(b,info), layers=layers}
  | addAsBindings _ = bug "addAsBindings"
    (* as-binding for a (say) constant will be attached to the parent OR node *)
				   
(* mergeConst: K.key * layer * path * variants -> variants *)
fun mergeConst (key, layer, path, variants) = 
    (case Variants.find (variants, key)
      of NONE =>  (* new constant variant *)
	 Variants.insert (variants, key, LEAF{path=extendPath(path, key), live=LL.newDirect layer})
       | SOME (LEAF{path=path',live}) =>
	 Variants.insert (variants, key, LEAF{path=path', live=LL.addDirect(layer, live)})
       | _ =>  bug "mergeConst")
			  
(* makeAndor : pat list * T.ty -> andor *)
(* construct the AND-OR tree for the sequence of patterns of a match
 * patTy is the common type of all the patterns (i.e. the domain type of the match) *)
fun makeAndor (pats: pat list, patTy: T.ty) =
let
    (* pushDefaults : LS.set * andor -> andor
     * Pushes down "default" layers introduced by variable patterns to all
     * the nodes in the andor tree to which the variable is attached by mergePat.
     * This is used as a post-processing phase after the initial andor is
     * constructued by calls of mergeAndor. It is expected that the AND-OR tree 
     * was constructed by the repeated calls of mergeAndor for each layer. *)
    fun pushDefaults (outerDefaults : LS.set, andor: andor) : andor =
	case andor
	 of OR{info,variants,live} =>  (* ASSERT: #defaults live = empty *)
	     let val newLayers = map #2 (#vars info)
		 val outerDefaults' = LS.addList (outerDefaults, newLayers)
		 fun push andor' = pushDefaults (outerDefaults', andor')
	      in OR{info=info, variants = Variants.map push variants,
		    live = LL.addDefaults (outerDefaults', live)}
	     end
	  | SINGLE{info,variant=(key,arg)} =>
	      let val newLayers = map #2 (#vars info)
		  val defaults' = LS.addList(outerDefaults, newLayers)
	       in SINGLE{info=info, variant=(key, pushDefaults(defaults',arg))}
	      end
	  | AND{info,andKind,children,live} =>
	      let val newLayers = map #2 (#vars info)
	          val outerDefaults' = LS.addList(outerDefaults, newLayers)
	       in AND{info=info, andKind=andKind,
		      children = map (fn andor => pushDefaults(outerDefaults',andor)) children,
		      live = LL.addDefaults (outerDefaults', live)}
	      end
	  | VARS{info,layers} =>
	      let val newLayers = map #2 (#vars info)
	          val defaults = LS.addList(outerDefaults, newLayers)
	       in VARS{info=info, layers = defaults}
	      end
	  | LEAF{path,live} =>
	      (* push defaults all the way down to LEAF nodes? Are defaults used here? *)
	      LEAF{path=path, live=LL.addDefaults(outerDefaults, live)}
	  | _ => bug "pushDefaults(INITIAL)"

    (* initAnd : pat list * T.ty list * layer * rpath -> andor list
     * Initializing an AND node, producing the children node list;
     * preserves the order of pattern list.
     *   pats: component patterns of a product;
     *   tys: types of product components
     *   path: path to the AND node *)
    fun initAnd (pats, tys, layer, rpath) =
	if length pats <> length tys
	then bug (concat["length pats: ", Int.toString(length pats), " length tys: ",
			   Int.toString(length tys), "\n"])
	else
	rev(#1 (foldl
		 (fn ((pat,ty), (andors,index)) =>
		     (mergeAndor(pat, ty, layer, extendRPath(rpath, K.R index), INITIAL)::andors,
		      index+1))
		 (nil,0) (ListPair.zipEq(pats,tys))))

    (* mergeAndor : pat * ty * layer * rpath * andor -> andor *)
    (* merge the next pat at layer into the partially constructed andor tree 
     *  -- don't need to pass a path argument because paths will be found in andor arg?
     *  -- We add layer to direct layerset at each merged node, except for VAR patterns.
     *  -- Will typically create new branches (variants) under OR nodes, unless keys agree
     *     with an existing variant, in which we merge the corresponding subpattern into
     *     that variant.
     *  -- Merging a variable pattern introduces a default rule that has to be propaged
     *     through the entire andor tree being merged into. Defaults arising from pvars
     *     will be  propagated into the default fields from vars fields by the
     *     pushDefaults pass.  Until pushDefaults is performed, all defaults are LS.empty.
     *  -- Path and type arguments are only relevant to initialization of new nodes. Once
     *     a new node is initialized, its path and its type (in the svar) are fixed,
     *     and those fields do not change as additional patterns are merged at the node.
     *     An unfortunate effect of merging initAndor into mergeAndor is that the real
     *     "merge" cases for mergeAndor have to take two arguments (ty and path) that 
     *     are irrelevant (and ignored?) because they are only needed in the initialization
     *     of a node (i.e. merging into INITIAL). Merging further patterns
     *     will maintain the same path and type of the initial creation of the node.
     *  -- Initialization of the andor tree is achieved by merging the first pattern
     *     into the special andor tree INITIAL, with path = rootPath and layer = (0,nil).
     *     (This was formerly a separate function, initAndor, but handling ORpats was cleaner
     *     with this single-pass method. The per-pattern case structure could be
     *     simplified by unifying treatment of INITIAL under each pattern case instead
     *     of having separate pattern cases for INITIAL and non-INITIAL andors -- this
     *     version has duplicate cases for patterns depending on whether it is the
     *     initial pattern or not. *)
    and mergeAndor (VARpat var, ty, layer, rpath, INITIAL) =
  	  VARS{info = {id = newId(), typ = ty, path = reverseRPath rpath, asvars = [],
		       vars = [(var,layer)]},
	       layers = LS.empty}
      | mergeAndor (WILDpat, ty, layer, rpath, INITIAL) =
          (* wildcard pat treated as a particular variable pattern *)
	  VARS{info = {id = newId(), typ = ty, path = reverseRPath rpath, asvars = nil,
		       vars = [(V.wildVar, layer)]},
	       layers = LS.empty}
      | mergeAndor (LAYEREDpat(VARpat(var),basepat), ty, layer, rpath, INITIAL) =
	  (* ignoring type constraint option *)
	  addAsBindings ([(var,layer)], mergeAndor (basepat, ty, layer, rpath, INITIAL))
          (* no link added for the basepat *)
      | mergeAndor (NUMpat(_, numLiteral), ty, layer, rpath, INITIAL) =
	  (* ASSERT: ty should be an integer or word ty (as defined in BasicTypes; not checked)
           * ASSERT: ty should be equal to the ty component of numLiteral; not checked *)
	  let val key = numToKey(numLiteral)
	              (* produces int (I) or word (W) key depending on ty *)
	      val path = reverseRPath rpath
	      val newRPath = extendRPath(rpath, key)
	      val newPath = reverseRPath newRPath
	      val live = LL.newDirect layer
	   in OR{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil}, live = live,
	         variants =
		   Variants.insert (Variants.empty key, key, LEAF{path=newPath, live = live})}
	  end
      | mergeAndor (STRINGpat s, ty, layer, rpath, INITIAL) =
	  (* ASSERT: ty = BT.stringTy *)
	  let val key = K.S s
	      val path = reverseRPath rpath
	      val newRPath = extendRPath(rpath,key)
	      val live = LL.newDirect layer
	  in OR{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil}, live = live,
		variants = Variants.insert (Variants.empty key, key, LEAF{path=reverseRPath newRPath, live=live})}
	  end
      | mergeAndor (CHARpat c, ty, layer, rpath, INITIAL) =
	  (* ASSERT: ty = BT.charTy *)
	  let val key = K.C c
	      val path = reverseRPath rpath
	      val newRPath = extendRPath(rpath, key)
	      val newPath = reverseRPath newRPath
	      val live = LL.newDirect layer
	  in OR{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil}, live = live,
		variants = Variants.insert (Variants.empty key, key, LEAF{path=newPath, live=live})}
	  end

(* QUESTION: In these constant key cases, we are adding the layer to _both_ the
 *    OR node and the descendent LEAF node? Is this necessary? *)

      | mergeAndor (pat as RECORDpat{fields,...}, ty, layer, rpath, INITIAL) =
	  let val path = reverseRPath rpath
	      val ty = TU.headReduceType ty
(*
	      val _ = (say "mergeAndor:RECORDpat(INITIAL):ty: "; ppType ty; newline())
	      val _ = (say "mergeAndor:RECORDpat(INITIAL):pat: "; ppPat pat; newline())
*)
	      val elemTys = TU.destructRecordTy ty
			    handle e => (ppPat pat; ppType ty; raise e)
(*	      val _ = (say "mergeAndor:RECORD: field types: "; ppTypes elemTys; newline()) *)
	      val children = initAnd(map #2 fields, elemTys, layer, rpath)
(*	      val _ = say ("mergeAndor:RECORDpat:|children| = " ^ 
                             Int.toString(length children) ^ "\n")
 *)
	   in AND{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil},
	          live = LL.newDirect layer, children = children, andKind = RECORD}
	  end

      | mergeAndor (CONpat(dcon,tvs), ty, layer, rpath, INITIAL) =  (* constant datacon *)
          (* ty is the (instantiated) type of the dcon *)
	  let val key = K.D (dcon, tvs)
	      val path = reverseRPath rpath
	      val newRPath = extendRPath(rpath, key)
	      val newPath = reverseRPath newRPath
	      val svar = SV.newSvar(pathToString path, ty)
	      val live = LL.newDirect layer
  	   in if TU.dataconWidth dcon = 1  (* single datacon *)
	      then SINGLE{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil},
			  variant = (key, LEAF{path = newPath, live = live})}
	      else OR{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil},
		      live = live,
		      variants =
		        Variants.insert (Variants.empty key, key, LEAF{path = newPath, live = live})}
	  end

      | mergeAndor (APPpat(dcon,tvs,pat), ty, layer, rpath, INITIAL) =
	  let (* val _ = print ">>>mergeAndor:APPpat\n" *)
	      val key = K.D (dcon,tvs)
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
			  variant = (key, mergeAndor (pat,argty,layer,newRPath,INITIAL))}
	      else ((* print "mergeAndor:APPpat:OR\n"; *)
		    OR{info = {id = newId(), typ = ty, path = path, asvars = nil, vars = nil},
		       live = LL.newDirect layer,
		       variants = Variants.insert (Variants.empty key, key,
						   mergeAndor (pat,argty,layer,newRPath,INITIAL))})
	  end

      | mergeAndor (VECTORpat(pats,elemty), vecty, layer, rpath, INITIAL) =
	  (* Note: the vector node and its descendent AND(VECTOR) node will share the
           * same svar, which is bound to the vector value itself. The vector OR node and
	   * the AND(VECTOR) node for its elements have the same vector type.
           * ASSERT: vecty = elemty vector *)
  	  let (* val _ = (say "mergeAndor[VECTORpat(INITIAL)]: vecty = "; ppType vecty;
		         say "; elemty = "; ppType elemty; newline()) *)
	      val vlen = length pats  (* vector length *)
	      val path = reverseRPath rpath  (* path for vector node *)
	      val vectorKey = K.V vlen
	      val newRPath = extendRPath(rpath, vectorKey)
	      val newPath = reverseRPath newRPath
	      val elemTys = TU.replicateTy(elemty, vlen)  (* list of replicated elemty *)
	      val live = LL.newDirect layer
	      val variantNode =  (* FIX: how to ensure that OR and AND have same svar? See MatchComp..genNode, etc. *)
		  AND{info = {id = newId(), typ = vecty, path = newPath, asvars = nil, vars = nil},
		      live = live,
		      children = initAnd(pats, elemTys, layer, newRPath),
		      andKind = VECTOR}
	   in OR{info = {id = newId(), typ = vecty, path = path, asvars = nil, vars = nil}, live = live,
		 variants = Variants.insert (Variants.empty (vectorKey), vectorKey, variantNode)}
	  end

      (* ====== following clauses merge a pattern into an existing andor node ====== *)

      | mergeAndor (VARpat v, ty, layer, rpath, andor) =
	  addVarBindings ([(v,layer)], andor)
          (* layer should be propagated into defaults for all nodes in andor!
           * Since the andor tree has already been built, this will be done
           * in a post-pass by the function pushDefaults. *)
      | mergeAndor (WILDpat, ty, layer, path, andor) =
	  addVarBindings ([(V.wildVar, layer)], andor)
          (* same as for VARpat case, but using V.wildVar, a "dummy" variable,
	   * to represent the wild card pattern*)
      | mergeAndor (CONSTRAINTpat(pat, ty), _, layer, path, andor) =
	  (* disregard type constraints -- they have no role in pattern matching
           * and they have been checked by the type checker *)
	  mergeAndor(pat, ty, layer, path, andor)
      | mergeAndor (LAYEREDpat(VARpat v, basepat), ty, layer, rpath, andor) =
	  addAsBindings ([(v,layer)], mergeAndor (basepat, ty, layer, rpath, andor))
      | mergeAndor (NUMpat(_, numLiteral), ty, layer, _, OR{info,variants,live}) =
	  OR{info = info, live = LL.addDirect(layer,live),
	     variants = mergeConst(numToKey(numLiteral), layer, (#path info), variants)}
      | mergeAndor (STRINGpat s, _, layer, _, OR{info,variants,live}) =
	  OR{info = info, live = LL.addDirect(layer,live),
	     variants = mergeConst(K.S s, layer, (#path info), variants)}
      | mergeAndor (CHARpat c, _, layer, _, OR{info,variants,live}) =
	  OR{info = info, live = LL.addDirect (layer,live),
	     variants = mergeConst(K.C c, layer, (#path info), variants)}
      | mergeAndor (RECORDpat{fields,...}, _, layer, _,
		    AND{info,andKind,children,live}) =
              (* mergeAnd : pat * andor -> andor *)
	  let fun mergeAnd (pat, andor) =
		  mergeAndor(pat, getType andor, layer, reversePath (#path info), andor)
	    (* arity of record and AND andor node are equal because they have the same type *)
	   in AND{info = info, andKind = andKind, live = LL.addDirect(layer,live),
		  children = ListPair.map mergeAnd (map #2 fields, children)}
	  end
      | mergeAndor (VECTORpat(pats,elemty), vecty, layer, _, OR{info,variants,live}) =
	  (* ASSERT: vecty = elemty vector = varType svar
           * pattern and OR-node both have type vecty
           * svar will be bound to the vector value *)
	  OR{info = info, live = LL.addDirect(layer, live),
	     variants = mergeVector (pats, vecty, elemty, layer, (#path info), variants)}
      | mergeAndor (CONpat(dcon,tvs), ty, layer, _, OR{info,variants,live}) =
	let val newVariants = mergeDataConst (K.D (dcon,tvs), ty, layer, #path info, variants)
	 in OR{info = info, live=LL.addDirect(layer,live), variants = newVariants}
	end
      | mergeAndor (CONpat (patDcon, tvs), _, layer, _,
		    SINGLE {info, variant=(key, arg)}) =
	  (case arg
	    of LEAF {path, live} =>
		 let val newArg = LEAF{path = path, live = LL.addDirect(layer,live)}
		  in SINGLE{info = info, variant = (key, newArg)}
		 end
	     | _ => bug "mergeAndor:CONpat:SINGLE:arg")

      | mergeAndor (APPpat(dcon,tvs,pat), _, layer, _, OR{info,variants,live}) =
	let val ty = #typ info
	    val newVariants = mergeData (K.D (dcon,tvs), pat, ty, layer, (#path info), variants)
	 in OR{info = info, variants = newVariants, live = LL.addDirect(layer,live)}
	end
      | mergeAndor (APPpat (patDcon, tvs, argPat), ty, layer, rpath,
		    SINGLE{info, variant = (key, arg)}) =
	  (* ASSERT: key = D (dcon,_) where dcon = patDcon *)
	  let val argTy = TU.destructDataconTy (#typ info, patDcon)
	      val argRpath = extendRPath (rpath, K.D(patDcon,tvs)) (* == reverse(rpath of arg)? *)
	      val mergedArg = mergeAndor(argPat, argTy, layer, argRpath, arg) 
	   in SINGLE{info = info, variant = (key, mergedArg)}
	  end
      | mergeAndor (ORpat(pat1,pat2), ty, layer, rpath, andor) =
	  mergeAndor(pat2, ty, Layers.extendRight layer, rpath,
		     mergeAndor(pat1, ty, Layers.extendLeft layer, rpath, andor))
      | mergeAndor (pat, _, layer, _, VARS{info = {typ,path,vars,asvars,...}, ...}) =
	  (* does live layerset from the VARS node play a part?  
	   * Is live = LS.empty an invariant? *)
	  let val andor0 = mergeAndor(pat, typ, layer, reversePath path, INITIAL)
	          (* This will generate new svar; should we be preserving the svar
                   * of the existing VARS node? *)
	      val andor1 = addVarBindings(vars, andor0)
	      val andor2 = addAsBindings(asvars, andor1)
	   in andor2
	  end
      | mergeAndor _ =  (* remaining cases impossible: incompatible types *)
	  bug "mergeAndor: incompatible pat and andor tree"

   (* mergeVector : pat list * layer * path * variants -> variants *)
   (* maintains vector variants in ascending order by length *)
    and mergeVector (pats, vecty, elemty, layer, path, variants: variants) : variants =
	let val vlen = length pats (* could be 0 *)
	    val key = K.V vlen
	    val newPath : path = extendPath(path, key)
	    val elemTys = TU.replicateTy(elemty, vlen)  (* list of replicated elem tys *)
	in case Variants.find (variants, key)
	    of NONE =>
	         let val andor' = 
			 AND{info = {id = newId(), typ = vecty, path = newPath,
				     asvars = nil, vars = nil},
			     live = LL.newDirect layer,
			     children = initAnd(pats, elemTys, layer, reversePath newPath),
			     andKind = VECTOR}
		 in Variants.insert (variants, key, andor')
		 end
	     | SOME (AND{info,andKind,children,live}) =>
	         let fun mergeNode (pat, andor) =
			 mergeAndor(pat, elemty, layer, path, andor)
		     val andor' =
			   AND{info = info, live = LL.addDirect(layer,live),
			       children = ListPair.map mergeNode (pats, children),
			       andKind = VECTOR}
		  in Variants.insert (variants, key, andor')
		 end
	     | _ => bug "mergeVector"
	end

    (* mergeDataConst : key * ty * layer * path * variants -> variants *)
    (* merging a _constant_ datacon (the key) into existing variants of an OR node *)
    and mergeDataConst (key, ty, layer, path, variants) =
	(case Variants.find (variants, key)
	   of NONE => (* new variant *)
		let val newpath = extendPath(path, key) (* path was the parent path *)
		    val newVariant = LEAF{path=newpath, live=LL.newDirect layer}
		 in Variants.insert (variants, key, newVariant)
		end
	    | SOME andor => (* variant key already occurs *)
		let val modifiedVariant =
				(* merge with existing variant for this dcon *)
			case andor
			 of LEAF{path,live} =>  (* constant dcon *)
			      LEAF{path=path, live=LL.addDirect(layer,live)}
			  | _ => bug "mergeDataConst"
		 in Variants.insert(variants, key, modifiedVariant)
		end)

    (* mergeData : key * pat * ty * layer * path * variant list -> variant list *)
    (* the new key and an existing matching key would have different tvs (instantiation
     * metatyvars), but this doesn't matter since the type (ty) of the pattern node is
     * the same for all variants, so instantiations will be equivalent for the same 
     * dataconstructor. *)
    and mergeData (key as K.D (dcon,tvs), pat, ty, layer, path, variants) =
	let val patTy = TU.destructDataconTy(ty, dcon)
	 in case Variants.find (variants, key)
	     of NONE => 
		   let val newpath = extendPath(path, key)
		       val andor' =  (* new variant andor created *)
			     mergeAndor(pat,patTy,layer,reversePath newpath,INITIAL)
		    in Variants.insert (variants, key, andor')
		   end
		| SOME andor =>   (* merge with existing variant for this dcon *)
		    let val andor' = mergeAndor(pat,patTy,layer,reversePath path,andor)
		     in Variants.insert (variants, key, andor')
		    end
	end
      | mergeData _ = bug "mergeData: bad key"

    (* makeAndor0 : pat list * T.ty -> andor *)
    (* ASSERT: length(pats) > 0 *)
    (* ASSERT: ty will the the type of all the patterns *)
    fun makeAndor0 (pats, ty) =
	(resetId ();
         #1 (foldl (fn (pat, (andor, ruleno)) =>
		       (mergeAndor (pat, ty, Layers.fromRule ruleno, rootRPath, andor),
			R.increment ruleno))
		   (INITIAL, 0) pats))

    val andOr1 = makeAndor0 (pats, patTy)
    val andOr2 = pushDefaults (LS.empty, andOr1)
	 (* propagating variable default layers down into live fields of the AndOr tree *)

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

(* FLINT/trans/generate.sml *)
(* revised "old" match compiler *)

(* generation of "code" (in the form of PLambda.lexp) from decision trees (type dectree) *)

structure Generate = 
struct

local
  structure S = Symbol
  structure SP = SymPath
  structure T = Types
  structure BT = BasicTypes
  structure TU = TypesUtil
  structure A = Access
  structure LV = LambdaVar
  structure V = VarCon
  structure AS = Absyn
  structure AU = AbsynUtil
  structure EU = ElabUtil
  structure MT = MCCommon
  structure DT = DecisionTree
  structure PO = Primop
  structure PL = PLambda
  structure LT = PLambdaType  (* LTextern *)
  structure PP = PrettyPrint
  structure PU = PPUtil
  structure ED = ElabDebug
  open MCCommon

  val debugging = Control.MC.debugging
  val stats = Control.MC.stats

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun says strings = say (concat strings)
  fun saysnl strings = saynl (concat strings)

  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)

  fun bug msg = ErrorMsg.impossible ("Generate: " ^ msg)

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
	       PPAbsyn.ppExp (StaticEnv.empty, NONE) ppstrm (exp, 100);
	       PP.newline ppstrm))

  fun ppDec (dec, msg) =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm msg;
	       PPAbsyn.ppDec (StaticEnv.empty, NONE) ppstrm (dec, 100);
	       PP.newline ppstrm))

  fun ppPat pat =
      PP.with_default_pp(fn ppstrm => PPAbsyn.ppPat StaticEnv.empty ppstrm (pat, 20))

  fun ppVar var =
      PP.with_default_pp(fn ppstrm => PPVal.ppVar ppstrm var)

  fun ppType msg ty =
      PP.with_default_pp
	(fn ppstrm => (PP.string ppstrm (msg^": "); PPType.ppType StaticEnv.empty ppstrm ty))

  fun ppLexp (lexp, msg) =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm msg;
	       PPLexp.ppLexp 100 ppstrm lexp;
	       PP.newline ppstrm))

  (* intCon : int -> PL.con *)
  fun intCon n = PL.INTcon {ival = IntInf.fromInt n, ty = Target.defaultIntSz}

  val markexn = PL.PRIM(PO.MARKEXN,
		     LT.ltc_parrow(LT.ltc_tuple [LT.ltc_exn, LT.ltc_string],
				    LT.ltc_exn), [])
in

(* How should we treat SINGLE constructor patterns, and in particular the
 * "special" ones like *ref* (and *susp* )?  We generate a special single
 * datacon "deconstructor" (expressed as a single-variant SWITCHexp) in these cases.
 * The special cases (ref,susp) are detected and handled in Translate
 * (FLINT/trans/translate.sml).
 * SWITCHexp translates almost directly to Plambda.SWITCH.
 *
 * Also need to deconstruct AND and SINGLE _below_ a terminal OR/Decision node,
 * since variables may occur below the node. This is done by the call of
 * genAndor within the body of genDecTree.
 *)

(* identity expression continuation *)
fun k_ident (lexp: PL.lexp) = lexp

(* -------------------------------------------------------------------------------- *)
(* mvarenv : environments mapping nodeIds to lvars *)

(* lvar analogue of mvarenv *)
structure M = IntBinaryMap

(* mvarenv: a finite mapping from AndOr node id numbers (andor.info.id) to
 * "mvars", which are lvars used as "administrative" variables in the match compiler
 * to represent values produced during value destruction. The do not derive from the pattern variables,
 * but in the end each pattern varialbe (for a given rule) will be associated with some mvar, which
 * in turn will be bound do the pattern variable's "matching value". *)
type mvarenv = LV.lvar M.map

val empty = M.empty

(* bindMvar : nodeId * LV.lvar * mvarenv -> mvarenv *)
fun bindMvar (id: int, svar: LV.lvar, env: mvarenv) =
    M.insert(env, id, svar)

(* lookMvar : mvarenv * nodeId -> LV.lvar option *)
fun lookMvar (env: mvarenv, id: nodeId) = M.find (env, id)

(* -------------------------------------------------------------------------------- *)
(* genMatch: main match "code generation" function *
 * top level code generating function for matches (formerly MCCode.genCode) *)

type mvar = LV.lvar  (* == int *)
fun mkv () = LV.mkLvar ()

(* "hybrid" rule -- rhs has been translated to an lexp *)
type hrule = AS.pat * PL.lexp

(* genMatch : hrule list * andor * MT.decTree * T.ty * lvar * pvarmap * (int -> (pvar list * fvar * ruleno))
              * PL.lexp * string option * toTcLt * giisTy
	      -> PL.lexp *)
fun genMatch (andor, decTree, rootvar, pvarmap, pvarsFvarRule,
	      (failExnLexp, rhsTy, location), (toTyc, toLty), genIntInfSwitch) =
    let val _ = dbsay ">> genMatch"

(*
    val _ = dbsays ["** genMatch: ruleCounts: ",
		    (PrintUtil.listToString ("[", ",", "]") Int.toString ruleCountsList),
		    ", length rules: ", Int.toString (length rules)]
	     
    val _ = dbsays ["multirules = ",
		    (PrintUtil.listToString ("[", ",", "]") Int.toString multirules)]
*)
     (* nodeIdFind : Andor.pvarmap * V.var * ruleno -> nodeId option *)
     fun nodeIdFind (pvarmap: Andor.pvarmap, pvar, ruleno) =
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

     (*  mkRHS : ruleno * mvarenv * pvarmap -> PL.lexp *)
     (* invoking the rule RHS function (abstracted rhs) on pattern variable mvars *)
     fun mkRHS (ruleno, mvarenv, pvarmap) =
	   let val (pvars, fvar, _) = pvarsFvarRule ruleno
	       fun lookupPvar (pvar: V.var) : PL.lexp = 
		    (case nodeIdFind (pvarmap, pvar, ruleno)
		       of NONE => bug "lookupPvar: (pvar, ruleno) not found in pvarmap"
			| SOME id =>
			   (case lookMvar (mvarenv, id)
			      of NONE => bug "lookupPvar: id not found in mvarenv"
			       | SOME mvar => PL.VAR mvar))
	    in case pvars
	         of [pvar] => PL.APP (PL.VAR fvar, lookupPvar pvar)
		  | pvars =>  (* multiple bound pvars, including none *)
		      PL.APP(PL.VAR fvar, PL.RECORD (map lookupPvar pvars))
	   end

    (* conToCon : MT.con * mvar option -> PL.con
     *  translates MCCommon.con to PLambda.con and introduces a variable naming
     *  the destruct of a datacon-headed value (even if the datacon is a constant!)
     *  mvarOp will be SOME mvar if the con is a non-constant datacon *)
    fun conToCon (con, mvarOp) =  (* needs toLty and toTyc *)
    (* transDcon : T.datacon -> PL.dataconstr
     *  uses toLty arg of generate; used only in conToCon *)
	let fun transDcon (T.DATACON {name, rep, typ, ...}: T.datacon) : PL.dataconstr =
	    let val lty = (* translation of the datacon type *)
		    (case typ
		       of T.POLYty{sign, tyfun=T.TYFUN{arity, body}} =>
			  if BT.isArrowType body then toLty typ
			  else toLty (T.POLYty{sign=sign,
						tyfun=T.TYFUN{arity=arity,
							       body=BT.-->(BT.unitTy, body)}})
			| _ => if BT.isArrowType typ then toLty typ
			       else toLty (BT.--> (BT.unitTy, typ)))
	     in (name, rep, lty)
	    end
	 in case con
	      of MT.DATAcon (datacon, ts) =>
		  let val nts = map (toTyc o T.VARty) ts
		      val mvar = getOpt (mvarOp, mkv())
			  (* get argument mvar, if provided, otherwise make a fresh one (not used) *)
		   in PL.DATAcon (transDcon datacon, nts, mvar)
		  end
	       | MT.VLENcon(i, t) => intCon i
	       | MT.INTcon i => PL.INTcon i
	       | MT.WORDcon w => PL.WORDcon w
	       | MT.STRINGcon s => PL.STRINGcon s
	end

    (* mkLetr : mvar list * mvar * lexp -> lexp *)
    (* "let (mv1,...,mvn) = mv0 in body";  destruct a record/tuple *)
    fun mkLetr (mvars, mvar, body) =
	let val mvarVAR = PL.VAR mvar
	    fun wrapLets (nil, _) = body
	      | wrapLets (mv::rest, n) =
		  PL.LET (mv, PL.SELECT (n, mvarVAR), wrapLets (rest, n+1))
	 in wrapLets (mvars, 0)  (* selection index 0 based *)
	end

    (* mkLetv : mvar list * mvar * lexp -> lexp *)
    (* "let #[sv1,...,svn] = sv0 in body";  destruct a vector *)
    (* mvar is bound to the vector value. mvars are to be bound to the vector
     * elements. *)
    fun mkLetv (mvars, mvar, body, ty) =
	let val defvar = PL.VAR mvar
	    fun wrapLets (nil, _) = body
	      | wrapLets (mv::rest, n) =
		let val tyc = toTyc ty
		    val lt_sub =
			let val x = LT.ltc_vector (LT.ltc_tv 0)
			in LT.ltc_poly([LT.tkc_mono],
				       [LT.ltc_parrow(LT.ltc_tuple [x, LT.ltc_int],
						      LT.ltc_tv 0)])
			end
		    val index_n = PL.INT {ival = IntInf.fromInt n, ty = Target.defaultIntSz}
		    val element_n =
			PL.APP(PL.PRIM(PO.SUBSCRIPTV, lt_sub, [tyc]),
			       (* apply vec. subscript primop *)
			       PL.RECORD [ PL.VAR mvar, index_n ]) (* (vector,index) *)
		 in PL.LET (mv, element_n, wrapLets (rest, n+1))
		end
	in wrapLets (mvars, 0)
	end

    (* Switch : mvar * (MT.con * PL.lexp) list * lexp option -> lexp  -- needs toTyc *)
    (* The default is now incorporated into the SWITCH. It should always be SOME
     * if the patterns in rules are not exhaustive.
     * -- Have to detect the vector length switch case, compute the length as the
     *    subject of an INTcon switch, and extract the vector element
     *    type from the VLENcon constructor. Then generate code to calculate the length of
     *    the vector and bind it to an fresh mvar (lenMvar) used as the switch subject.
     * -- Handles ref and susp constructors as special cases. *)

    fun mkSwitch (mvar: mvar, sign, cases: (MT.con * mvar option * PL.lexp) list, defaultOp: PL.lexp option) =
	let val subject = PL.VAR mvar
	    fun transCase (con, mvarOp, lexp) = (conToCon (con, mvarOp), lexp)
	 in case cases
	      of nil => bug "Switch: empty cases"
	       | (con, mvarOp, lexp) :: rest => 
	         (case (con, mvarOp)
		    of (MT.VLENcon (n,ty), SOME mvar) => (* switch on vector length(s) *)
			 (* "let val len = Vector.length mvar in <<switch over int values of len>>"
			  * where "len" is a fresh internal variable -- this is generated in 
			  * the VSWITCHexp case of Translate.mkExp0 to avoid the problem of
			  * accessing the vector length primop in absyn.
			  * -- Should there (always?) be a default lexp? *)
		         let val tyc = toTyc ty  (* vector element type *)
			     val lt_len = LT.ltc_poly([LT.tkc_mono],
					      [LT.ltc_parrow(LT.ltc_tv 0, LT.ltc_int)])
			     val argtyc = LT.tcc_vector tyc
			     val lenMvar = mkv ()
			     fun transVecCase (VLENcon(n, _), _, lexp) = (intCon n, lexp)
			       | transVecCase _ = bug "Switch:vector case: con not VLENcon"
			 in PL.LET (lenMvar,  (* bind lenMvar to the computed vector length *)
				    PL.APP (PL.PRIM(PO.LENGTH, lt_len, [argtyc]),  (* apply vec length primop *)
					    PL.VAR mvar), (* mvar designates the vector *)
				    PL.SWITCH (PL.VAR lenMvar, A.CNIL, map transVecCase cases, defaultOp))
			 end
		     | (MT.INTcon{ty=0, ...}, NONE) => (* switch on IntInf constant(s) *)
		         let fun strip (MT.INTcon{ty=0, ival}, NONE, lexp) = (ival, lexp)
			       | strip _ = bug "genswitch - INTINFcon"
			  in case defaultOp
			      of NONE => bug "Switch - no default in switch on IntInf"  (* ??? *)
			       | SOME d => genIntInfSwitch (subject, map strip cases, d)
			 end
		    |  _ =>  (* the general case, dispatching on the con *)
		       let val plcon = conToCon (con, mvarOp)  (* convert to PLambda con *)
                        in case plcon  (* first check for REF and SUSP special cases *)
		             of PL.DATAcon((_, A.REF, lty), tycs, lvar) => (* ref constructor *)
				(case rest (* check there is only one case *)
				  of nil =>
			             PL.LET(lvar, PL.APP (PL.PRIM (Primop.DEREF, LT.lt_swap lty, tycs), subject), lexp)
				   | _ => bug "mkSwitch: ref not singleton case")
			      | PL.DATAcon((_, A.SUSP(SOME(_, A.LVAR f)), lty), tycs, lvar) =>
				  (* SUSP constructor; "force" the suspension function *)
				(case rest
				   of nil => (* check there is only one case *)
				      let val v = mkv ()
				       in PL.LET(lvar, PL.LET(v, PL.TAPP(PL.VAR f, tycs), PL.APP(PL.VAR v, subject)), lexp)
				      end
				    | _ => bug "mkSwitch: susp not singleton case")
			      | _ => PL.SWITCH (subject, sign, map transCase cases, defaultOp)
		       end)
	end (* fun mkSwitch *)

    (* genNode: andor * mvarenv * (lexp -> lexp)
                 -> mvarenv * (lexp -> lexp) *)
    (* Translates top non-OR structure, i.e. AND structure, (if any) into nested let
     * expressions that are wrapped around a body expression (generated by a decision tree
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

    (* genNodes : andor list * mvarenv * (lexp -> lexp) -> mvarenv * (lexp -> lexp) *)
    fun genNodes (nodes: andor list, mvarenv: mvarenv, k: (PL.lexp -> PL.lexp)) =
	let fun genf (node, (mvarenv0, k0)) = genNode (node, mvarenv0, k0)
	 in foldr genf (mvarenv, k) nodes
	end
    
    (* genList : andor  list * mvarenv * (lexp -> lexp) -> mvar list * mvarenv * (lexp -> lexp) *)
    and genList (andors, mvarenv, k) =
	let fun genFreshMvar (andor, (mvars, mvarenv)) =
		let val id = getId andor
		    val freshmvar = mkv ()  (* fresh mvar for this andor *)
		 in (freshmvar :: mvars, bindMvar(id, freshmvar, mvarenv))
		end
	    val (freshmvars, mvarenv0) = foldr genFreshMvar ([], mvarenv) andors
	    val (mvarenv1, k1) = genNodes(andors, mvarenv0, k)
	 in (freshmvars, mvarenv1, k1)
	end

    (* genNode : andor * mvarenv * (exp -> exp) -> mvarenv * (exp -> exp)  *)
    (* INVARIANT: node id is already bound in mvarenv *)
    and genNode (AND {loc = {id,...}, children}, mvarenv, k) =
	  (dbsays [">> genNode:AND: ", Int.toString id];
	   case lookMvar (mvarenv, id)
	     of NONE => bug "genNode:AND: no mvar"
	      | SOME mvar =>
		  let val (mvars, mvarenv1, k1) = genList (children, mvarenv, k)
		   in (mvarenv1, (fn inner => mkLetr (mvars, mvar, k1 inner)))
		  end)
      | genNode (OR {loc = {id,...},...}, mvarenv, k) =
        (* this OR node is already accounted for in the dectree (if not redundant) *)
          (dbsays [">> genNode:OR: ", Int.toString id];
	   (mvarenv, k))
      | genNode (VAR {loc = {id,...}, ...}, mvarenv, k) =
          (dbsays [">> genNode:VAR: ", Int.toString id];
	   (mvarenv, k))

    (* genDecTree: decTree * mvarenv -> PL.lexp *)
    (* test with ?/t6.sml *)
    fun genDecTree (decTree, mvarenv) =
	(case decTree
	   of MT.CHOICE{andor, sign, cases = cases_CHOICE, default} =>
		(case andor    (* ASSERT: node must be OR *)
		  of OR {loc = {id,...}, cases = cases_OR, ...} =>
		     (* cases and variants should be "congruent", since choices is
		      * created as a Variants.map over variants, but we insure coordination
		      * by defining aoVariants and dtVariants bellow, which will be
		      * guaranteed to have keys in the same order. *)
		     (case lookMvar (mvarenv, id)
			of NONE => bug "genDecTree:CHOICE: mvar at this id"
		         | SOME mvar =>  (* mvar represents choice scrutinee value *)
			   (* switchBody: variant list * (MT.con * dectree) list
			                  * (MT.con * mvar option * PL.lexp) list
                                          -> (MT.con * mvar option * PL.lexp) list *)
			   let fun switchBody ((con, _, subcase) :: nrest, (con', decTree0) :: drest, sbody) =
				    (* ASSERT: con = con' -- the variant lists should be coordinated *)
				    let val _ = if not (conEq (con, con'))  (* verifying ASSERT above *)
						then bug "genDecTree:CHOICE:keys disagree"
						else ()
					(* val _ = say (concat ["genDecTree.switchBody:con = ",
					                        conToString con, "\n"]) *)
					val (mvarOp, caseLexp) =  (* argument svar + subtree *)
					    (case subcase   (* variant node assoc. with con *)
					      of CONST =>
						   (NONE, genDecTree(decTree0, mvarenv))
					       | VEC elements => (* => con = VLENcon(len,ty) *)
						   let val (elementMvars, mvarenv1, k0) =
							   genList (elements, mvarenv, k_ident)
							   (* destruct the vector elements, binding new mvars *)
						       val k1 =
							   (case con
							      of MT.VLENcon(_,elemty) => 
								 (fn inner =>
								    mkLetv (elementMvars, mvar, k0 inner, elemty))
							       | _ => bug "genDecTree: expected VLENcon")
						       val baseLexp = genDecTree (decTree0, mvarenv1)
						    in (SOME mvar, k1 baseLexp)
						       (* mvar is bound to the vector itself, inherited from OR *)
						   end
					       | DCON argNode =>  (* con is non-constant datacon *)
						   let val argMvar = mkv () (* fresh mvar0 for node0 *)
						       val mvarenv0 = bindMvar(getId argNode, argMvar, mvarenv)
						       val (mvarenv1, k_node0) =
							   genNode(argNode, mvarenv0, k_ident)
                                                           (* destruct node0, binding new mvars *)
						       val baseLexp = genDecTree(decTree0, mvarenv1)
						    in (SOME argMvar, k_node0 baseLexp)
						   end)
				     in switchBody(nrest, drest, (con, mvarOp, caseLexp)::sbody)
				    end
				  | switchBody (nil,nil,sbody) = rev sbody
				  | switchBody _ = bug "code.genDecTree.switchBody"
				val switchCases = switchBody(cases_OR, cases_CHOICE, nil)
				val defaultOp = Option.map (fn dt => genDecTree (dt, mvarenv)) default
			     in mkSwitch (mvar, sign, switchCases, defaultOp)
				(* mkSwitch detects and handles the vector length case *)
			    end)
		   | _ => bug "genDecTree: CHOICE node not an OR node")
            | MT.FAIL =>
	        let val lty = toLty rhsTy
		    val lexp' =
			if !Control.trackExn
			then PL.APP (markexn, PL.RECORD[failExnLexp, PL.STRING location])
			else failExnLexp
		in PL.RAISE (lexp', lty)
		end
	    | MT.RHS ruleno => mkRHS (ruleno, mvarenv, pvarmap)
	        (* dispatch to (i.e. call) appropriate rhs function for this ruleno *)
	(* end case *))
        (* end genDecTree *)

    and genCases (nil, mvarenv) = nil
      | genCases ((con, dectree)::rest, mvarenv) = 
	  let val ncon: PL.con = conToCon con
	      val caseLexp = genDecTree (dectree, mvarenv)
	   in (ncon, caseLexp) :: genCases (rest, mvarenv)
	  end

    val mvarenvInit = bindMvar (getId andor, rootvar, empty)
    val (mvarenv', k_andor) = genNode (andor, mvarenvInit, k_ident)
    val topLexp = genDecTree (decTree, mvarenv')
    val mainExp = k_andor topLexp

 in mainExp
end (* fun genMatch *)

end (* local *)
end (* structure MatchComp *)

(* obsolete code

(* subsumed under general OR -- special case where there is 1 variant, because 1 con *)
	      | genNode (SINGLE {info, variant = (key, arg)}, mvarenv, k) =
                (dbsays [">> genNode:SINGLE: ", Int.toString (#id info), ", ",
			 Int.toString (length (#vars info))];
		  case lookMvar (mvarenv, #id info)
		    of NONE => bug "genNode:SINGLE: no mvar"
                     | SOME thisMvar =>
			 (case arg
			    of LEAF _ => (mvarenv, varenvMC', k)
			     | _ =>  (* neither LEAF nor INITIAL *)
				  let val argLoc = getLoc arg (* defined -- not LEAF, INITIAL *)
				      val argMvar = mkv ()
				      val mvarenv' = bindMvar(#id argInfo, argMvar, mvarenv)
				      val (mvarenv'', k1) = genNode (arg, mvarenv', k)
				      val k2 =
					  fn (inner: exp) =>
					     Switch (thisMvar, [(key, SOME argMvar, k1 inner)],
						       NONE)
				  in (mvarenv'', varenvMC'', k2)
				  end)

*)

(* FLINT/trans/generate.sml *)
(* revised "old" match compiler *)

(* generation of "code" (in the form of PLambda.lexp) from decision trees (type dectree) *)

structure Generate = 
struct

local
  structure T = Types
  structure BT = BasicTypes
  structure LV = LambdaVar
  structure A = Access
  structure PO = Primop
  structure LT = PLambdaType  (* LTextern *)
  structure PL = PLambda
  structure P = Paths
  structure MC = MCCommon
  structure MU = MCUtil
  structure PP = PrettyPrint

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
	       MCPrint.ppDectree ppstrm dectree;
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

(* How are singleton constructor patterns, and in particular the
 * "special" ones like *ref* and *susp*?  In the general case we generate a single
 * datacon "deconstructor" expressed as a single-variant SWITCH in these cases.
 * It is possible that the single variant dies and this reduces to a SWITCH
 * with empty cases and a default (see MC Notes, sec 41.2).
 *
 * The special cases (ref,susp) are handled in genSwitch (along with switch on
 * intinf integers). Otherwise, MC.SWITCH translates almost directly to Plambda.SWITCH.
 *)

(* -------------------------------------------------------------------------------- *)
(* generate: main match "code generation" function
 * top level code generating function for matches (formerly MCCode.genCode) *)

(* "hybrid" rule -- where the rule rhs has been translated to an lexp before match compilation *)
type hrule = Absyn.pat * PL.lexp

(* generate : MC.andor * MC.decTree * ruleMap * ruleset * failInfo * toTcLt * giisTy
	      -> PL.lexp * mvar *)
fun generate (andor, decTree, ruleMap, allRules,
	      (failExnLexpOp, rhsTy, location), (toTyc, toLty), genIntInfSwitch) =
    let val _ = dbsay ">> generate"

    fun relativeLexp (mvar, suffix: P.link list) =
	let fun wrapSuffix (nil, lexp) = lexp
	      | wrapSuffix (P.PI n::rest, lexp) = wrapSuffix (rest, PL.SELECT(n, lexp))
	      | wrapSuffix (P.VI (n, elemTy)::rest, lexp) =
		let val elemTyc = toTyc elemTy
		    val lt_sub =
			let val x = LT.ltc_vector (LT.ltc_tv 0)
			in LT.ltc_poly([LT.tkc_mono],
				       [LT.ltc_parrow(LT.ltc_tuple [x, LT.ltc_int],
						      LT.ltc_tv 0)])
			end
		    val index_n = PL.INT {ival = IntInf.fromInt n, ty = Target.defaultIntSz}
		    val element_n =
			PL.APP(PL.PRIM(PO.SUBSCRIPTV, lt_sub, [elemTyc]),
			       (* apply vec. subscript primop *)
			       PL.RECORD [lexp, index_n]) (* (vector,index) *)
		 in wrapSuffix(rest, element_n)
		end
	      | wrapSuffix (link::rest, lexp) =
		(saynl ("relativeLexp:wrapSuffix suffix: " ^ P.pathToString suffix);
		 bug "wrapSuffix")
	    val suffix' = 
		case suffix
		 of P.DC(P.VLENcon _):: _ => tl suffix
		  | _ => suffix
	in wrapSuffix (suffix', PL.VAR mvar)
	end

    fun pathToLexp pathenv path =
	case MU.lookPath (pathenv, path)
	  of NONE => bug "pathToLexp"
	   | SOME(mvar, suffix) => relativeLexp (mvar, suffix)

     (* genRHS : ruleno * pathenv -> PL.lexp
      *  invoking the rule RHS function (fvar ~ abstracted rhs) on record of mvars corresponding to
      *    lhs pattern variables (which could be the empty record).
      *  The case where there are no pvars in the pattern is handled by default case in body. *)
     fun genRHS (ruleno: MC.ruleno, pathenv: MU.pathenv) =
	   let val (varPaths, fvar, _) = ruleMap ruleno
	       val argLexp =
	           case varPaths
		     of [path] => pathToLexp pathenv path
		      | _ => PL.RECORD (map (pathToLexp pathenv) varPaths)
			 (* for multiple pvars, _or none_ *)
	   in PL.APP(PL.VAR fvar, argLexp)
	   end

    (* conToCon : MC.con * mvar option -> PL.con
     *  translates MCCommon.con to PLambda.con and introduces a variable naming
     *  the destruct of a datacon-headed value (even if the datacon is a constant!)
     *  -- mvarOp will be SOME mvar if the con is a non-constant datacon or VLENcon,
     *  NONE for constant datacon. The mvar, if provided, is generated in genDecTree. *)
    fun conToCon (con, mvarOp) =  (* uses toLty and toTyc *)
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
	      of P.DATAcon (datacon, ts) =>
		  let val nts = map (toTyc o T.VARty) ts
		      val mvar = getOpt (mvarOp, MU.mkMvar())
			  (* get argument mvar from mvarOp = SOME mvar when the datacon is not
			     a constant, otherwise when datacon is a constant, mvarOp = NONE, and
			     we generate a new, but redundant, mvar that is required to construct
			     a PL.DATAcon. Probably don't need to pass argument mvar via DATAcon. *)
		   in PL.DATAcon (transDcon datacon, nts, mvar)
		  end
	       | P.VLENcon(i, t) => intCon i  (* element type t is no longer needed *)
	       | P.INTcon i => PL.INTcon i
	       | P.WORDcon w => PL.WORDcon w
	       | P.STRINGcon s => PL.STRINGcon s
	end

    (* genSwitch : mvar * A.sign * (P.con * PL.lexp) list * lexp option -> lexp
     * -- uses toTyc, which was passed to genMatch
     * -- the default will be SOME dt if the cons in the variants are not exhaustive (saturated)
     * -- Detects the vector length switch case and extracts the vector element
     *    type from the VLENcon constructor. Generates code to calculate the length of
     *    the vector and binds it to an fresh mvar (lenMvar) used as the switch subject.
     * -- Handles ref and susp constructors as special cases.
     * -- Handles intinf INTcon discrimination as special cases.
     * ASSERT: not (null cases) *)
    fun genSwitch (subject: PL.lexp, sign, cases: (P.con * MU.mvar option * PL.lexp) list,
		   defaultOp: PL.lexp option) =
	let fun transCase (con, mvarOp, lexp) = (conToCon (con, mvarOp), lexp)
	        (* only used in the general case where con is not a VLENcon or an intinf con,
		 * if con is a non-constant datacon, then mvarOp = SOME mvar, and the mvar
		 * is incorporated into the PL.datacon *)
	 in case cases
	      of nil => bug "Switch: empty cases"
	       | (con, mvarOp, lexp) :: rest => 
	         (case (con, mvarOp)
		    of (P.VLENcon (n,ty), SOME mvar) => (* switch on vector length(s) *)
			 (* "let val len = Vector.length mvar in <<switch over int values of len>>"
			  * where "len" is a fresh internal variable -- this is generated in 
			  * the VSWITCHexp case of Translate.mkExp0 to avoid the problem of
			  * accessing the vector length primop in absyn.
			  * -- defaultOp should be SOME _, not checked *)
		         let val elemTyc = toTyc ty  (* translated vector element type *)
			     val lt_len = LT.ltc_poly([LT.tkc_mono],
					      [LT.ltc_parrow(LT.ltc_tv 0, LT.ltc_int)])
			     val argtyc = LT.tcc_vector elemTyc
			     val lenMvar = MU.mkMvar ()
			     fun transVecCase (P.VLENcon(n, _), _, lexp) = (intCon n, lexp)
			       | transVecCase _ = bug "Switch:vector case: con not VLENcon"
			 in PL.LET (lenMvar,  (* bind lenMvar to the computed vector length *)
				    PL.APP (PL.PRIM(PO.LENGTH, lt_len, [argtyc]), (* apply vec length primop *)
					    subject), (* designates the vector *)
				    PL.SWITCH (PL.VAR lenMvar, A.CNIL, map transVecCase cases, defaultOp))
			 end
		     | (P.INTcon{ty=0, ...}, NONE) => (* switch on IntInf constant(s) *)
		         let fun strip (P.INTcon{ty=0, ival}, NONE, lexp) = (ival, lexp)
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
			             PL.LET(lvar,
					    PL.APP (PL.PRIM (Primop.DEREF, LT.lt_swap lty, tycs), subject),
					    lexp)
				   | _ => bug "switch: ref not singleton case")
			      | PL.DATAcon((_, A.SUSP(SOME(_, A.LVAR f)), lty), tycs, lvar) =>
				  (* SUSP constructor; "force" the suspension function *)
				(case rest
				   of nil => (* check there is only one case *)
				      let val v = MU.mkMvar ()
				      in PL.LET(lvar,
						PL.LET(v, PL.TAPP(PL.VAR f, tycs), PL.APP(PL.VAR v, subject)),
						lexp)
				      end
				    | _ => bug "switch: susp not singleton case")
			      | _ => PL.SWITCH (subject, sign, map transCase cases, defaultOp)
		       end)
	end (* fun genSwitch *)

    val rootMvar = MU.mkMvar ()
    val pathenv0 = MU.bindPath (MU.emptyPathenv, P.rootpath, rootMvar)
    val rootMvarOp = SOME rootMvar

    fun isVector ((P.VLENcon _, _) :: _) = true
      | isVector _ = false

    (* genDecTree: decTree * pathenv * path list -> PL.lexp *)
    fun genDecTree (decTree, pathenv) =
	(case decTree
	   of MC.SWITCH {andor, sign, cases = cases_SWITCH, default, live} =>
		(case andor    (* ASSERT: node must be OR *)
		  of OR {path, cases=cases_OR, ...} =>
		     (* cases and variants should be "congruent", since switch cases are
		      * created by mapping over OR node variants, but we insure coordination
		      * by comparing con's in matchCases. *)
		     (case MU.lookPath (pathenv, path)
			of NONE => bug "genDecTree:SWITCH: no mvar bound at this OR path (or prefix)"
		         | SOME (baseMvar, suffix) =>  
			   let val subject = relativeLexp (baseMvar, suffix) (* lexp for switch scrutinee value *)
			       val vecMvarInfo =
				   (case cases_SWITCH
				      of (P.VLENcon _, _) :: _ =>  (* vector-length switch *)
					 (case subject
					    of PL.VAR mvar => SOME (mvar, false)  (* null suffix, mvar bound to vector *)
					     | _ => SOME (MU.mkMvar (), true))  (* ow, create a new mvar to denote vector *)
				       | _ => NONE)  (* not a vector-length switch *)
					   (* if switch subject is a vector, check if subject lexp is an mvar
					    * (e.g. rootvar). If not, we need to bind subject to a fresh mvar
					    * and use that mvar as the the decon-bound variable (in this case
					    * bound to the vector) in each case. This new mvar will be bound to
					    * path in a modified pathenv. *)
			       fun genCase (con, decTree, subcase) =  (* caserules already taken into account building dectree *)
				    (* say (concat ["genDecTree.genCase:con = ", conToString con, "\n"]); *)
				    (case subcase   (* variant node assoc. with con *)
				       of CONST =>  (* constant, no destruct, no new values to be bound *)
					    (con, NONE, genDecTree(decTree, pathenv))
					| DCARG _ =>
					    let val deconMvar = MU.mkMvar() (* new decon variable denoting decon result *)
						val newPathenv = MU.bindPath (pathenv, P.addLink(P.DC con, path), deconMvar)
						val caseLexp = genDecTree(decTree, newPathenv)
					     in (con, SOME deconMvar, caseLexp)
					    end
					| VELEMS _ => (* => con = VLENcon(len,ty) *)
					    (case vecMvarInfo
					       of SOME(vecMvar,new) =>
						    if new (* vecMvar is a new mvar to be bound to vector *)
						    then let val newPathenv = MU.bindPath (pathenv, path, vecMvar)
							 in  (con, SOME vecMvar, genDecTree(decTree, newPathenv))
							 end
						    else (con, SOME vecMvar, genDecTree(decTree, pathenv))
						| NONE => bug "genDecTree:genCase:VELEMS")
				    (* end case *))  (* end genCase *)
			       fun matchCases (dtcases as ((con, dectree)::dtrest), (con', _, subcase)::aorest) =
				   if P.eqCon (con, con')
				   then genCase(con, dectree, subcase) :: matchCases(dtrest, aorest)
				   else matchCases (dtcases, aorest)
				 | matchCases (nil, _) = nil
				 | matchCases (_, nil) = bug "genDecTree..matchCases: andor case not found"
			        val switchCases = matchCases (cases_SWITCH, cases_OR)
				val defaultOp = Option.map (fn dt => genDecTree (dt, pathenv)) default
			     in case vecMvarInfo
				 of NONE => genSwitch (subject, sign, switchCases, defaultOp)  (* not a vector length switch *)
				  | SOME(vecMvar,new) => 
				     if new  (* PL.VAR mvar --> subject *)
				     then PL.LET (vecMvar, subject,
						  genSwitch (PL.VAR vecMvar, sign, switchCases, defaultOp))
				     else genSwitch (subject, sign, switchCases, defaultOp)
			    end)
		   | _ => bug "genDecTree: SWITCH andor not an OR node")
            | MC.FAIL =>
	        let val lty = toLty rhsTy
		    val failExnLexp = 
			(case failExnLexpOp
			  of NONE =>
			     (case rootMvarOp
			       of SOME mvar => PL.VAR mvar
				| NONE => bug "genDecTree: FAIL: no root mvar")
			   | SOME lexp => lexp)
		    val lexp' =
			if !Control.trackExn
			then PL.APP (markexn, PL.RECORD[failExnLexp, PL.STRING location])
			else failExnLexp
		in PL.RAISE (lexp', lty)
		end
	    | MC.RHS ruleno => genRHS (ruleno, pathenv)
	        (* dispatch to (i.e. call) appropriate rhs function for this ruleno, with mvar arguments
                 * corresponding to the pvars in the lhs pattern *)
	(* end case *))
        (* end genDecTree *)

    val lexp = genDecTree (decTree, pathenv0)
(*    val lexp = wrapBindings (dtLexp, andor, mvarenv0) *)

 in (lexp, rootMvarOp)
end (* fun generate *)

end (* top local *)
end (* structure Generate *)

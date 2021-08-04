(* Elaborator/matchcomp/matchtrans.sml *)

(* translation functions: translate AS.exp and AS.dec while compiling matches
 *   transExp : region -> AS.exp -> AS.exp
 *   transDec : region -> AS.dec -> AS.dec
 *)

structure MatchTrans =
struct

local
  structure T = Types
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure S = Symbol
  structure SP = SymPath
  structure A = Access
  structure V = VarCon
  structure AS = Absyn
  structure AU = AbsynUtil
  structure EU = ElabUtil
  structure PP = PrettyPrint
  structure MC = MatchComp
  open Absyn

  fun bug msg = ErrorMsg.impossible ("MatchTrans: " ^ msg)

  val printAndor = MCControl.printAndor
  val printDecisionTree = MCControl.printDecisionTree
  val printMatchAbsyn = MCControl.printMatchAbsyn
  val debugging = MCControl.mcdebugging
  val stats = MCControl.mcstats

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun says strings = saynl (concat strings)

  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)

  fun ppExp (exp, msg) =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm msg;
	       PPAbsyn.ppExp (StaticEnv.empty, NONE) ppstrm (exp, 20);
	       PP.newline ppstrm))

  fun ppDec (dec, msg) =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm msg;
	       PPAbsyn.ppDec (StaticEnv.empty, NONE) ppstrm (dec, 20);
	       PP.newline ppstrm))

  fun ppPat pat =
      PP.with_default_pp(fn ppstrm => PPAbsyn.ppPat StaticEnv.empty ppstrm (pat, 20))

  val matchComp = MatchComp.matchComp
		      
in

fun transMatchDec (dec, errorFn, region) =
let

(* simpleVALdec : V.var * AS.exp * T.tyvar list -> AS.dec *)
fun simpleVALdec (var, exp, boundtvs) =
    VALdec [VB{pat = VARpat var, exp = exp,
	       typ = V.varType var, boundtvs = boundtvs, tyvars = ref nil}]

(* transRules : region -> AS.rule list -> AS.rule list *)
fun transRules region rules =  (* apply fillPat to rule patterns, translate rhss *)
    map (fn RULE(pat,exp) => RULE(EU.fillPat pat, transExp region exp)) rules

(* transExp : region -> AS.exp -> AS.exp *)
and transExp region exp =
    let val _ = dbsay ">> transExp"
        fun trans region exp =
            (case exp
	       of RECORDexp fields =>
		    RECORDexp (map (fn (numberedLabel, exp) => (numberedLabel, trans region exp))
				   fields)
		| VECTORexp (exps,ty) => VECTORexp(map (trans region) exps, TU.prune ty)
		| APPexp (rator, rand) => APPexp (trans region rator, trans region rand)
		| FNexp (rules, argTy, resTy) =>
		    let val RULE(pat1,exp1)::_ = rules (* debugging *)
			val _ = (if !debugging
				 then (say "transExp:FNexp:pat1 = "; ppPat pat1; newline())
				 else ())
			val (bodyExp, matchVar) =
			     MC.matchCompile (transRules region rules, argTy, resTy, errorFn region)
		     in FNexp ([RULE(VARpat matchVar, bodyExp)], TU.prune argTy, TU.prune resTy)
		    end
		| HANDLEexp (baseExp, (rules, argTy, resTy)) =>
		    let val (handlerBody, matchVar) =
			    MC.handlerCompile (transRules region rules, argTy, resTy, errorFn region)
			val matchRule = RULE(VARpat matchVar, handlerBody)
		     in HANDLEexp(trans region baseExp, ([matchRule], TU.prune argTy, TU.prune resTy))
		    end
		| CASEexp (scrutinee, (rules, scrutTy, resTy)) =>
		    let val RULE(pat1,exp1)::_ = rules (* debugging *)
			val _ = (if !debugging
				 then (say "transExp:CASEexp:pat1 = "; ppPat pat1; newline())
				 else ())
			val (caseBody, matchVar) =
			    MC.matchCompile (transRules region rules, scrutTy, resTy, errorFn region)
		    in LETexp(VALdec[VB{pat = VARpat matchVar,
					exp = trans region scrutinee,
					typ = scrutTy,
					boundtvs = nil,
					tyvars = ref nil}],
			      caseBody)
		    end
		| RAISEexp (exp, ty) => RAISEexp (trans region exp, ty) (* original source raise *)
		| IFexp {test, thenCase, elseCase} =>
		  IFexp {test = trans region test,
			 thenCase = trans region thenCase,
			 elseCase = trans region elseCase}
		| ANDALSOexp (exp1, exp2) => ANDALSOexp (trans region exp1, trans region exp2)
		| ORELSEexp (exp1, exp2) => ORELSEexp (trans region exp1, trans region exp2)
		| WHILEexp { test, expr } => WHILEexp{ test = trans region test,
						       expr = trans region expr}
		| LETexp (dec, exp) => LETexp(transDec region dec, trans region exp)
		| SEQexp exps => SEQexp (map (trans region) exps)
		| CONSTRAINTexp (exp, ty) => CONSTRAINTexp (trans region exp, ty)
		| MARKexp (exp, region) => MARKexp (trans region exp, region)
		| _ => exp)
		  (* (VARexp _ | CONexp _ | NUMexp _ | REALexp _ | STRINGexp _  | CHARexp _ |
		   *  RSELECTexp _ | VSELECTexp _ | SWITCHexp _ | VSWITCHexp) => exp *)
     in trans region exp
    end (* transExp *)

(* transDec : AS.dec -> dec *)
and transDec (region: SourceMap.region) (dec: AS.dec): AS.dec =
    let fun transDec0 (region: SourceMap.region) (dec: AS.dec) : AS.dec =
            (case dec
	      of VALdec vbs => transVBs region vbs
	       | VALRECdec rvbs => VALRECdec (map (transRVB region) rvbs)
	       | DOdec exp => DOdec (transExp region exp)
	       | LOCALdec (innerDec, outerDec) =>
		   LOCALdec (transDec0 region innerDec, transDec0 region outerDec)
	       | SEQdec decs => SEQdec (map (transDec0 region) decs)
	       | ABSTYPEdec {abstycs, withtycs, body} =>
		   ABSTYPEdec {abstycs = abstycs, withtycs = withtycs, body = transDec0 region body}
	       | MARKdec (dec, region) => MARKdec (transDec0 region dec, region)
	       | _ => dec) (* transDec called locally for LETSTR, LETFCT, STRdec, FCTdec *)

	(* transvb : AS.vb -> AS.dec *)
	(* translation of vb to dec
	 * -- We can get away without a d (DB depth) parameter, leaving it to Translate.
	 * -- Looks like we can get away with never dealing with an internal mvar in the match.
	 * -- we need the type of the pat. Stored as typ field of VB.
	 * -- do we need an absyn equivalent to mkPE, say transPolyExp? We don't have an equivalent
	 *      to TFN in absyn -- yet!
         * -- following revmc translate.sml for treatment of VB: alpha-convert pattern, tuple even
         *    single variable case -- try special-case-ing single variable patterns later *)
	and transVB region (VB{pat, exp, typ, boundtvs, tyvars}) =
	    (* match compile [(pat,exp)] if pat is nontrivial (not a var);
	     * -- check what the match compiler does with (single) irrefutable patterns
	     *    DONE -- it does the right thing. *)
	    (if !debugging
	     then (say "transVB:pat = "; ppPat pat; ppExp (exp, "transVB:exp = "))
	     else ();
	     case EU.fillPat(AU.stripPatMarks pat)   (* does fillPat strip MARKpats? *)
	       of (WILDpat | CONSTRAINTpat(WILDpat, _)) =>  (* WILDpat pattern *)
		  let val exp' = transExp region exp
		   (* val _ = (print "transVB:exp' = "; ppExp exp') *)
		   in VALdec([VB{pat = WILDpat, exp = exp', typ = typ,
				 boundtvs = boundtvs, tyvars = tyvars}])
		  end
		| (VARpat var | CONSTRAINTpat(VARpat var, _)) =>
		  (* simple single variable pattern *)
		  let val exp' = transExp region exp
		  (* val _ = (print "transVB:exp' = "; ppExp exp') *)
		  in VALdec([VB{pat = VARpat var, exp = exp',
				typ = typ, boundtvs = boundtvs, tyvars = tyvars}])
		  end
		| pat =>  (* compound, possibly refutable, pattern. Does this work for constants? *)
		  let val (newpat,oldpvars,newpvars) = EU.aconvertPat pat
		      (* this is the only call of aconvertPat; it replaces pattern variables with
		       * new versions with fresh lvar access values. This is necessary to ensure that
		       * original pvars are only bound once in the generated absyn (and hence in the
		       * translated PLambda.lexp *)
		      val newPvarExps = map (fn v => VARexp(ref v,[])) newpvars
		      val pvarsTy = BT.tupleTy (map V.varType oldpvars)  (* same as newpvars *)
		      val bindRule = RULE(newpat, EU.TUPLEexp(newPvarExps))
		      val newexp = CASEexp(exp, ([bindRule], T.UNDEFty, pvarsTy))

		  in case newpvars
		       of nil =>   (* "constant" pattern, no pattern variables *)
			  let val (matchExp, matchVar) =
				  MC.bindCompile ([bindRule], typ, BT.unitTy,  (* typ = pattern type *)
						  errorFn region)
			      val resultDec =
				  LOCALdec(simpleVALdec(matchVar, transExp region exp, nil),
					   VALdec([VB{pat=WILDpat, exp = matchExp,
						      typ=typ, boundtvs=nil, tyvars = ref nil}]))
			   in if !debugging then ppDec (resultDec, "transVB (no var): \n") else ();
			      resultDec
			  end
(* can restore special treatment of single variable patterns later -- should be straightforward after
 *   also special-casing the single variable case of bindRule rhs
			| [pvar] =>     (* single pattern variable *)
			  let val pvarTy = V.varType pvar
			      val (matchExp, matchVar) =
				  MC.bindCompile ([RULE (pat, VARexp(ref pvar, nil))],
						  typ, pvarTy, errorFn region)
			      val resultDec =
				  LOCALdec(simpleVALdec(matchVar, transExp region exp, nil),
					   simpleVALdec(pvar, matchExp, boundtvs))
			   in if !debugging then ppDec (resultDec, "transVB (single var): \n") else ();
			      resultDec
			  end
*)
			| _ =>  (* "multiple" pattern variables (1 or more) *)
			  let val (matchExp, matchVar) =
				  MC.bindCompile ([bindRule], typ, pvarsTy, errorFn region)
				  (* matchVar will be bound to MC-translation of exp *)
			      val ptupleVar = V.VALvar{path = SP.SPATH [S.varSymbol "<ptupleVar>"],
						       typ = ref(pvarsTy),
						       btvs = ref(boundtvs),  (* possibly polymorphic *)
						       access = A.LVAR(LambdaVar.mkLvar()),
						       prim = PrimopId.NonPrim}
			      fun selectVBs([], _) = []
				| selectVBs (pvar::pvars, n) = (* non-polymorphic *)
				    simpleVALdec(pvar, RSELECTexp (ptupleVar,n), nil)
				      :: selectVBs(pvars, n+1)
				    (* defining a pattern var by (record) selection from a
				     * var (ptupleVar) bound to the tuple of all the pattern
				     * var values; the btvs of each pattern var is a subset
				     * of the btvs of ptupleVar. *)
			      val resultDec =
				  LOCALdec(SEQdec [simpleVALdec(matchVar, transExp region exp, nil),
						   simpleVALdec(ptupleVar, matchExp, boundtvs)],
					   SEQdec (selectVBs(oldpvars, 0)))
					  (* rebinding orig pattern variables *)
			   in if !debugging
			      then ppDec (resultDec, "transVB (multiple vars): \n")
			      else ();
			      resultDec
			  end
		  end (* pat case *))

	    (* NOTE: Given the way Translate.transDec deals with LOCALdec (i.e. not
             * hiding the local decls), we use a single SEQ encompassing all the
             * declarations. *)

	(* transVBs : region -> AS.vb list -> AS.dec *)
	and transVBs region vbs = 
            (case vbs
	      of nil => bug "transVBs: nil"  (* expect at least one *)
	       | [vb] => transVB region vb
	       | _ => SEQdec (map (transVB region) vbs))

	and transRVB region
		     (RVB{var: V.var, exp: exp, resultty: T.ty option, tyvars: T.tyvar list ref}) =
	    (dbsays [">> transRVB:var = ", Symbol.name (V.varName var)];
	     RVB {var = var, exp = transExp region exp, resultty = resultty, tyvars = tyvars})

     in transDec0 region dec
    end (* transDec *)

 in transDec region dec
end (* transMatchDec *)

end (* local *)
end (* structure MatchTrans *)

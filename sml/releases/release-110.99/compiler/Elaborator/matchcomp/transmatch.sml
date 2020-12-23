(* Elaborator/matchcomp/transmatch.sml *)

(* translate typed absyn, expanding matches/bindings as necessary with MatchComp.matchComp *)

(* Note: where transMatch is called:
 * match translation in LETstr(dec,strexp) and LETfct(dec,strexp) is handled in
 * ElabMod.elabDecl0, and this also holds for struct decls end structure expressions.
 * transDec is always called right after Typecheck.decType. *)

structure TransMatch =
struct

local
    structure S = Symbol
    structure SP = SymPath
    structure A = Access
    structure V = VarCon
    structure AS = Absyn
    structure AU = AbsynUtil
    structure T = Types
    structure TU = TypesUtil
    structure EU = ElabUtil
    structure MC = MatchComp
    structure PP = PrettyPrint
    open Absyn

    fun bug msg = ErrorMsg.impossible ("TransMatch: " ^ msg)

    fun ppPat pat =
        PP.with_default_pp(fn ppstrm => PPAbsyn.ppPat StaticEnv.empty ppstrm (pat, 20))
    fun ppExp exp =
        PP.with_default_pp(fn ppstrm => PPAbsyn.ppExp (StaticEnv.empty,NONE) ppstrm (exp, 20))

in

(* simpleVALdec : V.var * AS.exp * T.tyvar list -> AS.dec *)
fun simpleVALdec(var, exp, boundtvs) = 
    VALdec [VB{pat = VARpat var, exp = exp,
	       typ = V.varType var, boundtvs = boundtvs, tyvars = ref nil}]

(* transExp : AS.exp -> AS.exp *)
fun transExp exp =
    let val transRules =  (* just do fillPat on pattern *)
	    map (fn RULE(pat,exp) => RULE(EU.fillPat pat, (* transExp *) exp))
     in case exp
	 of RECORDexp fields =>
	      RECORDexp (map (fn (numberedLabel, exp) => (numberedLabel, transExp exp))
			     fields)
	  | VECTORexp (exps,ty) => VECTORexp(map transExp exps, TU.prune ty)
	  | APPexp (rator, rand) => APPexp (transExp rator, transExp rand)
	  | FNexp (rules, argTy, resTy) =>
	      let val RULE(pat1,exp1)::_ = rules (* debugging *)
		  val _ = (print "transExp:FNexp:pat1 = "; ppPat pat1 (* debugging *))
		  val (bodyExp, matchVar) =
		      MC.matchComp (transRules rules, argTy, resTy, EU.getMatchExn())
	       in FNexp ([RULE(VARpat matchVar, bodyExp)], TU.prune argTy, TU.prune resTy)
	      end
	  | HANDLEexp (baseExp, (rules, argTy, resTy)) =>
	      let val (handlerBody, matchVar) =
		      MC.matchComp (transRules rules, argTy, resTy, EU.getMatchExn())
		  val matchRule = RULE(VARpat matchVar, handlerBody)
	       in HANDLEexp(transExp baseExp, ([matchRule], TU.prune argTy, TU.prune resTy))
	      end
	  | RAISEexp (exp, ty) => RAISEexp (transExp exp, ty) (* original source raise *)
	  | CASEexp (scrutinee, (rules, scrutTy, resTy)) =>
	      let (* val _ = print ">>> transExp: CASEexp\n" *)
		  val (caseBody, matchVar) =
		      MC.matchComp(transRules rules, scrutTy, resTy, EU.getMatchExn())
	      in LETexp(VALdec[VB{pat = VARpat matchVar,
				  exp = transExp scrutinee,
				  typ = scrutTy,
				  boundtvs = nil,
				  tyvars = ref nil}],
			caseBody)
	      end
	  | IFexp {test, thenCase, elseCase} =>
	    IFexp {test = transExp test, thenCase = transExp thenCase,
		   elseCase = transExp elseCase}
	  | ANDALSOexp (exp1, exp2) => ANDALSOexp (transExp exp1, transExp exp2)
	  | ORELSEexp (exp1, exp2) => ORELSEexp (transExp exp1, transExp exp2)
	  | WHILEexp { test, expr } => WHILEexp{ test = transExp test, expr = transExp expr}
	  | LETexp (dec, exp) => LETexp(transDec dec, transExp exp)
	  | SEQexp exps => SEQexp (map transExp exps)
	  | CONSTRAINTexp (exp, ty) => CONSTRAINTexp (transExp exp, ty)
	  | MARKexp (exp, region) => MARKexp (transExp exp, region)
	  | _ => exp
	    (* (VARexp _ | CONexp _ | NUMexp _ | REALexp _ | STRINGexp _  | CHARexp _ |
	     *  SELECTexp _ | SWITCHexp _ | VSWITCHexp) => exp *)
    end (* transExp *)

(* transDec : dec -> dec *)
and transDec (dec: AS.dec) : AS.dec =
    case dec
      of VALdec vbs => transVBs vbs
       | VALRECdec rvbs => VALRECdec (map transRVB rvbs)
       | DOdec exp => DOdec (transExp exp)
       | LOCALdec (innerDec, outerDec) => LOCALdec (transDec innerDec, transDec outerDec)
       | SEQdec decs => SEQdec (map transDec decs)
       | ABSTYPEdec {abstycs, withtycs, body} => 
	   ABSTYPEdec {abstycs = abstycs, withtycs = withtycs, body = transDec body}
       | MARKdec (dec, region) => MARKdec (transDec dec, region)
       | _ => dec (* transDec called locally for LETSTR, LETFCT, STRdec, FCTdec *)

(* transvb : A.vb -> A.dec *)
(* translation of vb to dec
 * -- can we get away without a d (DB depth) parameter? Leaving it to Translate? Think so.
 * -- looks like we can get away with never dealing with an internal svar in the match.
 * -- we need to access or (re)construct the type of the pat.
 *      Could store this as a field of VB.
 * -- do we need an absyn equivalent to mkPE, say transPolyExp? We don't have an equivalent
 *      to TFN in absyn -- yet! *)
and transVB (VB{pat, exp, typ, boundtvs, tyvars}) =
    (* match compile [(pat,exp)] if pat is nontrivial (not a var);
     * -- check what the match compiler does with (single) irrefutable patterns
     *    DONE -- it does the right thing. *)
    ((* print "transVB:pat = "; ppPat pat;
     print "transVB:exp = "; ppExp exp; *)
     case AU.stripPatMarks pat
       of (VARpat var | CONSTRAINTpat(VARpat var, _)) =>  (* simple variable pattern *)
	  let val exp' = transExp exp
(*	      val _ = (print "transVB:exp' = "; ppExp exp') *)
	  in VALdec([VB{pat = VARpat var, exp = exp',
			typ = typ, boundtvs = boundtvs, tyvars = tyvars}])
	  end
	| pat =>  (* compound, possibly refutable, pattern *)
	  let val patvars = AU.patternVars pat
	      val patvarstuple = EU.TUPLEexp (map (fn var => VARexp(ref var, nil)) patvars)
	      val patvarstuplety = Tuples.mkTUPLEtype (map V.varType patvars)
	      val (matchExp, matchVar) =  (* matchVar will be bound to trans of exp *)
		  MC.matchComp([RULE(pat,patvarstuple)], typ, patvarstuplety, EU.getBindExn())
	      val ptupleVar = V.VALvar{path = SP.SPATH [S.varSymbol "topVar"],
				       typ = ref(patvarstuplety),
				       btvs = ref(boundtvs),  (* possibly polymorphic *)
				       access = A.LVAR(LambdaVar.mkLvar()),
				       prim = PrimopId.NonPrim}
	      fun selectVBs([], _) = []
		| selectVBs (pvar::pvars, n) =
		    simpleVALdec(pvar, SELECTexp(ptupleVar,n,true), nil) (* non-polymorphic *)
		      :: selectVBs(pvars, n+1) 
		    (* defining a pattern var by (record) selection from a var (ptupleVar)
		     * bound to the tuple of all the pattern var values; the btvs of each
		     * pattern var is a subset of the btvs of ptupleVar. *)
	  in LOCALdec(SEQdec [simpleVALdec(matchVar, transExp exp, nil),
		              simpleVALdec(ptupleVar, matchExp, boundtvs)],
                      SEQdec (selectVBs(patvars, 0))) (* rebinding orig pattern variables *)
	  end)

(* NOTE: the way transDec deals with LOCALdec (i.e. not hiding the local decls), we may
 * as well just have a single SEQ encompassing all the declarations. *)

(* transVBs : AS.vb list -> AS.dec *)
and transVBs nil = bug "transVBs: nil"  (* expect at least one *)
  | transVBs [vb] = transVB vb
  | transVBs vbs = SEQdec (map transVB vbs)

and transRVB (RVB{var: V.var, exp: exp, boundtvs: T.tyvar list,
		  resultty: T.ty option, tyvars: T.tyvar list ref}) =
    (print "transRVB:var = "; print (Symbol.name (V.varName var)); print "\n";
    RVB {var = var, exp = transExp exp, boundtvs = boundtvs,
	 resultty = resultty, tyvars = tyvars})

end (* local *)
end (* structure TransMatch *)

(* Copyright 1996 by AT&T Bell Laboratories *)
(* elabcore.sml *)

signature ELABCORE =
sig

  val elabABSTYPEdec :
        {abstycs: Ast.db list,withtycs: Ast.tb list,body: Ast.dec} 
        * StaticEnv.staticEnv * ElabUtil.context * InvPath.path 
        * SourceMap.region * ElabUtil.compInfo
        -> Absyn.dec * StaticEnv.staticEnv (* * Modules.entityEnv ??? *)

  val elabDec : 
        Ast.dec * StaticEnv.staticEnv * InvPath.path 
        * SourceMap.region * ElabUtil.compInfo
        -> Absyn.dec * StaticEnv.staticEnv

  val debugging : bool ref

end (* signature ELABCORE *)


structure ElabCore: ELABCORE =
struct

local structure EM = ErrorMsg
      structure SP = SymPath
      structure IP = InvPath
      structure SE = StaticEnv
      structure LU = Lookup
      structure V = VarCon
      structure B  = Bindings
      structure M  = Modules
      structure MU = ModuleUtil
      structure T  = Types
      structure TU = TypesUtil
      structure EE = EntityEnv
      structure EU = ElabUtil
      structure TS = TyvarSet
      structure ET = ElabType
      structure S = Symbol
      structure II = InlInfo
      structure A = Access

      open Absyn Ast BasicTypes Access ElabUtil Types VarCon
   (*
      open BasicTypes Symbol Absyn Ast PrintUtil AstUtil BasicTypes TyvarSet
           Types EqTypes TypesUtil Access ElabUtil
    *)
in

val say = Control.Print.say
val debugging = ref false
fun debugmsg (msg: string) = if !debugging then (say msg; say "\n") else ()
fun bug msg = ErrorMsg.impossible("ElabCore: "^msg)

infix -->

type tyvUpdate = TS.tyvarset -> unit
val --- = TS.diffPure
val union = TS.union
val diff = TS.diff
fun no_updt (_ : TS.tyvarset) = ()
fun noTyvars (dec,env) = (dec,env,TS.empty,no_updt)
infix ++ -- ---

fun stripExpAbs (MARKexp(e,_)) = stripExpAbs e
  | stripExpAbs (CONSTRAINTexp(e,_)) = stripExpAbs e
  | stripExpAbs e = e

fun stripExpAst(MarkExp(e,r'),r) = stripExpAst(e,r')
  | stripExpAst(ConstraintExp{expr=e,...},r) = stripExpAst(e,r)
  | stripExpAst(SeqExp[e],r) = stripExpAst(e,r)
  | stripExpAst(FlatAppExp[{item,region,...}],r) = stripExpAst(item,region)
  | stripExpAst x = x

val internalSym = S.varSymbol "<InternalVar>"

val dummyFNexp =
    FNexp([RULE(WILDpat,RAISEexp(CONexp(V.bogusEXN,[]),UNDEFty))],UNDEFty)

(**** ABSTRACT TYPE DECLARATIONS ****)
fun elabABSTYPEdec({abstycs,withtycs,body},env,context,rpath,region,compInfo) =
    let val (datatycs,withtycs,_,env1) =
	      ET.elabDATATYPEdec({datatycs=abstycs,withtycs=withtycs},env,
				 [],EE.empty,rpath,region,compInfo)

	val (body,env2) = 
              elabDec(body,Env.atop(env1,env),rpath,region,compInfo)

	(* datatycs will be changed to abstycs during type checking
	   by changing the eqprop field *)
        fun bind (x, e) = Env.bind(TU.tycName x, B.TYCbind x, e)
        val envt = foldl bind (foldl bind Env.empty datatycs) withtycs 

     in (ABSTYPEdec{abstycs=datatycs,withtycs=withtycs,body=body},
	 Env.atop(env2,envt))
    end

(**** ELABORATE GENERAL (core) DECLARATIONS ****)
and elabDec (dec, env, rpath, region,
             compInfo as {mkLvar=mkv,error,errorMatch,coreEnv,...}) =

let
    val _ = debugmsg ">>ElabCore.elabDec"

    val completeMatch = EU.completeMatch(coreEnv,"Match")
    val _ = debugmsg "--ElabCore.elabDec << completeBind Match"
    val completeBind = EU.completeMatch(coreEnv,"Bind")
    val _ = debugmsg "--ElabCore.elabDec << completeBind Bind"

    fun newVALvar s = V.mkVALvar(s, A.namedAcc(s, mkv))

    (**** EXCEPTION DECLARATIONS ****)

    fun elabEb (region:region) (env:SE.staticEnv) (eb:Ast.eb) =
	case eb
	  of EbGen{exn=id,etype=NONE} =>
	       let val exn =
		     DATACON{name=id, const=true, typ=exnTy,
			     rep=EXNCONST(LVAR(mkv(SOME id))), sign=CNIL}
		in ([EBgen{exn=exn, etype=NONE, 
                           ident=STRINGexp(S.name id)}], 
		    Env.bind(id, B.CONbind exn, Env.empty),TS.empty)
	       end
	   | EbGen{exn=id,etype=SOME typ} =>
	       let val (ty,vt) = ET.elabType(typ,env,error,region)
		   val exn = 
                     DATACON{name=id, const=false, typ=(ty --> exnTy),
			     rep=EXNFUN(LVAR(mkv(SOME id))), sign=CNIL}
		in ([EBgen{exn=exn,etype=SOME ty,
			   ident=STRINGexp(S.name id)}],
		    Env.bind(id,B.CONbind exn, Env.empty),vt) 
	       end
	   | EbDef{exn=id,edef=qid} =>
	       let val edef as DATACON{const,typ,sign,...} =
		     LU.lookExn(env,SP.SPATH qid,error region)
                   val nrep = if const then EXNCONST(LVAR(mkv(SOME id)))
 			               else EXNFUN(LVAR(mkv(SOME id)))
	           val exn = DATACON{name=id, const=const, typ=typ, 
                                     sign=sign, rep=nrep}
		in ([EBdef{exn=exn,edef=edef}],
		    Env.bind(id,B.CONbind exn,Env.empty),TS.empty)
	       end
	   | MarkEb(eb,region) => elabEb region env eb

    fun elabEXCEPTIONdec(excbinds:Ast.eb list, env: SE.staticEnv, region) =
	let val (ebs,env,vt) = 
	      foldl
		(fn (exc1,(ebs1,env1,vt1)) =>
		   let val (eb2,env2,vt2) = elabEb region env exc1
		    in (eb2@ebs1, Env.atop(env2,env1),
                        union(vt1,vt2,error region))
		   end)
		 ([],Env.empty,TS.empty) excbinds
	    fun getname(EBgen{exn=DATACON{name,...},...}) = name
	      | getname(EBdef{exn=DATACON{name,...},...}) = name
	 in EU.checkUniq (error region, "duplicate exception declaration",
		       map getname ebs);
	    (EXCEPTIONdec(rev ebs),env,vt,no_updt)
	end


    (**** PATTERNS ****)

    fun apply_pat (MarkPat(c,(l1,r1)),MarkPat(p,(l2,r2))) = 
	  MarkPat(AppPat{constr=c, argument=p},(Int.min(l1,l2),Int.max(r1,r2)))
      | apply_pat (c ,p) = AppPat{constr=c, argument=p}

    fun tuple_pat (MarkPat(a,(l,_)),MarkPat(b,(_,r))) =
	  MarkPat(TuplePat[a,b],(l,r))
      | tuple_pat (a,b) = TuplePat[a,b]

    val patParse = Precedence.parse{apply=apply_pat, pair=tuple_pat}

    exception FreeOrVars
    fun elabPat(pat:Ast.pat, env:SE.staticEnv, region:region) 
		 : Absyn.pat * TS.tyvarset =
      case pat
      of WildPat => (WILDpat, TS.empty)
       | VarPat path => 
	   (clean_pat (error region) 
              (pat_id(SP.SPATH path, env, error region, compInfo)),
	    TS.empty)
       | IntPat s => (INTpat(s,TU.mkLITERALty(T.INT,region)),TS.empty)
       | WordPat s => (WORDpat(s,TU.mkLITERALty(T.WORD,region)),TS.empty)
       | StringPat s => (STRINGpat s,TS.empty)
       | CharPat s => (CHARpat s,TS.empty)
       | RecordPat {def,flexibility} =>
	    let val (lps,tyv) = elabPLabel region env def
	    in (makeRECORDpat (lps,flexibility,error region),tyv) end
       | ListPat nil =>
	      (NILpat, TS.empty)
       | ListPat (a::rest) =>
	    let val (p, tyv) = elabPat(TuplePat[a,ListPat rest], env, region)
	     in (CONSpat p, tyv)
	    end
       | TuplePat pats =>
	    let val (ps,tyv) = elabPatList(pats, env, region)
	     in (TUPLEpat ps,tyv)
	    end
       | VectorPat pats =>
	    let val (ps,tyv) = elabPatList(pats, env, region)
	    in (VECTORpat(ps,UNDEFty),tyv) end
       | OrPat pats =>
         (* Check that the sub-patterns of an or-pattern have exactly the same
          * free variables, and rewrite the sub-pattersn so that all instances
          * of a given free variable have the same type ref and the same 
          * access.
          *)
	   let val (ps, tyv) = elabPatList(pats, env, region)
	       fun freeOrVars (pat::pats) =
		   let val tbl : (access * ty ref * int) IntStrMap.intstrmap =
			   IntStrMap.new(16, FreeOrVars)
		       fun symbToIntStr f symb =
			   (f tbl (S.number symb, S.name symb))
		       val ins =
			   let val ins' = IntStrMap.add tbl
			    in fn (symb, x) =>
				 ins' (S.number symb, S.name symb, x)
			   end
		       val look =
			   let val look' = IntStrMap.map tbl
			    in fn symb => 
			         look'(S.number symb, S.name symb)
			   end
		       fun errorMsg x = 
			     error region EM.COMPLAIN
			       ("variable " ^ x ^
			        " does not occur in all branches of or-pattern")
			       EM.nullErrorBody
		       fun insFn (id, access, ty) =
			   (ins(id, (access, ty, 1)); (access,ty))
		       fun bumpFn (id, access0, ty0) =
			   (let val (access, ty, n) = look id
			     in ins (id, (access, ty, n+1)); (access,ty)
			    end
			    handle FreeOrVars => 
				    (errorMsg(S.name id); (access0,ty0)))
		       fun checkFn (id, access0, ty0) = 
                           (let val (access, ty, _) = look id 
                             in (access, ty) 
                            end
			    handle FreeOrVars => 
				   (errorMsg(S.name id); (access0, ty0)))
		       fun doPat(insFn: (S.symbol*access*ty ref)
                                          ->access*ty ref) =
			   let fun doPat' (VARpat(VALvar{access, info, path, 
                                                         typ})) =
				     let val (access,typ) = 
					 insFn(SymPath.first path,access,typ)
				      in VARpat(VALvar{access=access, 
                                                       path=path,info=info,
						       typ=typ})
				     end
				 | doPat' (RECORDpat{fields, flex, typ}) =
				     RECORDpat
				       {fields = 
                                            map (fn (l, p) => (l, doPat' p))
						     fields,
					flex = flex, typ = typ}
				 | doPat' (APPpat(dc, ty, pat)) =
				     APPpat(dc, ty, doPat' pat)
				 | doPat' (CONSTRAINTpat(pat, ty)) =
				     CONSTRAINTpat(doPat' pat, ty)
				 | doPat' (LAYEREDpat(p1, p2)) =
				     LAYEREDpat(doPat' p1, doPat' p2)
				 | doPat' (ORpat(p1, p2)) =
				     ORpat(doPat' p1, doPat checkFn p2)
				 | doPat' (VECTORpat(pats, ty)) =
				     VECTORpat(map doPat' pats, ty)
				 | doPat' pat = pat
			      in doPat'
			     end
		     (* check that each variable occurs in each sub-pattern *)
		       fun checkComplete m (_, id, (_, _, n)) =
			   if (n = m) then () else (errorMsg id)
		       val pats = (doPat insFn pat) :: 
                                      (map (doPat bumpFn) pats)
		    in IntStrMap.app (checkComplete (length pats)) tbl;
		       pats
		   end (* freeOrVars *)
	       val (pat::pats) = freeOrVars ps
	       fun foldOr (p, []) = p
		 | foldOr (p, p'::r) = ORpat(p, foldOr(p', r))
	    in (foldOr(pat, pats), tyv)
	   end
       | AppPat {constr, argument} =>
	   let fun getVar (MarkPat(p,region),region') = getVar(p,region)
		 | getVar (VarPat path, region') = 
		      let val dcb = pat_id (SP.SPATH path, env, 
                                            error region', compInfo)
			  val (p,tv) = elabPat(argument, env, region)
		      in (makeAPPpat (error region) (dcb,p),tv) end
		 | getVar (_, region') = 
		   (error region' EM.COMPLAIN 
			 "non-constructor applied to argument in pattern"
			 EM.nullErrorBody;
		    (WILDpat,TS.empty))
	    in getVar(constr,region)
	   end
       | ConstraintPat {pattern=pat,constraint=ty} =>
	   let val (p1,tv1) = elabPat(pat, env, region)
	       val (t2,tv2) = ET.elabType(ty,env,error,region)
	    in (CONSTRAINTpat(p1,t2), union(tv1,tv2,error region))
	   end
       | LayeredPat {varPat,expPat} =>
	   let val (p1,tv1) = elabPat(varPat, env, region)
	       val (p2,tv2) = elabPat(expPat, env, region)
	    in (makeLAYEREDpat(p1,p2,error region),union(tv1,tv2,error region))
	   end
       | MarkPat (pat,region) =>
	   let val (p,tv) = elabPat(pat, env, region)
	    in (p,tv)
	   end
       | FlatAppPat pats => elabPat(patParse(pats,env,error), env, region) 

    and elabPLabel (region:region) (env:SE.staticEnv) labs =
	foldl
	  (fn ((lb1,p1),(lps1,lvt1)) => 
	      let val (p2,lvt2) = elabPat(p1, env, region)
	      in ((lb1,p2) :: lps1, union(lvt2,lvt1,error region)) end)
	  ([],TS.empty) labs

    and elabPatList(ps, env:SE.staticEnv, region:region) =
	foldr
	  (fn (p1,(lps1,lvt1)) => 
	      let val (p2,lvt2) = elabPat(p1, env, region)
	      in (p2 :: lps1, union(lvt2,lvt1,error region)) end)
	  ([],TS.empty) ps


    (**** EXPRESSIONS ****)

    val expParse = Precedence.parse
		     {apply=fn(f,a) => AppExp{function=f,argument=a},
		      pair=fn (a,b) => TupleExp[a,b]}

(*
    fun elabExp x = 
      Stats.doPhase (Stats.makePhase "Compiler 032 6-elabExp") elabExp0 x
*)

    fun elabExp(exp: Ast.exp, env: SE.staticEnv, region: region) 
		: (Absyn.exp * TS.tyvarset * tyvUpdate) =
	(case exp
	  of VarExp path =>
	       ((case LU.lookVal(env,SP.SPATH path,error region)
		  of V.VAL v => VARexp(ref v,[])  
		   | V.CON d => CONexp(d, [])), 
		TS.empty, no_updt)
	   | IntExp s => 
               (INTexp(s,TU.mkLITERALty(T.INT,region)),TS.empty,no_updt)
           | WordExp s => 
               (WORDexp(s,TU.mkLITERALty(T.WORD,region)),TS.empty,no_updt)
	   | RealExp r => (REALexp r,TS.empty,no_updt)
	   | StringExp s => (STRINGexp s,TS.empty,no_updt)
	   | CharExp s => (CHARexp s,TS.empty,no_updt)
	   | RecordExp cells => 
	       let val (les,tyv,updt) = elabELabel(cells,env,region)
		in (makeRECORDexp (les,error region),tyv,updt)
	       end
	   | SeqExp exps =>
	       (case exps
		  of [e] => elabExp(e,env,region)
		   | [] => bug "elabExp(SeqExp[])"
		   | _ =>
		     let val (es,tyv,updt) = elabExpList(exps,env,region)
		      in (SEQexp es,tyv,updt)
		     end)
	   | ListExp nil => (NILexp, TS.empty, no_updt)
	   | ListExp (a::rest) =>
	       let val (e,tyv,updt) = 
                     elabExp(TupleExp[a,ListExp rest],env,region)
		in (APPexp(CONSexp,e), tyv, updt)
	       end
	   | TupleExp exps =>
	       let val (es,tyv,updt) = elabExpList(exps,env,region)
		in (TUPLEexp es,tyv,updt)
	       end
	   | VectorExp exps =>
	       let val (es,tyv,updt) = elabExpList(exps,env,region)
		in (VECTORexp(es,UNDEFty),tyv,updt)
	       end
	   | AppExp {function,argument} =>
	       let val (e1,tv1,updt1) = elabExp(function,env,region)
		   and (e2,tv2,updt2) = elabExp(argument,env,region)
		   fun updt tv = (updt1 tv;updt2 tv)
		in (APPexp (e1,e2),union(tv1,tv2,error region),updt)
	       end
	   | ConstraintExp {expr=exp,constraint=ty} =>
	       let val (e1,tv1,updt) = elabExp(exp,env,region)
		   val (t2,tv2) = ET.elabType(ty,env,error,region)
		in (CONSTRAINTexp(e1,t2), union(tv1,tv2,error region),updt)
	       end
	   | HandleExp {expr,rules} =>
	       let val (e1,tv1,updt1) = elabExp(expr,env,region)
		   val (rls2,tv2,updt2) = elabMatch(rules,env,region)
		   fun updt tv = (updt1 tv;updt2 tv)
		in (makeHANDLEexp (e1, rls2, compInfo), 
                    union(tv1,tv2,error region), updt)
	       end
	   | RaiseExp exp =>
	       let val (e,tyv,updt) = elabExp(exp,env,region)
		in (RAISEexp(e,UNDEFty),tyv,updt)
	       end
	   | LetExp {dec,expr} => 
	       let val (d1,e1,tv1,updt1) =
			  elabDec'(dec,env,IP.IPATH[],region)
		   val (e2,tv2,updt2) = elabExp(expr,Env.atop(e1,env),region)
		   fun updt tv = (updt1 tv;updt2 tv)
		in (LETexp(d1,e2), union(tv1,tv2,error region),updt)
	       end
	   | CaseExp {expr,rules} =>
	       let val (e1,tv1,updt1) = elabExp(expr,env,region)
		   val (rls2,tv2,updt2) = elabMatch(rules,env,region)
		   fun updt tv = (updt1 tv;updt2 tv)
		in (CASEexp (e1,completeMatch rls2, true),
		    union(tv1,tv2,error region),updt)
	       end
	   | IfExp {test,thenCase,elseCase} =>
	       let val (e1,tv1,updt1) = elabExp(test,env,region)
		   and (e2,tv2,updt2) = elabExp(thenCase,env,region)
		   and (e3,tv3,updt3) = elabExp(elseCase,env,region)
		   fun updt tv = (updt1 tv;updt2 tv;updt3 tv)
		in (IFexp(e1,e2,e3),
		    union(tv1,union(tv2,tv3,error region),error region),
		    updt)
	       end
	   | AndalsoExp (exp1,exp2) =>
	       let val (e1,tv1,updt1) = elabExp(exp1,env,region)
		   and (e2,tv2,updt2) = elabExp(exp2,env,region)
		   fun updt tv = (updt1 tv;updt2 tv)
		in (IFexp(e1, e2, FALSEexp), union(tv1,tv2,error region),updt)
	       end
	   | OrelseExp (exp1,exp2) =>
	       let val (e1,tv1,updt1) = elabExp(exp1,env,region)
		   and (e2,tv2,updt2) = elabExp(exp2,env,region)
		   fun updt tv = (updt1 tv;updt2 tv)
		in (IFexp(e1 ,TRUEexp, e2), union(tv1,tv2,error region),updt)
	       end
	   | WhileExp {test,expr} =>
	       let val (e1,tv1,updt1) = elabExp(test,env,region)
		   and (e2,tv2,updt2) = elabExp(expr,env,region)
		   fun updt tv = (updt1 tv;updt2 tv)
		in (WHILEexp(e1,e2,compInfo), 
                    union(tv1,tv2,error region), updt)
	       end
	   | FnExp rules => 
	       let val (rls,tyv,updt) = elabMatch(rules,env,region)
		in (FNexp (completeMatch rls,UNDEFty),tyv,updt)
	       end
	   | MarkExp (exp,region) => 
	       let val (e,tyv,updt) = elabExp(exp,env,region)
		in (if !Control.markabsyn then MARKexp(e,region) else e,
                    tyv, updt)
	       end
	   | SelectorExp s => 
	       (let val v = newVALvar s
		 in FNexp(completeMatch
			  [RULE(RECORDpat{fields=[(s,VARpat v)], flex=true,
					  typ= ref UNDEFty},
				MARKexp(VARexp(ref v,[]),region))],UNDEFty)
		end,
		TS.empty, no_updt)
	   | FlatAppExp items => elabExp(expParse(items,env,error),env,region))


    and elabELabel(labs,env,region) =
	let val (les1,lvt1,updt1) =
	      foldr 
		(fn ((lb2,e2),(les2,lvt2,updts2)) => 
		    let val (e3,lvt3,updt3) = elabExp(e2,env,region)
		     in ((lb2,e3) :: les2, union(lvt3,lvt2,error region),
			 updt3 :: updts2)
		    end)
		([],TS.empty,[]) labs
	    fun updt tv : unit = app (fn f => f tv) updt1
	 in (les1, lvt1, updt)
	end

    and elabExpList(es,env,region) =
	let val (les1,lvt1,updt1) =
	      foldr 
		(fn (e2,(es2,lvt2,updts2)) => 
		    let val (e3,lvt3,updt3) = elabExp(e2,env,region)
		     in (e3 :: es2, union(lvt3,lvt2,error region), 
                         updt3 :: updts2)
		    end)
		([],TS.empty,[]) es
	    fun updt tv: unit = app (fn f => f tv) updt1
	 in (les1, lvt1, updt)
	end

    and elabMatch(rs,env,region) =
	let val (rs,lvt,updt1) =
	      foldr 
		(fn (r1,(rs1,lvt1,updt1)) => 
		    let val (r2,lvt2,updt2) = elabRule(r1,env,region)
		     in (r2 :: rs1, union(lvt2,lvt1,error region), 
                         updt2::updt1) 
                    end)
		([],TS.empty,[]) rs
	    fun updt tv: unit = app (fn f => f tv) updt1
	 in (rs, lvt, updt)
	end

    and elabRule(Rule{pat,exp},env,region)  =
	let val region' = case pat of MarkPat (p,reg) => reg | _ => region
	    val (p,tv1) = elabPat(pat, env, region)
	    val env' = Env.atop(bindVARp ([p],error region'), env)
	    val (e,tv2,updt) = elabExp(exp,env',region)
	 in (RULE(p,e),union(tv1,tv2,error region),updt)
	end


    (**** SIMPLE DECLARATIONS ****)

    and elabDec'(dec,env,rpath,region)
		: (Absyn.dec * SE.staticEnv * TS.tyvarset * tyvUpdate) =
	(case dec 
	  of TypeDec tbs => 
	      let val (dec', env') =
		  ET.elabTYPEdec(tbs,env,(* EU.TOP,??? *) rpath,region,compInfo)
	       in noTyvars(dec', env')
	      end
	   | DatatypeDec(x as {datatycs,withtycs}) => 
	     (case datatycs
		of (Db{rhs=(Constrs _), ...}) :: _ =>
		     let val (dtycs, wtycs, _, env') =
			 ET.elabDATATYPEdec(x,env,[],EE.empty,rpath,region,compInfo)
		      in noTyvars(DATATYPEdec{datatycs=dtycs,withtycs=wtycs}, env')
		     end
	         | (Db{tyc=name,rhs=Repl syms,tyvars=nil}::nil) =>
		     (case withtycs
			of nil =>
			    let val tyc = LU.lookTyc(env, SP.SPATH syms,
						    error region)
				val dcons = TU.extractDcons tyc
				val envDcons =
				     foldl (fn (d as T.DATACON{name,...},e)=>
					       SE.bind(name,B.CONbind d, e))
					   SE.empty 
					   dcons
				val env = SE.bind(name,B.TYCbind tyc,envDcons)
			     in noTyvars(DATATYPEdec{datatycs=[tyc], withtycs=[]},
					 env)
			    end
			 | _ => (error region EM.COMPLAIN
				  "withtype not allowed in datatype replication"
				  EM.nullErrorBody;
				 noTyvars(SEQdec[],SE.empty)))
		  | _ => (error region EM.COMPLAIN
			   "argument type variables in datatype replication"
			   EM.nullErrorBody;
			  noTyvars(SEQdec[],SE.empty)))
	   | AbstypeDec x => 
	      let val (dec', env') =
		  elabABSTYPEdec(x,env,EU.TOP,rpath,region,compInfo)
	       in noTyvars(dec', env')
	      end
	   | ExceptionDec ebs => elabEXCEPTIONdec(ebs,env,region)
	   | ValDec(vbs,explicitTvs) =>
	       elabVALdec(vbs,explicitTvs,env,rpath,region)
	   | FunDec(fbs,explicitTvs) =>
	       elabFUNdec(fbs,explicitTvs,env,rpath,region)
	   | ValrecDec(rvbs,explicitTvs) =>
	       elabVALRECdec(rvbs,explicitTvs,env,rpath,region)
	   | SeqDec ds => elabSEQdec(ds,env,rpath,region)
	   | LocalDec ld => elabLOCALdec(ld,env,rpath,region)
	   | OpenDec ds => elabOPENdec(ds,env,region)
	   | FixDec (ds as {fixity,ops}) => 
	       let val env = 
		 foldr (fn (id,env) => Env.bind(id,B.FIXbind fixity,env))
			Env.empty ops
		in (FIXdec ds,env,TS.empty,no_updt)
	       end
	   | OvldDec dec  => elabOVERLOADdec(dec,env,rpath,region)
	   | MarkDec(dec,region') =>
	       let val (d,env,tv,updt)= elabDec'(dec,env,rpath,region')
		in (if !Control.markabsyn then MARKdec(d,region') else d,
		    env,tv,updt)
	       end
	   | StrDec _ => bug "strdec"
	   | AbsDec _ => bug "absdec"
	   | FctDec _ => bug "fctdec"
	   | SigDec _ => bug "sigdec"
	   | FsigDec _ => bug "fsigdec"
           | _ => bug "unexpected ast declarations")
              

    (**** OVERLOADING ****)

    and elabOVERLOADdec((id,typ,exps),env,rpath,region) =
	let val (body,tyvars) = ET.elabType(typ,env,error,region)
	    val tvs = TS.elements tyvars
	    val scheme = (TU.bindTyvars tvs; TU.compressTy body;
			  TYFUN{arity=length tvs, body=body})
	    fun option (MARKexp(e,_)) = option e
	      | option (VARexp(ref (v as VALvar{typ,...}),_)) =
		  {indicator = TU.matchScheme(scheme,!typ), variant = v}
	      | option _ = bug "makeOVERLOADdec.option"
	    val exps = map (fn exp => elabExp(exp,env,region)) exps
	    val exps1 = map #1 exps
	    and exps3 = map #3 exps
	    fun updt tv: unit = app (fn f => f tv) exps3
	    val ovldvar = OVLDvar{name=id,scheme=scheme,
				  options=ref(map option exps1)}
	 in (OVLDdec ovldvar, Env.bind(id,B.VALbind ovldvar,Env.empty),
             TS.empty, updt)
	end

    (**** LOCAL ****)

    and elabLOCALdec((ldecs1,ldecs2),env,rpath:IP.path,region) =
	let val (ld1,env1,tv1,updt1) = elabDec'(ldecs1,env,IP.IPATH[],region)
	    val (ld2,env2,tv2,updt2) =
		  elabDec'(ldecs2,Env.atop(env1,env),rpath,region)
	    fun updt tv = (updt1 tv;updt2 tv)
	 in (LOCALdec(ld1,ld2), env2,union(tv1,tv2,error region),updt)
	end

    (**** OPEN ****)

    and elabOPENdec(spaths, env, region) = 
        let val err = error region
            val strs = map (fn s => let val sp = SP.SPATH s
                                     in (sp, LU.lookStr(env, sp, err))
                                    end) spaths

            fun loop([], env) = (OPENdec strs, env, TS.empty, no_updt)
              | loop((_, s)::r, env) = loop(r, MU.openStructure(env, s))

         in loop(strs, SE.empty)
        end 

    (****  VALUE DECLARATIONS ****)
    and elabVB (MarkVb(vb,region),etvs,env,_) = elabVB(vb,etvs,env,region)
      | elabVB (Vb{pat,exp},etvs,env,region) =
	  let val (pat,pv) = elabPat(pat, env, region)
	      val (exp,ev,updtExp) = elabExp(exp,env,region)
	      val tvref = ref []
	      fun updt tv: unit =
		let fun a++b = union(a,b,error region)
                    fun a--b = diff(a,b,error region)
		    val localtyvars = (ev++pv++etvs) -- (tv---etvs)
			 (* etvs should be the second argument to union
			  * to avoid having the explicit type variables
			  * instantiated by the union operation. *)
		    val downtyvars = localtyvars ++ (tv---etvs)
		 in tvref := TS.elements localtyvars; updtExp downtyvars
	        end

              (* 
               * WARNING: the following code is trying to propagate the 
               * PRIMOP access through simple value binding. It is a old
               * hack and should be cleaned up in the future. (ZHONG)
               *)
	      val pat = 
		case stripExpAbs exp
		 of VARexp(ref(VALvar{info=dinfo,...}),_) =>
                      (if II.isPrimInfo(dinfo) then
		        (case pat
			  of CONSTRAINTpat(VARpat(VALvar{path,typ,
                                                         access,...}), ty) =>
			      CONSTRAINTpat(VARpat(
                                   VALvar{path=path, typ=typ, access=access,
                                          info=dinfo}), ty)
			   | VARpat(VALvar{path, typ, access, ...}) =>
			      VARpat(VALvar{path=path, typ=typ, access=access,
                                            info=dinfo})
			   | _ => pat)
                       else pat)
		  | _ => pat

              fun bindpat(VARpat(VALvar{access=acc, ...})) = A.accLvar acc
                | bindpat(CONSTRAINTpat(VARpat(VALvar{access=acc, ...}),_)) = 
                      A.accLvar acc
                | bindpat _ = NONE

	   in case bindpat(pat) 
               of NONE =>
		(let val (newpat,oldvars,newvars) = patproc(pat, compInfo)
                    
                     val b = map (fn v => VARexp(ref v,[])) newvars 
		     val r = RULE(newpat, TUPLEexp(b))
                     val newexp = CASEexp(exp, completeBind[r], false)

                  in case oldvars
                      of [] => 
                           (let val nvb = VB{exp=newexp, tyvars=tvref,
                                             pat=WILDpat, boundtvs=[]}
                             in (VALdec [nvb], [], updt)
                            end)
                       | _ => 
                           (let val nv = newVALvar internalSym
                                val nvpat = VARpat(nv)
                                val nvexp = VARexp(ref nv, [])

                                val nvdec = 
                                  VALdec([VB{exp=newexp, tyvars=tvref, 
                                             pat=nvpat, boundtvs=[]}])

                                fun h([], _, d) =  
                                      LOCALdec(nvdec, SEQdec(rev d))
                                  | h(vp::r, i, d) = 
                                      let val nvb = VB{exp=TPSELexp(nvexp,i),
                                                       pat=vp, boundtvs=[],
                                                       tyvars=ref[]}

                                       in h(r, i+1, VALdec([nvb])::d)
                                      end

                             in (h(oldvars, 1, []), oldvars, updt)
                            end)
                 end)
               | SOME _ => 
                   (VALdec([VB{exp=exp, tyvars=tvref, pat=pat, boundtvs=[]}]),
                    [pat], updt) 
	  end

    and elabVALdec(vb,etvs,env,rpath,region) =
       let val etvs = ET.elabTyvList(etvs,error,region)
	   val (ds,pats,updt1) = 
	      foldr 
		(fn (vdec,(ds1,pats1,updt1)) => 
		   let val etvs = TS.mkTyvarset(map T.copyTyvar etvs)
		       val (d2,pats2,updt2) = elabVB(vdec,etvs,env,region)
		    in (d2::ds1,pats2@pats1,updt2::updt1) 
                   end)
		([],[],[]) vb
	    fun updt tv : unit = app (fn f => f tv) updt1
	in (SEQdec ds, bindVARp (pats,error region), TS.empty, updt)
       end

    and elabRVB(MarkRvb(rvb,region),env,_) = elabRVB(rvb,env,region)
      | elabRVB(Rvb{var,fixity,exp,resultty},env,region) =
         (case stripExpAst(exp,region)
	    of (FnExp _,region')=>
	        let val (e,ev,updt) = elabExp(exp,env,region')
		    val (t,tv) = 
			case resultty 
			  of SOME t1 => 
			       let val (t2,tv2) = ET.elabType(t1,env,error,region)
				in (SOME t2,tv2)
			       end
			   | NONE => (NONE,TS.empty)
		 in case fixity 
		     of NONE => ()
		      | SOME(f,region) => 
			 (case LU.lookFix(env,f) 
			   of Fixity.NONfix => ()
			    | _ =>
			      error region EM.COMPLAIN
			        ("infix symbol \""^ S.name f ^
				 "\" used where a nonfix identifier was expected")
				EM.nullErrorBody);
			  ({match = e , ty = t, name=var},
			   union(ev,tv,error region),updt)
		end
	     | _ =>
	       (error region EM.COMPLAIN
		 "fn expression required on rhs of val rec"
		 EM.nullErrorBody;
		({match = dummyFNexp, ty = NONE, name = var},TS.empty,no_updt)))

    and elabVALRECdec(rvb,etvs,env,rpath:IP.path,region) = 
	let val etvs = TS.mkTyvarset(ET.elabTyvList(etvs,error,region))
	    val env' = ref(SE.empty: SE.staticEnv)
	    fun makevar region (p as Rvb{var,...}) =
		  let val v = newVALvar var
                      val nv = newVALvar var
		   in (* checkBoundConstructor(env,var,error region); - fix 1357 *)
		      env' := Env.bind(var,B.VALbind v,!env');
		      (v, p)
		  end
	      | makevar _ (p as MarkRvb(rvb,region)) = 
		  let val (v,_) = makevar region rvb in (v,p) end

	    val rvbs' = map (makevar region) rvb
	    val env'' = Env.atop(!env', env)
	    val (rvbs,tyvars,updts)=
		foldl (fn((v,rvb1),(rvbs1,tvs1,updt1)) =>
			   let val (rvb2,tv2,updt2) =
				   elabRVB(rvb1,env'',region)
			    in ((v,rvb2)::rvbs1, 
				union(tv2,tvs1,error region),
				updt2::updt1)
			   end) 
			([],TS.empty,[]) rvbs' 
	    val tvref = ref []
	    fun updt tvs : unit =  
		let fun a++b = union(a,b,error region)
		    fun a--b = diff(a,b,error region)
		    val localtyvars = (tyvars ++ etvs) -- (tvs --- etvs)
		    val downtyvars = localtyvars ++ (tvs --- etvs)
		 in tvref := TS.elements localtyvars;
		    app (fn f => f downtyvars) updts
		end
	    val _ = EU.checkUniq(error region,
                        "duplicate function name in val rec dec",
  		        (map (fn (v,{name,...}) => name) rvbs))

            val (ndec, nenv) = 
  	      wrapRECdec((map (fn (v,{ty,match,name}) =>
		           RVB{var=v,resultty=ty,tyvars=tvref, exp=match,
			       boundtvs=[]}) rvbs), compInfo)
         in (ndec, nenv, TS.empty, updt)
        end

    and elabFUNdec(fb,etvs,env,rpath,region) =
	let val etvs = TS.mkTyvarset(ET.elabTyvList(etvs,error,region))
	    fun makevar _ (MarkFb(fb,region),ctx) = makevar region (fb,ctx)
	      | makevar region (Fb clauses,(lcl,env')) =
		 let fun getfix(SOME f) = LU.lookFix(env,f)
		       | getfix NONE = Fixity.NONfix
		     fun ensureInfix{item,fixity,region} =
			 (case getfix fixity
			   of Fixity.NONfix => error region EM.COMPLAIN
			     "infix operator required, or delete parentheses" 
			       EM.nullErrorBody
			    | _ => ();
			  item)
		     fun ensureNonfix{item,fixity,region} =
			 (case (getfix fixity, fixity)
			   of (Fixity.NONfix,_) => ()
			    | (_,SOME sym) => error region EM.COMPLAIN
				  ("infix operator \"" ^ S.name sym ^
				   "\" used without \"op\" in fun dec")
				  EM.nullErrorBody;
			  item)

		     fun getname(MarkPat(p,region),_) = getname(p,region)
		       | getname(VarPat[v], _) = v
		       | getname(_, region) = 
                           (error region EM.COMPLAIN
				"illegal function symbol in clause"
				 EM.nullErrorBody; 
			    bogusID)

   	             fun parse'({item=FlatAppPat[a,b as {region,...},c],...}
                                ::rest) = 
			    (getname(ensureInfix b, region),
			      tuple_pat(ensureNonfix a, ensureNonfix c)
			      :: map ensureNonfix rest)
		       | parse' [{item,region,...}] = 
				(error region EM.COMPLAIN
				 "can't find function arguments in clause"
				 EM.nullErrorBody;
				 (getname(item,region), [WildPat]))
		       | parse' ((a as {region,...}) :: rest) =
			     (getname(ensureNonfix a, region), 
			      map ensureNonfix rest)

		     fun parse({item=MarkPat(p,_),region,fixity}::rest) = 
			     parse({item=p,region=region,fixity=fixity}::rest)
		       | parse (pats as [a as {region=ra,...},
					 b as {item,fixity,region},c]) =
			   (case getfix fixity
			     of Fixity.NONfix => parse' pats
			      | _ => (getname(item,region),
				 [tuple_pat(ensureNonfix a, ensureNonfix c)]))
		       | parse pats = parse' pats

		     fun parseClause(Clause{pats,resultty,exp}) =
			 let val (funsym,argpats) = parse pats
			  in {funsym=funsym,argpats=argpats,resultty=resultty,
			      exp=exp}
			 end

		     val clauses as {funsym=var,...}::_ = 
                         map parseClause clauses

		     val _ = if List.exists (fn {funsym,...} => 
					not(S.eq(var,funsym))) clauses
			     then  error region EM.COMPLAIN 
				     "clauses don't all have function name"
				     EM.nullErrorBody
			     else ()

(* fix bug 1357
		     val _ = checkBoundConstructor(env,var,error region)
*)
		     val v = newVALvar var
		     val _ = 
		      case clauses
		       of ({argpats,...})::rest => 
			    let val len = length argpats
			     in if List.exists
			            (fn {argpats,...} => 
                                          len <> length argpats) rest
				then error region EM.COMPLAIN 
			       "clauses don't all have same number of patterns"
				      EM.nullErrorBody
				else ()
			    end
			| [] => bug "elabFUNdec 1"
		  in ((v,clauses,region)::lcl,Env.bind(var,B.VALbind v,env'))
		 end
	    val (clauses,env') = foldl (makevar region) ([],Env.empty) fb
	    val env'' = Env.atop(env',env)
	    fun makeclause region ({argpats=pats,resultty,exp,funsym}) =
		let val (pats,tv1) = elabPatList(pats, env, region)
                    val nenv = Env.atop(bindVARp(pats,error region), env'')
		    val (exp,tv2,updt) = elabExp(exp, nenv,region)
		    val (ty,tv3) =
		      case resultty
		       of NONE => (NONE,TS.empty)
			| SOME t => 
			    let val (t4,tv4) = ET.elabType(t,env,error,region)
			     in (SOME t4,tv4)
			    end
		 in ({pats=pats,resultty=ty,exp=exp},
		     union(tv1,union(tv2,tv3,error region),error region),updt)
		end
	    fun evalclauses ((var,clauses,region),(fs,tvs,updt)) = 
		let val (cs1,tvs1,updt1) =
		      foldl (fn (c2,(cs2,tvs2,updt2)) =>
			     let val (c3,tvs3,updt3) = makeclause region c2
			      in (c3::cs2,union(tvs3,tvs2,error region),
				  updt3::updt2)
			     end) 
			  ([],TS.empty,[]) clauses
		 in ((var,rev cs1)::fs, union(tvs1,tvs,error region),
                     updt1 @ updt)
		end
	    val (fbs1,ftyvars,updts) = foldl evalclauses ([],TS.empty,[]) clauses
	    val tvref = ref [] (* common tvref cell for all bindings! *)
	    fun updt tvs : unit =  
		let fun a++b = union(a,b,error region)
		    fun a--b = diff(a,b,error region)
		    val localtyvars = (ftyvars ++ etvs) -- (tvs --- etvs)
		    val downtyvars = localtyvars ++ (tvs --- etvs)
		 in tvref := TS.elements localtyvars;
		    app (fn f => f downtyvars) updts
		end
	    fun makefb (v as VALvar{path=SymPath.SPATH[_],...},cs) =
		  ({var=v,clauses=cs, tyvars=tvref})
	      | makefb _ = bug "makeFUNdec.makefb"
	 in EU.checkUniq(error region, "duplicate function names in fun dec",
		      (map (fn (VALvar{path=SymPath.SPATH[x],...},_) => x
			     | _ => bug "makeFUNdec:checkuniq")
			   fbs1));
	    (let val (ndec, nenv) = 
                   FUNdec(completeMatch,map makefb fbs1,region,compInfo)
              in (ndec, nenv, TS.empty, updt)
             end)
	end

    and elabSEQdec(ds,env,rpath:IP.path,region) =
	let val (ds1,env1,tv1,updt1) = 
	      foldl 
	       (fn (decl2,(ds2,env2,tvs2,updt2)) =>
		  let val (d3,env3,tvs3,updt3) =
			   elabDec'(decl2,Env.atop(env2,env),rpath,region)
		   in (d3::ds2, Env.atop(env3,env2), 
                       union(tvs3,tvs2,error region), updt3::updt2)
		  end)
	       ([],Env.empty,TS.empty,[]) ds 
	    fun updt tv : unit = app (fn f => f tv) updt1
	 in (SEQdec(rev ds1),env1,tv1,updt)
	end

    val _ = debugmsg ("EC.elabDec calling elabDec' - foo")
    val (dec',env',tyvars,tyvUpdate) = elabDec'(dec,env,rpath,region)

 in tyvUpdate tyvars;
    (dec',env')

end (* function elabDec *)

(*
val elabDec =
  Stats.doPhase (Stats.makePhase "Compiler 032 8-elabCoreDec") elabDec
*)

end (* top-level local *)
end (* structure ElabCore *)

(*
 * $Log: elabcore.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:45  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.12.2.1  1998/08/19 18:20:54  dbm
 * bug fixes for 110.9 [dbm]
 *
 * Revision 1.12  1997/11/07 05:40:07  dbm
 *   Fixed checking for "fn expression" on rhs of val rec declarations.
 *   Special case for SeqExp with singleton list: don't build SEQexp.
 *
 * Revision 1.11  1997/11/04  23:29:19  dbm
 *   Added syntax checking for val rec declarations.
 *
 * Revision 1.10  1997/09/10  22:10:59  dbm
 *   Fix regression failure on bug 134.
 *
 * Revision 1.9  1997/09/05  04:37:59  dbm
 *   Fixes for bugs 1244 and 1246.
 *
 * Revision 1.8  1997/05/05  19:55:09  george
 *    Turning off some measurement hooks - zsh
 *
 * Revision 1.7  1997/04/16  18:00:50  dbm
 *   Fix for bug 1192.
 *
 * Revision 1.6  1997/04/14  21:28:26  dbm
 *   Edited elabVALdec, etc. to implement explicit type variable bindings.
 *
 * Revision 1.5  1997/04/02  03:59:00  dbm
 * Minor typographical change.
 *
 * Revision 1.4  1997/03/22  18:10:42  dbm
 * New initial types for IntPat, IntExp, WordPat, WordExp for better
 * handling of literal overloading.
 *
 * Revision 1.3  1997/02/26  15:30:50  dbm
 * Fix for bug 1141.  Added empty entityEnv parameter to calls of elabDATATYPEdec.
 *
 * Revision 1.2  1997/01/21  13:23:31  george
 *   In pre-109.24, all abstype decs also exports the data constructors;
 *   this is fixed by modifying the elabABSTYPE function. -- from zsh
 *
 *)

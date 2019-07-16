(* fixityparse.sml *)

(* Thu Jul 10 12:15:43 EDT 1997 *)

(* NB:  When we add lazyness, for example, the user can affect the fixity
        of the identifiers that *they* introduce, but not the fixity of
        the derived identifiers.  Taking care of the latter would
        essentially require the work involved in doing the lazyness 
        translation here.

        This is no big dealy, as long as these identifiers are internal.
        Unfortunately, right now, they are also accessable to the user.

*)

(* SOMEWHAT OPEN QUESTION:

        What kinds of errors relating to fixity parsing can we detect at
        this point?  The only one that I pick-up know is the one which 
        results in an application of somethat that is not a constructor
        to something else (within a pattern.)

   IMMEDIATE PROBLEM:  How does the compiler currently deal with LocalDec?
                       (I saw the text, but I'm not quite sure what exactly
                        is going on there.)

*)

signature FIXITYPARSE = 
sig

  type T = {ast: Ast.dec,
            compenv: SCStaticEnv.staticEnv,
            compInfo: ElabUtil.compInfo}

  val fixityparse : T -> T

end

structure FixityParse : FIXITYPARSE =
struct

  type T = {ast: Ast.dec,
            compenv: SCStaticEnv.staticEnv,
            compInfo: ElabUtil.compInfo}

local

exception FixityParse

(* This Error function is just a hack that needs to be dealt with later. *)

fun Error (s:string) = (print "\n---";print s; print "\n"; raise FixityParse)

open Ast AstUtil

(* Stolen from elaborate.sml *)

    fun apply_pat (MarkPat(c,(l1,r1)),MarkPat(p,(l2,r2))) = 
	  MarkPat(AppPat{constr=c, argument=p},(Int.min(l1,l2),Int.max(r1,r2)))
      | apply_pat (c ,p) = AppPat{constr=c, argument=p}

    fun tuple_pat (MarkPat(a,(l,_)),MarkPat(b,(_,r))) =
	  MarkPat(TuplePat[a,b],(l,r))
      | tuple_pat (a,b) = TuplePat[a,b]

    val patParse = Precedence.parse{apply=apply_pat, pair=tuple_pat}

    val expParse = Precedence.parse
		     {apply=fn(f,a) => AppExp{function=f,argument=a},
		      pair=fn (a,b) => TupleExp[a,b]}

(* End Stolen from elaborate.sml *)


val first  = fn (x,y) => x
val second = fn (x,y) => y

fun Apply (f,x) = AppExp {argument=x,function=f}

fun ListApply (f,[]) = f
|   ListApply (f,(x::xs)) = ListApply(Apply(f,x),xs)

fun mapfixitem f {item,fixity,region} = 
              {item=f item,fixity=fixity,region=region}

fun mapsigConst f s CE =
(case s of
    NoSig => (CE, NoSig)
  | Transparent a => let val (CE',b) = f a CE 
                     in (CE, Transparent (b)) end
  | Opaque a => let val (CE',b) = f a CE 
                in (CE, Opaque (b)) end
)

val mapoption = Option.map

fun Smap f [] CE = (CE,[])
|   Smap f (x::xs) CE = let val (CE',y) = f x CE
                            val (CE'',ys) = Smap f xs CE'
                        in (CE'',y::ys) end;

(* The following functions are needed. *)

fun onFirst  f = fn (x,y) => fn CE =>
                  let val (CE',z) = f x CE
                  in (CE',(z,y)) end;

fun onSecond f = fn (x,y) => fn CE =>
                  let val (CE',z) = f y CE
                  in (CE',(x,z)) end;

fun onMiddle f = fn (x,y,z) => fn CE =>
                  let val (CE',a) = f y CE
                  in (CE',(x,a,z)) end;

fun onFixitem f {item,fixity,region} CE =
                  let val (CE',v) = f item CE
                  in (CE',{item=v,fixity=fixity,region=region}) end;

fun onOption f NONE CE = (CE,NONE)
|   onOption f (SOME a) CE = let val (CE',a')=f a CE in (CE',SOME a') end

(* WARNING:  While the names of all the functions to follow have Id in them,
   they are the ones that are actually doing the fixity parsing!  They are
   *almost* identity, but ofcourse, not quite. *)

(* CRITIQUE:  In retrospect, expId didn't really need to thread CE through
   it.  It's really only declarations that need to thread CE. 

   Actually, I have also just checked in the Definition.  The only place
   where it seems we need to thread is in "let".  But even then, no new 
   declarations escape into the surrounding context, and so, there is no
   case where we need to return a modified CE.  *)

fun expId e (CE as (env,error)) = 
(case e of
    VarExp (path)		(* variable *)     (* DO NOTHING *)
    => (CE,VarExp path)
  | FnExp rulelist		(* abstraction *)  (* BLOCK CE *)
    => 
       let val (_,rulelist') = Smap ruleId rulelist CE
       in (CE,FnExp (rulelist')) end

(*********************** REAL WORK ****************************************)

  | FlatAppExp expfixitemlist (* expressions before fixity parsing *)
    => expId(expParse(expfixitemlist,env,error)) CE

(* WHAT EXACTLY DOES "expParse" DO ? *)

  | AppExp {function,argument}  (* application *)
    => 
       let val (_,f') = expId function CE
           val (_,a') = expId argument CE
       in (CE,AppExp {function=f',argument=a'}) end  (* BLOCK BOTH CE's *)
  | CaseExp {expr,rules}  (* case expression *)
    => let val (_,e') = expId expr CE
           val (_,r') = Smap ruleId rules CE
        in (CE,CaseExp {expr=e',rules=r'}) end       (* BLOCK BOTH CE's *)
  | LetExp {dec,expr} (* let expression *)
    => let val (CE',d') = decId dec CE
           val (_,e') = expId expr CE'
       in (CE,LetExp {dec=d',expr=e'}) end           (* BLOCK LAST CE *)
  | SeqExp explist		(* sequence of expressions *)
    => let val (_,el') = Smap expId explist CE
       in (CE, SeqExp (el')) end                     (* BLOCK LAST CE *)
  | IntExp literal		(* integer *)
    => (CE,e)                                        (* DO NOTHING *)
  | WordExp literal		(* word literal *)
    => (CE,e)                                        (* DO NOTHING *)
  | RealExp string		(* floating point coded by its string *)
    => (CE,e)                                        (* DO NOTHING *)
  | StringExp string		(* string *)
    => (CE,e)                                        (* DO NOTHING *)
  | CharExp string			(* char *)
    => (CE,e)                                        (* DO NOTHING *)
  | RecordExp symbolexplist	(* record *)
    => let val (_, sel') = Smap (onSecond expId) symbolexplist CE
       in (CE, RecordExp (sel')) end                 (* BLOCK LAST CE *)
  | ListExp explist	        (*  [list,in,square,brackets] *)
    => let val (_,el') = Smap expId explist CE
       in (CE,ListExp (el')) end                     (* BLOCK LAST CE *)
  | TupleExp explist	(* tuple (derived form) *)
    => let val (_,el') = Smap expId explist CE
       in (CE,TupleExp (el')) end                    (* BLOCK LAST CE *)
  | SelectorExp symbol	(* selector of a record field *)
    => (CE,e)                                        (* DO NOTHING *)
  | ConstraintExp {expr,constraint}  (* type constraint *)
    => let val (_,e') = expId expr CE
       in (CE, ConstraintExp {expr=e',constraint=constraint}) end
                                                     (* BLOCK LAST CE *)
  | HandleExp {expr, rules}  (* exception handler *)
    => let val (_,e') = expId expr CE
           val (_,r') = Smap ruleId rules CE
       in (CE,HandleExp {expr=e', rules=r'}) end     (* BLOCK LAST CE *)
  | RaiseExp exp 		(* raise an exception *)
    => let val (_,e') = expId exp CE
       in (CE,RaiseExp (e')) end                     (* BLOCK LAST CE *)
  | IfExp {test, thenCase, elseCase}  (* if expression (derived form) *)
    => let val (_,c') = expId test CE
           val (_,t') = expId thenCase CE
           val (_,f') = expId elseCase CE
       in (CE, IfExp {test=c', thenCase=t', elseCase=f'}) end
                                                     (* BLOCK LAST CE *)
  | AndalsoExp (exp1,exp2)	(* andalso (derived form) *)
    => let val (_,e') = expId exp1 CE
           val (_,e'') = expId exp2 CE
       in (CE,AndalsoExp(e',e'')) end                (* BLOCK LAST CE *)
  | OrelseExp (exp1,exp2) (* orelse (derived form) *)
    => let val (_,e') = expId exp1 CE
           val (_,e'') = expId exp2 CE
       in (CE,OrelseExp(e',e'')) end                 (* BLOCK LAST CE *)

  | WhileExp {test,expr}  (* while (derived form) *)
    => let val (_,e') = expId test CE
           val (_,e'') = expId expr CE
       in (CE,WhileExp {test=e',expr=e''}) end       (* BLOCK LAST CE *)
  | MarkExp (exp,region) 	(* mark an expression *)
    => let val (_,e') = expId exp CE
       in (CE,MarkExp (e',region)) end               (* BLOCK LAST CE *)
  | VectorExp explist       (* vector *)
    => let val (_,el') = Smap expId explist CE
       in (CE, VectorExp (el')) end                  (* BLOCK LAST CE *)
)

(* CRITIQUE:  This shouldn't thread CE either. *)

(* RULE for case functions and exception handler *)
and ruleId r CE = 
(case r of 
   (Rule {pat,exp}) => let val (_,p') = patId pat CE
                           val (_,e') = expId exp CE
                       in (CE,Rule {pat=p',exp=e'}) end  (* NO THREADING *)
)

(* CRITIQUE:  This shouldn't thread CE either. *)

(* PATTERN *)
and patId p (CE as (env,error)) =
(case p of
          WildPat				(* empty pattern *)
          => (CE,p)                                       (* DO NOTHING *)
	| VarPat path	   			(* variable pattern *)
          => (CE,p)                                       (* DO NOTHING *) 
	| IntPat literal			(* integer *)
          => (CE,p)                                       (* DO NOTHING *) 
	| WordPat literal			(* word literal *)
          => (CE,p)                                       (* DO NOTHING *) 
	| StringPat string			(* string *)
          => (CE,p)                                       (* DO NOTHING *) 
	| CharPat string			(* char *)
          => (CE,p)                                       (* DO NOTHING *) 
	| RecordPat {def, flexibility}  (* record *)
          => let val (CE,d') = Smap (onSecond patId) def CE
             in (CE,RecordPat {def=d',flexibility=flexibility}) end
                                                          (* THE OBVIOUS *)
        | ListPat patlist		       (*  [list,in,square,brackets] *)
          => let val (CE,pl') = Smap patId patlist CE
             in (CE,ListPat (pl')) end
                                                          (* THE OBVIOUS *)
	| TuplePat patlist			(* tuple *)
          => let val (_,pl') = Smap patId patlist CE
             in (CE, TuplePat (pl')) end
                                                          (* THE OBVIOUS *)

(****************************************************************************)

        | FlatAppPat patfixitemlist (* patterns before fixity parsing *)
          => patId (patParse(patfixitemlist,env,error)) CE

(* WHAT EXACTLY DOES "patParse" DO? *)

(* In the following case, we check to make sure that an AppPat is well-
   formed.  Specifically, we make sure that the pattern in the function
   place is always a variable (or a variable surrounded by a number of
   Markers). *)

	| AppPat {constr,argument}	(* application *)
          => let fun IsVarPat (VarPat v) = true
                 |   IsVarPat (MarkPat (p,r)) = IsVarPat p
                 |   IsVarPat _ = false

                 val _ = if IsVarPat constr 
                         then ()
                         else Error "Constructors in patterns must be simple."

                 (* This deals with the bug that Wadler pointed out. *)
                 
                 val (_,c') = patId constr CE
                 val (_,a') = patId argument CE
             in (CE,AppPat ({constr=c',argument=a'})) end   (* THE OBVIOUS *) 
	| ConstraintPat {pattern,constraint} (* constraint *)
          => let val (_,p') = patId pattern CE
             in (CE,ConstraintPat {pattern=p', constraint = constraint}) end
                                                            (* THE OBVIOUS *)
	| LayeredPat {varPat,expPat}	(* as expressions *)
          => let val (_,p') = patId varPat CE
                 val (_,p'') = patId expPat CE
             in (CE,LayeredPat {varPat=p', expPat=p''}) end
                                                            (* THE OBVIOUS *)
	| MarkPat (pat,region)	(* mark a pattern *)
          => let val (_,p') = patId pat CE
             in (CE, MarkPat (p',region)) end               (* THE OBVIOUS *)
        | VectorPat patlist                 (* vector pattern *)
          => let val (_,pl') = Smap patId patlist CE
             in (CE,VectorPat (pl')) end                    (* THE OBVIOUS *)
	| OrPat patlist			(* or-pattern *)
          => let val (_,pl') = Smap patId patlist CE
             in (CE,OrPat (pl')) end                        (* THE OBVIOUS *)
)

(* SHOULD WE CONSTRAIN HOW WE DO THE THREADING HERE??? *)

(* STRUCTURE EXPRESSION *)
and strexpId s CE= 
(case s of 
             VarStr path			(* variable structure *)
             => (CE,s)                                      (* DO NOTHING *)
	   | BaseStr  dec			(* defined structure *)
             => let val (CE',d') = decId dec CE
                in (CE',BaseStr (d')) end                 (* THREAD *)
	   | AppStr (p,sbl) (* (path,(strexp,bool)list *) (* application *)
             => let val (CE',sbl') = Smap (onFirst strexpId) sbl CE
                in (CE',AppStr (p,sbl')) end                (* ---?--- *) 
	   | LetStr (dec, strexp)		(* let in structure *)
             => let val (CE',d') = decId dec CE
                    val (CE'',s') = strexpId strexp CE'
                in (CE'',LetStr (d', s')) end               (* THREAD *)
	   | MarkStr (strexp, region) (* mark *)
             => let val (CE',s') = strexpId strexp CE
                in (CE', MarkStr (s', region)) end          (* THREAD *)
)

(* AGAIN, I'M NOT 100% SURE EXACTLY HOW WE SHOULD THREAD THIS GUY. *)

(* FUNCTOR EXPRESSION *)
and fctexpId f CE= 
(case f of 
             VarFct (path, fsigexpsigConst)	(* functor variable *)
             => let val (CE',f') = mapsigConst fsigexpId fsigexpsigConst CE
                in (CE',VarFct (path,f')) end               (* THREAD *)
	   | BaseFct {params,body,constraint}  (* definition of a functor *)
             => let val (CE',p') = Smap (onSecond sigexpId) params CE
                in (CE',BaseFct {params=p',
                        body=body,constraint=constraint}) end
                                                            (* THREAD *)
	   | LetFct (dec, fctexp)
             => let val (CE',d') = decId dec CE
                    val (CE'',e') = fctexpId fctexp CE'
                in (CE'',LetFct (d', e')) end
                                                            (* THREAD *)
	   | AppFct (path, strexpboollist, fsigexpsigConst) (* application *)
             => let val (CE',sbl') = Smap (onFirst strexpId) strexpboollist CE
                    val (CE'',fC') = mapsigConst fsigexpId fsigexpsigConst CE'
                in (CE'', AppFct (path,sbl',fC')) end       (* THREAD *) 
           | MarkFct (fctexp, region)         (* mark *)
             => let val (CE',f') = fctexpId fctexp CE
                in (CE',MarkFct (f', region)) end           (* THREAD *)
)

(* SIGNATURE EXPRESSION *)
and sigexpId s CE = 
(case s of 
             VarSig symbol			(* signature variable *)
             => (CE,s)                                      (* DO NOTHING *)
           | AugSig (sigexp, wherespeclist)     (* sig augmented+where specs *)
             => let val (CE',s') = sigexpId sigexp CE
                in (CE',AugSig (s', wherespeclist)) end     (* THREAD *)
	   | BaseSig speclist		(* defined signature *)
             => let val (CE',s') = Smap specId speclist CE
                in (CE', BaseSig (s')) end                   (* THREAD *)
           | MarkSig (sigexp, region) 	(* mark *)
             => let val (CE',s') = sigexpId sigexp CE
                in (CE',MarkSig (s', region)) end           (* THREAD *)
)

(* FUNCTOR SIGNATURE EXPRESSION *)
and fsigexpId f CE = 
(case f of 
              VarFsig symbol			(* funsig variable *)
              => (CE,f)                                     (* DO NOTHING *)
	    | BaseFsig {param, result}             (* defined funsig *)
              => let val (CE',p') = Smap (onSecond sigexpId) param CE
                     val (CE'',d') = sigexpId result CE' 
                  in (CE'',BaseFsig {param=p', result=d'})
		 end                     (* THREAD *)
	    | MarkFsig (fsigexp, region) 	(* mark a funsig *)
              => let val (CE',f') = fsigexpId fsigexp CE
                 in (CE',MarkFsig (f', region)) end         (* THREAD *)
)

(* SPECIFICATION FOR SIGNATURE DEFINITIONS *)
and specId s CE = 
(case s of
           StrSpec list (* structure *)
           => let val (CE',l') = 
                   Smap (onMiddle (onOption sigexpId)) list CE
              in (CE', StrSpec (l')) end                    (* THREAD *)
         | TycSpec (list, bool) (* type *)
           => (CE,s)                                        (* DO NOTHING *) 
	 | FctSpec symbolfsigexplist		  (* functor *)
           => let val (CE',sfel') = 
                   Smap (onSecond fsigexpId) symbolfsigexplist CE
               in (CE',FctSpec (sfel')) end                 (* THREAD *)
	 | ValSpec symboltylist	          (* value *)
           => (CE,s)                                        (* DO NOTHING *)
         | DataSpec {datatycs, withtycs} (* datatype *)
           => let val (CE',d') = Smap dbId datatycs CE
                  val (CE'', w') = Smap tb withtycs CE'
              in (CE'',DataSpec {datatycs=d', 
                        withtycs=w'}) end                   (* THREAD *)
	 | ExceSpec symboltyoptionlist	  (* exception *)
           => (CE,s)                                        (* DO NOTHING *) 
	 | FixSpec {fixity, ops} (* fixity *)
           => (CE,s)                                        (* DO NOTHING *)
	 | ShareStrSpec pathlist			  (* structure sharing *)
           => (CE,s)                                        (* DO NOTHING *)
	 | ShareTycSpec pathlist			  (* type sharing *)
           => (CE,s)                                        (* DO NOTHING *)
	 | IncludeSpec symbol			  (* include specif *)
           => (CE,s)                                        (* DO NOTHING *)
	 | MarkSpec (spec, region)		          (* mark a spec *)
           => let val (CE',s') = specId spec CE
              in (CE',MarkSpec (s', region)) end            (* THREAD *)
)

(* I'M CERTAIN WE NEED THREADING HERE! *)

(* THE "local" DECLARATION CASE IS CURRENTLY PROBLEMATIC *)

(* DECLARATIONS (let and structure) *)
and decId d CE= 
(case d of 
          ValDec (vblist, tyvarlist)		(* values *)
          =>  let val (CE',v') = Smap vbId vblist CE
              in (CE',ValDec (v', tyvarlist)) end
	| ValrecDec (rvblist, tyvarlist)		(* recursive values *)
          => let val (CE',r') = Smap rvbId rvblist CE
             in (CE',ValrecDec (r', tyvarlist)) end
	| FunDec (fblist, tyvarlist)		(* recurs functions *)
          => let val (CE',f') = Smap fbId fblist CE
             in (CE',FunDec (f', tyvarlist)) end
	| TypeDec tblist				(* type dec *)
          => let val (CE',t') = Smap tb tblist CE
             in (CE',TypeDec (t')) end
	| DatatypeDec {datatycs, withtycs}   (* datatype dec *)
          => let val (CE',d') = Smap dbId datatycs CE
                 val (CE'',w') = Smap tb withtycs CE'
             in (CE'',DatatypeDec {datatycs=d',
                          withtycs=w'}) end
	| AbstypeDec {abstycs, withtycs, body}  (* abstract type *)
          => let val (CE',a') = Smap dbId abstycs CE
                 val (CE'',w') = Smap tb withtycs CE'
                 val (CE'''',b') = decId body CE''
             in (CE'''',AbstypeDec {abstycs=a',
                         withtycs=w',
                         body=b'}) end
	| ExceptionDec eblist			(* exception *)
          => let val (CE',e') = Smap eb eblist CE
             in (CE',ExceptionDec (e')) end
	| StrDec strblist				(* structure *)
          => let val (CE',s') = Smap strbId strblist CE
             in (CE',StrDec (s')) end
	| AbsDec strblist				(* abstract struct *)
          => let val (CE',s') = Smap strbId strblist CE 
             in (CE',AbsDec (s')) end
	| FctDec fctblist				(* functor *)
          => let val (CE', f') = Smap fctbId fctblist CE
             in (CE',FctDec (f')) end
	| SigDec sigblist				(* signature *)
          => let val (CE', s') = Smap sigbId sigblist CE
             in (CE',SigDec (s')) end
	| FsigDec fsigblist				(* funsig *)
          => let val (CE',f') = Smap fsigbId fsigblist CE
             in (CE', FsigDec (f')) end
	| LocalDec (dec1, dec2)				(* local dec *)
          => let val (CE',d') = decId dec1 CE
                 val (CE'',d'') = decId  dec2 CE'
             in (CE', LocalDec (d', d'')) end
	| SeqDec declist				(* sequence of dec *)
          => let val (CE',d') = Smap decId declist CE
             in (CE', SeqDec (d')) end
	| OpenDec pathlist				(* open structures *)
          => (CE,d)
	| OvldDec (symbol, ty, explist)	(* overloading (internal) *)
          => let val (CE',e') = Smap expId explist CE
             in (CE',OvldDec (symbol, ty, e')) end
        | FixDec {fixity, ops}  (* fixity *)
          => (CE,d) 
        | ImportDec stringlist		(* import (unused) *)
          => (CE,d) 
        | MarkDec (dec, region)		(* mark a dec *)
          => let val (CE',d') = decId dec CE
             in (CE', MarkDec (d',region)) end
)

(* VALUE BINDINGS *)  (* ZIDO:  This is dead code *)
and vbId v CE = 
(case v of
         Vb {pat, exp}
         => let val (CE', p') = patId pat CE
                val (CE'', e') = expId exp CE'
            in (CE'', Vb {pat=p', exp=e'}) end
       | LVb {pat, exp}   (* ZIDO:  PWLE *)
         => let val (CE', p') = patId pat CE
                val (CE'', e') = expId exp CE'
            in (CE'',LVb {pat=p', exp=e'}) end
       | MarkVb (vb, region)
         => let val (CE',v') = vbId vb CE
            in (CE',MarkVb (v', region)) end
)

(* RECURSIVE VALUE BINDINGS *)
and rvbId v CE = 
(case v of 
            Rvb {var, fixity, exp, resultty}
            => let val (CE',e') = expId exp CE
               in (CE', Rvb {var=var, fixity=fixity, exp=e', resultty=resultty}) end
          | LRvb {var, fixity, exp, resultty}   (* ZIDO:  PWLE *)
            => let val (CE',e') = expId exp CE
               in (CE',LRvb {var=var, fixity=fixity, exp=e', resultty=resultty}) end
	  | MarkRvb (rvb, region)
            => let val (CE',r') = rvbId rvb CE
               in (CE', MarkRvb (r', region)) end
)
(* RECURSIVE FUNCTIONS BINDINGS *)
and fbId f CE = 
(case f of 
         Fb clauselist
         => let val (CE',c') = Smap clauseId clauselist CE
            in (CE', Fb (c')) end
       | LFb clauselist  (* ZIDO:  PWLE *)
         => let val (CE',c') = Smap clauseId clauselist CE
            in (CE', LFb (c')) end
       | MarkFb (fb, region)
         => let val (CE',f') = fbId fb CE
            in (CE', MarkFb (f', region)) end
)

(* CLAUSE: a definition for a single pattern in a function binding *)
(* ZIDO:  THIS SHOULD NOT CHANGE!  IT WILL ALWAYS BE NEEDED!! *)
and clauseId c CE = 
(case c of 
            Clause {pats, resultty, exp}
            => let val (CE',p') = Smap (onFixitem patId) pats CE
                   val (CE'',e') = expId exp CE'
               in (CE'',Clause {pats=p', 
                       resultty=resultty, exp=e'}) end
)

(* TYPE BINDING *)
and tb t CE = 
(case t of 
         Tb {tyc, def, tyvars}
         => (CE,t)
       | MarkTb (tb, region)
         => (CE,t)
)

(* DATATYPE BINDING *)  (* ZIDO:  This code can be garbage collected. *)
and dbId d CE= 
(case d of
         Db {tyc, tyvars, rhs}
         => let val (CE',r') = dbrhs rhs CE
            in (CE', Db {tyc=tyc, tyvars=tyvars, rhs=r'}) end
       | LDb {tyc, tyvars, rhs}   (* ZIDO:  PWLE *)
         => let val (CE',r') = dbrhs rhs CE
            in (CE', LDb {tyc=tyc, tyvars=tyvars, rhs=r'}) end
       | MarkDb (db, region)
         => let val (CE',d') = dbId db CE
            in (CE',MarkDb (d', region)) end
)

(* DATATYPE BINDING RIGHT HAND SIDE *)
and dbrhs d CE = 
(case d of 
            Constrs symboltyoptionlist

            (* This case is a bit counterintuitive.  But we need to remove
               new bindings from CE if they "shadow" old ones. *)
            => (CE,d)
	  | Repl symbollist
            => (CE,d)
)

(* EXCEPTION BINDING *)
and eb e CE = 
(case e of 
         EbGen {exn, etype} (* Exception definition *)
         => (CE,e)
       | EbDef {exn, edef}	  (* defined by equality *)
         => (CE,e)
       | MarkEb (eb, region)
         => (CE,e)
)

(* STRUCTURE BINDING *)
and strbId s CE = 
(case s of
           Strb {name,def,constraint}
           => let val (CE',d') = strexpId def CE
                  val (CE'',c') = mapsigConst sigexpId constraint CE'
              in (CE'', Strb {name=name,def=d',
                             constraint=c'}) end
	 | MarkStrb (strb, region)
           => let val (CE',s') = strbId strb CE
              in (CE',MarkStrb (s', region)) end
)

(* FUNCTOR BINDING *)
and fctbId f CE = 
(case f of 
           Fctb {name,def}
           => let val (CE', d') = fctexpId def CE
              in (CE',Fctb {name=name,def=d'}) end
	 | MarkFctb (fctb, region)
           => let val (CE',f') = fctbId fctb CE
              in (CE', MarkFctb (f', region)) end
)

(* SIGNATURE BINDING *)
and sigbId s CE = 
(case s of 
           Sigb {name,def}
           => let val (CE',d') = sigexpId def CE
              in (CE',Sigb {name=name,def=d'}) end
	 | MarkSigb (sigb, region)
           => let val (CE',s') = sigbId sigb CE
              in (CE',MarkSigb (s', region)) end
)

(* FUNSIG BINDING *)
and fsigbId f CE = 
(case f of 
            Fsigb {name,def}
            => let val (CE',d') = fsigexpId def CE
               in (CE',Fsigb {name=name,def=d'}) end
	  | MarkFsigb (fsigb,region)
            => let val (CE',f') = fsigbId fsigb CE
               in (CE',MarkFsigb (f',region)) end
)

(* TYPE VARIABLE *)  (* ZIDO:  It seems that this is never used. *)
and tyvar t = 
(case t of 
            Tyv symbol 
            => t
	  | MarkTyv (tyvar, region)
            => t
)

(* TYPES *)  (* ZIDO:  and is this ever used??? *)
and ty t =
(case t of  
      VarTy tyvar			(* type variable *)
      => t
    | ConTy (symbollist, tylist)	(* type constructor *)
      => t
    | RecordTy symboltylist 	(* record *)
      => t
    | TupleTy tylist		(* tuple *)
      => t
    | MarkTy (ty, region)	        (* mark type *)
      => t
)
 
in

  fun fixityparse {ast,compenv,compInfo} =
       let val ({error,...}:ElabUtil.compInfo) = compInfo
           val env = SCStaticEnv.unSC compenv
           val CE = (env,error)

           (* We want to control fixity parsing, at least
              because it currently interferest with lazycomp. *)

           val (_,ast') = if !Control.Lazy.earlyFP then decId ast CE
                                                   else (CE,ast)
       in
       {ast=ast', compenv=compenv, compInfo=compInfo}
       end

end

end



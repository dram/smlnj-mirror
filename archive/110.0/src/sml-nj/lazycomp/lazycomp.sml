
(* lazycomp.sml *)  (* ZIDO:  PWLE:  This file is where we define *)
                    (* ZIDO:         the lazycomp translation     *)

(* Wed Jun 25 14:38:32 EDT 1997 *)

(* This structure contains the implementation of the functions
   used to translate "datatype lazy", "fun lazy", "val lazy",
   and "val rec lazy" into standard SML declarations.  WT.     *)

(* NB:  Eventually, you will need to go through and make sure
        that in the translation function, this "almostId"
        function applies the approriate almostId to its sub-components,
        instead of returning them unchanged.

        This is really important, and requires a careful systematic
        check.  Otherwise, the function will be left with some
        nasty (hard-to-find) bugs. 

        Unfortunately, this is an important invariant that we would like
        to hold, but is clearly not enforced by the type discipline.

*)

signature LAZY = 
sig

  (* The sole purpose of this type T is just
     as a short hand. *)

  type T = {ast: Ast.dec,
            compenv: SCStaticEnv.staticEnv,
            compInfo: ElabUtil.compInfo}

  val lazycomp : T -> T

  val delayStrRef   : string ref
  val forceStrRef   : string ref

end

structure Lazy : LAZY =
struct

  type T = {ast: Ast.dec,
            compenv: SCStaticEnv.staticEnv,
            compInfo: ElabUtil.compInfo}

local

exception LazyComp

fun error (s:string) = (print "\n---";print s; print "\n"; raise LazyComp)

val debug = fn s:string => if !Control.Lazy.printDebug then print s else ();

fun first (x,y) = x;
fun second (x,y) = y;

type P  = Symbol.symbol list
type CE = (P * bool) list

val CE0 = [];

fun ninCE [] x = false
|   ninCE ((y,A)::ys) x = if y=x then A=false
                                 else ninCE ys x;

fun uinCE [] x = false
|   uinCE ((y,A)::ys) x = if y=x then A=true
                                 else uinCE ys x;

fun p2s p = ((fn [s]=>Symbol.name s
                | _ =>error "In p2s:  Path name must be a singlton list") p);

fun pp p  = (debug "[Symbol : ";
             debug (p2s p);
             debug "[\n");

(* New Type Constructor Name *)  (* ZIDO:  Can be made more elaborate later *)

(* Warning:  Nasty Hack:  The space at the end of "_ " ensures that these
   names are not user-accessable. *)

fun nTCn s = Symbol.tycSymbol ((Symbol.name s)^"_ ");

(* New Variable Name *)  (* ZIDO:  Can be made more elaborate later *)

fun nVn s = Symbol.varSymbol ((Symbol.name s)^"_ ");

fun PnVn [a]      = [nVn a]
|   PnVn _        = error "In PnVn:  Must be a singleton list."


fun  remCE (CE:CE) (p:P) =
(case CE of
  [] => []:CE
| ((x,y)::xs) =>
           (let val l=remCE xs p
            in if x=p then l else (x,y)::l end):CE
)

val remCE = fn CE => fn p =>
  (debug "\n";
   debug ("--Removing "^(p2s p));
   remCE CE p);

fun purge (CE:CE) (p:P) = remCE CE (map nVn p)

fun uextCE (CE:CE) (p:P) = 
(debug "\n";
 debug ("--Extending Unarry "^(p2s p));
 debug "\n";
 ((p,true)::(purge CE p)):CE);

fun nextCE (CE:CE) (p:P) = 
(debug "\n";
 debug ("--Extending Nullary "^(p2s p));
 debug "\n";
 ((p,false)::(purge CE p)):CE);

fun lextCE CE [] = CE
|   lextCE CE ((p,to)::ps) = 
      let val CE' = if Option.isSome to
                       then uextCE CE p
                       else nextCE CE p
      in lextCE (CE') ps end;

fun lremCE CE [] = CE
|   lremCE CE ((p,to)::ps) = lremCE (remCE CE p) ps;

(* ZIDO:  Shouldn't we be able to hook "suspSym" used here
   to "suspSym" in basic types? *)

(* Functions used in the actually tranlsation from "sml+lazy" -> "sml+$$" *)

(* ZIDO:  The following functions must be deterministic, because we would
          like this encoding to be uniform (preserving equality) *)

val a = ref 0;

fun genSym s = s^"_"^(Int.toString (a:= !a+1; !a));

fun Nnames 0 = []
|   Nnames n = ([Symbol.varSymbol (genSym "$")])::(Nnames (n-1))

(* ZIDO:  I assume that the above is the right way to apply
          nVn to a name in path form *) 



(* **************************** *)


(* EXPRESSIONS *)

open Ast AstUtil

val suspSym      = Symbol.tycSymbol "susp"   (* ZIDO:  PWLE *)
val dollarSym    = Symbol.varSymbol "$"      (* ZIDO:  PWLE *)
val delaySym     = Symbol.varSymbol "delay"  (* ZIDO:  PWLE *)
val forceSym     = Symbol.varSymbol "force"  (* ZIDO:  PWLE *)


(* 
I don't know why this doesn't work:
val forceSym     = Symbol.varSymbol "Compiler.Lazy.force" (* ZIDO:  PWLE *)
*)

(* Note "forceSym" above is the wrong way to do things. *)

(* Here are some generic extra syntactic constructions that will come in
   handy during the translation of lazy constructs. *)

(* ZIDO:  DropExp is switched off because of the type casting that 
          we are doing with having a "delay" and a "$" that have
          different functions. *)

val S = Symbol.varSymbol;

val Dollar = VarExp [dollarSym]
val Delay  = 
      VarExp [Symbol.strSymbol "Compiler", Symbol.strSymbol "Lazy",delaySym]
val Force  = 
      VarExp [Symbol.strSymbol "Compiler", Symbol.strSymbol "Lazy",forceSym]

fun DollarExp e =
 let fun RealDelayExp e  = AppExp {function=Delay, 
                                   argument=FnExp [(Rule {pat=unitPat, 
                                                          exp= e})]}
     fun JustDollarExp e = AppExp {function=Dollar,argument=e}
 in
   if !Control.Lazy.earlyDT
     then RealDelayExp e
     else JustDollarExp e
 end

fun DollarPat p = AppPat {constr=VarPat [dollarSym], argument=p}

val UnDollarExp =
   let val X = [Symbol.varSymbol (genSym "$!")]
     in FnExp 
         [(Rule {pat=DollarPat(VarPat X),
                 exp= VarExp X})]
     end

fun isUnDollar e =

(* This function does not seem to be catching anything, probably because
   of all the "FlatAppExp"s and "FlatAppPat"s. *)

(case e of
   MarkExp (e,r) => isUnDollar e
 | FnExp [(Rule {pat=MarkPat(p,r),exp})] =>
                    isUnDollar (FnExp [(Rule {pat=p,exp=exp})])
 | FnExp [(Rule {pat,exp=MarkExp(e,r)})] =>
                    isUnDollar (FnExp [(Rule {pat=pat,exp=e})])
 | FnExp [(Rule {pat=AppPat {constr=MarkPat(p,r), argument},exp})] =>
   isUnDollar (FnExp [(Rule {pat=AppPat {constr=p, argument=argument},
                             exp=exp})])
 | FnExp [(Rule {pat=AppPat {constr, argument=MarkPat(p,r)},exp})] =>
   isUnDollar (FnExp [(Rule {pat=AppPat {constr=constr, argument=p},
                             exp=exp})])
 | FnExp [(Rule {pat=AppPat {constr=VarPat [dS], argument=VarPat x1},
                 exp= VarExp x2})] => (dS=dollarSym) andalso (x1=x2)
 | _ => false)
   

fun expEq e1 e2 =
(case (e1,e2) of
  (MarkExp (a,_),_) => expEq a e2
| (_,MarkExp (b,_)) => expEq e1 b
| (_,_) => (e1 = e2))

fun isDollared e =
(case e of
  MarkExp (a,_) => isDollared a
| AppExp {function=f,argument=a} => (expEq f Dollar)
| _ => false)

fun unDollar e =
(case e of
  MarkExp (a,_) => unDollar a
| AppExp {function=f,argument=a} => a
| _ => error "unDollar applied to an expression with no Dollar application")

fun AppExp' {function,argument} =
(if (isUnDollar function) andalso (isDollared argument)
 then (
      debug "--SMART AppExp WORKED ON ONE REDEX!!\n";
      unDollar argument
      )
 else AppExp {function=function,argument=argument})

fun ForceExp e =
    if (!Control.Lazy.redGenFD1) andalso (isDollared e)
    then (debug "--UnDollar one expression!";unDollar e)
    else (debug "--Adding one force.";
          AppExp'{function=UnDollarExp,argument=e})

(* ZIDO:  NB:  Major Bug:  I will first implement extending the 
               environment with names of variables that need to be
               "Dollared", and then I will deal with removing names
               later.  The latter is actually tricky because there are
               all sorts of binding mechanisms in SML.  So, I will
               postpone dealing with that for now.  *)

fun CheckVPP1 CE (p:P) = if ninCE CE p 
                                 then DollarPat (VarPat (PnVn p))
                                 else VarPat p;

fun CheckVEP CE (p:P) = 
 if uinCE CE p then 
  let val X = [Symbol.varSymbol (genSym "$!")]
  in FnExp 

      (* Treatment of Unarry Constructors *)

      [(Rule {pat=VarPat X,
              exp= DollarExp (AppExp' {function=VarExp (PnVn p),
                                      argument=VarExp X})})]
  end
 else 
 if ninCE CE p 
 then DollarExp (VarExp (PnVn p))
 else VarExp p;

fun Apply (f,x) = AppExp' {argument=x,function=f}

fun ListApply (f,[]) = f
|   ListApply (f,(x::xs)) = ListApply(Apply(f,x),xs)

fun mapfixitem f {item,fixity,region} = 
              {item=f item,fixity=fixity,region=region}

(* NOTE:  unFix is not good style.  It's used twice.  Once usage
          is irrelevant (FlatApp).  I'm not sure how to get rid
          of the other. *)

fun unFix {item,fixity,region} = item

(* NOTE:  Fix is even worse.  It is used in fn2let and rvBXmkY.  *)

fun Fix a = {item=a,fixity=NONE,region=(0,0)}

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

(* The following function loses the CE resulting from the
   compution, but I really have no idea what this "option"
   type is supposed to be doing, and don't know how to
   implement a proper "onOption" function. *)

fun onOption f xo CE = 
                  let fun F x = (fn(a,b)=>b) (f x CE)
                  in (CE,mapoption (F) xo) end;

(* ZIDO:  Write "f CE item" instead of "f item CE" in "onFixitem",
          and you get about 200 error messages in one shot!  (That
          give you no hint that this is really where you screwed up!) *)

fun Null [] = true
|   Null _  = false

fun expId e CE = 
(case e of
    VarExp (path)		(* variable *)
    => (CE,CheckVEP CE path)
  | FnExp rulelist		(* abstraction *)
    => 
       let val (CE',rulelist') = Smap ruleId rulelist CE
       in (CE',FnExp (rulelist')) end
  | FlatAppExp expfixitemlist (* expressions before fixity parsing *)
    =>(
      debug "--Lazycomp:  Encountered FlatAppExp!!\n";
      if (Null expfixitemlist) orelse (not (expEq (unFix (hd expfixitemlist)) Dollar))
      then 
       let val (CE',efil') = Smap (onFixitem expId) expfixitemlist CE
       in (CE', FlatAppExp (efil')) end
      else 
       let val (CE',efil') = Smap (onFixitem expId) (tl expfixitemlist) CE
       in (CE', DollarExp (unFix (hd (efil')))) end
      )
  | AppExp {function,argument}  (* application *)
    => if (expEq function Dollar) 
      then
       let val (CE',a) = expId argument CE
       in (CE',DollarExp a) end 
      else 
       let val (CE',f') = expId function CE
           val (CE'',a') = expId argument CE'
       in (CE'',AppExp' {function=f',argument=a'}) end
  | CaseExp {expr,rules}  (* case expression *)
    => let val (CE',e') = expId expr CE
           val (CE'',r') = Smap ruleId rules CE'
        in (CE'',CaseExp {expr=e',rules=r'}) end
  | LetExp {dec,expr} (* let expression *)
    => let val (CE',d') = decId dec CE
           val (CE'',e') = expId expr CE'
       in (CE'',LetExp {dec=d',expr=e'}) end
  | SeqExp explist		(* sequence of expressions *)
    => let val (CE',el') = Smap expId explist CE
       in (CE', SeqExp (el')) end
  | IntExp literal		(* integer *)
    => (CE,e)
  | WordExp literal		(* word literal *)
    => (CE,e)
  | RealExp string		(* floating point coded by its string *)
    => (CE,e)
  | StringExp string		(* string *)
    => (CE,e)
  | CharExp string			(* char *)
    => (CE,e)
  | RecordExp symbolexplist	(* record *)
    => let val (CE', sel') = Smap (onSecond expId) symbolexplist CE
       in (CE', RecordExp (sel')) end
  | ListExp explist	        (*  [list,in,square,brackets] *)
    => let val (CE',el') = Smap expId explist CE
       in (CE',ListExp (el')) end
  | TupleExp explist	(* tuple (derived form) *)
    => let val (CE',el') = Smap expId explist CE
       in (CE',TupleExp (el')) end
  | SelectorExp symbol	(* selector of a record field *)
    => (CE,e)
  | ConstraintExp {expr,constraint}  (* type constraint *)
    => let val (CE',e') = expId expr CE
       in (CE', ConstraintExp {expr=e',constraint=constraint}) end
  | HandleExp {expr, rules}  (* exception handler *)
    => let val (CE',e') = expId expr CE
           val (CE'',r') = Smap ruleId rules CE'
       in (CE'',HandleExp {expr=e', rules=r'}) end
  | RaiseExp exp 		(* raise an exception *)
    => let val (CE',e') = expId exp CE
       in (CE',RaiseExp (e')) end
  | IfExp {test, thenCase, elseCase}  (* if expression (derived form) *)
    => let val (CE',c') = expId test CE
           val (CE'',t') = expId thenCase CE'
           val (CE''',f') = expId elseCase CE''
       in (CE''', IfExp {test=c', thenCase=t', elseCase=f'}) end
  | AndalsoExp (exp1,exp2)	(* andalso (derived form) *)
    => let val (CE',e') = expId exp1 CE
           val (CE'',e'') = expId exp2 CE'
       in (CE'',AndalsoExp(e',e'')) end
  | OrelseExp (exp1,exp2) (* orelse (derived form) *)
    => let val (CE',e') = expId exp1 CE
           val (CE'',e'') = expId exp2 CE'
       in (CE'',OrelseExp(e',e'')) end

  | WhileExp {test,expr}  (* while (derived form) *)
    => let val (CE',e') = expId test CE
           val (CE'',e'') = expId expr CE'
       in (CE'',WhileExp {test=e',expr=e''}) end
  | MarkExp (exp,region) 	(* mark an expression *)
    => let val (CE',e') = expId exp CE
       in (CE',MarkExp (e',region)) end
  | VectorExp explist       (* vector *)
    => let val (CE',el') = Smap expId explist CE
       in (CE', VectorExp (el')) end
)

(* RULE for case functions and exception handler *)
and ruleId r CE = 
(case r of 
   (Rule {pat,exp}) => let val (CE',p') = patId pat CE
                           val (CE'',e') = expId exp CE'
                       in (CE'',Rule {pat=p',exp=e'}) end
)

(* PATTERN *)
and patId p CE =
(case p of
          WildPat				(* empty pattern *)
          => (CE,p)
	| VarPat path			(* variable pattern *)
          => (CE,CheckVPP1 CE path)
	| IntPat literal			(* integer *)
          => (CE,p)
	| WordPat literal			(* word literal *)
          => (CE,p)
	| StringPat string			(* string *)
          => (CE,p)
	| CharPat string			(* char *)
          => (CE,p)
	| RecordPat {def, flexibility}  (* record *)
          => let val (CE',d') = Smap (onSecond patId) def CE
             in (CE',RecordPat {def=d',flexibility=flexibility}) end
        | ListPat patlist		       (*  [list,in,square,brackets] *)
          => let val (CE',pl') = Smap patId patlist CE
             in (CE',ListPat (pl')) end
	| TuplePat patlist			(* tuple *)
          => let val (CE',pl') = Smap patId patlist CE
             in (CE', TuplePat (pl')) end
        | FlatAppPat patfixitemlist (* patterns before fixity parsing *)
          => (
             debug "--Lazycomp:  Encountered FlatAppPat!! \n";
             let val (CE',pfil') = Smap (onFixitem patId) patfixitemlist CE
                 val _           = debug "--Flat Pattern. \n"
             in (CE', CheckVPP3 CE (FlatAppPat (pfil'))) end
             )
	| AppPat {constr,argument}	(* application *)
          => let val (CE',c') = patId constr CE
                 val (CE'',a') = patId argument CE'
             in (CE'',AppPat (CheckVPP2 CE''{constr=c',argument=a'})) end
	| ConstraintPat {pattern,constraint} (* constraint *)
          => let val (CE',p') = patId pattern CE
             in (CE',ConstraintPat {pattern=p', constraint = constraint}) end
	| LayeredPat {varPat,expPat}	(* as expressions *)
          => let val (CE',p') = patId varPat CE
                 val (CE'',p'') = patId expPat CE'
             in (CE'',LayeredPat {varPat=p', expPat=p''}) end
	| MarkPat (pat,region)	(* mark a pattern *)
          => let val (CE',p') = patId pat CE
             in (CE', MarkPat (p',region)) end
        | VectorPat patlist                 (* vector pattern *)
          => let val (CE',pl') = Smap patId patlist CE
             in (CE',VectorPat (pl')) end
	| OrPat patlist			(* or-pattern *)
          => let val (CE',pl') = Smap patId patlist CE
             in (CE',OrPat (pl')) end
)
(* This test actually need to be defined in parallel *)

and CheckVPP2 CE {constr=VarPat p,argument=a'} =
      if uinCE CE p then {constr=VarPat [dollarSym],
                          argument=AppPat {constr=VarPat (PnVn p),
                                              argument=second (patId a' CE)}}
                    else {constr=VarPat p,argument=second (patId a' CE)}
  | CheckVPP2 CE {constr=MarkPat(p,r),argument=a'} = 
                CheckVPP2 CE {constr=p,argument=a'}
  | CheckVPP2 CE {constr=FlatAppPat l,argument=a'} = error "FlatAppPat in CheckVPP2"
  | CheckVPP2 CE _ = error "Other in CheckVPP2"

and CheckVPP3 CE (FlatAppPat []) = FlatAppPat []
  | CheckVPP3 CE (FlatAppPat (pfi::ps)) =
(case pfi of
  {item=VarPat p,fixity,region} =>
    if uinCE CE p then 
             DollarPat (FlatAppPat
               ({item=VarPat (PnVn p),fixity=fixity,region=region}
                ::ps))
    else (FlatAppPat (pfi::ps))

| _ => (FlatAppPat (pfi::ps)))
  | CheckVPP3 CE _ = error "CheckVPP3 is only defined over FlatAppPat's"
     

(* STRUCTURE EXPRESSION *)
and strexpId s CE= 
(case s of 
             VarStr path			(* variable structure *)
             => (CE,s)
	   | BaseStr  dec			(* defined structure *)
             => let val (CE',d') = decId dec CE
                in (CE',BaseStr (d')) end
	   | AppStr (p,sbl) (* (path,(strexp,bool)list *) (* application *)
             => let val (CE',sbl') = Smap (onFirst strexpId) sbl CE
                in (CE',AppStr (p,sbl')) end
	   | LetStr (dec, strexp)		(* let in structure *)
             => let val (CE',d') = decId dec CE
                    val (CE'',s') = strexpId strexp CE'
                in (CE'',LetStr (d', s')) end
	   | MarkStr (strexp, region) (* mark *)
             => let val (CE',s') = strexpId strexp CE
                in (CE', MarkStr (s', region)) end
)

(* FUNCTOR EXPRESSION *)
and fctexpId f CE= 
(case f of 
             VarFct (path, fsigexpsigConst)	(* functor variable *)
             => let val (CE',f') = mapsigConst fsigexpId fsigexpsigConst CE
                in (CE',VarFct (path,f')) end
	   | BaseFct {params,body,constraint}  (* definition of a functor *)
             => let val (CE',p') = Smap (onSecond sigexpId) params CE
                in (CE',BaseFct {params=p',
                        body=body,constraint=constraint}) end
	   | LetFct (dec, fctexp)
             => let val (CE',d') = decId dec CE
                    val (CE'',e') = fctexpId fctexp CE'
                in (CE'',LetFct (d', e')) end
	   | AppFct (path, strexpboollist, fsigexpsigConst) (* application *)
             => let val (CE',sbl') = Smap (onFirst strexpId) strexpboollist CE
                    val (CE'',fC') = mapsigConst fsigexpId fsigexpsigConst CE'
                in (CE'', AppFct (path,sbl',fC')) end 
           | MarkFct (fctexp, region)         (* mark *)
             => let val (CE',f') = fctexpId fctexp CE
                in (CE',MarkFct (f', region)) end
)

(* SIGNATURE EXPRESSION *)
and sigexpId s CE = 
(case s of 
             VarSig symbol			(* signature variable *)
             => (CE,s)
           | AugSig (sigexp, wherespeclist)     (* sig augmented with where specs *)
             => let val (CE',s') = sigexpId sigexp CE
                in (CE',AugSig (s', wherespeclist)) end
	   | BaseSig speclist		(* defined signature *)
             => let val (CE',s') = Smap specId speclist CE
                in (CE', BaseSig (s')) end
           | MarkSig (sigexp, region) 	(* mark *)
             => let val (CE',s') = sigexpId sigexp CE
                in (CE',MarkSig (s', region)) end
)

(* FUNCTOR SIGNATURE EXPRESSION *)
and fsigexpId f CE = 
(case f of 
              VarFsig symbol			(* funsig variable *)
              => (CE,f)
	    | BaseFsig {param, result}             (* defined funsig *)
              => let val (CE',p') = Smap (onSecond sigexpId) param CE
                     val (CE'',d') = sigexpId def CE' 
                  in (CE'',BaseFsig {param=p', result=d'})
		 end
	    | MarkFsig (fsigexp, region) 	(* mark a funsig *)
              => let val (CE',f') = fsigexpId fsigexp CE
                 in (CE',MarkFsig (f', region)) end
)

(* SPECIFICATION FOR SIGNATURE DEFINITIONS *)
and specId s CE = 
(case s of
           StrSpec list (* structure *)
           => let val (CE',l') = 
                   Smap (onMiddle (onOption sigexpId)) list CE
              in (CE', StrSpec (l')) end
         | TycSpec (list, bool) (* type *)
           => (CE,s)
	 | FctSpec symbolfsigexplist		  (* functor *)
           => let val (CE',sfel') = 
                   Smap (onSecond fsigexpId) symbolfsigexplist CE
               in (CE',FctSpec (sfel')) end
	 | ValSpec symboltylist	          (* value *)
           => (CE,s)
         | DataSpec {datatycs, withtycs} (* datatype *)
           => let val (CE',d') = Smap dbX1 datatycs CE
                  val (CE'', w') = Smap tb withtycs CE'
                  val (CE''',d'') = dbX2 datatycs CE''
              in (CE''',DataSpec {datatycs=d', 
                        withtycs=(w') @ (d'')}) end
	 | ExceSpec symboltyoptionlist	  (* exception *)
           => (CE,s) 
	 | FixSpec {fixity, ops} (* fixity *)
           => (CE,s)
	 | ShareStrSpec pathlist			  (* structure sharing *)
           => (CE,s)
	 | ShareTycSpec pathlist			  (* type sharing *)
           => (CE,s)
	 | IncludeSpec symbol			  (* include specif *)
           => (CE,s)
	 | MarkSpec (spec, region)		          (* mark a spec *)
           => let val (CE',s') = specId spec CE
              in (CE',MarkSpec (s', region)) end
)

(* DECLARATIONS (let and structure) *)
and decId d CE= 
(case d of 
          ValDec (vblist, tyvarlist)		(* values *)
          =>  let val (CE',v') = Smap vbX vblist CE
              in (CE',ValDec (v', tyvarlist)) end
	| ValrecDec (rvblist, tyvarlist)		(* recursive values *)
          => let val (svl,lvl) = rvbXfndS rvblist
                 val (CE',lrvblist) = Smap rvbXMkLazy rvblist CE
                 val Dec1 = rvBXmkY (length (lrvblist))
                 val Dec2 = rvBXuseY lrvblist
                 val Dec3= rvBXmkVal (svl,lvl)
             in (CE', LocalDec (SeqDec [Dec1,Dec2],Dec3)) end
	| FunDec (fblist, tyvarlist)		(* recurs functions *)
          => let val (CE',f') = Smap fbX1 fblist CE
                 val (CE'',f'') = fbX2 fblist CE'
             in (CE'',FunDec ((f') @ (f''), tyvarlist)) end
	| TypeDec tblist				(* type dec *)
          => let val (CE',t') = Smap tb tblist CE
             in (CE',TypeDec (t')) end
	| DatatypeDec {datatycs, withtycs}   (* datatype dec *)
          => let val (CE',d') = Smap dbX1 datatycs CE
                 val (CE'',w') = Smap tb withtycs CE'
                 val (CE''',d'') = dbX2 datatycs CE''
             in (CE''',DatatypeDec {datatycs=d',
                          withtycs=(w') @ (d'')}) end
	| AbstypeDec {abstycs, withtycs, body}  (* abstract type *)
          => let val (CE',a') = Smap dbX1 abstycs CE
                 val (CE'',w') = Smap tb withtycs CE'
                 val (CE''',a'') = dbX2 abstycs CE''
                 val (CE'''',b') = decId body CE'''
             in (CE'''',AbstypeDec {abstycs=a',
                         withtycs=(w') @ (a''),
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

(* VALUE BINDINGS TRANSFORMATION *)
and vbX v CE = 
(case v of
         Vb {pat, exp}
         => let val (CE',p') = patId pat CE
                val (CE'',e') = expId exp CE'
            in (CE'',Vb {pat=p', exp=e'}) end
       | LVb {pat, exp}   (* ZIDO:  PWLE *)
         => let val (CE',p') = patId pat CE
                val (CE'',e') = expId exp CE'
            in (CE'', Vb {pat=p', 
                          exp=DollarExp(ForceExp(e'))}) end
       | MarkVb (vb, region)
         => let val (CE',v') = vbX vb CE
            in (CE', MarkVb (v', region)) end
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
            in (CE'',Vb {pat=p', exp=e'}) end
       | MarkVb (vb, region)
         => let val (CE',v') = vbId vb CE
            in (CE',MarkVb (v', region)) end
)

(* IDENTIFY NAMES OF STRICT DECLARATIONS *)

and rvbXfndS []                    = ([],[])
  | rvbXfndS ((Rvb {var,...})::xs) =
      let val (a,b) =  (rvbXfndS xs)
      in (var::a,b) end
  | rvbXfndS ((LRvb {var,...})::xs) =
      let val (a,b) =  (rvbXfndS xs)
      in (a,var::b) end
  | rvbXfndS ((MarkRvb (x,_))::xs) =   (rvbXfndS (x::xs))
  | rvbXfndS (_ :: xs)             =        (rvbXfndS xs)

(* CODE TO FORCE ALL THE STRICT VALUES *)

and rvBXmkVal (svl,lvl) =
let val strct = map (fn s => Vb {pat=DollarPat(VarPat ([s])), 
                         exp=VarExp ([s])})
                    svl
    val lazee = map (fn s => Vb {pat=(VarPat ([s])), 
                         exp=VarExp ([s])})
                    lvl
in ValDec (strct @ lazee, [])
end

(* GENERATE Y COMBINATE TAKING FOR N-TUPLE OF FUNCTIONS *)

and rvBXmkY n =
let fun upto 0 = []
      | upto n = n::(upto (n-1))
    val base = upto n
    fun repeat f = map f base
    val nameY = ("$!Y"^(Int.toString n))
    val  symY = Symbol.varSymbol nameY
    val  YExp = VarExp [symY]
    val nameN = ("$!Never")
    val  symN = Symbol.varSymbol nameN
    val  NExp = VarExp [symN]
    val nameR = ("ref")
    val  symR = Symbol.varSymbol nameR
    val  RExp = VarExp [symR]
    val nameB = ("!")
    val  symB = Symbol.varSymbol nameB
    val  BExp = VarExp [symB]
    val Dec1  = ExceptionDec [(EbGen {exn=symN,etype=NONE})]

    val raExp = RaiseExp (NExp)
    val draEx = AppExp {function=Dollar,argument=raExp}
    val rdraE = AppExp {function=RExp,   argument=draEx}
    fun R n   = [Symbol.varSymbol ("$!R"^(Int.toString n))]
    fun F n   = [Symbol.varSymbol ("$!F"^(Int.toString n))]
    val Rpat  = TuplePat (repeat (VarPat o R))
    val Rexp  = TupleExp (repeat (fn _ => rdraE))
    val Dec2  = ValDec ([Vb {pat=Rpat, exp=Rexp}],[])

    fun Hold e = Apply (Dollar,ForceExp e)
    fun dfbr n = Hold (Apply (BExp,VarExp (R n)))
    val nameD = ("$!D")
    val  symD = Symbol.varSymbol nameD
    val  DExp = VarExp [symD]
    val Dpat  = VarPat [symD]
    val Dexp  = TupleExp (repeat dfbr)
    val Dec3  = ValDec ([Vb {pat=Dpat, exp=Dexp}],[])

    val nameA = (":=")
    val  symA = Symbol.varSymbol nameA
    val  AExp = VarExp [symA]

    fun arHa n = Apply (AExp,
                        TupleExp([VarExp(R n),
                                  Hold(Apply(VarExp(F n),
                                       Dexp))]))
    val AExp  = SeqExp(repeat arHa)
    val Dec4  = ValDec ([Vb {pat=WildPat, exp=AExp}],[])

    val Exp   = LetExp {dec=SeqDec [Dec1,Dec2,Dec3,Dec4],
                        expr=DExp}

    val ArgPat = TuplePat (repeat (VarPat o F))

    val FulExp = FnExp [Rule {pat=ArgPat,
                              exp=Exp}]
in

FunDec ([Fb [Clause {pats=[Fix(VarPat [symY]),
                           Fix(ArgPat)],
                     resultty=NONE,
                     exp=Exp}]],[])


end

and rvBXuseY lrvblist = 
   let fun split []                     = ([],[])
         | split ((LRvb {var,exp,...})::xs) =
              let val (a,b) = split xs
              in (var::a,exp::b) end
         | split ((MarkRvb (x,_))::xs) = split (x::xs)
         | split _ = error "split applied to list of non-LRvb's"

       val (vars,exps) = split lrvblist
       val Pat  = TuplePat (map (fn s => VarPat [s]) vars)
       fun Fn e = FnExp ([Rule {pat=Pat,exp=e}])
       val Fns  = TupleExp (map Fn exps)
       
       (* This should really be a common construction
          bettween this function and the function 
          that generates the code for the Y combinator *)

       val n     = length (lrvblist)
       val nameY = ("$!Y"^(Int.toString n))
       val  symY = Symbol.varSymbol nameY
       val  YExp = VarExp [symY]

       val Ap   = Apply (YExp, Fns)
       val Dec1 = ValDec ([Vb {pat=Pat,exp=Ap}],[])
in
  Dec1
end

(* RECURSIVE VALUE BINDINGS *)
and rvbId v CE = 
(case v of 
            Rvb {var, fixity, exp, resultty}
            => let val (CE',e') = expId exp CE
               in (CE', Rvb {var=var, fixity=fixity, exp=e', resultty=resultty}) end
          | LRvb {var, fixity, exp, resultty}   (* ZIDO:  PWLE *)
            => let val (CE',e') = expId exp CE
               in (CE',Rvb {var=var, fixity=fixity, exp=e', resultty=resultty}) end
	  | MarkRvb (rvb, region)
            => let val (CE',r') = rvbId rvb CE
               in (CE', MarkRvb (r', region)) end
)

(* We need this function to preserve polymorpshism *)
(*  when we change a valrec into a val. *)

and fn2let (MarkExp (e,r)) = fn2let e
  | fn2let (FnExp rl) =
     let val F =  [Symbol.varSymbol (genSym "$!")]
         fun rl2cs [] = []
           | rl2cs ((Rule {pat,exp})::rl) =
               [Clause {pats=[Fix(VarPat F),
                              Fix(pat)],
                        resultty=NONE,
                        exp=exp}]
         val cs = rl2cs rl
         val d = FunDec ([Fb cs],[])
     in LetExp {dec=d, expr=VarExp F}
     end
  | fn2let _ = error "fn2let is only defined for fn's"

(* MAKE ALL VALREC BINDINGS LAZY *)
and rvbXMkLazy v CE = 
(case v of 
            Rvb {var, fixity, exp, resultty}
            => let val (CE',e') = expId exp CE
               in (CE', LRvb {var=var, fixity=fixity, 
                              exp=DollarExp (fn2let e'), 
                              resultty=resultty}) end
          | LRvb {var, fixity, exp, resultty}   (* ZIDO:  PWLE *)
            => let val (CE',e') = expId exp CE
               in (CE',LRvb {var=var, fixity=fixity, exp=e', resultty=resultty}) end
	  | MarkRvb (rvb, region)
            => let val (CE',r') = rvbXMkLazy rvb CE
               in (CE', MarkRvb (r', region)) end
)
(* RECURSIVE FUNCTIONS BINDINGS TRANSLATION 1 *)
and fbX1 f CE= 
(case f of 
         Fb clauselist
         => let val (CE', c') = Smap clauseId clauselist CE
            in (CE', Fb (c')) end
       | LFb clauselist  (* ZIDO:  PWLE *)
         => let val (CE', c') = Smap clauseX1 clauselist CE
            in (CE', Fb (c')) end
       | MarkFb (fb, region)
         => let val (CE',f') = fbX1 fb CE
            in (CE',MarkFb (f', region)) end
)

(* RECURSIVE FUNCTIONS BINDINGS TRANSLATION 2 *)
and fbX2 fl CE = 
(case fl of [] => (CE,[])
          | (f::fs) =>
(case f of 
         Fb clauselist
         => fbX2 fs CE   (* ZIDO:  I wonder if we are losing CE info here. *)
       | LFb clauselist  (* ZIDO:  PWLE *)
         => let val (CE',c') = clauseX2 (List.hd clauselist) CE
                val (CE'',f') = fbX2 fs CE'
            in (CE'',(Fb [c'])::(f')) end
       | MarkFb (fb, region)
         => fbX2 (fb::fs) CE
)
)


(* RECURSIVE FUNCTIONS BINDINGS *)
and fbId f CE = 
(case f of 
         Fb clauselist
         => let val (CE',c') = Smap clauseId clauselist CE
            in (CE', Fb (c')) end
       | LFb clauselist  (* ZIDO:  PWLE *)
         => let val (CE',c') = Smap clauseId clauselist CE
            in (CE', Fb (c')) end
       | MarkFb (fb, region)
         => let val (CE',f') = fbId fb CE
            in (CE', MarkFb (f', region)) end
)

(* CLAUSE TRANSLATION 1 *)
(* ZIDO:  Do we need to change the type of the result here? *)
and clauseX1 c CE = 
(case c of 
            Clause {pats, resultty, exp}
            => let fun modify (VarPat [n]) = VarPat [nVn n]
                   |   modify (MarkPat (p,r)) = MarkPat (modify p,r)
                   |   modify _ = error "modify only defined on VarPat [n]"

                   fun changeFirst (pi::pil) =
                             (mapfixitem (fn (VarPat [n]) => (VarPat [nVn n]))
                               pi) :: pil

                   |   changeFirst z = z
                   
                   val (CE',p') = Smap (onFixitem patId) pats CE
                   val (CE'',e') = expId exp CE'
               in
               (CE'',
               Clause {pats=changeFirst (p'), 
                       resultty=resultty, exp=ForceExp(e')}
               )
               end
)

(* CLAUSE TRANSLATION 2 *)

and zip (x::xs) (y::ys) = ((x,y)::(zip xs ys))
  | zip x y = []

and clauseX2 c CE = 
(case c of 
            Clause {pats, resultty, exp}
            => let val names = Nnames ((length pats)-1)
                   fun reMap (pi::pil) = 
                             pi ::
                             (map 
                             (fn (pi,n) =>
                              mapfixitem (fn (p) => (VarPat n))
                              pi)
                              (zip pil names))
                   |   reMap z = z
                   val (CE',p') = Smap (onFixitem patId) pats CE
                   val newpats = reMap (p')
                   fun reMap' (pi::pil) = 
                             (mapfixitem (fn (VarPat [n]) => (VarPat [nVn n])
                                           | z => z (* ZIDO: ERROR *) )
                               pi) :: pil
                   |   reMap' z = z

                   fun unVarPat (VarPat n) = n
                   |   unVarPat (MarkPat (p,r)) = unVarPat p
                   |   unVarPat _ = error "unVarPat only defined on VarPat"

                   val newExp = map (VarExp o unVarPat o unFix) 
                                 (reMap' newpats)
               in
               (CE',
               Clause {pats=newpats, 
                       resultty=resultty, 
                       exp=DollarExp(ListApply(List.hd newExp,
                                               List.tl newExp))}
               )
               end
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

(* DATATYPE TRANSLATION 1 *)
and dbX1 d CE = 
(case d of
         Db {tyc, tyvars, rhs}
         => let val (CE',r') = dbrhs rhs CE
            in (CE', Db {tyc=tyc, tyvars=tyvars, rhs=r'}) end
       | LDb {tyc, tyvars, rhs}   (* ZIDO:  PWLE *)
         => let val (CE',r') = dbrhsX rhs CE
            in (CE',Db {tyc=nTCn tyc, tyvars=tyvars, rhs=r'}) end
       | MarkDb (db, region)
         => let val (CE',d') = dbX1 db CE
            in (CE',MarkDb (d', region)) end
)

(* DATATYPE TRANSLATION 2 *)

(* ZIDO:  I think that no useful work is being done on CE here  *)
(*        (dbX2 will probably turn out to be polymorphic in CE) *)

and dbX2 dl CE = 
(case dl of [] => (CE,[])
          | (d::ds) => 
(case d of
         Db {tyc, tyvars, rhs}
         => dbX2 ds CE            (* ZIDO:  Again.  Are losing CE info? *)
       | LDb {tyc, tyvars, rhs}   (* ZIDO:  PWLE *)
         => let val (CE',d') = dbX2 ds CE
            in
            (CE',
            (
             Tb {tyc=tyc,tyvars=tyvars, 
                 def=ConTy ([suspSym],[ConTy ([nTCn tyc],map VarTy tyvars)])}
            )
            :: (d')
            )
            end
       | MarkDb (db, region)
         => dbX2 (db::ds) CE  (* ZIDO:  Is there a way to preserve region info ??? *)
)
)

(* DATATYPE BINDING *)  (* ZIDO:  This code can be garbage collected. *)
and dbId d CE= 
(case d of
         Db {tyc, tyvars, rhs}
         => let val (CE',r') = dbrhs rhs CE
            in (CE', Db {tyc=tyc, tyvars=tyvars, rhs=r'}) end
       | LDb {tyc, tyvars, rhs}   (* ZIDO:  PWLE *)
         => let val (CE',r') = dbrhs rhs CE
            in (CE', Db {tyc=tyc, tyvars=tyvars, rhs=r'}) end
       | MarkDb (db, region)
         => let val (CE',d') = dbId db CE
            in (CE',MarkDb (d', region)) end
)

(* DATATYPE BINDING RIGHT HAND SIDE TRANSLATION *)
and dbrhsX d CE = 
(case d of 
            Constrs symboltyoptionlist
            => (lextCE CE (map (fn (a,b)=> ([a],b)) symboltyoptionlist),
                Constrs (map (fn (a,b) => (nVn a,b)) symboltyoptionlist))
	  | Repl symbollist
            => (CE,
                Repl (symbollist))

(* ZIDO:  ???  What is the last case?  Does it really need an "nVn"??? *)

)

(* DATATYPE BINDING RIGHT HAND SIDE *)
and dbrhs d CE = 
(case d of 
            Constrs symboltyoptionlist

            (* This case is a bit counterintuitive.  But we need to remove
               new bindings from CE if they "shadow" old ones. *)
            => (lremCE CE (map (fn (a,b)=> ([a],b)) symboltyoptionlist),
                d)
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

(* Printing the resulting term on the screen *)

(*

ZIDO:  Still need to implementing printAst 

(with_pp (defaultConsumer()) (fn ppstrm =>
        (add_string ppstrm "Error: Compiler bug: ";
         add_string ppstrm msg;
         body ppstrm;
         add_newline ppstrm));
       raise Error)

*) 
(* ***************************  *)

(* This is a very temporary hack.  The CE environment needs to 
   be propogated to the top-level environment when lazycomp
   is to become "official". *)

  val CER = ref ([]:CE);

in

  fun lazycomp {ast,compenv,compInfo} =
       let val (CE',ast') = decId ast (!CER)
           val _ = map (pp o first) CE'
           val _ = (CER:=CE')
       in
       {ast=ast', compenv=compenv, compInfo=compInfo}
       end

(*
  val delay:(unit -> 'a) -> 'a susp = 
                   System.Unsafe.cast (SMLofNJ.Susp.delay);

  val force:'a susp -> 'a = 
                   System.Unsafe.cast (SMLofNJ.Susp.force);
*)

  val delayStrRef   = ref "delay"
  val forceStrRef   = ref "force"

end

end



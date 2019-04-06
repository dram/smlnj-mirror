(* Copyright 1989 by AT&T Bell Laboratories *)
signature TRANSLATE = sig
	val transDec : ErrorMsg.inputSource -> Absyn.dec -> Lambda.lexp -> Lambda.lexp
end

structure Translat : TRANSLATE =
struct
open Access Absyn Lambda Basics BasicTyp Nonrec ErrorMsg Unboxed

exception Translat

val unitLexp = RECORD []

fun composeNOT (x) =  
    let val v = mkLvar()
    in FN(v,SWITCH(APP(x, VAR v),
                   [(DATAcon falseDcon, CON(trueDcon,unitLexp)),
		    (DATAcon trueDcon, CON(falseDcon,unitLexp))],NONE))
    end
val elemgtr = (fn ((LABEL{number=x,...},_),(LABEL{number=y,...},_))=> x>y);
val sorted = Sort.sorted elemgtr
val sortrec = Sort.sort elemgtr
		
val [bogusID,matchsym,bindsym] = map Symbol.symbol ["bogus","Match","Bind"]

val printDepth = System.Control.Print.printDepth

fun translatepath [v] = VAR v
  | translatepath (x::p) = SELECT(x,translatepath p)
  | translatepath nil = impossible "translate.translatepath nil"

fun getEqualElem (CONty(_,[CONty(_,[t,_]),_])) = t
  | getEqualElem _ = VARty(ref(IBOUND 0))

fun transDec (inputSource as {linePos=ref(pos0::rest),...}) rootdec =
let val polyequal = translatepath(!CoreInfo.polyequalPath)
    val loc0 = (pos0,pos0)
    val err = ErrorMsg.error inputSource

 fun fill loc =
  let fun f (APPpat(_,p)) = f p
	| f (CONSTRAINTpat (p,_)) = f p
	| f (LAYEREDpat (p,q)) = (f p; f q)
	| f (RECORDpat {pats = ref (_::_),...}) = ()
	| f (RECORDpat {fields,flex=false,pats,...}) =
	       pats := map (fn (_,p) => (f p; p)) fields
	| f (pat as RECORDpat {fields,flex=true,typ,pats}) =
	      (app (fn (_,p) => f p) fields;
	       let exception DontBother
		   fun find (FLEXRECORDty(ref(CLOSED ty))) = find(ty)
		     | find (t as CONty(RECORDtyc labels, _)) = 
				      (typ := t; labels)
		     | find _ = (err loc COMPLAIN "unresolved flexible record";
				 PrtAbsyn.printPat(pat,!printDepth);
				 raise DontBother)
		   fun merge (a as ((id,p)::r), lab::s) =
			 if Symbol.eq(id,lab) then p :: merge(r,s) 
			 else WILDpat :: merge(a,s)
		     | merge (nil, lab::s) = WILDpat :: merge(nil,s)
		     | merge (nil,nil) = nil
		     | merge _ = impossible "merge in translate"
		in pats := (merge(fields, find(!typ)) handle DontBother => [WILDpat])
	       end)
	| f _ = ()
   in f
  end
	  
 
 fun thinStr(e,NONE) = e
   | thinStr(e,SOME(v,locs)) = APP(FN(v,RECORD(map transLoc locs)), e)
 
 and transLoc trans =
  case trans
    of VALtrans(PATH p) => translatepath p
     | VALtrans(INLINE P.eql) => polyequal
     | VALtrans(INLINE P.neq) => composeNOT polyequal
     | VALtrans(INLINE i) => PRIM i
     | THINtrans(PATH p,v,locs) => thinStr(translatepath p, SOME(v,locs))
     | CONtrans(d as DATACON{const=true,...}) => CON(d, unitLexp)
     | CONtrans(d as DATACON{const=false,...}) => 
	 let val v = mkLvar() in FN(v,CON(d, VAR v)) end
     | _ => impossible "transLoc in translate"
 
 fun transStr loc sdec =
   case sdec
    of VARstr(STRvar{access=PATH(path),...}) => translatepath path
     | STRUCTstr{body,locations,...} =>
		 makedec loc (SEQdec body) (RECORD(map transLoc locations))
     | APPstr{oper=FCTvar{access=LVAR(v),...},argexp,argthin,...} =>
	       APP(VAR v, thinStr(transStr loc argexp, argthin))
     | LETstr(d,body) => makedec loc d (transStr loc body)
     | MARKstr(body,a,b) => transStr (a,b) body
     | _ => impossible "Translat.transStr"
 
 and makedec loc dec =
     case dec
      of VALdec vbl =>
     fold (fn (VB{pat=VARpat(VALvar{access=INLINE(_),...}),...},b) => b
	    | (VB{pat=CONSTRAINTpat(VARpat(VALvar{access=INLINE _,...}),_),
		     exp=_,...},b) => b
	    | (VB{pat=VARpat(VALvar{access=LVAR v,...}),exp,...},b) => 
		 APP(FN(v,b), translate loc exp)
	    | (VB{pat,exp,...},b) => 
	       (fill loc pat; 
		APP(MC.bindCompile 
		    ([(pat,b),(WILDpat,RAISE(CON(!CoreInfo.exnBind,unitLexp)))],
		     err loc WARN),
		    translate loc exp)))
	  vbl
      | a as VALRECdec rvbl =>
       (makedec loc (nonrec a)
	handle Isrec =>
	(fn e => FIX(fold
	 (fn (RVB{var=VALvar{access=LVAR(var),...},exp,...}, (vlist,llist,lexp))
	       => (var::vlist,  translate loc exp :: llist,  lexp)
	   | _ => impossible "#73 in translate")
	 rvbl (nil,nil,e))))
     | LOCALdec(localdec,visibledec) =>
	   makedec loc(SEQdec[localdec,visibledec])
 
     | EXCEPTIONdec ebl =>
       fold(fn (EBgen{exn=DATACON{rep=VARIABLE(LVAR v),name,const,...},...},lexp)=>
		 APP(FN(v,lexp),
		     if const
		     then RECORD[unitLexp,CON(refDcon,STRING (Symbol.name name))]
		     else CON(refDcon,STRING (Symbol.name name)))
	     | (EBdef{exn=DATACON{rep=VARIABLE(LVAR v),...},
		      edef=DATACON{rep=VARIABLE(PATH p),...}},
		lexp) => APP(FN(v,lexp),translatepath p)
	     | _ => impossible "in makedec EXCEPTIONdec")
	   ebl
 
     | SEQdec decl =>
      (* fold (fn (dec,exp) => makedec loc dec exp) decl *)
       let fun f(a::r) = (makedec loc a) o (f r) | f nil = (fn e=>e) in f decl end
 
     | DATATYPEdec _ => (fn e => e)
     | ABSTYPEdec{body,...} => makedec loc body
     | TYPEdec _ => (fn e => e)
     | STRdec sbl =>
       fold(fn (STRB{strvar=STRvar{access=LVAR(v),...},def,thin,...},lexp) =>
	       APP(FN(v,lexp),thinStr(transStr loc def, thin))
	     | _ => impossible "makedec(STRdec) in translate")
	   sbl
     | ABSdec sbl => makedec loc(STRdec sbl)
     | FCTdec fbl =>
       fold(fn (FCTB{fctvar=FCTvar{access=LVAR(v),binding,...},def,thin,
		     param=STRvar{access=LVAR p,...},...},
		lexp) =>
	       APP(FN(v,lexp),FN(p,thinStr(transStr loc def, thin)))
	     | _ => impossible "makedec(FCTdec) in translate")
	   fbl
     | SIGdec _ => (fn e => e)
     | OPENdec _ => (fn e => e)
     | MARKdec(dec,a,b) => makedec (a,b) dec 
     | IMPORTdec _ => impossible "makedec(IMPORTdec) in translate"

 and transrules loc rules = map (fn (RULE(p,e)) => ((fill loc p; p), translate loc e)) rules
 
 and translate loc exp =
   case exp 
    of  INTexp i => INT i
      | REALexp r => REAL r
      | STRINGexp s => STRING s
      | RECORDexp l =>
	 if sorted l
	 then RECORD(map (fn(_,e)=>translate loc e) l)
	 else let val vars = map (fn (l,e) => (l,(e,mkLvar()))) l
		  fun bind ((_,(e,v)), x) = APP(FN(v, x), translate loc e)
	      in fold bind vars (RECORD(map (fn(_,(_,v))=>VAR v) (sortrec vars)))
	      end
     | SEQexp [e] => translate loc e
     | SEQexp (e::r) => APP(FN(mkLvar(), translate loc (SEQexp r)), translate loc e)
     | APPexp(CONexp dcon, e) => CON (dcon, translate loc e)
     | MARKexp(e,a,b) => translate (a,b) e
     | CONexp(dcon as DATACON{const=false,...}) =>
	 let val v = mkLvar () in FN(v,CON (dcon, VAR v)) end
     | CONexp (dcon as DATACON{const=true,...}) => CON(dcon, unitLexp)
     | VARexp (ref(VALvar{access=PATH(path),...})) => translatepath path
     | VARexp (ref(VALvar{access=INLINE P.eql,typ,...})) => 
	       Equal.equal(getEqualElem(!typ))
     | VARexp (ref(VALvar{access=INLINE P.neq,typ=typ,...})) => 
	       composeNOT(Equal.equal(getEqualElem(!typ)))
     | VARexp (ref(VALvar{access=INLINE P.:=,typ,...})) => 
	       PRIM(unboxedAssign(!typ))
     | VARexp (ref(VALvar{access=INLINE P.update,typ,...})) => 
	       PRIM(unboxedUpdate(!typ))
     | VARexp (ref(VALvar{access=INLINE(n),...})) => PRIM n
     | VARexp (ref(OVLDvar{name,...})) =>
	      impossible("unresolved overloading: "^Symbol.name name)
     | APPexp (f,a) => APP(translate loc f, translate loc a)
     | CONSTRAINTexp (e,t) => translate loc e
     | HANDLEexp (e,HANDLER(FNexp l)) =>
	  HANDLE (translate loc e, 
	    MC.matchCompile(transrules loc l @ [(WILDpat,unitLexp)], err loc WARN))
     | RAISEexp e => RAISE (translate loc e)
     | FNexp l => MC.matchCompile(transrules loc l, err loc WARN)
     | CASEexp (e,l) => APP(MC.matchCompile(transrules loc l, err loc WARN),
			    translate loc e)
     | LETexp (d,e) => makedec loc d (translate loc e)
     | x => (PrtAbsyn.printExp(x,0,!printDepth); impossible "in translate")
  

in makedec loc0 rootdec
end

end (* structure Translat *)

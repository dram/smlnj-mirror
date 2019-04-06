(* translate.sml *)

signature TRANSLATE = sig
	structure Access : ACCESS
	structure Absyn : BAREABSYN
	structure Lambda : LAMBDA
	val makedec : Absyn.dec -> Lambda.lexp -> Lambda.lexp
	val translate : Absyn.exp -> Lambda.lexp
	val transStrb : Absyn.strb -> Lambda.lexp
	val transFctb : Absyn.fctb -> Lambda.lexp
end

structure Translate : TRANSLATE =
struct

structure Access = Access
structure Absyn = Absyn
structure Lambda = Lambda
open Absyn Lambda
open Access Basics BasicTypes Nonrec ErrorMsg Unboxed

val unitLexp = RECORD []

fun getEqualElem (CONty(_,[CONty(_,[t,_]),_])) = t
  | getEqualElem _ = impossible "getEqualElem in translate"

fun composeNOT (x) =  
	let val v = mkLvar()
	 in FN(v,SWITCH(APP(x, VAR v),
		[(DATAcon falseDcon, CON(trueDcon,unitLexp)),
		 (DATAcon trueDcon, CON(falseDcon,unitLexp))],NONE))
	end

val elemgtr = (fn ((LABEL{number=x,...},_),(LABEL{number=y,...},_))=> x>y);
val sorted = Sort.sorted elemgtr
val sortrec = Sort.sort elemgtr
		
val bogusID = Symbols.stringToSymbol "bogus"
val matchsym = Symbols.stringToSymbol "Match"
val bindsym = Symbols.stringToSymbol "Bind"

fun raisematch() = 
	(WILDpat,RAISE(CON(EnvAccess.lookEXNinBase matchsym,unitLexp)))
	handle (*x Table.notfound*) _ => (WILDpat,unitLexp)

fun raisebind() = 
	(WILDpat,RAISE(CON(EnvAccess.lookEXNinBase bindsym,unitLexp)))
	handle(*x Table.notfound*) _ => (WILDpat,unitLexp)

fun reraise() = let val v = mkLvar()
		 in (VARpat(VALvar{name=bogusID,access=LVAR v,
				   vtype=ref UNDEFty}),
		     RAISE(VAR v))
		end

val printDepth = System.Control.Print.printDepth

fun translatepath [v] = VAR v
  | translatepath (x::p) = SELECT(x,translatepath p)
  | translatepath nil = impossible "translate.translatepath nil"

fun fill (APPpat(_,p)) = fill p
  | fill (CONSTRAINTpat (p,_)) = fill p
  | fill (LAYEREDpat (p,q)) = (fill p; fill q)
  | fill (RECORDpat {pats = ref (_::_),...}) = ()
  | fill (RECORDpat {fields,flex=false,pats,...}) =
	 pats := map (fn (_,p) => (fill p; p)) fields
  | fill (pat as RECORDpat {fields,flex=true,typ,pats}) =
	(app (fn (_,p) => fill p) fields;
	 let fun find (FLEXRECORDty(ref(CLOSED ty))) = find(ty)
	       | find (t as CONty(ref(TYCON{kind=RECORDtyc labels,...}),_)) = 
				(typ := t; labels)
	       | find _ = (PrintAbsyn.printPat(pat,!printDepth);
			   condemn "unresolved flexible record")
	     fun merge (a as ((id,p)::r), lab::s) =
		     if Symbol.eq(id,lab) then p :: merge(r,s)
					  else WILDpat :: merge(a,s)
               | merge (nil, lab::s) = WILDpat :: merge(nil,s)
	       | merge (nil,nil) = nil
	       | merge _ = impossible "merge in translate"
          in pats := (merge(fields, find(!typ)) handle Syntax => [WILDpat])

         end)
  | fill _ = ()

fun thinStr(e,NONE) = e
  | thinStr(e,SOME(v,locs)) = APP(FN(v,RECORD(map transLoc locs)), e)

and transLoc trans =
    case trans
      of VALtrans(PATH p) => translatepath p
       | VALtrans(INLINE i) => SELECT(i,VAR(0))
       | THINtrans(PATH p,v,locs) => thinStr(translatepath p, SOME(v,locs))
       | CONtrans(d as DATACON{const=true,...}) =>
	   CON(d, unitLexp)
       | CONtrans(d as DATACON{const=false,...}) => 
	   let val v = mkLvar() in FN(v,CON(d, VAR v)) end
       | _ => impossible "transLoc in translate"

fun transStr (VARstr(STRvar{access=PATH(path),...})) = translatepath path
  | transStr (STRUCTstr{body,locations}) = 
		makedec (SEQdec body) (RECORD(map transLoc locations))
  | transStr (APPstr{oper=FCTvar{access=LVAR(v),...},argexp,argthin}) =
	      APP(VAR v, thinStr(transStr argexp, argthin))
  | transStr _ = impossible "48 in translate.transStr"
    

and makedec (VALdec vbl) =
      fold (fn (VB{pat=VARpat(VALvar{access=INLINE(_),...}),...},b) => b
	     | (VB{pat=CONSTRAINTpat(VARpat(VALvar{access=INLINE _,...}),_),
		    exp=_,...},b) => b
	     | (VB{pat,exp,...},b) => 
	       (fill pat; APP(MC.bindCompile [(pat,b),raisebind()],
			      translate exp)))
	   vbl
  | makedec (a as VALRECdec rvbl) =
      (makedec (nonrec a)
       handle Isrec =>
       (fn e => FIX(fold(fn (RVB{var=VALvar{access=LVAR(var),...},exp,...},
			     (vlist,llist,lexp))
			    => (var::vlist,translate exp :: llist,lexp)
			    | _ => impossible "#73 in translate")
			 rvbl (nil,nil,e))))
  | makedec (LOCALdec (localdec,visibledec)) =
	 makedec (SEQdec[localdec,visibledec])

  | makedec (EXCEPTIONdec ebl) =
      fold(fn (EBgen{exn=DATACON{rep=VARIABLE(LVAR v),name,const,...},...},lexp)=>
	    	APP(FN(v,lexp),
		    if const
		    then RECORD[unitLexp,CON(refDcon,STRING (Symbol.name name))]
		    else CON(refDcon,STRING (Symbol.name name)))
	    | (EBdef{exn=DATACON{rep=VARIABLE(LVAR v),...},
	             edef=DATACON{rep=VARIABLE(PATH p),...}},
	       lexp) =>
	    	APP(FN(v,lexp),translatepath p)
	    | _ => impossible "in makedec EXCEPTIONdec")
	  ebl

  | makedec (SEQdec decl) =
      let fun f(a::r) = (makedec a) o (f r) | f nil = (fn e=>e) in f decl end

  | makedec (DATATYPEdec _) = (fn e => e)
  | makedec (ABSTYPEdec{body,...}) = makedec body
  | makedec (TYPEdec _) = (fn e => e)
  | makedec (STRdec sbl) =
      fold(fn (STRB{strvar=STRvar{access=LVAR(v),...},def,thin,...},lexp) =>
	      APP(FN(v,lexp),thinStr(transStr def, thin))
	    | _ => impossible "makedec(FCTdec) in translate")
	  sbl
  | makedec (ABSdec sbl) =
      fold(fn (STRB{strvar=STRvar{access=LVAR(v),...},def,thin,...},lexp) =>
	      APP(FN(v,lexp),thinStr(transStr def, thin))
	    | _ => impossible "makedec(FCTdec) in translate")
	  sbl
  | makedec (FCTdec fbl) =
      fold(fn (FCTB{fctvar=FCTvar{access=LVAR(v),...},def,thin,
	            param=STRvar{access=LVAR p,...},...},
	       lexp) =>
              APP(FN(v,lexp),FN(p,thinStr(transStr def, thin)))
	    | _ => impossible "makedec(FCTdec) in translate")
	  fbl
  | makedec (SIGdec _) = (fn e => e)
  | makedec (OPENdec _) = (fn e => e)

and transStrb (STRB{def,thin,...}) = thinStr(transStr def, thin)

and transFctb (FCTB{param=STRvar{access=LVAR(p),...},def,thin,...}) =
      FN(p,thinStr(transStr def, thin))
  | transFctb _ = impossible "transFctb in translate"

and translate (INTexp i) = INT i
  | translate (REALexp r) = REAL r
  | translate (STRINGexp s) = STRING s
  | translate (RECORDexp l) =
	if sorted l then RECORD(map (fn(_,e)=>translate e) l)
	   else let val vars = map (fn (l,e) => (l,(e,mkLvar()))) l
		    fun bind ((_,(e,v)), x) = APP(FN(v, x), translate e)
		 in fold bind vars 
			    (RECORD(map (fn(_,(_,v))=>VAR v) (sortrec vars)))
		end
  | translate (SEQexp l) =
	let fun seq [e] = translate e
	      | seq (e::r) = APP(FN(mkLvar(), seq r), translate e)
	      | seq _ = impossible "empty SEQexp in translate"
	 in seq l
	end
  | translate (APPexp(CONexp dcon, e)) = CON (dcon, translate e)
  | translate (CONexp(dcon as DATACON{const=false,...})) =
	let val v = mkLvar ()
	 in FN(v,CON (dcon, VAR v))
	end
  | translate (CONexp (dcon as DATACON{const=true,...})) = CON(dcon, unitLexp)
  | translate (VARexp (ref(VALvar{access=PATH(path),...}))) = translatepath path
  | translate (VARexp (ref(VALvar{access=INLINE(n),vtype=ref ty,...}))) = 
        if n = Prim.equalSlot then Equal.equal(getEqualElem ty)
        else if n=Prim.notequalSlot 
		then composeNOT(Equal.equal(getEqualElem ty))
	else if n = Prim.assignSlot then translatepath [unboxedAssign ty, 0]
        else if n = Prim.updateSlot then translatepath [unboxedUpdate ty, 0]
	else translatepath [n,0]
  | translate (VARexp (ref(OVLDvar{name,...}))) =
	     (complain("unresolved overloading: " ^ Symbol.name name);
	      unitLexp)
  | translate (APPexp (f,a)) = APP(translate f, translate a)
  | translate (CONSTRAINTexp (e,t)) = translate e
  | translate (HANDLEexp (e,HANDLER(FNexp l))) =
	let val rules = map (fn (RULE(p,e)) => ((fill p; p), translate e)) l
	    fun anywild (WILDpat,_) = true
	      | anywild (VARpat _,_) = true
	      | anywild _ = false
	    val rules = if exists anywild rules then rules@[reraise()]
			else rules@[reraise(),reraise()]
	in
	    HANDLE (translate e,MC.matchCompile rules)
	end
  | translate (RAISEexp e) = RAISE (translate e)
  | translate (FNexp l) =
	MC.matchCompile ((map (fn (RULE(p,e)) =>
				((fill p; p), translate e)) l)@[raisematch()])
  | translate (CASEexp (e,l)) = APP(translate (FNexp l),translate e)
  | translate (LETexp (d,e)) = makedec d (translate e)
  | translate x =
      (PrintAbsyn.printExp(x,0,!printDepth);
       impossible "in translate.translate")

end (* structure Translate *)

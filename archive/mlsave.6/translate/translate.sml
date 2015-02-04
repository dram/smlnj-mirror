(* translate.sml *)

signature TRANSLATE = sig
	structure Access : ACCESS
	structure Absyn : BAREABSYN
	structure Lambda : LAMBDA
	val makedec : Absyn.dec -> Lambda.lexp -> Lambda.lexp
	val translate : Absyn.exp -> Lambda.lexp
	val topdec : Absyn.dec -> Lambda.lexp
 	val getvars : Absyn.dec -> Access.lvar list
end

structure translate : TRANSLATE =
struct

structure Access = Access
structure Absyn = Absyn
structure Lambda = Lambda
open Absyn Lambda
open Access Basics BasicTypes Nonrec ErrorMsg Unboxed

val unitLexp = RECORD []

fun getEqualElem (ref(CONty(_,[CONty(_,[t,_]),_]))) = t
  | getEqualElem _ = impossible "getEqualElem in translate"

fun composeNOT (x) =  
	let val v = mkLvar()
	 in FN(v,SWITCH(APP(x, VAR v),
		[(DATAcon falseDcon, CON(trueDcon,unitLexp)),
		 (DATAcon trueDcon ,CON(falseDcon,unitLexp))],NONE))
	end

val elemgtr = (fn ((LABEL{number=x,...},_),(LABEL{number=y,...},_))=> x>y);
val sorted = Sort.sorted elemgtr
val sortrec = Sort.sort elemgtr
		
val bogusID = SymbolTable.stringToSymbol "bogus"
val matchsym = SymbolTable.stringToSymbol "Match"
val bindsym = SymbolTable.stringToSymbol "Bind"

fun raisematch() = 
	(WILDpat,RAISE(CON(EnvAccess.lookEXNinBase matchsym,unitLexp)))
	handle (*x Table.notfound*) _ => (WILDpat,unitLexp)

fun raisebind() = 
	(WILDpat,RAISE(CON(EnvAccess.lookEXNinBase bindsym,unitLexp)))
	handle(*x Table.notfound*) _ => (WILDpat,unitLexp)

fun reraise() = let val v = mkLvar()
		 in (VARpat(VALvar{name=bogusID,access=LVAR v,
				   vtype=ref UNKNOWNty}),
		     RAISE(VAR v))
		end

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
	 let fun find (FLEXRECORDty{completion,...}) = find(!completion)
	       | find (t as CONty(ref(RECORDtyc{labels,...}),_)) = 
				(typ := t; labels)
	       | find _ = (PrintAbsyn.printPat pat;
			   condemn "unresolved flexible record")
	     fun merge (a as ((id,p)::r), lab::s) =
		     if Symbol.eq(id,lab) then p :: merge(r,s)
					  else WILDpat :: merge(a,s)
               | merge (nil, lab::s) = WILDpat :: merge(nil,s)
	       | merge (nil,nil) = nil
	       | merge _ = impossible "merge in translate"
          in pats := (merge(fields, find(!typ)) handlex Syntax => [WILDpat])

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

fun transStr (strexp: strexp, thin : thinning) =
    case strexp
     of VARstr(STRvar{access=PATH(path),...}) =>
	  thinStr(translatepath path,thin)
      | STRUCTstr{body,locations} =>  (* thin should be NONE *)
	  makedec (SEQdec body) (RECORD(map transLoc locations))
      | APPstr{oper=FCTvar{access=LVAR(v),...},argexp,argthin} =>
	  thinStr(APP(VAR v, transStr(argexp,argthin)),thin)
      | _ => impossible "48 in translate.transStr"
    

and makedec (VALdec vbl) =
      fold (fn (VB{pat=VARpat(VALvar{access=INLINE(_),...}),...},b) => b
	     | (VB{pat=CONSTRAINTpat(VARpat(VALvar{access=INLINE _,...}),_),
		    exp=_,...},b) => b
	     | (VB{pat,exp,...},b) => 
	       (fill pat; APP(MC.mc [(pat,b),raisebind()] MC.BIND,
			      translate exp)))
	   vbl
  | makedec (a as VALRECdec rvbl) =
      makedec (nonrec a)
      handlex isrec =>
       (fn e => FIX(fold(fn (RVB{var=VALvar{access=LVAR(var),...},exp,...},
			     (vlist,llist,lexp))
			    => (var::vlist,translate exp :: llist,lexp)
			    | _ => impossible "#73 in translate")
			 rvbl (nil,nil,e)))
  | makedec (LOCALdec (localdec,visibledec)) =
	 makedec (SEQdec[localdec,visibledec])

  | makedec (EXCEPTIONdec ebl) =
      fold(fn (EB{exn=DATACON{rep=ref(VARIABLE(LVAR v)),name,const,vtype,...},
		  def=NONE,...},lexp)=>
	    	APP(FN(v,lexp),
		    if const
		    then RECORD[unitLexp,CON(refDcon,STRING (Symbol.name name))]
		    else CON(refDcon,STRING (Symbol.name name)))
	    | (EB{def=SOME _,...},lexp) => lexp
	    | _ => impossible "in makedec EXCEPTIONdec")
	  ebl

  | makedec (SEQdec decl) =
      let fun f(a::r) = (makedec a) o (f r) | f nil = (fn e=>e) in f decl end

  | makedec (DATATYPEdec tl) = (fn e => e)
  | makedec (TYPEdec _) = (fn e => e)
  | makedec (STRdec(STRB{strvar=STRvar{access=LVAR(v),...},def,thin,...})) =
      (fn lexp => APP(FN(v,lexp),transStr(def,thin)))
  | makedec (FCTdec(FCTB{fctvar=FCTvar{access=LVAR(v),...},def,thin,
	     param=STRvar{access=LVAR p,...},...})) =
      (fn lexp => APP(FN(v,lexp),FN(p,transStr(def,thin))))

  | makedec (OPENdec _) = (fn e => e)
  | makedec _ = impossible "in translate.makedec"

and topdec (STRdec(STRB{strvar=STRvar{access=LVAR(v),...},def,thin,...})) =
      transStr(def,thin)
  | topdec (FCTdec(FCTB{fctvar=FCTvar{access=LVAR(v),...},
	   		param=STRvar{access=LVAR(p),...},def,thin,...})) =
      FN(p,transStr(def,thin))
  | topdec _ = impossible "topdec in translate"

(*
val reals = ref (nil: (lvar * string) list);
val RC = SymbolTable.stringToSymbol "realconst"
and topdec d = let val t = (reals := nil; topdec1 d)
		   fun bind((v,s), e) = 
		     let val VALvar{access=PATH p,...} = EnvAccess.lookVARinBase RC
		      in APP(FN(v,e),APP(translatepath p, STRING s))
		     end
		in fold bind (!reals) t
	       end
*)

(* and translate x = (transl1 x handlex ? => (PrintAbsyn.printExp x; 
let val 9=8 in transl1 x end)) *)

and translate (INTexp i) = INT i
  | translate (REALexp r) = REAL r
  | translate (STRINGexp s) = STRING s
(*			      let val v = mkLvar()
			       in reals := (v,s)::(!reals);	 
				  VAR v
			      end  *)
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
  | translate (VARexp (ref(VALvar{access=INLINE(n),vtype,...}))) = 
        if n = Prim.equalSlot then Equal.equal(getEqualElem vtype)
        else if n=Prim.notequalSlot 
		then composeNOT(Equal.equal(getEqualElem vtype))
	else if n = Prim.assignSlot then translatepath [unboxedAssign vtype, 0]
        else if n = Prim.updateSlot then translatepath [unboxedUpdate vtype, 0]
	else translatepath [n,0]
  | translate (VARexp (ref(OVLDvar{name,...}))) =
	     (complain("unresolved overloading: " ^ Symbol.name name);
	      unitLexp)
  | translate (APPexp (f,a)) = APP(translate f, translate a)
  | translate (CONSTRAINTexp (e,t)) = translate e
  | translate (HANDLEexp (e,HANDLER(FNexp l))) =
	let val rules = map (fn (RULE(p,e)) => ((fill p; p), translate e)) l
	    fun anywild (WILDpat,_) = true
	      | anywild _ = false
	    val rules = if exists(anywild,rules) then rules@[reraise()]
			else rules@[reraise(),reraise()]
	in
	    HANDLE (translate e,MC.mc rules MC.MATCH)
	end
  | translate (RAISEexp e) = RAISE (translate e)
  | translate (FNexp l) =
	MC.mc ((map (fn (RULE(p,e)) =>
		     ((fill p; p), translate e)) l)@[raisematch()]) MC.MATCH
  | translate (CASEexp (e,l)) = APP(translate (FNexp l),translate e)
  | translate (LETexp (d,e)) = makedec d (translate e)
  | translate x = (PrintAbsyn.printExp x; impossible "in translate.translate")

fun smash f nil = nil
  | smash f (a::r) = f a @ smash f r

fun patvars (VARpat(VALvar{access=LVAR v,...})) = [v]
  | patvars (VARpat(VALvar{access=INLINE _,...})) = nil
  | patvars (VARpat _ ) = impossible "non-LVAR in translate.patvars"
  | patvars (RECORDpat{pats=ref plist,...}) = smash patvars plist
  | patvars (APPpat(_,p)) = patvars p
  | patvars (CONSTRAINTpat(p,_)) = patvars p
  | patvars (LAYEREDpat(p,q)) = patvars p @ patvars q
  | patvars _ = impossible "patvars in translate"

fun getvars (VALdec vbl) =
      smash (fn VB{pat,...} => patvars pat) vbl

  | getvars (a as VALRECdec rvbl) =
      smash (fn RVB{var=VALvar{access=LVAR(var),...},exp,...} => [var]
	     | _ => impossible "#738 in translate")
	 rvbl

  | getvars (LOCALdec (localdec,visibledec)) = getvars visibledec

  | getvars (EXCEPTIONdec ebl) =
      smash (fn EB{exn=DATACON{rep=ref(VARIABLE(LVAR v)),...},def=NONE,...} => [v]
	    | EB{def=SOME _,...} => []
	    | _ => impossible "in getvars EXCEPTIONdec")
	  ebl

  | getvars (SEQdec decl) = smash getvars decl
  | getvars (DATATYPEdec tl) = []
  | getvars (TYPEdec _) = []
  | getvars (STRdec(STRB{strvar=STRvar{access=LVAR(v),...},...})) = [v]
  | getvars (FCTdec(FCTB{fctvar=FCTvar{access=LVAR(v),...},...})) = [v]
  | getvars (OPENdec _) = []
  | getvars _ = impossible "in translate.getvars"

end

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

val UNIT = RECORD []

fun getEqualElem (ref(CONty(_,[CONty(_,[t,_]),_]))) = t

fun composeNOT (x) =  
	let val v = mkLvar()
	 in FN(v,SWITCH(APP(x, VAR v),
		[(DATAcon FALSEdcon, CON(TRUEdcon,UNIT)),
		 (DATAcon TRUEdcon ,CON(FALSEdcon,UNIT))],NONE))
	end

val elemgtr = (fn ((LABEL{number=x,...},_),(LABEL{number=y,...},_))=> x>y);
val sorted = Sort.sorted elemgtr
val sortrec = Sort.sort elemgtr
		
val bogusID = SymbolTable.StringToSymbol "bogus"
val matchsym = SymbolTable.StringToSymbol "e_match"
val bindsym = SymbolTable.StringToSymbol "e_bind"

fun raisematch() = 
	(WILDpat,RAISE(CON(EnvAccess.lookEXNinBase matchsym,UNIT)))
	handle (*x Table.notfound*) _ => (WILDpat,UNIT)

fun raisebind() = 
	(WILDpat,RAISE(CON(EnvAccess.lookEXNinBase bindsym,UNIT)))
	handle(*x Table.notfound*) _ => (WILDpat,UNIT)

fun reraise() = let val v = mkLvar()
		 in (VARpat(VALvar{name=bogusID,access=LVAR v,
				   vtype=ref UNKNOWNty}),
		     RAISE(VAR v))
		end

fun translatepath [v] = VAR v
  | translatepath (x::p) = SELECT(x,translatepath p)
  | translatepath nil = Impossible "translate.translatepath nil"

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
			   Condemn "unresolved flexible record")
	     fun merge (a as ((id,p)::r), lab::s) =
		     if Symbol.Eq(id,lab) then p :: merge(r,s)
					  else WILDpat :: merge(a,s)
               | merge (nil, lab::s) = WILDpat :: merge(nil,s)
	       | merge (nil,nil) = nil
	       | merge _ = Impossible "merge in translate"
          in pats := (merge(fields, find(!typ)) handlex Syntax => [WILDpat])

         end)
  | fill _ = ()

fun thinStr(e,NOTHIN) = e
  | thinStr(e,THIN l) = 
      let val v = mkLvar()
       in APP(FN(v,RECORD(map (thinElem(VAR v)) l)), e)
      end

and thinElem e trans =
    case trans
      of VALtrans i => SELECT(i,e)
       | INLtrans i => SELECT(i,VAR(0))
       | STRtrans(i,thin) => thinStr(SELECT(i,e),thin)
       | CONtrans(d as DATACON{const=true,...}) =>
	   CON(d, UNIT)
       | CONtrans(d as DATACON{const=false,...}) => 
	   let val v = mkLvar() in FN(v,CON(d, VAR v)) end

fun transStr(strexp: strexp): lexp =
    case strexp
     of VARstr(STRvar{access=PATH(path),...}) => translatepath path
      | STRUCTstr{body,locations} =>
	  makedec (SEQdec body) 
		  (RECORD(map translatepath locations))
      | APPstr{oper=FCTvar{access=LVAR(v),...},args} =>
	  revfold addFctArg args (VAR v)
      | _ => Impossible "48 in translate.transStr"

and addFctArg((strexp,thin),lexp): lexp =
    APP(lexp,thinStr(transStr(strexp),thin))

and makedec (VALdec vbl) =
      fold (fn (VB{pat=VARpat(VALvar{access=INLINE(_),...}),...},b) => b
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
			    | _ => Impossible "#73 in translate")
			 rvbl (nil,nil,e)))
  | makedec (LOCALdec (localdec,visibledec)) =
	 makedec (SEQdec[localdec,visibledec])

  | makedec (EXCEPTIONdec ebl) =
      fold(fn (EB{exn=DATACON{rep=ref(VARIABLE(LVAR v)),name,const,vtype,...},
		  def=NONE,...},lexp)=>
	    	APP(FN(v,lexp), CON(REFdcon,STRING (Symbol.Name name)))
	    | (EB{def=SOME _,...},lexp) => lexp
	    | _ => Impossible "in makedec EXCEPTIONdec")
	  ebl

  | makedec (SEQdec decl) =
      let fun f(a::r) = (makedec a) o (f r) | f nil = (fn e=>e) in f decl end

  | makedec (DATATYPEdec tl) = (fn e => e)
  | makedec (TYPEdec _) = (fn e => e)
  | makedec (STRdec(STRB{strvar=STRvar{access=LVAR(v),...},def,thin,...})) =
      (fn lexp => APP(FN(v,lexp),thinStr(transStr(def),thin)))
  | makedec (FCTdec(FCTB{fctvar=FCTvar{access=LVAR(v),...},params,def,...})) =
      (fn lexp => 
	  APP(FN(v,lexp),
	      fold (fn (STRvar{access=LVAR(p),...},e) => FN(p,e)
		        | _ => Impossible "#66 in translate")
		   params
		   (transStr def)))  (* thinning of def? *)
  | makedec (OPENdec _) = (fn e => e)
  | makedec _ = Impossible "in translate.makedec"

and topdec (STRdec(STRB{strvar=STRvar{access=LVAR(v),...},def,thin,...})) =
      thinStr(transStr(def),thin)
  | topdec (FCTdec(FCTB{fctvar=FCTvar{access=LVAR(v),...},params,def,...})) =
	      fold (fn (STRvar{access=LVAR(p),...},e) => FN(p,e)
			| _ => Impossible "#67 in translate")
		   params
		   (transStr def)  (* thinning of def? *)
  | topdec _ = Impossible "topdec in translate"

(*
val reals = ref (nil: (lvar * string) list);
val RC = SymbolTable.StringToSymbol "realconst"
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
	      | seq _ = Impossible "empty SEQexp in translate"
	 in seq l
	end
  | translate (APPexp(CONexp dcon, e)) = CON (dcon, translate e)
  | translate (CONexp(dcon as DATACON{const=false,...})) =
	let val v = mkLvar ()
	 in FN(v,CON (dcon, VAR v))
	end
  | translate (CONexp (dcon as DATACON{const=true,...})) = CON(dcon, UNIT)
  | translate (VARexp (ref(VALvar{access=PATH(path),...}))) = translatepath path
  | translate (VARexp (ref(VALvar{access=INLINE(n),vtype,...}))) = 
        if n = Prim.equalSlot then Equal.equal(getEqualElem vtype)
        else if n=Prim.notequalSlot 
		then composeNOT(Equal.equal(getEqualElem vtype))
	else if n = Prim.assignSlot then translatepath [unboxedAssign vtype, 0]
        else if n = Prim.updateSlot then translatepath [unboxedUpdate vtype, 0]
	else translatepath [n,0]
  | translate (VARexp (ref(OVLDvar{name,...}))) =
	     (Complain("unresolved overloading: " ^ Symbol.Name name);
	      UNIT)
  | translate (APPexp (f,a)) = APP(translate f, translate a)
  | translate (CONSTRAINTexp (e,t)) = translate e
  | translate (HANDLEexp (e,HANDLERX(FNexp l))) =
	HANDLE (translate e,
	    MC.mc ((map (fn (RULE(p,e)) =>
			     ((fill p; p), translate e)) l)@[reraise()]) MC.MATCH)
  | translate (RAISEXexp e) = RAISE (translate e)
  | translate (FNexp l) =
	MC.mc ((map (fn (RULE(p,e)) =>
		     ((fill p; p), translate e)) l)@[raisematch()]) MC.MATCH

  | translate (CASEexp (e,l)) = APP(translate (FNexp l),translate e)
  | translate (LETexp (d,e)) = makedec d (translate e)
  | translate x = (PrintAbsyn.printExp x; Impossible "in translate.translate")

fun smash f nil = nil
  | smash f (a::r) = f a @ smash f r

fun patvars (VARpat(VALvar{access=LVAR v,...})) = [v]
  | patvars (VARpat(VALvar{access=INLINE _,...})) = nil
  | patvars (VARpat _ ) = Impossible "non-LVAR in translate.patvars"
  | patvars (RECORDpat{pats=ref plist,...}) = smash patvars plist
  | patvars (APPpat(_,p)) = patvars p
  | patvars (CONSTRAINTpat(p,_)) = patvars p
  | patvars (LAYEREDpat(p,q)) = patvars p @ patvars q

fun getvars (VALdec vbl) =
      smash (fn VB{pat,...} => patvars (fill pat; pat)) vbl

  | getvars (a as VALRECdec rvbl) =
      smash (fn RVB{var=VALvar{access=LVAR(var),...},exp,...} => [var]
	     | _ => Impossible "#738 in translate")
	 rvbl

  | getvars (LOCALdec (localdec,visibledec)) = getvars visibledec

  | getvars (EXCEPTIONdec ebl) =
      smash (fn EB{exn=DATACON{rep=ref(VARIABLE(LVAR v)),...},def=NONE,...} => [v]
	    | EB{def=SOME _,...} => []
	    | _ => Impossible "in getvars EXCEPTIONdec")
	  ebl

  | getvars (SEQdec decl) = smash getvars decl
  | getvars (DATATYPEdec tl) = []
  | getvars (TYPEdec _) = []
  | getvars (STRdec(STRB{strvar=STRvar{access=LVAR(v),...},...})) = [v]
  | getvars (FCTdec(FCTB{fctvar=FCTvar{access=LVAR(v),...},...})) = [v]
  | getvars (OPENdec _) = []
  | getvars _ = Impossible "in translate.getvars"

end

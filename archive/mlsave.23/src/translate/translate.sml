signature TRANSLATE = sig
	structure Access : ACCESS
	structure Absyn : BAREABSYN
	structure Lambda : LAMBDA
	val transDec : Absyn.dec -> Lambda.lexp -> Lambda.lexp
	val transStrb : Absyn.strb -> Lambda.lexp
	val transFctb : Absyn.fctb -> Lambda.lexp
end

structure Translate : TRANSLATE =
struct

structure Access = Access
structure Absyn = Absyn
structure Lambda = Lambda

open Access Absyn Lambda
open Basics BasicTypes Nonrec ErrorMsg Unboxed

val unitLexp = RECORD []

(* profiling globals *)
val profiling = System.Control.Profile.profiling
val profileList = ref([]: (int * string) list)

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
	handle _ => (WILDpat,unitLexp)

fun raisebind() = 
	(WILDpat,RAISE(CON(EnvAccess.lookEXNinBase bindsym,unitLexp)))
	handle _ => (WILDpat,unitLexp)

fun reraise() =
    let val v = mkLvar()
    in (VARpat(VALvar{name=bogusID,access=LVAR v, vtype=ref UNDEFty}), RAISE(VAR v))
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

fun lookbase name =
 let val sym = Symbols.stringToSymbol name
  in fn () => let val VALvar{access=PATH p,...} =
			EnvAccess.lookVARinBase sym
	       in translatepath p
	      end
 end

val polyequal = lookbase "polyequal"
val current = lookbase "zcurrent"
local val f = lookbase "zother"
  in val other = fn () => if !profiling then f() else RECORD nil
 end

fun thinStr(e,NONE) = e
  | thinStr(e,SOME(v,locs)) = APP(FN(v,RECORD(map transLoc locs)), e)

and transLoc trans =
    case trans
      of VALtrans(PATH p) => translatepath p
       | VALtrans(INLINE P.eql) => polyequal()
       | VALtrans(INLINE P.neq) => composeNOT(polyequal())
       | VALtrans(INLINE i) => PRIM i
       | THINtrans(PATH p,v,locs) => thinStr(translatepath p, SOME(v,locs))
       | CONtrans(d as DATACON{const=true,...}) => CON(d, unitLexp)
       | CONtrans(d as DATACON{const=false,...}) => 
	   let val v = mkLvar() in FN(v,CON(d, VAR v)) end
       | _ => impossible "transLoc in translate"


val anonSym = Symbols.stringToSymbol "anon"

fun profileBind(lexp) =
    fold (fn ((var,string),x) => APP(FN(var,x),STRING string)) (!profileList)
         (RECORD[lexp,RECORD(map (VAR o #1) (!profileList))])

fun transStr (sp,VARstr(STRvar{access=PATH(path),...})) = translatepath path
  | transStr (sp,STRUCTstr{body,locations}) = 
		makedec ((sp,other()),SEQdec body) (RECORD(map transLoc locations))
  | transStr (sp,APPstr{oper=FCTvar{access=LVAR(v),...},argexp,argthin}) =
	      APP(VAR v, thinStr(transStr(sp,argexp), argthin))
  | transStr _ = impossible "Translate.transStr"
    

and makedec (sp as (names,profv),VALdec vbl) =
    fold (fn (VB{pat=VARpat(VALvar{access=INLINE(_),...}),...},b) => b
	   | (VB{pat=CONSTRAINTpat(VARpat(VALvar{access=INLINE _,...}),_),
		    exp=_,...},b) => b
	   | (VB{pat=VARpat(VALvar{access=LVAR v,name,...}),exp,...},b) => 
	        APP(FN(v,b), translate (name::names,profv) false exp)
	   | (VB{pat,exp,...},b) => 
	       (fill pat; APP(MC.bindCompile [(pat,b),raisebind()],
	                      translate sp false exp)))
	 vbl
  | makedec (sp as (names,profv), a as VALRECdec rvbl) =
      (makedec (sp, nonrec a)
       handle Isrec =>
       (fn e => FIX(fold
        (fn (RVB{var=VALvar{access=LVAR(var),name,...},exp,...}, (vlist,llist,lexp))
              => (var::vlist, 
		  translate (name::names,profv) false exp :: llist,
		  lexp)
          | _ => impossible "#73 in translate")
        rvbl (nil,nil,e))))
  | makedec (sp, LOCALdec(localdec,visibledec)) = 
          makedec(sp, SEQdec[localdec,visibledec])

  | makedec (sp, EXCEPTIONdec ebl) =
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

  | makedec (sp, SEQdec decl) =
      let fun f(a::r) = (makedec (sp,a)) o (f r) | f nil = (fn e=>e) in f decl end

  | makedec (sp, DATATYPEdec _) = (fn e => e)
  | makedec (sp, ABSTYPEdec{body,...}) = makedec(sp, body)
  | makedec (sp, TYPEdec _) = (fn e => e)
  | makedec (sp, STRdec sbl) =
      fold(fn (STRB{strvar=STRvar{access=LVAR(v),name,...},def,thin,...},lexp) =>
	      APP(FN(v,lexp),thinStr(transStr(name::(#1 sp), def), thin))
	    | _ => impossible "makedec(STRdec) in translate")
	  sbl
  | makedec (sp, ABSdec sbl) =
      fold(fn (STRB{strvar=STRvar{access=LVAR(v),name,...},def,thin,...},lexp) =>
	      APP(FN(v,lexp),thinStr(transStr(name::(#1 sp), def), thin))
	    | _ => impossible "makedec(ABSdec) in translate")
	  sbl
  | makedec (sp,FCTdec fbl) =
      fold(fn (FCTB{fctvar=FCTvar{access=LVAR(v),name,...},def,thin,
	            param=STRvar{access=LVAR p,...},...},
	       lexp) =>
              APP(FN(v,lexp),FN(p,thinStr(transStr(name::(#1 sp), def), thin)))
	    | _ => impossible "makedec(FCTdec) in translate")
	  fbl
  | makedec (sp, SIGdec _) = (fn e => e)
  | makedec (sp, OPENdec _) = (fn e => e)

and translate sp =
 let fun istail tail =
  let val FNentry = 
	 if !profiling
	  then fn body =>
		let val arg = mkLvar()
	         in FN(arg,APP(FN(mkLvar(),APP(body,VAR arg)),
	                      APP(PRIM(P.unboxedassign),RECORD[current(),#2 sp])))
	        end
	  else fn body => body
      fun itrans exp = istail false exp
      fun otrans exp = istail true exp
      fun transrules tr rules = 
          map (fn (RULE(p,e)) => ((fill p; p), tr e)) rules
      fun ntrans exp = translate (anonSym::(#1 sp), #2 sp) true exp
      
      val rec trans =
    fn INTexp i => INT i
     | REALexp r => REAL r
     | STRINGexp s => STRING s
     | RECORDexp l =>
	if sorted l
	then RECORD(map (fn(_,e)=>itrans e) l)
	else let val vars = map (fn (l,e) => (l,(e,mkLvar()))) l
		 fun bind ((_,(e,v)), x) = APP(FN(v, x), itrans e)
	     in fold bind vars (RECORD(map (fn(_,(_,v))=>VAR v) (sortrec vars)))
	     end
    | SEQexp l =>
	let fun seq [e] = trans e
	      | seq (e::r) = APP(FN(mkLvar(), seq r), itrans e)
	      | seq _ = impossible "empty SEQexp in translate"
	in seq l
	end
    | APPexp(CONexp dcon, e) => CON (dcon, itrans e)
    | CONexp(dcon as DATACON{const=false,...}) =>
	let val v = mkLvar () in FN(v,CON (dcon, VAR v)) end
    | CONexp (dcon as DATACON{const=true,...}) => CON(dcon, unitLexp)
    | VARexp (ref(VALvar{access=PATH(path),...})) => translatepath path
    | VARexp (ref(VALvar{access=INLINE P.eql,vtype=ref ty,...})) => 
	      Equal.equal(getEqualElem ty)
    | VARexp (ref(VALvar{access=INLINE P.neq,vtype=ref ty,...})) => 
	      composeNOT(Equal.equal(getEqualElem ty))
    | VARexp (ref(VALvar{access=INLINE P.:=,vtype=ref ty,...})) => 
	      PRIM(unboxedAssign ty)
    | VARexp (ref(VALvar{access=INLINE P.update,vtype=ref ty,...})) => 
	      PRIM(unboxedUpdate ty)
    | VARexp (ref(VALvar{access=INLINE(n),vtype=ref ty,...})) => PRIM n
    | VARexp (ref(OVLDvar{name,...})) =>
	     (complain("unresolved overloading: " ^ Symbol.name name);
	      unitLexp)
    | APPexp (f,a) => if !profiling andalso not tail
	  then let val x = mkLvar()
	        in APP(FN(x,APP(FN(mkLvar(),VAR x),
                      APP(PRIM(P.unboxedassign),RECORD[current(),#2 sp]))),
		    APP(itrans f, itrans a))
	       end
          else APP(itrans f, itrans a)
    | CONSTRAINTexp (e,t) => trans e
    | HANDLEexp (e,HANDLER(FNexp l)) =>
	let val rules = transrules trans l
	    fun anywild (WILDpat,_) = true
	      | anywild (VARpat _,_) = true
	      | anywild _ = false
	    val rules = if exists anywild rules then rules@[reraise()]
			else rules@[reraise(),reraise()]
	 in HANDLE (itrans e, FNentry (MC.matchCompile rules))
	end
    | RAISEexp e => RAISE (itrans e)
    | FNexp l => 
       if !profiling
	then let val pathname = #1 sp
		 fun dot (a,x::(rest as _::_)) = dot("." :: Symbol.name x :: a, rest)
		   | dot (a,[z]) = Symbol.name z :: a
		   | dot _ = impossible "no path in profileEntry"
		 val name = implode(System.Control.Profile.header::(dot ([],pathname)))
		 val v' = mkLvar()  (* will be bound to name *)
		 val FN(w,body) = FNentry(MC.matchCompile((transrules (translate
			    (anonSym::pathname,VAR v') true) l)@[raisematch()]))
	     in profileList :=  (v',name) :: !profileList;
		FN(w, APP(FN(mkLvar(), body),
			  APP(PRIM(P.unboxedassign),
			      RECORD[VAR(v'),APP(PRIM(P.+),
						 RECORD[APP(PRIM(P.!),VAR(v')),
							INT 1])])))
	     end
	else FNentry(MC.matchCompile((transrules ntrans l)@[raisematch()]))
    | CASEexp (e,l) => APP(MC.matchCompile((transrules trans l)@[raisematch()]),
		           itrans e)
    | LETexp (d,e) => makedec(sp,d) (trans e)
    | x => (PrintAbsyn.printExp(x,0,!printDepth); impossible "in translate")
   in trans
  end
  in istail
 end

fun transDec absyn lexp = (profileList := []; 
			   profileBind(makedec (([],other()),absyn) lexp))

fun transStrb (STRB{def,thin,strvar=STRvar{name,...},...}) = 
    (profileList := []; profileBind(thinStr(transStr([name],def), thin)))

fun transFctb 
  (FCTB{param=STRvar{access=LVAR(p),...},def,thin, fctvar=FCTvar{name,...},...}) =
      (profileList := []; profileBind(FN(p,thinStr(transStr([name],def), thin))))
  | transFctb _ = impossible "transFctb in translate"

end (* structure Translate *)

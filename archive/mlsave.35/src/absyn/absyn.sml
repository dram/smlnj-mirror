structure Absyn = struct

structure Basics = Basics

local
  open Access Basics BasicTypes
  val whileSym = Symbol.symbol "while"
  and argSym = Symbol.symbol "arg"
in
	
open BareAbsyn

val unitPat = RECORDpat{fields = nil, flex = false, typ = ref UNDEFty,
			pats = ref nil}
val unitExp = RECORDexp nil

val truePat = CONpat(trueDcon)
val trueExp = CONexp(trueDcon)
val falsePat = CONpat(falseDcon)
val falseExp = CONexp(falseDcon)

val nilPat = CONpat(nilDcon)
val nilExp = CONexp(nilDcon)
val consPat = fn pat => APPpat(consDcon,pat)
val consExp = CONexp(consDcon)

fun TUPLEexp l = 
   let fun addlabels(i,e::r) = 
	     (LABEL{number=i-1, name=(Tuples.numlabel i)}, e) :: addlabels(i+1, r)
	 | addlabels(_, nil) = nil
    in RECORDexp (addlabels(1,l))
   end

fun TUPLEpat l =
   let fun addlabels(i,e::r) = (Tuples.numlabel i, e) :: addlabels(i+1, r)
	 | addlabels(_, nil) = nil
    in RECORDpat{fields = addlabels(1,l), flex = false, typ = ref UNDEFty,
		 pats = ref nil}
   end

fun LISTexp l = fold (fn (e,rest) => APPexp(consExp,TUPLEexp[e,rest])) l nilExp

fun IFexp (a,b,c) =
    CASEexp(a, [RULE(truePat,b), RULE(falsePat,c)])

fun ORELSEexp(a,b) =
    IFexp(a,trueExp,b)

fun ANDALSOexp(a,b) =
    IFexp(a,b,falseExp)

fun WHILEexp (a,b) =
    let val tyref = ref UNDEFty
	val lvar = namedLvar(whileSym)
	val fb = VALvar{name=whileSym,
		        access=LVAR(lvar),
			vtype=tyref}
	val fa = VALvar{name=whileSym,
		      	access=PATH[lvar],
			vtype=tyref}
        val id = fn x => x
	val (markdec,markall,markend,markbody) =
	    case (a,b)
	     of (MARKexp(_,a1,a2), MARKexp(_,b1,b2)) =>
		(fn e => MARKdec(e,a1,b2), fn e => MARKexp(e,a1,b2),
		 fn e => MARKexp(e,b2,b2), fn e => MARKexp(e,b1,b2))
	      | _ => (id,id,id,id)
      in LETexp(
	 markdec(
	  VALRECdec[
	    RVB{var=fb,
		exp=markall(FNexp[
		      RULE(unitPat,
		           markbody(IFexp(a,
				 SEQexp[b, APPexp(markend(VARexp(ref fa)),
						  markend unitExp)],
			         unitExp)))]),
		resultty = NONE,
		tyvars = nil}]),
	    APPexp(markall(VARexp (ref fa)), markend unitExp))
    end

fun LISTpat l = fold (fn (e,rest) => APPpat(consDcon, TUPLEpat[e,rest])) l nilPat

fun FUNdec fbl =
    let fun fb2rvb (FB {var, clauses as (CLAUSE{pats,...}::_),tyvars}) =
	    let fun getvars (hd::tl) = mkVALvar(argSym,ref UNDEFty) :: getvars tl
		  | getvars nil = nil;
		val vars = getvars pats
		fun not1(f,[a]) = a
		  | not1(f,l) = f l
		fun dovar (VALvar{access=LVAR n,name,vtype}) =
		       VARexp(ref(VALvar{access=PATH[n],name=name,vtype=vtype}))
		  | dovar _ = ErrorMsg.impossible "Absyn.FUNdec.dovar"

		fun doclause (CLAUSE{pats,exp,resultty=NONE}) =
			      RULE(not1(TUPLEpat,pats), exp)
		  | doclause (CLAUSE{pats,exp,resultty=SOME ty}) =
			      RULE(not1(TUPLEpat,pats), CONSTRAINTexp(exp,ty))

	        fun last[x] = x | last (a::r) = last r
		val mark =  case (hd clauses, last clauses)
	                     of (CLAUSE{exp=MARKexp(_,a,_),...},
				 CLAUSE{exp=MARKexp(_,_,b),...}) =>
			         (fn e => MARKexp(e,a,b))
			      | _ => fn e => e
	     in RVB {var=var,
		     exp=fold (fn (w,e) =>FNexp[RULE(VARpat w, mark e)]) vars
			 (CASEexp(not1(TUPLEexp, map dovar vars),
				  map doclause clauses)),
		     resultty=NONE,
		     tyvars=tyvars}
	    end
          | fb2rvb _ = ErrorMsg.impossible "absyn.38"
     in VALRECdec (map fb2rvb fbl)
    end

end (* local open Access Basics ... *)
end (* structure Absyn *)

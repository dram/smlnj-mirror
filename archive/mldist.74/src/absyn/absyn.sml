(* Copyright 1989 by AT&T Bell Laboratories *)
structure Absyn = struct

local
  open Symbol Access Types Variables BasicTypes BareAbsyn
  val whileSym = Symbol.varSymbol "while"
  and argName = [Symbol.varSymbol "arg"]
in
	
open BareAbsyn

fun completeMatch' (RULE(p,e)) [r as RULE(pat,MARKexp(_,left,right))] =
				[r, RULE(p,MARKexp(e,left,left))]
  | completeMatch' r' [r] = [r, r']
  | completeMatch' r' (a::r) = a :: completeMatch' r' r

fun completeMatch x= 
  completeMatch' (RULE(WILDpat,RAISEexp(CONexp(!CoreInfo.exnMatch)))) x

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
    CASEexp(a, completeMatch[RULE(truePat,b), RULE(falsePat,c)])

fun ORELSEexp(a,b) =
    IFexp(a,trueExp,b)

fun ANDALSOexp(a,b) =
    IFexp(a,b,falseExp)

fun WHILEexp (a,b) =
    let val fvar = mkVALvar whileSym
        val id = fn x => x
	val (markdec,markall,markend,markbody) =
	    case (a,b)
	     of (MARKexp(_,a1,a2), MARKexp(_,b1,b2)) =>
		(fn e => MARKdec(e,a1,b2), fn e => MARKexp(e,a1,b2),
		 fn e => MARKexp(e,b2,b2), fn e => MARKexp(e,b1,b2))
	      | _ => (id,id,id,id)
      in markall (LETexp(
	  markdec (VALRECdec[
	    RVB{var=fvar,
		exp=markall(FNexp(completeMatch[
		      RULE(unitPat,
		           markall(IFexp(a,
			      markbody (SEQexp[b, APPexp(markend(VARexp(ref fvar)),
							 markend unitExp)]),
					 markend unitExp)))])),
		resultty = NONE,
		tyvars = nil}]),
          APPexp(markall(VARexp (ref fvar)), markend unitExp)))
    end

fun LISTpat l = fold (fn (e,rest) => APPpat(consDcon, TUPLEpat[e,rest])) l nilPat

fun FUNdec fbl =
    let fun fb2rvb (FB {var, clauses as (CLAUSE{pats,...}::_),tyvars}) =
	    let fun getvar _ =  VALvar{access=PATH[mkLvar()],name=argName,
				       typ=ref UNDEFty}
		val vars = map getvar pats
		fun not1(f,[a]) = a
		  | not1(f,l) = f l
		fun dovar valvar = VARexp(ref(valvar))
		fun doclause (CLAUSE{pats,exp,resultty=NONE}) =
			      RULE(not1(TUPLEpat,pats), exp)
		  | doclause (CLAUSE{pats,exp,resultty=SOME ty}) =
			      RULE(not1(TUPLEpat,pats),CONSTRAINTexp(exp,ty))

	        fun last[x] = x | last (a::r) = last r
		val mark =  case (hd clauses, last clauses)
	                     of (CLAUSE{exp=MARKexp(_,a,_),...},
				 CLAUSE{exp=MARKexp(_,_,b),...}) =>
			         (fn e => MARKexp(e,a,b))
			      | _ => fn e => e
		fun makeexp [var] = FNexp(completeMatch(map doclause clauses))
		  | makeexp vars = fold 
				(fn (w,e) => FNexp(completeMatch
						    [RULE(VARpat w,mark e)]))
				vars
				(CASEexp(TUPLEexp(map dovar vars),
					 completeMatch(map doclause clauses)))
	     in RVB {var=var,
		     exp=makeexp vars,
		     resultty=NONE,
		     tyvars=tyvars}
	    end
          | fb2rvb _ = ErrorMsg.impossible "absyn.38"
     in VALRECdec (map fb2rvb fbl)
    end

fun SELECTORexp id = 
	let val v = mkVALvar id
	 in FNexp(completeMatch[RULE(RECORDpat{fields=[(id,VARpat v)], 
					       flex=true, typ= ref UNDEFty,
					       pats=ref nil},
			VARexp(ref v))])
	end

end (* local open Access Basics ... *)
end (* structure Absyn *)

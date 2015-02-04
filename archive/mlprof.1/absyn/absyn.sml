(* absyn.sml (formerly derived.sml) *)

structure Absyn = struct

structure Basics = Basics

local
  open Access Basics BasicTypes
  val whileSym = Symbols.stringToSymbol("while")
  and argSym = Symbols.stringToSymbol("arg")
  and handleSym = Symbols.stringToSymbol("handle")
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
   end;

fun TUPLEpat l =
   let fun addlabels(i,e::r) = (Tuples.numlabel i, e) :: addlabels(i+1, r)
	 | addlabels(_, nil) = nil
    in RECORDpat{fields = addlabels(1,l), flex = false, typ = ref UNDEFty,
		 pats = ref nil}
   end;

fun LISTexp l = fold (fn (e,rest) => APPexp(consExp,TUPLEexp[e,rest])) l nilExp;

fun IFexp (a,b,c) =
    CASEexp(a, [RULE(truePat,b), RULE(falsePat,c)]);

fun ORELSEexp(a,b) =
    IFexp(a,trueExp,b);

fun ANDALSOexp(a,b) =
    IFexp(a,b,falseExp);

fun WHILEexp (a,b) =
    let val tyref = ref UNDEFty
	val lvar = namedLvar(handleSym)
	val fb = VALvar{name=handleSym,
		        access=LVAR(lvar),
			vtype=tyref}
	val fa = VALvar{name=handleSym,
		      	access=PATH[lvar],
			vtype=tyref}
     in LETexp(
	  VALRECdec[
	    RVB{var=fb,
		exp=FNexp[
		      RULE(unitPat,
		           IFexp(a,
				 SEQexp[b, APPexp(VARexp(ref fa),unitExp)],
			         unitExp))],
		resultty = NONE,
		tyvars = nil}],
	  APPexp(VARexp (ref fa), unitExp))
    end;

fun LISTpat l = 
    fold (fn (e,rest) => APPpat(consDcon, TUPLEpat[e,rest])) l nilPat;

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
	     in RVB {var=var,
		     exp=fold (fn (w,e) => FNexp[RULE(VARpat w,e)]) vars
			 (CASEexp(not1(TUPLEexp, map dovar vars),
				  map doclause clauses)),
		     resultty=NONE,
		     tyvars=tyvars}
	    end
          | fb2rvb _ = ErrorMsg.impossible "absyn.38"
     in VALRECdec (map fb2rvb fbl)
    end

datatype hrule	= WITHhrule of datacon * rule list  (* only for handlex *)
		| ARROWhrule of datacon * exp
		| QUERYhrule of exp

fun HANDLERX hrules =
    let val rec hrule2rules =
	     (fn (WITHhrule(exn,rules)) => 
		    (map (fn (RULE(pat,e)) => RULE(APPpat(exn,pat),e))
			 rules)
	       | (QUERYhrule e) => [RULE(WILDpat,e)]
	       | (ARROWhrule(exn as DATACON{const,...},e)) =>
		   if const
		   then [RULE(CONpat exn, e)]
		   else [RULE(APPpat(exn,WILDpat),e)])
     in HANDLER(FNexp(fold (fn(hrule,l) => hrule2rules hrule @ l) hrules nil))
    end

fun RAISEXexp (exn,exp_opt) = 
    case exp_opt
      of SOME e => RAISEexp(APPexp(CONexp(exn),e))
       | NONE => RAISEexp(CONexp exn)

end (* local open Access Basics ... *)
end (* structure Absyn *)

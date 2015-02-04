(* absyn.sml (formerly derived.sml) *)

structure Absyn = struct

structure Basics = Basics

local
  open Access Basics BasicTypes
  val whileSym = SymbolTable.StringToSymbol("while")
  and argSym = SymbolTable.StringToSymbol("arg")
  and handleSym = SymbolTable.StringToSymbol("handle")
in
	
open BareAbsyn

val UNITpat = RECORDpat{fields = nil, flex = false, typ = ref UNKNOWNty,
			pats = ref nil}
val UNITexp = RECORDexp nil

val TRUEpat = CONpat(TRUEdcon)
val TRUEexp = CONexp(TRUEdcon)
val FALSEpat = CONpat(FALSEdcon)
val FALSEexp = CONexp(FALSEdcon)

val NILpat = CONpat(NILdcon)
val NILexp = CONexp(NILdcon)
val CONSpat = fn pat => APPpat(CONSdcon,pat)
val CONSexp = CONexp(CONSdcon)

fun TUPLEexp l = 
   let fun addlabels(i,e::r) = 
	     (LABEL{number=i-1, name=(Tuples.numlabel i)}, e) :: addlabels(i+1, r)
	 | addlabels(_, nil) = nil
    in RECORDexp (addlabels(1,l))
   end;

fun TUPLEpat l =
   let fun addlabels(i,e::r) = (Tuples.numlabel i, e) :: addlabels(i+1, r)
	 | addlabels(_, nil) = nil
    in RECORDpat{fields = addlabels(1,l), flex = false, typ = ref UNKNOWNty,
		 pats = ref nil}
   end;

fun LISTexp l = fold (fn (e,rest) => APPexp(CONSexp,TUPLEexp[e,rest])) l NILexp;

fun IFexp (a,b,c) =
    CASEexp(a, [RULE(TRUEpat,b), RULE(FALSEpat,c)]);

fun ORELSEexp(a,b) =
    IFexp(a,TRUEexp,b);

fun ANDALSOexp(a,b) =
    IFexp(a,b,FALSEexp);

fun WHILEexp (a,b) =
    let val tyref = ref UNKNOWNty
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
		      RULE(UNITpat,
		           IFexp(a,
				 SEQexp[b, APPexp(VARexp(ref fa),UNITexp)],
			         UNITexp))],
		resultty = NONE,
		tyvars = nil}],
	  APPexp(VARexp (ref fa), UNITexp))
    end;

fun LISTpat l = 
    fold (fn (e,rest) => APPpat(CONSdcon, TUPLEpat[e,rest])) l NILpat;

fun FUNdec fbl =
    let fun fb2rvb (FB {var, clauses as (CLAUSE{pats,...}::_),tyvars}) =
	let fun getvars (hd::tl) = mkVALvar(argSym,ref UNKNOWNty) :: getvars tl
	      | getvars nil = nil;
	    val vars = getvars pats
	    fun not1(f,[a]) = a
	      | not1(f,l) = f l
	    fun dovar (VALvar{access=LVAR n,name,vtype}) =
		   VARexp(ref(VALvar{access=PATH[n],name=name,vtype=vtype}))
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
       | fb2rvb _ = ErrorMsg.Impossible "absyn.38"
      in
	VALRECdec (map fb2rvb fbl)
     end

fun HANDLER hrules =
    let val rec hrule2rules =
	     (fn (WITHhrule(exn,rules)) => 
		    (map (fn (RULE(pat,e)) => RULE(APPpat(exn,pat),e))
			 rules)
	       | (WILDhrule e) => [RULE(WILDpat,e)])
	fun anywild ( WITHhrule _ ) = false
	  | anywild ( WILDhrule _ ) = true
     in HANDLERX(FNexp(fold (fn(hrule,l) => hrule2rules hrule @ l) hrules 
		    (if exists(anywild,hrules)
		       then nil
		       else [let val tyref = ref UNKNOWNty
				 val lvar = namedLvar(handleSym)
				 val vb = VALvar{name=handleSym,
				      	         access=LVAR(lvar),
						 vtype=tyref}
				 val va = VALvar{name=handleSym,
					      	 access=PATH[lvar],
						 vtype=tyref}
			      in RULE(VARpat vb, RAISEXexp (VARexp (ref va)))
			     end])))
    end;

fun RAISEexp (exn,e) = RAISEXexp(APPexp(CONexp(exn),e));

local exceptionx Abstype
 in
    fun ABSTYPEdec (a,b) = raisex Abstype
end

end (* local open B Basics *)
end (* structure Absyn *)

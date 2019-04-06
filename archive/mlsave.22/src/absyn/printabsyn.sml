signature PRINTABSYN =
sig
    structure BareAbsyn : BAREABSYN
    val printPat : BareAbsyn.pat * int -> unit
    val printExp : BareAbsyn.exp * int * int -> unit
    val printRule : BareAbsyn.rule * int * int -> unit
    val printVB : BareAbsyn.vb * int * int -> unit
    val printRVB : BareAbsyn.rvb * int * int -> unit
    val printDec : BareAbsyn.dec * int * int -> unit
    val printStrexp : BareAbsyn.strexp * int * int -> unit
end

structure PrintAbsyn : PRINTABSYN = struct

structure BareAbsyn = BareAbsyn
open BareAbsyn Access Basics PrintUtil PrintType PrintBasics ErrorMsg Tuples

fun checkpat (n,nil) = true
  | checkpat (n, (sym,_)::fields) = 
	Symbol.eq(sym, numlabel n) andalso checkpat(n+1,fields)

fun checkexp (n,nil) = true
  | checkexp (n, (LABEL{name=sym,...},_)::fields) = 
	Symbol.eq(sym, numlabel n) andalso checkexp(n+1,fields)

fun isTUPLEpat (RECORDpat{fields=[_],...}) = false
  | isTUPLEpat (RECORDpat{flex=false,fields,...}) = checkpat(1,fields)
  | isTUPLEpat _ = false
	
fun isTUPLEexp (RECORDexp [_]) = false
  | isTUPLEexp (RECORDexp fields) = checkexp(1,fields)
  | isTUPLEexp _ = false

fun printPat (_,0) = prstr "<pat>"
  | printPat (VARpat v,_) = printVar v
  | printPat (WILDpat,_) = prstr "_"
  | printPat (INTpat i,_) = (print i; ())
  | printPat (REALpat r,_) = (print r; ())
  | printPat (STRINGpat s,_) = pr_mlstr s
  | printPat (LAYEREDpat (v,p),d) = (printPat(v,d); prstr " as "; printPat(p,d-1))
  | printPat (r as RECORDpat{fields,flex,...},d) =
    if isTUPLEpat r
    then printClosedSequence ("(",",",")") (fn(sym,pat)=>printPat(pat,d-1)) fields
    else printClosedSequence
	 ("{", ",", (if flex then ",...}" else "}"))
         (fn (sym,pat) => (printSym sym; prstr "="; printPat(pat,d-1)))
	 fields
  | printPat (CONpat e,_) = printDcon e
  | printPat (p as APPpat _, d) =
	let val noparen = INfix(0,0)
	in  printDconPat(p,noparen,noparen,d)
	end
  | printPat (CONSTRAINTpat (p,t),d) = (printPat(p,d-1); prstr " : "; printType t)

and printDconPat(_,_,_,0) = prstr "<pat>"
  | printDconPat(CONpat(DATACON{name,...}),l:fixity,r:fixity,_) = printSym name
  | printDconPat(CONSTRAINTpat(p,t),l,r,d) =
	(print "("; printPat(p,d-1); print " : "; printType t; prstr ")")
  | printDconPat(LAYEREDpat(v,p),l,r,d) =
	(print "("; printPat(v,d); print " as "; printPat(p,d-1); prstr ")")
  | printDconPat(APPpat(DATACON{name,...},p),l,r,d) =
    let val dname = Symbol.name name
	val fixity = EnvAccess.lookFIX name
	fun prdcon() =
	    case (fixity,isTUPLEpat p,p)
	      of (INfix _,true,RECORDpat{fields=[(_,pl),(_,pr)],...}) =>
			 (printDconPat(pl,NONfix,fixity,d-1);
			  print " "; print dname; print " ";
			  printDconPat(pr,fixity,NONfix,d-1))
		| _ => (print dname; print " "; printDconPat(p,NONfix,NONfix,d-1))
    in  case(l,r,fixity) of
	      (NONfix,NONfix,_) => (print "("; prdcon(); prstr ")")
	    | (INfix _,INfix _,_) => prdcon()
	    | (_,_,NONfix) => prdcon()
	    | (INfix(_,p1),_,INfix(p2,_)) => if p1 >= p2
					     then (print "("; prdcon(); prstr ")")
					     else prdcon()
	    | (_,INfix(p1,_),INfix(_,p2)) => if p1 > p2
					     then (print "("; prdcon(); prstr ")")
					     else prdcon()
    end
  | printDconPat (p,_,_,d) = printPat(p,d)

fun printExp(_,_,0) = prstr "<exp>"
  | printExp(VARexp(ref var),_,_) = printVar var
  | printExp(CONexp(con),_,_) = printDcon con
  | printExp(INTexp i,_,_) = (print i; ())
  | printExp(REALexp r,_,_) = (print r; ())
  | printExp(STRINGexp s,_,_) = pr_mlstr s
  | printExp(r as RECORDexp fields,ind,d) =
    if isTUPLEexp r
    then printClosedSequence ("(",",",")") (fn(_,exp)=>printExp(exp,ind+1,d-1))
	 fields
    else printClosedSequence ("{", ",", "}")
	 (fn (LABEL{name,...},exp) =>
	   (printSym name; prstr "="; printExp(exp,ind+1,d)))
	 fields
  | printExp(SEQexp exps,ind,d) =
      printClosedSequence ("(", ";", ")") (fn exp => printExp(exp,ind+1,d-1)) exps
  | printExp(e as APPexp _,ind,d) = let val noparen = INfix(0,0)
				    in  printAppExp(e,noparen,noparen,ind,d)
				    end
  | printExp(CONSTRAINTexp(e, t),ind,d) =
      (prstr "("; printExp(e,ind,d); prstr ":"; printType t; prstr ")")
  | printExp(HANDLEexp(exp, HANDLER handler),ind,d) =
      (printExp(exp,ind,d-1); nlindent(ind); prstr "handle ";
       printExp(handler,ind+7,d-1))
  | printExp(RAISEexp exp,ind,d) = (prstr "raise "; printExp(exp,ind+6,d-1))
  | printExp(LETexp(dec, exp),ind,d) =
      (prstr "let "; printDec(dec,ind+4,d-1); nlindent(ind);
       prstr " in "; printExp(exp,ind+4,d-1); nlindent(ind);
       prstr "end")
  | printExp(CASEexp(exp, rules),ind,d) =
      (prstr "(case "; printExp(exp,ind+5,d-1); nlindent(ind+3);
       prstr "of "; printvseq (ind+4) "| " (fn r => printRule(r,ind+4,d-1)) rules;
       prstr ")")
  | printExp(FNexp rules,ind,d) =
      (prstr "(fn "; printvseq (ind+1) "| " (fn r => printRule(r,ind+3,d-1)) rules;
       prstr ")")

and printAppExp(_,_,_,_,0) = prstr "<exp>"
  | printAppExp arg =
    let fun fixityprint(name,e,l,r,ind,d) =
	    let val dname = Symbol.name name
		val fixity = EnvAccess.lookFIX name
		fun pr() =
		    case (fixity,isTUPLEexp e,e)
		      of (INfix _,true,RECORDexp[(_,pl),(_,pr)]) =>
				 (printAppExp(pl,NONfix,fixity,ind,d-1);
				  print " "; print dname; print " ";
				  printAppExp(pr,fixity,NONfix,ind+2,d-1))
			| _ => (print dname; print " ";
			        printAppExp(e,NONfix,NONfix,ind+2,d-1))
	    in  case(l,r,fixity) of
		      (NONfix,NONfix,_) => (print "("; pr(); prstr ")")
		    | (INfix _,INfix _,_) => pr()
		    | (_,_,NONfix) => pr()
		    | (INfix(_,p1),_,INfix(p2,_)) =>
				if p1 >= p2 then (print "("; pr(); prstr ")")
				else pr()
		    | (_,INfix(p1,_),INfix(_,p2)) =>
				if p1 > p2 then (print "("; pr(); prstr ")")
				else pr()
	    end
	fun appPrint(_,_,_,_,0) = prstr "#"
	  | appPrint(CONSTRAINTexp(e,t),l,r,ind,d) =
	      (print "("; printExp(e,ind+1,d-1);
	       print " : "; printType t; prstr ")")
	  | appPrint(APPexp(CONexp(DATACON{name,...}),e),l,r,ind,d) =
		fixityprint(name,e,l,r,ind,d)
	  | appPrint(APPexp(VARexp(ref(VALvar{name,...})),e),l,r,ind,d) =
		fixityprint(name,e,l,r,ind,d)
	  | appPrint(APPexp(VARexp(ref(OVLDvar{name,...})),e),l,r,ind,d) =
		fixityprint(name,e,l,r,ind,d)
	  | appPrint(APPexp(app as APPexp _,rand),NONfix,NONfix,ind,d) =
		let val yesparen = INfix(0,100000000) (* a hack *)
		in  print "("; appPrint(app,yesparen,NONfix,ind+1,d-1);
		    print " ";
		    appPrint(rand,NONfix,NONfix,ind+2,d-1); prstr ")"
		end
	  | appPrint(APPexp(app as APPexp _,rand),l,r,ind,d) =
		let val yesparen = INfix(0,100000000) (* a hack *)
		in  appPrint(app,yesparen,NONfix,ind+1,d-1);
		    print " ";
		    appPrint(rand,NONfix,NONfix,ind+2,d-1)
		end
	  | appPrint(APPexp(rator,rand),_,_,ind,d) =
		(printExp(rator,ind,d-1); print " "; printExp(rand,ind+2,d-1))
	  | appPrint (e,_,_,ind,d) = printExp(e,ind,d)
    in  appPrint arg
    end

and printRule(RULE(pat,exp),ind,d) =
    if d>0
    then (printPat(pat,d-1); prstr " => "; printExp(exp,ind+2,d-1))
    else prstr "<rule>"

and printVB(VB{pat,exp,...},ind,d) =
    if d>0
    then (printPat(pat,d-1); prstr " = "; printExp(exp,ind+2,d-1))
    else prstr "<binding>"

and printRVB(RVB{var,exp,...},ind,d) = 
    if d>0
    then (printVar var; prstr " = "; printExp(exp,ind+2,d-1))
    else prstr "<rec binding>"

and printDec(_,_,0) = prstr "<dec>"
  | printDec(VALdec vbs,ind,d) =
      (prstr "val "; printvseq ind "and " (fn vb => printVB(vb,ind+4,d-1)) vbs)
  | printDec(VALRECdec rvbs,ind,d) =
      (prstr "val rec "; 
       printvseq (ind+4) "and " (fn rvb => printRVB(rvb,ind+8,d-1)) rvbs)
  | printDec(TYPEdec tbs,ind,d) =
      (prstr "type ";
       printvseq ind " and "
         (fn (TB{tyc=ref(TYCON{path=name::_, arity,...}),def}) =>
	     (case arity
		of 0 => ()
		 | 1 => (prstr "'a ")
		 | n => (printTuple prstr (typeFormals n); prstr " ");
	      printSym name; prstr " = "; printType def)
	   | _ => impossible "printabsyn.398")
	 tbs)
  | printDec(DATATYPEdec{datatycs,withtycs},ind,d) =
      (prstr "datatype ";
       printvseq (ind+5) "and "
         (fn (ref(TYCON{path=name::_, arity, kind=DATAtyc dcons,...})) =>
	     (case arity
		of 0 => ()
		 | 1 => (prstr "'a ")
		 | n => (printTuple prstr (typeFormals n); prstr " ");
	      printSym name; prstr " = ";
	      printSequence " | " (fn (DATACON{name,...}) => printSym name) dcons)
           | _ => impossible "printabsyn.8")
	 datatycs;
       nlindent(ind); prstr "with"; printDec(TYPEdec withtycs,ind+4,d-1))
  | printDec(ABSTYPEdec _,ind,d) = prstr "abstype"
  | printDec(EXCEPTIONdec ebs,ind,d) =
      (prstr "exception ";
       printvseq (ind+6) "and "
         (fn (EBgen{exn=DATACON{name,...},etype}) =>
	       (printSym name;
	        case etype of NONE => ()
			    | SOME ty' => (prstr " of "; printType ty'))
	   | (EBdef{exn=DATACON{name,...},edef=DATACON{name=dname,...}}) =>
	       (printSym name; prstr "="; printSym dname))
	 ebs)
  | printDec(STRdec sbs,ind,d) =
      (prstr "structure ";
       printvseq ind "and "
	 (fn (STRB{strvar=STRvar{access,name,...},def,...}) =>
	     (printSym name; printAccess access; prstr " = "; nlindent(ind+4);
	      printStrexp(def,ind+4,d-1)))
         sbs)
  | printDec(ABSdec sbs,ind,d) =
      (prstr "abstraction ";
       printvseq ind "and "
	 (fn (STRB{strvar=STRvar{access,name,...},def,...}) =>
	     (printSym name; printAccess access; prstr " = "; nlindent(ind+4);
	      printStrexp(def,ind+4,d-1)))
         sbs)
  | printDec(SIGdec sigvars,ind,d) =
      printvseq ind ""
	(fn SIGvar{name,...} => (print "signature "; printSym name))
	sigvars
  | printDec(LOCALdec(inner,outer),ind,d) =
      (prstr "local"; nlindent(ind+3);
       printDec(inner,ind+3,d-1); nlindent(ind);
       prstr "in ";
       printDec(outer,ind+3,d-1); nlindent(ind);
       prstr "end")
  | printDec(SEQdec decs,ind,d) =
      printvseq ind "" (fn dec => printDec(dec,ind,d)) decs
  | printDec(OPENdec strVars,ind,d) =
      (prstr "open ";
       printSequence " " (fn STRvar{name,...} => printSym name) strVars)
  | printDec(_) = prstr "printDec gives up"

and printStrexp(_,_,0) = prstr "<strexp>"
  | printStrexp(VARstr(STRvar{access,name,...}),ind,d) = 
      printSym name
  | printStrexp(STRUCTstr{body,locations},ind,d) =
      (prstr "struct"; nlindent(ind+2);
       printvseq (ind+2) "" (fn dec => printDec(dec,ind+2,d-1)) body;
       nlindent(ind); prstr "end")
  | printStrexp(APPstr{oper=FCTvar{name,...}, argexp,...},ind,d) =
      (printSym name; prstr"(";
       printStrexp(argexp,ind+4,d-1);
       prstr")")

end (* structure PrintAbsyn *)

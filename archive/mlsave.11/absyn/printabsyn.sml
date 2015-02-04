(* printabsyn.sml *)

(* ugly-printer for bareabsyn *)
signature PRINTABSYN =
sig
    structure A : BAREABSYN
    val printPat : A.pat -> unit
    val printExp : A.exp -> unit
    val printRule : A.rule -> unit
    val printVB : A.vb -> unit
    val printRVB : A.rvb -> unit
    val printDec : A.dec -> unit
    val printStrexp : A.strexp -> unit
end

structure PrintAbsyn : PRINTABSYN = struct

structure A : BAREABSYN = BareAbsyn
open A Access Basics PrintUtil PrintType PrintBasics ErrorMsg Tuples

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

fun printPat (VARpat v) = printVar v
  | printPat WILDpat = prstr "_"
  | printPat (INTpat i) = (print i; ())
  | printPat (REALpat r) = (print r; ())
  | printPat (STRINGpat s) = pr_mlstr s
  | printPat (LAYEREDpat (v,p)) = (printPat v; prstr " as "; printPat p)
  | printPat (r as RECORDpat{fields,flex,...}) =
      if isTUPLEpat r 
	then printClosedSequence("(",",",")") (fn(sym,pat)=>printPat pat)fields
        else printClosedSequence
	("{", ",", (if flex then ",...}" else "}"))
        (fn (sym,pat) => 
	     (printSym sym; prstr "="; printPat pat))
	fields
  | printPat (CONpat e) = printDcon e
  | printPat (p as APPpat _) =
	let val noparen = INfix(0,0)
	in  printDconPat(p,noparen,noparen)
	end
  | printPat (CONSTRAINTpat (p,t)) =
      (printPat p; prstr " : "; printType t)

and printDconPat(CONpat(DATACON{name,...}),l:fixity,r:fixity) =
	prstr(Symbol.name name)
  | printDconPat(CONSTRAINTpat(p,t),l,r) =
	(print "("; printPat p; print " : "; printType t; prstr ")")
  | printDconPat(LAYEREDpat(v,p),l,r) =
	(print "("; printPat v; print " as "; printPat p; prstr ")")
  | printDconPat(APPpat(DATACON{name,...},p),l,r) =
    let val dname = Symbol.name name
	val fixity = EnvAccess.lookFIX name
	fun prdcon() =
	    case (fixity,isTUPLEpat p,p)
	      of (INfix _,true,RECORDpat{fields=[(_,pl),(_,pr)],...}) =>
			 (printDconPat(pl,NONfix,fixity);
			  print " "; print dname; print " ";
			  printDconPat(pr,fixity,NONfix))
		| _ =>
		   (print dname; print " ";
		    printDconPat(p,NONfix,NONfix))
    in  case(l,r,fixity) of
	      (NONfix,NONfix,_) => (print "("; prdcon(); prstr ")")
	    | (INfix _,INfix _,_) => prdcon() (* special case:
						 only occurs on first iteration,
						 which wants no parens *)
	    | (_,_,NONfix) => prdcon()
	    | (INfix(_,p1),_,INfix(p2,_)) =>
			if p1 >= p2 then (print "("; prdcon(); prstr ")")
			else prdcon()
	    | (_,INfix(p1,_),INfix(_,p2)) =>
			if p1 > p2 then (print "("; prdcon(); prstr ")")
			else prdcon()
    end
  | printDconPat (p,_,_) = printPat p

fun printExp(VARexp(ref var)) = printVar var
  | printExp(CONexp(con)) = printDcon con
  | printExp(INTexp i) = (print i; ())
  | printExp(REALexp r) = (print r; ())
  | printExp(STRINGexp s) = pr_mlstr s
  | printExp(r as RECORDexp fields) = 
      if isTUPLEexp r
        then printClosedSequence("(",",",")")(fn(_,exp)=>printExp exp)fields
        else printClosedSequence("{", ",", "}")
		(fn (LABEL{name,...},exp) => 
		     (printSym name; prstr "="; printExp exp))
		fields
  | printExp(SEQexp exps) =
      printClosedSequence ("(", ";", ")") printExp exps
  | printExp(e as APPexp _) =
	let val noparen = INfix(0,0)
	in  printAppExp(e,noparen,noparen)
	end
  | printExp(CONSTRAINTexp(e, t)) =
      (prstr "("; printExp e; prstr ":"; printType t; prstr ")")
  | printExp(HANDLEexp(exp, HANDLER handler)) =
      (printExp exp; prstr " handle "; printExp handler)
  | printExp(RAISEexp exp) = (prstr "raise "; printExp exp)
  | printExp(LETexp(dec, exp)) =
      (prstr "let "; printDec dec; prstr " in "; printExp exp; prstr " end")
  | printExp(CASEexp(exp, rules)) =
      (prstr "case "; printExp exp; prstr " of "; printSequence "|" printRule rules)
  | printExp(FNexp rules) =
      (prstr "(fn "; printSequence "|" printRule rules; prstr ")")

and printAppExp arg =
let fun fixityprint(name,e,l,r) =
    let val dname = Symbol.name name
	val fixity = EnvAccess.lookFIX name
	fun pr() =
	    case (fixity,isTUPLEexp e,e)
	      of (INfix _,true,RECORDexp[(_,pl),(_,pr)]) =>
			 (printAppExp(pl,NONfix,fixity);
			  print " "; print dname; print " ";
			  printAppExp(pr,fixity,NONfix))
		| _ =>
		   (print dname; print " ";
		    printAppExp(e,NONfix,NONfix))
    in  case(l,r,fixity) of
	      (NONfix,NONfix,_) => (print "("; pr(); prstr ")")
	    | (INfix _,INfix _,_) => pr() (* special case:
						 only occurs on first iteration,
						 which wants no parens *)
	    | (_,_,NONfix) => pr()
	    | (INfix(_,p1),_,INfix(p2,_)) =>
			if p1 >= p2 then (print "("; pr(); prstr ")")
			else pr()
	    | (_,INfix(p1,_),INfix(_,p2)) =>
			if p1 > p2 then (print "("; pr(); prstr ")")
			else pr()
    end
    fun appPrint(CONSTRAINTexp(e,t),l,r) =
	    (print "("; printExp e; print " : "; printType t; prstr ")")
      | appPrint(APPexp(CONexp(DATACON{name,...}),e),l,r) =
	    fixityprint(name,e,l,r)
      | appPrint(APPexp(VARexp(ref(VALvar{name,...})),e),l,r) =
	    fixityprint(name,e,l,r)
      | appPrint(APPexp(VARexp(ref(OVLDvar{name,...})),e),l,r) =
	    fixityprint(name,e,l,r)
      | appPrint(APPexp(app as APPexp _,rand),NONfix,NONfix) =
	    let val yesparen = INfix(0,100000000) (* a hack *)
	    in  print "("; appPrint(app,yesparen,NONfix);
		print " ";
	 	appPrint(rand,NONfix,NONfix); prstr ")"
	    end
      | appPrint(APPexp(app as APPexp _,rand),l,r) =
	    let val yesparen = INfix(0,100000000) (* a hack *)
	    in  appPrint(app,yesparen,NONfix);
		print " ";
	 	appPrint(rand,NONfix,NONfix)
	    end
      | appPrint(APPexp(rator,rand),_,_) =
	    (printExp rator; print " "; printExp rand)
      | appPrint (e,_,_) = printExp e
in  appPrint arg
end

and printRule(RULE(pat,exp)) = (printPat pat; prstr " => "; printExp exp)

and printVB(VB{pat,exp,...}) = (printPat pat; prstr " = "; printExp exp)

and printRVB(RVB{var,exp,...}) = (printVar var; prstr " = "; printExp exp)

and printDec(VALdec vbs) =
      (prstr "val "; printSequence "\n  and " printVB vbs)
  | printDec(VALRECdec rvbs) =
      (prstr "val rec "; printSequence "\n  and " printRVB rvbs)
  | printDec(TYPEdec tycons) =
      (prstr "type ";
       printSequence "\n  and "
         (fn (TYPEtyc{name,params,def,...}) =>
	     (case params
		of [] => ()
		 | [tv] => (printTyvar tv; prstr " ")
		 | tvs => printTuple printTyvar tvs;
	      printSym name; prstr " = "; printType def)
	     | _ => impossible "printabsyn.398")
	 tycons)
  | printDec(DATATYPEdec tycons) =
      (prstr "datatype ";
       printSequence "\n  and "
         (fn (DATAtyc{name,params,dcons,...}) =>
	     (case params
		of [] => ()
		 | [tv] => (printTyvar tv; prstr " ")
		 | tvs => printTuple printTyvar tvs;
			  printSym name; prstr " = ";
			  printSequence " | "
					(fn (DATACON{name,...}) => printSym name)
					(!dcons))
	     | _ => impossible "printabsyn.8")
	 tycons)
  | printDec(EXCEPTIONdec ebs) =
      (prstr "exception ";
       printSequence "\n  and "
         (fn (EB{exn=DATACON{name,...},ty,def}) =>
	     (printSym name;
	      case ty of NONE => () | SOME ty' => (prstr ":"; printType ty');
	      case def
	        of NONE => ()
		 | SOME(DATACON{name,...}) => (prstr "="; printSym name)))
	 ebs)
  | printDec(STRdec sb) =
      (prstr "structure ";
       let val STRB{strvar=STRvar{access,name,...},def,...} = sb
        in printSym name; printAccess access; prstr " = "; printStrexp def
       end)
(* for simultaneous structure declarations 
       printSequence
         "\n  and "
	 (fn (STRB{strvar=STRvar{access,name,...},def,...}) =>
	     (printSym name; printAccess access; prstr " = "; printStrexp def))
         sbs)
*)
  | printDec(ABSdec sb) =
      (prstr "abstraction ";
       let val STRB{strvar=STRvar{access,name,...},def,...} = sb
        in printSym name; printAccess access; prstr " = "; printStrexp def
       end)
  | printDec(SIGdec sigvars) = printSequence "\n"
					     (fn SIGvar{name,...} =>
					       (print "signature "; printSym name))
					     sigvars
  | printDec(LOCALdec(inner,outer)) =
      (prstr "local\n";
       printDec inner;
       prstr "\nin\n";
       printDec outer;
       prstr "\nend")
  | printDec(SEQdec decs) =
      printSequence "\n " printDec decs
  | printDec(OPENdec _) = prstr "open..."
  | printDec(_) = prstr "printDec gives up"

and printStrexp(VARstr(STRvar{access,name,...})) = 
      (printSym name; printAccess access)
  | printStrexp(STRUCTstr{body,locations}) =
      (prstr "struct\n";
       printSequence "\n " printDec body;
       prstr "\nend";
(*       prstr "\nlocations:\n";
       printSequence "\n " 
		     (fn path => (printPath path;()))
		     locations;
*)
       newline())
  | printStrexp(APPstr{oper=FCTvar{name,...}, argexp,...}) =
	    (printSym name; prstr"(";
	     printStrexp argexp;
	     prstr")")

end (* structure PrintAbsyn *)

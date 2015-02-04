(* printabsyn.sml *)

(* ugly-printer for bareabsyn *)

structure PrintAbsyn = struct

local 
  open Access Basics BareAbsyn PrintUtil PrintType PrintBasics ErrorMsg Tuples

 fun checkpat (n,nil) = true
   | checkpat (n, (sym,_)::fields) = 
	Symbol.Eq(sym, numlabel n) andalso checkpat(n+1,fields)

 fun checkexp (n,nil) = true
   | checkexp (n, (LABEL{name=sym,...},_)::fields) = 
	Symbol.Eq(sym, numlabel n) andalso checkexp(n+1,fields)
in

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
  | printPat (STRINGpat s) = (prstr "\""; prstr(s); prstr "\"")
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
  | printPat (APPpat (e,p)) = (printDcon e; prstr "("; printPat p; prstr ")")
  | printPat (CONSTRAINTpat (p,t)) =
      (prstr "("; printPat p; prstr ":"; printType t; prstr ")")

fun printExp(VARexp(ref var)) = printVar var
  | printExp(CONexp(con)) = printDcon con
  | printExp(INTexp i) = (print i; ())
  | printExp(REALexp r) = (print r; ())
  | printExp(STRINGexp s) = (prstr "\""; prstr(s); prstr "\"")
  | printExp(r as RECORDexp fields) = 
      if isTUPLEexp r
        then printClosedSequence("(",",",")")(fn(_,exp)=>printExp exp)fields
        else printClosedSequence("{", ",", "}")
		(fn (LABEL{name,...},exp) => 
		     (printSym name; prstr "="; printExp exp))
		fields
  | printExp(SEQexp exps) =
      printClosedSequence ("(", ";", ")") printExp exps
  | printExp(APPexp(rator,rand)) =
      (printExp rator; prstr "("; printExp rand; prstr ")")
  | printExp(CONSTRAINTexp(e, t)) =
      (prstr "("; printExp e; prstr ":"; printType t; prstr ")")
  | printExp(HANDLEexp(exp, HANDLERX handler)) =
      (printExp exp; prstr " handle "; printExp handler)
  | printExp(RAISEXexp exp) = (prstr "raise "; printExp exp)
  | printExp(LETexp(dec, exp)) =
      (prstr "let "; printDec dec; prstr " in "; printExp exp; prstr " end")
  | printExp(CASEexp(exp, rules)) =
      (prstr "case "; printExp exp; prstr " of "; printSequence "|" printRule rules)
  | printExp(FNexp rules) =
      (prstr "(fn "; printSequence "|" printRule rules; prstr ")")

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
	     | _ => Impossible "printabsyn.398")
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
	      printSequence
	         " | " (fn (DATACON{name,...}) => printSym name) (!dcons))
	     | _ => Impossible "printabsyn.8")
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

end (* local BareAbsyn PrintUtil *)

end (* structure PrintAbsyn *)

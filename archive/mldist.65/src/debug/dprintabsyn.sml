signature DPRINTABSYN =
sig
    type cp2lnf
    val printPat : BareAbsyn.pat * int -> unit
    val printExp : BareAbsyn.exp * cp2lnf * int * int -> unit
    val printRule : BareAbsyn.rule * cp2lnf * int * int -> unit
    val printVB : BareAbsyn.vb * cp2lnf * int * int -> unit
    val printRVB : BareAbsyn.rvb * cp2lnf * int * int -> unit
    val printDec : BareAbsyn.dec * cp2lnf * int * int -> unit
    val printStrexp : BareAbsyn.strexp * cp2lnf * int * int -> unit
end

structure DPrintAbsyn : DPRINTABSYN = struct

open BareAbsyn Access Basics PrintUtil PrintType DPrintBasics ErrorMsg Tuples

val markprint = ref true
val lineprint = ref false

type cp2lnf = int -> (string * int * int)

fun printPat (_,0) = print "<pat>"
  | printPat (VARpat v,_) = (print "VARpat("; printVar v; print ")";())
  | printPat (WILDpat,_) = print "WILDpat"
  | printPat (INTpat i,_) = (print "INTpat("; print i; print ")"; ())
  | printPat (REALpat r,_) = (print "REALpat("; print r;print ")"; ())
  | printPat (STRINGpat s,_) = (print "STRINGpat("; pr_mlstr s; print ")"; ())
  | printPat (LAYEREDpat (v,p),d) = (print "LAYEREDpat("; printPat(v,d);
					print ","; printPat(p,d-1); print ")"; ())
  | printPat (RECORDpat{fields,flex,typ=t,pats=p},d) =
    (print "RECORDpat{fields=[";
	printvseq 0 "," (fn(sym,pat)=>(print "("; printSym sym; print ",";
						printPat(pat,d-1); print ")")) fields;
	print "],flex = "; print flex; print ", typ = ref"; printType (!t);
	print ",pats = ref [";
	printvseq 0 ", " (fn pat => printPat(pat,d-1)) (!p); 
	print "]}")
  | printPat (CONpat e,_) = (print "CONpat("; printDcon e; print ")"; ())
  | printPat (APPpat (e,p), d) = (print "APPpat("; printDcon e; print ",";
				printPat(p, d-1); print ")"; ())
  | printPat (CONSTRAINTpat (p,t),d) = (print "CONSTRAINTpat("; printPat(p,d-1); print " , "; printType t; print ")"; ())

and printExp(_,_,_,0) = print "<exp>"
  | printExp(VARexp(ref var),_,_,_) = (print "VARexp(ref "; printVar var; print ")")
  | printExp(CONexp(con),_,_,_) = (print "CONexp("; printDcon con; print ")")
  | printExp(INTexp i,_,_,_) = (print "INTexp("; print i; print ")")
  | printExp(REALexp r,_,_,_) = (print "REALexp("; print r; print ")")
  | printExp(STRINGexp s,_,_,_) = (print "STRINGexp("; pr_mlstr s; print ")")
  | printExp(r as RECORDexp fields,cp2lnf,ind,d) = 
      (print "RECORDexp(["; map (fn (label,exp) =>
				 (print "("; printNumberedLabel(label); print ",";
						printExp(exp,cp2lnf,ind+1,d-1); print ")"))
				fields; print "])")
  | printExp(SEQexp exps,cp2lnf,ind, d) = 
      (print "SEQexp(["; map (fn exp=> printExp(exp,cp2lnf,ind+1,d-1)) exps; print "])")
  | printExp(APPexp(exp1,exp2),cp2lnf,ind, d) = 
      (print "APPexp("; printExp(exp1,cp2lnf, ind+1, d-1); print ","; printExp(exp2,cp2lnf,ind+1,d-1);
		print ")")
  | printExp(CONSTRAINTexp(e, t),cp2lnf,ind,d) =
      (print "CONSTRAINTexp("; printExp(e,cp2lnf,ind,d); print ","; printType t; print ")")
  | printExp(HANDLEexp(exp, HANDLER handler),cp2lnf,ind,d) =
      (print "HANDLEexp("; printExp(exp,cp2lnf,ind,d-1); print ", HANDLE(";
    			printExp(handler,cp2lnf,ind+7,d-1); print "))")
  | printExp(RAISEexp exp,cp2lnf,ind,d) = (print "RAISEexp("; printExp(exp,cp2lnf,ind+6,d-1); 
					print ")")
  | printExp(LETexp(dec, exp),cp2lnf,ind,d) =
      (print "LETexp( "; printDec(dec,cp2lnf,ind+4,d-1); nlindent(ind);
       print " , "; printExp(exp,cp2lnf,ind+4,d-1); nlindent(ind);
       print ")")
  | printExp(CASEexp(exp,rules),cp2lnf,ind,d) = 
      (print "CASEexp("; printExp(exp,cp2lnf,ind+5,d-1); print(", [");
		printvseq (ind+4) ", " (fn r =>printRule(r,cp2lnf,ind+4,d-1)) rules;
		print "])")
  | printExp(FNexp rules,cp2lnf,ind,d) =
      (print "FNexp([ "; printvseq (ind+1) ", " (fn r => printRule(r,cp2lnf,ind+3,d-1)) rules;
       print "])")
  | printExp(MARKexp (exp,s,e), cp2lnf,ind, d) = 
      (if !markprint then 
        (print "MARKexp("; printExp(exp,cp2lnf,ind,d-1); print ","; prpos(cp2lnf,s); print ","; prpos(cp2lnf,e); print ")")
       else printExp (exp,cp2lnf,ind,d))

and printNumberedLabel(LABEL{name,number}) = 
    (print "LABEL{name="; printSym name; print ",number="; print number; print "}")

and printRule(RULE(pat,exp),cp2lnf,ind,d) =
    (print "RULE("; printPat(pat,d-1); print ","; printExp(exp,cp2lnf,ind+2,d-1);print ")")

and printVB(VB{pat,exp,...},cp2lnf,ind,d) =
    (print "VB{pat="; printPat(pat,d-1); print ",exp="; printExp(exp,cp2lnf,ind+2,d-1);
	print ",tyvars=?")

and printRVB(RVB{var,exp,resultty,...},cp2lnf,ind,d) = 
    (print "RVB{var="; printVar var; print ",exp="; printExp(exp,cp2lnf,ind+2,d-1);
	print ",resultty="; (case resultty
				of NONE => print "NONE"
				 | SOME(ty) => (print "SOME("; printType(ty); print ")"));
	print ",tyvars=?")

and printDec(_,_,_,0) = print "<dec>"
  | printDec(VALdec vbs,cp2lnf,ind,d) =
      (print "VALdec(["; printvseq ind ", " (fn vb => printVB(vb,cp2lnf,ind+4,d-1)) vbs;
		print "])")
  | printDec(VALRECdec rvbs,cp2lnf,ind,d) =
      (print "VALRECdec([ "; 
       printvseq (ind+4) ", " (fn rvb => printRVB(rvb,cp2lnf,ind+8,d-1)) rvbs;
		print "])")
  | printDec(TYPEdec tbs,cp2lnf,ind,d) =
      (print "TYPEdec([";
       printvseq ind ", "
         (fn (TB{tyc=DEFtyc{path=name::_, tyfun=TYFUN{arity,...}}, def}) =>
	     (case arity
		of 0 => ()
		 | 1 => (print "'a ")
		 | n => (printTuple print (typeFormals n); print " ");
	      printSym name; print " = "; printType def)
	   | _ => impossible "printabsyn.398")
	 tbs; print "])")
  | printDec(DATATYPEdec{datatycs,withtycs},cp2lnf,ind,d) =
      (print "DATATYPEdec{datatycs=[";
       printvseq (ind+5) ", "
         (fn GENtyc{path=name::_, arity, kind=ref(DATAtyc dcons),...} =>
	     (case arity
		of 0 => ()
		 | 1 => (print "'a ")
		 | n => (printTuple print (typeFormals n); print " ");
	      printSym name; print " = ";
	      printSequence " | " (fn (DATACON{name,...}) => printSym name) dcons)
           | _ => impossible "printabsyn.8")
	 datatycs;
       nlindent(ind); print "],withtycs="; printDec(TYPEdec withtycs,cp2lnf,ind+4,d-1); print"}")
  | printDec(ABSTYPEdec{abstycs,withtycs,body},cp2lnf,ind,d) = 
	(print "ABSTYPEdec{abstycs=[";
       printvseq (ind+5) ", "
         (fn GENtyc{path=name::_, arity, kind=ref(DATAtyc dcons),...} =>
	     (case arity
		of 0 => ()
		 | 1 => (print "'a ")
		 | n => (printTuple print (typeFormals n); print " ");
	      printSym name; print " = ";
	      printSequence " | " (fn (DATACON{name,...}) => printSym name) dcons)
           | tyc  => (print "weirdo TYCON: "; printTycon tyc))
	 (abstycs);
       nlindent(ind); print "],withtycs=";
       printDec(TYPEdec withtycs,cp2lnf,ind+4,d-1);
       nlindent(ind); print ",body="; 
       printDec(body,cp2lnf,ind+4,d-1);print "}")
  | printDec(EXCEPTIONdec ebs,cp2lnf,ind,d) =
      (print "EXCEPTIONdec[";
       printvseq (ind+6) ", "
         (fn (EBgen{exn=DATACON{name,...},etype}) =>
	       (printSym name;
	        case etype of NONE => ()
			    | SOME ty' => (print " of "; printType ty'))
	   | (EBdef{exn=DATACON{name,...},edef=DATACON{name=dname,...}}) =>
	       (printSym name; print "="; printSym dname))
	 ebs;
	print "]")
  | printDec(STRdec sbs,cp2lnf,ind,d) =
      (print "STRdec[";
       printvseq ind ","
	 (fn (STRB{strvar,def,constraint,...}) =>
	     (print "STRB{strvar="; printStrVar strvar; print ",def=";
		printStrexp(def,cp2lnf,ind+4,d-1); print ",thin,constraint=";
		case constraint of SOME s => (print "SOME "; printStructure s)
				 | NONE => print "NONE";
	 	print "}"))
         sbs;
       print "]")
  | printDec(ABSdec sbs,cp2lnf,ind,d) =
      (print "ABSdec[";
       printvseq ind ","
	 (fn (STRB{strvar,def,constraint,...}) =>
	     (print "STRB{strvar="; printStrVar strvar; print ",def=";
		printStrexp(def,cp2lnf,ind+4,d-1); print ",thin,constraint=";
		case constraint of SOME s => (print "SOME "; printStructure s)
				 | NONE => print "NONE";
	 	print "}"))
         sbs;
       print "]")
  | printDec(FCTdec fbs,cp2lnf,ind,d) = 
      (print "FCTdec[";
       printvseq ind ","
	(fn (FCTB{fctvar=FCTvar{access,name,...},param,def,...}) =>
	     (print "FCTB{fctvar=FCTvar{name=";printSym name;
		print ",access=";printAccess access; print ",...},param=";
		printStrVar param; print ",def="; nlindent(ind+4);
		printStrexp(def,cp2lnf,ind+4,d-1); print ",...}"))
	fbs;
	print "]")
  | printDec(SIGdec sigvars,cp2lnf,ind,d) =
     (print "SIGdec[";
      printvseq ind ","
        (fn SIGvar{name,...} => (print "SIGvar{name="; printSym name;
				 print ",...}"))
	sigvars;
      print "]")
  | printDec(LOCALdec(inner,outer),cp2lnf,ind,d) =
      (print "LOCALdec("; nlindent(ind+3);
       printDec(inner,cp2lnf,ind+3,d-1); nlindent(ind);
       print ",";
       printDec(outer,cp2lnf,ind+3,d-1); nlindent(ind);
       print ")")
  | printDec(SEQdec decs,cp2lnf,ind,d) =
      (print "SEQdec[";
      printvseq ind "," (fn dec => printDec(dec,cp2lnf,ind,d)) decs;
      print "]")
  | printDec(OPENdec strVars,cp2lnf,ind,d) =
      (print "OPENdec[";
       printSequence " " printStrVar strVars;
       print "]")
  | printDec(MARKdec (dec,s,e), cp2lnf,ind, d) = 
      if !markprint then
        (print "MARKdec("; printDec(dec,cp2lnf,ind,d-1); print ","; prpos(cp2lnf,s); print ","; prpos(cp2lnf,e); print ")")
       else printDec(dec,cp2lnf,ind,d)
  | printDec(_) = print "printDec gives up"

and printStrexp(_,_,_,0) = print "<strexp>"
  | printStrexp(VARstr(STRvar{access,name,...}),cp2lnf,ind,d) = 
      print (formatQid name)
  | printStrexp(STRUCTstr{body,...},cp2lnf,ind,d) =
      (print "struct"; nlindent(ind+2);
       printvseq (ind+2) "" (fn dec => printDec(dec,cp2lnf,ind+2,d-1)) body;
       nlindent(ind); print "end")
  | printStrexp(APPstr{oper=FCTvar{name,...}, argexp,...},cp2lnf,ind,d) =
      (printSym name; print"(";
       printStrexp(argexp,cp2lnf,ind+4,d-1);
       print")")
  | printStrexp(LETstr(dec,body),cp2lnf,ind,d) =
      (print "let "; printDec(dec,cp2lnf,ind+4,d-1); nlindent(ind);
       print " in "; printStrexp(body,cp2lnf,ind+4,d-1); nlindent(ind);
       print "end")
  | printStrexp(MARKstr(body,s,e),cp2lnf,ind,d) = 
      if !markprint then
	(print "MARKstr("; printStrexp(body,cp2lnf,ind,d); print ","; 
		prpos (cp2lnf,s); print ","; prpos (cp2lnf,e); print ")")
      else printStrexp(body,cp2lnf,ind,d)

and prpos (cp2lnf,charpos:int) =
 	if (!lineprint) then
	  let val (file:string,line:int,pos:int) = cp2lnf charpos
	  in print line; print "."; print pos
	  end
	else print charpos

end (* structure PrintAbsyn *)

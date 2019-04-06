(* Copyright 1989 by AT&T Bell Laboratories *)
signature PRINTABSYN =
sig
    val printPat : Modules.env -> BareAbsyn.pat * int -> unit
    val printExp : Modules.env -> BareAbsyn.exp * int * int -> unit
    val printRule : Modules.env -> BareAbsyn.rule * int * int -> unit
    val printVB : Modules.env -> BareAbsyn.vb * int * int -> unit
    val printRVB : Modules.env -> BareAbsyn.rvb * int * int -> unit
    val printDec : Modules.env -> BareAbsyn.dec * int * int -> unit
    val printStrexp : Modules.env -> BareAbsyn.strexp * int * int -> unit
end

structure PrintAbsyn: PRINTABSYN = struct

open BareAbsyn Access PrintUtil PrintType PrintBasics ErrorMsg Tuples
open Fixity Variables Types Modules

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
  | isTUPLEexp (MARKexp(a,_,_)) = isTUPLEexp a
  | isTUPLEexp _ = false

fun lookFIX (env,sym) =
    ModuleUtil.lookFIX (env,Symbol.fixSymbol(Symbol.name sym))

fun prunemark (MARKexp(a,_,_)) = prunemark a | prunemark x = x

fun printPat env =
  let
    fun printPat' (_,0) = print "<pat>"
      | printPat' (VARpat v,_) = printVar v
      | printPat' (WILDpat,_) = print "_"
      | printPat' (INTpat i,_) = print i
      | printPat' (REALpat r,_) = print r
      | printPat' (STRINGpat s,_) = pr_mlstr s
      | printPat' (LAYEREDpat (v,p),d) = (printPat'(v,d); print " as "; printPat'(p,d-1))
      | printPat' (r as RECORDpat{fields,flex,...},d) =
	let val (a,b) =
	    if isTUPLEpat r
	    then (("(", ",", ")"), (fn (sym,pat) => printPat'(pat,d-1)))
	    else (("{", ",", (if flex then ",...}" else "}")),
		  (fn (sym,pat) => (printSym sym; print "="; printPat'(pat,d-1))))
	in printClosedSequence a b fields
	end
      | printPat' (CONpat e,_) = printDcon e
      | printPat' (p as APPpat _, d) =
	    let val noparen = INfix(0,0)
	    in  printDconPat env (p,noparen,noparen,d)
	    end
      | printPat' (CONSTRAINTpat (p,t),d) = (printPat'(p,d-1); print " : "; printType std_out env t)
  in printPat'
  end

and printDconPat env = 
  let 
    fun printDconPat'(_,_,_,0) = print "<pat>"
      | printDconPat'(CONpat(DATACON{name,...}),l:fixity,r:fixity,_) = printSym name
      | printDconPat'(CONSTRAINTpat(p,t),l,r,d) =
	    (print "("; printPat env (p,d-1); print " : "; printType std_out env t; print ")")
      | printDconPat'(LAYEREDpat(v,p),l,r,d) =
	    (print "("; printPat env (v,d); print " as "; printPat env (p,d-1); print ")")
      | printDconPat'(APPpat(DATACON{name,...},p),l,r,d) =
	let val dname = Symbol.name name
	    val fixity = lookFIX(env,name)
	    fun prdcon() =
		case (fixity,isTUPLEpat p,p)
		  of (INfix _,true,RECORDpat{fields=[(_,pl),(_,pr)],...}) =>
			     (printDconPat'(pl,NONfix,fixity,d-1);
			      print " "; print dname; print " ";
			      printDconPat'(pr,fixity,NONfix,d-1))
		    | _ => (print dname; print " "; printDconPat'(p,NONfix,NONfix,d-1))
	in  case(l,r,fixity) of
		  (NONfix,NONfix,_) => (print "("; prdcon(); print ")")
		| (INfix _,INfix _,_) => prdcon()
		| (_,_,NONfix) => prdcon()
		| (INfix(_,p1),_,INfix(p2,_)) => if p1 >= p2
						 then (print "("; prdcon(); print ")")
						 else prdcon()
		| (_,INfix(p1,_),INfix(_,p2)) => if p1 > p2
						 then (print "("; prdcon(); print ")")
						 else prdcon()
	end
	| printDconPat' (p,_,_,d) = printPat env (p,d)
  in printDconPat'
  end

fun trim [x] = nil | trim (a::b) = a::trim b

fun printExp env =
  let
    fun printExp'(_,_,0) = print "<exp>"
      | printExp'(VARexp(ref var),_,_) = printVar var
      | printExp'(CONexp(con),_,_) = printDcon con
      | printExp'(INTexp i,_,_) = print i
      | printExp'(REALexp r,_,_) = print r
      | printExp'(STRINGexp s,_,_) = pr_mlstr s
      | printExp'(r as RECORDexp fields,ind,d) =
	let val (a,b) =
	    if isTUPLEexp r
	    then (("(", ",", ")"), (fn(_,exp)=>printExp'(exp,ind+1,d-1)))
	    else (("{", ",", "}"),
		  (fn (LABEL{name,...},exp) =>
		      (printSym name; print "="; printExp'(exp,ind+1,d))))
	in printClosedSequence a b fields
	end
      | printExp'(SEQexp exps,ind,d) =
	  printClosedSequence ("(", ";", ")") (fn exp => printExp'(exp,ind+1,d-1)) exps
      | printExp'(e as APPexp _,ind,d) = let val noparen = INfix(0,0)
					in  printAppExp env (e,noparen,noparen,ind,d)
					end
      | printExp'(CONSTRAINTexp(e, t),ind,d) =
	  (print "("; printExp'(e,ind,d); print ":"; printType std_out env t; print ")")
      | printExp'(HANDLEexp(exp, HANDLER(FNexp rules)),ind,d) =
	  (printExp'(exp,ind,d-1); nlindent(ind); print "handle ";
	   printvseq(ind+5) "| " (fn r => printRule env (r,ind+7,d-1)) rules)
      | printExp'(RAISEexp exp,ind,d) = (print "raise "; printExp'(exp,ind+6,d-1))
      | printExp'(LETexp(dec, exp),ind,d) =
	  (print "let "; printDec env (dec,ind+4,d-1); nlindent(ind);
	   print " in "; printExp'(exp,ind+4,d-1); nlindent(ind);
	   print "end")
      | printExp'(CASEexp(exp, rules),ind,d) =
	  (print "(case "; printExp'(exp,ind+5,d-1); nlindent(ind+3);
	   print "of "; printvseq (ind+4) "| " (fn r => printRule env (r,ind+4,d-1)) (trim rules);
	   print ")")
      | printExp'(FNexp rules,ind,d) =
	  (print "(fn "; printvseq (ind+1) "| " (fn r => printRule env (r,ind+3,d-1)) (trim rules);
	   print ")")
      | printExp'(MARKexp (e,_,_),ind,d) = printExp'(e,ind,d)
  in printExp'
  end

and printAppExp _ (_,_,_,_,0) = print "<exp>"
  | printAppExp env arg =
    let fun fixityprint(name,e,l,r,ind,d) =
	    let val dname = formatQid name
		val fixity = case name of [id] => lookFIX(env,id)
					| _ => NONfix
		fun pr() =
		    case (fixity,isTUPLEexp e,prunemark e)
		      of (INfix _,true,RECORDexp[(_,pl),(_,pr)]) =>
				 (printAppExp env (pl,NONfix,fixity,ind,d-1);
				  print " "; print dname; print " ";
				  printAppExp env (pr,fixity,NONfix,ind+2,d-1))
			| _ => (print dname; print " ";
			        printAppExp env (e,NONfix,NONfix,ind+2,d-1))
	    in  case(l,r,fixity) of
		      (NONfix,NONfix,_) => (print "("; pr(); print ")")
		    | (INfix _,INfix _,_) => pr()
		    | (_,_,NONfix) => pr()
		    | (INfix(_,p1),_,INfix(p2,_)) =>
				if p1 >= p2 then (print "("; pr(); print ")")
				else pr()
		    | (_,INfix(p1,_),INfix(_,p2)) =>
				if p1 > p2 then (print "("; pr(); print ")")
				else pr()
	    end
	fun appPrint(_,_,_,_,0) = print "#"
	  | appPrint(CONSTRAINTexp(e,t),l,r,ind,d) =
	      (print "("; printExp env (e,ind+1,d-1);
	       print " : "; printType std_out env t; print ")")
	  | appPrint(APPexp(CONexp(DATACON{name,...}),e),l,r,ind,d) =
		fixityprint([name],e,l,r,ind,d)
	  | appPrint(APPexp(VARexp(ref(VALvar{name,...})),e),l,r,ind,d) =
		fixityprint(name,e,l,r,ind,d)
	  | appPrint(APPexp(VARexp(ref(OVLDvar{name,...})),e),l,r,ind,d) =
		fixityprint([name],e,l,r,ind,d)
	  | appPrint(APPexp(app as APPexp _,rand),NONfix,NONfix,ind,d) =
		let val yesparen = INfix(0,100000000) (* a hack *)
		in  print "("; appPrint(app,yesparen,NONfix,ind+1,d-1);
		    print " ";
		    appPrint(rand,NONfix,NONfix,ind+2,d-1); print ")"
		end
	  | appPrint(APPexp(app as APPexp _,rand),l,r,ind,d) =
		let val yesparen = INfix(0,100000000) (* a hack *)
		in  appPrint(app,yesparen,NONfix,ind+1,d-1);
		    print " ";
		    appPrint(rand,NONfix,NONfix,ind+2,d-1)
		end
	  | appPrint(APPexp(rator,rand),_,_,ind,d) =
		(printExp env (rator,ind,d-1); print " "; printExp env (rand,ind+2,d-1))
	  | appPrint(MARKexp(e,_,_),l,r,ind,d) = appPrint(e,l,r,ind,d)
	  | appPrint (e,_,_,ind,d) = printExp env (e,ind,d)
    in  appPrint arg
    end

and printRule env (RULE(pat,exp),ind,d) =
    if d>0
    then (printPat env (pat,d-1); print " => "; printExp env (exp,ind+2,d-1))
    else print "<rule>"

and printVB env (VB{pat,exp,...},ind,d) =
    if d>0
    then (printPat env (pat,d-1); print " = "; printExp env (exp,ind+2,d-1))
    else print "<binding>"

and printRVB env (RVB{var,exp,...},ind,d) = 
    if d>0
    then (printVar var; print " = "; printExp env (exp,ind+2,d-1))
    else print "<rec binding>"

and printDec env =
  let 
    fun printDec'(_,_,0) = print "<dec>"
      | printDec'(VALdec vbs,ind,d) =
	  (print "val "; printvseq ind "and " (fn vb => printVB env (vb,ind+4,d-1)) vbs)
      | printDec'(VALRECdec rvbs,ind,d) =
	  (print "val rec "; 
	   printvseq (ind+4) "and " (fn rvb => printRVB env (rvb,ind+8,d-1)) rvbs)
      | printDec'(TYPEdec tbs,ind,d) =
	  (print "type ";
	   printvseq ind " and "
	     (fn (TB{tyc=DEFtyc{path=name::_, tyfun=TYFUN{arity,...},...},def}) =>
		 (case arity
		    of 0 => ()
		     | 1 => (print "'a ")
		     | n => (printTuple print (typeFormals n); print " ");
		  printSym name; print " = "; printType std_out env def)
	       | _ => impossible "printabsyn.398")
	     tbs)
      | printDec'(DATATYPEdec{datatycs,withtycs},ind,d) =
	  (print "datatype ";
	   printvseq (ind+5) "and "
	     (fn GENtyc{path=name::_, arity, kind=ref(DATAtyc dcons),...} =>
		 (case arity
		    of 0 => ()
		     | 1 => (print "'a ")
		     | n => (printTuple print (typeFormals n); print " ");
		  printSym name; print " = ";
		  printSequence " | " (fn (DATACON{name,...}) => printSym name) dcons)
	       | _ => impossible "printabsyn.8")
	     datatycs;
	   nlindent(ind); print "with"; printDec'(TYPEdec withtycs,ind+4,d-1))
      | printDec'(ABSTYPEdec _,ind,d) = print "abstype"
      | printDec'(EXCEPTIONdec ebs,ind,d) =
	  (print "exception ";
	   printvseq (ind+6) "and "
	     (fn (EBgen{exn=DATACON{name,...},etype}) =>
		   (printSym name;
		    case etype of NONE => ()
				| SOME ty' => (print " of "; printType std_out env ty'))
	       | (EBdef{exn=DATACON{name,...},edef=DATACON{name=dname,...}}) =>
		   (printSym name; print "="; printSym dname))
	     ebs)
      | printDec'(STRdec sbs,ind,d) =
	  (print "structure ";
	   printvseq ind "and "
	     (fn (STRB{strvar=STRvar{access,name,...},def,...}) =>
		 (printSym name; printAccess access; print " = ";
		  nlindent(ind+4); printStrexp env (def,ind+4,d-1)))
	     sbs)
      | printDec'(ABSdec sbs,ind,d) =
	  (print "abstraction ";
	   printvseq ind "and "
	     (fn (STRB{strvar=STRvar{access,name,...},def,...}) =>
		 (printSym name; printAccess access; print " = ";
		  nlindent(ind+4); printStrexp env (def,ind+4,d-1)))
	     sbs)
      | printDec'(FCTdec fbs,ind,d) =
	  (print "functor ";
	   printvseq ind "and "
	     (fn (FCTB{fctvar=FCTvar{access,name=fname,...},
		       param=STRvar{name=pname,...},def,...}) =>
		 (printSym fname; printAccess access; print " ("; 
		    printSym pname; print ") = "; nlindent(ind+4);
		    printStrexp env (def,ind+4,d-1)))
	     fbs)
      | printDec'(SIGdec sigvars,ind,d) =
	  printvseq ind ""
	    (fn SIGvar{name,...} => (print "signature "; printSym name))
	    sigvars
      | printDec'(LOCALdec(inner,outer),ind,d) =
	  (print "local"; nlindent(ind+3);
	   printDec'(inner,ind+3,d-1); nlindent(ind);
	   print "in ";
	   printDec'(outer,ind+3,d-1); nlindent(ind);
	   print "end")
      | printDec'(SEQdec decs,ind,d) =
	  printvseq ind "" (fn dec => printDec'(dec,ind,d)) decs
      | printDec'(FIXdec {fixity,ops},ind,d) =
	  (case fixity of
	     NONfix => print "nonfix "
	   | INfix (i,_) => 
	         (if i mod 2 = 0 then 
	            print "infix "
		  else print "infixr ";
	          if i div 2 > 0 then
		    (print (i div 2);
		     print " ")
		  else ());
	   printSequence " " printSym ops)
      | printDec'(OVLDdec ovldvar,ind,d) =
	  (print "overload "; printVar ovldvar)
      | printDec'(OPENdec strVars,ind,d) =
	  (print "open ";
	   printSequence " " (fn STRvar{name,...} => printSym name) strVars)
      | printDec'(IMPORTdec _,_,_) = print "printDec' gives up: IMPORT in abstract syntax"
      | printDec'(MARKdec(dec,_,_),ind,d) = printDec'(dec,ind,d)
  in printDec'
  end

and printStrexp env =
  let 
    fun printStrexp'(_,_,0) = print "<strexp>"
      | printStrexp'(VARstr(STRvar{access,name,...}),ind,d) = 
	  printSym name
      | printStrexp'(STRUCTstr{body,...},ind,d) =
	  (print "struct"; nlindent(ind+2);
	   printvseq (ind+2) "" (fn dec => printDec env (dec,ind+2,d-1)) body;
	   nlindent(ind); print "end")
      | printStrexp'(APPstr{oper=FCTvar{name,...}, argexp,...},ind,d) =
	  (printSym name; print"(";
	   printStrexp'(argexp,ind+4,d-1);
	   print")")
      | printStrexp'(LETstr(dec,body),ind,d) =
	  (print "let "; printDec env (dec,ind+4,d-1); nlindent(ind);
	   print " in "; printStrexp'(body,ind+4,d-1); nlindent(ind);
	   print "end")
      | printStrexp'(MARKstr(body,_,_),ind,d) = printStrexp'(body,ind,d)
  in printStrexp'
  end

end (* structure PrintAbsyn *)

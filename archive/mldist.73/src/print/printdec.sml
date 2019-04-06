(* Copyright 1989 by AT&T Bell Laboratories *)
(* printdec.sml *)

(* functor PrintDec (strucure Env:ENV
		  structure PrintType:PRINTTYPE
		  structure PrintBasics:PRINTBASICS
		    sharing Env = PrintType.Env = PrintBasics.Env) : PRINTDEC =
struct 

structure Env = Env *)

structure PrintDec : PRINTDEC =
struct 

type object = System.Unsafe.object

open Types Variables Modules Fixity BareAbsyn PrintUtil PrintType PrintVal Access
val printType = printType std_out

val signatures = System.Control.Print.signatures
val printDepth = System.Control.Print.printDepth

fun printFormals 0 = ()
  | printFormals 1 = (print "'a ")
  | printFormals n = (PrintBasics.printTuple (fn s => (print "'"; print s)) (typeFormals n);
		      print " ")

fun printDec env dec =
 let val dec = (* pruneDec *) dec
  in fn lookup => 
    let fun printVb (VB{pat,...}) =
	    let fun printBind(pat) =
		    case pat
		      of VARpat(VALvar{name=[n],access=PATH[lv],typ=ref ty}) => 
			   (print "val "; printSym n; print " = ";
			    printVal env (lookup lv, ty, !printDepth);
			    print " : "; printType env ty;
			    newline())
		       | RECORDpat{pats=ref pl,...} => app printBind pl
		       | APPpat(_,pat) => printBind pat
		       | CONSTRAINTpat(pat,_) => printBind pat
		       | LAYEREDpat(pat1,pat2) => (printBind pat1; printBind pat2)
		       | _ => ()
	     in printBind pat
	    end

	and printRvb (RVB{var=VALvar{name=[n],access=PATH[lv],typ},...}) = 
	    (print "val "; printSym n; print " = ";
	     printVal env (lookup lv, !typ, !printDepth);
	     print " : "; printType env (!typ); newline())

	and printTb(TB{tyc=DEFtyc{path=name::_, tyfun=TYFUN{arity,...}},def}) =
	    (print "type "; 
	     printFormals arity; print " ";
	     printSym name; print " = "; printType env def; newline())

	and printAbsTyc(GENtyc{path=name::_, arity, eq, kind=ref(ABStyc _), ...}) =
	    (print(if (!eq=YES) then "eqtype" else "type"); 
	     printFormals arity; print " ";
	     printSym name; newline())

	and printDataTyc(GENtyc{path=name::_,arity,kind=ref(DATAtyc dcons),...}) =
	    (print "datatype ";
	     printFormals arity; print " ";
	     printSym name; newline();
	     app (fn DATACON{name,typ,...} => 
		     (print "con "; printSym name; print " : ";
		      printType env typ; newline()))
		 dcons)

	and printEb(EBgen{exn=DATACON{name,...},etype}) =
	      (print "exception "; printSym name;
	       case etype
		 of NONE => ()
		  | SOME ty' => 
		      if BasicTypes.isArrowType ty'
		      then (print " of "; printType env (BasicTypes.domain ty'))
		      else ();
	       newline())
	  | printEb(EBdef{exn=DATACON{name,...},edef=DATACON{name=dname,...}}) =
	      (print "exception "; printSym name; print " = "; printSym dname;
	       newline())

	and printStrb isAbs (STRB{strvar,...}) =
	    PrintBasics.printStructureVar(env,strvar,0,!signatures) (* isAbs strvar *)

	and printFctb(FCTB{fctvar,...}) = 
	    PrintBasics.printFunctorVar(env,fctvar,0,!signatures)

        and printFixity{fixity,ops} =
	    (print (Fixity.fixityToString fixity);
	     printSequence " " printSym ops;
	     newline())

	and printOpen(strvl) =  
	    (print "open ";
	     printSequence " " (fn STRvar{name,...} => printSym name) strvl;
	     newline())

        and printDec0 dec =
	case (resetPrintType(); dec)
	  of VALdec vbs => app printVb vbs
	   | VALRECdec rvbs => app printRvb rvbs
	   | TYPEdec tbs => app printTb tbs
	   | DATATYPEdec{datatycs,withtycs} =>
	       (app printDataTyc datatycs; app printTb withtycs)
	   | ABSTYPEdec{abstycs,withtycs,body} =>
	       (app printAbsTyc abstycs;
		app printTb withtycs;
		printDec0 body)
	   | EXCEPTIONdec ebs => app printEb ebs
	   | STRdec strbs => app (printStrb false) strbs
	   | ABSdec strbs => app (printStrb true) strbs
	   | FCTdec fctbs => app printFctb fctbs
	   | SIGdec sigvars => app (fn s => PrintBasics.printSignatureVar(env,s,0,!signatures)) sigvars
	   | LOCALdec(decIn,decOut) => printDec0 decOut
	   | SEQdec decs => app printDec0 decs
	   | FIXdec fixd => printFixity fixd
	   | OVLDdec _ => print "overload"
	   | OPENdec strvs => printOpen strvs
	   | IMPORTdec _ => ErrorMsg.impossible "printDec(IMPORT)"
	   | MARKdec(dec,a,b) => printDec0 dec
   in printDec0 dec
  end
 end

fun printBindingTbl (tbl: Modules.env)=
   (let val _ = resetPrintType(); 
	val bindlist = ref []
	val _ = Env.app (fn x => bindlist:= x :: (!bindlist)) tbl
	val binders = !bindlist
        fun printBinding(FCTbind(FCTvar{name,...})) =
	    (print "functor "; printSym name; newline())
	  | printBinding(SIGbind(SIGvar{name,...})) =
	    (print "signature "; printSym name; newline())
	  | printBinding _ = print "<other binding>\n"
     in app printBinding (map (fn (_,b)=> b) binders)
    end)

end (* structure PrintDec *)

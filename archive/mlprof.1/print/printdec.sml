(* printdec.sml *)

structure PrintDec : PRINTDEC =
struct

structure BareAbsyn = BareAbsyn
type object = System.object

open Basics BareAbsyn PrintUtil PrintBasics PrintType PrintVal Access

val printDepth = System.Control.Print.printDepth

fun printDec lookup =
    let fun printDec0 dec =
	case (resetPrintType(); dec)
	  of VALdec vbl => app printVb vbl
	   | VALRECdec rvbl => app printRvb rvbl
	   | TYPEdec tycl => app printTyc tycl
	   | DATATYPEdec{datatycs,withtycs} =>
	       (app printDataTyc datatycs; app printTyc withtycs)
	   | ABSTYPEdec{abstycs,withtycs,body} =>
	       (app printAbsTyc abstycs;
		app printTyc withtycs;
		printDec0 body)
	   | EXCEPTIONdec ebl => app printEb ebl
	   | STRdec strbs => app printStrb strbs
	   | FCTdec fctbs => app printFctb fctbs
	   | SIGdec _ => 
	       (PrintAbsyn.printDec(dec,0,!System.Control.Print.printDepth);
	        newline())
	   | LOCALdec(decIn,decOut) => printDec0 decOut
	   | SEQdec decl => app printDec0 decl
	   | OPENdec strvl => printOpen strvl

	and printVb (VB{pat,...}) =
	    let fun printBind(pat) =
		    case pat
		      of VARpat(VALvar{name,access=LVAR lv,vtype=ref ty}) => 
			   (print "val "; printSym name; print " = ";
			    printVal(lookup lv, ty, !printDepth);
			    print " : "; printType ty;
			    newline())
		       | RECORDpat{pats=ref pl,...} => app printBind pl
		       | APPpat(_,pat) => printBind pat
		       | CONSTRAINTpat(pat,_) => printBind pat
		       | LAYEREDpat(pat1,pat2) => (printBind pat1; printBind pat2)
		       | _ => ()
	     in printBind pat
	    end

	and printRvb(RVB{var=VALvar{name,access=LVAR lv,vtype=ref ty},...}) = 
	    (print "val "; printSym name; print " = ";
	     printVal(lookup lv, ty, !printDepth);
	     print " : "; printType ty; newline())

	and printTyc(TYCON{name,kind=DEFtyc(TYFUN{arity,body}),...}) =
	    (print "type "; 
	     case arity
	       of 0 => ()
		| 1 => (prstr "'a ")
		| n => (printTuple prstr (typeFormals n); prstr " ");
	     printSym name; prstr " = "; printType body; newline())

	and printAbsTyc(TYCON{name,kind=ABStyc,...}) =
	    (print "type "; printSym name; newline())

	and printDataTyc(TYCON{name,arity,kind=DATAtyc dcons,...}) =
	    (print "datatype ";
	     case arity
		of 0 => ()
		 | 1 => (prstr "'a ")
		 | n => (printTuple prstr (typeFormals n); prstr " ");
	     printSym name; newline();
	     app (fn DATACON{name,typ,...} => 
		     (print "con "; printSym name; print " : ";
		      printType typ; newline()))
		 (!dcons))

	and printEb(EB{exn=DATACON{name,typ,...},...}) =
	    (print "exception "; printSym name; print " : "; printType typ;
	     newline())

	and printStrb(STRB{strvar=STRvar{name,...},...}) =
	    (print "structure "; printSym name; print " : <sig>\n"; ())

	and printFctb(FCTB{fctvar=FCTvar{name,...},...}) =
	    (print "functor "; printSym name; print " : <sig>\n"; ())

	and printOpen(strvl) =
	    (print "open ";
	     printSequence " " (fn STRvar{name,...} => printSym name) strvl;
	     newline())

     in printDec0
    end (* fun printDec *)

end  (* structure PrintDec *)

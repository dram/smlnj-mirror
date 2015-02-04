(* printdec.sml *)

structure PrintDec : PRINTDEC =
struct

structure BareAbsyn = BareAbsyn
type object = System.object

open Basics BareAbsyn PrintUtil PrintType PrintVal  Access

val printDepth = ref 5

fun printDec lookup =
 let fun printDec0 dec =
    case dec
      of VALdec vbl => app printVb vbl
       | VALRECdec rvbl => app printRvb rvbl
       | TYPEdec tycl => app printTyc tycl
       | DATATYPEdec tycl => app printDataTyc tycl
       | EXCEPTIONdec ebl => app printEb ebl
       | STRdec strb => printStrb strb
       | FCTdec fctb => printFctb fctb
       | SIGdec _ => (PrintAbsyn.printDec dec; newline())
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

and printTyc(TYPEtyc{name,params,def,...}) =
    (print "type "; printSequence "," printTyvar params;
     printSym name; print " = "; printType def; newline())

and printDataTyc(DATAtyc{name,params,dcons,...}) =
    (print "datatype "; printSequence "," printTyvar params;
     printSym name; newline();
     app (fn DATACON{name,vtype,...} => 
	     (print "con "; printSym name; print " : ";
	      printType vtype; newline()))
	 (!dcons))

and printEb(EB{exn=DATACON{name,vtype,...},...}) =
    (print "exception "; printSym name; print " : "; printType vtype;
     newline())

and printStrb(STRB{strvar=STRvar{name,...},...}) =
    (print "structure "; printSym name; print " : <sig?>\n"; ())

and printFctb(FCTB{fctvar=FCTvar{name,...},...}) =
    (print "functor "; printSym name; print " : <sig?>\n"; ())

and printOpen(strvl) =
    (print "open ";
     printSequence " " (fn STRvar{name,...} => printSym name) strvl;
     newline())

  in printDec0
 end

end

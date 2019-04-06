(* printdec.sml *)

structure PrintDec : PRINTDEC =
struct

structure BareAbsyn = BareAbsyn
type object = System.object

open Basics BareAbsyn PrintUtil PrintBasics PrintType PrintVal Access

val printDepth = System.Control.Print.printDepth

val sortBinders = Sort.sort EnvAccess.binderGt

fun printFormals 0 = ()
  | printFormals 1 = (prstr "'a "; prstr " ")
  | printFormals n = (printTuple (fn s => (prstr "'"; prstr s)) (typeFormals n);
		      prstr " ")

fun printDec lookup =
    let fun printDec0 dec =
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
	   | STRdec strbs => app printStrb strbs
	   | FCTdec fctbs => app printFctb fctbs
	   | SIGdec sigvars => app printSigVar sigvars
	   | LOCALdec(decIn,decOut) => printDec0 decOut
	   | SEQdec decs => app printDec0 decs
	   | OPENdec strvs => printOpen strvs

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

	and printTb(TB{tyc=ref(TYCON{path=name::_, arity, ...}),def}) =
	    (print "type "; 
	     printFormals arity; prstr " ";
	     printSym name; prstr " = "; printType def; newline())

	and printAbsTyc(ref(TYCON{path=name::_, arity, eq, kind=ABStyc, ...})) =
	    (print(if (!eq=YES) then "eqtype" else "type"); 
	     printFormals arity; prstr " ";
	     printSym name; newline())

	and printDataTyc(ref(TYCON{path=name::_,arity,kind=DATAtyc dcons,...})) =
	    (print "datatype ";
	     printFormals arity; prstr " ";
	     printSym name; newline();
	     app (fn DATACON{name,typ,...} => 
		     (print "con "; printSym name; print " : ";
		      printType typ; newline()))
		 dcons)

	and printEb(EBgen{exn=DATACON{name,...},etype}) =
	      (print "exception "; printSym name;
	       case etype
		 of NONE => ()
		  | SOME ty' => 
		      if BasicTypes.isArrowType ty'
		      then (prstr " of "; printType(BasicTypes.domain ty'))
		      else ();
	       newline())
	  | printEb(EBdef{exn=DATACON{name,...},edef=DATACON{name=dname,...}}) =
	      (print "exception "; printSym name; prstr " = "; printSym dname;
	       newline())

	and printSigVar(SIGvar{name,binding}) =
	    (prstr "signature "; printSym name;
	     if !System.Control.Print.signatures
	     then (prstr " =\n  sig"; printStr binding; prstr "\n  end\n")
	     else newline())

	and printStrVar(STRvar{name,binding,...}) =
	    (prstr "structure "; printSym name;
	     if !System.Control.Print.signatures
	     then (prstr " :\n  sig"; printStr binding; prstr "\n  end\n")
	     else newline())

	and printStr(str as STRstr{table,env,kind,...}) =
	    let val tInC = case kind
			    of STRkind _ => (fn t => TypesUtil.typeInContext(t,env))
			     | SIGkind _ =>  TypesUtil.printableType str
		fun printBinder(_,VARbind(VALvar{name,vtype,...})) = 
		     (nlindent 4; prstr "val ";
		      printSym name; prstr " : ";
		      printType(tInC(!vtype)))
		  | printBinder(_,CONbind(DATACON{name,typ,rep=VARIABLE _,...})) = 
		     (nlindent 4; prstr "exception "; printSym name;
		      if BasicTypes.isUnitTy(typ)
		      then ()
		      else (prstr " of "; printType(tInC typ)))
		  | printBinder(_,TYCbind(ref(tyc))) = 
		      let val TYCON{path=name::_,kind,arity,eq,...} =
			      TypesUtil.tyconInContext env tyc
		       in nlindent 4;
			  case kind
			    of DATAtyc dcons =>
				 (prstr "datatype ";
				  printFormals arity;
				  printSym name; nlindent(6); prstr "con ";
				  printvseq (6) "con "
				      (fn DATACON{name,typ,...} => 
					  (printSym name; prstr " : ";
					   printType(tInC typ)))
				      dcons)
			     | _ =>
				 (if (!eq=YES)
				  then prstr "eqtype "
				  else prstr "type ";
				  printFormals arity;
				  printSym(name))
		      end
		  | printBinder(_,STRbind(STRvar{name,binding,...})) = 
		      (nlindent 4; prstr "structure ";
		       printSym name; prstr " : sig...end")
		  | printBinder(id,FIXbind(INfix(l,r))) =
		      (nlindent 4;
		       prstr "infix "; print l; prstr " "; print r; prstr " ";
		       printSym id)
		  | printBinder _ = ()
		val bindlist = ref nil
		val _ = Table.app (table,fn x => bindlist := x ::(!bindlist))
		val binders = sortBinders(!bindlist)
		val m = Env.mark() before Env.openOld((nil,env),table)
	     in app printBinder binders;
	        Env.close m
	    end

	and printStrb(STRB{strvar,...}) =
	    printStrVar strvar

	and printFctb(FCTB{fctvar=FCTvar{name,...},...}) =
	    (print "functor "; printSym name; prstr " : <sig>\n")

	(* not used, because of special top-level open *)
	and printOpen(strvl) =  
	    (print "open ";
	     printSequence " " (fn STRvar{name,...} => printSym name) strvl;
	     newline())

     in printDec0
    end (* fun printDec *)

end  (* structure PrintDec *)

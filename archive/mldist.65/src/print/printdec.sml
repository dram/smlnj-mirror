(* Copyright 1989 by AT&T Bell Laboratories *)
(* printdec.sml *)

structure PrintDec : PRINTDEC =
struct

structure BareAbsyn = BareAbsyn
type object = System.Unsafe.object

open Basics BareAbsyn PrintUtil PrintBasics PrintType PrintVal Access

val printDepth = System.Control.Print.printDepth

val dummyStruct = STRUCTstr{body=nil,str=NULLstr,locations=nil}

val rec pruneDec = 
  fn VALdec vbl => VALdec(map (fn VB{pat,exp,tyvars}=>
			          VB{pat=pat,exp=INTexp 0,tyvars=tyvars}) vbl)
   | VALRECdec rvb =>
		VALRECdec(map(fn RVB{var,exp,resultty,tyvars}=>
				RVB{var=var,exp=INTexp 0,resultty=resultty,
					tyvars=tyvars}) rvb)
   | ABSTYPEdec{abstycs,withtycs,body}=>
	ABSTYPEdec{abstycs=abstycs,withtycs=withtycs,body=pruneDec body}
   | STRdec l => STRdec(map(fn STRB{strvar,...} =>
			STRB{strvar=strvar,def=dummyStruct,thin=NONE,
				constraint=NONE})  l)
   | ABSdec l => ABSdec(map(fn STRB{strvar,...} =>
			STRB{strvar=strvar,def=dummyStruct,thin=NONE,
				constraint=NONE})  l)
   | FCTdec l => FCTdec(map(fn FCTB{fctvar,param,...} =>
			FCTB{fctvar=fctvar,param=param,def=dummyStruct,
			thin=NONE,constraint=NONE})  l)
   | LOCALdec(a,b) => pruneDec b
   | SEQdec l => SEQdec(map pruneDec l)
   | MARKdec(d,_,_) => pruneDec d
   | d => d

val sortBinders = Sort.sort BuildMod.binderGt

fun printFormals 0 = ()
  | printFormals 1 = (print "'a ")
  | printFormals n = (printTuple (fn s => (print "'"; print s)) (typeFormals n);
		      print " ")

fun printDec dec =
 let val dec = pruneDec dec
  in fn lookup => 
    let fun printVb (VB{pat,...}) =
	    let fun printBind(pat) =
		    case pat
		      of VARpat(VALvar{name=[n],access=LVAR lv,typ=ref ty}) => 
			   (print "val "; printSym n; print " = ";
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

	and printRvb (RVB{var=VALvar{name=[n],access=LVAR lv,typ},...}) = 
	    (print "val "; printSym n; print " = ";
	     printVal(lookup lv, !typ, !printDepth);
	     print " : "; printType (!typ); newline())

	and printTb(TB{tyc=DEFtyc{path=name::_, tyfun=TYFUN{arity,...}},def}) =
	    (print "type "; 
	     printFormals arity; print " ";
	     printSym name; print " = "; printType def; newline())

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
		      printType typ; newline()))
		 dcons)

	and printEb(EBgen{exn=DATACON{name,...},etype}) =
	      (print "exception "; printSym name;
	       case etype
		 of NONE => ()
		  | SOME ty' => 
		      if BasicTypes.isArrowType ty'
		      then (print " of "; printType(BasicTypes.domain ty'))
		      else ();
	       newline())
	  | printEb(EBdef{exn=DATACON{name,...},edef=DATACON{name=dname,...}}) =
	      (print "exception "; printSym name; print " = "; printSym dname;
	       newline())

	and printSigVar(SIGvar{name,binding}) =
	    (print "signature "; printSym name;
	     if !System.Control.Print.signatures
	     then (print " =\n  sig"; printStr binding; print "\n  end\n")
	     else newline())

	and printStrVar(STRvar{name,binding,...}) =
	    (print "structure "; print(formatQid name);
	     if !System.Control.Print.signatures
	     then (print " :\n  sig"; printStr binding; print "\n  end\n")
	     else newline())

	and printStr(str as STRstr{table,env,kind,...}) =
	    let val tInC = case kind
			    of STRkind _ => (fn t => TypesUtil.typeInContext(t,env))
			     | SIGkind _ =>  TypesUtil.printableType str
		fun visibleDcons(tyc,dcons) =
		    let fun dconTyc(POLYty{tyfun=TYFUN{body,...},...},b) = 
								dconTyc(body,b)
			  | dconTyc(CONty(tyc,_),true) = tyc
			  | dconTyc(CONty(_,[_,CONty(tyc,_)]),false) = tyc
			  | dconTyc _ = ErrorMsg.impossible "printdec.dconTyc"
			fun find(DATACON{name,...}::rest) =
			    (let val dcon as DATACON{typ,const,...} =
				      EnvAccess.lookCONinTable(table,name)
			 	 val dcontyc = TypesUtil.tyconInContext env
				                 (dconTyc(typ,const))
			      in if TypesUtil.eqTycon(tyc,dcontyc)
				 then dcon :: find rest
				 else find rest
			     end handle Env.UnboundTable => find rest)
			  | find [] = []
		     in find dcons
		    end
		fun printBinder(VARbind(VALvar{name=[n],typ,...})) = 
		     (nlindent 4; print "val ";
		      printSym n; print " : ";
		      printType(tInC(!typ)))
		  | printBinder(CONbind(DATACON{name,typ,rep=VARIABLE _,...})) = 
		     (nlindent 4; print "exception "; printSym name;
		      if BasicTypes.isArrowType typ
		      then (print " of "; 
			    printType(tInC(BasicTypes.domain typ)))
		      else ())
		  | printBinder(TYCbind tyc) = 
		      let val tyc' = TypesUtil.tyconInContext env tyc
			  fun showit(arity, name) =
			      (if EqTypes.isEqTycon tyc'
			       then print "eqtype " 
			       else print "type ";
			       printFormals arity; printSym name)
		       in nlindent 4;
			  case tyc'
			   of GENtyc{path=name::_,arity,
				     kind=ref(DATAtyc dcons),...} =>
				      (case visibleDcons(tyc',dcons)
					of [] => showit(arity,name)
					 | visdcons =>
					    (print "datatype ";
					     printFormals arity;
					     printSym name; nlindent(6); 
					     print "con ";
					     printvseq (6) "con "
					       (fn DATACON{name,typ,...} => 
						 (printSym name; print " : ";
						  printType(tInC typ)))
					       visdcons;
					     if length visdcons < length dcons
					     then (nlindent(6);
						   print "... hidden cons")
					     else ()))
			    | GENtyc{path=name::_,arity,...} =>
					   showit(arity,name)
			    | DEFtyc{path=name::_,tyfun=TYFUN{arity,...},...}=>
					   showit(arity,name)
		      end
		  | printBinder(STRbind(STRvar{name=[n],binding,...})) = 
		      (nlindent 4; print "structure ";
		       printSym n; print " : sig...end")
		  | printBinder(FIXbind(FIXvar{name,binding=INfix(l,r)})) =
		      (nlindent 4;
		       print(if l > r then "infixr " else "infix ");
		       print(r div 2); print " "; printSym name)
		  | printBinder _ = ()
		val bindlist = ref nil
		val _ = IntStrMap.app (fn x => bindlist := x ::(!bindlist)) table
		val binders = sortBinders(!bindlist)
		val e = Env.current()
	     in Env.openOld({path=nil,strenv=env},table); (* affects printType *)
		app (fn (_,_,b) => printBinder b) binders;
	        Env.resetEnv e
	    end

	and printStrb(STRB{strvar,...}) =
	    printStrVar strvar

	and printFctb(FCTB{fctvar,...}) = 
	    printFunctorVar fctvar

        and printFunctorVar(FCTvar{name,...}) =
	    (print "functor "; printSym name; print " : <sig>\n")

	and printOpen(strvl) =  
	    (print "open ";
	     printSequence " " (fn STRvar{name,...} => print(formatQid name)) strvl;
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
	   | STRdec strbs => app printStrb strbs
	   | FCTdec fctbs => app printFctb fctbs
	   | SIGdec sigvars => app printSigVar sigvars
	   | LOCALdec(decIn,decOut) => printDec0 decOut
	   | SEQdec decs => app printDec0 decs
	   | OPENdec strvs => printOpen strvs
	   | IMPORTdec _ => ErrorMsg.impossible "printDec(IMPORT)"
	   | MARKdec(dec,a,b) => printDec0 dec

   in printDec0 dec
  end
 end

fun printBindingTbl(tbl: Basics.symtable)=
   (let val _ = resetPrintType(); 
	val bindlist = ref []
	val _ = IntStrMap.app (fn x => bindlist:= x :: (!bindlist)) tbl
	val binders = sortBinders (!bindlist)
        fun printBinding(FCTbind(FCTvar{name,...})) =
	    (print "functor "; printSym name; newline())
	  | printBinding(SIGbind(SIGvar{name,...})) =
	    (print "signature "; printSym name; newline())
	  | printBinding _ = print "<other binding>\n"
     in app printBinding (map (fn(i,s,b)=> b) binders)
    end)

end

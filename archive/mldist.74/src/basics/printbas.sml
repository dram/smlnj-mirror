(* 1989, 1990, 1991 by AT&T Bell Laboratories *)
(* printbasics.sml *)

signature PRINTBASICS = 
sig
    val printTuple: ('a -> unit) -> 'a list -> unit
    val printAccess: Access.access -> unit
    val printRep: Access.conrep -> unit
    val printDcon: Types.datacon -> unit
    val printVar: Variables.var -> unit
    val printSignature: Modules.env * Modules.Signature * int * int -> unit
    val printStructure: Modules.env * Modules.Structure * int * int  -> unit
    val printStructureVar: Modules.env * Modules.structureVar * int 
	                     * int -> unit
    val printSignatureVar: Modules.env * Modules.signatureVar * int
	                     * int -> unit 
    val printBinding: Modules.env * Modules.binding * int * int -> unit
    val printFunctor : Modules.env * Modules.Functor * int * int -> unit
    val printFunctorVar : Modules.env * Modules.functorVar * int * int -> unit
    val printStructureName : Modules.env * Modules.Structure -> unit
    val printTable : Modules.env * Modules.env * int * int *
	           Symbol.symbol list option -> unit
    val printDebugDcon : Modules.env -> Types.datacon -> unit
    val printDebugVar: Modules.env -> Variables.var -> unit
end

structure PrintBasics : PRINTBASICS = struct

open PrintUtil Access Variables Types Fixity Modules System.Control

val say = outputc std_out
val printType = PrintType.printType std_out
val printTycon = PrintType.printTycon std_out
fun printTyfun env (TYFUN{arity,body}) =
	(print "TYFUN({arity="; print arity; print ",body="; 
	 printType env body; print "})")

fun printArray (f : 'a -> unit, a : 'a array,indent) =
  let fun loop i = 
        let val elem = Array.sub(a,i)
	in tab indent; say (makestring i); say " : "; f elem;
	   newline(); loop (i+1)
	end
  in loop 0 handle Array.Subscript => ()
  end

val printTuple = printClosedSequence ("(", ",", ")")

fun printFormals 0 = ()
  | printFormals 1 = (print "'a ")
  | printFormals n = (printTuple (fn s => (print "'"; print s)) (PrintType.typeFormals n);
		      print " ")

fun printAccess a = (say " ["; say(Access.pr_access a); say "]")

fun printRep UNDECIDED = say "UNDECIDED"
  | printRep (TAGGED i) = (say "TAGGED["; print i; say "]")
  | printRep (CONSTANT i) = (say "CONSTANT["; print i; say "]")
  | printRep TRANSPARENT = say "TRANSPARENT"
  | printRep TRANSU = say "TRANSU"
  | printRep TRANSB = print "TRANSB"
  | printRep REF = print "REF"
  | printRep (VARIABLE a) = (print "VARIABLE["; printAccess a; print "]")
  | printRep (VARIABLEc a) = (print "VARIABLEc["; printAccess a; print "]")

fun printDcon (DATACON{name,rep=VARIABLE(access),...}) =
      (printSym(name); if !internals then printAccess access else ())
  | printDcon (DATACON{name,rep=VARIABLEc(access),...}) =
      (printSym(name); if !internals then printAccess access else ())
  | printDcon (DATACON{name,...}) = printSym(name)

fun printDebugDcon env (DATACON{name,rep,const,typ,sign}) =
    (print "DATACON{name="; printSym name; print ",const="; print const;
     print ",typ="; printType env typ; print ",rep="; printRep rep; 
     print ",sign=["; printvseq 0 "," (fn x=>printRep x) sign; print "]}")

(* dconTyc: get the range type of a data constructor *)

fun dconTyc(DATACON{typ,const,...}) =
    let fun f (POLYty{tyfun=TYFUN{body,...},...},b) = f (body,b)
	  | f (CONty(tyc,_),true) = tyc
	  | f (CONty(_,[_,CONty(tyc,_)]),false) = tyc
	  | f _ = ErrorMsg.impossible "printdec.dconTyc"
    in f (typ,const)
    end

fun printDatacon (env:Modules.env,DATACON{name,typ,...}) =
    (printSym name; print " : "; printType env typ)

fun printConBinding (DATACON{name,typ,rep=VARIABLE _,...},env,indent) =
	  (tab indent;
	   print "exception "; printSym name;
	   print " of "; 
	   printType env (BasicTypes.domain typ);
	   newline())
  | printConBinding (DATACON{name,typ,rep=VARIABLEc _,...},env,indent) =
	  (tab indent; print "exception "; printSym name; newline())
  | printConBinding (con,env,indent) = 
      let exception Hidden
	  val visibleDconTyc =
	    case dconTyc con
	    of RELtyc _ => true (* datacons are always visible in sigs *)
	     | tyc =>
	       (TypesUtil.equalTycon
		         (ModuleUtil.lookTYC (env,[TypesUtil.tycName tyc],
					      fn _ => raise Hidden),tyc)
		 handle Hidden => false)
       in
          (if !internals orelse not visibleDconTyc then
	       (tab indent;
		print "con ";
	        printDatacon(env,con);
		newline())
	   else ())
       end

fun printVar (VALvar {access,name,...}) =
      (print(formatQid name);
       if !internals then printAccess access else ())
  | printVar (OVLDvar {name,...}) = (printSym(name); print " : overloaded")
  | printVar (ERRORvar) = print "<ERRORvar>"

fun printDebugVar env (VALvar {access,name,typ}) = 
	(print "VALvar({access="; printAccess access;
 	 print ",name="; print (formatQid name); print ",typ=ref "; 
 	 printType env (!typ); print "})")
  | printDebugVar env (OVLDvar {name,options,scheme}) = 
	(print "OVLDvar({name="; printSym(name);
 	 print ",options=["; 
	(printvseq 0 "," (fn {indicator,variant} =>
			    (print "{indicator="; printType env indicator;
			     print ",variant ="; printDebugVar env variant; print "}"))
			  (!options));
 	 print "],scheme="; printTyfun env scheme; print "})")
  | printDebugVar _ (ERRORvar) = print "<ERRORvar>"

fun printStructureName (env,str) =
  let open ModuleUtil
      val path =
        case str
        of SIMPLE{path,...} =>  path
        | INSTANCE{path,...} => path
        | _ => ErrorMsg.impossible "PrintBasics.printStructureName"
      val look = fn (a,b) => case lookSTR(env,a,b)
	                 of STRvar{binding,...} => binding
  in print (findPath(path,str,eqOrigin,look))
  end

fun printVariable (env:Modules.env,VALvar{name,access,typ}) = 
      (print(formatQid name);
       if !internals then printAccess access else ();
       print " : "; printType env (!typ))
  | printVariable (env,OVLDvar {name,options=ref optl,
				scheme=TYFUN{body,...}}) =
    (printSym(name); print " : "; printType env body; print " as ";
     printSequence " " (fn {variant,...} =>
			     printVariable(env,variant)) optl)
  | printVariable (_,ERRORvar) = print "<ERRORvar>"

and printStructure(topenv,str,indent,depth) =
   let fun printSig bindings =
	            (nlindent indent;
		     say "sig\n";
		     printTable(topenv,
                                ModuleUtil.makeEnv(str,[]),
				indent+2,depth-1,bindings);
		     tab indent;
		     say "end\n")
   in case str
	    of SIMPLE {stamp,env=strenv,path} =>
	      (if depth <= 1 then say "...\n"
	       else if !internals 
	       then (tab indent;
		     print "SIMPLE Stamp= ";
		     say (Stamps.stampToString stamp);
		     nlindent indent;
		     say "Path= ";
		     prSymPath path;
		     nlindent indent;
		     say "Env=\n";
		     printTable(topenv,ModuleUtil.makeEnv(str,[]),indent+2,depth-1,NONE))
	       else printSig NONE)
         | INSTANCE {origin,sign,subStrs,types,path} =>
	   (if !internals then
	       (nlindent indent;
		print "INSTANCE ";
		nlindent(indent+2);
		print "Origin=";
		printStructure(topenv,origin,indent+4,depth-1);
		case sign
		of SIG{kind=TOP _,...} =>
		    (tab (indent+2);
		     say "Substructures:\n";
		     printArray (fn a =>
				    printStructure(topenv,a,indent+6,
						    depth-1),
				  subStrs,indent+4);
		     tab(indent+2);
		     say "Types:\n";
		     printArray (fn t => printTycon topenv t,types,indent+4))
	         | _ => ();
		 tab(indent+2);
                 say "Path= ";
		 prSymPath path;
		 nlindent(indent+2);
                 say "Sign=";
		 printSignature(topenv,sign,indent+4,depth-1))
            else case sign
	          of SIG{symbols,path,...} =>
	 	    let fun f () =
		        if depth <= 1 then say "...\n"
			  else (printSig (SOME symbols); say "\n")
                    in case path
		       of SOME p =>
		        ((if ModuleUtil.eqSign(sign,
			         ModuleUtil.lookSIG(topenv,p,
					           fn _ => raise Env.Unbound))
                            then (printSym p; say "\n")
		            else f()) handle Env.Unbound => f())
		        | NONE => f()
		    end
                  | ERROR_SIG => say "<error>\n")
	 | FORMAL{pos,spec,...} =>
            (if depth <= 1 then say "...\n"
	     else if !internals then
	       (nlindent indent;
		print "FORMAL";
		nlindent (indent+2);
		say "pos=";
		print pos;
		nlindent (indent+2);
		say "spec=";
	        printSignature(topenv,spec,indent+4,depth-1))
	     else  printSignature(topenv,spec,indent,depth-1))
	 | SELF stamp => 
	   (say "SELF "; say (Stamps.stampToString stamp); say "\n")
	 | ERROR_STR => say  "<error>\n"
	 | ABSFB_STR loc =>
	   (say "ABSFB_STR(";
	    case loc
	    of PARAM path =>
	         (say "PARAM "; prIntPath path)
             | SEQ index =>
	         (say "SEQ "; print index);
		  print ")\n")
    end (* end *)
        
 
and printStructureVar(env,STRvar{name,access,binding},indent,depth) =
    (tab indent;
     say "structure ";
     printSym name;
     if !internals then printAccess access else ();
     say" : ";
     printStructure(env,binding,indent+2,depth))

(* assumes newline may be needed before printing *)

and printSignature(env,sign,indent,depth) =
  if depth<=0 then say "...\n"
  else case sign
    of SIG {symbols,env=sigenv,kind,path,stamp} =>
     if !internals then
	(nlindent indent;
	 print "SIG";
	 nlindent (indent+2);
	 say "Path = ";
	 case path
	 of NONE => say "NONE"
	  | SOME p => printSym p;
	 nlindent (indent+2);
	 say "Bound symbols = "; printSequence "," printSym symbols;
	 nlindent (indent+2);
	 say "Kind=";
	 case kind
	 of TOP {strcount,typecount,slotcount,sConstraints,tConstraints} =>
	  let fun printConstraints (f,c) =
	      printArray (fn {internal,external} =>
		           (tab 2;
		            say "Coherence:\n";
		            tab(indent+10);
		            printSequence " = " prSymPath internal;
		            nlindent(indent+6);
		            print "Definitional:\n";
			    case external
			    of NONE => ()
			     | SOME c => f c),Array.arrayoflist c,indent+4)
              fun printDefnStructure s =
		   printStructure(env,s,indent+4,depth-1)
          in (nlindent (indent+4);
	      say "TOP";
	      nlindent (indent+6);
	      say "Strcount=";
	      print strcount;
	      nlindent (indent+6);
	      print "Typecount=";
	      print (makestring typecount);
	      nlindent (indent+6);
	      print "Slot count=";
	      print (makestring slotcount);
	      nlindent (indent+6);
	      print "Structure sharing constraints:\n";
	      printConstraints(printDefnStructure,sConstraints);
	      tab (indent+4);
	      print "Type sharing constraints:\n";
	      printConstraints (printTycon env,tConstraints))
            end
	   | EMBEDDED => print "EMBEDDED,";
	 tab(indent+2);
	 print "Stamp=";
	 print (Stamps.stampToString stamp);
	 nlindent(indent+2);
	 say "Env=\n";
         printTable(env,sigenv,indent+2,depth-1,SOME symbols))
     else (* !internals *)
        (nlindent indent;
	 say "sig\n";
	 printTable(env,sigenv,indent+2,depth-1,SOME symbols);
	 let fun printConstraints (name,f,c) =
	       (* if internal=nil, then all the constraints were definitional.
		  Thus there is no point in printing anything.*)
	     app (fn {internal=nil,...} => ()   
		   | {internal,external} =>
		 (tab(indent+2);
		  say "sharing ";
		  say name;
		  printSequence " = " prSymPath internal;
		  case external
		  of NONE =>()
		   | SOME c => (say " = "; f c);
		  newline())) c
         in case kind
	    of TOP {sConstraints,tConstraints,...} =>
		 (printConstraints("",fn s => printStructureName(env,s),
				   sConstraints);
		  printConstraints("type ",printTycon env,tConstraints))
	     | _ => ()
	 end;
	 tab indent;
	 say "end\n")
      | ERROR_SIG => say "<error>\n"

(* assumes no newline is needed before printing *)

and printSignatureVar(env:Modules.env,SIGvar{name,binding},indent,depth) =
    (tab indent;
     print "signature "; printSym name; print " = ";
     printSignature(env,binding,indent+2,depth))

and printFunctor(env,FCT({paramName,argument,body={tyseq,strseq,str}}),
		 indent,depth) =
    if depth <= 1 
	then say "...\n"
        else
	    (nlindent indent;
	     print "paramName = ";
	     print (Symbol.name paramName);
	     nlindent indent;
	     print "argument signature:";
	     printSignature(env,argument,indent+2,depth-1);
	     nlindent indent;
	     print "type sequence:";
	     printArray (fn t => printTycon env t,
			 Array.arrayoflist tyseq,indent+2);
             nlindent indent;
	     print "structure sequence:\n";
	     printArray (fn s => printStructure(env,s,indent+6,depth-1),
			 Array.arrayoflist strseq,indent+2);
	     tab indent;
	     print "str:\n";
	     printStructure(env,str,indent+2,depth-1))	     
  | printFunctor (_,ERROR_FCT,_,_) = say "<error functor>"

(* assumes no newline is needed before printing "functor ..." *)

and printFunctorVar(env,FCTvar{name,access,binding},indent,depth) =
     (tab indent;
      print "functor ";  print(Symbol.name name);
      if !internals then
	  (print " = ";
	   nlindent(indent+2);
	   print "access= ";
	   printAccess access;
	   nlindent(indent+2);
	   print "binding=\n";
	   printFunctor(env,binding,indent+4,depth-1))
      else say " : <sig>\n")

and printTycBind(env,tyc,indent) =
    let fun visibleDcons(tyc,dcons) =
	    let fun checkCON(CONbind c) = c
		  | checkCON _ = raise Env.Unbound
		fun find ((actual as DATACON{name,...}) :: rest) =
		    (let val found =
			  checkCON(ModuleUtil.lookVARCON(env,[name],
							 fn _ => raise Env.Unbound))
		      in if TypesUtil.eqTycon(dconTyc actual, dconTyc found)
			 then actual :: find rest
			 else find rest
			  (* ??? use of eqTycon may not be robust *)
		     end handle Env.Unbound => find rest)
		  | find [] = []
	     in find dcons
	    end
        val tyc' = case tyc
		     of FORMtyc{spec,...} => spec
		      | _ => tyc        
	fun showit(arity, name) =
	    (if EqTypes.isEqTycon tyc'
	     then print "eqtype " 
	     else print "type ";
	     printFormals arity; printSym name)
     in tab indent;
        if !internals then (say "type "; printTycon env tyc)
        else
	 (case tyc'
	    of GENtyc{path=name::_,arity,
		      kind=ref(DATAtyc dcons),...} =>
		 (case visibleDcons(tyc',dcons)
		    of [] => showit(arity,name)
		     | visdcons =>
			 (print "datatype ";
			  printFormals arity;
			  printSym name; nlindent(indent+2); 
			  print "con ";
			  printvseq (indent+2) "con "
			    (fn DATACON{name,typ,...} => 
			       (printSym name; print " : ";
				printType env typ))
				visdcons;
				if length visdcons < length dcons
				then (nlindent(6);
				      print "... hidden cons")
				else ()))
	     | GENtyc{path=name::_,arity,...} => showit(arity,name)
	     | DEFtyc{path=name::_,tyfun=TYFUN{arity,...},...} =>
		 showit(arity,name)
	     | tycon => (print "bogus tycon: ";
			 PrintType.printTycon std_out env tycon;
			 ErrorMsg.impossible "PrintBas.printBinder"));
        say "\n"
    end

and printBinding (env:Modules.env,binding,indent,depth) =
    case binding
    of VARbind var => (tab indent; print "val ";
		       printVariable(env,var); print "\n")
     | CONbind con => printConBinding(con,env,indent)
     | TYCbind tycon => (printTycBind(env,tycon,indent))
     | SIGbind binding => (printSignatureVar(env,binding,indent,depth))
     | STRbind binding => (printStructureVar(env,binding,indent,depth))
     | FCTbind binding => (printFunctorVar(env,binding,indent,depth))
     | FIXbind(FIXvar{name,binding}) =>
	    (tab indent; print (Fixity.fixityToString binding);
	     printSym name; newline())

(* printTable: prints an environment in the context of the top environment.
   The environment must either be for a signature or be absolute (i.e.
   all types and structures have been interpreted) *)

and printTable(topenv,env,indent,depth,boundsyms) =
  let val bindings = 
      case boundsyms
      of NONE => map #2 (ModuleUtil.sortEnvBindings env)
       | SOME l => map (fn x => Env.look(env,x)) l
      val printEnv = Env.atop(env,topenv)
   in app (fn binding => (printBinding(printEnv,binding,indent,depth)))
           bindings
   end

end (* PrintBasics *)

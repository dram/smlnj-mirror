(* dprintbas.sml *)

signature DPRINTBASICS = 
sig
    val printTuple: ('a -> unit) -> 'a list -> unit
    val printRep: Access.conrep -> unit
    val printDcon: Basics.env -> Basics.datacon -> unit
    val printVar: Basics.env -> Basics.var -> unit
    val printStr: Basics.Structure -> unit
    val printStrVar: Basics.structureVar -> unit
    val printBinding: Basics.env -> Basics.binding -> unit
    val printStructure: Basics.env -> Basics.Structure -> unit
end

(*functor DPrintBasics (PrintType:PRINTTYPE) : DPRINTBASICS = struct

structure Env = PrintType.Env *)

structure DPrintBasics : DPRINTBASICS = struct

local open PrintUtil Access Basics PrintType System.Control in

val printTuple = printClosedSequence ("(", ",", ")")

val printList = printClosedSequence ("[", ",", "]") (* printList not used here *)
		
fun printPath [n:int] = (print n; ())
  | printPath (n::p) = (printPath p; print "."; print n; ())
  | printPath [] = ()

fun printTyfun env (TYFUN{arity,body}) =
	(print "TYFUN({arity="; print arity; print ",body="; 
	 printType std_out env body; print "})")

fun printRep UNDECIDED = print "UNDECIDED"
  | printRep (TAGGED i) = (print "TAGGED["; print i; print "]")
  | printRep (CONSTANT i) = (print "CONSTANT["; print i; print "]")
  | printRep TRANSPARENT = print "TRANSPARENT"
  | printRep TRANSU = print "TRANSU"
  | printRep TRANSB = print "TRANSB"
  | printRep REF = print "REF"
  | printRep (VARIABLE a) =
      (print "VARIABLE["; PrintBasics.printAccess a; print "]")
  | printRep (VARIABLEc a) =
      (print "VARIABLEc["; PrintBasics.printAccess a; print "]")


fun printDcon env (DATACON{name,const,typ,rep,sign}) =
		(print "DATACON{name="; printSym name; print ",const="; print const;
		 print ",typ="; printType std_out env typ; print ",rep="; printRep rep; 
		 print ",sign=["; printvseq 0 "," (fn x=>printRep x) sign; print "]}")

fun printDatacon env (DATACON{name,typ,...}) =
    (printSym name; print " : "; printType std_out env typ) (* ???? *)

fun printExn env (DATACON{name,typ,...}) =
    (printSym name; print " : "; printType std_out env typ)

fun printVar env (VALvar {access,name,typ}) = 
	(print "VALvar({access="; PrintBasics.printAccess access;
 	 print ",name="; print (formatQid name); print ",typ=ref "; 
 	 printType std_out env (!typ); print "})")
  | printVar env (OVLDvar {name,options,scheme}) = 
	(print "OVLDvar({name="; printSym(name);
 	 print ",options=["; 
	(printvseq 0 "," (fn {indicator,variant} =>
			    (print "{indicator="; printType std_out env indicator;
			     print ",variant ="; printVar env variant; print "}"))
			  (!options));
 	 print "],scheme="; printTyfun env scheme; print "})")

fun printVariable env (VALvar{name,access,typ}) =  (* ???? *)
    (print (formatQid name); PrintBasics.printAccess access;
     print " : "; printType std_out env (!typ))
  | printVariable env (OVLDvar{name,...}) = 
    (printSym name; print " : overloaded")

fun printStr(STRstr _) = print "STRstr"
  | printStr(INDstr _) = print "INDstr"
  | printStr(SHRstr _) = print "SHRstr"
  | printStr(NULLstr) = print "NULLstr"

fun printStrVar(STRvar{name,access,binding}) =
    (print "STRvar{name="; print (formatQid name); print ",access="; 
     PrintBasics.printAccess access; print ",binding=";
     printStr binding; print "}")

fun printBinding env (VARbind(var)) = (print "val "; printVariable env var)
  | printBinding env (CONbind(con)) = (print "con "; printDatacon env con)
  | printBinding env (TYCbind(tycon)) = (print "type "; printTycon std_out env tycon)
  | printBinding _ (SIGbind(SIGvar{name,...})) = (print "signature "; printSym name)
  | printBinding _ (STRbind(strVar)) = (print "structure "; printStrVar strVar)
  | printBinding _ (FCTbind(FCTvar{name,...})) = (print "functor "; printSym name)
  | printBinding _ (FIXbind(FIXvar{name,binding=NONfix})) = (print "nonfix "; printSym name)
  | printBinding _ (FIXbind(FIXvar{name,binding=INfix _})) = (print "infix "; printSym name)

fun printTable env table =
    Env.app (fn (_,binding) => (printBinding env binding; newline())) table

fun printStructure env (STRstr{stamp,table,env=strenv,...}) =
    let fun printTenv (t:tycon array) =
	 let fun foreach i =
		 (print i; print " "; PrintType.printTycon std_out env (t sub i); newline();
		  foreach(i+1))
	  in print "types\n";
	     foreach 0
	     handle Subscript => print "end types\n"
	 end
     in
      (print "STRstr["; print stamp; print "]\n";
       case strenv 
         of REL{t,...} => printTenv t
          | DIR => ();
       printTable env table)
    end
  | printStructure _ (INDstr _) = ErrorMsg.impossible "printStructure: INDstr"
  | printStructure _ (SHRstr _) = ErrorMsg.impossible "printStructure: SHRstr"
  | printStructure _ (NULLstr) = ErrorMsg.impossible "printStructure: NULLstr"

end (* local *)

end (* DPrintBasics *)

(* dprintbas.sml *)

structure DPrintBasics = struct

local open PrintUtil Access Basics PrintType System.Control in

val printTuple = printClosedSequence ("(", ",", ")")

val printList = printClosedSequence ("[", ",", "]") (* printList not used here *)
		
fun printPath [n:int] = (print n; ())
  | printPath (n::p) = (printPath p; print "."; print n; ())
  | printPath [] = ()

fun printTyfun (TYFUN{arity,body}) =
	(print "TYFUN({arity="; print arity; print ",body="; printType body; print "})")

fun printAccess a =
     case a
	    of (LVAR v) => (print "@"; print v; ())
	     | (SLOT n) => (print "#"; print n; ())
	     | (PATH p) => (print "$"; printPath p; ())
	     | (INLINE p) => (print "%"; print(Prim.inLineName p); ())

fun printRep UNDECIDED = print "UNDECIDED"
  | printRep (TAGGED i) = (print "TAGGED["; print i; print "]")
  | printRep (CONSTANT i) = (print "CONSTANT["; print i; print "]")
  | printRep TRANSPARENT = print "TRANSPARENT"
  | printRep TRANSU = print "TRANSU"
  | printRep TRANSB = print "TRANSB"
  | printRep REF = print "REF"
  | printRep (VARIABLE a) = (print "VARIABLE["; printAccess a; print "]")


fun printDcon (DATACON{name,const,typ,rep,sign}) =
		(print "DATACON{name="; printSym name; print ",const="; print const;
		 print ",typ="; printType (!typ); print ",rep="; printRep rep; 
		 print ",sign=["; printvseq 0 "," (fn x=>printRep x) sign; print "]}")

fun printDatacon(DATACON{name,typ,...}) =
    (printSym name; print " : "; printType(!typ)) (* ???? *)

fun printExn(DATACON{name,typ,...}) =
    (printSym name; print " : "; printType(!typ))

fun printVar (UNKNOWNvar s) = (print "UNKNOWNvar("; printSym s; print ")")
  | printVar (VALvar {access,name,typ}) = (print "VALvar({access="; printAccess access;
			print ",name="; print (formatQid name); print ",typ=ref "; 
			printType (!typ); print "})")
  | printVar (OVLDvar {name,options,scheme}) = (print "OVLDvar({name="; printSym(name);
			print ",options=["; 
			(printvseq 0 "," (fn {indicator,variant} =>
					 (print "{indicator="; printType  indicator;
					  print ",variant ="; printVar variant; print "}"))
					  (!options));
			print "],scheme="; printTyfun scheme; print "})")


fun printVariable(VALvar{name,access,typ}) =  (* ???? *)
    (print (formatQid name); printAccess access;
     print " : "; printType(!typ))
  | printVariable(OVLDvar{name,...}) = (printSym name; print " : overloaded")
  | printVariable(UNKNOWNvar name) = (printSym name; print " : ?")

fun printStr(STRstr _) = print "STRstr"
  | printStr(INDstr _) = print "INDstr"
  | printStr(SHRstr _) = print "SHRstr"
  | printStr(NULLstr) = print "NULLstr"

fun printStrVar(STRvar{name,access,binding}) =
    (print "STRvar{name="; print (formatQid name); print ",access="; 
	printAccess access; print ",binding="; printStr binding; print "}")

fun printBinding(VARbind(var)) = (print "val "; printVariable var)
  | printBinding(CONbind(con)) = (print "con "; printDatacon con)
  | printBinding(TYCbind(ref tycon)) = (print "type "; printTycon tycon)
  | printBinding(TYVbind v) = (print "type "; printTyvar v)
  | printBinding(SIGbind(SIGvar{name,...})) = (print "signature "; printSym name)
  | printBinding(STRbind(strVar)) = (print "structure "; printStrVar strVar)
  | printBinding(FCTbind(FCTvar{name,...})) = (print "functor "; printSym name)
  | printBinding(FIXbind(FIXvar{name,binding=NONfix})) = (print "nonfix "; printSym name)
  | printBinding(FIXbind(FIXvar{name,binding=INfix _})) = (print "infix "; printSym name)

fun printTable(table) =
    IntStrMap.app (fn (_,_,binding) => (printBinding(binding); newline())) table

fun printStructure(STRstr{stamp,table,env,...}) =
    let fun printTenv (t:tycon array) =
	 let fun foreach i =
		 (print i; print " "; PrintType.printTycon(t sub i); newline();
		  foreach(i+1))
	  in print "types\n";
	     foreach 0
	     handle Subscript => print "end types\n"
	 end
     in
      (print "STRstr["; print stamp; print "]\n";
       case env 
         of REL{t,...} => printTenv t
          | DIR => ();
       printTable table)
    end
  | printStructure(INDstr _) = ErrorMsg.impossible "printStructure: INDstr"
  | printStructure(SHRstr _) = ErrorMsg.impossible "printStructure: SHRstr"
  | printStructure(NULLstr) = ErrorMsg.impossible "printStructure: NULLstr"

end (* local *)

end (* DPrintBasics *)

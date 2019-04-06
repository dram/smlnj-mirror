(* printbasics.sml *)

structure PrintBasics = struct

local open PrintUtil Access Basics PrintType System.Control in

val printTuple = printClosedSequence ("(", ",", ")")

val printList = printClosedSequence ("[", ",", "]") (* printList not used here *)
		
fun printPath [n:int] = (print n; ())
  | printPath (n::p) = (printPath p; print "."; print n; ())
  | printPath [] = ()

fun printAccess a =
    if !internals then case a
	    of (LVAR v) => (prstr "@"; print v; ())
	     | (SLOT n) => (prstr "#"; print n; ())
	     | (PATH p) => (prstr "$"; printPath p; ())
	     | (INLINE p) => (prstr "%"; print(Prim.inLineName p); ())
	else ()

fun printRep UNDECIDED = prstr "UNDECIDED"
  | printRep (TAGGED i) = (prstr "TAGGED["; print i; prstr "]")
  | printRep (CONSTANT i) = (prstr "CONSTANT["; print i; prstr "]")
  | printRep TRANSPARENT = prstr "TRANSPARENT"
  | printRep TRANSU = prstr "TRANSU"
  | printRep TRANSB = prstr "TRANSB"
  | printRep REF = prstr "REF"
  | printRep (VARIABLE a) = (prstr "VARIABLE["; printAccess a; prstr "]")

fun printDcon (DATACON{name,rep=VARIABLE(access),...}) =
		(printSym(name); printAccess access)
  | printDcon (DATACON{name,...}) = printSym(name)

fun printDatacon(DATACON{name,typ,...}) =
    (printSym name; prstr " : "; printType(typ))

fun printExn(DATACON{name,typ,...}) =
    (printSym name; prstr " : "; printType(typ))

fun printVar (UNKNOWNvar s) = (printSym s; if !internals then prstr "?" else())
  | printVar (VALvar {access,name,...}) = (printSym(name); printAccess access)
  | printVar (OVLDvar {name,...}) = printSym(name)

fun printVariable(VALvar{name,access,vtype}) = 
    (printSym name; printAccess access;
     prstr " : "; printType(!vtype))
  | printVariable(OVLDvar{name,...}) = (printSym name; prstr " : overloaded")
  | printVariable(UNKNOWNvar name) = (printSym name; prstr " : ?")

fun printStr(STRstr _) = prstr "STRstr"
  | printStr(INDstr _) = prstr "INDstr"

fun printStrVar(STRvar{name,access,binding}) =
    (printSym name; printAccess access;
     if !internals then (prstr "["; printStr binding; prstr "]") else ())

fun printBinding(VARbind(var)) = (prstr "val "; printVariable var)
  | printBinding(CONbind(con)) = (prstr "con "; printDatacon con)
  | printBinding(TYCbind(ref tycon)) = (prstr "type "; printTycon tycon)
  | printBinding(TYVbind v) = (prstr "type "; printTyvar v)
  | printBinding(SIGbind(SIGvar{name,...})) = (prstr "signature "; printSym name)
  | printBinding(STRbind(strVar)) = (prstr "structure "; printStrVar strVar)
  | printBinding(FCTbind(FCTvar{name,...})) = (prstr "functor "; printSym name)
  | printBinding(FIXbind NONfix) = prstr "nonfix"
  | printBinding(FIXbind (INfix _)) = prstr "infix"

fun printTable(table) =
    Table.app(table, (fn (_,binding) => (printBinding(binding); newline())))

fun printStructure(STRstr{stamp,table,env={s,t},...}) =
    let fun printTenv () =
	 let fun foreach i =
		 (print i; print " "; PrintType.printTycon(t sub i); newline();
		  foreach(i+1))
	  in print "types\n";
	     foreach 0
	     handle Subscript => (print "end types\n"; ())
	 end
     in
      (prstr "STRstr["; print stamp; prstr "]\n";
       printTenv();
       printTable table)
    end
  | printStructure(INDstr _) = ErrorMsg.impossible "printStructure: INDstr"


end (* local *)

end (* PrintBasics *)

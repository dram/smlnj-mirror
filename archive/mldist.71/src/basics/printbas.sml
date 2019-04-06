(* Copyright 1989 by AT&T Bell Laboratories *)
(* printbasics.sml *)

signature PRINTBASICS = 
sig
    val printTuple: ('a -> unit) -> 'a list -> unit
    val printAccess: Access.access -> unit
    val printRep: Access.conrep -> unit
    val printDcon: Basics.datacon -> unit
    val printVar: Basics.var -> unit
    val printStr: Basics.Structure -> unit
    val printStrVar: Basics.structureVar -> unit
    val printBinding: Basics.env -> Basics.binding -> unit
end

structure PrintBasics : PRINTBASICS = struct

local 
    open Array List PrintUtil Access Basics PrintType System.Control 
    infix 9 sub
in

val printTuple = printClosedSequence ("(", ",", ")")

fun printAccess a = print(Access.pr_access a)

fun printRep UNDECIDED = print "UNDECIDED"
  | printRep (TAGGED i) = (print "TAGGED["; print i; print "]")
  | printRep (CONSTANT i) = (print "CONSTANT["; print i; print "]")
  | printRep TRANSPARENT = print "TRANSPARENT"
  | printRep TRANSU = print "TRANSU"
  | printRep TRANSB = print "TRANSB"
  | printRep REF = print "REF"
  | printRep (VARIABLE a) = (print "VARIABLE["; printAccess a; print "]")
  | printRep (VARIABLEc a) = (print "VARIABLEc["; printAccess a; print "]")

fun printDcon (DATACON{name,rep=VARIABLE(access),...}) =
      (printSym(name); if !internals then printAccess access else ())
  | printDcon (DATACON{name,rep=VARIABLEc(access),...}) =
      (printSym(name); if !internals then printAccess access else ())
  | printDcon (DATACON{name,...}) = printSym(name)

fun printDatacon (env:Basics.env) (DATACON{name,typ,...}) =
    (printSym name; print " : "; printType std_out env typ)

fun printExn (env:Basics.env) (DATACON{name,typ,...}) =
    (printSym name; print " : "; printType std_out env typ)

fun printVar (VALvar {access,name,...}) =
      (print(formatQid name);
       if !internals then printAccess access else ())
  | printVar (OVLDvar {name,...}) = (printSym(name); print " : overloaded")

fun printVariable (env:Basics.env) (VALvar{name,access,typ}) = 
      (print(formatQid name);
       if !internals then printAccess access else ();
       print " : "; printType std_out env (!typ))
  | printVariable env (OVLDvar {name,options=ref optl,scheme=TYFUN{body,...}}) =
    (printSym(name); print " : "; printType std_out env body; print " as ";
     printSequence " " (fn {variant=VALvar{name,...},...} =>
                             print(formatQid name)) optl)

fun printStr(STRstr _) = print "STRstr"
  | printStr(INDstr _) = print "INDstr"
  | printStr(SHRstr _) = print "SHRstr"
  | printStr(NULLstr) = print "NULLstr"

fun printStrVar(STRvar{name,access,binding}) =
    (print(formatQid name);
     if !internals
     then (printAccess access; print "["; printStr binding; print "]")
     else ())

fun printBinding (env:Basics.env) (VARbind(var)) = (print "val "; printVariable env var)
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
    let fun printTenv (t: tycon array) =
	 let fun foreach i =
		 (print i; print " "; PrintType.printTycon std_out env (t sub i);
		  newline();
		  foreach(i+1))
	  in print "types\n";
	     foreach 0
	     handle Subscript => print "end types\n"
	 end
     in
      (print "STRstr["; print stamp; print "]\n";
       case strenv
         of REL{t,...} => printTenv(t)
          | DIR => ();
       printTable env table)
    end
  | printStructure _ (INDstr _) = ErrorMsg.impossible "printStructure: INDstr"
  | printStructure _ (SHRstr _) = ErrorMsg.impossible "printStructure: SHRstr"
  | printStructure _ (NULLstr) = ErrorMsg.impossible "printStructure: NULLstr"

fun pr_path'[] = "]"
  | pr_path'[x:int] = makestring x ^ "]"
  | pr_path'((x:int)::rest)= makestring x ^ "," ^ pr_path' rest
fun pr_path path = "[" ^ pr_path' path

end (* local *)

end (* PrintBasics *)

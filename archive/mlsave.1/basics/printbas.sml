(* printbasics.sml *)

structure PrintBasics = struct

local open PrintUtil Access Basics PrintType in

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
	     | (INLINE n) => (prstr "%"; print n; ())
	else ()

fun printRep UNDECIDED = prstr "UNDECIDED"
  | printRep (TAGGED i) = (prstr "TAGGED["; print i; prstr "]")
  | printRep (CONSTANT i) = (prstr "CONSTANT["; print i; prstr "]")
  | printRep TRANSPARENT = prstr "TRANSPARENT"
  | printRep TRANSU = prstr "TRANSU"
  | printRep TRANSB = prstr "TRANSB"
  | printRep REF = prstr "REF"
  | printRep (VARIABLE a) = (prstr "VARIABLE["; printAccess a; prstr "]")

fun printDcon (DATACON{name,rep=ref(VARIABLE(access)),...}) =
				(printSym(name); printAccess access)
  | printDcon (DATACON{name,...}) = printSym(name)

fun printDatacon(DATACON{name,vtype,...}) =
    (printSym name; prstr " : "; printType(vtype))

fun printExn(DATACON{name,vtype,...}) =
    (printSym name; prstr " : "; printType(vtype))

fun printVar (UNKNOWNvar s) = (printSym s; if !internals then prstr "?" else())
  | printVar (VALvar {access,name,...}) = (printSym(name); printAccess access)
  | printVar (OVLDvar {name,...}) = printSym(name)

fun printVariable(VALvar{name,access,vtype}) = 
    (printSym name; printAccess access;
     prstr " : "; printType(!vtype))
  | printVariable(OVLDvar{name,...}) = (printSym name; prstr " : overloaded")
  | printVariable(UNKNOWNvar name) = (printSym name; prstr " : ?")

fun printStr(DIRECT _) = prstr "DIRECT"
  | printStr(FCTAPP _) = prstr "FCTAPP"
  | printStr(PARAM _) = prstr "PARAM"
  | printStr(SPEC _) = prstr "SPEC"

fun printStrVar(STRvar{name,access,binding,sign}) =
    (printSym name; printAccess access;
     if !internals then (prstr "["; printStr binding; prstr "]") else ())

(* this is duplicated from printtype
fun printTycon(ATOMtyc{name,stamp,arity}) =
      (if !internals then prstr "ATOM/" else ();
       printSym name)
  | printTycon(SYMtyc(spath, name)) =
      (if !internals then prstr "SYM/" else ();
       printSequence "." printSym spath; prstr "."; printSym name)
  | printTycon(VARtyc{name,context,...}) =
      (if !internals then prstr "VART/" else ();
       printSym name;
       if !internals then case context
		    of SIGctx => prstr ":S" 
		     | FCTctx => prstr ":F"
		     | TOPctx => prstr ":T"
		     | PARctx _ => prstr ":P"
		     | RELctx _ => prstr ":R"
		     | SHRctx _ => prstr ":H"
		else ())
  | printTycon(TYPEtyc{name,...}) =
      (if !internals then prstr "TYPE/" else (); printSym name)
  | printTycon(DATAtyc{name,...}) =
      (if !internals then prstr "DATA/" else (); printSym name)
  | printTycon(RECORDtyc _) = prstr "REC/"
  | printTycon(UNKNOWNtyc name) = (prstr "UNKT/"; printSym name)
*)

fun printBinding(VARbind(var)) = (prstr "val "; printVariable var)
  | printBinding(CONbind(con)) = (prstr "con "; printDatacon con)
  | printBinding(EXNbind(exn)) = (prstr "exception"; printExn exn)
  | printBinding(TYCbind(ref tycon)) = (prstr "type "; printTycon tycon)
  | printBinding(TYVbind v) = (prstr "type "; printTyvar v)
  | printBinding(SIGbind(SIGvar{name,...})) = (prstr "signature "; printSym name)
  | printBinding(STRbind(strVar)) = (prstr "structure "; printStrVar strVar)
  | printBinding(FCTbind(FCTvar{name,...})) = (prstr "functor "; printSym name)
  | printBinding(FIXbind NONfix) = prstr "nonfix"
  | printBinding(FIXbind (INfix _)) = prstr "infix"

fun printTable(table) =
    Table.app(table, (fn (_,binding) => (printBinding(binding); newline())))

fun printContext(TOPctx) = prstr "TOPctx"
  | printContext(FCTctx) = prstr "FCTctx"
  | printContext(RELctx _) = prstr "RELctx"
  | printContext(PARctx _) = prstr "PARctx"
  | printContext(SHRctx _) = prstr "SHRctx"
  | printContext(SIGctx) = prstr "SIGctx"

fun printStructure(DIRECT{table,context,stamp}) =
      (prstr "DIRECT["; print stamp; prstr ","; printContext context; prstr "]\n";
       printTable table)
  | printStructure(FCTAPP{context,...}) =
      (prstr "FCTAPP["; printContext context; prstr "]\n")
  | printStructure(PARAM{pnode=STRpnode{stamp,...},spath,...}) =
      (prstr "PARAM[STR"; print stamp; prstr ",";
       printSequence "." printSym spath; prstr "]\n")
  | printStructure(PARAM{pnode=TYCpnode{stamp},spath,...}) =
      (prstr "PARAM[TYC"; print stamp; prstr ",";
       printSequence "." printSym spath; prstr "]\n")
  | printStructure(SPEC sign) = prstr "SPEC\n"


end (* local *)

end (* PrintBasics *)

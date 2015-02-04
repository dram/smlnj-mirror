(* printtype.sml *)

(*
signature PRINTTYPE = sig
  structure Basics : BASICS
  val printTyvar : B.tyvar -> unit
  val printType : B.ty -> unit
end 
*)
structure PrintType (* : PRINTTYPE *) = struct

structure Basics = Basics

local
  open Basics List2 PrintUtil
in

fun printTyvar(TYVAR{name,stamp,status} : tyvar) : unit =
    (printSym name; print stamp;
     case !status
       of BOUND => prstr "B"
        | METAARG => prstr "A"
	| METALAM n => (prstr "L"; print n; ())
	| FIXED => prstr "F"
	| INSTANTIATED _ => prstr "I")

fun printTycon(ATOMtyc{name,stamp,...}) =
      (if !internals then (prstr "ATOM/"; print stamp; prstr "/") else ();
       printSym name)
  | printTycon(VARtyc{name,stamp,context,...}) =
      (if !internals then (prstr "VAR/"; print stamp; prstr "/") else ();
       printSym name;
       if !internals then case context
		    of SIGctx => prstr ":S" 
		     | FCTctx => prstr ":F"
		     | TOPctx => prstr ":T"
		     | PARctx _ => prstr ":P"
		     | RELctx _ => prstr ":R"
		     | SHRctx _ => prstr ":H"
		else ())
  | printTycon(SYMtyc(spath,name)) = 
      (if !internals then (prstr "SYM/") else ();
       printSequence "." printSym spath; prstr "."; printSym name)
  | printTycon(TYPEtyc{name,stamp,...}) =
      (if !internals then (prstr "TYPE/"; print stamp; prstr "/") else ();
       printSym name)
  | printTycon(DATAtyc{name,stamp,...}) =
      (if !internals then (prstr "DATA/"; print stamp; prstr "/") else ();
       printSym name)
  | printTycon(RECORDtyc{labels,stamp}) =
      (if !internals then (prstr "RECORD/"; print stamp; prstr "/") else ();
       printClosedSequence("{",",","}") printSym labels)
  | printTycon(UNKNOWNtyc name) =
      (if !internals then (prstr "UNKNOWN/") else ();
       printSym name)

fun printType(ty: ty) : unit =
    case prune(ty)
      of VARty(tv) => printTyvar tv
       | CONty(ref tycon, args) =>
	   (case tycon
	      of ATOMtyc{name,...} => 
		   if eqTycon(tycon,!BasicTypes.arrowTycon)
		   then let val [domain,range] = args
			 in prstr "("; printType domain;
			    prstr " -> ";
			    printType range; prstr ")"
			end
		   else (printTypeArgs(args); printSym name)	
	       | SYMtyc(spath,name) =>
		   (printTypeArgs(args); 
		    printSequence "." printSym spath;
		    prstr "."; printSym name)
	       | VARtyc{name,...} =>
		   (printTypeArgs(args); printSym name)
	       | TYPEtyc{name,...} =>
		   (printTypeArgs(args); printSym name)
	       | DATAtyc{name,...} =>
		   (printTypeArgs(args); printSym name)
	       | RECORDtyc{labels,...} =>
		   if Tuples.isTUPLEtyc(tycon)
		   then printTUPLEty(args)
		   else printRECORDty(labels, args)
	       | UNKNOWNtyc name =>
		   (printTypeArgs(args); printSym name; prstr "?"))
       | FLEXRECORDty{fields,...} => printFLEXRECORDty fields
       | UNKNOWNty => prstr "??"

and printTypeArgs nil = ()
  | printTypeArgs [ty] = (printType ty; prstr " ")
  | printTypeArgs tys = printClosedSequence ("(", ",", ") ") printType tys

and printTUPLEty nil = prstr "unit"
  | printTUPLEty tys = printClosedSequence ("(", "*", ")") printType tys

and printField(lab,arg) = (printSym lab; prstr ":"; printType arg)

and printRECORDty([],[]) = prstr "unit"
  | printRECORDty(lab::labels, arg::args) =
      (prstr "{";
       printField(lab,arg);
       app2 
         (fn field => (prstr ","; printField field))
	 (labels,args);
       prstr "}")
  | printRECORDty _ = ErrorMsg.Impossible "PrintType.printRECORDty"

and printFLEXRECORDty [] = prstr "{...}"	
  | printFLEXRECORDty (field::fields) =
      (prstr "{";
       printField field;
       app 
         (fn field => (prstr ","; printField field))
	 fields;
       prstr ",...}");

end (* local open ... *)

end (* structure PrintType *)

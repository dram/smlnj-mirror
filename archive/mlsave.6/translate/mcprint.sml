(* mcprint.sml *)

signature MCprint = sig
	structure A : BAREABSYN
	structure L : LAMBDA
	val printCon : L.con -> unit
	val printLexp : L.lexp -> unit
	val printMatch : (A.pat * L.lexp) list -> unit
	end

structure MCprint : MCprint = struct

structure A = BareAbsyn
structure L = Lambda

local
  open Access Basics A L PrintUtil PrintBasics PrintAbsyn ErrorMsg
  val shiftwidth = 4
  val margin = ref 0
  
  fun indent () = margin := !margin + shiftwidth
  
  exceptionx undent
  
  fun undent () = if (!margin) = 0
		    then raisex undent
		    else margin := !margin - shiftwidth
  
  fun dent () = tab(!margin)

in

fun printCon (DATAcon dcon) = printDcon dcon
  | printCon (INTcon i) = (print i; ())
  | printCon (REALcon r) = (print r; ())
  | printCon (STRINGcon s) = prstr(ml_string s)

fun printLexp (VAR v) = prstr(lvarName v)
  | printLexp (INT i) = (print i; ())
  | printLexp (REAL s) = prstr s
  | printLexp (STRING s) = prstr(ml_string s)
  | printLexp (RECORD llist) = printTuple printLexp llist
  | printLexp (SELECT (i,l)) = 
      (prstr "SELECT ("; print i; prstr ","; printLexp l; prstr ")")
  | printLexp (FN (v,l)) = 
      (indent();
       prstr "FN("; prstr(lvarName v); prstr ",\n"; dent(); printLexp l; prstr ")";
       undent())
  | printLexp (APP(FN(v,l),r)) =
      (prstr(lvarName v); prstr " = "; printLexp r; prstr "\n"; dent(); printLexp l)
  | printLexp (APP (l,r)) = 
      (prstr "APP("; printLexp l; prstr ","; printLexp r; prstr ")")
  | printLexp (SWITCH (l,llist,default)) =
	let fun switch ((c,l)::more) = 
		  (printCon c; prstr " =>\n";
		   indent(); dent(); printLexp l; undent(); prstr "\n"; dent();
		   switch more)
		| switch nil = ()
	 in indent();
	    prstr "SWITCH "; printLexp l; prstr " of\n"; dent();
	    switch llist;
	    case default
	      of NONE => ()
	       | (SOME l) =>
		   (prstr "_ =>\n";
		    indent(); dent(); printLexp l; undent(); prstr "\n"; dent());
	    undent()
	end
  | printLexp (RAISE l) = (prstr "RAISE "; printLexp l)
  | printLexp (FIX (varlist,lexplist,lexp)) =
      (prstr "FIX("; printList (fn v => prstr(lvarName v)) varlist; prstr ",";
       printList printLexp lexplist; prstr ","; printLexp lexp; prstr ")")
  | printLexp (HANDLE (lexp,withlexp)) =
      (prstr "HANDLE "; printLexp lexp; prstr " WITH "; printLexp withlexp)

fun printMatch ((p,r)::more) =
      (printPat p; prstr " => "; printLexp r; newline(); printMatch more)
  | printMatch nil = ()

end (* local *)



end (* struct MCprint *)

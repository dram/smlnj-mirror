(* mcprint.sml *)

signature MCprint =
  sig
    structure Access : ACCESS
    structure A : BAREABSYN
    structure L : LAMBDA
    val printCon : L.con -> unit
    val printLexp : L.lexp -> unit
    val printMatch : (A.pat * L.lexp) list -> unit
    val printFun : L.lexp -> Access.lvar -> unit
  end

structure MCprint : MCprint = struct

structure Access = Access
structure A = BareAbsyn
structure L = Lambda

open Access Basics A L PrintUtil PrintBasics PrintAbsyn ErrorMsg
val margin = ref 0
  
fun indent i = margin := !margin + i
  
exception Undent
  
fun undent i = (margin := !margin - i;
		if !margin < 0 then raise Undent else ())

fun dent () = tab(!margin)

fun printCon (DATAcon dcon) = printDcon dcon
  | printCon (INTcon i) = (print i; ())
  | printCon (REALcon r) = prstr r
  | printCon (STRINGcon s) = pr_mlstr s

(* use of complex in printLexp may lead to stupid n^2 behavior. *)
val rec complex =
     fn VAR w => false
      | FN(_,b) => complex b
      | FIX(vl,ll,b) => true
      | APP(FN _,_) => true
      | APP(l,r) => complex l orelse complex r
      | SWITCH (l,ls,d) => true
      | RECORD l => let fun f nil = false | f (hd::tl) = complex hd orelse f tl
		    in  f l
		    end
      | SELECT(_,l) => complex l
      | HANDLE _ => true | RAISE l => complex l
      | INT _ => false | STRING _ => false | REAL _ => false

fun printLexp (VAR v) = prstr(lvarName v)
  | printLexp (INT i) = (print i; ())
  | printLexp (REAL s) = prstr s
  | printLexp (STRING s) = pr_mlstr s
  | printLexp (r as RECORD l) =
	if complex r
	   then (prstr "RECORD";
		 indent 7;
		 printTuple (fn l => (printLexp l; newline(); dent())) l;
		 undent 7)
	   else (prstr "RECORD"; printTuple printLexp l)
  | printLexp (SELECT(i,VAR 0)) =
	(prstr(nth(Prim.inLineNames,i))
		handle Nth => prstr ("v0["^makestring i^"]"))
  | printLexp (l as SELECT(i,_)) =
	let fun gather(SELECT(i,l)) =
		let val (more,root) = gather l
		in  (i :: more,root)
		end
	      | gather l = (nil,l)
	    val (path,root) = gather l
	    fun ipr (i:int) = (print i; ())
	in  printLexp root;
	    printClosedSequence ("[",",","]") ipr (rev path)
	end
  | printLexp (FN(v,l)) = 
	(prstr "FN("; prstr(lvarName v); prstr ",";
	 if complex l then (newline(); indent 3; dent();
			    printLexp l; prstr ")"; undent 3)
	 else (printLexp l; prstr ")")
	 )
  | printLexp (APP(FN(v,l),r)) =
	let val lv = lvarName v
	    val len = length lv + 3
	in  prstr lv; prstr " = ";
	    indent len ; printLexp r; undent len;
	    newline(); dent(); printLexp l
	end
  | printLexp (APP(l,r)) = 
	(prstr "APP(";
	 if complex l orelse complex r
	   then (indent 4; printLexp l; prstr ",\n"; dent();
		 printLexp r; prstr ")"; undent 4)
	   else (printLexp l; prstr ",";
		 printLexp r; prstr ")"))
  | printLexp (SWITCH (l,llist,default)) =
	let fun switch [(c,l)] =
		  (printCon c; prstr " => ";
		   indent 8; printLexp l; undent 8)
	      | switch ((c,l)::more) = 
		  (printCon c; prstr " => ";
		   indent 8; printLexp l; undent 8; newline(); dent();
		   switch more)
	in  prstr "SWITCH ";
	    indent 7;
	    printLexp l;
	    undent 6;
	    newline(); dent();
	    prstr "of "; indent 3;
	    switch llist;
	    case (default,llist)
	      of (NONE,_) => ()
	       | (SOME l,nil) =>
		   (prstr "_ => ";
		    indent 5; printLexp l; undent 5)
	       | (SOME l,_) =>
		   (newline(); dent(); prstr "_ => ";
		    indent 5; printLexp l; undent 5);
	    undent 4
	end
  | printLexp (FIX (varlist,lexplist,lexp)) =
	let fun flist([v],[l]) =
		let val lv = lvarName v
		    val len = length lv + 2
		in  prstr lv; prstr ": ";
		    indent len ; printLexp l; undent len
		end
	      | flist(v::vs,l::ls) =
		let val lv = lvarName v
		    val len = length lv + 2
		in  prstr lv; prstr ": ";
		    indent len ; printLexp l; undent len;
		    newline(); dent(); flist(vs,ls)
		end
	      | flist(nil,nil) = ()
	in  prstr "FIX("; indent 4; flist(varlist,lexplist); undent 4;
	    newline(); dent(); prstr "IN  ";
	    indent 4; printLexp lexp; prstr ")"; undent 4
	end
  | printLexp (RAISE l) = (prstr "RAISE "; indent 6; printLexp l; undent 6)
  | printLexp (HANDLE (lexp,withlexp)) =
      (prstr "HANDLE "; indent 7; printLexp lexp;
       undent 5; newline(); dent();
       prstr "WITH "; indent 5; printLexp withlexp; undent 7)

fun printMatch ((p,r)::more) =
      (printPat(p,!System.Control.Print.printDepth);
       prstr " => "; printLexp r; newline(); printMatch more)
  | printMatch nil = ()

fun printFun l v =
    let val rec findit =
	 fn VAR w => if v=w 
		       then (print("VAR " ^ lvarName v ^ " is free in <lexp>\n");
			     ())
		       else ()
	  | l as FN(w,b) => if v=w then printLexp l else findit b
	  | l as FIX(vl,ll,b) => if exists (fn w => v=w) vl
				   then printLexp l
				   else (app findit ll; findit b)
	  | APP(l,r) => (findit l; findit r)
	  | SWITCH (l,ls,d) =>
		(findit l;
	         app (fn(_,l) => findit l) ls;
		 case d of NONE => () | SOME l => findit l)
	  | RECORD l => app findit l | SELECT(_,l) => findit l
	  | HANDLE(e,h) => (findit e; findit h) | RAISE l => findit l
	  | INT _ => () | STRING _ => () | REAL _ => ()
    in  findit l
    end


end (* struct MCprint *)

(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* ppflint.sml -- Pretty printer for Flint IL. *)


structure PPFlint :> PPFLINT =
struct
    (** frequently used structures *)
    structure F = FLINT
    structure S = Symbol
    structure LV = LambdaVar
    structure LT = LtyExtern
    structure PO = PrimOp
    structure PU = PrintUtil

    (** some print utilities **)
    val say = Control.Print.say
    val margin = ref 0
    exception Undent
    fun indent n = margin := !margin + n
    fun undent n = (margin := !margin - n;
		    if !margin < 0 then raise Undent
		    else ())
    fun dent () = PU.tab(!margin)
    val newline = PU.newline

    infix &
    fun op& (f1,f2) () = (f1(); f2())


    (** classifications of various kinds of functions: not used *)
    fun toStringFKind ({isrec=SOME _, ...} : F.fkind) = "FK_REC"
      | toStringFKind {isrec=_, ...} = "FK_ESCAPE"
(*
    fun toStringFKind F.FK_ESCAPE  = "FK_ESCAPE"
      | toStringFKind F.FK_KNOWN   = "FK_KNOWN"
      | toStringFKind F.FK_KREC    = "FK_KREC"
      | toStringFKind F.FK_KTAIL   = "FK_KTAIL"
      | toStringFKind F.FK_NOINL   = "FK_NOINL"
      | toStringFKind F.FK_HANDLER = "FK_HANDLER"
*)

    val printFKind = say o toStringFKind

    (** classifications of various kinds of records *)
    fun toStringRKind (F.RK_VECTOR tyc) = "VECTOR[" ^ LT.tc_print tyc ^ "]"
      | toStringRKind F.RK_RECORD = "RECORD"
      | toStringRKind F.RK_STRUCT = "STRUCT"

    val printRKind = say o toStringRKind

    (** con: used to specify all possible switching statements. *)
    fun toStringCon (F.DATAcon((symbol,_,_),_,_))   = S.name symbol
      | toStringCon (F.INTcon i)    = "(I)" ^ (Int.toString i)
      | toStringCon (F.INT32con i)  = "(I32)" ^ (Int32.toString i)
      | toStringCon (F.WORDcon i)   = "(W)" ^ (Word.toString i)
      | toStringCon (F.WORD32con i) = "(W32)" ^ (Word32.toString i)
      | toStringCon (F.REALcon r)   = r
      | toStringCon (F.STRINGcon s) = PU.mlstr s
      | toStringCon (F.VLENcon n)   = Int.toString n

    val printCon = say o toStringCon

    (** simple values, including variables and static constants. *)
    fun toStringValue (F.VAR v)    = LV.lvarName v
      | toStringValue (F.INT i)    = "(I)" ^ Int.toString i
      | toStringValue (F.INT32 i)  = "(I32)" ^ Int32.toString i
      | toStringValue (F.WORD i)   = "(W)" ^ Word.toString i
      | toStringValue (F.WORD32 i) = "(W32)" ^ Word32.toString i
      | toStringValue (F.REAL r)   = r
      | toStringValue (F.STRING s) = PU.mlstr s

    val printSval = say o toStringValue
    val LVarString = ref LV.lvarName

    fun printVar v = say (!LVarString v)
    val printTyc = say o LT.tc_print
    val printLty = say o LT.lt_print
    fun printTvTk (tv:LT.tvar,tk) = 
	say (LT.tk_print tk)

    val parenCommaSep = ("(", ",", ")")
    val printValList = PU.printClosedSequence ("[",",","]") printSval
    val printVarList = PU.printClosedSequence ("[",",","]") printVar
    val printTycList = PU.printClosedSequence ("[",",","]") printTyc
    val printLtyList = PU.printClosedSequence parenCommaSep printLty
    val printTvTkList = PU.printClosedSequence ("[",",","]") printTvTk

    fun appPrint prfun sepfun [] = ()
      | appPrint prfun sepfun (x::xs) =
	(prfun x;  app (fn y => (sepfun(); prfun y)) xs)

    fun printDecon (F.DATAcon(_,_,[])) = ()
      | printDecon (F.DATAcon((symbol,conrep,lty),tycs,lvars)) =
	(* [<lvars>] = DECON(<symbol>,<conrep>,<lty>,[<tycs>]) *)
	(printVarList lvars; 
	 say " = DECON("; 
	 say (S.name symbol); say ",";
	 say (Access.prRep conrep); say ",";
	 printLty lty; say ",";
	 printTycList tycs; say ")"; 
	 newline(); dent())
      | printDecon _ = ()

    (** the definitions of the lambda expressions *)

    fun complex (F.LET _) = true
      | complex (F.FIX _) = true
      | complex (F.TFN _) = true
      | complex (F.SWITCH _) = true
      | complex (F.CON _) = true
      | complex (F.HANDLE _) = true
      | complex _ = false

    fun pLexp (F.RET values) = 
	(* RETURN [values] *)
	(say "RETURN "; printValList values)
	
      | pLexp (F.APP (f, args)) =
	(* APP(f, [args]) *)
	(say "APP(";
	 printSval f;
	 say ",";
	 printValList args;
	 say ")")

      | pLexp (F.TAPP (tf, tycs)) =
	(* TAPP(tf, [tycs]) *)
	(say "TAPP(";
	 printSval tf;
	 say ",";
	 printTycList tycs;
	 say ")")

      | pLexp (F.LET (vars, lexp, body)) =
	(* [vars] = lexp   OR   [vars] =
	 *   body                 lexp
	 *                      body
	 *)
	(printVarList vars; say " = ";  
	 if complex lexp then
	     (indent 2; newline(); dent(); pLexp lexp; undent 2)
	 else
	     let val len = (3		(* for the " = " *)
			    + 2		(* for the "[]" *)
			    + (length vars) (* for each comma *)
			    + (foldl	(* sum of varname lengths *)
			       (fn (v,n) => n + (size (!LVarString v)))
			       0 vars))
	     in
		 indent len;  pLexp lexp;  undent len
	     end;
	 newline();  dent();  pLexp body)

      | pLexp (F.FIX (fundecs, body)) =
      (* FIX(<fundec1>,
       *     <fundec2>,
       *     <fundec3>)
       * <body>
       *)
	(say "FIX(";
	 indent 4;
	 appPrint printFundec (newline & dent) fundecs;
	 undent 4;  say ")";  newline();
	 dent();  
	 pLexp body)
	
      | pLexp (F.TFN ((lvar, tv_tk_list, tfnbody), body)) =
	(* v = 
	 *   TFN([tk],lty,
	 *     <tfnbody>)
	 * <body>
	 *)
	(printVar lvar; say " = "; newline();
	 indent 2; dent();
	 say "TFN(";
	 printTvTkList tv_tk_list; say ",";
	 (*** printLty lty; say ","; *** lty no longer available ***)
         newline();
	 indent 2;
	 dent();
	 pLexp tfnbody;
	 undent 4; say ")"; newline();
	 dent();
	 pLexp body)

      (** NOTE: I'm ignoring the consig here **)
      | pLexp (F.SWITCH (value, consig, con_lexp_list, lexpOption)) =
	(* SWITCH <value>
	 *   <con> => 
	 *       <lexp>
	 *   <con> => 
	 *       <lexp>
	 *)
	 (say "SWITCH "; printSval value; newline();
	  indent 2;  dent();  
	  appPrint printCase (newline & dent) con_lexp_list;
	  case  lexpOption of
	      NONE => ()
	    | SOME lexp =>		(* default case *)
		  (newline(); dent(); say "_ => ";
		      indent 4;  newline();  dent();
		      pLexp lexp;  undent 4);
		      undent 2)

      | pLexp (F.CON ((symbol,_,_), tycs, values, lvar, body)) =
	 (* <lvar> = CON(<symbol>, <tycs>, <values>)
	  * <body>
	  *)
	 (printVar lvar; say " = CON(";
	  say (S.name symbol); say ", ";
	  printTycList tycs;  say ", ";
	  printValList values;  say ")";  
	  newline();  dent();  pLexp body)
	  
      | pLexp (F.RECORD (rkind, values, lvar, body)) =
	 (* <lvar> = RECORD(<rkind>, <values>)
	  * <body>
	  *)
	 (printVar lvar;  say " = ";
	  printRKind rkind; say " ";
	  printValList values; 
	  newline();  dent();  pLexp body)
	 
      | pLexp (F.SELECT (value, int, lvar, body)) =
	 (* <lvar> = SELECT(<value>, <int>)
	  * <body>
	  *)
	 (printVar lvar;  say " = SELECT(";
	  printSval value;  say ", ";
	  say (Int.toString int);  say ")";
	  newline();  dent();  pLexp body)
	 
      | pLexp (F.RAISE (value, ltys)) =
	 (* NOTE: I'm ignoring the lty list here. It is the return type 
	  * of the raise expression.  
	  *)
	 (* RAISE(<value>) *)
	 (say "RAISE(";
	  printSval value; say ")")
	 
      | pLexp (F.HANDLE (body, value)) =
	 (* <body>
	  * HANDLE(<value>)
	  *)
	 (pLexp body;  
	  newline();  dent();
	  say "HANDLE(";  printSval value;  say ")")
	 
      | pLexp (F.ETAG (tyc, value, lvar, body)) =
	 (* <lvar> = ETAG(<value>[<tyc>])
	  * <body>
	  *)
	 (printVar lvar;  say " = ETAG(";
	  printSval value;  say "[";
	  printTyc tyc;  say "])";
	  newline();  dent();  pLexp body)
	 
      | pLexp (F.PRIMOP ((primop, lty, tycs), values, lvar, body)) =
	 (* <lvar> = PRIM(<primop>, <lty>, [<tycs>]) [<values>]
	  * <body>
	  *)
	 (printVar lvar;  say " = PRIMOP(";
	  say (PO.prPrimop primop);  say ", ";
	  printLty lty;  say ", ";
	  printTycList tycs;  say ") ";
	  printValList values;
	  newline();  dent();  pLexp body)
	 
      | pLexp (F.GENOP (dict, (primop, lty, tycs), values, lvar, body)) =
	 (* NOTE: I'm ignoring the `dict' here. *)
	 (* <lvar> = GENOP(<primop>, <lty>, [<tycs>]) [<values>]
	  * <body>
	  *)
	 (printVar lvar;  say " = GENOP(";
	  say (PO.prPrimop primop);  say ", ";
	  printLty lty;  say ", ";
	  printTycList tycs;  say ") ";
	  printValList values;
	  newline();  dent();  pLexp body)
	 
      | pLexp (F.WRAP (tyc, value, lvar, body)) =
	 (* <lvar> = WRAP(<tyc>, <value>)
	  * <body>
	  *)
	 (printVar lvar;  say " = WRAP(";
	  printTyc tyc;  say ", ";
	  printSval value;  say ")";
	  newline();  dent();  pLexp body)
	 
      | pLexp (F.UNWRAP (tyc, value, lvar, body)) =
	 (* <lvar> = UNWRAP(<tyc>, <value>)
	  * <body>
	  *)
	 (printVar lvar;  say " = UNWRAP(";
	  printTyc tyc;  say ", ";
	  printSval value;  say ")";
	  newline();  dent();  pLexp body)

    and printFundec (fkind, lvar, lvar_lty_list, body) =
	(*  <lvar> : (<fkind>) <lty> =
	 *    FN([v1 : lty1,
	 *        v2 : lty2],
	 *      <body>)
	 *)
	(printVar lvar; say " : "; 
	 say "("; printFKind fkind; say ") ";
	 (*** the return-result lty no longer available ---- printLty lty; **)
         say " = "; newline();
	 indent 2;
	 dent();
	 say "FN([";
	 indent 4;
	 (case lvar_lty_list of
	      [] => ()
	    | ((lvar,lty)::L) => 
		  (printVar lvar; say " : "; printLty lty;
		   app (fn (lvar,lty) =>
			(say ","; newline(); dent();
			 printVar lvar; say " : "; printLty lty)) L));
	      print "],"; newline();
	      undent 2;  dent();
	      pLexp body;
	      undent 4; say ")")

    and printCase (con, lexp) =
	(printCon con;
	 say " => ";
         indent 4; newline(); dent();
	 printDecon con;
	 pLexp lexp; undent 4)

    fun printLexp lexp = pLexp lexp before (newline(); newline())
	 

end (* structure PPFlint *)


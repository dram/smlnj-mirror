(* ppflint.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * "True" Pretty Printer for Flint IL, using PrettyPrint library.
 *)

structure PPFlint :> PPFLINT =
struct

local
    (** frequently used structures *)
    structure F = FLINT
    structure FU = FlintUtil
    structure S = Symbol
    structure LV = LambdaVar
    structure LT = LtyExtern
    structure PO = Primop
    structure PU = PrintUtil
    structure PP = PrettyPrint
    structure PPU = PPUtil
    structure CTRL = Control.FLINT
in

    (* fflagToString : F.fflag -> string *)
    fun fflagToString ff =
      let fun h b = if b then "r" else "c"
       in LT.ffw_var (ff, fn (b1,b2) => (h b1)^(h b2), fn _ => "f")
      end

    (* fkindToString : F.fkind -> string *)
    fun fkindToString ({isrec,cconv,inline,...}:F.fkind) =
	(case inline
	   of F.IH_ALWAYS => "(i)"
	    | F.IH_UNROLL => "(u)"
	    | F.IH_MAYBE(s,ws) => "(i:" ^ Int.toString s ^ ")"
	    | F.IH_SAFE => "")  ^
	(case isrec
	   of SOME(_,F.LK_UNKNOWN) => "R"
	    | SOME(_,F.LK_LOOP) => "LR"
	    | SOME(_,F.LK_TAIL) => "TR"
	    | NONE => "")  ^
	(case cconv
	   of F.CC_FCT => "FCT"
	    | F.CC_FUN fixed => "FUN " ^ fflagToString fixed)

    fun ppFKind ppstrm fkind = PP.string ppstrm (fkindToString fkind)

    (** classifications of various kinds of records *)
    fun rkindToString (F.RK_VECTOR tyc) = "VECTOR[" ^ LT.tc_print tyc ^ "]"
      | rkindToString F.RK_STRUCT = "STRUCT"
      | rkindToString F.RK_TUPLE = "RECORD"

    fun ppRKind ppstrm fkind = PP.string ppstrm (rkindToString rkind)

    (** con: used to specify all possible switching statements. *)
    fun conToString (F.DATAcon((symbol,_,_),_,_))   = S.name symbol
      | conToString (F.INTcon{ival, ty}) =
	  concat["(I", Int.toString ty, ")", IntInf.toString ival]
      | conToString (F.WORDcon{ival, ty}) =
	  concat["(W", Int.toString ty, ")", IntInf.toString ival]
      | conToString (F.STRINGcon s) = PrintUtil.formatString s
      | conToString (F.VLENcon n)   = Int.toString n

    fun ppCon ppstrm con = PP.string ppstrm (conToString con)

    (** simple values, including variables and static constants. *)
    fun valueToString (F.VAR v) = LV.lvarName v
      | valueToString (F.INT{ival, ty}) =
	  concat["(I", Int.toString ty, ")", IntInf.toString ival]
      | valueToString (F.WORD{ival, ty}) =
	  concat["(W", Int.toString ty, ")", IntInf.toString ival]
      | valueToString (F.REAL{rval, ty}) =
	  concat["(R", Int.toString ty, ")", RealLit.toString rval]
      | valueToString (F.STRING s) = PrintUtil.formatString s

    fun ppValue ppstrm value = PP.string ppstrm (valueToString value)

    val lvarToStringRef = ref LV.lvarName
    fun ppVar ppstrm lvar = PP.string ppstrm (!lvarToStringRef lvar)

    fun ppTyc ppstrm tyc = PP.string ppstrm (LT.tc_print tyc)
    fun ppLty ppstrm lty = PP.string ppstrm (LT.lt_print lty)
    fun ppTvTk ppstrm (tv:LT.tvar,tk) =
	PP.string ppstrm ((LV.lvarName tv)^":"^(LT.tk_print tk))

    val ppValList = PPU.ppBracketedSequence ("[", "]", ppValue)
    val ppVarList = PPU.ppBracketedSequence ("[", "]", ppVar)
    val ppTycList = PPU.ppBracketedSequence ("[", "]", ppTyc)
    val ppLtyList = PPU.ppBracketedSequence ("[", "]", ppLty)
    val ppTvTkList = PPU.ppBracketedSequence ("[", "]", ppTvTk)

    fun ppDecon ppstrm (F.DATAcon((_,Access.CONSTANT _,_),_,_)) = ()
        (* WARNING: a hack, but then what about constant exceptions ? *)
      | ppDecon ppstrm (F.DATAcon((symbol,conrep,lty),tycs,lvar)) =
	(* <lvar> = DECON(<symbol>,<conrep>,<lty>,[<tycs>]) *)
	(PP.openHBox ppstrm;
	 ppVar ppstrm lvar;
	 PP.string ppstrm " = DECON(";
	 PP.string ppstrm (S.name symbol); PP.string ppstrm ",";
	 pp.string ppstrm (Access.prRep conrep); PP.string ppstrm ",";
	 ppLty ppstrm lty; PP.string ppstrm ",";
	 ppTycList ppstrm tycs; PP.string ppstrm ")";
	 PP.closeBox ppstrm)
      | ppDecon _ _ = ()

    fun ppApp ppstrm prfun sepfun [] = ()
      | ppApp ppstrm prfun sepfun (x::xs) =
	(prfun ppstrm x;  app (fn y => (sepfun ppstrm; prfun ppstrm y)) xs)

    (** the definitions of the lambda expressions *)

    fun complex (F.LET _ | F.FIX _ | F.TFN _ | F.SWITCH _ | F.CON _ | F.HANDLE _) = true
      | complex _ = false

    fun ppLexp ppstrm (F.RET values, pd) =
	(* RETURN [values] *)
	(PP.string ppstrm "RETURN "; ppValList ppstrm values)

      | ppLexp ppstrm (F.APP (f, args), pd) =
	(* APP(f, [args]) *)
	(PP.string ppstrm "APP(";
	 ppValue ppstrm f;
	 PP.string ppstrm ",";
	 ppValList ppstrm args;
	 PP.string ppstrm ")")

      | ppLexp ppstrm ppstrm (F.TAPP (tf, tycs), pd) =
	(* TAPP(tf, [tycs]) *)
	(PP.string ppstrm "TAPP(";
	 ppValue ppstrm tf;
	 PP.string ppstrm ",";
	 ppTycList ppstrm tycs;
	 PP.string ppstrm ")")

      | ppLexp ppstrm (F.LET (vars, lexp, body), pd) =
	(* [vars] = lexp   OR   [vars] =
	 *   body                 lexp
	 *                      body
	 *)
	(PP.openVBox ppstr (PP.Abs 0);
	 ppVarList ppstrm vars; PP.string ppstrm " = ";
	 if complex lexp
	 then (PP.openVBox ppstrm (PP.Abs 2);
	       PP.cut ppstrm;
	       ppLexp ppstrm (lexp, pd-1);
	       PP.closeBox ppstrm)
	 else
	     let val len = (3		(* for the " = " *)
			    + 2		(* for the "[]" *)
			    + (length vars) (* for each comma *)
			    + (foldl	(* sum of varname lengths *)
			       (fn (v,n) => n + (size (!LVarString v)))
			       0 vars))
	     in PP.openHVBox ppstrm (PP.Abs len);
		ppLexp ppstrm (lexp, pd-1);
		PP.closeBox ppstrm
	     end;
         PP.cut ppstrm;
	 ppLexp ppstrm (body, pd-1);
	 PP.closeBox ppstrm)

      | ppLexp ppstrm (F.FIX (fundecs, body), pd) =
      (* FIX(<fundec1>,
       *     <fundec2>,
       *     <fundec3>)
       * <body>
       *)
	(PP.openVBox ppstrm (PP.Abs 0);
	  PP.string ppstrm "FIX(";
	  PP.openVBox ppstrm (PP.Abs 4);
	   ppApp ppFundec PP.cut fundecs;
	   PP.string ppstrm ")";
	  PP.closeBox ppstrm;
	  PP.cut ppstrm;
	  ppLexp ppstrm body
	 PP.closeBox ppstrm)

      | ppLexp ppstrm (F.TFN ((tfk as {inline}, lvar, tv_tk_list, tfnbody), body), pd) =
	(* v =
	 *   TFN([tk],lty,
	 *     <tfnbody>)
	 * <body>
	 *)
	(PP.openVBox ppstrm (PP.Abs 2);
	  ppVar ppstrm lvar; PP.string ppstrm " = ";
	  PP.cut ppstrm;
	  PP.string ppstrm "TFN";
	  PP.string ppstrm
           (case inline
	      of IH_SAFE => "s "
	       | IH_ALWAYS => "a "
	       | IH_UNROLL => "u "
	       | IH_MAYBE _ => "m ");
	  ppTvTkList ppstrm tv_tk_list; PP.string ppstrm ".";
	 (*** ppLty ppstrm lty; PP.string ppstrm ","; *** lty no longer available ***)
          PP.openVBox ppstrm (PP.Abs 2);
           PP.cut ppstrm;
	   ppLexp ppstrm tfnbody;
          PP.closeBox ppstrm;
	  PP.cut ppstrm;
	  ppLexp ppstrm (body, pd-1);
	 PP.closeBox ppstrm)

      (** NOTE: I'm ignoring the consig here **)
      | ppLexp ppstrm (F.SWITCH (value, consig, con_lexp_list, lexpOption), pd) =
	(* SWITCH <value>
	 *   <con> =>
	 *       <lexp>
	 *   <con> =>
	 *       <lexp>
	 *)
	 (PP.openVBox ppstrm (PP.Abs 2);
	   PP.string ppstrm "SWITCH "; ppValue ppstrm value;
	   PP.cut ppstrm;
	   ppApp ppCase PP.cut con_lexp_list;
	   case  lexpOption
	    of NONE => ()
	     | SOME lexp =>		(* default case *)
	         (PP.cut ppstrm; PP.string ppstrm "_ => ";
		  PP.openVBox ppstrm (PP.abs 4);
		   PP.cut ppstrm;
		   ppLexp ppstrm (lexp, pd-1);
		  PP.closeBox ppstrm);
	  PP.closeBox ppstrm)

      | ppLexp ppstrm (F.CON ((symbol,_,_), tycs, value, lvar, body), pd) =
	 (* <lvar> = CON(<symbol>, <tycs>, <value>)
	  * <body>
	  *)
	 (PP.openVBox ppstrm (PP.Abs 0);
	   ppVar ppstrm lvar; PP.string ppstrm " = CON(";
	   PP.string ppstrm (S.name symbol); PP.string ppstrm ", ";
	   ppTycList ppstrm tycs;  PP.string ppstrm ", ";
	   ppValue ppstrm value;  PP.string ppstrm ")";
           PP.cut ppstrm;
	   ppLexp ppstrm (body, pd-1);
	  PP.closeBox ppstrm)

      | ppLexp ppstrm (F.RECORD (rkind, values, lvar, body), pd) =
	 (* <lvar> = RECORD(<rkind>, <values>)
	  * <body>
	  *)
	(PP.openVBox ppstrm (PP.Abs 0);
	  ppVar ppstrm lvar;  PP.string ppstrm " = ";
	  printRKind rkind; PP.string ppstrm " ";
	  ppValList ppstrm values;
	  PP.cut ppstrm;
	  ppLexp ppstrm (body, pd-1);
	 PP.closeBox ppstrm)

      | ppLexp ppstrm (F.SELECT (value, int, lvar, body), pd) =
	 (* <lvar> = SELECT(<value>, <int>)
	  * <body>
	  *)
	(PP.openVBox ppstrm (PP.Abs 0);
	  ppVar ppstrm lvar;  PP.string ppstrm " = SELECT(";
	  ppValue ppstrm value;  PP.string ppstrm ", ";
	  PP.string ppstrm (Int.toString int);  PP.string ppstrm ")";
	  PP.cut ppstrm;
	  ppLexp ppstrm (body, pd-1);
	 PP.closeBox ppstrm)

      | ppLexp ppstrm (F.RAISE (value, ltys), pd) =
	 (* NOTE: I'm ignoring the lty list here. It is the return type
	  * of the raise expression. (ltys temporarily being printed --v)
	  *)
	 (* RAISE(<value>) *)
	 (PP.string ppstrm "RAISE(";
	  ppValue ppstrm value; PP.string ppstrm ") : "; ppLtyList ppstrm ltys)

      | ppLexp ppstrm (F.HANDLE (body, value), pd) =
	 (* <body>
	  * HANDLE(<value>)
	  *)
	(PP.openVBox ppstrm (PP.Abs 0);
	  ppLexp ppstrm (body, pd-1);
	  PP.cut ppstrm;
	  PP.string ppstrm "HANDLE(";  ppValue ppstrm value;  PP.string ppstrm ")";
	 PP.closeBox ppstrm)

      | ppLexp ppstrm (F.BRANCH ((d, primop, lty, tycs), values, body1, body2), pd) =
	 (* IF PRIM(<primop>, <lty>, [<tycs>]) [<values>]
          * THEN
	  *   <body1>
          * ELSE
	  *   <body2>
	  *)
	(PP.openVBox ppstrm (PP.Abs 0);
	  case d
	    of NONE => PP.string ppstrm "IF PRIMOP("
             | _ => PP.string ppstrm "IF GENOP(";
	  PP.string ppstrm (PrimopUtil.toString primop);  PP.string ppstrm ", ";
	  ppLty ppstrm lty;  PP.string ppstrm ", ";
	  ppTycList ppstrm tycs;  PP.string ppstrm ") ";
	  ppValList ppstrm values;
          PP.cut ppstrm;
          ppApp printBranch PP.cut [("THEN", body1), ("ELSE", body2)];
         PP.closeBox ppstrm)

      | ppLexp ppstrm (F.PRIMOP (p as (_, PO.MKETAG, _, _), [value], lvar, body), pd) =
	 (* <lvar> = ETAG(<value>[<tyc>])
	  * <body>
	  *)
	(PP.openVBox ppstrm (PP.Abs 0);
	  ppVar ppstrm lvar;  PP.string ppstrm " = ETAG(";
	  ppValue ppstrm value;  PP.string ppstrm "[";
	  ppTyc ppstrm (FU.getEtagTyc p);  PP.string ppstrm "])";
	  PP.cut ppstrm;
	  ppLexp ppstrm (body, pd-1);
         PP.closeBox ppstrm)

      | ppLexp ppstrm (F.PRIMOP (p as (_, PO.WRAP, _, _), [value], lvar, body), pd) =
	 (* <lvar> = WRAP(<tyc>, <value>)
	  * <body>
	  *)
	(PP.openVBox ppstrm (PP.Abs 0);
	  ppVar ppstrm lvar;  PP.string ppstrm " = WRAP(";
	  ppTyc ppstrm (FU.getWrapTyc p);  PP.string ppstrm ", ";
	  ppValue ppstrm value;  PP.string ppstrm ")";
	  PP.cut ppstrm;
	  ppLexp ppstrm (body, pd-1);
         PP.closeBox ppstrm)

      | ppLexp ppstrm (F.PRIMOP (p as (_, PO.UNWRAP, _, []), [value], lvar, body), pd) =
	 (* <lvar> = UNWRAP(<tyc>, <value>)
	  * <body>
	  *)
	(PP.openVBox ppstrm (PP.Abs 0);
	  ppVar ppstrm lvar;  PP.string ppstrm " = UNWRAP(";
	  ppTyc ppstrm (FU.getUnWrapTyc p);  PP.string ppstrm ", ";
	  ppValue ppstrm value;  PP.string ppstrm ")";
	  PP.cut ppstrm;
	  ppLexp ppstrm (body, pd-1);
         PP.closeBox ppstrm)

      | ppLexp ppstrm (F.PRIMOP ((d, primop, lty, tycs), values, lvar, body), pd) =
	 (* <lvar> = PRIM(<primop>, <lty>, [<tycs>]) [<values>]
	  * <body>
	  *)
	(PP.openVBox ppstrm (PP.Abs 0);
	  ppVar ppstrm lvar;
          (case d
	     of NONE => PP.string ppstrm " = PRIMOP("
              | _ => PP.string ppstrm " = GENOP(" );
	  PP.string ppstrm (PrimopUtil.toString primop);  PP.string ppstrm ", ";
	  ppLty ppstrm lty;  PP.string ppstrm ", ";
	  ppTycList ppstrm tycs;  PP.string ppstrm ") ";
	  ppValList ppstrm values;
	  PP.cut ppstrm;
	  ppLexp ppstrm (body, pd-1);
         PP.closeBox ppstrm)

    and ppFundec ppstrm (fkind as {cconv,...}, lvar, lvar_lty_list, body, pd) =
	(*  <lvar> : (<fkind>) <lty> =
	 *    FN([v1 : lty1,
	 *        v2 : lty2],
	 *      <body>)
	 *)
	let fun ppargs nil = ()
	      | ppargs ((lvar,lty)::rest) =
		  (PP.openVBox ppstrm (PP.Abs 0);
		    ppVar ppstrm lvar; PP.string ppstrm " : ";
		    if !CTRL.printFctTypes orelse cconv <> F.CC_FCT
		    then ppLty ppstrm lty
		    else PP.string ppstrm "???";
		    if null rest
		    then (PP.string ppstrm "]"; PP.closeBox)
		    else (PP.cut ppstrm; ppargs rest))
         in PP.openVBox ppstrm (PP.Abs 2);ppVar ppstrm lvar; PP.string ppstrm " : ";
	     PP.string ppstrm "("; ppFKind ppstrm fkind; PP.string ppstrm ") ";
	     (*** the result lty no longer available ---- ppLty ppstrm lty; **)
	     PP.string ppstrm " = ";
	     PP.cut ppstrm;
	     PP.string ppstrm "FN([";
	     ppargs lvar_lty_list;
	     PP.openVBox ppstrm (PP.Abs 2);
	      PP.cut ppstrm;
	      ppLexp ppstrm (body, pd-1); PP.string ppstrm ")";
	     PP.closeBox ppstrm;
	    PP.closeBox ppstrm
	end

    and ppCase ppstrm (con, lexp, pd) =
	(PP.openVBox ppstrm (PP.Abs 2);
	   PP.cut ppstrm;
	   ppCon ppstrm con;
	   PP.string ppstrm " => ";
	   PP.openVBox ppstrm (PP.Abs 4);
	     PP.cut ppstrm;
	     ppDecon ppstrm con;
	     PP.cut ppstrm;
	     ppLexp ppstrm (lexp, pd-1);
	   PP.closeBox ppstrm;
	 PP.closeBox ppstrm)

    and ppBranch ppstrm (s, lexp, pd) =
	(PP.openVBox ppstrm (PP.Abs 4);
	   PP.string ppstrm s;
	   PP.cut ppstrm;
	   ppLexp ppstrm (lexp, pd-1);
	 PP.closeBox ppstrm)

    fun printLexp lexp =
	PP.with_pp (PP.mkDevice (!CTRL.lineWidth))
	  (fn ppstrm => (ppLexp ppstrm (lexp, !CTRL.printDepth)))

    fun printLexpLimited (lexp, printDepth) =
	PP.with_pp (PP.mkDevice (!CTRL.lineWidth))
		   (fn ppstrm => (ppLexp ppstrm (lexp, printDepth)))

    fun printProg prog =
	PP.with_pp (PP.mkDevice (!CTRL.LineWidth))
		   (fn ppstrm => (ppFundec ppstrm (prog, !CTRL.printDepth)))

    fun printProgLimited (prog, printDepth) =
	PP.with_pp (PP.mkDevice (!CTRL.LineWidth))
		   (fn ppstrm => (ppFundec ppstrm (prog, printDepth)))

end (* local *)
end (* structure PPFlint *)

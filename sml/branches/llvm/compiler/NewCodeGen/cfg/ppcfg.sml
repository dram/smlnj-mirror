(* ppcfg.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PPCfg : sig

    val prCluster : CFG.cluster -> unit

    val expToString : CFG.exp -> string

  (* string conversions for various CFG_Prim types *)
    val allocToString : CFG_Prim.alloc -> string
    val arithopToString : CFG_Prim.arithop -> string
    val pureopToString : CFG_Prim.pureop -> string
    val cmpopToString : CFG_Prim.cmpop -> string
    val fcmpopToString : CFG_Prim.fcmpop -> string
    val branchToString : CFG_Prim.branch -> string
    val setterToString : CFG_Prim.setter -> string
    val lookerToString : CFG_Prim.looker -> string
    val arithToString : CFG_Prim.arith -> string
    val pureToString : CFG_Prim.pure -> string

  end = struct

    structure C = CFG
    structure LV = LambdaVar
    structure P = CFG_Prim

    val say = Control.Print.say

    fun numkindToString (P.INT, bits) = ["i", Int.toString bits]
      | numkindToString (P.UINT, bits) = ["u", Int.toString bits]
      | numkindToString (P.FLOAT, bits) = ["f", Int.toString bits]

    val arithopToString = ArithOps.arithopToString
    val cmpopToString = PPCps.cmpopToString
    val fcmpopToString = PPCps.fcmpopToString
    val branchToString = PPCps.branchToString

    fun allocToString (P.RECORD{tag, mut}) =
      | allocToString (P.RAW_RECORD{tag, align}) =
      | allocToString (P.RAW_ALLOC{tag, align, len}) =

    fun setterToString P.UNBOXED_UPDATE = "unboxedupdate"
      | setterToString P.UPDATE = "update"
      | setterToString P.UNBOXED_ASSIGN = "unboxedassign"
      | setterToString P.ASSIGN = "assign"
      | setterToString (P.RAW_UPDATE{kind, size}) =
      | setterToString P.SETHDLR = "sethdlr"
      | setterToString P.SETVAR = "setvar"

    fun lookerToString P.DEREF = "!"
      | lookerToString P.SUBSCRIPT = "subscript"
      | lookerToString (P.RAW_SUBSCRIPT{kind, size}) =
      | lookerToString P.GETHDLR = "gethdlr"
      | lookerToString P.GETVAR = "getvar"

    fun arithToString (P.IARITH{oper, sz}) = arithopToString oper ^ cvtParam sz
      | arithToString (P.TEST{from, to}) = cvtParams ("test_", from, to)
      | arithToString (P.TESTU{from, to}) = cvtParams ("testu_", from, to)
      | arithToString (P.REAL_TO_INT{floor, from, to}) = concat[
	    if floor then "floor_" else "round_", cvtParam from, "to", cvtParam to
	  ]

    fun pureToString (P.PURE_ARITH{oper, size}) =
	  pureopToString oper ^ Int.toString size
      | pureToString (P.COPY{from, to}) = cvtParams ("copy_", from, to)
      | pureToString (P.EXTEND{from, to}) = cvtParams ("extend_", from, to)
      | pureToString (P.TRUNC{from, to}) = cvtParams ("trunc_", from, to)
      | pureToString (P.INT_TO_REAL{from, to}) =
	  concat ["real", cvtParam from, "_", cvtParam to]
      | pureToString (P.LOAD_WORD{offset, ty}) =
      | pureToString (P.LOAD_RAW{offset, kind, sz}) =
      | pureToString P.PURE_SUBSCRIPT = "subscriptv"
      | pureToString (P.PURE_RAW_SUBSCRIPT{kind, sz}) =

    fun space n = say(String.tabulate(n, fn _ => #" "))

    fun sayList f [x] = f x | saylist f nil = ()
      | sayList f (x::r) = (f x; say ","; saylist f r)

    fun expToString e = (case e
	   of C.VAR x => LV.lvarName x
	    | C.LABEL lab => "(L)" ^ LV.lvarName lab
	    | C.NUM{iv, signed=true, sz} =>
		concat["(i", Int.toString sz, ")", IntInf.toString iv]
	    | C.NUM{iv, signed=false, sz} =>
		concat["(u", Int.toString sz, ")", IntInf.toString iv]
	    | C.LOOKER(p, args) => appToS(lookerToString p, args)
	    | C.PURE(p, args) => appToS(pureToString p, args)
	    | C.SELECT(i, e) => appToS("#" ^ Int.toString i, [e])
	    | C.OFFSET(i, e) => appToS("@" ^ Int.toString i, [e])
	  (* end case *))

    and appToS (prefix, es) = String.concat[
	    prefix, "(", String.concatWithMap "," expToString es, ")"
	  ]

    fun sayv x = say(LV.lvarName x)

    fun sayList sayItem [] = say "()"
      | sayList sayItem [item] = (say "("; sayItem item; say ")")
      | sayList sayItem (fst::rest) = (
	  say "("; sayItem fst;
	  List.app (fn item => (say ","; sayItem item)) rest;
	  say ")")

    fun sayParam (x, cty) = (sayv w; say ":"; sayTy ty)

    fun prStm n = let
	  val sayArgs = sayList prExp
	  fun sayApp (prefix, args) = (say(appToS(prefix, args)); say "\n")
	  fun sayTy cty = say(CFGUtil.tyToString cty)
	  fun pr stm = (
		space n;
		case stm
		 of C.LET(e, x, stm) => (prExp e; say " -> "; sayv x; say "\n"; pr stm)
		  | C.ALLOC(P.RECORD{tag, mut}, args, x, stm) =>
		  | C.ALLOC(P.RAW_RECORD{tag, align}, args, x, stm) =>
		  | C.ALLOC(P.RAW_ALLOC{tag, align, len}, args, x, stm) =>
		  | C.APP(cc, f, args) => (
		      case cc
		       of C.STD_FUN => say "std_apply "
			| C.STD_CONT => say "std_throw "
			| C.KNOWN => say "known_apply "
		      (* end case *);
		      say (appToS (expToString f, args)); say "\n")
		  | C.GOTO(lab, args) => (
		      say (appToS ("goto (L)" ^ LV.lvarName lab, args)); say "\n")
		  | C.SWITCH(arg, cases) =>  let
		      fun sayCase (i, e) = (
			    space n; say "case "; say(Int.toString i);
			    say ":\n"; prStm (n+2) e)
		      in
			space n; say "switch ("; say(expToString arg); say ") {\n";
			List.appi sayCase cases;
			space n; say "}\n"
		      end
		  | C.BRANCH(p, args, stm1, stm2) => (
		      say "if "; sayApp (branchToString p, args); say " {\n";
		      prStm (n+2, stm1);
		      space n; say "} else {\n";
		      prStm (n+2, stm2);
		      space n; say "}\n")
		  | C.ARITH(p, args, x, stm) => (
		      say (arithToString p, args); say " -> "; sayv x; say "\n")
		  | C.SETTER(p, args, x, stm) => (
		      space n; say(setterToString i); say "("; sayvlist vl;
		      say ")"; nl(); f e)
		  | C.RCC(rk, f, proto, args, results, stm) => (
		      if k = REENTRANT_RCC
			then say "reentrant_cc "
			else say "fast_cc ";
		      if f = "" then () else (say l; say " ");
		      say "("; sayvlist vl; say ") -> ";
		      sayList sayParam results;
		      say "\n")
		(* end case *))
	  in
	    pr stm
	  end

    fun prFrag n (fk, lab, params, stm) = (
	  space n;
	  case fk
	   of C.GC_CHECK => say "GC_CHECK ";
	    | C.INTERNAL => ()
	  (* end case *);
	  say "(L)"; sayv lab; say " "; sayList sayParam params; say " {\n";
	  prStm (n+2) stm;
	  space n; say "}\n")

    fun prCluster ((cc, f, params, stm), frags) = (
	  case cc
	   of C.STD_FUN => say "std_fun "
	    | C.STD_CONT => say "std_cont "
	    | C.KNOWN => say "known "
	  (* end case *);
	  say "(L)"; sayv f; say " "; sayList sayParam params; say " {\n";
	  say "  entry {\n";
	  prStm (n+4) stm;
	  say "  }\n";
	  List.app prFrag 2 frags;
	  say "}\n")

  end

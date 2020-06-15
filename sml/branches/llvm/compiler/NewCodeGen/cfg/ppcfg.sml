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

    val i2s = Int.toString

    fun numkindToString (P.INT, bits) = ["i", i2s bits]
      | numkindToString (P.UINT, bits) = ["u", i2s bits]
      | numkindToString (P.FLT, bits) = ["f", i2s bits]

    val arithopToString = ArithOps.arithopToString
    val cmpopToString = PPCps.cmpopToString
    val fcmpopToString = PPCps.fcmpopToString
    val branchToString = PPCps.branchToString

    fun allocToString (P.RECORD{tag, mut=false}) =
	  concat["record[0x", IntInf.fmt StringCvt.HEX tag, "]"]
      | allocToString (P.RECORD{tag, mut=true}) =
	  concat["mut_record[0x", IntInf.fmt StringCvt.HEX tag, "]"]
      | allocToString (P.RAW_RECORD{tag, align}) = concat[
	    "raw_record", i2s align, "[0x", IntInf.fmt StringCvt.HEX tag, "]"
	  ]
      | allocToString (P.RAW_ALLOC{tag, align, len}) = concat[
	    "raw_alloc", i2s align, "[0x", IntInf.fmt StringCvt.HEX tag,
	    ";", i2s len, "]"
	  ]

    fun setterToString P.UNBOXED_UPDATE = "unboxedupdate"
      | setterToString P.UPDATE = "update"
      | setterToString P.UNBOXED_ASSIGN = "unboxedassign"
      | setterToString P.ASSIGN = "assign"
      | setterToString (P.RAW_UPDATE{kind, sz}) =
	  concat("update_" :: numkindToString(kind, sz))
      | setterToString P.SETHDLR = "sethdlr"
      | setterToString P.SETVAR = "setvar"

    fun lookerToString P.DEREF = "!"
      | lookerToString P.SUBSCRIPT = "array_sub"
      | lookerToString (P.RAW_SUBSCRIPT{kind, sz}) =
	  concat("array_sub_" :: numkindToString(kind, sz))
      | lookerToString P.GETHDLR = "gethdlr"
      | lookerToString P.GETVAR = "getvar"

    fun cvtParams (prefix, from, to) =
	  concat[prefix, "_", i2s from, "_to_", i2s to]

    fun arithToString (P.ARITH{oper, sz}) = arithopToString oper ^ i2s sz
      | arithToString (P.TEST{from, to}) = cvtParams ("test_", from, to)
      | arithToString (P.TESTU{from, to}) = cvtParams ("testu_", from, to)
      | arithToString (P.REAL_TO_INT{floor, from, to}) =
	  cvtParams (if floor then "floor" else "round", from, to)

    fun pureopToString rator = (case rator
	   of P.ADD => "ADD"
	    | P.SUB => "SUB"
	    | P.MUL => "MUL"
	    | P.SDIV => "SDIV"
	    | P.SREM => "SREM"
	    | P.UDIV => "UDIV"
	    | P.UREM => "UREM"
	    | P.LSHIFT => "LSHIFT"
	    | P.RSHIFT => "RSHIFT"
	    | P.RSHIFTL => "RSHIFTL"
	    | P.ORB => "ORB"
	    | P.XORB => "XORB"
	    | P.ANDB => "ANDB"
	    | P.FADD => "FADD"
	    | P.FSUB => "FSUB"
	    | P.FMUL => "FMUL"
	    | P.FDIV => "FDIV"
	    | P.FABS => "FABS"
	    | P.FSQRT => "FSQRT"
	  (* end case *))

    fun pureToString (P.PURE_ARITH{oper, sz}) = pureopToString oper ^ i2s sz
      | pureToString (P.COPY{from, to}) = cvtParams ("copy_", from, to)
      | pureToString (P.EXTEND{from, to}) = cvtParams ("extend_", from, to)
      | pureToString (P.TRUNC{from, to}) = cvtParams ("trunc_", from, to)
      | pureToString (P.INT_TO_REAL{from, to}) = cvtParams ("real", from, to)
      | pureToString (P.LOAD_WORD{offset}) =
	  concat ["load[@", i2s offset, "]"]
      | pureToString (P.LOAD_RAW{offset, kind, sz}) =
	  concat("load_" :: numkindToString(kind, sz)
	    @ ["[@", i2s offset, "]"])
      | pureToString P.PURE_SUBSCRIPT = "vector_sub"
      | pureToString (P.PURE_RAW_SUBSCRIPT{kind, sz}) =
	  concat("vector_sub_" :: numkindToString(kind, sz))

    fun space n = say(CharVector.tabulate(n, fn _ => #" "))

    fun expToString e = (case e
	   of C.VAR x => LV.lvarName x
	    | C.LABEL lab => "(L)" ^ LV.lvarName lab
	    | C.NUM{iv, signed=true, sz} =>
		concat["(i", i2s sz, ")", IntInf.toString iv]
	    | C.NUM{iv, signed=false, sz} =>
		concat["(u", i2s sz, ")", IntInf.toString iv]
	    | C.LOOKER(p, args) => appToS(lookerToString p, args)
	    | C.PURE(p, args) => appToS(pureToString p, args)
	    | C.SELECT(i, e) => appToS("#" ^ i2s i, [e])
	    | C.OFFSET(i, e) => appToS("@" ^ i2s i, [e])
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

    fun sayTy cty = say(CFGUtil.tyToString cty)

    fun sayParam (x, ty) = (sayv x; say ":"; sayTy ty)

    fun prStm n = let
	  fun sayApp (prefix, args) = (say(appToS(prefix, args)); say "\n")
	  fun pr stm = (
		space n;
		case stm
		 of C.LET(e, x, stm) => (
		      say(expToString e); say " -> "; sayv x; say "\n"; pr stm)
		  | C.ALLOC(p as P.RAW_ALLOC _, [], x, stm) => (
		      say (allocToString p); say " -> "; sayv x; say "\n"; pr stm)
		  | C.ALLOC(p, args, x, stm) => (
		      sayApp (allocToString p, args);
		      say " -> "; sayv x; say "\n"; pr stm)
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
			    space n; say "case "; say(i2s i);
			    say ":\n"; prStm (n+2) e)
		      in
			space n; say "switch ("; say(expToString arg); say ") {\n";
			List.appi sayCase cases;
			space n; say "}\n"
		      end
		  | C.BRANCH(p, args, stm1, stm2) => (
		      say "if "; sayApp (branchToString p, args); say " {\n";
		      prStm (n+2) stm1;
		      space n; say "} else {\n";
		      prStm (n+2) stm2;
		      space n; say "}\n")
		  | C.ARITH(p, args, x, stm) => (
		      sayApp (arithToString p, args);
		      say " -> "; sayv x; say "\n"; pr stm)
		  | C.SETTER(p, args, stm) => (
		      sayApp (setterToString p, args); say "\n"; pr stm)
		  | C.RCC(rk, f, proto, args, results, stm) => (
		      if rk = C.REENTRANT_RCC
			then say "reentrant_cc "
			else say "fast_cc ";
		      if f = "" then () else (say f; say " ");
		      sayList (fn e => say(expToString e)) args;
		      say " -> "; sayList sayParam results; say "\n";
		      pr stm)
		(* end case *))
	  in
	    pr
	  end

    fun prFrag n (fk, lab, params, stm) = (
	  space n;
	  case fk
	   of C.GC_CHECK => say "GC_CHECK "
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
	  prStm 4 stm;
	  say "  }\n";
	  List.app (prFrag 2) frags;
	  say "}\n")

  end

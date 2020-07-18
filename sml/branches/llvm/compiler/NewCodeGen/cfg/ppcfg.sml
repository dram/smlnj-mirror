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
      | numkindToString (P.FLT, bits) = ["f", i2s bits]

    val cmpopToString = ArithOps.cmpopToString
    val fcmpopToString = PPCps.fcmpopToString

    fun arithopToString oper = (case oper
	   of P.IADD => "IADD"
	    | P.ISUB => "ISUB"
	    | P.IMUL => "IMUL"
	    | P.IDIV => "IDIV"
	    | P.IREM => "IREM"
	  (* end case *))

    fun branchToString oper = (case oper
	   of P.CMP{oper, signed, sz} => concat[
		  cmpopToString oper, if signed then "_i" else "_u",
		  Int.toString sz
		]
	    | P.FCMP{oper, sz} => concat[fcmpopToString oper, "_f", Int.toString sz]
	    | P.FSGN sz => concat["f", Int.toString sz, "sgn"]
	    | P.PEQL => "peql"
	    | P.PNEQ => "pneq"
	    | P.STREQL n => "streql" ^ Int.toString n
	  (* end case *))

    fun allocToString (P.RECORD{desc, mut=false}) =
	  concat["record[0x", IntInf.fmt StringCvt.HEX desc, "]"]
      | allocToString (P.RECORD{desc, mut=true}) =
	  concat["mut_record[0x", IntInf.fmt StringCvt.HEX desc, "]"]
      | allocToString (P.RAW_RECORD{desc, kind, sz}) = concat(
	  "raw_" :: numkindToString(kind, sz) @ [
	      "_record[0x", IntInf.fmt StringCvt.HEX desc, "]"
	    ])
      | allocToString (P.RAW_ALLOC{desc, align, len}) = concat(
	  "raw_" :: i2s align :: "_alloc[" ::
	  (case desc
	   of SOME d => ["0x", IntInf.fmt StringCvt.HEX d, ";"]
	    | _ => []
	  (* end case *)) @ [i2s len, "]"])

    fun setterToString P.UNBOXED_UPDATE = "unboxedupdate"
      | setterToString P.UPDATE = "update"
      | setterToString P.UNBOXED_ASSIGN = "unboxedassign"
      | setterToString P.ASSIGN = "assign"
      | setterToString (P.RAW_UPDATE{kind, sz}) =
	  concat("update_" :: numkindToString(kind, sz))
      | setterToString (P.RAW_STORE{kind, sz}) =
	  concat("store_" :: numkindToString(kind, sz))
      | setterToString P.SET_HDLR = "sethdlr"
      | setterToString P.SET_VAR = "setvar"

    fun lookerToString P.DEREF = "!"
      | lookerToString P.SUBSCRIPT = "array_sub"
      | lookerToString (P.RAW_SUBSCRIPT{kind, sz}) =
	  concat("array_sub_" :: numkindToString(kind, sz))
      | lookerToString (P.RAW_LOAD{kind, sz}) =
	  concat("load_" :: numkindToString(kind, sz))
      | lookerToString P.GET_HDLR = "gethdlr"
      | lookerToString P.GET_VAR = "getvar"

    fun cvtParams (prefix, from, to) =
	  concat[prefix, "_", i2s from, "_to_", i2s to]

    fun arithToString (P.ARITH{oper, sz}) = arithopToString oper ^ i2s sz
      | arithToString (P.TEST{from, to}) = cvtParams ("test_", from, to)
      | arithToString (P.TESTU{from, to}) = cvtParams ("testu_", from, to)
      | arithToString (P.REAL_TO_INT{mode, from, to}) = let
	  fun toS prefix = concat[prefix, i2s from, "_i", i2s to]
	  in
	    case mode
	     of P.TO_NEAREST => toS "round_f"
	      | P.TO_NEGINF => toS "floor_f"
	      | P.TO_POSINF => toS "ceil_f"
	      | P.TO_ZERO => toS "trunc_f"
	    (* end case *)
	  end

    fun pureopToString rator = (case rator
	   of P.ADD => "add"
	    | P.SUB => "sub"
	    | P.SMUL => "smul"
	    | P.SDIV => "sdiv"
	    | P.SREM => "srem"
	    | P.UMUL => "umul"
	    | P.UDIV => "udiv"
	    | P.UREM => "urem"
	    | P.LSHIFT => "lshift"
	    | P.RSHIFT => "rshift"
	    | P.RSHIFTL => "rshiftl"
	    | P.ORB => "orb"
	    | P.XORB => "xorb"
	    | P.ANDB => "andb"
	    | P.FADD => "fadd"
	    | P.FSUB => "fsub"
	    | P.FMUL => "fmul"
	    | P.FDIV => "fdiv"
	    | P.FNEG => "fneg"
	    | P.FABS => "fabs"
	    | P.FSQRT => "fsqrt"
	  (* end case *))

    fun pureToString (P.PURE_ARITH{oper, sz}) = pureopToString oper ^ i2s sz
      | pureToString (P.EXTEND{signed=true, from, to}) =
	  cvtParams ("sign_extend_", from, to)
      | pureToString (P.EXTEND{signed=false, from, to}) =
	  cvtParams ("zero_extend_", from, to)
      | pureToString (P.INT_TO_REAL{from, to}) = cvtParams ("real", from, to)
      | pureToString P.PURE_SUBSCRIPT = "vector_sub"
      | pureToString (P.PURE_RAW_SUBSCRIPT{kind, sz}) =
	  concat("vector_sub_" :: numkindToString(kind, sz))

    fun space n = say(CharVector.tabulate(n, fn _ => #" "))

    fun expToString e = (case e
	   of C.VAR x => LV.lvarName x
	    | C.LABEL lab => "L_" ^ LV.lvarName lab
	    | C.NUM{iv, sz} =>
		concat["(i", i2s sz, ")", IntInf.toString iv]
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
    fun sayArg (e, ty) = (say(expToString e); say ":"; sayTy ty)

    fun sayArgs ([], []) = say "()"
      | sayArgs (arg::args, ty::tys) = (
	  say "("; sayArg (arg, ty);
	  ListPair.app (fn arg => (say ","; sayArg arg)) (args, tys);
	  say ")")

    fun prStm n = let
	  fun sayExp e = say(expToString e)
	  fun sayApp (prefix, args) = (say(appToS(prefix, args)); say "\n")
	  fun pr stm = (
		space n;
		case stm
		 of C.LET(e, x, stm) => (
		      say(expToString e); say " -> "; sayParam x; say "\n"; pr stm)
		  | C.ALLOC(p as P.RAW_ALLOC _, [], x, stm) => (
		      say (allocToString p); say " -> "; sayv x; say "\n"; pr stm)
		  | C.ALLOC(p, args, x, stm) => (
		      sayApp (allocToString p, args);
		      say " -> "; sayv x; say "\n"; pr stm)
		  | C.APPLY(args as f::_, tys) => (
		      say "apply "; sayExp f; sayArgs (args, tys); say "\n")
		  | C.APPLY _ => raise Fail "malformed APPLY"
		  | C.THROW(f::args, fTy::tys) => (
		      say "throw "; sayExp f; sayArgs (args, tys); say "\n")
		  | C.THROW _ => raise Fail "malformed THROW"
		  | C.GOTO(cc, lab, args, tys) => (
		      case cc
		       of C.STD_FUN => say "goto std_fun "
			| C.STD_CONT => say "goto std_cont "
			| C.KNOWN_CHK => say "goto known_chk "
			| C.KNOWN => say "goto known "
		      (* end case *);
		      say ("L_" ^ LV.lvarName lab); sayArgs (args, tys); say "\n")
		  | C.SWITCH(arg, cases) =>  let
		      fun sayCase (i, e) = (
			    space n; say "case "; say(i2s i);
			    say ":\n"; prStm (n+2) e)
		      in
			space n; say "switch ("; say(expToString arg); say ") {\n";
			List.appi sayCase cases;
			space n; say "}\n"
		      end
		  | C.BRANCH(p, args, 0, stm1, stm2) => (
		      say "if "; sayApp (branchToString p, args); say " {\n";
		      prStm (n+2) stm1;
		      space n; say "} else {\n";
		      prStm (n+2) stm2;
		      space n; say "}\n")
		  | C.BRANCH(p, args, prob, stm1, stm2) => (
		      say "if "; sayApp (branchToString p, args);
		      say " { ["; say(Int.toString prob); say "/100]\n";
		      prStm (n+2) stm1;
		      space n; say "} else { [";
		      say(Int.toString(100-prob)); say "/100]\n";
		      prStm (n+2) stm2;
		      space n; say "}\n")
		  | C.ARITH(p, args, x, stm) => (
		      sayApp (arithToString p, args);
		      say " -> "; sayParam x; say "\n"; pr stm)
		  | C.SETTER(p, args, stm) => (
		      sayApp (setterToString p, args); say "\n"; pr stm)
		  | C.RCC{reentrant, linkage, proto, args, results, live, k} => (
		      if reentrant
			then say "reentrant c_call "
			else say "c_call ";
		      if linkage = "" then () else (say linkage; say " ");
		      sayList (fn e => say(expToString e)) args;
		      say " -> "; sayList sayParam results; say "\n";
(* FIXME: print live set too *)
		      pr k)
		(* end case *))
	  in
	    pr
	  end

    fun prFrag n (C.Frag(fk, lab, params, stm)) = (
	  space n;
	  case fk
	   of C.GC_CHECK => say "GC_CHECK "
	    | C.INTERNAL => ()
	  (* end case *);
	  say "(L)"; sayv lab; say " "; sayList sayParam params; say " {\n";
	  prStm (n+2) stm;
	  space n; say "}\n")

    fun prCluster (C.Cluster(attrs, C.Entry(cc, f, params, stm), frags)) = (
	  case cc
	   of C.STD_FUN => say "std_fun "
	    | C.STD_CONT => say "std_cont "
	    | C.KNOWN_CHK => say "known_chk "
	    | C.KNOWN => say "known "
	  (* end case *);
	  say "(L)"; sayv f; say " "; sayList sayParam params; say " {\n";
	  say "  entry {\n";
	  prStm 4 stm;
	  say "  }\n";
	  List.app (prFrag 2) frags;
	  say "}\n")

  end

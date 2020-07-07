(* word64.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Tests for Word64.
 *)

local
  datatype 'a result = DIV | OVFLW | VAL of 'a

(* evaluate `f arg` and return wrapped result *)
  fun eval f arg = ((VAL(f arg)) handle Overflow => OVFLW | Div => DIV);

  fun resultToString valToString (VAL v) = valToString v
    | resultToString _ OVFLW = "Overflow exception"
    | resultToString _ DIV = "Div exception"

  fun check (same, toString) (f, expected) () = let
	val res = eval f ()
	val toS = resultToString toString
	in
	  case (res, expected)
	   of (VAL v1, VAL v2) => if same(v1, v2)
		then NONE
		else SOME(concat[
		    "expected ", toString v2, ", but got ", toString v1
		  ])
	    | (OVFLW, OVFLW) => NONE
	    | (DIV, DIV) => NONE
	    | _ => SOME(concat["expected ", toS expected, ", but got ", toS res])
	  (* end case *)
	end;

  val chkArith = check (op =, fn w => "0wx"^ Word64.toString w);
  val chkCmp = check (op =, Bool.toString);

  fun runTests tests = let
	val nTests = List.length tests
	val nFails = ref 0
	fun runOne (msg, t) = (case t()
	       of NONE => print(msg ^ ": ok\n")
		| (SOME errMsg) => (
		    nFails := !nFails + 1;
		    print(concat[msg, ": ", errMsg, "\n"]))
	      (* end case *))
	in
	  List.app runOne tests;
	  if (!nFails > 0)
	    then print(concat[
		Int.toString(nTests - !nFails), " / ", Int.toString nTests,
		" tests passed\n"
	      ])
	    else print(concat["all ", Int.toString nTests, " tests passed\n"])
	end

  val inputs : Word64.word list = [
	  0wx0, 0wx1, 0wx2, 0wx3, 0wxF, 0wx11, 0wxffff, 0wx10000, 0wx1ffff,
	  0wx100000001, 0wx800080008000, 0wx1000100010001,
	  0wx7FFF00000000FFFF, 0wxFFFFFFFF0000, 0wxFFFF0000FFFF0000,
	  0wx7FFFFFFFFFFFFFFF, 0wx7FFFFFFFFFFFFFFE, 0wx8000000000000000,
	  0wx8000000000000001, 0wxFFFFFFFFFFFFFFFE, 0wxFFFFFFFFFFFFFFFF
	]
  val inputs' : IntInf.int list = [
	  0x0, 0x1, 0x2, 0x3, 0xF, 0x11, 0xffff, 0x10000, 0x1ffff,
	  0x100000001, 0x800080008000, 0x1000100010001,
	  0x7FFF00000000FFFF, 0xFFFFFFFF0000,0xFFFF0000FFFF0000,
	  0x7FFFFFFFFFFFFFFF, 0x7FFFFFFFFFFFFFFE, 0x8000000000000000,
	  0x8000000000000001, 0xFFFFFFFFFFFFFFFE, 0xFFFFFFFFFFFFFFFF
	]
  val sInputs : IntInf.int list = [
	  0x0, 0x1, 0x2, 0x3, 0xF, 0x11, 0xffff, 0x10000, 0x1ffff,
	  0x100000001, 0x800080008000, 0x1000100010001,
	  0x7FFF00000000FFFF, 0xFFFFFFFF0000, ~281470681808896,
	  0x7FFFFFFFFFFFFFFF, 0x7FFFFFFFFFFFFFFE, ~9223372036854775808,
	  ~9223372036854775807, ~2, ~1
	]

  fun maskLarge n = IntInf.andb(0xffffffffffffffff, n)

  fun chkBinOp (name, w64Op, infOp) (arg1, arg2) = let
	val expected = eval (Word64.fromLargeInt o infOp) (arg1, arg2)
	val chk = chkArith (fn () => w64Op(arg1, arg2), expected)
	val msg = concat[
		"  check (", IntInf.toString arg1, " ", name, " ",
		IntInf.toString arg2, ")"
	      ]
	in
	  (msg, chk)
	end

  fun chkCvt (name, fromToS, toToS, cvt) (from, to) = let
	val msg = concat["check (", name, " ", fromToS from, ")"]
	in
	  (msg, check (op =, toToS) (fn () => cvt from, to))
	end

  val chkToLargeInt = chkCvt (
	  "Word64.toLargeInt",
	  fn w => "0wx"^ Word64.toString w,
	  IntInf.toString,
	  Word64.toLargeInt)
  val chkToLargeIntX = chkCvt (
	  "Word64.toLargeIntX",
	  fn w => "0wx"^ Word64.toString w,
	  IntInf.toString,
	  Word64.toLargeIntX)
  val chkToId1 = chkCvt (
	  "(Word64.fromLargeInt o Word64.toLargeInt)",
	  fn w => "0wx"^ Word64.toString w,
	  fn w => "0wx"^ Word64.toString w,
	  Word64.fromLargeInt o Word64.toLargeInt)
in

val _ = runTests (ListPair.map chkToLargeInt (inputs, map VAL inputs'))
val _ = runTests (ListPair.map chkToLargeIntX (inputs, map VAL sInputs))
val _ = runTests (ListPair.map chkToId1 (inputs, map VAL inputs))

end;

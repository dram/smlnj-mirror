(* tests of the Real.{toLargeInt,fromLargeInt} functions, where the LargeInt
 * module is an alias for IntInf.
 *)

local
  structure R = Real
  fun check f = (if f () then "OK" else "WRONG") handle _ => "EXN";
  fun checkOverflow f = (f() ; "WRONG") handle Overflow => "OK" | _ => "EXN";
  fun checkDomain f = (f() ; "WRONG") handle Domain => "OK" | _ => "EXN";

  val modes = [
          IEEEReal.TO_NEAREST, IEEEReal.TO_NEGINF, IEEEReal.TO_POSINF, IEEEReal.TO_ZERO
        ]

  fun chkToInt (r, expected) =
        ListPair.map
          (fn (mode, n) => check (fn () => (R.toLargeInt mode r = n)))
            (modes, expected)
  (* 1/2^52 *)
  val two_to_the_minus_52 = Fn.repeat 52 (fn x => 0.5 * x) 1.0
in

val test01 = chkToInt (0.0, [0, 0, 0, 0])
val test02 = chkToInt (~0.0, [0, 0, 0, 0])
val test03 = chkToInt (~1.0, [~1, ~1, ~1, ~1]);
val test04 = chkToInt (~2.0, [~2, ~2, ~2, ~2]);
val test05 = chkToInt (~1.75, [~2, ~2, ~1, ~1]);
val test06 = chkToInt (~1.5, [~2, ~2, ~1, ~1]);
val test07 = chkToInt (~2.5, [~2, ~3, ~2, ~2]);
val test08 = chkToInt (~2.6, [~3, ~3, ~2, ~2]);
val test09 = chkToInt (~0.3, [0, ~1, 0, 0]);
val test10 = chkToInt (~0.5, [0, ~1, 0, 0]);
val test11 = chkToInt (~0.6, [~1, ~1, 0, 0]);
val test12 = chkToInt (1.0, [1, 1, 1, 1]);
val test13 = chkToInt (2.0, [2, 2, 2, 2]);
val test14 = chkToInt (1.75, [2, 1, 2, 1]);
val test15 = chkToInt (1.5, [2, 1, 2, 1]);
val test16 = chkToInt (2.5, [2, 2, 3, 2]);
val test17 = chkToInt (2.6, [3, 2, 3, 2]);
val test18 = chkToInt (0.3, [0, 0, 1, 0]);
val test19 = chkToInt (0.5, [0, 0, 1, 0]);
val test20 = chkToInt (0.6, [1, 0, 1, 0]);
val test21 = chkToInt (two_to_the_minus_52, [0, 0, 1, 0]);
val test22 = chkToInt (~two_to_the_minus_52, [0, ~1, 0, 0]);
(* ~2^53 = ~9007199254740992 is bottom of the exact-integer range *)
val test23 = chkToInt (~9007199254740992.0, [
        ~9007199254740992, ~9007199254740992, ~9007199254740992, ~9007199254740992
      ]);
(* 2^53 = 9007199254740992 is top of the exact-integer range *)
val test24 = chkToInt (9007199254740992.0, [
        9007199254740992, 9007199254740992, 9007199254740992, 9007199254740992
      ]);

end;

(* bug1387.1.sml *)

StringCvt.scanString (LargeInt.scan StringCvt.HEX) "0xffffffff"; (* SOME 4294967295 *)

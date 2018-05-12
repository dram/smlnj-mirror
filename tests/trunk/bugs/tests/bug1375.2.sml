(* bug1375.2.sml *)

Word31.fromString "100000000";	(* should raise Overflow on 32-bit machines *)

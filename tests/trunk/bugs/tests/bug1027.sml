(* bug1027.sml *)

Word32.fromString "0wx2";	(* should be 2 *)
Word.fromString "0w2";		(* should be 0 *)

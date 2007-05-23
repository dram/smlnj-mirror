(* bug627.1.sml *)
(* causes Bus error *)

val x = Unsafe.blastWrite "abc";
val y: string = Unsafe.blastRead x;

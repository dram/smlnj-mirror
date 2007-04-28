(* bug1469.1.sml *)

Control.CG.printit := true;
val x = ref 0;
fun f y = x := y;

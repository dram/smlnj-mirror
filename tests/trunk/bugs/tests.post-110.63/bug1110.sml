(* bug1110.sml *)

datatype lyst = NIL of string | CONS of (string * lyst) ref;
val a = CONS (ref ("foo", NIL "nil"));
Control.Print.printDepth := 100;
Control.Print.printLength := 100;
val a = CONS (ref ("foo", NIL "nil"));


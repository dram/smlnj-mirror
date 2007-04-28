(* bug1023.sml *)

(* [GK 4/18/07] This test case was updated to conform to the 
   current Basis specification, thus using Array.vector 
   instead of Array.extract 
 *)
fun a2v a = Array.vector a;
val a0 : int Array.array = Array.fromList [];
a2v a0;

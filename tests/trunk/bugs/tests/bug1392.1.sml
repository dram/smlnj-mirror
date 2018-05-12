(* bug1392.1.sml *)

Control.printWarnings := true; (* override testml.sh settings *)

structure S =
struct

  val x = 3

  val y = 1

  fun f nil = 0

  val z = 4

end;



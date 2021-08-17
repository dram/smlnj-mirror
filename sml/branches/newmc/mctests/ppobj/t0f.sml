(* mctests/ppobj/t0f.sml -- from ppobj.sml *)
 
datatype tk
  = DT of ({ds : string list, x: int} vector) * int

fun foo (DT (#[{ds, ... }], u)) = ds
  | foo _ = nil;


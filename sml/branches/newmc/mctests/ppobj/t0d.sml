(* mctests/ppobj/t0d.sml -- from ppobj.sml *)
 
datatype tk
  = DT of {ds : string list, x: int} vector

fun foo (DT (#[{ds, ... }])) = ds
  | foo _ = nil;


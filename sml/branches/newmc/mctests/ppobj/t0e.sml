(* mctests/ppobj/t0e.sml -- from ppobj.sml *)
 
datatype tk
  = DT of {family : {ds : string list, x: int} vector}

fun foo (DT { family = #[{ds, ... }]}) = ds
  | foo _ = nil;


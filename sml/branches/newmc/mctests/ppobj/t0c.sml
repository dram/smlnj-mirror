(* mctests/ppobj/t0c.sml -- from ppobj.sml *)
 
datatype tk
  = DT of {family : {ds : string list, x: int} vector,
	   z : unit}

fun foo (DT { family = #[{ds, ... }], ... }) = ds
  | foo _ = nil;


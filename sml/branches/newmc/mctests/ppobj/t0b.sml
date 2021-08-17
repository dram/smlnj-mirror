(* mctests/ppobj/t0b.sml -- from ppobj.sml *)
 
datatype tk
  = DT of {family : {dcons : string list, x: int} vector,
	   z : unit}

datatype tc
  = G of {kind : tk, y : bool}

fun foo (G{ kind = DT { family = #[{dcons, ... }], ... }, ...}) = dcons
  | foo _ = nil;


(* mctests/ppobj/t0a.sml -- from ppobj.sml *)
 
datatype tk
  = DT of {family : {members : {dcons : string list, x: int} vector},
	   z : unit}

datatype tc
  = G of {kind : tk, y : bool}

fun foo (G{ kind = DT { family =
				{ members = #[{dcons, ... }], ... },
			    ... },
		   ... }) = dcons
  | foo _ = nil;


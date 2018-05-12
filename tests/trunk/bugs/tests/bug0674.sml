(* bug674.sml *)

val e = Environment.filterEnv(#get EnvRef.pervasive (),
			      [Symbol.varSymbol "hd"]);

signature SIGMATCH =
   sig
     structure Modules : MODULES
     val match : {sign : Modules.Signature,
		  str : Modules.Structure,
		  spath : Modules.spath,
		  scope : Stamps.scope,
		  err : ErrorMsg.severity -> string -> unit,
		  printEnv : Modules.env,
		  abstract : bool,
		  self : bool} -> Modules.Structure * Modules.thinning
   end

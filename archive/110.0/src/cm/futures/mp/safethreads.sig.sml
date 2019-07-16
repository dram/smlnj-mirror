signature SAFETHREADS =
    sig
	structure Thread  : 
	    sig
		val fork   : (unit -> unit) -> unit
		val exit   : unit -> 'a
		val yield  : unit -> unit

		(* for debugging mp *)
		val safeIO : string -> string -> unit
	    end
    end	

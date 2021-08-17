(* tests/ppobj/t0.sml -- from ppobj.sml *)
 
fun dconsOf (Types.GENtyc
		 { kind = Types.DATATYPE
			      { family =
				{ members = #[{dcons, ... }], ... },
				... },
		   ... }) = dcons
  | dconsOf _ = nil;


(* varcon.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature VARCON = 
sig

  datatype var
    = VALvar of				(* ordinary variables *)
        {path : SymPath.path,
	 typ : Types.ty ref,
         access : Access.access,
         info   : II.ii}
    | OVLDvar of			(* overloaded identifier *)
	  {name : Symbol.symbol,
	   options: {indicator: Types.ty, variant: var} list ref,
	   scheme: Types.tyfun}
    | ERRORvar

  type datacon = Types.datacon

  datatype value
    = VAL of var
    | CON of datacon

  val mkVALvar : Symbol.symbol * Access.access ->  var

  val bogusCON : datacon
  val mkBogusEXN : Types.ty -> datacon

end

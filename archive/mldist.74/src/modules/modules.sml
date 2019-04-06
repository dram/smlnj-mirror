(* Copyright 1989, 1990, 1991 by AT&T Bell Laboratories *)

(* modules.sml *)

structure Modules : MODULES =
struct

  open Types Symbol Access

  type spath = Symbol.symbol list
  type strpos = int

  (* This is the definition of type "binding". *)

  datatype fixityVar
      = FIXvar of {name: Symbol.symbol,
		   binding: Fixity.fixity}

  datatype signatureVar
      = SIGvar of {name: Symbol.symbol,
		   binding: Signature}

  and structureVar
      = STRvar of {name: Symbol.symbol,
		   access: Access.access,	   
		   binding: Structure (* was strb *)}

  and functorVar
      = FCTvar of {name: Symbol.symbol,
		   access: Access.access,
		   binding: Functor}

  and binding
      = VARbind of Variables.var
      | CONbind of Types.datacon
      | TYCbind of Types.tycon
      | SIGbind of signatureVar
      | STRbind of structureVar
      | FCTbind of functorVar
      | FIXbind of fixityVar

 (* These are the types used for the implementation of modules. *)

  and Signature
      = SIG of {symbols : symbol list,
		env : env,
		stamp : Stamps.stamp, (* for fast equality test *)
		path : symbol option, (* name for abbrev. in printing *)
		kind : sigkind}
      | ERROR_SIG

  and sigkind = TOP of
                     {strcount : int,
		      typecount : int,
		      slotcount : int,
		      sConstraints : {internal: spath list,
			              external: Structure option} list,
		      tConstraints : {internal: spath list,
				      external: Types.tycon option} list}
		   | EMBEDDED

  and Structure
      = SIMPLE of {stamp: Stamps.stamp, env: env, path : spath}
      | INSTANCE of
	   {sign : Signature,
	    subStrs : Structure array,
	    types : Types.tycon array,
	    origin : Structure,
	    path : spath}
      | FORMAL of {pos : strpos, spec : Signature}
      | OPENFORMAL of {pos : int list,spec : Signature, name : spath}
      | SELF of Stamps.stamp   (* this only appears in the origin field of
				  "self-origin" INSTANCE structures.  These
				  arise from definitions like
				  "structure A: SIG = struct ... end" *)
      | ABSFB_STR of Types.absfbpos
      | ERROR_STR


  and Functor
      = FCT of {paramName : Symbol.symbol,
		argument : Signature,
		body : {tyseq: Types.tycon list, strseq : Structure list,
			str : Structure}}
      | ERROR_FCT

  withtype env = binding Env.env

  type binder = Symbol.symbol * binding

      (* thinning *)

  datatype trans 
       (* old position, val, exn, or unthinned str *)
     = VALtrans of Access.access  

       (* old str position, substr thinning *)
     | THINtrans of access * lvar * trans list
       (* constructor as value component *)

     | CONtrans of Types.datacon            

  type thinning = (lvar * trans list) option

end (* structure Modules *)

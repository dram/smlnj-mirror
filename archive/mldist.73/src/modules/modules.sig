(* Copyright 1989,1990,1991 by AT&T Bell Laboratories *)
(* modules.sig *)

signature MODULES =
sig

  (* definitions for environments:
       1. a top-level signature is a signature that is not nested within
          another signature.
     invariants for environments:
       1. FORMAL structures and types occur only  within the environment of
          signatures.
       2. INSTANCE structures occur only within the environment of top-level
          actual (non-formal) structures.
       3. ABSFB_STR structures occur only with the environment of INSTANCE
          structures being used to represent the body of the functor.
       3. Functor and signature bindings never occur within signature
          environments.
       4. Access paths:
             1. All access paths in structure environments are SLOTs
	     2. For values which have access paths, when these values
	        are returned by environment lookup functions these 
		access paths will be PATH's (i.e. they are absolute).
	     3. All variables in the top-level environment will have
	        an access of PATH.  For variables bound directly in the
		top level environment the list for PATH will be a singleton
	        list, whose single element is the lvar for the variable.
		For variables introduced by an open declaration the path
		will have length greater than one.
	     4. Any local binding occurrence will have an access of 
	        a singleton PATH, whose single element is the lvar for
		the variable.  The term binding occurrences refers to
		variables bound in core-language expressions (by a let
		binding or in pattern match).
	     5. All uses (applied occurrences) of variables will have an
	        access of PATH.
       5. Only top-level signatures will have sharing constraints.
          All sharing constraints defined in an embedded signature are placed
          in the top-level signature enclosing the embedded signature.  This
          requires some care when building signatures.
       6. The origin field of an INSTANCE structure can only be set
          to SELF or SIMPLE structures.
    *)
		
  type env  (* = binding Env.env *)
  type spath (* = Sumbol.symbol list *)
  type binder (* = Symbol.symbol * binding *)
  type strpos

  datatype signatureVar
    = SIGvar of
	{name: Symbol.symbol,
	 binding: Signature}

  and structureVar
    = STRvar of
	{name: Symbol.symbol,
	 access: Access.access,	   
	 binding: Structure}

  and functorVar
    = FCTvar of
	{name: Symbol.symbol,
	 access: Access.access,	   
	 binding: Functor}

  and fixityVar
    = FIXvar of
	{name: Symbol.symbol,
	 binding: Fixity.fixity}

  and binding 
    = VARbind of Variables.var
    | CONbind of Types.datacon
    | TYCbind of Types.tycon
    | SIGbind of signatureVar
    | STRbind of structureVar
    | FCTbind of functorVar
    | FIXbind of fixityVar

 (* These are the types used for the implementation of modules.*)

  (* the path field is the signature name (if any) to which this
     signature was bound in an environment after it was created *)

  and Signature
      = SIG of {symbols : Symbol.symbol list,
		env : env,
		kind : sigkind,
		path : Symbol.symbol option,
		stamp : Stamps.stamp}
      | ERROR_SIG

  (* There are two kinds of signatures.  TOP signatures are signatures
     which are not nested within other signatures.  EMBEDDED signatures
     are nested or "embedded" within other signatures.   EMBEDDED structures
     use the instantiation arrays of the TOP structure which contains them.*)

  and sigkind = TOP of
                     {strcount : int,
		      typecount : int,
		      slotcount : int, (* used when including sigs *)
		      sConstraints : {internal: spath list,
			              external: Structure option} list,
		      tConstraints : {internal: spath list,
				      external: Types.tycon option} list}
	       | EMBEDDED

  (* SIMPLE structures result from struct ... end expressions.

     INSTANCE structures are views of structures created by signature
     matching

     FORMAL structures are specifications of structures in signatures.

     SELF structures appear in the origin of INSTANCE structures

     ABSFB_STR structures represent structures in a functor body.

     Path fields, when present, are the full symbolic path name, relative
     to the top-level environment, to which a structure was initially bound.
     They are stored in reverse order (i.e. structure A = struct
     structure B = ...  is stored as B.A).  They are used for printing.*)

  and Structure
    = SIMPLE of {stamp: Stamps.stamp, env: env, path : spath}
    | INSTANCE of
	{sign : Signature,
	 subStrs : Structure array,
	 types : Types.tycon array,
	 origin : Structure,
	 path : spath}
    | FORMAL of {pos : strpos, spec : Signature}
    | SELF of Stamps.stamp   (* this only appears in the origin field of
				"self-origin" INSTANCE structures.  These
				arise from definitions like
				"structure A: SIG = struct ... end" *)
    | ABSFB_STR of Types.absfbpos
    | ERROR_STR

  (* invariants for the type sequence and the structure sequence representing
     the body of the functor:
            For the type sequence:
	        All DEFtycs follow the type constructors on which they
		depend.

                The kind field in a GENtyc which is a datatype may
		refer to any element of the type sequence.

	    For the structure sequence:

	        All structures follow their origin structure in the sequence,
		if the origin structure is also in the sequence.

		The "str" field contains the body of the functor.

		The substructure instantiation array of INSTANCE structures
		may refer to any element in the sequence, structures in
		the parameter or externally-defined structures.*)

  and Functor
      = FCT of {paramName : Symbol.symbol,
		argument : Signature,
		body : {tyseq: Types.tycon list, strseq : Structure list,
			str : Structure}}
      | ERROR_FCT

  (* trans: used to construct structure records:
       VALtrans: The access path is the dynamic path to a value which is to
                 be placed in a structure.
       THINtrans(a,x,s): Bind structure a to x and thin x by creating a
                 structure consisting of the values listed in s. All the
		 dynamic access paths for values listed in s end with the
		 lvar x.
       CONtrans: create a function for the constructor when it is a value
                 component of a structure.*)

  datatype trans
    = VALtrans of Access.access
    | THINtrans of Access.access * Access.lvar * trans list
    | CONtrans of Types.datacon

  type thinning (* = (lvar * trans list) option *)

end  (* signature MODULES *)

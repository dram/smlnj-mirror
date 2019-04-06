(* Copyright 1991 by AT&T Bell Laboratories *)
(* moduleutil.sig *)

signature MODULE_UTIL =
sig
    structure Modules : MODULES
    exception UnboundComponent of Modules.spath
    exception ErrorStructure

    val transType : Modules.Structure array * Types.tycon array
		    -> Types.ty -> Types.ty

   (*** lookup functions for structures:
     
     transPosTycon:
     transPosStr: lookup functions to find type constructors
                and structures in actual functor parameter structures

    These functions raise the exception ErrorStructure if you try to look
    up a binding in an error structure.  They raise UnboundComponent if a
    lookup fails because a component is missing.  The exception returns
    the portion of the path which could not be found (i.e. A.B.x returns
    B.x if A exists but A.B dooes not exist):

     lookBinding: look up a binding in an actual structure environment.
                  Takes a qualified identifier and the access path for
		  the structure as arguments and returns a binding where
		  a relativized types and structures have been interpreted
                  and all access paths adjusted to be absolute (i.e. PATH).

     lookbindingTYC: lookBinding specialized to find type constructors

     lookBindingSTR: lookBinding specialized to find structures.  Does
                       not adjust access paths.
    ***)

    val transPosTycon : Modules.Structure -> int list -> Types.tycon

    val transPosStr : Modules.Structure -> int list -> Modules.Structure

    val lookBinding : Modules.Structure * Modules.spath * int list ->
	                Modules.binding
  
    val lookBindingTYC: Modules.Structure * Modules.spath -> Types.tycon

    val lookBindingSTR: Modules.Structure * Modules.spath ->
	                        Modules.structureVar
    
    (*** lookup functions for environments:

       All these functions print an error message using the
       ErrorMsg.complainer argument and return an error value
       if an environment look-up fails.

       lookFCT, lookSIG, lookShortVARCON, lookFIX:
                look up the bindings of identifiers an environment.

       lookEXN: look up qualified exception identifers.  Only works for
                identifiers in actual structures.  

       lookTYC: find the binding of a qualified type identifier in an
                environment.  Works for qualified identifiers in all
		kinds of structures.   For INSTANCE structures, it interprets
		relativized type constructors.  For FORMAL structures it
		relativizes FORMtycs.
 
       lookArTYC:  like lookTYC, but also checks arity

       lookVARCON, lookSTR: find the binding of a qualified identifer
                in an environment.   For identifiers bound in INSTANCE
		structures, it interprets relativized type constructors
		in value bindings and relativized structures.  For
		FORMAL structures, it looks up the bindings but does not
		relativize them.
      ***)

    val lookTYC : Modules.env * Modules.spath * ErrorMsg.complainer
	             -> Types.tycon
     
    val lookArTYC : Modules.env * Modules.spath * int * ErrorMsg.complainer
		      -> Types.tycon

    val lookEXN : Modules.env * Modules.spath * ErrorMsg.complainer
	              -> Types.datacon

    val lookShortVARCON : Modules.env * Symbol.symbol * ErrorMsg.complainer 
	              -> Modules.binding
	              
    val lookVARCON : Modules.env * Modules.spath * ErrorMsg.complainer 
		      -> Modules.binding

    val lookSTR : Modules.env * Modules.spath * ErrorMsg.complainer
		      -> Modules.structureVar

    val lookFCT : Modules.env * Symbol.symbol * ErrorMsg.complainer
	              -> Modules.functorVar

    val lookSIG : Modules.env * Symbol.symbol * ErrorMsg.complainer
	              -> Modules.Signature

    val lookFIX : Modules.env * Symbol.symbol -> Fixity.fixity

   (*** Testing structures for equality:
       getStrStamp: returns the stamp for a structure
       getOrigin: returns the origin structure for a structure.
       eqOrigin: true if two structure are views of the same structure.
    ***)

    val getStrStamp : Modules.Structure -> Stamps.stamp
    val getOrigin : Modules.Structure -> Modules.Structure
    val eqOrigin : Modules.Structure * Modules.Structure -> bool

    (*** Testing signatures for equality:

        getSignStamp: returns the stamp for a signature
        eqSign: true if two signatues have the same stamp
     ***)

    val getSignStamp : Modules.Signature -> Stamps.stamp

    val eqSign : Modules.Signature * Modules.Signature -> bool


    (*** Creating structures:

       mkStructure: construct a simple structure given an environment
       newStr: create a simple structure.  Returns the trans list which
               will build the dynamic value for the structure.
       compose: given a thinning of a structure S and the trans list which
                would build S, create a trans list which directly builds 
		the thinned structure.
     ***)

    val mkStructure : Modules.env * Modules.spath -> Modules.Structure
  
    val newStr : Stamps.stamp * Modules.spath * Modules.env ->
	            Modules.Structure * Modules.trans list

    val compose : Modules.thinning * Modules.trans list -> Modules.trans list

    (*** misc. utilities:

       openStructureVar: opens a structure variable in an environment

       openSigStructure: look up a structure in the first environment, and
                          open it in the second environment, given the
			  qualified identifier for the structure. Return the
			  new environment.
 
       staleLvars(delta,base): return the lvars that will be made stale
                          (unreachable) when the delta-environment is added
			  to the base environment.

       makeEnv: convert a structure to an environment where all types
                 and structures have been interpreted and access information
		 is absolute.  The int list is the access path of the
                 structure  argument.

       findPath:  convert symbolic path names to a printable string in the
	          context of an environment.  Its arguments are the reversed
		  path name, a static semantic value, an equality function on
		  those values, and a lookup function mapping paths to their
		  values (if any) in an environment.  The second argument of
		  the lookup function should called if no binding is found
		  for a name in the environment.

       sortEnvBindings: sort the bindings in an environment for printing
                        purposes.  It is only correct to sort environments
                        with no duplicate bindings.
     ***)

    val getSubStrs : Modules.Structure ->
	                  (Symbol.symbol * Modules.Structure) list

    val openStructureVar : Modules.env * Modules.structureVar -> Modules.env

    val openSigStructure : Modules.env * Modules.spath * Modules.env 
	                                   * ErrorMsg.complainer
	                          -> Modules.env

    val staleLvars : Modules.env * Modules.env -> int list

    val makeEnv : Modules.Structure * int list -> Modules.env

    val findPath : Modules.spath * 'a * ('a * 'a -> bool) * 
	            (Modules.spath * ('b -> 'c) -> 'a)  -> string

    val sortEnvBindings: Modules.env -> (Symbol.symbol * Modules.binding) list

    val getStrPath : Modules.Structure -> Modules.spath

end (* signature MODULES_UTIL *)

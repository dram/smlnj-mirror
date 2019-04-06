(* Copyright 1989 by AT&T Bell Laboratories *)
(* types.sml *)

structure Types : TYPES =
struct

  (* tycpos: type constructor or structure position in an INSTANCE structure.
     The integer list gives a path through the structure instantiation
     arrays and the offset gives the position in the type array of the
     last structure *)
     
  type tycpos = int list * int

  type polysign = {weakness: int, eq: bool} list
  type label = Symbol.symbol

  datatype absfbpos = PARAM of int list (* position within parameter *)
                    | SEQ of int        (* position in sequence *)

  datatype eqprop = YES | NO | IND | OBJ | DATA | UNDEF

  val infinity = 10000000

  datatype tvkind
    = IBOUND of int	 (* inferred bound type variables -- indexed *)
    | META of	         (* metavariables -- depth = infinity for metaargs *)
	{depth: int,
	 weakness: int,
	 eq: bool}
    | INSTANTIATED of ty
    | UBOUND of  (* user bound type variables -- user name*)
	{name: Symbol.symbol,
	 depth : int,
	 weakness: int,
	 eq: bool}

  and datacon  (* exceptions are a special case with rep=VARIABLE() *)
    = DATACON of
	{name  : Symbol.symbol,
	 const : bool,
	 typ   : ty,
	 rep   : Access.conrep,
	 sign  : Access.conrep list}

  and tyckind
    = PRIMtyc  	        (* primitive type constructors like int *)
    | ABStyc of tycon   (* abstract type constructors formed by abstype *)
    | DATAtyc of datacon list (* datatype constructors *)
    | FORMtyck		(* dummy type constructors used in functor body *)

  and tycon
    = GENtyc of {stamp : Stamps.stamp, 
		 arity : int, 
		 eq    : eqprop ref,
		 path  : Symbol.symbol list,
		 kind  : tyckind ref}
    | DEFtyc of {path  : Symbol.symbol list,
		 strict: bool,
		 tyfun : tyfun}
    | RECORDtyc of label list
    | FORMtyc of {pos : int, spec : tycon, name: Symbol.symbol}
    | OPENFORMtyc of {pos: tycpos, spec : tycon, name : Symbol.symbol list}
    | RELtyc of  {pos : tycpos, name : Symbol.symbol list}
    | ABSFBtyc of absfbpos
    | ERRORtyc

  and ty 
    = VARty of tyvar
    | CONty of tycon * ty list
    | FLEXRECORDty of rowty ref
    | POLYty of {sign: {weakness:int, eq:bool} list, tyfun: tyfun, abs: int}
    | UNDEFty
    | ERRORty

  and rowty
    = OPEN of (label * ty) list * int (* weakness *)
    | CLOSED of ty

  and tyfun
    = TYFUN of
	{arity : int,
	 body : ty}

  withtype tyvar = tvkind ref

  fun mkTyvar(kind: tvkind) : tyvar = ref kind

  val bogusCON = DATACON{name=Symbol.varSymbol "bogus",
			 const=true,typ=ERRORty,
			 rep=Access.UNDECIDED,sign=[]}

  val bogusEXN = DATACON{name=Symbol.varSymbol "bogus",
			 const=true,typ=ERRORty,
			 rep=Access.CONSTANT 0,sign=[]}

end (* structure Types *)

(* Copyright 1989 by AT&T Bell Laboratories *)
(* basics.sml *)

(* basic datatypes *)

structure Basics : BASICS = struct

  open Symbol Access PrintUtil

    type spath = symbol list
    type label = symbol
    type stamp = int
    type polysign = {weakness: int, eq: bool} list
    type sharespec = {s: (spath*spath) list,
		      t: (spath*spath) list}

    datatype eqprop = YES | NO | IND | OBJ | DATA | UNDEF

  (* fixity attributes *)

    datatype fixity = NONfix | INfix of (int*int)

    val infinity = 10000000

    datatype tvkind
      = IBOUND of int	 (* inferred bound type variables -- indexed *)
      | META of	         (* metavariables -- depth = infinity for metaargs *)
	  {depth: int,
	   weakness: int,
	   eq: bool}
      | INSTANTIATED of ty
      | UBOUND of  (* user bound type variables -- user name*)
	  {name: symbol,
	   depth: int,
	   weakness: int,
	   eq: bool}

    and datacon  (* exceptions are a special case with rep=VARIABLE() *)
      = DATACON of
	  {name  : symbol,
	   const : bool,
	   typ   : ty,
	   rep   : conrep,
	   sign  : conrep list}

    and tyckind
      = PRIMtyc  		(* primitive type constructors like int *)
      | FORMtyc			(* formal type constructors in signatures *)
      | ABStyc of tycon		(* abstract type constructors formed by abstype *)
      | DATAtyc of datacon list (* datatype constructors *)

    and tycon
      = GENtyc of {stamp : stamp, 
		   arity : int, 
		   eq    : eqprop ref,
		   path  : Symbol.symbol list,
		   kind	 : tyckind ref}
      | DEFtyc of {path : Symbol.symbol list,
		   tyfun : tyfun}
      | RECORDtyc of label list
      | INDtyc of int   (* indirect tycon -- only in bindings (in sigs) *)
      | SHRtyc of int list   (* sharing indirection *)
      | RELtyc of int list   (* relative tycon -- only in type expressions *)
      | NULLtyc

    and ty 
      = VARty of tyvar
      | CONty of tycon * ty list
      | FLEXRECORDty of rowty ref
      | POLYty of {sign: {weakness:int, eq:bool} list, tyfun: tyfun, abs: int}
      | UNDEFty
      | ERRORty

    and rowty
      = OPEN of (label * ty) list
      | CLOSED of ty

    and tyfun
      = TYFUN of
	  {arity : int,
	   body : ty}


  (* variables *)

    and var
      = VALvar of 		      (* ordinary variables *)
	  {access: access,
	   name : symbol list,
	   typ : ty ref}
      | OVLDvar of       	      (* overloaded identifier *)
	  {name : symbol,
	   options: {indicator: ty, variant: var} list ref,
	   scheme: tyfun}

  (* structures and signatures *)

    and strenv 
      =	DIR
      | REL of {s: Structure array, t: tycon array}

    and strkind
      = STRkind of
          {path : symbol list}
      | SIGkind of
	  {share: sharespec,
	   bindings : binding list,
	   stamps : Stampset.stampsets}

    and Structure
      = STRstr of
	  {stamp : stamp,
	   sign  : stamp,
	   table : env,
	   env   : strenv,
	   kind  : strkind}
      | INDstr of int        (* indirect substructure binding *)
      | SHRstr of int list   (* sharing indirection *)
      | NULLstr

    and Functor
      = FUNCTOR of
	  {paramName: symbol,
	   param: Structure,
	   body: Structure,
	   paramVis: bool,
	   stamps: Stampset.stampsets}

    and signatureVar
      = SIGvar of
	  {name: symbol,
	   binding: Structure}

    and structureVar
      = STRvar of
	  {name: symbol list,
	   access: access,	   
	   binding: Structure}

    and functorVar
      = FCTvar of
	  {name: symbol,
	   access: access,	   
	   binding: Functor}

    and fixityVar
      = FIXvar of
	  {name: symbol,
	   binding: fixity}

    and binding 
      = VARbind of var  
      | CONbind of datacon
      | TYCbind of tycon
      | SIGbind of signatureVar
      | STRbind of structureVar
      | FCTbind of functorVar
      | FIXbind of fixityVar

    (* Note: exceptions are identified with data constructors; they
       no longer have a separate name space, hence no EXNbind constructor.
       On the other hand, structures and functors have separate name spaces,
       which may not be correct. *)

    withtype tyvar = tvkind ref
    and binder = symbol * binding
    and env = binding Env.env

    datatype trans
      = VALtrans of access	(* old position, val, exn, or unthinned str *)
      | THINtrans of access * lvar * trans list
	    (* old str position, substr thinning *)
      | CONtrans of datacon		(* constructor as value component *)

    type thinning = (lvar * trans list) option

  (* building fixities *)

    fun infixleft n = INfix (n+n, n+n+1)
    fun infixright n = INfix (n+n+1, n+n)

  (* building variables *)

    fun mkVALvar id =
	VALvar{access = PATH[namedLvar(id)], name = [id], 
	       typ = ref UNDEFty}

  (* building tycons, signatures, structures, and functors *)

    fun mkTyvar(kind: tvkind) : tyvar = ref kind
    
    fun mkSTR(path, table, env, {strStamps,...}: Stampset.stampsets) = 
        STRstr{stamp = Stampset.newStamp(strStamps),
               sign = 0,  (* guaranteed not to agree with any valid sig stamp *)
	       table = table,
	       env = env,
	       kind = STRkind{path=path}}

end (* structure Basics *)

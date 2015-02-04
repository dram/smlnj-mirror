(* types.sig *)

(* basic type datatypes *)

signature BASICS = sig

    structure Symbol : SYMBOL
          and Access : ACCESS
	  and Table : TABLE

    datatype 'a Option = NONE | SOME of 'a

    type label and spath

    datatype fixity = NONfix | INfix of (int*int)

    datatype conrep
      = UNDECIDED 
      | TAGGED of int 
      | CONSTANT of int 
      | TRANSPARENT 
      | TRANSU
      | TRANSB
      | REF
      | VARIABLE of Access.access  (* exception constructor *)

    datatype tvstatus
      = BOUND 		(* universally bound type variables (generics) *)
      | METAARG		(* instantiation metavariables *)
      | METALAM of int  (* lambda depth *)
      | INSTANTIATED of ty
      | FIXED		(* explicitly occuring type variables - become bound *)

    and tyvar
      = TYVAR of 
	  {stamp : int,
	   name : Symbol.symbol,
	   status: tvstatus ref}

    and datacon
      = DATACON of
	  {name  : Symbol.symbol,
	   const : bool,
	   vtype : ty,
	   rep   : conrep ref,
	   tycon : tycon}

    and tycon
      = ATOMtyc of	 (* built in primitive types like int, list *)
	  {stamp : int,
	   name  : Symbol.symbol,
	   arity : int}
      | SYMtyc of spath * Symbol.symbol  (* symbolic tycons in sig type specs *)
      | VARtyc of	 (* simple type components of signatures, functor params *)
	  {stamp  : int,
	   context: context,  (* SIGctx or PARctx only *)
	   name   : Symbol.symbol,
	   arity  : int}
      | TYPEtyc of  	 (* defined type constructor *)
	  {stamp  : int,
	   context: context,
	   name   : Symbol.symbol,
	   params : tyvar list,
	   def    : ty}
      | DATAtyc of
	  {stamp  : int,
	   context: context,
	   name   : Symbol.symbol,
	   params : tyvar list,
	   dcons  : datacon list ref}
      | RECORDtyc of
	  {stamp  : int,
	   labels : label list}
      | UNKNOWNtyc of Symbol.symbol

    and ty 
      = VARty of tyvar
      | CONty of tycon ref * ty list
      | FLEXRECORDty of
	  {fields : (label * ty) list,
	   completion : ty ref}
      | UNKNOWNty

    and var
    = VALvar of 		      (* ordinary variables *)
        {access : Access.access,
         name : Symbol.symbol,
         vtype : ty ref}
    | OVLDvar of       	      	      (* overloaded identifier *)
        {name : Symbol.symbol,
         options: {indicators: ty list, variant: var} list ref,
         scheme: ty}
    | UNKNOWNvar of Symbol.symbol        (* place holder for backpatching *)

    and Signature
      = SIG of {stamp: int,
	        elements: binding list,
		env: binding Table.table}

    and pnode
      = STRpnode of
	  {stamp: int,
	   sign: Signature,  (* necessary? *)
	   sons: (Symbol.symbol * (int * pnode)) list ref}
      | TYCpnode of
	  {stamp: int}

    and context  (* used to identify structures and types *)
      = TOPctx 		(* absolute *)
      | FCTctx		(* in functor defn *)
      | RELctx of Structure * context  (* relative to functor application(s) *)
      | PARctx of Structure (* relative to functor parameter *)
      | SHRctx of Structure (* shared tycon relative to functor parameter *)
      | SIGctx

    and Structure
      = DIRECT of  (* struct ... end *)
	  {stamp: int,
	   context: context,
	   table: binding Table.table}
      | FCTAPP of  (* functor applications *)
	  {stamp: int,
	   context: context,
	   body: Structure,
	   env: (pnode*Structure) list}
      | PARAM of  (* functor formals *)
	  {pnode: pnode,
	   spath: Symbol.symbol list,
	   sign: Signature}
      | SPEC of Signature  (* structure components in signatures *)

    and Functor
      = FUNCTOR of
	  {params: structureVar list,
	   body: Structure,
	   sign: Signature Option}

    and signatureVar
      = SIGvar of
	  {name: Symbol.symbol,
	   binding: Signature}

    and structureVar
      = STRvar of
	  {name: Symbol.symbol,
	   access: Access.access,	   
	   binding: Structure,
	   sign: Signature Option}

    and functorVar   (* tentative *)
      = FCTvar of
	  {name: Symbol.symbol,
	   access: Access.access,	   
	   binding: Functor}

    and binding 
      = VARbind of var  
      | CONbind of datacon
      | EXNbind of datacon  
      | TYCbind of tycon ref (* patchable *)
      | TYVbind of tyvar
      | SIGbind of signatureVar
      | STRbind of structureVar
      | FCTbind of functorVar
      | FIXbind of fixity

    (* whew! *)

    type binder

    (* type equality functions *)
    
    val eqTyvar : tyvar * tyvar -> bool

    val eqSymbolList : Symbol.symbol list * Symbol.symbol list -> bool

    val eqContext : context * context -> bool

    val eqPnode : pnode * pnode -> bool
    
    val eqTycon : tycon * tycon -> bool
    
    val prune : ty -> ty
    
    val eqTy : ty * ty -> bool

    val mkVALvar : Symbol.symbol * ty ref ->  var

    val tyconArity : tycon -> int

    val eqSignature : Signature * Signature -> bool

    val mkSTRpnode : Signature -> pnode
    val mkSIG : binding list * binding Table.table -> Signature
    val mkDIRECT : context * binding Table.table -> Structure
    val mkFCTAPP : context * Structure * (pnode * Structure) list -> Structure

    val infixleft : int -> fixity
    val infixright : int -> fixity

    val internals : bool ref

end (* signature BASICS *)

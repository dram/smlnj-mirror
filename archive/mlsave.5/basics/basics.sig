(* basics.sig *)

(* basic datatypes *)

signature BASICS = sig

    structure Symbol : SYMBOL
          and Access : ACCESS
	  and Table : TABLE

    datatype 'a Option = NONE | SOME of 'a

    type label and spath and stamp

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
	   dcons : datacon list ref}

    and tycon
      = ATOMtyc of	 (* built in primitive types like int, list *)
	  {stamp : stamp,
	   name  : Symbol.symbol,
	   arity : int}
      | TYPEtyc of  	 (* defined type constructor *)
	  {stamp  : stamp,
	   name   : Symbol.symbol,
	   params : tyvar list,
	   def    : ty}
      | DATAtyc of
	  {stamp  : stamp,
	   name   : Symbol.symbol,
	   params : tyvar list,
	   dcons  : datacon list ref}
      | RECORDtyc of
	  {stamp  : stamp,
	   labels : label list}
      | INDtyc of int list    (* indexed type component *)
      | VARtyc of	 (* dummy type components in sig structures *)
	  {stamp  : stamp,
	   name	  : Symbol.symbol,
	   arity  : int}
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

    and strkind
      = STRkind
      | SIGkind of
	  {shpaths: int list list list,
	   bindings : binding list,
	   stampcounts : {s:int,t:int}}

    and Structure
      = STRstr of
	  {stamp : stamp,   (* to update for sharing *)
	   sign  : stamp Option,
	   table : binding Table.table,  (* symtable *)
	   env   : {t:tycon array, s: Structure array},
	   kind  : strkind}
      | INDstr of int

    and Functor
      = FUNCTOR of
	  {param: Structure,
	   body: Structure,
	   tycCount: int}

    and signatureVar
      = SIGvar of
	  {name: Symbol.symbol,
	   binding: Structure}

    and structureVar
      = STRvar of
	  {name: Symbol.symbol,
	   access: Access.access,	   
	   binding: Structure}

    and functorVar   (* tentative *)
      = FCTvar of
	  {name: Symbol.symbol,
	   access: Access.access,	   
	   binding: Functor}

    and binding 
      = VARbind of var  
      | CONbind of datacon
(*    | EXNbind of datacon  *)
      | TYCbind of tycon ref (* patchable *)
      | TYVbind of tyvar
      | SIGbind of signatureVar
      | STRbind of structureVar
      | FCTbind of functorVar
      | FIXbind of fixity

    datatype trans
      = VALtrans of Access.access	(* old position, val, exn, or unthinned str *)
      | THINtrans of Access.access * Access.lvar * trans list
            (* old str position, substr thinning *)
      | CONtrans of datacon		(* constructor as value component *)
    
    type thinning

    (* whew! *)

    type binder
    type strenv
    val emptyStrenv : strenv

    val tycStamp : tycon -> int
    val tycName : tycon -> Symbol.symbol
    val tyconInContext : tycon * strenv -> tycon
    val typeInContext : ty * strenv -> ty
    val eqTyconRel : tycon * strenv * tycon * strenv -> bool
    val eqTycon : tycon * tycon -> bool

    (* type equality functions *)
    
    val eqTyvar : tyvar * tyvar -> bool

    val prune : ty -> ty
    
    val eqTy : ty * ty -> bool

    val mkVALvar : Symbol.symbol * ty ref ->  var

    val tyconArity : tycon -> int

    val stampBase : int
    val genTycStamp : (unit -> int) ref
    val genStrStamp : (unit -> int) ref

    val mkTyvar   : Symbol.symbol * tvstatus -> tyvar
    val newTyvar  : tvstatus -> tyvar
    val mkATOMtyc : Symbol.symbol * int -> tycon
    val mkVARtyc  : Symbol.symbol * int -> tycon
    val mkTYPEtyc : Symbol.symbol * tyvar list * ty
		    -> tycon
    val mkDATAtyc : Symbol.symbol * tyvar list * datacon list ref
		    -> tycon
    val mkSIG : (binding list * binding Table.table) * strenv
	        * {s:int,t:int} -> Structure
    val mkSTR : stamp Option * binding Table.table * strenv -> Structure

    val infixleft : int -> fixity
    val infixright : int -> fixity

    val internals : bool ref

end (* signature BASICS *)

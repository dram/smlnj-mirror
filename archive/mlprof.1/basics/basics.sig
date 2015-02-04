(* basics.sig *)

(* basic datatypes *)

signature BASICS = sig

    structure Symbol : SYMBOL
          and Access : ACCESS
	  and Table : TABLE

    type label and spath and stamp and sharespec

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

    type tyvar  (* == tvkind ref *)

    datatype tvkind  (* type variable kinds *)
      = IBOUND of int	 (* inferred bound type variables -- indexed *)
      | METAARG		 (* argument metavariables -- instantiating polytypes *)
      | METALAM of int   (* lambda metavariables -- depth *)
      | INSTANTIATED of ty
      | UBOUND of Symbol.symbol (* user bound type variables -- user name*)

    and datacon  (* exceptions are a special case with rep=VARIABLE() *)
      = DATACON of
	  {name  : Symbol.symbol,
	   const : bool,
	   typ   : ty,
	   rep   : conrep,
	   sign  : conrep list}

    and tyckind
      = ABStyc  (* atomic and abstract types *)
      | DEFtyc of tyfun
      | DATAtyc of datacon list ref
      | RECORDtyc of label list

    and tycon
      = TYCON of
	  {stamp : stamp,
	   arity : int,
	   name  : Symbol.symbol,
	   home  : Structure option,
	   kind  : tyckind}
      | INDtyc of int list    (* indexed type component *)
      | UNDEFtyc of Symbol.symbol

    and ty 
      = VARty of tyvar
      | CONty of tycon ref * ty list
      | FLEXRECORDty of rowty ref
      | POLYty of tyfun
      | UNDEFty
      | ERRORty

    and rowty
      = OPEN of (label * ty) list
      | CLOSED of ty

    and tyfun
      = TYFUN of
          {arity : int,
	   body : ty}

    and var
      = VALvar of 		      (* ordinary variables *)
	  {access : Access.access,
	   name : Symbol.symbol,
	   vtype : ty ref}
      | OVLDvar of       	      	      (* overloaded identifier *)
	  {name : Symbol.symbol,
	   options: {indicator: ty, variant: var} list ref,
	   scheme: tyfun}
      | UNKNOWNvar of Symbol.symbol        (* place holder for backpatching *)

    and strkind
      = STRkind of
          {name : Symbol.symbol,
	   home : Structure option}
      | SIGkind of
	  {share: sharespec,
	   bindings : binding list,
	   stampcounts : {s:int,t:int}}

    and Structure
      = STRstr of
	  {stamp : stamp,
	   sign  : stamp option,
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
      | TYCbind of tycon ref (* patchable *)
      | TYVbind of tyvar
      | SIGbind of signatureVar
      | STRbind of structureVar
      | FCTbind of functorVar
      | FIXbind of fixity

    type binder
    type symtable
    type strenv

    val emptyStrenv : strenv
    val isEmptyStrenv : strenv -> bool

    datatype trans
      = VALtrans of Access.access
	   (* old position, val, exn, or unthinned str *)
      | THINtrans of Access.access * Access.lvar * trans list
           (* old str position, substr thinning *)
      | CONtrans of datacon		(* constructor as value component *)
    type thinning

    (* whew! *)

    val infixleft : int -> fixity
    val infixright : int -> fixity

    val mkVALvar : Symbol.symbol * ty ref ->  var

    val stampBase : int
    val fixedStamp : stamp -> bool
    val tycStampCount : stamp ref
    val strStampCount : stamp ref
    val genSigStamp : unit -> int
    val genTycStamp : unit -> int
    val genStrStamp : unit -> int

    val mkTyvar   : tvkind -> tyvar
    val mkABStyc : Symbol.symbol * int * Structure option -> tycon
    val mkDEFtyc : Symbol.symbol * Structure option * tyfun -> tycon
    val mkDATAtyc : Symbol.symbol * int * Structure option * datacon list ref
		    -> tycon
    val mkSTR : Symbol.symbol * Structure option * stamp option * 
	        symtable * strenv -> Structure

end (* signature BASICS *)

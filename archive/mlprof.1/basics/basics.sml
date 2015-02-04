(* basics.sml *)

(* basic datatypes *)

structure Basics : BASICS = struct

  structure Symbol = Symbol
  structure Access = Access
  structure Table = Table

  open Symbol Access PrintUtil

    type spath = symbol list
    type label = symbol
    type stamp = int
    type sharespec = {s:(spath*spath) list,
		      t:(spath*spath) list}

  (* fixity attributes *)

    datatype fixity = NONfix | INfix of (int*int)

    datatype conrep
      = UNDECIDED 
      | TAGGED of int 
      | CONSTANT of int 
      | TRANSPARENT 
      | TRANSU
      | TRANSB
      | REF
      | VARIABLE of access (* exception constructor *)


    datatype tvkind
      = IBOUND of int	 (* inferred bound type variables -- indexed *)
      | METAARG		 (* argument metavariables -- instantiating polytypes *)
      | METALAM of int   (* lambda metavariables -- depth *)
      | INSTANTIATED of ty
      | UBOUND of symbol (* user bound type variables -- user name*)

    and datacon  (* exceptions are a special case with rep=VARIABLE() *)
      = DATACON of
	  {name  : symbol,
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
	   name  : symbol,
	   home  : Structure option,
	   kind  : tyckind}
      | INDtyc of int list    (* indexed type component *)
      | UNDEFtyc of symbol

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


  (* variables *)

    and var
      = VALvar of 		      (* ordinary variables *)
	  {access: access,
	   name : symbol,
	   vtype : ty ref}
      | OVLDvar of       	      (* overloaded identifier *)
	  {name : symbol,
	   options: {indicator: ty, variant: var} list ref,
	   scheme: tyfun}
      | UNKNOWNvar of symbol      (* place holder for backpatching *)


  (* structures and signatures *)

    and strkind
      = STRkind of
          {name : symbol,
	   home : Structure option}
      | SIGkind of
	  {share: sharespec,
	   bindings : binding list,
	   stampcounts : {s:int,t:int}}

    and Structure
      = STRstr of
	  {stamp : stamp,
	   sign  : stamp option,
	   table : symtable,
	   env   : strenv,
	   kind  : strkind}
      | INDstr of int

    and Functor
      = FUNCTOR of
	  {param: Structure,
	   body: Structure,
	   tycCount: int}

    and signatureVar
      = SIGvar of
	  {name: symbol,
	   binding: Structure}

    and structureVar
      = STRvar of
	  {name: symbol,
	   access: access,	   
	   binding: Structure}

    and functorVar
      = FCTvar of
	  {name: symbol,
	   access: access,	   
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

    (* Note: exceptions are identified with data constructors; they
       no longer have a separate name space, hence no EXNbind constructor.
       On the other hand, structures and functors have separate name spaces,
       which may not be correct. *)

    withtype tyvar = tvkind ref
    and binder = symbol * binding
    and symtable = binding Table.table
    and strenv = {s: Structure array, t: tycon array}

    val emptyStrenv = {s=arrayoflist [], t=arrayoflist []} : strenv
    fun isEmptyStrenv({s,t}:strenv) = 
	length t = 0 andalso length s = 0

    datatype trans
      = VALtrans of access	(* old position, val, exn, or unthinned str *)
      | THINtrans of access * lvar * trans list
	    (* old str position, substr thinning *)
      | CONtrans of datacon		(* constructor as value component *)

    type thinning = (lvar * trans list) option


  (* building fixities *)

    fun infixleft n = INfix (n+n, n+n)
    fun infixright n = INfix (n+n+1, n+n)


  (* building variables *)

    fun mkVALvar(id: symbol, refty: ty ref) : var =
	VALvar{access = LVAR(namedLvar(id)), name = id, vtype = refty}


  (* building tycons, signatures, structures, and functors *)

    val stampBase = 1000000

    fun fixedStamp s = s >= stampBase

    val sigStampCount = ref(~1)
    val tycStampCount = ref(2*stampBase)
    val strStampCount = ref(2*stampBase)

    fun genSigStamp () = (!sigStampCount before inc sigStampCount)
    fun genTycStamp () = (!tycStampCount before inc tycStampCount)
    fun genStrStamp () = (!strStampCount before inc strStampCount)

    fun mkTyvar(kind: tvkind) : tyvar = ref kind
    
    fun mkABStyc(name: symbol, arity: int, home: Structure option) : tycon =
	TYCON{stamp = genTycStamp(), name = name, arity = arity, home = home,
	      kind = ABStyc}
    
    fun mkDEFtyc(name: symbol, home: Structure option,
		 def as TYFUN{arity,...}: tyfun) : tycon =
	TYCON{stamp = genTycStamp(), name = name, arity = arity,
	      home = home, kind = DEFtyc def}
    
    fun mkDATAtyc(name: symbol, arity: int, home: Structure option, 
		  dcons: datacon list ref) : tycon =
	TYCON{stamp = genTycStamp(), name = name, arity = arity,
	      home = home, kind = DATAtyc dcons}
    
    fun mkSTR(name, home, sign, table, env) = 
        STRstr{stamp= genStrStamp(),
               sign=sign,
	       table=table,
	       env=env,
	       kind=STRkind{name=name,home=home}}

end (* structure Basics *)

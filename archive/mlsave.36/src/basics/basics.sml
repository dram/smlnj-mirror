(* basics.sml *)

(* basic datatypes *)

structure Basics : BASICS = struct

  structure Symbol = Symbol
  structure Access = Access
  structure IntStrMap = IntStrMap

  open Symbol Access PrintUtil

    type spath = symbol list
    type label = symbol
    type stamp = int
    type polysign = {weakness: int, eq: bool} list
    type sharespec = {s: (spath*spath) list,
		      t: (spath*spath) list}

    datatype bool3 = YES | NO | MAYBE

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
      = ABStyc (* atomic and abstract types *)
      | DEFtyc of tyfun
      | DATAtyc of datacon list
      | RECORDtyc of label list
      | UNDEFtyc of symbol list option

    and tycon
      = TYCON of
	  {stamp : stamp,
	   arity : int,
	   eq    : bool3 ref,
	   path  : symbol list,
	   kind  : tyckind}
      | INDtyc of int list    (* indirect type constructor *)

    and ty 
      = VARty of tyvar
      | CONty of tycon ref * ty list
      | FLEXRECORDty of rowty ref
      | POLYty of {sign: {weakness:int, eq:bool} list, tyfun: tyfun}
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
          {path : symbol list}
      | SIGkind of
	  {share: sharespec,
	   bindings : binding list,
	   stampcounts : {s:int,t:int}}

    and Structure
      = STRstr of
	  {stamp : stamp,
	   sign  : stamp,
	   table : symtable,
	   env   : strenv,
	   kind  : strkind}
      | INDstr of int

    and Functor
      = FUNCTOR of
	  {paramName: symbol,
	   param: Structure,
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

    and fixityVar
      = FIXvar of
	  {name: symbol,
	   binding: fixity}

    and binding 
      = VARbind of var  
      | CONbind of datacon
      | TYCbind of tycon ref (* patchable *)
      | TYVbind of tyvar
      | SIGbind of signatureVar
      | STRbind of structureVar
      | FCTbind of functorVar
      | FIXbind of fixityVar

    (* Note: exceptions are identified with data constructors; they
       no longer have a separate name space, hence no EXNbind constructor.
       On the other hand, structures and functors have separate name spaces,
       which may not be correct. *)

    withtype tyvar = tvkind ref
    and binder = int * string * binding
    and symtable = binding IntStrMap.intstrmap
    and strenv = {s: Structure array, t: tycon array}

    val emptyStrenv = {s=arrayoflist [], t=arrayoflist []} : strenv
    fun isEmptyStrenv({s,t}:strenv) = 
	Array.length t = 0 andalso Array.length s = 0

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

    val sigStampCount = ref(1)
    val tycStampCount = ref(2*stampBase)
    val strStampCount = ref(2*stampBase)

    fun genSigStamp () = (!sigStampCount before inc sigStampCount)
    fun genTycStamp () = (!tycStampCount before inc tycStampCount)

    fun genStrStamp () = (!strStampCount before inc strStampCount)

    fun mkTyvar(kind: tvkind) : tyvar = ref kind
    
    fun mkABStyc(path: symbol list, arity: int, eq: bool3) : tycon =
	TYCON{stamp = genTycStamp(), path = path, arity = arity, eq = ref eq,
	      kind = ABStyc}
    
    fun mkDEFtyc(path: symbol list, def as TYFUN{arity,...}: tyfun, eq: bool3)
        : tycon =
	TYCON{stamp = genTycStamp(), path = path, arity = arity, eq = ref eq,
	      kind = DEFtyc def}
    
    fun mkDATAtyc(path: symbol list, arity: int, dcons: datacon list, eq: bool3)
	 : tycon =
	TYCON{stamp = genTycStamp(), path = path, arity = arity, eq = ref eq,
	      kind = DATAtyc dcons}
    
    fun mkUNDEFtyc(name: symbol, arity: int) : tycon =
	TYCON{stamp = ~1, path = [name], arity = arity, eq = ref MAYBE,
	      kind = UNDEFtyc NONE}

    fun mkSTR(path, table, env) = 
        STRstr{stamp = genStrStamp(),
               sign = 0,  (* guaranteed not to agree with any valid sig stamp *)
	       table = table,
	       env = env,
	       kind = STRkind{path=path}}

    type stampInfo = {nextTypeStamp: int, nextStrStamp: int}  
      (* first unused free (generative) type and structure stamp *)

    fun currentInfo() = {nextTypeStamp= !tycStampCount,
                         nextStrStamp = !strStampCount}

    fun restoreInfo(stampInfo as {nextTypeStamp,nextStrStamp}):unit=
        (tycStampCount:= nextTypeStamp;
         strStampCount:= nextStrStamp)

    fun pr_stampInfo{nextTypeStamp,nextStrStamp}  = 
           "(nextTypeStamp " ^ makestring (nextTypeStamp:int) ^ "," ^
           "nextStrStramp "  ^ makestring (nextStrStamp:int)  ^ ")"
           

end (* structure Basics *)

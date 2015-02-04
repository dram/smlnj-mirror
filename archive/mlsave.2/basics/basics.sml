(* basics.sml *)

(* basic datatypes *)

structure Basics : BASICS = struct

   structure Symbol = Symbol
   structure Access = Access
   structure Table = Table

   datatype 'a Option = NONE | SOME of 'a;

  local open Symbol Access in

    type spath = symbol list
    type label = symbol
    type stamp = int

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
      | VARIABLE of Access.access (* exception constructor *)

    datatype tvstatus
      = BOUND 		(* universally bound type variables (generics) *)
      | METAARG		(* instantiation metavariables *)
      | METALAM of int  (* lambda depth *)
      | INSTANTIATED of ty
      | FIXED		(* explicitly occuring type variables -- become bound *)

    and tyvar
      = TYVAR of 
	  {stamp : int,
	   name : symbol,
	   status: tvstatus ref}

    and datacon  (* exceptions are a special case with rep=VARIABLE() *)
      = DATACON of
	  {name  : symbol,
	   const : bool,
	   vtype : ty,
	   rep   : conrep ref,
	   dcons : datacon list ref}

    and tycon
      = ATOMtyc of	 (* built in primitive types like int, list *)
	  {stamp : stamp,
	   name  : symbol,
	   arity : int}
      | TYPEtyc of  	 (* defined type constructor *)
	  {stamp  : stamp,
	   name   : symbol,
	   params : tyvar list,
	   def    : ty}
      | DATAtyc of
	  {stamp  : stamp,
	   name   : symbol,
	   params : tyvar list,
	   dcons  : datacon list ref}
      | RECORDtyc of
	  {stamp  : stamp,
	   labels : label list}
      | INDtyc of int list    (* indexed type component *)
      | VARtyc of	 (* dummy type components in sig structures *)
	  {stamp  : stamp,
	   name	  : symbol,
	   arity  : int}
      | UNKNOWNtyc of symbol

    and ty 
      = VARty of tyvar
      | CONty of tycon ref * ty list
      | FLEXRECORDty of
	  {fields : (label * ty) list,
	   completion : ty ref}
      | UNKNOWNty


  (* variables *)

    and var
      = VALvar of 		      (* ordinary variables *)
	  {access: access,
	   name : symbol,
	   vtype : ty ref}
      | OVLDvar of       	      (* overloaded identifier *)
	  {name : symbol,
	   options: {indicators: ty list, variant: var} list ref,
	   scheme: ty}
      | UNKNOWNvar of symbol      (* place holder for backpatching *)


  (* structures and signatures *)

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
	   env   : {t:tycon array, s: Structure array},  (* strenv *)
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
	   access: Access.access,	   
	   binding: Structure}

    and functorVar   (* tentative *)
      = FCTvar of
	  {name: symbol,
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

    (* Note: exceptions are identified with data constructors; they
       no longer have a separate name space, hence no EXNbind constructor *)

    type binder = symbol * binding

    (* trans and thinning belong in access, but Option not available there *)
    datatype trans
      = VALtrans of access	(* old position, val, exn, or unthinned str *)
      | THINtrans of access * lvar * trans list
	    (* old str position, substr thinning *)
      | CONtrans of datacon		(* constructor as value component *)
    
    type thinning = (lvar * trans list) Option

    type stamp = int
    type strenv = {s: Structure array, t: tycon array}
    val emptyStrenv = {s=arrayoflist [], t=arrayoflist []} : strenv
    
    (* getTyc: int list * strenv -> tycon
	 interpret relative path through structures *)
    fun getTyc([i],{s,t}) = t sub i
      | getTyc(i::rest,{s,t}) =
	  let val STRstr{env as {s=s1,t=t1},...} = s sub i
	   in getTyc(rest,env)
	  end
      | getTyc _ = ErrorMsg.Impossible "getTyc"
    
    fun tycStamp(tycon) =
	case tycon
	  of ATOMtyc{stamp,...} => stamp
	   | TYPEtyc{stamp,...} => stamp
	   | DATAtyc{stamp,...} => stamp
	   | RECORDtyc{stamp,...} => stamp
	   | VARtyc{stamp,...} => stamp
	   | _ => ErrorMsg.Impossible "tycStamp"
    
    fun tycName(tycon) =
	case tycon
	  of ATOMtyc{name,...} => name
	   | TYPEtyc{name,...} => name
	   | DATAtyc{name,...} => name
	   | VARtyc{name,...} => name
	   | _ => ErrorMsg.Impossible "tycName"
    

    fun tyconInContext(INDtyc path, env) = getTyc(path,env)
      | tyconInContext(t,_) = t
    
    fun tycStampRel(tyc,env) = tycStamp(tyconInContext(tyc,env))
    
    fun eqTyconRel(tyc1, env1, tyc2, env2) = tycStampRel(tyc1,env1)=tycStampRel(tyc2,env2)
    
    fun eqTycon(tyc1,tyc2) = tycStamp(tyc1) = tycStamp(tyc2)
    
    (* relativize type to structure context, represented by the strenv *)
    (* could optimize to share *)
    (* should merge with freshTy *)
    fun typeInContext(ty:ty,env as {s,t}) : ty =
      if length s = 0 andalso length t = 0 then ty
       else let fun rel ty =
	      case ty
		of VARty _ => ty
		 | CONty(ref tyc, argtys) =>
		     CONty(ref(tyconInContext(tyc,env)), map rel argtys)
		 | _ => ty
              in rel ty
	    end

  (* type functions *)
    
    fun eqTyvar(TYVAR{stamp = n,...}, TYVAR{stamp = m,...}) = (n = m);
    
    fun prune(VARty(TYVAR{status = (stat as ref(INSTANTIATED ty)),...})) : ty =
	  let val pruned = prune ty
	   in stat := INSTANTIATED pruned; pruned
	  end
      | prune(ty as FLEXRECORDty{completion = ref UNKNOWNty,...}) = ty
      | prune(FLEXRECORDty{completion = refty,...}) =
	  let val pruned = prune (!refty)
	   in refty := pruned; pruned
	  end
      | prune ty = ty;
    
    fun eqTy(ty,ty') =
	let fun eq(VARty(tv),VARty(tv')) = eqTyvar(tv,tv')
	      | eq(CONty(ref tycon, args), CONty(ref tycon', args')) =
		  eqTycon(tycon, tycon') andalso List2.all2 eqTy (args,args')
	      | eq _ = false
	 in eq(prune ty, prune ty')
	end;

    fun tyconArity(ATOMtyc{arity,...}) = arity
      | tyconArity(VARtyc{arity,...}) = arity
      | tyconArity(TYPEtyc{params,...}) = length params
      | tyconArity(DATAtyc{params,...}) = length params
      | tyconArity(RECORDtyc{labels,...}) = length labels
      | tyconArity(_) = 0

  (* variable functions *)

    fun mkVALvar(id: symbol, ty: ty ref) : var =
	VALvar{access = LVAR(namedLvar(id)), name = id, vtype = ty}


  (* fixity functions *)

    fun infixleft n = INfix (n+n, n+n)
    fun infixright n = INfix (n+n+1, n+n)


  (* signature, structure, and functor functions *)

    val stampBase = 1000000

    local 
      val tyvStampCount = ref(~1)
      val tycStampCount = ref(2*stampBase)
      val strStampCount = ref(2*stampBase)
    in
      fun genTyvStamp () = (inc tyvStampCount; !tyvStampCount)
      val genTycStamp = ref(fn () => (inc tycStampCount; !tycStampCount))
      val genStrStamp = ref(fn () => (inc strStampCount; !strStampCount))
    end

    fun mkTyvar(id: symbol, status: tvstatus) : tyvar =
	  TYVAR{stamp = genTyvStamp(), name = id, status = ref status}
    
    local
      val name = SymbolTable.StringToSymbol("'x")
    in
      fun newTyvar (status : tvstatus) : tyvar =
	  TYVAR{name = name, stamp = genTyvStamp(), status = ref status}
    end
    
    fun mkATOMtyc(id: symbol, arity: int) : tycon =
	  ATOMtyc{stamp = !genTycStamp(), name = id, arity = arity}
    
    fun mkVARtyc(id: symbol, arity: int) : tycon =
	  VARtyc{stamp = !genTycStamp(), name = id, arity = arity}
    
    fun mkTYPEtyc(id: symbol, args: tyvar list, body: ty) : tycon =
	  TYPEtyc{stamp = !genTycStamp(), name = id, params = args, def = body}
    
    fun mkDATAtyc(id: symbol, args: tyvar list, body: datacon list ref) : tycon =
	  DATAtyc{stamp = !genTycStamp(), name = id, params = args, dcons = body}
    
    fun mkSIG((bindings,table),env,counts) =
	ErrorMsg.Impossible "mkSIG obsolete"

    fun mkSTR(sign,table,env) = 
        STRstr{stamp= !genStrStamp(),
               sign=sign,
	       table=table,
	       env=env,
	       kind=STRkind}

  end (* local open Symbol ... *)

  val internals = ref false  (* governs printing of internal information *)

end

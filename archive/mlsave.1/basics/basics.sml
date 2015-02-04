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
	   tycon : tycon}

    and tycon
      = ATOMtyc of	 (* built in primitive types like int, list *)
	  {stamp : int,
	   name  : symbol,
	   arity : int}
      | SYMtyc of spath * symbol  (* symbolic tycons in signature type specs *)
      | VARtyc of	 (* simple type components of signatures, functor params *)
	  {stamp  : int,
	   context: context,  (* SIGctx or PARctx only *)
	   name   : symbol,
	   arity  : int}
      | TYPEtyc of  	 (* defined type constructor *)
	  {stamp  : int,
	   context: context,
	   name   : symbol,
	   params : tyvar list,
	   def    : ty}
      | DATAtyc of
	  {stamp  : int,
	   context: context,
	   name   : symbol,
	   params : tyvar list,
	   dcons  : datacon list ref}
      | RECORDtyc of
	  {stamp  : int,
	   labels : label list}
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

    and Signature
      = SIG of
          {stamp: int,
	   elements: binding list,
	   env: binding Table.table}

    and pnode
      = STRpnode of
	  {stamp: int,
	   sign: Signature,  (* necessary? *)
	   sons: (symbol * (int * pnode)) list ref}
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
	   spath: symbol list,
	   sign: Signature}
      | SPEC of Signature  (* structure components in signatures *)

    and Functor
      = FUNCTOR of
	  {params: structureVar list,
	   body: Structure,
	   sign: Signature Option}

    and signatureVar
      = SIGvar of
	  {name: symbol,
	   binding: Signature}

    and structureVar
      = STRvar of
	  {name: symbol,
	   access: Access.access,	   
	   binding: Structure,
	   sign: Signature Option}

    and functorVar   (* tentative *)
      = FCTvar of
	  {name: symbol,
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

    type binder = symbol * binding

    (* Note: exceptions are identified with data constructors, but they
       still have a separate name space, hence the EXNbind constructor *)

  (* type functions *)
    
    fun eqTyvar(TYVAR{stamp = n,...}, TYVAR{stamp = m,...}) = (n = m);
    
    fun eqSymbolList(nil,nil) = true
      | eqSymbolList(a::ar,b::br) = Symbol.Eq(a,b) andalso eqSymbolList(ar,br)
      | eqSymbolList _ = ErrorMsg.Impossible "basics.eqSymbolList"

    fun eqPnode(TYCpnode{stamp=a},TYCpnode{stamp=b}) = a=b
      | eqPnode(STRpnode{stamp=a,...},STRpnode{stamp=b,...}) = a=b
      | eqPnode _ = false

    fun eqContext(TOPctx,TOPctx) = true
      | eqContext(FCTctx,FCTctx) = true
      | eqContext(SIGctx,SIGctx) = true
      | eqContext(RELctx(FCTAPP{stamp=a,...},ca),RELctx(FCTAPP{stamp=b,...},cb)) = 
            a = b andalso eqContext(ca,cb)
      | eqContext(PARctx(PARAM{pnode=pa,spath=sa,...}),
	          PARctx(PARAM{pnode=pb,spath=sb,...})) =
	    eqPnode(pa,pb) andalso eqSymbolList(sa,sb)
      | eqContext(SHRctx(_),SHRctx(_)) = true
      | eqContext _ = false

    fun eqTycon(ATOMtyc{stamp=n,...}, ATOMtyc{stamp=m,...}) = (n = m)
      | eqTycon(SYMtyc(p1,n1), SYMtyc(p2,n2)) = 
	  eqSymbolList(p1,p2) andalso Symbol.Eq(n1,n2)
      | eqTycon(VARtyc{stamp=n,context=c,...}, VARtyc{stamp=m,context=d,...}) =
	  (n = m) andalso eqContext(c,d)
      | eqTycon(TYPEtyc{stamp=n,context=c,...}, TYPEtyc{stamp=m,context=d,...}) =
	  (n = m) andalso eqContext(c,d)
      | eqTycon(DATAtyc{stamp=n,context=c,...}, DATAtyc{stamp=m,context=d,...}) =
	  (n = m) andalso eqContext(c,d)
      | eqTycon(RECORDtyc{stamp=n,...}, RECORDtyc{stamp=m,...}) = (n = m)
      | eqTycon(_) = false;
    
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

    fun eqSignature(SIG{stamp=sa,...},SIG{stamp=sb,...}) = (sa = sb)

    local 
      val stampCounter = ref 0
      fun next () = (inc stampCounter; !stampCounter)
    in
      fun mkSTRpnode(sign: Signature) : pnode =
	   STRpnode{stamp = next(), sign=sign, sons=ref []}
      fun mkSIG(elements, env) = SIG{stamp=next(),elements=elements,env=env}
      fun mkDIRECT(context,table) = DIRECT{stamp=next(),context=context,table=table}
      fun mkFCTAPP(c,b,e) = FCTAPP{stamp=next(),context=c,body=b,env=e}
    end

  end (* local open Symbol ... *)

  val internals = ref false  (* governs printing of internal information *)

end

(* Copyright 1989 by AT&T Bell Laboratories *)
structure Unboxed : sig	val unboxedAssign : Basics.ty -> Access.primop
			val unboxedUpdate : Basics.ty -> Access.primop
		    end =
struct
  open Access Prim Basics BasicTypes TypesUtil
  val GENtyc{stamp=intStamp,...} = intTycon
  fun alwaysunboxed ty =
    case TypesUtil.headReduceType ty
     of CONty(GENtyc{arity,
		   kind=ref(DATAtyc [DATACON{const=false,
					    typ=CONty(_,[ty,_]),...}]),...},
	      args) => 
	      alwaysunboxed(applyTyfun(TYFUN{arity=arity,body=ty},args))
      | CONty(GENtyc{kind=ref(DATAtyc dcons),...}, _) =>
	    not(exists (fn (DATACON{rep=CONSTANT _,...})=>false 
			 | _ => true)
		       dcons) 
      | CONty(RECORDtyc nil, _) => true
      | CONty(GENtyc{stamp,...},_) => stamp = intStamp
      | _ => false

  fun unboxedAssign ty =
     let val CONty(_,[ty',_]) = headReduceType ty
	 val CONty(_,[_,ty'']) = headReduceType ty'
      in if alwaysunboxed ty'' then P.unboxedassign else P.:=
     end 
	 handle Bind => P.:=

  fun unboxedUpdate ty =
     let val CONty(_,[ty',_]) = headReduceType ty
	 val CONty(_,[_,_,ty'']) = headReduceType ty'
      in if alwaysunboxed ty'' then P.unboxedupdate else P.update
     end 
	 handle Bind => P.update

end

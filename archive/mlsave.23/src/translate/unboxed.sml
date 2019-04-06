structure Unboxed : sig structure Basics : BASICS
			val unboxedAssign : Basics.ty -> Access.primop
			val unboxedUpdate : Basics.ty -> Access.primop
		    end =
struct

  structure Basics = Basics
  open Access Prim Basics BasicTypes ErrorMsg

  fun alwaysunboxed ty =
    case ty
     of VARty(ref(INSTANTIATED t)) => alwaysunboxed t
      | VARty _  => false
      | CONty(ref(TYCON{kind=DATAtyc dcons,...}), _) =>
	    not(exists (fn (DATACON{rep=CONSTANT _,...})=>false 
			 | _ => true)
		       dcons) 
      | CONty(tr,_) =>
	   (tr = intTycon) orelse (tr = unitTycon)
      | _ => false (* impossible ? *)

  fun unboxedAssign(CONty(_,[CONty(_,[_,VARty(ref kind)]),_])) =
      (case kind
	of INSTANTIATED ty => if alwaysunboxed ty then P.unboxedassign
			       else P.:=
	 | _ => P.:=)
    | unboxedAssign _ = impossible "unboxedAssign in Unboxed"

  fun unboxedUpdate(CONty(_,[CONty(_,[_,_,VARty(ref kind)]),_]))=
      (case kind
	of INSTANTIATED ty => if alwaysunboxed ty then P.unboxedupdate
			       else P.update
	 | _ => P.update)
    | unboxedUpdate _ = impossible "unboxedUpdate in Unboxed"
  
end

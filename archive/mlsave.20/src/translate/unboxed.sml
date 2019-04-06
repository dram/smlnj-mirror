structure Unboxed : sig structure Basics : BASICS
			val unboxedAssign : Basics.ty -> int
			val unboxedUpdate : Basics.ty -> int
		    end =
struct

  structure Basics = Basics
  open Prim Basics BasicTypes ErrorMsg

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
	of INSTANTIATED ty => if alwaysunboxed ty then unboxedAssignSlot
			       else assignSlot
	 | _ => assignSlot)
    | unboxedAssign _ = impossible "unboxedAssign in Unboxed"

  fun unboxedUpdate(CONty(_,[CONty(_,[_,_,VARty(ref kind)]),_]))=
      (case kind
	of INSTANTIATED ty => if alwaysunboxed ty then unboxedUpdateSlot
			       else updateSlot
	 | _ => updateSlot)
    | unboxedUpdate _ = impossible "unboxedUpdate in Unboxed"
  
end

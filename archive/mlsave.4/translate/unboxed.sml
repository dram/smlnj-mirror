structure Unboxed : sig structure Basics : BASICS
			val unboxedAssign : Basics.ty ref -> int
			val unboxedUpdate : Basics.ty ref -> int
		    end
=
struct
  structure Basics = Basics
  open Prim Basics BasicTypes ErrorMsg

  fun alwaysunboxed ty =
    case ty
     of VARty(TYVAR{status=ref(INSTANTIATED t),...}) => alwaysunboxed t
      | VARty _  => false
      | CONty(ref(DATAtyc{dcons=ref l,...}), _) =>
	    not(exists((fn (DATACON{rep=ref(CONSTANT _),...})=>false 
			 | _ => true),
		       l))		    
      | CONty(tr,_) =>
	   (tr = intTycon) orelse (tr = unitTycon)
      | _ => false (* impossible ? *)

  infix before
 fun a before b = a

  fun unboxedAssign(ref(ty as CONty(_,[CONty(_,[_,VARty(TYVAR{status,...})]),_]))) =
   case !status
	 of INSTANTIATED ty => if alwaysunboxed ty then unboxedAssignSlot
				else assignSlot
	  | _ => assignSlot

  fun unboxedUpdate(ref(CONty(_,[CONty(_,[_,_,VARty(TYVAR{status,...})]),_])))=
       case !status
	 of INSTANTIATED ty => if alwaysunboxed ty then unboxedUpdateSlot
				else updateSlot
	  | _ => updateSlot
  
end

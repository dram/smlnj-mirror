(* basictypes.sig *)

signature BASICTYPES = sig

structure Basics: BASICS

val mkTyvar   : Basics.Symbol.symbol * Basics.tvstatus -> Basics.tyvar
val newTyvar  : Basics.tvstatus -> Basics.tyvar
val mkATOMtyc : Basics.Symbol.symbol * int -> Basics.tycon
val mkVARtyc  : Basics.context * Basics.Symbol.symbol * int -> Basics.tycon
val mkTYPEtyc : Basics.context * Basics.Symbol.symbol * Basics.tyvar list * Basics.ty -> Basics.tycon
val mkDATAtyc : Basics.context * Basics.Symbol.symbol * Basics.tyvar list * Basics.datacon list ref
		 -> Basics.tycon

val arrowTycon : Basics.tycon ref
val --> : Basics.ty * Basics.ty -> Basics.ty

val intTycon : Basics.tycon ref
val intTy : Basics.ty

val realTycon  : Basics.tycon ref
val realTy : Basics.ty

val stringTycon  : Basics.tycon ref
val stringTy : Basics.ty

val exnTycon : Basics.tycon ref
val exnTy : Basics.ty

val arrayTycon : Basics.tycon ref

val byte_arrayTycon : Basics.tycon ref
val byte_arrayTy : Basics.ty

val unitTycon : Basics.tycon ref
val unitTy : Basics.ty

val RECORDty : (Basics.label * Basics.ty) list -> Basics.ty
val TUPLEty : Basics.ty list -> Basics.ty

val boolTycon : Basics.tycon ref
val boolTy : Basics.ty
val FALSEdcon : Basics.datacon
val TRUEdcon : Basics.datacon

val refTycon : Basics.tycon ref
val REFdcon : Basics.datacon

val listTycon : Basics.tycon ref
val NILdcon : Basics.datacon
val CONSdcon : Basics.datacon

val newEqualityType : unit -> Basics.ty
val newAssignType : unit -> Basics.ty
val newUpdateType : unit -> Basics.ty

end (* signature BASICTYPES *)

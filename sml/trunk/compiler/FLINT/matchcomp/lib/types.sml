(* mc/lib/types.sml *)

structure Types =
struct

datatype tycon
  = Tycon of string

datatype tyvarKind
  = INST of ty
  | PARAM of LambdaVar.lvar

and ty
  = UNDEFty
  | CONty of tycon * ty list

withtype tyvar = tyvarKind ref
			
type datacon =
     {name: Symbol.symbol,
      width: int}

type label = string

end (* structure Types *)


structure TypesUtil =
struct

local open Types in

  (* equalTycon : tycon * tycon -> bool *)
  fun equalTycon (Tycon s1, Tycon s2) = s1 = s2

  (* equalType : ty * ty -> bool *)
  fun equalType (ty1: Types.ty, ty2: Types.ty) =
      case (ty1,ty2)
       of (UNDEFty, UNDEFty) => true
	| (CONty(tyc1,args1), CONty(tyc2,args2)) =>
	  equalTycon(tyc1, tyc2) andalso ListPair.allEq equalType (args1,args2)

  (* dataconEq : datacon * datacon -> bool *)
  fun dataconEq ({name = name1, ...}: datacon, {name = name2,...}: datacon) =
      name1 = name2

  (* dataconWidth : datacon -> int *)
  fun dataconWidth ({width,...}: datacon) = width

end (* local *)
end (* structure TypesUtil *)

structure BasicTypes =
struct

local open Types in

val intTycon = Tycon "intTycon"
val int32Tycon = Tycon "int32Tycon"
val int64Tycon = Tycon "int64Tycon"
val intinfTycon = Tycon "intinfTycon"
val wordTycon = Tycon "wordTycon"
val word8Tycon = Tycon "word8Tycon"
val word32Tycon = Tycon "word32Tycon"
val word64Tycon = Tycon "word64Tycon"
val boolTycon = Tycon "bool"

val intTy = CONty(intTycon, [])
val int32Ty = CONty(int32Tycon, [])
val int64Ty = CONty(int64Tycon, [])
val intinfTy = CONty(intinfTycon, [])
val wordTy = CONty(wordTycon, [])
val word8Ty = CONty(word8Tycon, [])
val word32Ty = CONty(wordTycon, [])
val word64Ty = CONty(wordTycon, [])
val boolTy = CONty(boolTycon, [])
		  
end (* local *)

end (* structure BasicTypes *)

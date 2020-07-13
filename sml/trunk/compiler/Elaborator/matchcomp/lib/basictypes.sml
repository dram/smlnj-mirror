(* basictypes.sml *)

structure BasicTypes =
struct

local
  open Types TypesUtil
in

val intTycon = mkPrimTycon ("int",0)
val int32Tycon = mkPrimTycon ("int32",0)
val int64Tycon = mkPrimTycon ("int64",0)
val intinfTycon = mkPrimTycon ("intinf",0)
val wordTycon = mkPrimTycon ("word",0)
val word8Tycon = mkPrimTycon ("word8",0)
val word32Tycon = mkPrimTycon ("word32",0)
val word64Tycon = mkPrimTycon ("word64",0)

val intTy = CONty(intTycon, [])
val int32Ty = CONty(int32Tycon, [])
val int64Ty = CONty(int64Tycon, [])
val intinfTy = CONty(intinfTycon, [])
val wordTy = CONty(wordTycon, [])
val word8Ty = CONty(word8Tycon, [])
val word32Ty = CONty(wordTycon, [])
val word64Ty = CONty(wordTycon, [])

val (boolTycon, boolDconsRef) = mkDataTycon("bool", 0)
val boolTy = CONty(boolTycon, nil)
val boolPolyTy = POLY{arity = 0, body = boolTy}
val trueDcon = DCON{name = "true",
		    stamp = Stamp.new(),
		    owner = boolTycon,
		    polyty = boolPolyTy}
val falseDcon = DCON{name = "false",
		     stamp = Stamp.new(),
		     owner = boolTycon,
		     polyty = boolPolyTy}
val _ = boolDconsRef := [trueDcon, falseDcon]

val (listTycon, listDconsRef) = mkDataTycon("list", 1)
fun listTy ty = CONty(listTycon, [ty])
val nilTy = POLY{arity = 1, body = listTy(DBI 0)}
val consTy = POLY{arity = 1,
		  body = funTy(CONty(tupleTycon 2, [DBI 0, listTy(DBI 0)]),
			       listTy(DBI 0))}

val nilDcon = DCON{name = "Nil",
		   stamp = Stamp.new(),
		   owner = listTycon,
		   polyty = nilTy}
val consDcon = DCON{name = "Cons",
		    stamp = Stamp.new(),
		    owner = listTycon,
		    polyty = consTy}
val _ = listDconsRef := [nilDcon, consDcon]

end (* local *)
end (* structure BasicTypes *)

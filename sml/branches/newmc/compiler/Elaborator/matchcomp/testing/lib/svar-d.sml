(* svar-d.sml *)

(* svar, simplified variables, defined in terms of VarCon *)

structure SVar :> SVAR =
struct

local
    structure S = Symbol
    structure SP = SymPath
    structure A = Access
    structure V = VarCon
    structure LV = LambdaVAr
    structure PI = PrimopId
in

fun bug msg = ErrorMsg.impossible ("SVar"^msg)

type svar = VC.var

(* mkSvar : symbol * ty * lvar -> svar *)
fun mkSvar (name, ty, lvar) =
    V.VALvar{path = SP.SPATH [name],
	     typ = ref ty,  (* won't be updated *)
	     btvs = ref nil, (* won't be updated *)
	     access = A.LVAR (LV.mkLvar ()),
	     prim = PI.NonPrim}
			     
(* newSvar : string * ty -> svar *)
fun newSvar (s, ty) =
    mkSvar(S.varSymbol s, ty, LV.mkLvar())

fun svarName (V.VALvar{name, ...}) = name
  | svarName _ = bug "svarName"

fun svarType (V.VALvar{ty, ...}) = !ty
  | svarName _ = bug "svarType"

fun svarLvar (V.VALvar{access, ...}) =
    (case access
      of A.LVAR lv => lv
       | _ => bug "svarLvar - access")
  | svarName _ = bug "svarLvar"

fun svarToVar (svar) = svar

end (* local *)
end (* structure SVar *)

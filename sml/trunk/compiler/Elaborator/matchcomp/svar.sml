(* svar.sml *)

(* svar, simplified, "administrative" variables, defined in terms of VarCon.VALvar.
 * these variables will represent pattern nodes, and will have the corresponding type *)

structure SVar :> SVAR =
struct

local
    structure S = Symbol
    structure SP = SymPath
    structure A = Access
    structure V = VarCon
    structure LV = LambdaVar
    structure PI = PrimopId
in

fun bug msg = ErrorMsg.impossible ("SVar"^msg)

type svar = V.var

(* mkSvar : symbol * ty * lvar -> svar *)
(* "simple", internal administrative variables, not polymorphic;
 * some will have external "source" variables bound to them *)
fun mkSvar (name, ty, lvar) =
    V.VALvar{path = SP.SPATH [name],
	     typ = ref ty,            (* won't be updated for svar*)
	     btvs = ref nil,          (* won't be updated for svar *)
	     access = A.LVAR lvar,    (* always a locally let-bound variable *)
	     prim = PI.NonPrim}
			     
(* newSvar : string * ty -> svar *)
fun newSvar (s, ty) =
    mkSvar(S.varSymbol s, ty, LV.mkLvar())

fun svarName (V.VALvar{path=SP.SPATH syms, ...}) = List.last syms
  | svarName _ = bug "svarName"

fun svarType (V.VALvar{typ, ...}) = !typ
  | svarType _ = bug "svarType"

fun svarLvar (V.VALvar{access, ...}) =
    (case access
      of A.LVAR lvar => lvar
       | _ => bug "svarLvar - access")
  | svarLvar _ = bug "svarLvar"

fun svarToVar (svar: svar) : VarCon.var = svar

end (* local *)
end (* structure SVar *)

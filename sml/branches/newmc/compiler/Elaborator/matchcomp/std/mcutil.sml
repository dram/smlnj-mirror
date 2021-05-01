(* ElabData/matchcomp/mcutil.sml *)

structure MCUtil =
struct

local
  structure S = Symbol
  structure A = Access
  structure LV = LambdaVar
  structure V = VarCon
  structure AS = Absyn
  structure K = Key
  open Absyn MCTypes

  fun bug msg = ErrorMsg.impossible ("MCUtil: "^ msg)

in

(* debugging print function *)
fun printMappedVars (var, svar, header) =
    let val varLvar =
	    case V.varAccess var
	      of A.LVAR lv => lv
	       | _ => bug "printMappedVars 0"
        val svarLvar =
	    case V.varAccess svar
	      of A.LVAR lv => lv
	       | _ => bug "printMappedVars 1"
    in print (concat [header, Symbol.name (V.varName var), "[", LambdaVar.prLvar varLvar,
		      "] ==> ", Symbol.name (V.varName svar), "[",
		      LambdaVar.prLvar svarLvar, "]\n"])
    end

(* intToIntLiteral : int -> Types.ty IntConst.t *)
fun intToIntLiteral (n: int) =
    {ival = IntInf.fromInt n, ty = BasicTypes.intTy} : Types.ty IntConst.t

(* path compatibility *)

(* incompatible : path * path -> bool *)
(* Two paths are incompatible if they diverge at a choice (OR) node.
 * Paths that diverge at a product node (diff first at R links) are
 * compatible; paths that are prefix comparable are compatible. *)
fun incompatible (k1::rest1, k2::rest2) =
      if K.eqKey(k1,k2) then incompatible(rest1, rest2)
      else (case k1
	     of K.R _ => false
	      | _ => true)
  | incompatible (nil, path2) = false
  | incompatible (path1, nil) = false

(* consistentPath : path -> path -> bool *)
(* path1 (a var node path) is consistent with path2 (a decision point) *)
(* NOT USED (except in MatchComp..allConsistent, which is not used. *)
fun consistentPath path1 path2 = 
    case (path1, path2)
     of (K.R n1 :: rest1, K.R n2 :: rest2) =>
	if n1 = n2 then consistentPath rest1 rest2
        else true  (* consistent if diverge at R key *)
      | (k1 :: rest1, k2 :: rest2) =>
	if K.eqKey(k1,k2) then consistentPath rest1 rest2
	else false  (* branch at an OR node *)
      | (_, nil) => true (* consistent all choices on path2 *)
      | (nil, _) => true (* consistent down to variable node *)

(* uniqueVars : V.var list -> V.var list *)
(* elimiate redundant vars -- returns reduced list of vars with each var occurring
 * only once, in original order *)
fun uniqueVars vars =
    let fun unique (vs, nil) = rev vs
	  | unique (vs, v::others) =
	    if List.exists (fn var => V.eqVar(var, v)) vs
	    then unique (vs, others)     (* ignore v -- v already in vs *)
	    else unique (v::vs, others)  (* v is a new variable, add it *)
     in unique (nil, vars)
    end

(* infoToSvar : andor -> var *)
fun infoToSvar (info : AOinfo) =
    V.newVALvar (S.varSymbol (pathToString (#path info)), #typ info)

(* andorToSvar : andor -> var *)
fun andorToSvar andor =
    infoToSvar (getInfo andor)

end (* local *)
end (* structure MCUtil *)

(* varenvs.sml *)

(* Two flavors of var envs.
 *   1. varenvmc: internal to matchcomp
     
 *   2. varenvac: used on rhs (via transmatch) for (a) alpha-converting
 *      source vars and (b) substituting svars for source vars in single-use
 *      rhs.
 *)

(* varenvMC : var * ruleno -> svar * path
At genRHS, this environment is used to filter out nonconsistent vars (based on
path) and map them to svars. These svars then become either

  a) arguments to a rhs function (hence only local to this match), or
  b) are substituted for the corresponding source variables in the rhs.
     (becoming part of the varenvac argument to transMatch applied to the
     rhs exp).

This environment is build as we generate the match code. At the rhs leaf nodes,
it is processed to select the relevant vars (by rule and path) to produce the
product for a) or b).  During match compilation, we accumulate the information
but don't use it until we need it for translating the rhs.

Note that we can't simply "precompute" the rhs functions for multiple-use rhs's,
or can we???  -- only need the source variables (even with OR patterns).
Then at leafs, have to compute the corresponding svars in the right order.
 *)


(* varenvAC : var -> var
A simple replacement mapping applied to free variable occurrences in the rhs
as it is being translated by transmatch (and any level of nested scoping, hence
propagated and augmented through nested matchcomps.

The range var is either 

  a) an svar being substituted for the var in a match rhs, or
  b) an alpha-conversion variant of the var (new lvar).

The b) case is to deal with the fact that a given variable that occurs
multiple times in a single rule because of OR patterns might otherwise
be bound multiple types (i.e its lvar might be bound multiple times) in
the translated plambda/flint expression.
 *)

structure VarEnvMC =
struct

local
  structure LV = LambdaVar
  structure M = LV.Map
  structure V = VarCon
  open MCTypes
in

type varenvMC = (ruleno * path * svar) list M.map

val empty : varenvMC = M.empty

(* bindVar : V.var * (ruleno * path * svar) * varenvmc -> varenvmc *)
fun bindVar (var, (ruleno, path, svar), varenvmc) =
    let val lvar = V.varToLvar var
     in case lookVar(varenvmc, lvar)
          of NONE => M.insert(varenvmc, lvar, [(ruleno, path, svar)])
           | SOME bindings =>
	     M.insert(varenvmc, lvar, (ruleno, path, svar) :: bindings)
    end

(* lookVar : V.var * varenvMC -> (ruleno, * path * svar) list *)
fun lookVar (var, varenvmc) = M.find (varenvmc, V.varToLvar var)

end (* local *)
end (* structure VarEnv *)

structure VarEnvAC =
struct

local
  structure LV = LambdaVar
  structure M = LV.Map
  structure V = VarCon
  open MCTypes
in

type varenvAC = (V.var * V.var) list

val empty : varenvAC = nil

(* bind : V.var * V.var * varenvAC -> varenvAC *)
fun bind (var, svar, venv: varenvAC) = (var,svar) :: venv

(* look : varenvAC * V.var -> V.var option *)
fun look (varenvAC, var) =
    let val lvar = V.varToLvar var
	fun loop nil = NONE
	  | loop ((var0,svar0)::rest) = 
	      if LV.same (V.varToLvar var0, lvar) then SOME svar0
	      else loop rest
    in loop varenvAc
    end

(* append : varenvAC * varenvAC -> varenvAC *)
(* "domains" of the two environments will be disjoint *)
fun append (e1: varenvAC, v2: varenvAC) = e1 @ e2

(* range : varenvAC -> V.var list *)
fun range (venv: varenvAC) = map #2 venv

end (* local *)
end (* structure VarEnvAC *)

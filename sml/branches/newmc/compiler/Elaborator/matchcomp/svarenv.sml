(* svarenv.sml *)

(* environments mapping andor node ids (ints) to svars (== var) *)

(* These environments take the place of "pregenerated" svar
 * components previously included in the AND-OR nodes (the svar
 * field. New svars for nodes are generated as we process the
 * nodes in genAndor and are recorded in an svarenv that is "private"
 * to each node use in the decision tree.  Thus one node may have
 * multiple svars associated to multiple uses of the node in the
 * decision tree. This avoids the problem (bug) where the same svar
 * lvar was bound multiple times in the plambda translation of a match.
 *)

structure SVarEnv =
struct

local
    structure V = VarCon
in

structure M = IntBinaryMap

type svarenv = V.var M.map
(* key = int ( = andor.info.id) *)

val empty = M.empty

(* bindSvar : id * V.var * svarenv -> svarenv *)
fun bindSvar (id: int, svar: V.var, env: svarenv) =
    M.insert(env, id, svar)

(* lookSvar : svarenv * id -> V.var option *)
fun lookSvar (env, id) = M.find (env, id)

end (* local *)
end (* structure SVarEnv *)

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
 *
 * The fresh svar for an AndOr node is generated and bound in an svarenv
 * just before applying genAndor to build a corresponding decision tree.
 * (see svarTop, svarenv0 at the end of the body of MatchComp..genMatch, and
 * collectChild in MatchComp..genMatch..genAndor).
 *
 * [Note: it may be possible to eliminate this device if we introduce
 * some form of "pattern continuations" to deal with the replication
 * of AndOr subtrees during construction of decision trees (i.e. deferred
 * OR nodes being used in multiple variants of a dominating OR case).]
 *)

structure SVarEnv =
struct

local
    structure V = VarCon
in

structure M = IntBinaryMap

(* svarenv: a finite mapping from AndOr node id numbers (andor.info.id) to
 * "svars", which have type V.var, but are "administrative", not source, variables. *)
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

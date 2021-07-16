(* mc/lib/symbol.sml *)

structure Symbol =
struct

type symbol = string

(* make : string -> symbol *)
fun make s = s

(* name: symbol -> string *)
fun name (s: symbol) = s

(* eq: symbol * symbol -> bool *)
fun eq (s1,s2) = s1 = s2

end (* structure Symbol *)

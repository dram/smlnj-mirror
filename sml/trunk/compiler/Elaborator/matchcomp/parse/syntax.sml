(* syntax.sml *)

(* pattern "concrete" syntax *)

structure PatSyntax =
struct

datatype pattern
  = Id of string
  | NumPat of string
  | AppPat of string * pattern
  | OrPat of pattern * pattern
  | TuplePat of pattern list
  | NullPat
	     
end (* structure PatSyntax *)

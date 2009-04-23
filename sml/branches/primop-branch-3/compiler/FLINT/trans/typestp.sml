structure TypesTP =
struct

(* tycpaths: an intermediate representation of FLINT types that will be
 * translated into true FLINT types in translate. *)

datatype tycpath (* (instantiated) functor type parameter path *)
  = TP_VAR of { tdepth: DebIndex.depth, num: int, kind: LT.tkind }   
  | TP_TYC of Types.tycon
  | TP_FCT of tycpath list * tycpath list
  | TP_APP of tycpath * tycpath list
  | TP_SEL of tycpath * int

end (* structure TypesTP *)

(* structure should be renamed "TycPaths" and then eliminated by directly
 * constructing FLINT types. *)

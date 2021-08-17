(* FLINT/match/mcutil.sml; revmc *)

structure MCUtil =
struct

datatype link = VI | DC

datatype subcase
  = CONST
  | DCARG of andor
  | VELEMS of andor list

and andor
  = OR of (string * subcase) list

fun branch x = NONE

fun pathToNode (andor: andor, path: link list) =
    case (andor, path)
     of (_, nil) => SOME andor
      | (OR cases, DC :: rest) =>
	(case (branch (), rest)
	  of (SOME (DCARG argAndor), _) => NONE
	   | (SOME CONST, nil) => NONE
	   | (SOME (VELEMS elems), VI::rest) => NONE
	   | (NONE, _) => NONE)

end (* structure MCUtil *)


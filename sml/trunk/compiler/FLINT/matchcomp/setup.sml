(* setup.sml *)

(* test rig for testing and debugging the match compiler *)
(* prerequisite structures *)

structure T =  (* substitute for Types *)
struct

type ty	      

type datacon

end

structure TU =  (* substitute for TypesUtil *)
struct

fun dataconEq

end

structure Absyn =
struct	      

datatype pat

end


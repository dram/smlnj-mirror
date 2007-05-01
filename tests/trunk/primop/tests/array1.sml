(* Control.internals := true;
(* Control.Print.printDepth := 20; *)
Control.FLINT.printFctTypes := true;
Control.FLINT.print := true; *)

signature S0 =
sig
    val length : 'a array -> int
end

structure M0 =
struct
  val length = Array.length
end

signature S1 =
sig
    structure M : S0
end

structure M1 =
struct
  structure M = M0 :> S0
end

signature S2 = 
sig
    structure M : S1
end

structure M2 =
struct
  structure M' = M1 :> S2
end

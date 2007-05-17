Control.Elab.internals := true;
Control.FLINT.printFctTypes := true;
Control.FLINT.print := true;


signature S0 =
sig
    val length : int array -> int
end

signature S1 =
sig
    val length : (int * bool) array -> int
end

signature S2 =
sig
    val length : ((int -> int) array) array -> int
end

signature S3 =
sig
    val length : ('a * 'a) array -> int
end

structure M =
struct
  val length = Array.length
end

structure M0 = M :> S0
structure M1 = M :> S1
structure M2 = M :> S2
structure M3 = M :> S3


Control.CG.smdebugging := true;

signature S0 = 
sig
    val length : int array -> int
end

signature S1 =
sig
    structure M1 : S0
end

structure M0 =
struct 
    val length = Array.length
end

structure M : S1 =
struct
  structure M2 = M0 :> S0
end

Control.internals := true;
Control.FLINT.printFctTypes := true;
Control.FLINT.print := true;

signature S = 
  sig 
      val x : int * int -> int
  end

structure M :> = 
  struct 
      val x = Int.+
  end

val _ = M.x(1,2)
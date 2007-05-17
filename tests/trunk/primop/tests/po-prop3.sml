Control.Elab.internals := true;
Control.FLINT.printFctTypes := true;
Control.FLINT.print := true;

signature S = 
  sig 
      val x : int * int -> int
  end

structure M :> S = 
  struct 
      val x = Int.+
  end

val _ = M.x(1,2)

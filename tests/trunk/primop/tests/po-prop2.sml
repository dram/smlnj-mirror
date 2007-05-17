Control.Elab.internals := true;
Control.FLINT.printFctTypes := true;
Control.FLINT.print := true;

(* This kind of primop propagation is currently (5/1/07) not supported *)
val x = (1; Int.+)
val _ = x(1,2)

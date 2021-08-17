(* bt2c.sml -- from trace-debug-profile/back-trace.sml *)

structure S :
  sig
    val g : 'a -> unit
    val h : unit -> unit
  end =

struct

    fun g x = ()  (* m : ['a] ('a -> unit *)

    fun h () = let val r : (unit -> unit) ref = ref g in () end

end;

(*
[opening bt2c.sml]
Error: Compiler bug: SpecializeNvar: unexpected case A in kBnd

while in specialize phase
  raised at:	../compiler/Basics/errormsg/errormsg.sml:55.14-55.19
		../compiler/Basics/stats/stats.sml:198.40
val it = () : unit
*)

(* bt2b.sml -- from trace-debug-profile/back-trace.sml *)

structure BackTrace :
  sig
    val m : (unit -> 'a) -> 'a
    val g : unit -> unit
  end =

struct

    fun m f = f ()  (* m : ['a] (unit -> 'a) -> 'a *)

    fun g () = let val r : ((unit -> unit) -> unit) = m in () end

end;

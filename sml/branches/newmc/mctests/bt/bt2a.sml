(* bt2a.sml -- from trace-debug-profile/back-trace.sml *)

structure BackTrace :
  sig
    val m : (unit -> 'a) -> 'a
    val g : unit -> unit
  end =

struct

    fun m f = f ()  (* m : ['a] (unit -> 'a) -> 'a *)

    fun g () = let val r : ((unit -> unit) -> unit) ref = ref m in () end

end;

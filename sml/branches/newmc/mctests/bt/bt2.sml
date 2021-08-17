(* bt2.sml -- from trace-debug-profile/back-trace.sml *)

structure BackTrace :
  sig
    val m : (unit -> 'a) -> 'a
    val g : unit -> unit
  end =

struct

    val r : ((unit -> unit) -> unit) list ref = ref nil

    fun m f = f ()  (* m : ['a] (unit -> 'a) -> 'a *)

    fun g () = r := [m]

end;

(* bt1.sml -- from trace-debug-profile/back-trace.sml *)

structure S:
  sig
    val f : (bool -> 'a) -> 'a
  end =

struct
    fun f g = g true
end;

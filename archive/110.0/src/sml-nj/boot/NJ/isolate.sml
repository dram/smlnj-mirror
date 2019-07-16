(* isolate.sml
 *
 * COPYRIGHT (c) 1997 AT&T Labs Research.
 *)
structure Isolate : sig

    val isolate : ('a -> unit) -> 'a cont

  end = struct

    exception Isolate
    val isolate : ('a -> unit) -> 'a cont = InLine.isolate

  end;


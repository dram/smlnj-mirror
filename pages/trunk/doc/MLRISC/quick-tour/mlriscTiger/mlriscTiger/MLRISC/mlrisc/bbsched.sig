signature BBSCHED = sig
  structure F : FLOWGRAPH

  val bbsched : F.cluster -> unit
  val finish : unit -> unit
  val cleanUp : unit -> unit
end

(*
 * $Log$
 * Revision 1.1  2001/10/11 09:52:26  macqueen
 * Initial revision
 *
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)

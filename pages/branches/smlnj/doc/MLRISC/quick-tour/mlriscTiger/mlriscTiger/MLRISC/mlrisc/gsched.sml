(* gsched.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature GLOBAL_SCHED = sig
  structure F : FLOWGRAPH

  val gsched : F.block list -> unit

end

functor GlobalSched(structure Flowgraph : FLOWGRAPH
			  val codegen : Flowgraph.cluster -> unit) =
struct
    fun gsched cluster = codegen cluster
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

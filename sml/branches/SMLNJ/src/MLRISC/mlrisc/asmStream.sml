(* asmStream.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(* AsmStream - this structure is available to all codegenerators.
 *             Typically asmOutStream is rebound to a file.
 *)

structure AsmStream = struct
  val asmOutStream = ref TextIO.stdOut
  fun withStream stream body x = let
     val s = !asmOutStream 
     val _ = asmOutStream := stream
  in
    (body x before asmOutStream := s)
       handle e => (asmOutStream := s; raise e)
  end   
end



(*
 * $Log: asmStream.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)

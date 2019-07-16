(* cont.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Continuation operations
 *)

structure Cont : CONT =
  struct

    type 'a cont = 'a cont

    val callcc : ('a cont -> 'a) -> 'a = InlineT.callcc
    val throw : 'a cont -> 'a -> 'b = InlineT.throw

  (* a function for creating an isolated continuation from a function *)
    val isolate = Isolate.isolate

  (* versions of the continuation operations that do not capture/restore the
   * exception handler context.
   *)
    type 'a control_cont = 'a InlineT.control_cont
    val capture = InlineT.capture
    val escape = InlineT.escape

  end;


(*
 * $Log: cont.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:40  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1  1997/02/11 20:44:47  george
 *   Version 109.25.1
 *
 *)

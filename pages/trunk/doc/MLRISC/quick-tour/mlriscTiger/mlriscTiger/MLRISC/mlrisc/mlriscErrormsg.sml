structure MLRiscErrorMsg = struct
  exception Error
  val print = fn s => TextIO.output(TextIO.stdOut, s)
  fun impossible msg =
      (app print ["Error: MLRisc bug: ",msg,"\n"];
       TextIO.flushOut TextIO.stdOut;
       raise Error)
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

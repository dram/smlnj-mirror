signature ALPHA32SHUFFLE = sig
  structure I : ALPHA32INSTR

  type t = {regMap:int -> int, temp:I.ea option, dst:int list, src:int list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end

(*
 * $Log: alpha32Shuffle.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)

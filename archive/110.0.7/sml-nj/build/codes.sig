(* Copyright 1989 by AT&T Bell Laboratories 
 *
 *)
signature ABSTRACT_MACHINE = sig
  type EA
  val arithtemps : EA list
  val miscregs : EA list
  val floatregs : EA list
  val savedfpregs : EA list
end

signature CODEGENERATOR = sig
  type csegments = { c0: Word8Vector.vector, cn: Word8Vector.vector list , name : string option ref}
  val architecture : string
  val codegen : ErrorMsg.errors * Lambda.lexp -> csegments
  (* val collect : unit -> string *)
end

signature CMACHINE_CODEGENERATOR = sig
  include CODEGENERATOR
  structure CMachine: CMACHINE
end


(*
 * $Log: codes.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/08/25 19:20:01  riccardo
 *   Added support for tagging code objects with their source/bin file name.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:27  george
 *   Version 109.24
 *
 *)

(* Copyright 1996 by AT&T Bell Laboratories *)
(* persstamps.sig *)

signature PERSSTAMPS =
sig
  eqtype persstamp

  val compare : persstamp * persstamp -> order
      (* total ordering on persstamps *)

  val toHex : persstamp -> string
      (* convert the persstamp to a printable representation (hex digits) *)

  val toBytes   : persstamp -> Word8Vector.vector
  val fromBytes : Word8Vector.vector -> persstamp

end (* signature PERSSTAMPS *)

(*
 * $Log: persstamps.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:10  george
 *   Version 109.24
 *
 *)

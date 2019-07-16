(* math-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature MATH =
  sig
    type real

    val pi : real
    val e  : real
    val sqrt  : real -> real
    val sin   : real -> real
    val cos   : real -> real
    val tan   : real -> real
    val asin  : real -> real
    val acos  : real -> real
    val atan  : real -> real
    val atan2 : real * real -> real
    val exp   : real -> real
    val pow   : real * real -> real
    val ln    : real -> real
    val log10 : real -> real
    val sinh  : real -> real
    val cosh  : real -> real
    val tanh  : real -> real
  end


(*
 * $Log: math-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:38  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/05/09 20:20:02  george
 *   Implemented remaining basis functions. -- appel
 *
 * Revision 1.1.1.1  1997/01/14  01:38:15  george
 *   Version 109.24
 *
 *)

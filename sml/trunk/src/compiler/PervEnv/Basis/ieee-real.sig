(* sigs/ieee-real.sig
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *)

signature IEEE_REAL =
  sig

    exception Unordered

    datatype real_order = LESS | EQUAL | GREATER | UNORDERED

    datatype nan_mode = QUIET | SIGNALLING

    datatype float_class
      = NAN of nan_mode
      | INF
      | ZERO
      | NORMAL
      | SUBNORMAL

    datatype rounding_mode
      = TO_NEAREST
      | TO_NEGINF
      | TO_POSINF
      | TO_ZERO

    val setRoundingMode : rounding_mode -> unit
    val getRoundingMode : unit -> rounding_mode

    type decimal_approx = {
	kind : float_class,
	sign : bool,
	digits : int list,
	exp : int
      }

    val toString   : decimal_approx -> string
    val fromString : string -> decimal_approx option

  end;


(*
 * $Log: ieee-real.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:04  george
 * Version 110.5
 *
 *)

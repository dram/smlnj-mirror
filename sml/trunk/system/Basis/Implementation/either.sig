(* either.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The EITHER signature is a propsed SML Basis Library extension (proposal 2015-002).
 *)

signature EITHER =
  sig

    datatype ('left, 'right) either = INL of 'left | INR of 'right

    val isLeft : ('left, 'right) either -> bool
    val isRight : ('left, 'right) either -> bool

    val asLeft : ('left, 'right) either -> 'left option
    val asRight : ('left, 'right) either -> 'right option

    val map : ('ldom -> 'lrng) * ('rdom -> 'rrng)
	      -> ('ldom, 'rdom) either
		-> ('lrng, 'rrng) either
    val app : ('left -> unit) * ('right -> unit)
	      -> ('left, 'right) either
		-> unit

    val partition : (('left, 'right) either) list -> ('left list * 'right list)

  end

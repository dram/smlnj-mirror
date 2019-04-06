(* Copyright 1989 by AT&T Bell Laboratories *)
(* ieeereal.sml
 *
 * J.H. Reppy
 * Cornell University
 * Ithaca, NY  14853
 * jhr@cs.cornell.edu
 *
 * HISTORY:
 *   03/15/89  created
 *   11/20/89  changed argument sig of IEEEReal
 *
 * Support for IEEE floating-point constants (for M68881 and SPARC FPU).
 *
 * Double precision format (for normalized numbers):
 *   Bias = 1023.
 *   Exponent = 11 bits.
 *   Range of exponent = [1..2046]
 *   Mantissa = 52 (+1) bits.
 *   Value = (-1)^s * 2^(e-1023) * 1.f
 *
 * Sub-normal numbers (biased exponent = 0)
 *   Bias = 1022
 *   Mantissa = 52 bits.
 *   Value = (-1)^s * 2^-1022 * 0.f
 *)

functor IEEEReal (val emitWord : int -> unit) : PRIMREAL =
struct

    val significant = 53 (* 52 + redundant 1 bit *)

    val minexp = ~1022 and  maxexp = 1025

  (* Emit a real constant with the given sign, the mantissa frac and with the
   * unbiased exponent exp.
   *)
    fun emitreal (sign, frac, exp) =
		 (emitWord (if frac(0,1)=1
				then Bits.orb(Bits.lshift(sign,15),
	 			       Bits.orb(Bits.lshift(exp+1022,4), 
						frac(1,4)))
				else 0);
		  emitWord (frac(5,16));
		  emitWord (frac(21,16));
		  emitWord (frac(37,16)))

end (* functor IEEEReal *)

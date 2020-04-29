(* int-const.sml
 *
 * A common representation of typed integer literals to use throughout the
 * different intermediate representations (from Absyn to CPS).
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure IntConst : sig

    type 'ty t = {
	ival : IntInf.int,	(* the value *)
	ty : 'ty		(* the "type" of the value *)
      }

    val toString : 'ty t -> string

    val fmt : ('ty -> string) -> 'ty t -> string

  (* do two constants have equal values? *)
    val same : 'ty t * 'ty t -> bool

  end = struct

    type 'ty t = {ival : IntInf.int, ty : 'ty}

    fun toString {ival, ty} = IntInf.toString ival

    fun fmt tyToString {ival, ty} = concat[IntInf.toString ival, ":", tyToString ty]

    fun same (a : 'ty t, b : 'ty t) = (#ival a = #ival b)

  end

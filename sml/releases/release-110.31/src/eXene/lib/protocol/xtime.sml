(* xtime.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * An abstract interface to server time values.
 *)

structure XTime : sig

    datatype time = XT of Word32.word

    val toReal : time -> real

    val + : (time * time) -> time
    val - : (time * time) -> time

    val <  : (time * time) -> bool
    val <= : (time * time) -> bool
    val >  : (time * time) -> bool
    val >= : (time * time) -> bool

  end = struct

  (* X time values are 32-bit values in milliseconds since the server was booted
   * and they wrap around every 49.7 days.
   *)
    datatype time = XT of Word32.word

    fun binOp rator (XT t1, XT t2) = XT(rator(t1, t2))
    fun cmpOp rator (XT t1, XT t2) = rator(t1, t2)

(** NOTE: the following works for small time values, but not for big ones!! **)
    fun toReal (XT w) = Real.fromInt(Word32.toInt w)

    val (op +) = binOp Word32.+
    val (op -) = binOp Word32.-

    val (op <)  = cmpOp Word32.<
    val (op <=) = cmpOp Word32.<=
    val (op >)  = cmpOp Word32.>
    val (op >=) = cmpOp Word32.>=

  end (* XTime *)


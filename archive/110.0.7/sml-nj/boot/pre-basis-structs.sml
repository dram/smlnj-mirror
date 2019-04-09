(* pre-basis-structs.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * These are basis structures with only types, so that the basis signatures
 * can compile.
 *
 *)

structure Int =
  struct
    type int = PrimTypes.int
  end;

structure Int31 =
  struct
    type int = PrimTypes.int
  end;

structure Int32 =
  struct
    type int = PrimTypes.int32
  end;

structure Word =
  struct
    type word = PrimTypes.word
  end;

structure Word8 =
  struct
    type word = PrimTypes.word8
  end;

structure Word31 =
  struct
    type word = PrimTypes.word
  end;

structure Word32 =
  struct
    type word = PrimTypes.word32
  end;

structure Real64 =
  struct
    type real = PrimTypes.real
  end;

structure StringCvt =
  struct
    datatype radix = BIN | OCT | DEC | HEX
    datatype realfmt
      = EXACT
      | SCI of int option
      | FIX of int option
      | GEN of int option
    type ('a, 'b) reader = 'b -> ('a * 'b) option
  end;


(*
 * $Log: pre-basis-structs.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:38  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/07/31 17:24:59  jhr
 *   We are now using 32-bit ints to represent the seconds portion of a
 *   time value.  This was required to handle the change in the type of
 *   Time.{to,from}{Seconds,Milliseconds,Microseconds}.
 *
 * Revision 1.2  1997/05/29  14:44:24  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:16  george
 *   Version 109.24
 *
 *)

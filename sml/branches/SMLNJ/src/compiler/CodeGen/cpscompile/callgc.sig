(* callGc.sig --- cluster of gc invocation code.
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

(* All code to invoke GC is generated once at the end of the
 * compilation unit---with one exception. For each cluster, a 
 * call to GC is a jump  to the end of the cluster  where there 
 * is another jump.
 * Code to invoke GC for known functions is generated at the end of
 * the cluster. This is important as there may be spilling across
 * gc invocation calls.
 *)

signature CALLGC = sig
  structure T : MLTREE
  type t = {maxAlloc: int,
	    regfmls:  T.mlrisc list,
	    regtys : CPS.cty list,
	    return: T.stm}

  val stdCheckLimit : t -> unit
  val knwCheckLimit : t -> unit
  val emitLongJumpsToGCInvocation : int Intmap.intmap -> unit
  val emitInvokeGC : int Intmap.intmap -> unit
end

(*
 * $Log: callgc.sig,v $
 * Revision 1.2  1997/07/17 12:36:07  george
 *   The regmap is now represented as an int map rather than using arrays.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:33  george
 *   Version 109.24
 *
 *)

(* posix-flags.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Signature for bit flags.
 *
 *)

signature POSIX_FLAGS =
  sig
    eqtype flags

    val toWord   : flags -> SysWord.word
    val fromWord : SysWord.word -> flags

      (* Create a flags value corresponding to the union of all flags
       * set in the list.
       *)
    val flags  : flags list -> flags

      (* allSet(s,t) returns true if all flags in s are also in t. *)
    val allSet : flags * flags -> bool

      (* anySet(s,t) returns true if any flag in s is also in t. *)
    val anySet : flags * flags -> bool
  end


(*
 * $Log: posix-flags.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:56  george
 * Version 110.5
 *
 *)

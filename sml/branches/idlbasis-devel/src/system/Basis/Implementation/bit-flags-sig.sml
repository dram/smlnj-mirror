(* bit-flags-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from bit-flags.mldoc (v. 1.6; 2000-05-28)
 *)

signature BIT_FLAGS =
  sig
    eqtype flags
    val toWord   : flags -> SysWord.word
    val fromWord : SysWord.word -> flags
    val all : flags
    val flags : flags list -> flags
    val intersect : flags list -> flags
    val clear : flags * flags -> flags
    val allSet : flags * flags -> bool
    val anySet : flags * flags -> bool
    
  end

(* alpha32gen.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure Alpha32MC = 
  FLINTComp(
    structure Gen=Alpha32CG
    fun collect() = (Alpha32CG.finish(); CodeString.getCodeString()))




(* rl.sml
 *
 *   Simple stub for readline to hide NLFFI's C types and take
 *   care of freeing malloc'd data.							
 *
 * Copyright (c) 2004 by Toyota Technological Institute at Chicago
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure RL : sig
    type prompt
    val noprompt : prompt
    val prompt : string -> prompt
    val rl : prompt -> string option
end = struct
    type prompt = (C.uchar, C.ro) C.obj C.ptr'
    val noprompt = C.Ptr.null'
    val prompt = ZString.dupML'
    fun rl p = let
      val cres = F_readline.f' p
    in
      if C.Ptr.isNull' cres then NONE
      else SOME (ZString.toML' cres) before (F_add_history.f' (C.Ptr.ro' cres);
					     C.free' cres)
    end
end

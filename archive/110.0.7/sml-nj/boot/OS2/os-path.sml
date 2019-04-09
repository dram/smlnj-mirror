(* os-path.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is the OS/2 implementation of the generic OS.Path structure.
 *
 * Peter Bertelsen, July 1995.
 *
 *)

structure OS_Path = OS_PathFn (
  struct

    structure SS = Substring
    val unsafeSub = InlineT.CharVector.sub

    exception Path

    datatype arc_kind = Null | Parent | Current | Arc of string

    fun classify "" = Null
      | classify "." = Current
      | classify ".." = Parent
      | classify a = Arc a

    val parentArc  = ".."
    val currentArc = "."
    val arcSepChar = #"\\"
    val arcSep     = "\\"
    val volSepChar = #":"
    val emptySS    = Substring.all ""

    (* unsafeVolOk: string -> bool *)
    fun unsafeVolOk vol = Char.isAlpha(unsafeSub (vol,0)) andalso
                          (unsafeSub(vol,1) = volSepChar)

    (* volOk: string -> bool *)
    fun volOk vol = (String.size vol = 2) andalso (unsafeVolOk vol)

    fun validVolume (_, vol) = (SS.isEmpty vol) orelse volOk(SS.string vol)

    (* splitPath: 'a * substring -> bool * 'a * substring *)
    fun splitPath (vol, s) = 
      if (SS.size s >= 1) andalso (SS.sub(s, 0) = arcSepChar)
      then (true, vol, SS.triml 1 s)
      else (false, vol, s)

    fun splitVolPath "" = (false, emptySS, emptySS)
      | splitVolPath s = if (String.size s >= 2) andalso (unsafeVolOk s)
          then splitPath (SS.splitAt (SS.all s, 2))
          else splitPath (emptySS, SS.all s)

    fun checkVol vol = if (volOk vol) then vol else raise Path

    fun joinVolPath (true, "", "") = arcSep
      | joinVolPath (true, "", s)  = arcSep ^ s
      | joinVolPath (true, vol, "") = (checkVol vol) ^ arcSep
      | joinVolPath (true, vol, s) = (checkVol vol) ^ arcSep ^ s
      | joinVolPath (false, "", s) = s
      | joinVolPath (false, vol, "") = checkVol vol
      | joinVolPath (false, vol, s) = (checkVol vol) ^ s

  end);


(*
 * $Log: os-path.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:40  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:21  george
 *   Version 109.24
 *
 *)

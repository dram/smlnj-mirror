(* os-path.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Win32 implementation of the OS.Path structure.
 *
 *)

structure OS_Path = OS_PathFn (
  struct
      structure W32G = Win32_General
      structure C = Char
      structure S = String
      structure SS = Substring

      exception Path

      datatype arc_kind = Null | Parent | Current | Arc of string

      fun classify "" = Null
	| classify "." = Current
	| classify ".." = Parent
	| classify a = Arc a

      val parentArc  = ".."
      val currentArc = "."

      val volSepChar = #":"

      val arcSepChar = W32G.arcSepChar
      val arcSep = S.str arcSepChar

      fun volPresent vol = 
          (String.size vol >= 2) andalso
	  (C.isAlpha(S.sub(vol,0)) andalso (S.sub(vol,1) = volSepChar))

      fun validVolume (_,vol) = 
	  (SS.isEmpty vol) orelse volPresent(SS.string vol)

      val emptySS    = SS.all ""

      fun splitPath (vol, s) = 
	  if (SS.size s >= 1) andalso (SS.sub(s, 0) = arcSepChar) then
	       (true, vol, SS.triml 1 s)
	  else (false, vol, s)

      fun splitVolPath "" = (false, emptySS, emptySS)
	| splitVolPath s = 
	  if volPresent s then splitPath (SS.splitAt (SS.all s, 2))
	  else splitPath (emptySS, SS.all s)

      fun joinVolPath arg = 
	  let fun checkVol vol = if (volPresent vol) then vol else raise Path
	      fun aux (true,"","") = arcSep
		| aux (true,"",s) = arcSep^s
		| aux (true,vol,"") = (checkVol vol)^arcSep
		| aux (true,vol,s) = (checkVol vol)^arcSep^s
		| aux (false,"",s) = s
		| aux (false,vol,"") = checkVol vol
		| aux (false,vol,s) = (checkVol vol)^s
	  in  aux arg
	  end
  end);



(*
 * $Log: os-path.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:40:02  george
 * Version 110.5
 *
 *)

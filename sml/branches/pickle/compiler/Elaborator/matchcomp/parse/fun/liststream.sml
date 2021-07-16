(* liststream.sml *)

(* ListStream --
 * streams represented by char lists *)

structure ListStream :> CHAR_STREAM
			where type source = char list  =
struct

   type instream = char list

   type source = char list

   fun mkInstream (chars: char list) = chars

   fun charRdr [] = NONE
     | charRdr (c::cs) = SOME(c, cs)

end (* structure ListStream *)


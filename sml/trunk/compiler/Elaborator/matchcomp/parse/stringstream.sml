(* stringstream.sml *)

(* StringStream --
 * streams represented by strings *)

structure StringStream :> CHAR_STREAM
                          where type source = string  =
struct

  type instream = {buffer: (string * int), pos: int}

  type source = string

  (* initial stream is contains the charaters in the
   * string argument str *)
  fun mkInstream str = {buffer=(str,size str), pos=0}

  fun charRdr ({buffer as (s,limit), pos}: instream) = 
      if pos < limit
      then (* at least one unused character remains in buffer *)
        SOME(String.sub(s,pos), {buffer=buffer,pos=pos+1})
      else NONE (* string is exhausted *)

end (* structure StringStream *)

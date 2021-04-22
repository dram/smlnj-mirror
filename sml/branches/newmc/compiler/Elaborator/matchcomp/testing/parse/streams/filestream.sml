(* filestream.sml *)

(* deriving an instream from a text file *)

structure FileStream :> CHAR_STREAM
                        where type source = string
= struct

  type instream = InstreamStream.instream    (* lazy list of characters *)

  type source = string  (* file path *)

  fun mkInstream (filename: source) = 
      InstreamStream.mkInstream (TextIO.openIn filename)

  val charRdr = InstreamStream.charRdr

end  (* structure FileStream *)

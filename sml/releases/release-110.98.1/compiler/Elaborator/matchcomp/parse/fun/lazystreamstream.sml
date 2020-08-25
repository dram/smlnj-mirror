(* lazystreamstream.sml *)

structure LazyStreamStream :> CHAR_STREAM
                              where type source = char LazyStream.stream  =
struct

  structure LS = LazyStream

  type instream = char LazyStream.stream

  type source = instream

  fun mkInstream s = s

  val charRdr = LS.streamRdr

end (* structure LazyStreamStream *)

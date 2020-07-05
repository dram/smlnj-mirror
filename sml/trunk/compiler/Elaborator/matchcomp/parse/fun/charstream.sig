(* charstream.sig *)

signature CHAR_STREAM =
sig

  type instream
  type source

  val mkInstream : source -> instream
  val charRdr : (char, instream) Reader.reader

end (* signature CHAR_STREAM *)

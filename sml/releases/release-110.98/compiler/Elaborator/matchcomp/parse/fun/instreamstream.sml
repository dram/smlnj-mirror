(* instreamstream.sml *)

(* A functional instream generated from an imperative TextIO.instream.
 *
 * Turning the characters produced by the TextIO.instream into
 * a lazy list memoizes the sequence of characters and allows
 * the sequence to read nondestructively. *)

structure InstreamStream :> CHAR_STREAM
                            where type source = TextIO.instream  =
struct

  type instream = GeneratorStream.instream    (* lazy list of characters *)

  type source = TextIO.instream

  fun mkInstream (source: source) =
      GeneratorStream.mkInstream (fn () => TextIO.input1 source)

  val charRdr = GeneratorStream.charRdr

end  (* structure InstreamStream *)

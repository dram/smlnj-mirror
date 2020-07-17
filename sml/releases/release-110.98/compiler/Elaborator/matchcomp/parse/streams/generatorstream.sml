(* generatorstream.sml *)

(* A (character) generator function is a function of type
 * unit -> char option.  Each time it is called it generates
 * a new character.
 *
 * Turning the characters produced by the generator function
 * into a lazy list memoizes the sequence of characters and
 * allows the sequence to read nondestructively *)

structure GeneratorStream :> CHAR_STREAM
                             where type source = unit -> char option =
struct

  structure LS = LazyStream

  type instream = char LS.stream    (* lazy list of characters *)

  type source = unit -> char option

  fun mkInstream (genfn: source) : instream =
      let fun strmfn () = 
	      case genfn ()
                of NONE => LS.SNil
                 | SOME(c) => LS.SCons(c,LS.mkStream strmfn)
       in LS.mkStream strmfn 
      end

  val charRdr = LS.streamRdr

end  (* structure GeneratorStream *)

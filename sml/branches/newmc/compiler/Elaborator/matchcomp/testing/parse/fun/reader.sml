(* reader.sml *)

(* the generic reader monad: a model for purely functional input processing *)

structure Reader : READER =
struct

  (* an ('a,'b) reader is a function that consumes some
   * of its source (of type 'b) and, if successful, produces a result 
   * of type 'a and the remainder of the source (SOME(a,b)).
   * If the read action is unsuccessful, it returns NONE.
   * As a function, a reader action is performed by applying it
   * to a source. *)

  type ('a, 'b) reader = 'b -> ('a * 'b) option

  (* "monadic operations for readers: return, fail, chain *)

  (* return :  'a -> ('a,'b) reader *)
  fun return v = fn instream => SOME(v, instream)

  (* failure : ('a, 'b) reader *)
  val fail : ('a, 'b) reader = fn instream => NONE

  (* chain: ('a,'b) reader -> ('a -> ('c,'b) reader) -> ('c,'b) reader
   * this chains together a reader, and another reader computed
   * from the value produced by the first reader (if successful). *)
  fun chain (p: ('a,'b) reader) (f: 'a -> ('c,'b) reader) (instream: 'b) =
      case p instream
	of NONE => NONE
	 | SOME(v, instream') => f v instream'


  (* additional useful parsing operations: choice, star, starPlus *)
 
  (* choice: ('a,'b) reader * ('a,'b) reader -> ('a,'b) reader  *)
  (* perform p, but if it fails, perform q on same instream *)
  fun choice (p, q) instream =
      case p instream
	of NONE => q instream
	 | res => res

  (* star : ('a,'b) reader -> ('a list,'b) reader 
   * A "reader transform" that corresponds to the Kleene star. *)
  fun star r = choice (starPlus r, return [])

  (* starPlus : ('a,'b) reader -> ('a list,'b) reader *)
  and starPlus r = chain r (fn v => chain (star r) (fn vs => return (v::vs)))

  (* invert: 'a -> ('c,'b) reader -> ('a,'b) reader *)
  (* used for "lookahead", e.g. for keyword tokens, where we have to
   * check that the keyword is not a prefix of an identifier *)
  fun invert (v: 'a) (p: ('c,'b) reader) (instream: 'b) : ('a * 'b) option =
      case p instream
        of NONE => SOME(v,instream)
         | SOME _ => NONE

end (* structure Reader *)

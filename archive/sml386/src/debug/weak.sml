(* weak.sml
 *
 * COPYRIGHT 1990 AT&T BELL LABORATORIES
 *)

signature WEAK = 
  sig
    type 'a weak
    val weak : 'a -> 'a weak
    val strong : 'a weak -> 'a option
  end

abstraction Weak : WEAK =
  struct
    open System.Unsafe
    type 'a weak = 'a
    val weak_desc = System.Tags.desc_weak div 2
    fun weak x = cast(delay(weak_desc,x))
    fun strong x = if (subscript(cast x, ~1) = weak_desc)
	  then SOME(cast(subscript(cast x,0)))
	  else NONE
  end (* Weak *)

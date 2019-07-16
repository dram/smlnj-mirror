(*
 * util/table.sml:
 *   Lookup tables -- the unfanciest kind imaginable
 *    (If this really turns out to be a bottleneck, then I will
 *     replace it with something more efficient.)
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure Table: TABLE = struct

    datatype ('a, 'b) table =
	T of { eq: 'a * 'a -> bool, tab: ('a * 'b ref) list ref }

    fun create cmp = T { eq = cmp, tab = ref [] }
    fun clear (T { tab, ... }) = (tab := [])

    fun findref (x, T { eq, tab = ref l }) = let
	fun loop [] = NONE
	  | loop ((a, b) :: t) = if eq (x, a) then SOME b else loop t
    in
	loop l
    end

    fun enter (t as T { tab, ...}, a, b) =
	case findref (a, t) of
	    NONE => (tab := ((a, ref b) :: !tab))
	  | SOME r => (r := b)

    fun find (t, a) =
	case findref (a, t) of
	    NONE => NONE
	  | SOME r => SOME (!r)

    fun fold f t = let
	val T { tab = ref tab, ... } = t
	fun loop ([], a) = a
	  | loop ((x, ref y) :: tl, a) = loop (tl, f (x, y, a))
    in
	fn a => loop (tab, a)
    end

end

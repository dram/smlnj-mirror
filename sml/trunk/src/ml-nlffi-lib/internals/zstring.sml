(*
 * Functions for translating between 0-terminated C strings and native
 * ML strings.
 *
 *  (C) 2001, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure ZString : ZSTRING = struct
    local
	open C
	fun get' p = Cvt.ml_uchar (Get.uchar' (Ptr.|*! p))
	fun set' (p, w) = Set.uchar' (Ptr.|*! p, Cvt.c_uchar w)
	fun nxt' p = Ptr.|+! S.uchar (p, 1)
    in
        type 'c zstring = (uchar, 'c) ptr
	type 'c zstring' = (uchar, 'c) ptr'

	fun length' p = let
	    fun loop (n, p) = if get' p = 0w0 then n else loop (n + 1, nxt' p)
	in
	    loop (0, p)
	end
	fun length p = length' (Light.ptr p)

	fun toML' p = let
	    fun loop (l, p) =
		case get' p of
		    0w0 => String.implode (rev l)
		  | c => loop (Char.chr (Word32.toInt c) :: l, nxt' p)
	in
	    loop ([], p)
	end
	fun toML p = toML' (Light.ptr p)

	fun cpML' { from, to } = let
	    val n = String.size from
	    fun loop (i, p) =
		if i >= n then set' (p, 0w0)
		else (set' (p, Word32.fromInt (Char.ord
						   (String.sub (from, i))));
		      loop (i+1, nxt' p))
	in
	    loop (0, to)
	end
	fun cpML { from, to } = cpML' { from = from, to = Light.ptr to }

	fun dupML' s = let
	    val z = C.alloc' C.S.uchar (Word.fromInt (size s + 1))
	in
	    cpML' { from = s, to = z };
	    z
	end

	fun dupML s = let
	    val z = C.alloc C.T.uchar (Word.fromInt (size s + 1))
	in
	    cpML { from = s, to = z };
	    z
	end
    end
end

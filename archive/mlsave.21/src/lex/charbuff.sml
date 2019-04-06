(* lex.sml *)

(* charbuff.sml *)

signature CHAR_BUFFER = sig
    structure S: SYMBOL
    exception LexBufferOverflow
    val reset: unit -> unit
    and insertChar: int -> unit
    and makeId: unit -> S.symbol
    and makeInt: unit -> int
    and makeReal: unit -> real
    and makeString: unit -> string
end

structure CharBuffer: CHAR_BUFFER = struct

    structure S = Symbol

    exception LexBufferOverflow
    and NotImplemented

    val nchars = ref 0
    and max = 999
    and buffer: ByteArray.bytearray = ByteArray.array(1000, 0)

    fun reset () = (nchars := 0)

    fun insertChar(c) = 
	if !nchars > max
	    then raise LexBufferOverflow
	    else (ByteArray.update(buffer, !nchars, c); nchars := !nchars + 1)

    fun makeId () =
	let val sym = Symbols.lookupSymbol(buffer,!nchars)
	in (reset(); sym) end

    fun makeInt () =
	let val (i,mult) = if ByteArray.sub(buffer,0)=Ascii.tilde then (ref 1, ~1)
							  else (ref 0, 1)
	    and n = ref 0
	in  (while !i < !nchars do
		(n := 10 * !n + (ByteArray.sub(buffer,!i) - Ascii.zero); inc i))
	    handle Overflow => ErrorMsg.condemn "integer too large";
	    reset();
	    mult * !n
	end

    fun makeReal () =
	raise NotImplemented

    fun makeString () =
	let val str = ByteArray.extract(buffer, 0, !nchars)
	in  (reset(); str) end

end (* structure CharBuffer *)

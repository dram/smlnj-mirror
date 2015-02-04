(* lex.sml *)

(* charbuff.sml *)

signature CHAR_BUFFER = sig
    structure S: SYMBOL
    exceptionx LexBufferOverflow: unit
    val Reset: unit -> unit
    and InsertChar: int -> unit
    and MakeId: unit -> S.symbol
    and MakeInt: unit -> int
    and MakeReal: unit -> real
    and MakeString: unit -> string
end;

structure CharBuffer: CHAR_BUFFER = struct

    structure S = Symbol;

    exceptionx LexBufferOverflow: unit
    and NotImplemented: unit;

    open Byte_array;
    val nchars = ref 0
    and max = 999
    and buffer: byte_array = create(1000, 0);

    fun Reset () = (nchars := 0);

    fun InsertChar(c) = 
	if !nchars > max
	    then raisex LexBufferOverflow
	    else (store(buffer, !nchars, c); nchars := !nchars + 1);

    fun MakeId () =
	let val sym = SymbolTable.LookupSymbol(buffer,!nchars)
	in (Reset(); sym) end;

    fun MakeInt () =
	let val (i,mult) = if fetch(buffer,0)=Ascii.TILDE then (ref 1, ~1)
							  else (ref 0, 1)
	    and n = ref 0
	in  (while !i < !nchars do
		(n := 10 * !n + (fetch(buffer,!i) - Ascii.ZERO); i := !i + 1))
	    handlex overflow => ErrorMsg.Complain "integer too large";
	    Reset();
	    mult * !n
	end;

    fun MakeReal () =
	raisex NotImplemented;

    fun MakeString () =
	let val str = extract(buffer, 0, !nchars)
	in  (Reset(); str) end;

end; (* CharBuffer *)

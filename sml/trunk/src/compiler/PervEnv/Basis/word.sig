(* word.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature WORD = 
  sig
    eqtype word

    val wordSize : int

    val toLargeWord   : word -> LargeWord.word
    val toLargeWordX  : word -> LargeWord.word
    val fromLargeWord : LargeWord.word -> word

    val toLargeInt   : word -> LargeInt.int
    val toLargeIntX  : word -> LargeInt.int
    val fromLargeInt : LargeInt.int -> word

    val toInt   : word -> int
    val toIntX  : word -> int
    val fromInt : int -> word

    val orb  : word * word -> word
    val xorb : word * word -> word
    val andb : word * word -> word
    val notb : word -> word

    val << : (word * Word.word) -> word
    val >> : (word * Word.word) -> word
    val ~>> : (word * Word.word) -> word

    val + : word * word -> word
    val - : word * word -> word
    val * : word * word -> word
    val div : word * word -> word
    val mod : word * word -> word

    val compare : (word * word) -> order
    val >  : word * word -> bool
    val >= : word * word -> bool
    val <  : word * word -> bool
    val <= : word * word -> bool

    val min : (word * word) -> word
    val max : (word * word) -> word

    val scan :
	  StringCvt.radix -> (char, 'a) StringCvt.reader
	    -> (word, 'a) StringCvt.reader
    val fromString : string -> word option

    val fmt : StringCvt.radix -> word -> string
    val toString   : word -> string

  end;


(*
 * $Log$
 *)

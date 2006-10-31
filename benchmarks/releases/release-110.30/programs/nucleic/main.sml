(* main.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure Main : BMARK =
  struct
    val name = "Nucleic"
    fun doit () = (Nucleic.anticodon_length (); ())
    fun testit strm = TextIO.output(strm, concat[
	    Int.toString (Nucleic.anticodon_length ()), "\n"
	  ])
  end;


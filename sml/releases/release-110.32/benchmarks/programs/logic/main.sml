(* main.sml *)

structure Main : BMARK = 
  struct
    val name = "Logic"

    exception Done 

    fun testit strm = Data.exists(fn Z => Data.solution2 Z (fn () => raise Done))
	  handle Done => TextIO.output(strm, "yes\n")

    fun doit () = Data.exists(fn Z => Data.solution2 Z (fn () => raise Done))
	  handle Done => ()

  end; (* Main *)

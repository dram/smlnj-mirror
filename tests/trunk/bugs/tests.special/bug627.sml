(* bug627.sml *)
(* blast-writing objects outside the heap leads to failure *)

structure Blast =
struct
  val test : string = SMLNJVersion.banner
  structure B = BinIO
  fun outblast () = let
	val outs = B.openOut "/tmp/testblast"
	in
	  B.output(outs, Unsafe.blastWrite test); B.closeOut outs
	end
  fun inblast () = let
	val ins = B.openIn "/tmp/testblast"
	val res : string = Unsafe.blastRead(B.inputAll ins)
	in
	  B.closeIn ins;
	  print "Got back: "; print res; print " : from testblast\n"
	end
  val _ = outblast ()
  val _ = inblast ()
end;

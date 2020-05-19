(* run-asciidoctor.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure RunAsciidoctor : sig

    val run : string * TextIO.outstream -> OS.Process.status

  end = struct

    fun pipe (inS, outS) = (case TextIO.input inS
	   of "" => TextIO.closeIn inS
	    | s => (TextIO.output(outS, s); pipe(inS, outS))
	  (* end case *))

    val cmd = Config.asciidoctor

    fun args file = [
	    "-b", "html",
	    "--no-header-footer",
	    "-o", "-",
            "-a", concat["sml-basis-url=", !Options.basisLibURL],
	    "-a", concat["smlnj-version=", !Options.version],
	    "-a", concat["release-date=", !Options.releaseDate],
	    file
	  ]

    fun run (srcFile, outS) = let
	  val args = args srcFile
	  val _ = if !Options.verbose
		then print(String.concatWith " " (cmd :: args @ ["\n"]))
		else ()
	  val proc = Unix.execute (cmd, args)
	  val (fromProc, toProc) = Unix.streamsOf proc
	  in
	    pipe (fromProc, outS);
	    TextIO.closeOut toProc;
	    Unix.reap proc
	  end

  end
